

library(pacman)
p_load(here,
       tidyverse,
       labelled,
       sjlabelled,
       sjmisc,
       wCorr,
       rstatix,
       survey,
       officer,
       flextable,
       ggpattern,
       ggthemes,
       wesanderson,
       tryCatchLog,
       qreport)

options(silent.warnings = TRUE)
options(silent.messages = TRUE)


# Wrangling  --------------------------------------------------------------
## easy viewing
### ex. view(df_a, "N", "B)
view <- function(df, ...) {
  View(df %>% dplyr::relocate(tidyselect::starts_with(c(...))))
}

# generate scales
scale.means <- function(df, ..., na.rm = FALSE) {
  vars <- unlist(list(...))
  mean_vars <- rowMeans(df[, vars], na.rm = na.rm)
  return(mean_vars)
}


# hist_quantiles <- function(data = cur_data(), quantile = 0.5) {
#   delta_cumsums <- data %>%
#     select(starts_with("midpoint_"), starts_with("delta_")) %>%
#     pivot_longer(cols = everything(), names_to = c(".value", "bin"), names_sep = "_") %>%
#     mutate(across(delta, ~tidyr::replace_na(., 0)),
#            cumsum = cumsum(delta))
#   
#   # print(delta_cumsums)  # For debugging
#   
#   idx <- which(delta_cumsums$cumsum >= quantile)[1]
#   if (is.na(idx)) NA else delta_cumsums$midpoint[idx]
# }

pull_wide <- function(val_is_name, data) {
  return(data[[val_is_name]])
}

# Clean strings by replacing punctuation with English
# & deleting punctuation at start and end of string
replace_punct_with_english <- function(name) {
  punct_to_english <- list(
    '<' = "less_than",
    '>' = "greater_than",
    '=' = "equals",
    '+' = "plus",
    '*' = "star",
    '$' = "dollars_",
    '"' = "quote",
    '\'' = "quote"
    # Add more mappings as needed
  )
  
  # Replace known punctuation with English equivalents
  for (punct in names(punct_to_english)) {
    name <- gsub(punct, punct_to_english[[punct]], name, fixed = TRUE)
  }
  
  # Replace any remaining punctuation with underscores
  name <- gsub("[[:punct:]]", "_", name)
  # Also delete punctuation at the start or end of the string
  name <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", name)
  
  return(name)
}

# Function to find identical and nearly identical columns with NA handling
col_similarity <- function(data, target_column, tolerance = 0.10) {
  target <- as.character(data[[target_column]])

  results <- data.frame(col_names = colnames(df)[colnames(df) != target_column],
                        diff_pcnt = NA,
                        NA_pcnt = NA)
  
  i <- 0
  
  for (col_name in names(data)) {
    if (col_name != target_column) {
      column <- as.character(data[[col_name]])
      i <- i + 1

      # NA values are counted and filtered out
      NA_indices <- !is.na(target) & is.na(column)
      results$NA_pcnt[i] <- sum(NA_indices) / sum(!is.na(target))
      
      valid_indices <- !is.na(target) & !is.na(column)
      # Compute difference ratio only for non-NA values
      diff_ratio <- mean(target[valid_indices] != column[valid_indices])
      results$diff_pcnt[i] <- round(diff_ratio * 100, 2)
    }
  }

  results <- results %>% dplyr::arrange(diff_pcnt)
  
  return(results)
}


# Function to retain prior line breaks but wrap if longer than width
wrap_long_lines <- function(string, width = 80, paren_break = FALSE){
  # Split the input text into lines
  lines <- unlist(stringr::str_split(string, "\n"))
  
  # Wrap lines longer than 80 characters
  wrapped_lines <- purrr::map(lines, \(line) {
    if (nchar(line) > width) {
      stringr::str_wrap(line, width = width)
    } else {
      line
    }
  })
  
  # Combine the wrapped lines back into a single string
  wrapped_text <- paste(wrapped_lines, collapse = "\n")
  
  if(paren_break == TRUE){
    wrapped_text <- stringr::str_replace_all(wrapped_lines, ' \\(', '\n\\(')
  }
     
  
  return(wrapped_text)
}




# Function to keep only the longer elements if any are substrings of others
keep_longer_elements <- function(vec) {
  # Iterate over each pair of elements
  keep <- purrr::map_lgl(seq_along(vec), function(i) {
    # Check if the current element is not a substring of any longer element
    !any(purrr::map_lgl(seq_along(vec), function(j) {
      i != j && stringr::str_length(vec[j]) > stringr::str_length(vec[i]) && stringr::str_detect(vec[j], fixed(vec[i]))
    }))
  })
  
  # Return the filtered vector with only longer elements
  vec[keep]
}


# Function to generate subsets of data
make_subsets_list <- function(data = df, var) {
  # Get unique levels of the variable
  levels <- unique(data[[var]])
  
  # Create a list of data frames, each filtered by one level
  subsets_list <- purrr::map(rlang::set_names(levels), ~ data %>% dplyr::filter(.data[[var]] == .x))
  
  # Remove empty data frames from the list
  subsets_list <- purrr::keep(subsets_list, ~ nrow(.x) > 0)
}


# test_list <- list(
#   a1 = list(b = list(d = 1, e = 2), c = list(d = 3)),
#   a2 = list(a2 = list(b = 4)),
#   a3 = list(a3 = list(a3 = list(b = 4))),
#   a4 = list(a4 = 7),
#   a5 = list(a5 = list(b = 4), b = 4),
#   a6 = list(b = list(c = list(d = list(e = 4))))
# )
# 
# t <- flatten_containers(test_list)

flatten_containers <- function(lst, prefix = "") {
  
  # Function to recursively merge lists where the only child is another list
  merge_upward_if_single_child <- function(lst) {
    while (identical(class(lst),"list") && length(lst) == 1) {
      # If the only element is also a list, merge it upward
      sublist_names <- names(lst)
      if (identical(class(lst[[sublist_names]]),"list")) {
        cat(sprintf("Merging '%s' into '%s'\n", paste0(prefix, ".", sublist_names), prefix))
        lst <- lst[[sublist_names]]
      } else {
        break  # Exit if the single element is not a list
      }
    }
    return(lst)
  }
  
  # Check if the current element is a list and apply recursive merging if necessary
  if (identical(class(lst),"list")) {
    lst <- merge_upward_if_single_child(lst)  # Attempt to merge at the current level
    
    # Iterate through each element in the merged list and apply recursively
    sublist_names <- names(lst)
    for (name in sublist_names) {
      if (!is.null(name)) {
        full_name_path <- if (nzchar(prefix)) paste0(prefix, ".", name) else name
        lst[[name]] <- flatten_containers(lst[[name]], full_name_path)
      }
    }
  }
  return(lst)
}


# Conditional fill (down) based on text value
fill_where <- function(df, x, prefix) {
  v <- df[[x]]
  for (i in 1:(length(v)-1)){
    # If the current value matches the prefix &
    # the next value is missing,
    # fill in the next value with the previous value
    if(!is.na(v[i]) && startsWith(v[i], prefix) && is.na(v[i+1])){ 
      v[i+1] <- v[i]
    }
  }
  
  df[[x]] <- v
  return(df)
}

# Conditional fill (down)
# Fill in rows until date in first row of 'col' is greater than the value of 'date_ref_col'
fill_if_before_date <- function(df, col, date_ref_col) {
  v <- df[[col]]
  date_ref_v <- df[[date_ref_col]]
  for (i in 1:(length(v)-1)){
    # If the current value matches the prefix &
    # the next value is missing,
    # fill in the next value with the previous value
    if(!is.na(v[i]) & (v[i] <= date_ref_v[i])){
      v[i+1] <- v[i]
    }
  }
  
  df[[col]] <- v
  return(df)
}


# Conditional fill (down)
# Fill in rows until date in first row of 'col' is greater than the value of 'date_ref_col'
# fill_vector <- c(NA, 7, NA, 10, NA, NA, 15, NA)
# ref_vector <- c(5, 6, 8, 9, 10, 12, 13, 16)
# filled_vector <- fill_if_greater_than_equal(fill_vector, ref_vector)
fill_if_greater_than_equal <- function(col, ref_col) {
  
  # v <- data[[col]]
  # ref_v <- data[[ref_col]]
  
  v <- col
  ref_v <- ref_col
  
  for (i in 1:(length(v)-1)){
    # If the current value is less than the ref value &
    # the next value is missing,
    # fill in the next value with the previous value
    if(!is.na(v[i]) & !is.na(ref_v[i+1]) & (v[i] >= ref_v[i+1])){
      v[i+1] <- v[i]
    }
  }
  
  # data[[col]] <- v
  # return(data)
  return(v)
}

# Function to pad vectors with NAs to the same length
pad_na2 <- function(vec, max_length) {
  if (length(vec) < max_length) {
    vec <- c(vec, rep(NA, max_length - length(vec)))
  }
  return(vec)
}


# Bind two numeric data frame columns in ascending order
# fill_vector <- c(NA, 7, NA, 10, NA, NA, 15, NA)
# ref_vector <- c(5, 6, 8, 9, 10, 12, 13, 16)
# filled_vector <- bin_midpts_values_join(fill_vector, ref_vector)
# filled_vector <- bin_midpts_values_join(b, a)
bin_midpts_values_join <- function(col, ref_col) {
  
  ref_v <- ref_col
  
  v <- col
  col_name <- deparse(substitute(col))  # Get the name of the 'col' parameter as a string
  first_non_missing <- v[which(!is.na(v))][1]  # Find the first non-NA value
  
  for (i in 1:(length(v)-1)){
    # If the current value is less than the ref value &
    # the next value is missing,
    # fill in the next value with the previous value
    if(!is.na(ref_v[i+1])
       & (first_non_missing >= ref_v[i+1])
       & (first_non_missing < ref_v[i+2])){
      v[i+1] <- paste0(col_name,'_',first_non_missing)
    }
    
    if(!is.na(v[i]) & v[i] == first_non_missing){v[i] <- NA}
  } 

  return(v)
}


# Bind two numeric data frame columns in ascending order
# vals_test_df <- data.frame(vals = c(NA, 7, NA, 10, NA, NA, 15, NA, 17),
#                            test = rep("a", 9))
# bins_test_df <- data.frame(bin_midpts = c(5, 6, 8, 9, 10, 12, 13, 16),
#                            test2 = rep("b", 8))
# sorted <- bin_midpts_values_sort(bins_df = bins_test_df,
#                                  bins_col = "bin_midpts",
#                                  vals_df = vals_test_df,
#                                  vals_col = "vals")
# 
# sorted <- bin_midpts_values_sort(a, b)
bin_midpts_values_sort <- function(bins_df, bins_col,
                                   vals_df, vals_col) {
  
  bins_v <- bins_df[[bins_col]]
  bins_v <- bins_v[!is.na(bins_v)]
  
  vals_v <- vals_df[[vals_col]]
  vals_v <- vals_v[!is.na(vals_v)]
  
  # Combine and sort the values
  sorted_values <- sort(c(bins_v, vals_v))
  
  # Create a data frame from sorted values
  sorted_df <- data.frame(sorted = sorted_values)
  
  # Merge with bins_col and vals_col to keep track of original columns
  out <- sorted_df %>%
    dplyr::mutate(sorted_order = row_number()) %>%
    dplyr::full_join(bins_df, by = c("sorted" = bins_col),
              keep = TRUE, relationship = "many-to-one") %>%
    dplyr::full_join(vals_df, by = c("sorted" = vals_col),
              keep = TRUE, relationship = "many-to-one")
  
  return(out)
}


# Functions for Recoding & Labels ----------------------------------------------
# Function to reorder factor levels when the factor has labels
reorder_labelled_factor <- function(x, levs, copy_old_lbls = FALSE) {
  old_x <- x
  
  # Take a factor vector as input
  x <- factor(x, levels = levs, ordered = TRUE)
  
  # Copy the original var label to the new factor
  attr(x, "label") <- attr(old_x, "label")
  
  # Replicate old val labels if user requested
  if(copy_old_lbls == TRUE){
    attr(x, "labels") <- attr(old_x, "labels")
  } else {
    # Otherwise set labels from new levels
    attr(x, "labels") <- rlang::set_names(levs, levs)
  }
  
  return(x)
}

# Function to recognize numeric values in factor levels for reordering
# E.g.
# x <- factor(c("$10,000 to under $20,000", "$100,000 to under $150,000",
#               "$150,000 or more", "<$10,000"))
# levels(recognize_num_levs(x))
recognize_num_levs <- function(x) {
  old_x <- x
  
  # Take a factor vector as input
  # Assuming "e.g. <$10,000" should be treated as "0" for sorting purposes
  levs <- gsub("^(<|Less than |Under )", "0 ", levels(x))
  # Extract numeric values
  nums <- gsub(",", "", sapply(regmatches(x=levs, gregexpr("[[:digit:],]+", levs)), `[`, 1))
  x <- factor(x, levels = levels(x)[order(as.numeric(nums))])
  
  # Copy the original attributes to the new factor
  attr(x, "labels") <- attr(old_x, "labels")
  attr(x, "label") <- attr(old_x, "label")
  
  return(x)
}

# recode_labelled_factor: Function to recode factors w/ label
## Modified from forcats::fct_recode
## lvls_rename: Helper function from forcats

# f <- to_factor_with_labs(df$educ5)
# lvls_rename(f, new_levels = c(
#                   "< HS" = "Less than HS",
#                   "HS grad" = "HS graduate or equivalent",
#                   "Some college" = "Some college/ associates degree",
#                   "Bachelor's" = "Bachelor's degree",
#                   "Post grad" = "Post grad study/professional degree"))


lvls_rename <- function (f, new_levels) {
  old_levels <- levels(f)
  idx <- match(new_levels, old_levels)
  if (any(is.na(idx))) {
    bad <- new_levels[is.na(idx)]
    warning("Unknown levels in `f`: ", paste(bad, collapse = ", "),
            call. = FALSE)
    new_levels <- new_levels[!is.na(idx)]
    idx <- idx[!is.na(idx)]
  }
  old_levels[idx] <- names(new_levels)
  old_levels
}


# Function to recode a factor w/ labels
# x <- to_factor_with_labs(df$educ5)
# head(recode_labelled_factor(x,
# levs = c(
#   "< HS" = "Less than HS",
#   "HS grad" = "HS graduate or equivalent",
#   "Some college" = "Some college/ associates degree",
#   "Bachelor's" = "Bachelor's degree",
#   "Post grad" = "Post grad study/professional degree")))

recode_labelled_factor <- function (x, levs, copy_old_lbls = FALSE, ordered = TRUE) {
  old_x <- x
  # x <- checkmate::check_factor(x)
  # levs <- check_recode_levels(levs)
  # nulls <- names(levs) == "NULL"
  # if (any(nulls)) {
  #   levs <- setdiff(levels(x), levs[nulls])
  #   levs <- levs[!nulls]
  # }
  
  # to_NAs_levs <- levs[is.na(names(levs))]
  # x <- fct_recode(x, !!!rlang::set_names(rep(list(NULL), length(to_NAs_levs)), to_NAs_levs))
  
  if(ordered == TRUE){
    idx <- match(levs, levels(x))  #; levels(x); levels(x)[idx]
    x <- forcats::lvls_reorder(
      forcats::lvls_revalue(x, lvls_rename(x, levs)),
      idx = idx, ordered = TRUE)
  } else {
    x <- forcats::lvls_revalue(x, lvls_rename(x, levs))
  }
  
  # Note, the default behavior is to discard NAs from the levels
  # x2 <- fct_na_value_to_level(x) would restore NAs to the levels
  x <- forcats::fct_na_level_to_value(x)
  
  # Copy the original var label to the new factor
  attr(x, "label") <- attr(old_x, "label")
  
  # Replicate old val labels if user requested
  if(copy_old_lbls == TRUE){
    attr(x, "labels") <- attr(old_x, "labels")
  } else {
    # Otherwise set labels from new levels
    attr(x, "labels") <- rlang::set_names(names(levs), levs)
  }
  
  return(x)
}

# Function to replace string patterns within factor levels
# Defaults to deleting the pattern
# levels(str_replace_levs(x, " or equivalency diploma"))
str_replace_levs <- function(x, pattern, replacement = ""){
  # Ensure 'x' is a factor
  if(!is.factor(x)) {
    stop("Input 'x' must be a factor.")
  }
  
  old_levs <- levels(x)
  
  levs <- stringr::str_replace_all(old_levs, pattern, replacement)
  
  x <- forcats::lvls_revalue(x, levs)
  if(is.ordered(x)){
    x <- forcats::lvls_reorder(x, seq_along(levs), ordered = TRUE)
  }
  
  return(x)
}


# indicator/wide to long for factor indicators
indicator_to_long <- function(x, id="caseid", prefix, suffix, match, numeric_cols = TRUE) { # x is data frame,
  # id is the id column for tidyr::pivot_longer,
  # to identify the columns to collapse, match can be used
  # or prefix and suffix can be used; prefix and suffix can be used together or independently
  # prefix is a chr string
  # suffix is a chr string
  # match is a regex
  #ex. who_in_hh_long = indicator_to_long(df_raw, id="caseid", prefix="who_in_hh_", suffix="_txt")
  
  # Select cols using match
  if(!missing(match)){
    x2 <- x %>%
      dplyr::select(tidyselect::all_of(id), tidyselect::matches(match))
    
    # Otherwise select cols using prefix & suffix
  } else if (!missing(prefix) & !missing(suffix)) {
    x2 <- x %>%
      dplyr::select(tidyselect::all_of(id), (tidyselect::starts_with(prefix) & tidyselect::ends_with(suffix)))
    
    # Otherwise select cols using prefix
  } else if (!missing(prefix) & missing(suffix)){
    x2 <- x %>%
      dplyr::select(tidyselect::all_of(id), tidyselect::starts_with(prefix))
    
    # Otherwise select cols using suffix
  } else {
    x2 <- x %>%
      dplyr::select(tidyselect::all_of(id), tidyselect::ends_with(suffix))
  }
  
  # Store old labels from first column to be collapsed
  # print(colnames(x2[,2]))
  old_var_lab <- var_label(x2[,2])
  # print(old_var_lab)
  old_val_labs <- val_labels(x2[,2])
  # print(old_val_labs)
  
  if(numeric_cols){
    x2 <- x2 %>%
      dplyr::mutate(dplyr::across(-tidyselect::all_of(id), ~to_factor_with_labs(.)))
  }
  
  # Pivot longer
  x2 <- x2 %>%
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(id),
      names_to = "dummy_name", values_to = "dummy_val"
    )
  
  # Use the prefix or suffix given to name the long column
  if(!missing(prefix)){
    x2 <- x2 %>%
      dplyr::mutate(dummy_name = stringr::str_remove(dummy_name, paste0(prefix)))
  }
  if(!missing(suffix)){
    x2 <- x2 %>%
      dplyr::mutate(dummy_name = stringr::str_remove(dummy_name, paste0(suffix)))
  }
  
  x2 <- x2 %>%
    # Group by row
    dplyr::group_by(!!sym(id)) %>%
    # Paste NA if only NAs or "No" values in the row
    dplyr::reframe(long = if(all(is.na(dummy_val)) | all(dummy_val == "No", na.rm = TRUE)) NA_character_
            # Otherwise collapse all values from the row
            else paste(dummy_val[dummy_val != "No"], collapse = ","))
  
  # Reassign variable label
  var_label(x2$long) <- stringr::str_match(old_var_lab, "\\] (.+)")[,2]
  
  # Update value labels so that labels = values
  if (!is.null(old_val_labs)) {
    x2$long <- sjlabelled::add_labels(x2$long, labels = rlang::set_names(names(old_val_labs), names(old_val_labs)))
  }
  
  return(x2$long)
}

# t <- indicator_to_long(df_raw, id="caseid", prefix="who_in_hh_", suffix="_txt")
# # labelled::get_variable_labels(t)
# labelled::get_value_labels(t)

bind_cols_pad_na <- function(data = NULL){
  # Function to pad vectors with NAs to the same length
  pad_na <- function(vec, max_length) {
    length(vec) <- max_length
    vec
  }
  
  max_length <- max(purrr::map_int(data, length))
  
  data <- data %>%
    purrr::map(~pad_na(.x, max_length)) %>%
    bind_cols()
  
  return(data)
}


library(sjmisc)
# Edit sjmisc::rec to not delete spaces
replace_function_line <- function(call_obj, pattern, replacement) {
  call_list <- as.list(call_obj)  # Convert the call to a list to manipulate it
  
  # Check if the first element (the function name in the call) matches the pattern
  if (is.symbol(call_list[[1]]) && deparse(call_list[[1]]) == pattern) {
    call_list[[1]] <- as.symbol(replacement)
  }
  
  # Loop through the list to find and replace pattern within any nested calls
  for (i in seq_along(call_list)) {
    if (is.call(call_list[[i]])) {  # If the element is a call
      call_list[[i]] <- replace_function_line(call_list[[i]], pattern, replacement) # Recursively modify it
    }
  }
  
  # Convert the list back to a call
  return(as.call(call_list))
}

## Function to replace function calls in the body of another function
replace_function_calls_in_body <- function(func_obj, pattern, replacement) {
  lines_to_edit <- grep(pattern, body(func_obj))
  for (line in lines_to_edit) {
    body(func_obj)[[line]] <- replace_function_line(
      body(func_obj)[[line]], pattern, replacement
    )
  }
  return(func_obj)
}


## Edit helper function where the 'remove spaces' line resides
rec_helper_with_spaces <- getAnywhere(rec_helper)
lines_to_remove <- grep("^rec_string <- gsub\\(\" \", \"\"", body(rec_helper_with_spaces[["objs"]][[1]]))
body(rec_helper_with_spaces[["objs"]][[1]])[lines_to_remove] <- NULL
body(rec_helper_with_spaces[["objs"]][[1]])[c(lines_to_remove-1, lines_to_remove, lines_to_remove+1)]
assign("rec_helper_with_spaces", rec_helper_with_spaces[["objs"]][[1]], envir = .GlobalEnv)

## Then rec_core_fun
rec_core_fun_with_spaces <- getAnywhere(rec_core_fun)[["objs"]][[1]]
rec_core_fun_with_spaces <- replace_function_calls_in_body(rec_core_fun_with_spaces, "rec_helper", "rec_helper_with_spaces")
# rec_core_fun_with_spaces
assign("rec_core_fun_with_spaces", rec_core_fun_with_spaces, envir = .GlobalEnv)

## Then rec.default
rec.default_with_spaces <- getAnywhere(rec.default)[["objs"]][[1]]
rec.default_with_spaces <- replace_function_calls_in_body(rec.default_with_spaces, "rec_core_fun", "rec_core_fun_with_spaces")
assign("rec.default_with_spaces", rec.default_with_spaces, envir = .GlobalEnv)
# rec.default_with_spaces

# Function to convert numeric columns into chr columns from the numeric values' labels
# E.g. to_factor_with_labs(df_raw$who_in_hh_1)
to_factor_with_labs <- function(x, explicit_tagged_na = TRUE, ordered = FALSE) {
  # Extract the names and values of value labels
  old_x <- x
  old_val_labs <- attr(x, "labels") #; print(old_val_labs)
  
  # Adapted from haven::format_tagged_na
  format_tagged_na_val_labs <- function(labs) {
    old_names <- names(labs)
    nas_idx <- which(is_tagged_na(labs))
    non_nas_idx <- which(!is_tagged_na(labs))
    out <- as.character(labs)
    out[non_nas_idx] <- old_names[non_nas_idx]
    out[nas_idx] <- paste0(na_tag(labs)[is_tagged_na(labs)], "_NA")
    # out[nas_idx] <- paste0("`NA(", na_tag(labs)[is_tagged_na(labs)], ")`")
    names(out) <- old_names
    return(out)
  }
  
  aux_val_labs <- format_tagged_na_val_labs(attr(x, "labels"))
  
  # Replace NA labs with NA
  # names(aux_val_labs)[is.na(aux_val_labs)] <- NA
  # aux_val_labs <- aux_val_labs[!duplicated(aux_val_labs)]
  
  # Construct recoding scheme 
  labs_val_labs <- gsub(";",",", names(aux_val_labs), fixed=T)
  vals_val_labs <- unname(aux_val_labs)
  
  ## Prepare new value labels
  new_val_labs <- rlang::set_names(labs_val_labs, vals_val_labs)
  # print(new_val_labs)
  
  # ## Use paste to format each element as 'value ~ "name"'
  # cases <- paste(vals_val_labs, '=', labs_val_labs, sep = "")
  # # print(cases)
  # 
  # ## Combine all elements into a single character string
  # cases <- paste(c(cases,"else=copy"), collapse = ";")
  # # print(cases)
  # 
  # x2 <- rec.default_with_spaces(x2, rec = cases, val.labels = new_val_labs,
  #                               as.num = FALSE, to.factor = TRUE)
  
  ## Recode vector
  if(ordered){
    x <- labelled::to_factor(x, ordered = T)
  } else {
    x <- labelled::to_factor(x)
  }
  
  x <- sjlabelled::add_labels(x, labels = new_val_labs)
  
  return(x)
}

# Create value labels from the variable label
val_labs_from_var_lab <- function(x) {
  # Get variable label, assuming it is in the format "[Label] Description"
  var_label <- attr(x, "label")
  new_val_label <- sub("^\\[(.*)\\].*$", "\\1", var_label) # Extract the label inside brackets
  # Set column's labels with 1 = new value label
  x <- sjlabelled::add_labels(x, labels = rlang::set_names(1, new_val_label))
  # Return the modified vector
  return(x)
}

# # Create value labels from the variable's unique values
# val_labs_from_unq_vals <- function(x) {
#   # Get unique values of the column
#   unq_vals <- if(sum(is.na(x))>0){unclass(c(unique(x)), NA)} else {unclass(unique(x))}
#   print(unq_vals)
#   print("nnnn")
#   new_val_labs <- rlang::set_names(unq_vals, unq_vals) # Extract the label inside brackets
#   # Set column's labels with 1 = new value label
#   #x <- sjlabelled::add_labels(x, labels = new_val_labs)
#   # Return the modified vector
#   return(new_val_labs)
# }
# str(val_labs_from_unq_vals(df_raw2$state))
# val_labels(df_raw2$state)

# Rename columns using variable labels (assuming particular format)
colname_from_var_lab <- function(.data, subset_colnames) {
  # Access the relevant cols
  subset <- .data %>% dplyr::select(matches(subset_colnames))
  
  # Use sapply to iterate over column names and apply the renaming logic
  new_names <- sapply(colnames(subset), function(colname) {
    # Get variable label
    var_label <- attr(subset[[colname]], "label")[1]
    if (!is.null(var_label) & grepl("^\\[(.*)\\].*$", var_label)) {
      # Extract the label inside brackets
      new_colname <- sub("^\\[(.*)\\].*$", "\\1", var_label)
      # Paste variable label as suffix
      new_colname <- paste0(sub("\\d+$", "", colname), new_colname)
      return(gsub(" ", "_", new_colname, fixed = TRUE))
    } else {
      # Use the original column name if there's no label
      return(colname)
    }
  }, USE.NAMES = TRUE)  # Ensure output is named
  
  # Rename the columns in the original data frame using the new_names vector
  .data <- .data %>%
    dplyr::rename_with(.fn = function(x) new_names[x],
                .cols = tidyselect::matches(subset_colnames)) %>%
    clean_names()
  
  # Return the data frame containing renamed columns
  return(.data)
}


add_labels_abridged_num <- function(x, value){
  
  current.labels <- get_labels(
    x,
    attr.only = TRUE,
    values = "n",
    non.labelled = FALSE,
    drop.na = TRUE
  )
  
  if (!is.null(current.labels)) {
    # remove multiple value labels
    doubles <- names(current.labels) %in% as.character(value)
    # print(doubles)
    
    # switch value and names attribute, since get_labels()
    # returns the values as names, and the value labels
    # as "vector content"
    val.switch <- as.numeric(names(current.labels))
    names(val.switch) <- as.character(current.labels)
    # print(val.switch)
    
    # update all labels
    all.labels <- c(val.switch[!doubles], value)
    # print(all.labels)
    
    # tell user
    if (any(doubles)) {
      message(sprintf(
        "label '%s' was replaced with new value label.\n",
        current.labels[doubles]
      ))
    }
  } else {
    all.labels <- value
  }
  
  if (requireNamespace("haven", quietly = TRUE)) {
    # get current NA values - requires haven
    current.na <- unique(x[is.na(x)])
  }
  
  # sort labels by values
  all.labels <- all.labels[order(as.numeric(all.labels))]
  
  # set back value labels
  attr(x, "labels") <- all.labels
  
  x
}

# x <- df_raw_aux$industry_txt
# value <- c("Don't Know" = tagged_na("d"),
#            "Skipped" = tagged_na("s"),
#            "Refused" = tagged_na("r"))

add_labels_abridged_chr <- function(x, value){
  
  current.labels <- get_labels(
    x,
    attr.only = TRUE,
    values = "n",
    non.labelled = FALSE,
    drop.na = TRUE
  )
  
  if (!is.null(current.labels)) {
    # remove multiple value labels
    doubles <- names(current.labels) %in% as.character(value)
    # print(doubles)
    
    # switch value and names attribute, since get_labels()
    # returns the values as names, and the value labels
    # as "vector content"
    val.switch <- current.labels
    names(val.switch) <- as.character(current.labels)
    # print(val.switch)
    
    # update all labels
    all.labels <- c(val.switch[!doubles], value)
    # print(all.labels)
    
    # tell user
    if (any(doubles)) {
      message(sprintf(
        "label '%s' was replaced with new value label.\n",
        current.labels[doubles]
      ))
    }
  } else {
    all.labels <- value
  }
  
  if (requireNamespace("haven", quietly = TRUE)) {
    # get current NA values - requires haven
    current.na <- unique(x[is.na(x)])
  }
  
  # set back value labels
  attr(x, "labels") <- all.labels
  
  x
}

# attr(
#     add_labels_abridged_chr(df_raw_aux$industry_txt,
#                           c("Don't Know" = tagged_na("d"),
#                             "Skipped" = tagged_na("s"),
#                             "Refused" = tagged_na("r"))),
#     "labels")

# attr(df_raw_aux$industry_txt, "labels")
# attr(df_raw_aux2$industry_txt, "labels")
# str(df_raw_aux2$industry_txt)


make_var_label_tbl <- function(var_list, data) {
  var_label_tbl <- list(variable = var_list)
  var_label_tbl$variable_label <- labelled::get_variable_labels(df %>% dplyr::select(tidyselect::all_of(var_list)))
  var_label_tbl <- dplyr::bind_rows(var_label_tbl)
  colnames(var_label_tbl) <- stringr::str_to_title(stringr::str_replace_all(colnames(var_label_tbl), '_', ' '))
  return(kbl_local_default(var_label_tbl))
}

# Function (with helpers) to conditionally label as tagged NAs
## Adapted from ipumsr lbl_helpers.r ####
# https://github.com/mnpopcenter/ipumsr/blob/main/R/lbl_helpers.r
## Helper 1
abort_coercion_function <- function(x) {
  x_type <- friendly_type_of(x)
  abort(paste0("Can't convert ", x_type, " to function"))
}

## Helper 2
as_lbl_function <- function(x, env = caller_env()) {
  if (rlang::is_function(x)) {
    return(x)
  }
  
  if (rlang::is_quosure(x)) {
    return(eval(rlang::expr(function(...) rlang::eval_tidy(!!x))))
  }
  
  if (rlang::is_formula(x)) {
    if (length(x) > 2) {
      rlang::abort("Can't convert a two-sided formula to a function")
    }
    
    args <- list(... = rlang::missing_arg(), .val = quote(..1), .lbl = quote(..2))
    fn <- new_function(args, rlang::f_rhs(x), rlang::f_env(x))
    return(fn)
  }
  
  if (rlang::is_string(x)) {
    return(get(x, envir = env, mode = "function"))
  }
  
  abort_coercion_function(x)
}

# Main Function
lbl_tagged_na_if <- function(x, tag, .predicate) {
  pred_f <- as_lbl_function(.predicate, caller_env())
  
  labels <- attr(x, "labels")
  to_zap <- pred_f(.val = unname(labels), .lbl = names(labels))
  
  if (any(is.na(to_zap))) {
    stop("Predicates cannot evaluate to missing in `lbl_na_if()`.", call. = FALSE)
  }
  
  vals_to_zap <- unname(labels[to_zap])
  new_labels <- labels[!to_zap]
  
  out <- x
  out[out %in% vals_to_zap] <- tagged_na(tag)
  attr(out, "labels") <- new_labels
  
  out
}


# Function to count each kind of tagged NA
count_tagged_na <- function(col) {
  # Extract the unique tagged NA values in the col
  na_tags <- na.omit(unique(na_tag(col)))
  
  # Count each type of tagged NA
  counts <- numeric(length(na_tags)) # Initialize vector
  for (i in seq_along(na_tags)) {
    counts[i] <- sum(na_tag(col) == na_tags[i], na.rm = T)
  }
  
  data.frame(tag = rev(na_tags), count = rev(counts)) # Return a df
}


reverse_tagged_na_col <- function(x) {    # x is a column in a data frame
  old_x <- x
  
  col_max <- max(x, na.rm = T) + 1    # take the max of the column and add 1
  reversed <- col_max - x    # subtract the calculated value
  
  old_label <- attributes(old_x)$label
  old_labels <- attributes(old_x)$labels
  na_labs <- old_labels[is.na(old_labels)]
  range <- old_labels[!is.na(old_labels)]
  new_labels <- rlang::set_names(sort(range), names(sort(range, decreasing = T)))
  new_labels <- c(new_labels, na_labs)
  
  attributes(reversed)$label <- old_label
  attributes(reversed)$labels <- new_labels
  
  return(reversed)
}

# Statistics ------------------------------------------------------------------
# basic stats
sem <- function(x) {
  sd(x, na.rm = TRUE) / sqrt(length(x))
}

ci <- function(x) {
  sem(x) * 1.96
}

# normalize <- function(x) (x - min(x))/(max(x)-min(x))

reverse <- function(x) {    # x is a column in a data frame
  col_max <- max(x, na.rm = T) + 1    # take the max of the column and add 1
  reversed <- col_max - x    # subtract the calculated value
  return(reversed)
}

# color palette
library(wesanderson)
pal <- wesanderson::wes_palette("Zissou1", 100, type = "continuous")

welch_p_value <- function(mean1, mean2, sd1, sd2, n1, n2) {
  t_stat <- (mean1 - mean2) / sqrt((sd1^2 / n1) + (sd2^2 / n2))
  print(t_stat)
  df <- (((sd1^2 / n1) + (sd2^2 / n2))^2) / ((sd1^4 / (n1^2 * (n1 - 1))) + (sd2^4 / (n2^2 * (n2 - 1))))
  2 * pt(-abs(t_stat), df)
}

round_nearest <- function(x, nearest = 0.2, method = "round") {
  if (method == "round") {
    return(round(x / nearest) * nearest)
  } else if (method == "floor") {
    return(floor(x / nearest) * nearest)
  } else if (method == "ceiling") {
    return(ceiling(x / nearest) * nearest)
  } else {
    stop("Invalid method. Choose 'round', 'floor', or 'ceiling'.")
  }
}

# Adapted from rstatix::dunn_test
dunn_test_super_detailed <- function(data, formula, p.adjust.method = "holm", detailed = FALSE){
  args <- as.list(environment()) %>%
    .add_item(method = "dunn_test")
  if(is_grouped_df(data)){
    results <- data %>%
      rstatix::doo(.dunn_test, formula, p.adjust.method )   # rstatix utilities
  }
  else{
    results <- .dunn_test(data, formula, p.adjust.method)
  }
  
  if(!detailed){
    results <- results %>%
      dplyr::select(-.data$method, -.data$estimate, -.data$estimate1, -.data$estimate2)
  }
  results %>%
    set_attrs(args = args) %>%   # rstatix utilities
    add_class(c("rstatix_test", "dunn_test"))   # rstatix utilities
}


.dunn_test <- function(data, formula, p.adjust.method = "holm"){
  get_ties <- function(x, n) {
    x.sorted <- sort(x)
    pos <- 1
    tiesum <- 0
    while (pos <= n) {
      val <- x.sorted[pos]
      nt <- length(x.sorted[x.sorted == val])
      pos <- pos + nt
      if (nt > 1){
        tiesum <- tiesum + nt^3  - nt
      }
    }
    tiesum / (12 * (n - 1))
  }
  
  
  outcome <- get_formula_left_hand_side(formula)   # rstatix utilities
  group <- get_formula_right_hand_side(formula)   # rstatix utilities
  number.of.groups <- guess_number_of_groups(data, group)   # rstatix utilities
  if(number.of.groups == 1){
    stop("all observations are in the same group")
  }
  
  data <- data %>%
    dplyr::select(!!!syms(c(outcome, group))) %>%
    get_complete_cases() %>%   # rstatix utilities
    .as_factor(group)
  
  x <- data %>% pull(!!outcome)
  g <- data %>% pull(!!group)
  group.size <- data %>% get_group_size(group)   # rstatix utilities
  if (!all(is.finite(g)))
    stop("all group levels must be finite")
  
  x.rank <- rank(x)
  mean.ranks <- tapply(x.rank, g, mean, na.rm=TRUE)
  sd.ranks <- tapply(x.rank, g, sd, na.rm=TRUE)
  sem.ranks <- tapply(x.rank, g, sem)
  median.ranks <- tapply(x.rank, g, FUN = function(x) quantile(x, 0.5, na.rm = TRUE))
  q25.ranks <- tapply(x.rank, g, FUN = function(x) quantile(x, 0.25, na.rm = TRUE))
  q75.ranks <- tapply(x.rank, g, FUN = function(x) quantile(x, 0.75, na.rm = TRUE))
  grp.sizes <- tapply(x, g, length)
  n <- length(x)
  C <- get_ties(x.rank, n)
  
  compare.meanrank <- function(i, j){
    mean.ranks[i] - mean.ranks[j]
  }
  compare.stats <- function(i,j) {
    dif <- mean.ranks[i] - mean.ranks[j]
    A <- n * (n+1) / 12
    B <- (1 / grp.sizes[i] + 1 / grp.sizes[j])
    zval <- dif / sqrt((A - C) * B)
    zval
  }
  compare.levels <- function(i, j) {
    dif <- abs(mean.ranks[i] - mean.ranks[j])
    A <- n * (n+1) / 12
    B <- (1 / grp.sizes[i] + 1 / grp.sizes[j])
    zval <- dif / sqrt((A - C) * B)
    pval <- 2 * stats::pnorm(abs(zval), lower.tail = FALSE)
    pval
  }
  ESTIMATE <- stats::pairwise.table(
    compare.meanrank, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("diff")   # rstatix utilities
  
  PSTAT <- stats::pairwise.table(
    compare.stats, levels(g),
    p.adjust.method = "none"
  ) %>% tidy_squared_matrix("statistic")   # rstatix utilities
  
  PVAL <- stats::pairwise.table(
    compare.levels, levels(g),
    p.adjust.method = "none"
  ) %>%
    tidy_squared_matrix("p") %>%   # rstatix utilities
    dplyr::mutate(method = "Dunn Test", .y. = outcome) %>%
    adjust_pvalue(method = p.adjust.method) %>%   # rstatix utilities
    add_significance("p.adj") %>%   # rstatix utilities
    tibble::add_column(statistic = PSTAT$statistic, .before = "p") %>%
    tibble::add_column(estimate = ESTIMATE$diff, .before = "group1") %>%
    dplyr::select(".y.", .data$group1, .data$group2, .data$estimate, everything())
  
  n1 <- group.size[PVAL$group1]
  n2 <- group.size[PVAL$group2]
  mean.ranks1 <- mean.ranks[PVAL$group1]
  mean.ranks2 <- mean.ranks[PVAL$group2]
  sd.ranks1 <- sd.ranks[PVAL$group1]
  sd.ranks2 <- sd.ranks[PVAL$group2]
  sem.ranks1 <- sem.ranks[PVAL$group1]
  sem.ranks2 <- sem.ranks[PVAL$group2]
  median.ranks1 <- median.ranks[PVAL$group1]
  median.ranks2 <- median.ranks[PVAL$group2]
  q25.ranks1 <- q25.ranks[PVAL$group1]
  q25.ranks2 <- q25.ranks[PVAL$group2]
  q75.ranks1 <- q75.ranks[PVAL$group1]
  q75.ranks2 <- q75.ranks[PVAL$group2]
  PVAL %>%
    tibble::add_column(n1 = n1, n2 = n2, .after = "group2") %>%
    tibble::add_column(estimate1 = mean.ranks1, estimate2 = mean.ranks2,
               sd1 = sd.ranks1, sd2 = sd.ranks2,
               sem1 = sem.ranks1, sem2 = sem.ranks2,
               median1 = median.ranks1, median2 = median.ranks2,
               q251 = q25.ranks1, q252 = q25.ranks2,
               q751 = q75.ranks1, q752 = q75.ranks2,
               .after = "estimate")
}

## Weighted --------------------------------------------------------------------


# Custom correlation matrix function (sensitive to use of polychoric, pearson correlations and weights)
flex_corr_mat <- function(df, wt_col, cont_cols, less_than_10levs_cols=NULL, factor_cols=NULL) {
  df <- df %>%
    dplyr::select(tidyselect::all_of(c(cont_cols, less_than_10levs_cols, factor_cols, wt_col))) %>%
    dplyr::relocate(tidyselect::all_of(factor_cols), .after = last_col()) %>%
    dplyr::relocate(tidyselect::all_of(wt_col), .after = last_col()) %>%
    labelled::unlabelled()
  
  # Shorten variable labels
  short_colnames <- shorten_colnames(colnames(df))
  
  # Initialize matrix to store corr coefficients
  corr_matrix <- matrix(nrow = ncol(df)-1, ncol = ncol(df)-1,
                        dimnames = list(colnames(df)[1:(ncol(df)-1)],
                                        short_colnames[1:(ncol(df)-1)]))
  
  setDT(df)  # Convert 'df' to a data.table in place
  for(j in 1:(ncol(df)-1)) {
    n_levels_y <- uniqueN(df[[j]])  # Check the number of levels of variable 'j'
    
    for(i in 1:(ncol(df)-1)) {
      n_levels_x <- uniqueN(df[[i]])  # Check the number of levels of variable 'i'
      
      if((i != j) & is.na(corr_matrix[j,i])){  # Fill empty cells (not diagonal)
        if(!any(grepl("simpl", colnames(df[,c(i,j)])))) {  # Avoid factor variables
          df <- as.data.frame(df)
          subset <- df[,c(colnames(df)[c(i,j)],wt_col)] %>% tidyr::drop_na()  # Select only variables 'i','j', and weights
          #print(c(i,", ",j))
          #print(colnames(subset))
          x <- unlist(subset[,1])  # Convert to vectors
          y <- unlist(subset[,2])
          wts <- unlist(subset[,3])
          # Determine correlation method based on levels
          method <- if (n_levels_x < 11 | n_levels_y < 11) "Polychoric" else "Pearson"
          # Compute corr coeff
          r <- wCorr::weightedCorr(x=x, y=y,
                            method = method,
                            weights = wts,
                            ML = FALSE, fast = TRUE)
          
          if(abs(r) == 1){
            r <- 0
          }
          
          # Store corr coeff in i,j and reflection across diagonal
          corr_matrix[i, j] <- r
          corr_matrix[j, i] <- r
        }
      }
    }
  }
  
  return (corr_matrix)
}

# Tables -----------------------------------------------------------------------
# Function to shorten column names
shorten_colnames <- function(colnames_vector) {
  sapply(colnames_vector, function(colname) {
    if (nchar(colname) > 40) {
      # Split the column name by underscore
      parts <- stringr::str_split(colname, "_", simplify = TRUE)
      
      # Select the parts following the second to last underscore
      if (ncol(parts) > 1) {  # Ensure there are at least two parts
        short_name <- paste(parts[(ncol(parts)-2):ncol(parts)], collapse = "_")
      } else {
        short_name <- colname  # Use the original name if it can't be split
      }
      
      return(short_name)
    } else {
      return(colname)  # Return the original name if it's not longer than 40 chars
    }
  })
}

makeStars <- function(x, keep_val = FALSE) {
  stars <- c("***", "**", "*", "`", "-")
  vec <- c(0, 0.001, 0.01, 0.05, 0.10, 1)
  i <- findInterval(x, vec)
  if (keep_val == TRUE){
    x <- paste(format(x, nsmall = 3), stars[i], sep = "")
  } else {
    x <- stars[i]
  }
}


# Function for default knitr table formatting
kbl_local_default <- function(df, title = NULL) {
  kbl(x = df, format = "html", align = "l", booktabs = TRUE,
      table.attr = "style='width:90%;'", digits = 3, caption = title) %>%
    kable_classic(full_width = FALSE, lightable_options = "striped",
                  html_font = "Cambria", latex_options = "scale_down")
}

# Function for default flextable formatting
flextable_local_default <- function(data, title = " ", footnote = NULL, ...) {
  # set_flextable_defaults()
  init_flextable_defaults()
  nrow <- nrow(data)#; print(nrow)
  ncol <- ncol(data)#; print(ncol)
  indices_not_header_not_bottom <- if(nrow > 1) 1:(nrow - 1) else NULL
  indices_not_left <- if(ncol > 1) 2:ncol else NULL
  
  set_flextable_defaults(font.family = "Source Sans Pro",
                         text.align = "center",
                         table_align = "center",
                         padding = 2)
  
  tbl <- flextable(data) |>
    flextable::bold(part = "header") |>
    flextable::align(align = "center", part = "header") |>
    flextable::set_caption(caption =
                             flextable::as_paragraph(
                               flextable::as_chunk(title, props = flextable::fp_text_default(
                      font.family = "Source Sans Pro", bold = TRUE, underlined = FALSE)
                    )),
                fp_p = officer::fp_par(text.align = "left")
    )|>
    flextable::border_inner_h(part="all", border = officer::fp_border(
      color="gray91", width = 1, style = "dotted")) |>
    flextable::border_inner_v(part="body", border = officer::fp_border(
      color="gray91", width = 1, style = "dotted")) |>
    flextable::colformat_double(big.mark = ",", digits = 3, na_str = "NA")
  
  if(!is.null(indices_not_left)){
    # print("inside1")
    tbl <- flextable::align(tbl, #i = 1:nrow,
                 j = indices_not_left, align = "center", part = "all") # Center align body (not left)
  }
  
  tbl <- flextable::align(tbl, #i = 1:nrow, j = 1:ncol,
               align = "left", part = "footer") # Left align footer 
  
  if(!is.null(footnote)){
    # print("inside2")
    tbl <- flextable::footnote(tbl, #i = 1,
                               j = 1, ref_symbols = "a",
                               value = as_paragraph(footnote))
  }
  
  # Adjustments to first column
  # Horizontal divider
  if(!is.null(indices_not_header_not_bottom)){
    # print("inside3")
    tbl <- flextable::hline(tbl, i=indices_not_header_not_bottom,
                 j=1, border=officer::fp_border(color="white", width=0), part="body")
  }
  
  tbl <- flextable::width(tbl, j = 1, width = 1.5) # Width (decides word wrapping)
  tbl <- flextable::align(tbl, j = 1, align = "left", part = "all") # Left alignment 
  
  return(tbl)
}

# Adapted from gtsummary::add_significance_stars
# add_significance_stars_q <- 
#   function (x, pattern = ifelse(inherits(x, c("tbl_regression", 
#                                               "tbl_uvregression")), "{estimate}{stars}", "{q.value}{stars}"), 
#             thresholds = c(0.001, 0.01, 0.05), hide_ci = TRUE, hide_p = inherits(x, 
#                                                                                  c("tbl_regression", "tbl_uvregression")), hide_se = FALSE) 
#   {
#     get_cli_abort_call()
#     updated_call_list <- c(x$call_list, list(add_significance_stars = match.call()))
#     check_not_missing(x)
#     check_class(x, "gtsummary")
#     check_class(thresholds, "numeric")
#     check_range(thresholds, range = c(0, 1), include_bounds = c(TRUE, 
#                                                                 TRUE))
#     check_scalar_logical(hide_ci)
#     check_scalar_logical(hide_p)
#     check_scalar_logical(hide_se)
#     if (!"q.value" %in% names(x$table_body)) {
#       cli::cli_abort("There is no p-value column in the table and significance stars cannot be placed.", 
#                      call = get_cli_abort_call())
#     }
#     thresholds <- unique(sort(thresholds, decreasing = TRUE))
#     pattern_cols <- .extract_glue_elements(pattern)
#     if (is_empty(pattern_cols)) {
#       cli::cli_abort("The {.arg pattern} argumnet must be a string using glue syntax to select columns.", 
#                      call = get_cli_abort_call())
#     }
#     if (!"stars" %in% pattern_cols) {
#       cli::cli_inform("The {.arg pattern} argument does not contain {.val {{stars}}} column, and no stars will be added.")
#     }
#     p_footnote <- paste(unlist(paste0(imap(thresholds, ~rep_len("*", 
#                                                                 .y) %>% paste(collapse = "")), "p<", thresholds)), collapse = "; ")
#     x <- modify_footnote(x, any_of(pattern_cols[1]) ~ p_footnote)
#     thresholds <- union(thresholds, 0L)
#     expr_stars_case_when <- parse_expr(map2(thresholds, seq_along(thresholds), 
#                                             ~expr_deparse(expr(q.value >= !!.x ~ !!paste(rep_len("*", 
#                                                                                                  .y - 1), collapse = "")))) %>% reduce(.f = function(.x, 
#                                                                                                                                                      .y) paste(.x, .y, sep = ", ")) %>% {
#                                                                                                                                                        paste0("dplyr::case_when(is.na(q.value) ~ '', ", ., ")")
#                                                                                                                                                      })
#     x <- modify_table_body(x, ~dplyr::mutate(.x, stars = !!expr_stars_case_when))
#     cols_to_hide <- c(conf.low = hide_ci, q.value = hide_p, std.error = hide_se)
#     cols_to_hide <- cols_to_hide[c("conf.low", "q.value", "std.error") %in% 
#                                    names(x$table_body)]
#     x <- modify_table_styling(x, columns = all_of(names(cols_to_hide)), 
#                               hide = cols_to_hide)
#     x <- modify_column_merge(x, rows = !is.na(.data$q.value), 
#                              pattern = pattern)
#     x <- .fill_table_header_modify_stats(x)
#     x$call_list <- updated_call_list
#     x
#   }

# Adpated from gtsummary::bold_p
# bold_asterisk <- function(x) {
#   # Update the call list with the new function call
#   updated_call_list <- c(x$call_list, list(bold_asterisk = match.call()))
#   
#   # Identify columns that contain an asterisk
#   cols_with_asterisks <- x$table_body %>%
#     select(where(~ any(grepl("\\*", .))))
#   
#   # If no columns with asterisks are found, return the object unmodified
#   if (ncol(cols_with_asterisks) == 0) {
#      return(x)
#   }
#   
#   # Apply bold formatting to cells containing an asterisk
#   for (col in colnames(cols_with_asterisks)) {
#     x <- modify_table_styling(
#       x,
#       columns = all_of(col),
#       rows = grepl("\\*", x$table_body[[col]]),
#       text_format = "bold"
#     )
#   }
#   
#   x <- modify_table_styling(x,
#                             columns = all_of(colnames(cols_with_asterisks)), 
#                             rows = !!expr(!!sym(colnames(cols_with_asterisks)) <= !!t),
#                             text_format = "bold")
#   
#   # Update the call list
#   x$call_list <- updated_call_list
#   x
# }

# Function for returning gtsummary::tbl_regression formatted coefficient labels
gt_coef_labs <- function(mod, delete_prefix = FALSE, intercept = FALSE){
  vars <- as.character(attr(mod$terms, "predvars"))
  vars <- vars[vars != "list"]
  
  old_labs <- names(mod$coefficients)
  labs <- stringr::str_remove_all(old_labs, '_txt')
  labs <- stringr::str_replace_all(labs, fixed('.L'), ' - Linear')
  labs <- stringr::str_replace_all(labs, fixed('.Q'), ' - Quadratic')
  labs <- stringr::str_replace_all(labs, fixed('.C'), ' - Cubic')
  
  if(delete_prefix == TRUE){
    for(var in vars){
      if(length(grep(var, labs)) > 1){
        labs <- stringr::str_remove_all(labs, var) 
      }
    }
  }
  
  labs <- stringr::str_remove_all(labs, '[0-9][0-9][0-9]pcntFPL')
  labs <- stringr::str_replace_all(labs, fixed('>'), fixed('\\>'))
  labs <- stringr::str_replace_all(labs, fixed('='), fixed('\\='))
  labs <- stringr::str_replace_all(labs, fixed('%'), fixed('\\%'))
  labs <- stringr::str_to_title(stringr::str_replace_all(labs, '_', ' '))
  
  labs2 <- purrr::map2(labs, old_labs, \(new_lab, old_lab){
    # formula_elem <- paste0(old_lab, '~ "', new_lab, '"')
    formula_elem <- as.formula(paste0(old_lab, '~ "', new_lab, '"'),
                               env = .GlobalEnv)
    return(formula_elem)
    # as.formula(paste0("`", name, "` ~ `", new_name, "`"))
  })
  
  if (intercept == FALSE){
    labs <- labs[-1]
  }
  
  return(labs)
}


gt_coef_labs2 <- function(x, ...) {
  
  # italicize section
  var_labs <- x$table_body$var_label
  val_labs <- x$table_body$label
  type <- x$table_body$row_type
  
  data <- data.frame(var_labs = x$table_body$var_label,
                     var_labs_names = names(x$table_body$var_label),
                     val_labs = x$table_body$label,
                     val_labs_names = names(x$table_body$label),
                     row_type = x$table_body$row_type)
  
  data <- data %>%
    dplyr::mutate(row = row_number()#,
           # not_footer = ifelse(type != "glance_statistic", 1, 0),
           # unq_var_name = ifelse(!duplicated(var_labs_names), 1, 0),
           # complete_var_lab = ifelse(!is.na(var_labs), 1, 0)
           ) %>%
    dplyr::mutate(var_labs = case_when(nchar(var_labs) > 50 |
                                stringr::str_detect(var_labs, '\\?') ~ var_labs_names,
                                TRUE ~ var_labs)) %>%
    dplyr::mutate(var_labs = case_when(stringr::str_detect(var_labs, '_') ~ 
                                stringr::str_to_title(
                                  stringr::str_replace_all(
                                    stringr::str_remove_all(var_labs, '_txt'),
                                  '_', ' ')),
                                TRUE ~ var_labs)) %>%
    dplyr::mutate(val_labs = stringr::str_replace_all(val_labs, "\\.L$", ' - Linear')) %>%
    dplyr::mutate(val_labs = stringr::str_replace_all(val_labs, "\\.Q$", ' - Quadratic')) %>%
    dplyr::mutate(val_labs = stringr::str_replace_all(val_labs, "\\.C$", ' - Cubic')) %>%
    dplyr::group_by(var_labs_names) %>%
      dplyr::mutate(var_n_rows = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(val_labs = case_when(
      var_n_rows == 1 | var_labs_names == "" ~ var_labs,
      TRUE ~ val_labs)) %>%
    dplyr::mutate(val_labs = case_when(stringr::str_detect(val_labs, var_labs_names) ~
                                stringr::str_replace_all(val_labs, var_labs_names, var_labs),
                                TRUE ~ val_labs))
    
  
  
  x$table_body$var_label <- data$var_labs
  x$table_body$label <- data$val_labs
  
  return(x)
}


writeTable <- function(html_table, out_path) {
  Sys.which("pandoc")
  temp_html <- tempfile(fileext = ".html")
  writeLines(html_table, temp_html)
  output_docx <- paste0(tables_path, out_path)
  cmd <- sprintf('"%s" "%s" -o "%s"', Sys.which("pandoc"), temp_html, output_docx)
  system(cmd)
  unlink(temp_html)
}

# Visuals ----------------------------------------------------------------------
# Function to create a continuous color scale from a Brewer palette
# blues_gradient <- distiller_local("Blues", from = 0, to = 0.7, length.out = 100)
# scales::show_col(blues_gradient)
distiller_local <- function(palette, from = 0, to = 1, length.out = 100) {
  brewer_colors <- scales::brewer_pal(palette = palette)(7)
  palette_aux <- scales::pal_gradient_n(brewer_colors, NULL, "Lab")
  seq_range <- seq(from, to, length.out = length.out)
  # scales::show_col(distiller_local(seq_range))
  
  return(palette_aux(seq_range))
}



gg_density_vector <- function(vec, title = NULL) {
  ggplot2::ggplot() +
    ggplot2::geom_density(aes(vec), color = "deepskyblue2", linewidth = 0.7) +
    ggplot2::geom_vline(aes(xintercept=mean(vec, na.rm = T), linetype="solid"), linewidth=0.5) +
    ggplot2::geom_vline(aes(xintercept=quantile(vec, 0.5, na.rm = T), linetype="dashed"), linewidth=0.5) +
    ggplot2::xlim(min(vec, na.rm = T)-3*sd(vec, na.rm = T),
         max(vec, na.rm = T)+3*sd(vec, na.rm = T)) +
    ggplot2::scale_linetype_manual(name = "Central Tendency", values = c("solid", "dashed"), labels = c("Mean", "Median")) +
    ggplot2::labs(title=title, x = NULL, y = "Density",
         caption = paste0("N: ", length(vec),
                          ",   Mean: ", round(mean(vec, na.rm=T),3),
                          ",   Median: ", round(quantile(vec, 0.5, na.rm = T),3),
                          ",   SD: ", round(sd(vec, na.rm = T),3))) +
    ggplot2::theme(legend.position = 'right',
          legend.spacing.y = unit(10, 'cm'))
}

# Flat violin
# Credit: https://stackoverflow.com/questions/52034747/plot-only-one-side-half-of-the-violin-plot
# Ex.
# ggplot(dframe, aes(group, val)) +
#   geom_flat_violin()

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}

geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x - width / 2,
                     xmax = x)
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, 
                              xmaxv = x,
                              xminv = x + violinwidth * (xmin - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", linewidth = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )

# Summarizes a data frame without weights
prep_plot_df_unwgtd <- function(data, main_var, group_vars) {
  tagged_na_levels <- c("DON'T KNOW","I do not know",
                        "SKIPPED ON WEB",
                        "refused", "Refused")
  
  if (!missing(group_vars)) {
    
    data <- data %>%
      tidyr::drop_na(tidyselect::all_of(c(main_var, group_vars))) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::select(main_var, tidyselect::all_of(group_vars)) %>%
      # Use across with all_of for dynamic grouping if multiple columns are provided
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) %>%
      dplyr::mutate(N = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_vars, main_var)))) %>%
      dplyr::reframe(n_unwgtd = dplyr::n(),
              N_unwgtd = mean(N),
              prcnt = round(100 * n_unwgtd / N_unwgtd, 2)) %>%
      dplyr::arrange(dplyr::across(tidyselect::all_of(c(group_vars, main_var)))) %>%
      # Convert group_vars to factors
      dplyr::mutate(dplyr::across(tidyselect::all_of(group_vars), as.factor))
    
    # print(colnames(data))
  } else {
    data <- data %>%
      tidyr::drop_na(main_var) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::mutate(N = nrow(.)) %>%
      dplyr::arrange(dplyr::across(main_var)) %>%
      dplyr::group_by(dplyr::across(main_var)) %>%
      dplyr::reframe(n_unwgtd = dplyr::n(),
              N_unwgtd = mean(N),
              prcnt = round(100 * n_unwgtd / N_unwgtd, 2))
  }
  
  # Ensure data is returned at the end of the function
  return(data)
}

# Summarizes a data frame with weights
prep_plot_df_wgtd <- function(dsgn=NULL, unwgtd_data=NULL, main_var, group_vars = NULL) {
  
  
  tagged_na_levels <- c("DON'T KNOW","I do not know",
                        "SKIPPED ON WEB",
                        "refused", "Refused")
  
  wgtd_chi <- NULL
  wgtd_logit <- NULL
  main_var_num <- stringr::str_remove(main_var, '_txt')
  if(!is.null(unwgtd_data) & (main_var_num == main_var)){
    unwgtd_data <- unwgtd_data %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(main_var), ~as.numeric(as.factor(.x)) - 1, .names = "{.col}_num"))
    
    main_var_num <- paste0(main_var, "_num")
    main_var_levs <- unique(unwgtd_data[[main_var_num]])
    main_var_levs <- main_var_levs[!is.na(main_var_levs)]
    
    unwgtd_data <- unwgtd_data %>%
      dplyr::mutate(fpc = ifelse(fpl_flag==1, 10e7, 24e7),
             poverty_status_200pcntFPL_txt = fpl_flag_txt)
    
    dsgn <- survey::svydesign(ids = ~1,
                      strata = ~fpl_flag_txt,
                      weights = ~weight1,
                      fpc = ~fpc,
                      data = unwgtd_data)
  } else {
    main_var_levs <- unique(dsgn$variables[[main_var_num]])
    main_var_levs <- main_var_levs[!is.na(main_var_levs)]
    # if(!is.numeric(main_var_levs)){main_var_levs <- as.numeric(main_var_levs)-1}
  }
  
  if (all(main_var_levs >= 0 & main_var_levs <= 1)) {
    glm_family <- quasibinomial(link = "logit")
  } else {
    glm_family <- gaussian()
  }
  
  # # Define Survey Design ####
  # if(is.null(dsgn) & !is.null(unwgtd_data)){
  #   unwgtd_data <- unwgtd_data %>%
  #     dplyr::mutate(fpc = ifelse(fpl_flag==1, 10e7, 24e7),
  #            poverty_status_200pcntFPL_txt = fpl_flag_txt)
  #   
  #   dsgn <- survey::svydesign(ids = ~1,
  #                     strata = ~fpl_flag_txt,
  #                     weights = ~weight1,
  #                     fpc = ~fpc,
  #                     data = unwgtd_data)
  # }
  
  # Filter out NAs of specified variables, start with main_var
  filter_na_condition <- sprintf("!is.na(%s)", main_var)
  # dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
  
  
  if(missing(group_vars) | is.null(group_vars)){
    # Use eval and parse to apply the condition to subset the svydesign object
    dsgn.nomiss <- subset(dsgn, eval(parse(text = filter_na_condition)), envir = as.list(dsgn$variables))
    
    # Directly construct the formula
    formula.table <- reformulate(termlabels = main_var)#; cat(as.character(formula.table, "\n")) 
    formula.mean <- reformulate(termlabels = main_var, response = NULL)#; cat(as.character(formula.mean, "\n"))
    formula.glm <- reformulate(response = main_var_num, termlabels = "1")#; cat(as.character(formula.glm, "\n"))
    
    # wgtd_logit table
    tryCatchLog::tryCatchLog({
      
      wgtd_logit <- tidyr::tidy(survey::svyglm(formula.glm, design = dsgn.nomiss, family = gaussian())) %>%
        dplyr::mutate(`P BH` = p.adjust(p.value, "BH"),
                      dplyr::across(tidyselect::where(is.numeric), ~round(., 3)),
                      term = stringr::str_remove_all(term, main_var_num))
      colnames(wgtd_logit) <- stringr::str_to_title(stringr::str_replace_all(colnames(wgtd_logit), '\\.', ' '))
      
    }, error=function(e) {
      message(paste('plot_stacked (grouped) | Error in processing', main_var, ":", e$message))
      return(NULL)  # Return NULL or a default value on error
    })
    
  } else {
    # If present, filter out NAs in group_vars
    for (x in group_vars) {
      filter_na_condition <- sprintf("%s & !is.na(%s)", filter_na_condition, x)
    }
    # Use eval and parse to apply the condition to subset the svydesign object
    dsgn.nomiss <- subset(dsgn, eval(parse(text = filter_na_condition)), envir = as.list(dsgn$variables))
    
    # Directly construct the formula
    formula.table <- reformulate(termlabels = c(main_var, group_vars))#; cat(as.character(formula.table, "\n"))
    formula.mean <- reformulate(termlabels = group_vars, response = NULL)#; cat(as.character(formula.mean, "\n"))
    formula.chisq <- reformulate(termlabels = c(main_var, group_vars[1]))#; cat(as.character(formula.chisq, "\n"))
    formula.glm <- reformulate(response = main_var_num, termlabels = group_vars)#; cat(as.character(formula.glm, "\n"))
    
    # wgtd_chi caption
    # Function to capture and handle survey::svychisq output
    # (needed because tryCatchLog is not compatible with capture.output())
    capture_svychisq <- function(formula, design) {
      tryCatch({
        output <- capture.output(survey::svychisq(formula, design))[5]
        return(output)
      }, error = function(e) {
        message(paste('plot_stacked (grouped) | Error in svychisq:', e$message))
        return(NULL)  # Return NULL or handle the error in some other way
      })
    }
    
    # Using the function to get svychisq output and capture its specific line
    wgtd_chi <- capture_svychisq(formula.chisq, dsgn.nomiss)
    
    # wgtd_logit table
    tryCatchLog::tryCatchLog({
      
      wgtd_logit <- tidyr::tidy(survey::svyglm(formula.glm, design = dsgn.nomiss, family = glm_family)) %>%
        dplyr::mutate(`P BH` = p.adjust(p.value, "BH"),
                      dplyr::across(tidyselect::where(is.numeric), ~round(., 3)),
                      term = stringr::str_remove_all(term, main_var_num))
      colnames(wgtd_logit) <- stringr::str_to_title(stringr::str_replace_all(colnames(wgtd_logit), '\\.', ' '))
      
    }, error=function(e) {
      message(paste('plot_stacked (grouped) | Error in processing', main_var, ":", e$message))
      return(NULL)  # Return NULL or a default value on error
    })
  }
  
  if(missing(group_vars) | is.null(group_vars)){
    # Use do.call to programmatically call svyby with constructed arguments
    aux_count <- do.call("svytable", list(
      formula = formula.table, design = quote(dsgn.nomiss))) %>%
      as.data.frame(.) %>%
      dplyr::rename(n = Freq) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~factor(., levels = levels(.), ordered = TRUE)))
    # dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) %>%
    #   mutate(N = sum(n)) %>%
    #   mutate(prcnt2 = n/N) %>%
    # dplyr::ungroup() %>%
    # dplyr::arrange(dplyr::across(tidyselect::all_of(main_var)))
  } else {
    # Use do.call to programmatically call svyby with constructed arguments
    aux_count <- do.call("svytable", list(
      formula = formula.table, design = quote(dsgn.nomiss))) %>%
      as.data.frame(.) %>%
      dplyr::rename(n = Freq) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(group_vars))) %>%
      dplyr::mutate(N = sum(n)) %>%
      dplyr::mutate(prcnt2 = n/N) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~factor(., levels = levels(.), ordered = TRUE)))
    # dplyr::arrange(dplyr::across(tidyselect::all_of(c(rev(group_vars), main_var))))
  }
  
  
  if(missing(group_vars) | is.null(group_vars)){
    aux_prop <- do.call("svymean", list(
      x = formula.mean, design = quote(dsgn.nomiss))) %>%
      as.data.frame(.) %>%
      dplyr::rename_with(.cols = c(1,2), ~paste0(c("prcnt", "SE"))) %>%
      rownames_to_column(main_var) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(main_var),
                    ~gsub(main_var, "", .x, fixed = T))) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::mutate(prcnt = prcnt*100) %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.factor), ~factor(., levels = levels(.), ordered = TRUE)))
    # dplyr::arrange(dplyr::across(tidyselect::all_of(main_var))) %>%
    # dplyr::rename_with(.col = -tidyselect::all_of(c("SE", "prcnt")),
    #           .fn = function(x){paste0(x, "_prcnt")})
    if(!is.null(levels(dsgn$variables[[main_var]]))){
      aux_prop[[main_var]] <- factor(aux_prop[[main_var]],
                                     levels = levels(dsgn$variables[[main_var]]),
                                     ordered = TRUE)
    }
  } else {
    aux_prop <- do.call("svyby", list(
      formula = as.formula(paste0("~as.factor(",main_var,")")),
      by = formula.mean,
      design = quote(dsgn.nomiss),
      FUN = quote(survey::svymean),
      na.rm = TRUE#,
      #vartype = list("cv") # 'vartype' expects a list if specifying more than one type
    )) %>%
      tidyr::pivot_longer(cols = tidyselect::contains(substr(main_var, 0,15)) & tidyselect::where(is.numeric),
                          names_to = "stat.mainVarLev") %>%
      tidyr::separate_wider_delim(stat.mainVarLev, paste0("as.factor(",main_var,")"),
                                  names = c("stat", main_var)) %>%
      dplyr::filter(dplyr::if_any(main_var, ~!(.x %in% tagged_na_levels))) %>%
      dplyr::mutate(stat = case_match(stat, ""~"prcnt", .default="se")) %>%
      tidyr::pivot_wider(names_from = stat, values_from = value) %>%
      dplyr::mutate(prcnt = prcnt*100)
    # dplyr::arrange(dplyr::across(tidyselect::all_of(c(rev(group_vars), main_var)))) #%>%
    # dplyr::rename_with(.col = -tidyselect::all_of(c("se", "prcnt")),
    #             .fn = function(x){paste0(x, "_prcnt")})
    for (x in c(main_var, group_vars)){
      if(!is.null(levels(dsgn$variables[[x]]))){
        aux_prop[[x]] <- factor(aux_prop[[x]], levels = levels(dsgn$variables[[x]]), ordered = TRUE)
      }
    }
  }
  
  
  # data <- bind_cols(aux_prop, #%>% dplyr::select(-tidyselect::ends_with("_prcnt")),
  #                 aux_count) #%>%
  # if("educ5_txt" %in% group_vars){
  #   aux_count[["educ5_txt"]] <- ordered(aux_count[["educ5_txt"]], levels(aux_count[["educ5_txt"]]))
  #   print("educ5 group_var")
  # }
  
  # wgtd_chi <- NULL
  
  data <- dplyr::left_join(aux_prop, aux_count, by = c(group_vars, main_var)) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c(group_vars)))) %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(main_var))) %>%
    dplyr::mutate(N = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across(tidyselect::all_of(c(rev(group_vars), main_var)))) %>%
    dplyr::mutate(dplyr::across(c(n,N,prcnt), ~round(.x, 2))) %>%
    dplyr::relocate(tidyselect::ends_with("_txt")) %>%
    dplyr::relocate(N,prcnt, .after = n) %>%
    dplyr::mutate(wgtd_chi = NA,
                  wgtd_logit = NA) %>%
    tidyr::unite("join", tidyselect::all_of(c(main_var, group_vars)), remove = FALSE)
  data$wgtd_chi[1] <- list(wgtd_chi)
  data$wgtd_logit[1] <- list(wgtd_logit)
  
  unwgtd <- prep_plot_df_unwgtd(data = df, main_var = main_var, group_vars = group_vars)
  unwgtd <- unwgtd %>% dplyr::rename(prcnt_unwgtd = prcnt) %>%
    tidyr::unite("join", tidyselect::all_of(c(main_var, group_vars)), remove = TRUE)
  data <- dplyr::left_join(data, unwgtd, by = "join", keep = FALSE) %>% dplyr::select(-join)
  
  return(data)
}

# Function to calculate density at median for each group
density_at_median <- function(data, wgtd_median, wgts) {
  density_sum <- sum(density(data[[var]], weights = data[[wgts]])$y, na.rm = T)
  index <- which.min(abs(density(data[[var]], weights = data[[wgts]])$x - wgtd_median))
  density_at_median <- density(data[[var]], weights = data[[wgts]])$y[index]
  density_at_median_scaled <- 100*density_at_median / density_sum
  return(density_at_median_scaled)
}

# density_at_median <- function(data, wgtd_median, wgts) {
#   group_wgts_sum <- sum(data[[wgts]], na.rm = T)
#   density_sum <- sum(density(data[[var]], weights = (data[[wgts]]/group_wgts_sum))$y, na.rm = T)
#   index <- which.min(abs(density(data[[var]], weights = (data[[wgts]]/group_wgts_sum))$x - wgtd_median))
#   density_at_median <- density(data[[var]], weights = (data[[wgts]]/group_wgts_sum))$y[index]
#   density_at_median_scaled <- density_at_median / density_sum
#   return(density_at_median_scaled)
# }

dens_at_mean <- function(data, wgtd_median, wgts)
{
  # Copied from a StackOverflow solution by whuber:
  #http://stats.stackexchange.com/questions/32093/
  #how-to-draw-mean-median-and-mode-lines-in-r-that-end-at-density
  dens <- density(data[[var]], weights = data[[wgts]])
  n <- length(dens$y)
  dx <- mean(diff(dens$x))                  # Typical spacing in x
  y.unit <- sum(dens$y) * dx                # Check: this should integrate to 1
  dx <- dx / y.unit                         # Make a minor adjustment
  y.est <- dens$y[length(dens$x[dens$x < wgtd_median])]
  return(y.est)
}


measure_text_size <- function(txt, gp = gpar(), to = "in") {
  if (is.grob(txt)) {
    grobs <- lapply(seq_along(txt$label), function(i) {
      g <- txt
      # Subset grob per label
      g$label <- g$label[[i]]
      g$gp[]  <- lapply(g$gp, function(x) {x[pmin(i, length(x))]})
      g$rot   <- g$rot[pmin(i, length(g$rot))]
      g
    })
  } else {
    grobs <- lapply(txt, function(t) textGrob(t, gp = gp))
  }
  
  heights <- do.call(unit.c, lapply(grobs, grobHeight))
  widths  <- do.call(unit.c, lapply(grobs, grobWidth))
  
  cbind(
    height = grid::convertHeight(heights, to, valueOnly = TRUE),
    width = grid::convertWidth(widths,   to, valueOnly = TRUE)
  )
}
# measure_text_size(survey_item_txt)

# Place a custom annotation using normalized plot coordinates
annotate_npc <- function(label, x, y, ...)
{
  ggplot2::annotation_custom(grid::textGrob(
    x = unit(x, "npc"), y = unit(y, "npc"), label = label, ...))
}


# ggplot Stacked Bar Charts by grouping variables
# Referenced by 'plot_stacked'
plot_stacked_pooled <- function(data, main_var,
                                x_labs = NULL,
                                fill_vals = "Blues", fill_title = NULL,
                                pattern = FALSE,
                                aspect.ratio = 1/0.6, text_theme = NULL,
                                title = NULL, subtitle = NULL,
                                x_title = NULL,
                                caption = NULL,
                                table = NULL, table_caption = NULL) {
  
  # # Fallback or handling for non-grouped/pooled main variable
  # if(grepl("educ|behavior", main_var)){
  #   data[[main_var]] <- ordered(data[[main_var]], rev(levels(data[[main_var]])))
  #   # print("factor levels reversed for plotting")
  # }
  
  main_var_levs <- unique(data[[main_var]])[!is.na(unique(data[[main_var]]))]
  main_var_levs_n <- length(main_var_levs)
  mid_index <- ceiling(main_var_levs_n / 2)
  pcnt_text_fill <- rev(c(rep("black", mid_index),
                          rep("white", main_var_levs_n - mid_index)))
  
  pattern_fill <- c(rep("gray50", main_var_levs_n - mid_index),
                    rep("gray90", mid_index))
  pattern_shapes <- c(4, 1, 3, 20, 16, 7)[1:main_var_levs_n]  # also pch 8
  
  fill_lev_labs <- lapply(rev(main_var_levs), wrap_long_lines, width = 12)
  
  
  if(pattern){
    p <- data %>% 
      ggplot2::ggplot() +
       ggpattern::geom_col_pattern(aes(x = factor(1),
                            y = prcnt,
                            fill = forcats::fct_rev(.data[[main_var]]),
                            pattern_shape = .data[[main_var]],
                            pattern_fill = .data[[main_var]],
                            pattern_color = .data[[main_var]]
                            ),
                        pattern_spacing = 0.07,
                        pattern = 'pch',
                        pattern_density = 0.42,
                        pattern_alpha = 0.05,
                        pattern_angle = 315,
                        # pattern_size = 0.1,
                        pattern_key_scale_factor = 0.5,
                        color    = 'gray90', 
                        position = position_stack(reverse = FALSE)) +
      ggpattern::scale_pattern_shape_manual(values = pattern_shapes) +
      ggpattern::scale_pattern_colour_manual(values = rev(pattern_fill)) + 
      ggpattern::scale_pattern_fill_manual(values = rev(pattern_fill))
    
  } else {
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(aes(x = factor(1),
                   y = prcnt,
                   fill = forcats::fct_rev(.data[[main_var]])),
               position = position_stack(reverse = FALSE))
  }
  
  p <- p +
    # Percent labels
    # Layer for prcnt >= 6 with larger font size
    ggplot2::geom_label(aes(x = factor(1), y = prcnt,
                   label = ifelse(prcnt >= 6, paste0(round(prcnt,1), '%'), ""),
                   fill = forcats::fct_rev(.data[[main_var]]),
                   color = .data[[main_var]]), # match fill color to bars
               fontface = "bold", size = 6,
               vjust = 0,
               label.size = NA, # remove label border
               label.padding = unit(0.05, "lines"), # adjust padding inside the label
               label.r = unit(0.3, "lines"),
               position = position_stack(reverse = FALSE, vjust = 0.4),
               show.legend = FALSE) +
    # Layer for prcnt < 6 with smaller font size
    ggplot2::geom_label(aes(x = factor(1), y = prcnt,
                   label = ifelse(prcnt < 6, ifelse(prcnt > 2, paste0(round(prcnt,1), '%'), "-"), ""),
                   fill = forcats::fct_rev(.data[[main_var]]),
                   color = .data[[main_var]]), # match fill color to bars
               fontface = "bold", size = 2,
               vjust = 0,
               label.size = NA, # remove label border
               label.padding = unit(0.05, "lines"), # adjust padding inside the label
               label.r = unit(0.3, "lines"),
               position = position_stack(reverse = FALSE, vjust = 0.4),
               show.legend = FALSE) +
    # N
    ggplot2::geom_text(aes(x = factor(1), y = 100, label = paste0('N: ', N_unwgtd)),
              color = "black", vjust = -0.5, size = 6, position = position_identity()) +
    # Y = 0
    ggplot2::geom_hline(aes(yintercept = 0), linetype = 2) +
    # Median
    ggplot2::geom_hline(aes(yintercept = 50), linetype = 2, alpha = 0.2) +
    ggplot2::scale_y_continuous(expand = c(0, 0.04), limits = c(0,107)) +
    ggplot2::scale_color_manual(values = pcnt_text_fill) +
    ggplot2::scale_fill_manual(values = RColorBrewer::brewer.pal(main_var_levs_n + 1,
                                                        fill_vals)[2:(main_var_levs_n + 1)],
                      labels = fill_lev_labs,
                      name = wrap_long_lines(fill_title, 20)) +
    ggplot2::labs(title = wrap_long_lines(title, 60),
         subtitle = wrap_long_lines(subtitle, 60),
         x = NULL, y = "Median", caption = caption) +
    text_theme +
    ggplot2::theme(aspect.ratio = aspect.ratio,
          axis.title.y = element_text(angle = 0, vjust = 0.49999),
          axis.text.y = element_text(size = 12),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(hjust = 0, family = "TT Courier New"))
  
  if(pattern){
    p <- p +
      ggplot2::guides(
        fill = guide_legend(override.aes = list(pattern_shape = rev(pattern_shapes),
                                                pattern_color = pattern_fill,
                                                pattern_fill = pattern_fill),
                            byrow = TRUE),
        pattern_shape = "none",
        pattern_color = "none",
        pattern_fill = "none",
        color = "none"
      )
  } else {
    p <- p +
      ggplot2::guides(fill = guide_legend(byrow = TRUE),
             color = "none")
  }
  
  if (!is.null(x_labs) & !is.null(x_title)){
    p <- p +
      ggplot2::scale_x_discrete(labels = lapply(x_labs, wrap_long_lines, width = 6),
                       expand = c(0, 0),
                       name = x_title)
  } else if (!is.null(x_labs)) {
    p <- p +
      ggplot2::scale_x_discrete(labels = lapply(x_labs, wrap_long_lines, width = 6),
                       expand = c(0, 0))
  } else if (!is.null(x_labs)) {
    p <- p +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 7), #function(x) wrap_long_lines(x, width = 6),
                       expand = c(0, 0),
                       name = x_title)
  } else {
    p <- p +
      ggplot2::scale_x_discrete(labels = function(x) stringr::str_wrap(x, 7), #function(x) wrap_long_lines(x, width = 6),
                       expand = c(0, 0))
  }
  
  # Add the table as a grob if it's not NULL
  if(!is.null(table)) {
    table <- flextable_local_default(table)
    table <- flextable::add_footer_lines(table, values = table_caption)
    result <- list(p = p, table = table)
    class(result) <- "multiple_leaves"
    graphics.off()
    return(result)
  } else {
    # If no table is provided, just return the ggplot object
    result <- list(p = p, table = NULL)
    class(result) <- "multiple_leaves"
    graphics.off()
    return(result)
  }
}

# ggplot Stacked Bar Charts by grouping variables
# Referenced by 'plot_stacked'
plot_stacked_grouped <- function(data, main_var, group_vars = NULL,
                                 x_labs = NULL,
                                 fill_vals = "Blues", fill_title = NULL,
                                 pattern = FALSE,
                                 N_size = 6,
                                 aspect.ratio = 1/0.6, text_theme = NULL,
                                 title = NULL, subtitle = NULL,
                                 caption = NULL,
                                 x_title = NULL,
                                 table = NULL, table_caption = NULL) {
  
  # if(grepl("educ|behavior", main_var)){
  #   data[[main_var]] <- ordered(data[[main_var]], rev(levels(data[[main_var]])))
  #   # print("factor levels reversed for plotting")
  # }
  
  main_var_levs <- unique(data[[main_var]])[!is.na(unique(data[[main_var]]))]
  main_var_levs_n <- length(main_var_levs)
  mid_index <- ceiling(main_var_levs_n / 2)
  pcnt_text_fill <- rev(c(rep("black", mid_index),
                        rep("white", main_var_levs_n - mid_index)))
  
  pattern_fill <- c(rep("gray50", mid_index),
                    rep("gray90", main_var_levs_n - mid_index))
  pattern_shapes <- c(4, 1, 3, 20, 16, 7)[1:main_var_levs_n]  # also pch 8
  
  fill_lev_labs <- lapply(rev(main_var_levs), wrap_long_lines, width = 12)
  
  if(pattern){
    p <- data %>%
      ggplot2::ggplot() +
      ggpattern::geom_col_pattern(aes(x = .data[[group_vars[1]]],
                           y = prcnt,
                           fill = forcats::fct_rev(.data[[main_var]]),
                           pattern_shape = .data[[main_var]],
                           pattern_fill = .data[[main_var]],
                           pattern_color = .data[[main_var]]),
                        pattern_spacing = 0.03,
                        pattern = 'pch',
                        pattern_density = 0.45,
                        pattern_alpha = 0.05,
                        pattern_angle = 315,
                        # pattern_size = 0.1,
                        pattern_key_scale_factor = 0.8,
                        color    = 'gray90', 
                        position = position_stack(reverse = FALSE)) +
      ggpattern::scale_pattern_shape_manual(values = pattern_shapes) +
      ggpattern::scale_pattern_colour_manual(values = rev(pattern_fill)) + 
      ggpattern::scale_pattern_fill_manual(values = rev(pattern_fill))
    
  } else {
    p <- data %>%
      ggplot2::ggplot() +
      ggplot2::geom_col(mapping = aes(x = .data[[group_vars[1]]],
                             y = prcnt,
                             fill = forcats::fct_rev(.data[[main_var]])),
               position = position_stack(reverse = FALSE))
  }
  
  p <- p +
    # Percent labels
    # Layer for prcnt >= 6 with larger font size
    ggplot2::geom_label(aes(x = .data[[group_vars[1]]], y = prcnt,
                   label = ifelse(prcnt >= 6, paste0(round(prcnt,1), '%'), ""),
                   fill = forcats::fct_rev(.data[[main_var]]),
                   color = .data[[main_var]]), # match fill color to bars
               fontface = "bold", size = 6,
               vjust = 0,
               label.size = NA, # remove label border
               label.padding = unit(0.05, "lines"), # adjust padding inside the label
               label.r = unit(0.3, "lines"),
               position = position_stack(reverse = FALSE, vjust = 0.4),
               show.legend = FALSE) +
    # Layer for prcnt < 6 with smaller font size
    ggplot2::geom_label(aes(x = .data[[group_vars[1]]], y = prcnt,
                   label = ifelse(prcnt < 6, ifelse(prcnt > 2, paste0(round(prcnt,1), '%'), "-"), ""),
                   fill = forcats::fct_rev(.data[[main_var]]),
                   color = .data[[main_var]]), # match fill color to bars
               fontface = "bold", size = 2,
               vjust = 0,
               label.size = NA, # remove label border
               label.padding = unit(0.05, "lines"), # adjust padding inside the label
               label.r = unit(0.3, "lines"),
               position = position_stack(reverse = FALSE, vjust = 0.4),
               show.legend = FALSE) +
    # N
    ggplot2::geom_text(aes(x = .data[[group_vars[1]]], y = 100, label = paste0('N: ', N_unwgtd)),
              color = "black", vjust = -0.5, size = N_size, position = position_identity()) +
    # Y = 0
    ggplot2::geom_hline(aes(yintercept = 0), linetype = 2) +
    # Median
    ggplot2::geom_hline(aes(yintercept = 50), linetype = 2, alpha = 0.2) +
    ggplot:scale_y_continuous(expand = c(0, 0.04), limits = c(0,107)) +
    ggplot:scale_color_manual(values = pcnt_text_fill) +
    ggplot:scale_fill_manual(values = RColorBrewer::brewer.pal(main_var_levs_n + 1,
                                                        fill_vals)[2:(main_var_levs_n + 1)],
                      labels = fill_lev_labs,
                      name = wrap_long_lines(fill_title, 20)) +
    ggplot2::labs(title = wrap_long_lines(title, 60),
         subtitle = wrap_long_lines(subtitle, 60),
         y = "Median", caption = caption) +
    text_theme +
    ggplot2::theme(aspect.ratio = aspect.ratio,
          axis.title.y = element_text(angle = 0, vjust = 0.49999),
          axis.text.y = element_text(size = 14),
          panel.border = element_blank(),
          plot.caption = element_text(hjust = 0, family = "TT Courier New"))
  
  if(pattern){
    p <- p +
      ggplot2::guides(
        fill = guide_legend(override.aes = list(pattern_shape = rev(pattern_shapes),
                                                pattern_color = pattern_fill,
                                                pattern_fill = pattern_fill),
                            byrow = TRUE),
        pattern_shape = "none",
        pattern_color = "none",
        pattern_fill = "none",
        color = "none")
  } else {
    p <- p +
      ggplot2::guides(fill = guide_legend(byrow = TRUE),
             color = "none")
  }
  
  
  if (!is.null(x_labs) & !is.null(x_title)){
    p <- p +
      ggplot:scale_x_discrete(labels = lapply(x_labs, wrap_long_lines, width = 6),
                       expand = c(0, 0),
                       name = x_title)
  } else if (!is.null(x_labs)) {
    p <- p +
      ggplot:scale_x_discrete(labels = lapply(x_labs, wrap_long_lines, width = 6),
                       expand = c(0, 0),
                       name = NULL)
  } else if (!is.null(x_title)) {
    p <- p +
      ggplot:scale_x_discrete(labels = function(x) stringr::str_wrap(x, 7),#function(x) wrap_long_lines(x, width = 6),
                       expand = c(0, 0),
                       name = x_title)
  } else {
    p <- p +
      ggplot:scale_x_discrete(labels = function(x) stringr::str_wrap(x, 7), #function(x) wrap_long_lines(x, width = 6),
                       expand = c(0, 0),
                       name = NULL)
  }
  
  
  # Dynamically add facets if more than one grouping variable
  if (!is.null(group_vars) & length(group_vars) > 1) {
    p <- p + 
      ggplot2::labs(y = NULL) +
      facet_grid(cols = vars(as.factor(!!sym(group_vars[2]))))
  } # else if (!is.null(group_vars) & length(group_vars) == 1) {
  #   p <- p + 
  #     ggplot2::theme(axis.text.x = element_text(size = 14, vjust = 0, margin = margin(t = 0)))
  # }
  
  # Add the table as a flextable if it's not NULL
  if(!is.null(table)) {
    table <- flextable_local_default(table)
    table <- flextable::add_footer_lines(table, values = table_caption)
    result <- list(p = p, table = table)
    class(result) <- "multiple_leaves"
    graphics.off()
    return(result)
  } else {
    # If no table is provided, just return the ggplot object
    result <- list(p = p, table = NULL)
    class(result) <- "multiple_leaves"
    graphics.off()
    return(result)
  }
  
}

# ggplot Stacked Bar Charts (sensitive to grouping vars or pooled)
plot_stacked <- function(data, main_var, group_vars = NULL,
                         x_labs = NULL,
                         fill_vals = "Blues", fill_title = NULL,
                         pattern = FALSE,
                         aspect.ratio = 1/0.6, text_theme = NULL,
                         title = NULL, subtitle = NULL,
                         caption = NULL,
                         x_title = NULL,
                         table = NULL, table_caption = NULL) {

  # Dynamically construct the x aesthetic
  ## If main variable is stratified by grouping variables
  if (!is.null(group_vars) & length(group_vars) >= 1) {
    
    return(plot_stacked_grouped(data, main_var, group_vars,
                                x_labs,
                                fill_vals, fill_title,
                                pattern,
                                aspect.ratio, text_theme,
                                title, subtitle,
                                caption,
                                x_title,
                                table, table_caption))
    
    
  } else {
    return(plot_stacked_pooled(data, main_var,
                               x_labs,
                               fill_vals, fill_title,
                               pattern,
                               aspect.ratio, text_theme,
                               title, subtitle,
                               caption,
                               x_title,
                               table, table_caption))
  }
}

# ggplot Stacked Bar Charts
# plot_stacked <- function(data, main_var,group_vars = NULL,
#                          x_labs = NULL, fill_vals = NULL,
#                          aspect.ratio = 1/0.6, text_theme = NULL,
#                          title = NULL, caption = NULL, table = NULL) {
#   
#   if(grepl("educ|behavior", main_var)){
#     data[[main_var]] <- ordered(data[[main_var]], rev(levels(data[[main_var]])))
#     # print("factor levels reversed for plotting")
#   }
#   
#   main_var_levs <- unique(data[[main_var]])
#   main_var_levs <- main_var_levs[!is.na(main_var_levs)]
#   mid_index <- ceiling(length(main_var_levs) / 2)
#   pcnt_text_fill <- c(rep("black", mid_index),
#                       rep("white", length(main_var_levs) - mid_index))
#   
#   # Dynamically construct the x aesthetic
#   ## If main variable is stratified by grouping variables
#   if (!is.null(group_vars) & length(group_vars) >= 1) {
#     
#     p <- data %>%
#       ggplot2::ggplot() +
#       ggplot2::geom_col(mapping = aes(x = .data[[group_vars[1]]], y = prcnt, fill = .data[[main_var]]), position = position_stack()) +
#       ggplot2::geom_text(aes(x = .data[[group_vars[1]]], y = prcnt, label = paste0(prcnt, '%'), color = .data[[main_var]]),
#                 fontface = "bold", vjust = 0, position = position_stack(reverse = FALSE, vjust = 0.4)) +
#       ggplot2::geom_text(aes(x = .data[[group_vars[1]]], y = 100, label = paste0('N*: ', N)),
#                 color = "black", vjust = -0.5, size = 4, position = position_identity()) +
#       ggplot2::geom_hline(aes(yintercept = 0), linetype = 2) +
#       ggplot2::geom_hline(aes(yintercept = 50), linetype = 2, alpha = 0.2) +
#       ggplot:scale_color_manual(values = pcnt_text_fill) +
#       ggplot2::labs(title = title, y = "Median", caption = caption) +
#       ggplot2::guides(color = "none") +
#       text_theme +
#       ggplot2::theme(aspect.ratio = aspect.ratio,
#             axis.title.y = element_text(angle = 0, vjust = 0.49999),
#             axis.text.y = element_text(size = 12),
#             panel.border = element_blank(),
#             plot.caption = element_text(hjust = 0, family = "TT Courier New"))
#     
#     
#   } else {
#     # Fallback or handling for non-grouped/pooled main variable
#     p <- data %>%
#       ggplot2::ggplot() +
#       ggplot2::geom_col(mapping = aes(x = factor(1), y = prcnt, fill = .data[[main_var]]), position = position_stack()) +
#       ggplot2::geom_text(aes(x = factor(1), y = prcnt, label = paste0(prcnt, '%'), color = .data[[main_var]]),
#                 fontface = "bold", vjust = 0, position = position_stack(reverse = FALSE, vjust = 0.4)) + #vjust = 5
#       ggplot2::geom_text(aes(x = factor(1), y = 100, label = paste0('N*: ', N)),
#                 color = "black", vjust = -0.5, size = 5, position = position_identity()) +
#       ggplot2::geom_hline(aes(yintercept = 0), linetype = 2) +
#       ggplot2::geom_hline(aes(yintercept = 50), linetype = 2, alpha = 0.2) +
#       ggplot:scale_color_manual(values = pcnt_text_fill) +
#       ggplot2::labs(title = title, y = "Median", caption = caption) +
#       ggplot2::guides(color = "none") +
#       text_theme +
#       ggplot2::theme(aspect.ratio = aspect.ratio,
#             axis.title.y = element_text(angle = 0, vjust = 0.49999),
#             axis.text.y = element_text(size = 12),
#             panel.border = element_blank(),
#             plot.caption = element_text(hjust = 0, family = "TT Courier New"))
#   }
#   
#   
#   if (!missing(x_labs)){
#     p <- p +
#       ggplot:scale_x_discrete(labels = x_labs)
#   }
#   
#   if (!is.null(fill_vals)){
#     p <- p +
#       ggplot:scale_fill_manual(values = fill_vals, name = NULL)
#   }
#   
#   # Dynamically add facets if more than one grouping variable
#   if (!is.null(group_vars) & length(group_vars) > 1) {
#     p <- p + 
#       ggplot2::labs(y = NULL) +
#       ggplot2::theme(axis.text.x = element_text(size = 8)) +
#       facet_wrap(vars(as.factor(!!sym(group_vars[2]))))
#   } else if (!is.null(group_vars) & length(group_vars) == 1) {
#     p <- p + 
#       ggplot2::theme(axis.text.x = element_text(size = 8))
#   }
#   
#   # Add the table as a grob if it's not NULL
#   if(!is.null(table)) {
#     table <- flextable_local_default(table)
#     result <- list(p = p, table = table)
#     class(result) <- "multiple_leaves"
#     return(result)
#   } else {
#     # If no table is provided, just return the ggplot object
#     result <- list(p = p, table = NULL)
#     class(result) <- "multiple_leaves"
#     return(result)
#   }
# }

# Plot trendlines using ggpredict data frames
trend_pooled <-
  function(.df, .x, .y, .y_line_int, .y_lim_lo, .y_lim_hi,
           .x_lab, .y_lab, .title, .subtitle = "", .caption = "") {
    .df %>%
      ggplot2::ggplot(aes({{ .x }}, {{ .y }}, group = 1)) +
      ggplot2::geom_line(linewidth = 1.2, color = "black") +
      ggplot2::geom_point(size = 4, color = "black") +
      ggplot2::geom_hline(yintercept = {{ .y_line_int }}, color = "gray70", linetype = 2, linewidth = 1.2) +
      ggplot2::geom_errorbar(aes(ymin = conf.low, ymax = conf.high),
                    color = "black",
                    width = 0.1, position = position_dodge(0.01)
      ) +
      ggplot2::coord_cartesian(ylim = c(.y_lim_lo, .y_lim_hi)) +
      ggplot:scale_y_continuous(breaks = c(.y_lim_lo:.y_lim_hi)) +
      ggplot2::guides(color = "none") +
      text_theme +
      ggplot2::labs(
        x = .x_lab, y = .y_lab,
        title = .title,
        subtitle = .subtitle,
        caption = .caption
      )
  }

trendByCondition <-
  function(.df, .x, .y, .linetype_var = cond.cent, .y_line_int, .y_lim_lo, .y_lim_hi,
           .x_lab, .y_lab, .title, .subtitle = "", .caption = "") {
    .df %>%
      ggplot2::ggplot(aes({{ .x }}, {{ .y }}, group = {{ .linetype_var }})) +
      ggplot2::geom_line(aes(linetype = as.factor({{ .linetype_var }})), linewidth = 1.2) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_hline(yintercept = {{ .y_line_int }}, color = "gray70", linetype = 2, linewidth = 1.2) +
      ggplot2::geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(0.01)) +
      ggplot2::coord_cartesian(ylim = c(.y_lim_lo, .y_lim_hi)) +
      ggplot:scale_y_continuous(breaks = c(.y_lim_lo:.y_lim_hi)) +
      ggplot:scale_linetype_manual(
        values = c("solid", "dashed"),
        labels = c("Control", "Treatment")
      ) +
      text_theme +
      ggplot2::labs(
        x = .x_lab, y = .y_lab,
        title = .title,
        subtitle = .subtitle,
        caption = .caption,
        linetype = "Condition"
      )
  }

trendByPolitCat <-
  function(.df, .x, .y, .y_line_int, .y_lim_lo, .y_lim_hi,
           .x_lab, .y_lab, .title, .subtitle = "", .caption = "") {
    .df %>%
      ggplot2::ggplot(aes({{ .x }}, {{ .y }},
                 color = as.factor(polit.cat.cent),
                 group = as.factor(polit.cat.cent)
      )) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_hline(yintercept = {{ .y_line_int }}, color = "gray70", linetype = 2, linewidth = 1.2) +
      ggplot:scale_color_manual(
        values = c("dodgerblue3", "mediumpurple3", "firebrick2"),
        labels = c("Liberal", "Moderate", "Conservative")
      ) + #
      ggplot2::geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(0.01)) +
      ggplot2::coord_cartesian(ylim = c(.y_lim_lo, .y_lim_hi)) +
      ggplot:scale_y_continuous(breaks = c(.y_lim_lo:.y_lim_hi)) +
      text_theme +
      ggplot2::labs(
        x = .x_lab, y = .y_lab,
        title = .title,
        subtitle = .subtitle,
        caption = .caption,
        color = "Political Ideology", linetype = "Condition"
      )
  }

trendByPolitCat_ConditionInt <-
  function(.df, .x, .y, .linetype_var = cond.cent, .y_line_int, .y_lim_lo, .y_lim_hi,
           .x_lab, .y_lab, .title, .subtitle = "", .caption = "") {
    .df %>%
      ggplot2::ggplot(aes({{ .x }}, {{ .y }},
                 color = as.factor(polit.cat.cent),
                 group = interaction({{ .linetype_var }}, as.factor(polit.cat.cent))
      )) +
      ggplot2::geom_line(aes(linetype = as.factor({{ .linetype_var }})), linewidth = 1.2) +
      ggplot2::geom_point(size = 4) +
      ggplot2::geom_hline(yintercept = {{ .y_line_int }}, color = "gray70", linetype = 2, linewidth = 1.2) +
      ggplot:scale_color_manual(
        values = c("dodgerblue3", "mediumpurple3", "firebrick2"),
        labels = c("Liberal", "Moderate", "Conservative")
      ) + #
      ggplot2::geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1, position = position_dodge(0.01)) +
      ggplot2::coord_cartesian(ylim = c(.y_lim_lo, .y_lim_hi)) +
      ggplot:scale_y_continuous(breaks = c(.y_lim_lo:.y_lim_hi)) +
      ggplot:scale_linetype_manual(
        values = c("solid", "dashed"),
        labels = c("Control", "Treatment")
      ) +
      text_theme +
      ggplot2::labs(
        x = .x_lab, y = .y_lab,
        title = .title,
        subtitle = .subtitle,
        caption = .caption,
        color = "Political Ideology", linetype = "Condition"
      )
  }

# Quarto & Markdown Dynamic content --------------------------------------------
# Modified from qreport
# Helper function to maketabs
# addCap <- {
#   g <- function(tag1, tag2) {
#     r <- knitr::opts_current$get(tag1)
#     if (!length(r)) 
#       r <- knitr::opts_current$get(tag2)
#     r
#   }
#   h <- function() {
#     lab <- g("label")
#     if (length(lab) && !grepl("^fig-", lab)) 
#       lab <- paste0("fig-", lab)
#     lab
#   }
#   if (!length(label)) 
#     label <- h()
#   deb <- .Options$debugaddCap
#   deb <- length(deb) && deb
#   if (deb) 
#     cat("label:", label, "\n", file = "/tmp/z", append = TRUE)
#   if (!length(label)) 
#     return(invisible(list(NULL, NULL, NULL)))
#   if (is.logical(label) && !label) 
#     return(invisible(list(NULL, NULL, NULL)))
#   if (!length(cap)) 
#     cap <- g("fig.cap", "cap")
#   if (!length(scap)) 
#     scap <- g("fig.scap", "scap")
#   if (!length(cap) && length(scap)) 
#     cap <- scap
#   if (!length(scap) && length(cap)) 
#     scap <- cap
#   ge <- .GlobalEnv
#   if (!exists(".captions.")) 
#     assign(".captions.", NULL, envir = ge)
#   info <- data.frame(label = label, cap = cap, scap = scap)
#   if (deb) 
#     prn(info, file = "/tmp/z")
#   if (!length(.captions.) || label %nin% .captions.$label) 
#     assign(".captions.", rbind(.captions., info), envir = ge)
#   invisible(list(label = label, cap = cap, scap = scap))
# }
# print("addCap sourced")

makecodechunk <- function (cmd, opts = NULL, results = "asis", lang = "r", callout = NULL, 
          h = NULL, w = NULL) 
{
  if (!length(cmd) || (is.character(cmd) && length(cmd) == 
                       1 && cmd %in% c("", " ", "` `"))) 
    return("")
  r <- paste0("results=\"", results, "\"")
  if (length(opts)) 
    for (oname in names(opts)) {
      op <- opts[[oname]]
      if (is.character(op)) 
        op <- paste0("\"", op, "\"")
      r <- paste0(r, ",", oname, "=", op)
    }
  if (!exists(".chunknumber.")) 
    .chunknumber. <- 0
  .chunknumber. <- .chunknumber. + 1
  ge <- .GlobalEnv
  assign(".chunknumber.", .chunknumber., envir = ge)
  cname <- paste0("chnk", .chunknumber.)
  if (length(callout)) 
    callout <- paste("#|", callout)
  if (length(h)) 
    h <- paste("#| fig.height:", h)
  if (length(w)) 
    w <- paste("#| fig.width:", w)
  c("", if (lang == "r") paste0("```{r ", cname, ",", r, ",echo=FALSE}") else paste0("```{", 
                                                                                     lang, " ", cname, "}"), callout, cmd, h, w, "```", "")
}
# print("makecodechunk sourced")

maketabs <- function (..., wide = FALSE, cwidth = if (wide) "column-page", 
                      initblank = FALSE, baselabel = NULL, cap = NULL, basecap = NULL, 
                      parent_label = NULL, debug = FALSE) 
{
  en <- parent.frame()
  assign(envir = en, "caption", function(cap, label = NULL) list(label = label, 
                                                                 cap = cap))
  assign(envir = en, "fig.size", function(width = NULL, height = NULL, 
                                          column = NULL) list(width = width, height = height, column = column))
  fs <- list(...)
  if (length(fs) == 1 && "formula" %nin% class(fs[[1]])) {
    fs <- fs[[1]]
    ge <- .GlobalEnv
    assign(".fs.", fs, envir = ge)
  }
  if (!length(baselabel))
    baselabel <- knitr::opts_current$get("label")
  else if (baselabel == "none") 
    baselabel <- NULL
  if (length(baselabel) && !grepl("^fig-", baselabel))
    baselabel <- paste0("fig-", baselabel)
  yaml <- paste0(".panel-tabset", if (length(cwidth)) 
    paste0(" .", cwidth))
  k <- c("", paste0("::: {", yaml, "}"), "") 
  if (initblank) 
    k <- c(k, "", "##   ", "")
  for (i in 1:length(fs)) {
    label <- baselabel#; print(c("label1: ", label))
    capt <- NULL
    size <- NULL
    f <- fs[[i]]
    #(c("parent_label: ", parent_label))
    #parent_label <- stringr::str_remove(parent_label, '_txt')
    isform <- FALSE
    if ("formula" %in% class(f)) {
      isform <- TRUE
      capt <- NULL
      v <- as.character(attr(terms(f), "variables"))[-1]
      y <- v[1]
      y <- gsub("`", "", y)
      x <- v[-1]
      label <- paste0(label, '-', parent_label, '-', y, '-')#; print(c("label2 (formula): ", label))
      raw <- "raw" %in% x
      if (raw) 
        x <- setdiff(x, "raw")
      jc <- grep("caption\\(", x)
      if (length(jc)) {
        capt <- eval(parse(text = x[jc]), en)
        if (length(capt$label)) 
          label <- capt$label#; print(c("label3 (formula): ", label))
        capt <- capt$cap
        x <- x[-jc]
      }
      sz <- grep("fig.size\\(", x)
      if (length(sz)) {
        siz <- eval(parse(text = x[sz]), en)
        if (length(siz$width)) 
          size <- paste("fig-width:", siz$width)
        if (length(siz$height)) 
          size <- c(size, paste("fig-height:", siz$height))
        if (length(siz$column)) 
          size <- c(size, paste("column:", siz$column))
        x <- x[-sz]
      }
    }
    else {
      raw <- FALSE
      y <- names(fs)[i]
      y <- stringr::str_remove(y, '_txt')
      label <- paste0(label, parent_label, '-', y, '-')#; print(c("label2 (list): ", label))
      y <- stringr::str_to_title(stringr::str_replace_all(y, '_', ' '))
      x <- paste0(".fs.[[", i, "]]")
      if (i %in% cap) 
        capt <- basecap
    }
    r <- paste0("results=\"", if (raw) 
      "markup"
      else "asis", "\"")
    callout <- NULL
    if (length(label) && length(capt)) {
      lab <- paste0(label, i)#; print(c("lab: ", lab))
      callout <- c(paste0("label: ", lab), paste0("fig-cap: \"", 
                                                  capt, "\""))
      qreport::addCap(lab, capt)
    }
    if (length(size)) 
      callout <- c(callout, size)#; print(c("callout: ", callout))
    k <- c(k, "", paste("##", y), "", makecodechunk(x, callout = callout, 
                                                    results = if (raw) "markup" else "asis"))
  }
  k <- c(k, ":::", "")
  if (debug) 
    cat(k, sep = "\n", file = "/tmp/z", append = TRUE)
  cat(knitr::knit(text = k, quiet = TRUE))
  return(invisible())
}


# tabList = normative_expec_histograms

maketabs_nested_list <- function(tabList, str_remove_pattern = '_txt|_common|_approval',
                                 clean_elem_names = TRUE,
                                 parent_path = "", cap = NULL,
                                 depth = 1) {
  
  if(clean_elem_names == TRUE) {
    # First clean list element names
    clean_elem_names <- function(x) {
      # Check if the current object is a list
      if (identical(class(x), "list")) {
        new_names <- sapply(names(x), stringr::str_remove_all, pattern = str_remove_pattern)
        new_names <- sapply(new_names, replace_punct_with_english)
        # Temporarily store the old elements with new names
        new_elems <- rlang::set_names(lapply(names(x), function(n) x[[n]]), new_names)
        # Replace the original elements with the newly named elements
        x <- new_elems
        
        # Iterate through each element in the list to clean nested list names
        for (name in names(x)) {
          x[[name]] <- clean_elem_names(x[[name]])
        }
      }
      return(x)
    }
    
    tabList <- clean_elem_names(tabList)
  }
  
  en <- parent.frame()
  ge <- .GlobalEnv
  assign(envir = en, "caption", function(cap, label = NULL) list(label = label, 
                                                                 cap = cap))
  assign(envir = en, "fig.size", function(width = NULL, height = NULL, 
                                          column = NULL) list(width = width,
                                                              height = height,
                                                              column = column))
  assign(".tabList.", tabList, envir=ge)
  
  
  
  # Define the recursive function to collect paths
  generate_quarto_tabset_headers <- function(tabList,
                                             parent_path = "", cap = NULL,
                                             depth = 1) {
    tabList_name <- deparse(substitute(tabList))#; print(tabList_name)
    headers <- c()  # Initialize an empty vector to store headers
    
    # Iterate through each element in tabList
    for (name in names(tabList)) {
      # Construct the current path
      current_path <- ifelse(nchar(parent_path) > 0,
                             paste(parent_path, "[[\"", name, "\"]]", sep = ""),
                             paste0(tabList_name, "[[\"", name, "\"]]"))
      
      # Generate the header with the appropriate number of '#' based on depth
      header <- c()
      for (i in 1:depth){
        header <- paste(header, '#')
      }
      header <- stringr::str_remove_all(header, ' ')
      name_title <- stringr::str_to_title(stringr::str_replace_all(name, '_', ' '))
      name_title <- stringr::str_replace_all(name_title, ' ', '_')
      name_title <- stringr::str_replace_all(name_title, "[[:punct:]\\s]", '-')
      name_title <- stringr::str_replace_all(name_title, '-+', '-'); # cat(name_title, "\n")
      # header <- paste(header, " ", name_title, "")
      header <- paste(header, name_title)
      # Check if the current element is a list itself
      if (identical(class(tabList[[name]]), "list")) {
        # If it's the top level, add {.panel-tabset} class
        header <- c(header, "", "::: {.panel-tabset}", "")
        
        # Recursively call the function to go deeper, and collect headers from this subtree
        child_headers <- generate_quarto_tabset_headers(tabList = tabList[[name]],
                                                        parent_path = current_path,
                                                        depth = depth + 1)
        # Combine the current header with the collected child headers
        # If the element is a non-leaf list, add the header and child headers, and append ":::"
        headers <- c(headers, header, child_headers, ":::", "")
      } else {
        # Process leaves
        leaves <- c()
        if(any(class(tabList[[name]]) == "multiple_leaves")){
          for(i in seq_along(tabList[[name]])){
            label <- stringr::str_replace_all(paste0(parent_path, '-', name_title, '-'), "[[:punct:]]", '-')
            label <- stringr::str_replace_all(label, '-+', '-')
            # label <- paste0(label, i, "-") 
            # basecap <- name_title
            # if (name %in% cap) 
            capt <- name_title
            callout <- NULL
            if (length(label) && length(capt)) {
              lab <- paste0(label,sample(10000:99999, 1))#; print(c("lab: ", lab))
              callout <- c(paste0("label: ", '"', lab, '"'), paste0("fig-cap: \"", 
                                                                    capt, "\""))
              qreport::addCap(lab, capt)
            }
            x <- stringr::str_extract(current_path, "(?<=\\[).+")
            x <- paste0(".tabList.[",x,'[[',i,']]')
            leaves <- c(leaves, makecodechunk(x, callout = callout, results = "asis"))
          }
        
        } else {
          label <- stringr::str_replace_all(paste0(parent_path, '-', name_title, '-'), "[[:punct:]]", '-')
          label <- stringr::str_replace_all(label, '-+', '-')
          # basecap <- name_title
          # if (name %in% cap) 
          capt <- name_title
          callout <- NULL
          if (length(label) && length(capt)) {
            lab <- paste0(label,sample(10000:99999, 1))#; print(c("lab: ", lab))
            callout <- c(paste0("label: ", '"', lab, '"'), paste0("fig-cap: \"", 
                                                                  capt, "\""))
            qreport::addCap(lab, capt)
          }
          # if (length(size)) 
          #   callout <- c(callout, size)
          x <- stringr::str_extract(current_path, "(?<=\\[).+")
          x <- paste0(".tabList.[",x)
          leaves <- makecodechunk(x, callout = callout, results = "asis")
        }
        headers <- c(headers, header, leaves)
      }
    }
    
    # Replace sequences of "" with single ""
    if(!is.null(headers)){
      headers <- headers[headers != lag(headers) | is.na(lag(headers))]  
    }
    return(headers)
  }
  headers <- generate_quarto_tabset_headers(tabList, parent_path = "",
                                            cap = NULL, depth = 1) 
  
  cat(knitr::knit(text = headers, quiet = TRUE))
  return(invisible())
}


# Adapted from purrr:list_flatten to accommodate class 'multiple leaves'
# For use with makechunks() to prepare flat lists for slides
list_flatten <- function(
    x,
    ...,
    name_spec = "{outer}_{inner}",
    name_repair = c("minimal", "unique", "check_unique", "universal")
) {
  vctrs::obj_check_list(x)
  rlang::check_dots_empty()
  checkmate::check_string(name_spec)
  
  # Take the proxy as we restore on exit
  proxy <- vctrs::vec_proxy(x)
  
  # Unclass S3 lists and multiple leaves to avoid their coercion methods.
  # Wrap atoms in a list of size 1 so the elements can be concatenated in a single list.
  proxy <- purrr::map_if(proxy, function(branch) {
    vctrs::vec_is_list(branch) | inherits(branch, "multiple_leaves")
    },
    unclass, .else = list)
  
  out <- vctrs::list_unchop(
    proxy,
    ptype = list(),
    name_spec = name_spec,
    name_repair = name_repair,
    error_arg = x,
    error_call = current_env()
  )
  
  # Preserve input type
  t <- vctrs::vec_restore(out, x)
  t
}



list_flatten1 <- function(
    x,
    ...,
    name_spec = "{outer}_{inner}",
    name_repair = c("minimal", "unique", "check_unique", "universal"),
    append_names = FALSE
) {
  vctrs::obj_check_list(x)
  rlang::check_dots_empty()
  checkmate::check_string(name_spec)
  
  # Take the proxy as we restore on exit
  proxy <- vctrs::vec_proxy(x)
  
  # Unclass S3 lists and multiple leaves to avoid their coercion methods.
  # Wrap atoms in a list of size 1 so the elements can be concatenated in a single list.
  proxy <- purrr::map(proxy, function(branch) {
    if (vctrs::vec_is_list(branch) | inherits(branch, "multiple_leaves")) {
      if (length(names(branch)) > 1) {
        # Do not flatten if the length is greater than 1
        return(branch)
      } else {
        return(unclass(branch))
      }
    } else {
      return(list(branch))
    }
  })
  
  # Conditionally apply name_spec
  if (append_names == TRUE) {
    out <- vctrs::list_unchop(
      proxy,
      ptype = list(),
      name_spec = name_spec,
      name_repair = name_repair,
      error_arg = x,
      error_call = rlang::current_env()
    )
  } else {
    out <- vctrs::list_unchop(
      proxy,
      ptype = list(),
      name_spec = "{inner}",
      name_repair = name_repair,
      error_arg = x,
      error_call = rlang::current_env()
    )
  }
  
  # Preserve input type
  t <- vctrs::vec_restore(out, x)
  t
}



makeslides <- function (..., wide = FALSE, cwidth = if (wide) "column-page", 
                      initblank = FALSE, baselabel = NULL, cap = NULL, basecap = NULL, 
                      parent_label = NULL, debug = FALSE) 
{
  en <- parent.frame()
  assign(envir = en, "caption", function(cap, label = NULL) list(label = label, 
                                                                 cap = cap))
  assign(envir = en, "fig.size", function(width = NULL, height = NULL, 
                                          column = NULL) list(width = width, height = height, column = column))
  fs <- list(...)
  if (length(fs) == 1 && "formula" %nin% class(fs[[1]])) {
    fs <- fs[[1]]
    ge <- .GlobalEnv
    assign(".fs.", fs, envir = ge)
  }
  if (!length(baselabel))
    baselabel <- knitr::opts_current$get("label")
  else if (baselabel == "none") 
    baselabel <- NULL
  if (length(baselabel) && !grepl("^fig-", baselabel))
    baselabel <- paste0("fig-", baselabel)
  # yaml <- paste0(".panel-tabset", if (length(cwidth)) 
  #   paste0(" .", cwidth))
  yaml <- ''
  k <- c("", yaml, "")
  # k <- c("", paste0("::: {", yaml, "}"), "") 
  if (initblank) 
    k <- c(k, "", "##   ", "")
  for (i in 1:length(fs)) {
    label <- baselabel#; print(c("label1: ", label))
    capt <- NULL
    size <- NULL
    f <- fs[[i]]
    #(c("parent_label: ", parent_label))
    #parent_label <- stringr::str_remove(parent_label, '_txt')
    isform <- FALSE
    if ("formula" %in% class(f)) {
      isform <- TRUE
      capt <- NULL
      v <- as.character(attr(terms(f), "variables"))[-1]
      y <- v[1]
      y <- gsub("`", "", y)
      x <- v[-1]
      label <- paste0(label, '-', parent_label, '-', y, '-')#; print(c("label2 (formula): ", label))
      raw <- "raw" %in% x
      if (raw) 
        x <- setdiff(x, "raw")
      jc <- grep("caption\\(", x)
      if (length(jc)) {
        capt <- eval(parse(text = x[jc]), en)
        if (length(capt$label)) 
          label <- capt$label#; print(c("label3 (formula): ", label))
        capt <- capt$cap
        x <- x[-jc]
      }
      sz <- grep("fig.size\\(", x)
      if (length(sz)) {
        siz <- eval(parse(text = x[sz]), en)
        if (length(siz$width)) 
          size <- paste("fig-width:", siz$width)
        if (length(siz$height)) 
          size <- c(size, paste("fig-height:", siz$height))
        if (length(siz$column)) 
          size <- c(size, paste("column:", siz$column))
        x <- x[-sz]
      }
    }
    else {
      raw <- FALSE
      y <- names(fs)[i]
      y <- stringr::str_remove(y, '_txt')
      label <- paste0(label, parent_label, '-', y, '-')#; print(c("label2 (list): ", label))
      y <- stringr::str_to_title(stringr::str_replace_all(y, '_', ' '))
      x <- paste0(".fs.[[", i, "]]")
      if (i %in% cap) 
        capt <- basecap
    }
    r <- paste0("results=\"", if (raw) 
      "markup"
      else "asis", "\"")
    callout <- NULL
    if (length(label) && length(capt)) {
      lab <- paste0(label, i)#; print(c("lab: ", lab))
      callout <- c(paste0("label: ", lab), paste0("fig-cap: \"", 
                                                  capt, "\""))
      qreport::addCap(lab, capt)
    }
    if (length(size)) 
      callout <- c(callout, size)#; print(c("callout: ", callout))
    k <- c(k, "", paste("#", y), "", makecodechunk(x, callout = callout, 
                                                    results = if (raw) "markup" else "asis"))
  }
  k <- c(k, "") # k <- c(k, ":::", "")
  if (debug) 
    cat(k, sep = "\n", file = "/tmp/z", append = TRUE)
  cat(knitr::knit(text = k, quiet = TRUE))
  return(invisible())
}
