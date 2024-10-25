

let initialized = false;

document.addEventListener('DOMContentLoaded', () => {
  
  console.log('Initialized (Chunk 1 Start): ', initialized)
  
  // Select the search button and the 'check this out image'
  const searchButtonIcon = document.querySelector('.aa-DetachedSearchButtonIcon');
  const imageElement = document.querySelector('img[src="images/check_this_out.png"]');

  // Ensure the elements exist
  if (searchButtonIcon) {
    // Get the bounding rectangle of the element
    const rect = searchButtonIcon.getBoundingClientRect();
    window.searchButtonX = rect.left;
    console.log('X position:', searchButtonX, 'px');
  } else {
    console.error('Element .aa-DetachedSearchButtonIcon not found');
  }

  if (imageElement) {
    // Get the bounding rectangle of the element
    const rect = imageElement.getBoundingClientRect();

    // Calculate the right border position
    window.rightBorderPosition = rect.left + rect.width;
  }

  // Set the initial width of the horizontal line
  const horizontalLine = document.querySelector('.horizontal-line');

  if (horizontalLine) {
    console.log('Element horizontalLine width: ', horizontalLine.style.width);
  } else {
    console.error('Element horizontalLine not found in the DOM.');
  }
  
  
  window.minLength = 80; // Minimum width for the horizontal line
  if (horizontalLine) {
    console.log(minLength)
    horizontalLine.style.width = `${window.minLength}px`;
  }
  
  const tocElement = document.getElementById('TOC');

  if (tocElement) {
    const tocRect = tocElement.getBoundingClientRect();
    const tocXPosition = tocRect.left; // X position relative to the viewport
    console.log('TOC X Position:', tocXPosition, 'px');

    // Now, apply this X position to another element, e.g., horizontalEnd
    const horizontalEnd = document.querySelector('.horizontal-line');
    
    // if (horizontalEnd) {
    //   horizontalEnd.style.left = `${tocXPosition}px`; // Align horizontalEnd with the x position of TOC
    // }
  } else {
    console.error('#TOC element not found');
  }
  
  initialized = true;  // Allow the rest to prcoeed
  console.log('Initialized (Chunk 1 End): ', initialized)
  
  // Add the scroll event listener after DOM is fully loaded
  window.addEventListener('scroll', longerWithScroll);
});

// window.addEventListener('scroll', () => {
function longerWithScroll() {
  
  console.log('Initialized (Chunk 2 Start): ', initialized)
  if (!initialized) {
    // If not initialized, skip the scroll event handling
    return;
  }
  
  const scrollPosition = window.scrollY;
  const windowHeight = window.innerHeight;
  const totalHeight = document.body.scrollHeight - windowHeight;
  const scrollPercentage = scrollPosition / totalHeight;
  console.log('Scroll Percentage:', scrollPercentage);

  const xLength = window.searchButtonX - window.rightBorderPosition;
  console.log('xLength:', xLength);

  const horizontalLine = document.querySelector('.horizontal-line');
  const verticalLine = document.querySelector('.vertical-line');
  const maxHeight = 1000; // Maximum height the arrow can reach vertically

  if (scrollPercentage > 0.25 & scrollPercentage <= 0.5) {
    // Adjust the width of the horizontal line based on scroll percentage
    const newWidth = window.minLength + (xLength * ( (scrollPercentage - 0.25) / (0.5 - 0.25) ) );
    console.log('newWidth:', newWidth);
    horizontalLine.style.width = `${newWidth}px`;
    horizontalLine.classList.remove('hide-arrowhead');
    verticalLine.style.display = 'none';

  } else if (scrollPercentage > 0.5) {
    // Adjust the height of the vertical line based on scroll percentage
    const horizontalEnd = window.minLength + xLength;
    const newHeight = maxHeight * (scrollPercentage - 0.5) * (scrollPercentage + 0.7); // Adjust scroll percentage for vertical movement
    horizontalLine.style.width = `${horizontalEnd}px`;
    verticalLine.style.display = 'block';
    verticalLine.style.left = `${horizontalEnd - 82}px`; // Position the vertical line where the horizontal line ends
    verticalLine.style.height = `${newHeight}px`;
    horizontalLine.classList.add('hide-arrowhead');
  }
}
