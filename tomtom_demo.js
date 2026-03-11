

var apiKey = "0SaclIbTplyYvBQjgKzAmzCBBfALIFuw"
var searchBoxInstance

var commonSearchBoxOptions = {
  key: apiKey,
  center: map.getCenter(),
}

function initApplication() {
  searchBoxInstance = new tt.plugins.SearchBox(tt.services, {
    minNumberOfCharacters: 0,
    labels: {
      placeholder: "Search",
    },
    noResultsMessage: "No results found.",
    searchOptions: commonSearchBoxOptions,
    autocompleteOptions: commonSearchBoxOptions,
  })

  searchBoxInstance.on("tomtom.searchbox.resultselected", onSearchBoxResult)
  document
    .getElementById("search-panel")
    .append(searchBoxInstance.getSearchBoxHTML())
  map.on("moveend", updateSearchBoxOptions)
}

function updateSearchBoxOptions() {
  var updatedOptions = Object.assign(commonSearchBoxOptions, {
    center: map.getCenter(),
  })
  searchBoxInstance.updateOptions({
    minNumberOfCharacters: 0,
    searchOptions: updatedOptions,
    autocompleteOptions: updatedOptions,
  })
}

function onSearchBoxResult(result) {
  map.flyTo({
    center: result.data.result.position,
    speed: 3,
  })
}

initApplication()

var styleBase = "tomtom://vector/1/"
var styleS1 = "s1"
var styleRelative = "relative"
var refreshTimeInMillis = 30000

var trafficFlowTilesTier = new tt.TrafficFlowTilesTier({
  key: apiKey,
  style: styleBase + styleRelative,
  refresh: refreshTimeInMillis,
})

function toggleTrafficFlowTilesTier() {
  if (document.getElementById("flow-toggle").checked) {
    map.addTier(trafficFlowTilesTier)
  } else {
    map.removeTier(trafficFlowTilesTier.getId())
  }
}
document
  .getElementById("flow-toggle")
  .addEventListener("change", toggleTrafficFlowTilesTier)
  
var trafficIncidentCheckbox = document.getElementById("incidents-toggle")
var styleBase = "tomtom://vector/1/"
var styleS1 = "s1"
var styleRelative = "relative"
var refreshTimeInMillis = 30000

var trafficIncidentsTier = new tt.TrafficIncidentTier({
  key: apiKey,
  incidentDetails: {
    style: styleS1,
  },
  incidentTiles: {
    style: styleBase + styleS1,
  },
  refresh: refreshTimeInMillis,
})

function showTrafficIncidentsTier() {
  trafficIncidentCheckbox.checked = true
  map.addTier(trafficIncidentsTier)
}

function hideTrafficIncidentsTier() {
  trafficIncidentCheckbox.checked = false
  map.removeTier(trafficIncidentsTier.getId())
  clearIncidentList()
  removeBoundingBox()
}

function toggleTrafficIncidentsTier() {
  if (trafficIncidentCheckbox.checked) {
    showTrafficIncidentsTier()
  } else {
    hideTrafficIncidentsTier()
  }
}

trafficIncidentCheckbox.addEventListener("change", toggleTrafficIncidentsTier)


  
  