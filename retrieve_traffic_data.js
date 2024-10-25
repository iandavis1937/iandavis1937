

document.addEventListener("DOMContentLoaded", function() {
  
    var trafficData = document.getElementById('traffic-data').value;
    // Parse the JSON data
    trafficData = JSON.parse(trafficData);

    // Example: Render the data in the document
    document.getElementById('traffic-info').innerText = JSON.stringify(trafficData, null, 2);
  });