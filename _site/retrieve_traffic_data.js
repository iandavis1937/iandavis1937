

document.addEventListener("DOMContentLoaded", function() {
    fetch('traffic-data.json')
      .then(response => response.json())
      .then(data => {
        // Log the first few entries to the console
        console.log("Traffic Data (Head):", data.slice(0, 5));
        
        // You can store the data in a variable for further use
        window.trafficData = data;
      })
      .catch(error => console.error('Error fetching traffic data:', error));
  
  
    var trafficData = document.getElementById('traffic-data').value;
    // Parse the JSON data
    trafficData = JSON.parse(trafficData);

    // Example: Render the data in the document
    document.getElementById('traffic-info').innerText = JSON.stringify(trafficData, null, 2);
  });