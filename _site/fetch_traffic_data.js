

async function fetchTrafficData() {
  const response = await fetch('https://api.tomtom.com/traffic/services/4/incidentDetails');
  const data = await response.json();
  console.log("Traffic Data (Head):", data.slice(0, 5));
  return data;
}

fetchTrafficData().then(data => {
  // Make the data available globally or process it here
  window.trafficData = data;
  // Optionally, store the data in a hidden input field or use it directly
  document.getElementById('traffic-data').value = JSON.stringify(data);
});

