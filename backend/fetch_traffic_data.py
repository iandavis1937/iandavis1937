import requests
import urllib.parse as urlparse
 
# Route parameters
start = "37.77493,-122.419415"               # San Francisco
end = "34.052234,-118.243685"                # Los Angeles
routeType = "fastest"                        # Fastest route
traffic = "true"                             # To include Traffic information
travelMode = "truck"                         # Travel by truck
avoid = "unpavedRoads"                       # Avoid unpaved roads
departAt = "2021-10-20T10:00:00"             # Departure date and time
vehicleCommercial = "true"                    # Commercial vehicle
key = "key=0SaclIbTplyYvBQjgKzAmzCBBfALIFuw"         # API Key
 
# Building the request URL
baseUrl = "https://api.tomtom.com/routing/1/calculateRoute/";
 
requestParams = (
    urlparse.quote(start) + ":" + urlparse.quote(end) 
    + "/json?routeType=" + routeType
    + "&traffic=" + traffic
    + "&travelMode=" + travelMode
    + "&avoid=" + avoid 
    + "&vehicleCommercial=" + vehicleCommercial
    + "&departAt=" + urlparse.quote(departAt))
 
requestUrl = baseUrl + requestParams + "&key=" + key

response = requests.get(requestUrl)

if(response.status_code == 200):
    # Get response's JSON
    jsonResult = response.json()
 
    # Read summary of the first route
    routeSummary = jsonResult['routes'][0]['summary'];
    
    # Read ETA
    eta = routeSummary['arrivalTime']
 
    # Read travel time and convert it to hours
    travelTime = routeSummary['travelTimeInSeconds'] / 3600
    
    # Print results
    print(f"{departAt}, ETA: {eta}, Travel time: {travelTime:.2f}h")
    
else:
    print(f"Failed to fetch data: {response.status_code} - {response.text}")
