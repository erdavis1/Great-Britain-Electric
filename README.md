# Mapping Great Britain's electrical grid
## Getting the data
- Run getData.R to get the data from OpenStreetMap
- Data from National Grid and SSEN is not available for commercial use or republication, so OSM is the next best substitute
- The data needs a fair amount of cleaning, particularly subsurface cables
  - Remove all lines shorter than 1km and under 132kv
  - Remove lines that don't have both ends within GB
  - Remove substations not within 100m of a power/cable line we retained
  - Subsurface cables may not be a 100% accurate representation, but it's the best available for now
 
## Plotting the map
- Run plotMap.R to create the plot
- Simplifies the cable lines one step further by forcing them to lie at multiples of 15 degrees (subway-map adjacent)
- Outputs an svg for further refining in Illustrator
