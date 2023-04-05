### Explanation of Figures

To measure intensity of sampling in ecoregions as well as richness I conducted the following:
- I converted the ecoregions shapefiles & the occurrence data into Lambert Azimuthal Equal Area. 
- Created a grid of 10x10km over the ecoregion
- Buffered the occurrence data points to account for potential of overlapping into neighboring grid cells. This was done by creating a radius of buffer equal to 1/2 the distance of a grid cell side. 
- Occurrence data was then joined to the ecoregion gridded data, cells were counted for how many times an occurrence overlapped each grid cell creating the output for the intensity maps. 
- Each grid cell was measured for number of unique species occurring, creating the output for the richness maps. 

Examples of intensity maps and richness maps:
<img src="./fig/i_plot_Southern Coastal Plain.png" width = "100%" align="middle"/>
<img src="./fig/r_plot_Southern-Coastal-Plain.png" width = "100%" align="middle"/>
