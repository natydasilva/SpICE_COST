# SpICE: An interpretable method for spatial data

This repo contains all the needed code and data to reproduce the paper SpICE: An interpretable method for spatial data sent to Computational Statistics Journal.


## Reproducible materials

- data: contains data and intermediate output

- rcode_workflow: rstats code used to produce results and figures 

- paper: manuscript and figures

## Requirements: 
Several R packages were used to produce paper's reults: 

- General data manipulation and ploting: tidyverse
- Machine learning models: h2o.
        H2O runs on Java. To build H2O or run H2O tests, the 64-bit JDK is required. To run the H2O binary using either the command line, R, or Python packages, only 64-bit JRE is required.
H2O supports the following versions of Java: Java SE 17, 16, 15, 14, 13, 12, 11, 10, 9, 8.
More details here: https://docs.h2o.ai/h2o/latest-stable/h2o-docs/welcome.html

- Packages needed for smoothing and clustering ICE curves: KernSmooth, ClustGeo, sf
- Packages neede for maps and more ploting: RColorBrewer,  patchwork, leaflet, mapview, ggmap (an API key is required, check https://github.com/dkahle/ggmap) 

