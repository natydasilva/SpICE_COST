# SpICE: An interpretable method for spatial data

This repo contains all the needed code and data to reproduce the paper SpICE: An interpretable method for spatial data under review in Computational Statistics Journal.


## Reproducible materials

- data: contains data and intermediate output
- paper: manuscript and figures
- rcode_workflow: rstats code used to produce results and figures
    - 01_dataset_map.R: code, data transformation and map
    - 02_run_models.R: Code to run models
    - 03_model_results.R: Code with model results
    - 04_spice_clusters.R: Code for cluster ICE curves
    - zapp_simStudy.R: Appendix, simulation study
    - zapp_toyExample.R: Appendix, Toy example
    
    
## Requirements: 

Several R packages were used to produce paper's reults: 

- General data manipulation and ploting: tidyverse, RColorBrewer,  patchwork
- Machine learning models: h2o.
        H2O runs on Java. To build H2O or run H2O tests, the 64-bit JDK is required. To run the H2O binary using either the command line, R, or Python packages, only 64-bit JRE is required.
H2O supports the following versions of Java: Java SE 17, 16, 15, 14, 13, 12, 11, 10, 9, 8.
More details here: https://docs.h2o.ai/h2o/latest-stable/h2o-docs/welcome.html

- Packages needed for smoothing and clustering ICE curves: KernSmooth, ClustGeo, sf
- Packages neede for maps and more ploting: leaflet, mapview, ggmap (an API key is required, check https://github.com/dkahle/ggmap) 

