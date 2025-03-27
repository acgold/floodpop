# FloodPop: High-resolution estimates of the US population in high flood hazard areas
![image](https://github.com/user-attachments/assets/730079e8-d974-44ce-9290-e078f7a6c74b)

This repository presents a workflow to create estimates of the US population living in high flood hazard areas using classified building footprints and 2020 decennial Census data with confidence intervals.

## Workflow
Files are within the `code` folder of this repository.

1. Download data
    - `download_mbd.ipynb`: Downloads [Microsoft building footprints (US Release)](https://github.com/microsoft/USBuildingFootprints) by state to an Esri File Geodatabase.
    - `download_overture.ipynb`: Uses DuckDB to download [Overture building footprints](https://overturemaps.org/) by state to an Esri File Geodatabase.
    - `download_usa_structures.ipynb`: Download [FEMA's USA Structures database](https://disasters.geoplatform.gov/USA_Structures/) by state to an Esri File Geodatabase.
    - `download_nsi.ipynb`: Download [USACE's National Structure Inventory](https://www.hec.usace.army.mil/confluence/nsi) by state to an Esri File Geodatabase.
    - `download_nfhl.ipynb`: Download [FEMA regulatory floodplain](https://www.fema.gov/flood-maps/national-flood-hazard-layer) for all US counties.
    - `get_nfhl_footprints.ipynb`: Creates a national layer of FEMA study footprints.
    - There are other datasets that are not downloaded programattically. These include:
        - 2020 decennial Census data ([link](https://www.census.gov/programs-surveys/decennial-census/decade/2020/2020-census-results.html))
        - The estimated 100-year floodplain ([link](https://www.sciencedirect.com/science/article/pii/S0048969718328481))

2. Classify Overture footprints and fill in any gaps
    - `classify_overture.ipynb`: Classifies Overture building footprints as "residential", "not residential", or "unclassified" using other sources. Adds any potentially missed buildings. Tags buildings if they intersect the FEMA Special Flood Hazard Area (SFHA), the [estimated SFHA](https://www.sciencedirect.com/science/article/pii/S0048969718328481), or the footprint of a FEMA study.

3. Validate building footprint classifications and building counts
    - `validate_res_or_not.ipynb`: Classifies local parcel datasets and creates spatially joined feature classes for further analysis in R.
    - `building_classification_validation.R`: Functions to re-create validation of building type ("residential" or "not residential") for Mecklenburg County, NC, Miami-Dade County, FL, and Sacramento, CA.

4. Estimate confidence intervals for Census data
    -   `census_error_estimate.ipynb`: Follows methods from [here](https://registry.opendata.aws/census-2020-amc-mdf-replicates/?utm_campaign=20241024cnmps1&utm_medium=email&utm_source=govdelivery) to estimate confidence intervals of 2020 decennial census data

5. Create FloodPop
    - `create_floodpop.R`: Creates the tabular and geospatial files of FloodPop by merging the building footprint information with census data.

6. Create Figures and Tables
    - `create_figures.R`: Creates maps and tables for the corresponding manuscript.
