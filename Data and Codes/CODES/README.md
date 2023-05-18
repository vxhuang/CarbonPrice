This section provides all Scripts and data used in the Carbon Price model

# Module Summary

* **GCAM 32 (with Taiwan)** - Directory the shapefile for the 32 GCAM geopolitical regions
* **GCAM emissions matching** - Directory containing emissions data from EDGAR, regional mapping between GCAM and TM5-FASST, and sector mapping between GCAM and EDGAR  
* **HIA** - Directory containing baseline mortalities derived from IFs, relative risks from GBD 2019, and demographics data
* **TM5_SRMs** - Directory containing TM5-FASST source receptor matrices
* **R_scripts** - Directory containing data analysis and processing codes
* **Huang_etal_2023** - Directory containing code to generate visualizations from sample simulation results

## Prerequisite Software

Please install software listed below.

* Python v3.6.8
* R (version later than 3.6.8), with packages: ggplot2, ggpubr, dplyr, tidyr, tidyverse, maps, mapdata, readxl, scales, RColorBrewer, ggpattern, ggtext, foreach, doParallel


## Download Repository and Input Data

* Clone the repository by running the following command. 

    git clone https://github.com/vxhuang/CarbonPrice/

If you are unfamiliar with git, download a zip file of the repository by clicking on the green Code button.

* Download files from http://zenodo.org/record/6975580 and store in "/Data and Codes/DATA/".

<br>**Note** 
dataExtraction.R script is used to extract the above files from Raw GCAM output. this script can be used as a guideline for extracting users specific data from GCAM output databases.
 
