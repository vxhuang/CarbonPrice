# README
This repository contains codes for Huang et al. Effects of global climate mitigation on regional air quality and health. Nature Sustainability (2023).
* Codes in "GCAM ensemble" are used to generate an ensemble of states of the world (SOWs). The codes are produced by [Dr. Vivek Srikrishnan](https://github.com/vsrikrish).
* Codes in "Data and Codes" are used to analyse the ensemble results and generate the figures. 
  * The "dataExtraction.R" script extracts data from the ensemble results. The generated data are available at http://zenodo.org/record/6975580. The can be stored in the "Data and Codes/DATA" folder. 
  * The "downscaling.R" script can be used to downscale emissions from GCAM's 32 regional resolution to the country level using the Emissions Database for Global Atmospheric Research (EDGAR v4.3) as calibration. 
  * The "HEALED_XH_1.R" script performs the air quality modeling using the TM5-FASST model, and the health impact assessments. 
  * The "main_text.R" script generates figures for the research paper. 
