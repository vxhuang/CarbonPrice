## Workflow to reproduce the results and figures 
1. Install [GCAM v5.4](http://jgcri.github.io/gcam-doc/v5.4/toc.html). 
2. Generate an ensemble of SOWs using GCAM by running `GCAM ensemble/scripts/run_ensemble.py`
3. After the ensemble is generated and the result databases are stored, run `ensemble/scripts/query_db_xh.py` to extract the results of interest from the GCAM result databases. The result databases are not in this repository as they are too large. 
4. Run `Data and Codes/CODES/R_scripts/healthImpacts.R` to perform the air quality modeling and health impact assessment. 
5. After steps 3 and 4 are completed, run `Data and Codes/CODES/R_scripts/dataExtraction.R` to extract the health results, analyze and process other GCAM raw results, and generate intermediate results for figures and tables. The intermediate results are available at https://zenodo.org/record/6975580. 
7. Run `Data and Codes/CODES/Huang_etal_2023/main_text.R` to generate the figures in the main text of the paper. 
