# README
This repository contains codes for Huang et al. Effects of global climate mitigation on regional air quality and health. Nature Sustainability (2023). This code is developed by [Xinyuan Huang](https://github.com/vxhuang) and [Dr. Vivek Srikrishnan](https://github.com/vsrikrish), organized and reproduced by [Sitara Baboolal](https://github.com/SBabs08), and reproduced by Jinyu Shiwang. 

Module Summary
--------------
* "GCAM ensemble" - Codes in this directory are used to generate an ensemble of states of the world (SOWs). The codes are produced by [Dr. Vivek Srikrishnan](https://github.com/vsrikrish).
  * Codes in "GCAM ensemble" are executed using Python 3.6.8.  
* "Data and Codes" - Codes used to analyse the ensemble results and generate the figures. 
  * Scripts provided in "Data and Codes/CODES/R_scripts" are executed using R 3.6.0 or R 4.1.2 (both available).
    * "dataExtraction.R" - script extracts data from the ensemble results. The generated data are available at http://zenodo.org/record/6975580. The can be stored in the "Data and Codes/DATA" folder. 
    * The "downscaling.R" - script can be used to downscale emissions from GCAM's 32 regional resolution to the country level using the Emissions Database for Global Atmospheric Research (EDGAR v4.3) as calibration. 
    * "healthImpacts.R" - script performs the air quality modeling using the TM5-FASST model, and the health impact assessments. 
  * Scripts and instructions provided in "Data and Codes/CODES/Huang_etal_2023/" are used to replicate results from paper.
    *  For the steps needed to replicate the exact results and figures presented, please follow **/Data and Codes/CODES/Huang_etal_2023/README.md**.
    * "main_text.R" - script generates figures for the research paper. 
    *  Example figures are provided in Data and Codes/CODES/Huang_etal_2023/Figures/" for comparison


## Huang et al. 
**Effects of global climate mitigation on regional air quality and health. Nature Sustainability (2023).**  
Xinyuan Huang<sup>1</sup> , Vivek Srikrishnan<sup>2</sup> , Jonathan Lamontagne<sup>3</sup> , Klaus Keller<sup>4</sup> , and Wei Peng,<sup>1,5\*</sup> 

<sup>1 </sup> Department of Civil and Environmental Engineering, The Pennsylvania State University, University Park, PA, USA

<sup>2 </sup> Department of Biological and Environmental Engineering, Cornell University, Ithaca, NY, USA

<sup>3 </sup> Department of Civil and Environmental Engineering, Tufts University, Medford, MA, USA

<sup>4 </sup> Thayer School of Engineering, Dartmouth College, Hanover, NH, USA

<sup>5 </sup> School of International Affairs, The Pennsylvania State University, University Park, PA, USA

## Abstract
Climate mitigation can bring air quality and health co-benefits. How these health impacts might be distributed across countries remains unclear. Here we use a coupled climate–energy–health model to assess the country-varying health effects of a global carbon price across nearly 30,000 future states of the world (SOWs). As a carbon price lowers fossil fuel use, our analysis suggests consistent reductions in ambient fine particulate matter (PM<sub>2.5</sub>) levels and associated mortality risks in countries that currently suffer most from air pollution. For a few less-polluted countries, however, a carbon price can increase the mortality risks under some of the considered SOWs due to emissions increases from bioenergy use and land-use changes. These potential health co-harms are largely driven in our model by the scale and method of deforestation. A robust and quantitative understanding of these distributional outcomes requires improved representations of relevant deep uncertainties, country-specific characteristics and cross-sector interactions.

## Journal reference
Huang et al. Effects of global climate mitigation on regional air quality and health. Nature Sustainability (2023). https://www.nature.com/articles/s41893-023-01133-5. 

## Code reference
Xinyuan Huang, vxhuang/CarbonPrice: Codes for Huang et al. Effects of global climate mitigation on regional air quality and health. Nature Sustainability (2023). Zenodo, May 3, 2023. https://zenodo.org/record/7894050. 
