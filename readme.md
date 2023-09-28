
# SQL code: 

Master_Thesis_Brexit_Isa: merge and creation nationality and migration variables
- input: raw data
- output: bucket-ifo-labor-market-skills-standard-vm-00/UK/bac_EU_v3 

The R codes are written assuming that the data is downloaded from GCS and that 1_data_cleaning.R is ran as the first code. 
preliminary step: download parquet data from bucket-ifo-labor-market-skills-standard-vm-00/UK/bac_EU_v3 to a folder in local machine 

# R codes:

1_data_cleaning.R: (cleaning data, creates variables necessary for other codes.)
- input: GCS data  
- outupt: df and df_UK 

2_descriptives.R: 
- input: df and df_UK 
- output: tables and figures descriptives  

2b_regressions.R: 
- input: df
- output: df_panel and parallel trends assumption figures and regressions  

3_robustness_placebo.R: 
- input: df_panel
- output: robustenss and placebo tests
