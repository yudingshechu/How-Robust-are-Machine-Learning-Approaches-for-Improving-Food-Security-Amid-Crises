Here are the files for machine learning 
by: Gewei Cao 
################################
\bld folder contains generated items

\python_functions contains self-defined functions

00_DataGeneration creates the dataset used for analysis

01_... to 02_... are Juypter Notebooks with Python code for the analysis 

03_... are R code for plots 

04_... are R code for SHAP part

environment.yml can be used to create the anaconda environment for the ML analysis 

################################
in 03_01_AUC_plot.R the AUC.xlsx needed to be created by researchers manually, as the following stucture: 

model,	AUC_district,	AUC_county,	AUC_subcounty,	AUC_district_ada,	AUC_county_ada, ....
LR_during,	0.781031031,	0.771978022,	0.822641958,	0.71021021,	0.720659341, ....
RF_during,	0.805805806,	0.812527473,	0.83812104,	0.794044044,	0.808974359, ....
XGB_during,	0.8003003,	0.797106227,	0.824726955,	0.797547548,	0.801904762, ....
LR_before,	0.781456954,	0.858091787,	0.879076299,	0.79580574,	0.776350461, ....
RF_before	,        0.753090508,	0.87928195,	0.877307952,	0.794481236,	0.859354414, ....
XGB_before,	0.815452539,	0.888669302,	0.842357102,	0.804966887,	0.891633729, ....
