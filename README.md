[readme.txt](https://github.com/user-attachments/files/26348517/readme.txt)
[readme.txt](https://github.com/user-attachments/files/26348517/readme.txt)
#### Gender and Reconciliation: A Case Study of Post-conflict Colombia ####

1. For replication of tables and graphs in manuscript,

Run gender_reconciliation_manuscript_code

2. Data is taken from Americas Barometer LAPOP surveys (2013-2021)

Data are from the following: LAPOP survey Colombia 2013, version 17
	      		     LAPOP survey Colombia 2014, version 15.2.3.0
	      		     LAPOP survey Colombia 2016, version 9.0.5.2
	      		     LAPOP survey Colombia 2018, version 9.0.5.6
	      		     LAPOP survey Colombia 2021,  version 17.1.2.5

LAPOP datasets can be found at http://datasets.americasbarometer.org/database/index.php

dataset_construction file creates subsetted datasets from the Americas Barometer surveys

The resulting datasets are lapop_13_14_16_18.csv and lapop_2021.csv.

Data from rebel_munis_by_year adds data on rebel-controlled territory in Colombia. The dataset was constructed using data from Anders, Therese (2020) "Territorial control in civil wars: Theory and measurement using machine learning". Municipalities receive the same rebel-control designation as the hexagon with which they share the most spatial overlap.


3. gender_reconciliation_manuscript_code file uses the constructed dataset lapop_13_14_16_18.csv to produce the tables and plots found in the manuscript. lapop_2021.csv is used to create Table 12.  



