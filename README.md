# Analysis-Minimum-Wage

### This project cleans microdata from the SOEP and consequently applies (fixed effects) regression analysis to estimate the effect of Germany's Minimum Wage on employment. 

To practice dealing with raw data, I used the wide datasets from the German Microcensus "GSOEP" (https://www.diw.de/sixcms/detail.php?id=diw_01.c.444333.de). The first R script, 
"DataCleaning.R" therefore uses 4 loops to read in the different documents. I specified which variables should be read in in the csv file 
"soep-equiv-selection" and "soep-feature-selection" and can easily be adjusted if one is interested in different variables of the GSOEP. 
Next, the script creates several dummy variables and recodes many of them. Finally, I merge the data with newly downloaded google-trends data and merge it. 
The "analysis.R" script then uses a Differences-in-Differences estimation to attain a causal effect. One regression does this by estimating the 
"Kaitz-Index", that is which regions in germany had people working below the minimum wage. The second analysis uses the google trends and compares
region in germany in which people looked a lot for terms like "minimum wage" in 2015". The results are then explained and showcased in "report.pdf". 

