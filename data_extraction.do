/* 
Many data sources used in this paper are produced in stata by the original authors. This file does three things. 

i) Takes the original files containing heritage variables, and makes them consistent across sources (country naming etc)
ii) Ancestry adjusts where necessary. 
iii) Combines the European and World Values Survey, removes values that are not related to gender.

Once this is done, the machine learning happens in R. When there are two datasets with the same variables, I use the dataset with the largest country coverage.

Official documentation on creating the Integrated Values Survey (i.e. combining the World Values Survey, and the European Values Survey) is available here: 
https://europeanvaluesstudy.eu/methodology-data-documentation/integrated-values-surveys-ivs-1981-2021/
https://europeanvaluesstudy.eu/methodology-data-documentation/integrated-values-surveys-ivs-1981-2021/data-and-documentation-ivs-1981-2021/ 
*/

version 16
ssc install mdesc
ssc install asgen 

// Change to your own path.
gl fpath "/Users/alistair/Desktop/R/Preferences/For submission/" 
cd "$fpath"

qui do "${fpath}code/heritage_measures.do"
qui do "${fpath}code/ancestry_adjustment.do"
qui do "${fpath}code/ivs.do"
