# NFWF_OutplantMonitoring
This will use TagLab's export file to report on outplanted APAL array monitoring data. Looking at longterm monitoirng of **APAL** arrays and the use of top-down photos allows post-outplant analysis via TagLab where areas will be outputed. There should be an intial and final area in order to this script to work succesfully.  

Both scripts will require *libraries*: **tidyverse,diplyr,readxl & ggplot2**

Outputs: 
 TOTAL_FINAL_AREA
 TOTAL_INITIAL_AREA
 LIVE_PLANAR_AREA_CM2 
 PERCENT_LIVE_TISSUE
 MEAN/SE/SD
 CHANGE/GROWTH_CM2
 GROWTHRATE_PERCENT(%)

There are 2 different codes listed in this source: MaxGrowthPotential_ & Array_Growths


**MaxGrowthPotential SCRIPT** - has a different function as it accounts for mortality and determine a more accurate growth rate of the surviving fragments, maximum growth potential was calculated by removing dead and missing fragments from the growth analyses.
**TotalGrowth SCRIPT** - does not do that.

