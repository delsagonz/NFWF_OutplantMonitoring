# NFWF_OutplantMonitoring
:ocean::shell::palm_tree::tropical_fish:



*This will use TagLab's export file to report on outplanted APAL array monitoring data. Looking at longterm monitoirng of **APAL** arrays and the use of top-down photos allows post-outplant analysis via TagLab where areas will be outputed. There should be an intial and final area in order to this script to work succesfully.* 

Required libraries: **tidyverse,diplyr,readxl & ggplot2**

                                   **Outputs:** 
                                 TOTAL_FINAL_AREA
                                TOTAL_INITIAL_AREA
                               LIVE_PLANAR_AREA_CM2 
                                PERCENT_LIVE_TISSUE
                                  MEAN/SE/SD
                                CHANGE/GROWTH_CM2
                                 GROWTHRATES_(%)
                              MAX_GROWTHPOTENTIAL_CM2
                        

There are 2 different codes listed in this source: MaxGrowthPotential_ & Array_Growths. The codes may look different but they are not - there is a step where grouping is different..

**MaxGrowthPotential** - has a different function as it accounts for mortality and determine a more accurate growth rate of the surviving fragments, maximum growth potential was calculated by removing dead and missing fragments from the growth analyses.
                                                                         
**TotalGrowth** - does not do use the subtracted dead fragments, therefore there will be a difference in growth results between both scripts.

Choose the one that works best for your purposes. Godspeed :vulcan_salute::mermaid:

