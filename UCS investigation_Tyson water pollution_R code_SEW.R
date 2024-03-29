# For UCS investigation into water pollution from Tysons meat processing plants
# Work done in 2023, report expected release April 2024
#####################################################################

# packages
library(tidyverse)


#################################################################################
########################NPDES ECHO DMR data #########
#################################################################################

setwd("...")
getwd()

DMR1822 <- read.csv(file="DMR1822all.csv", header=TRUE, sep=",")

### explore data ### 

colnames(DMR1822)
nrow(DMR1822)
str(DMR1822)

sum(DMR1822$Total.Pounds..lb.yr.)
# Note that EPA requires states to enter facilities w major status into NPDES 
# but does NOT require entry for non-major facilities

sum(DMR1822$Total.TWPE..lb.eq.yr.)
#note Total TWPE = Toxic Weighted Pounds Equivalent (TWPE) is the 
# mass of a pollutant or chemical discharged that accounts for its 
# relative toxicity. To convert pollutant loadings into TWPE, EPA 
# multiplies the pollutant mass (in pounds) by its toxic weighting 
# factor (TWF).

#note SIC code describes the primary activity of the facility

# Count number of unique states 
length(unique(DMR1822$State))

# States?
unique(DMR1822$State)

#FRS.ID?
unique(DMR1822$FRS.ID)
# Is not a unique ID for facility

############ clean data: pass 1 ############

# Create column of fac name + city + county + state
DMR1822$fac.ci.co.st <- paste(DMR1822$Facility.Name, ",", DMR1822$City, ",", DMR1822$County, ",", DMR1822$State)
colnames(DMR1822)

unique(DMR1822$fac.ci.co.st)
# 49 unique facilities

### Remove facility TYSON MHP , WEST MONROE , OUACHITA , LA because this is a mobile home park, not a 
# meat packing facility, and therefore not of interest to this investigation

colnames(DMR1822)
unique(DMR1822$fac.ci.co.st)

DMR1822v1 <- subset(DMR1822, fac.ci.co.st!= "TYSON MHP , WEST MONROE , OUACHITA , LA")

# States?
unique(DMR1822v1$State)

# How many unique facilities?
unique(DMR1822v1$fac.ci.co.st)

############ Investigation: Nitrate ############

unique(DMR1822v1$Pollutant.Name)

# isolate "Nitrate nitrogen"
nitrate <- subset(DMR1822v1, Pollutant.Name== "Nitrogen, nitrate dissolved") 
colnames(nitrate)
unique(nitrate$Pollutant.Name)

# How many pounds nitrate Tyson released over study period?
sum(nitrate$Total.Pounds..lb.yr.)

# Where did Tysons release nitrate into waters during study period?
unique(nitrate$State)

# How much nitrate in NE?
NEnitrate <- subset(nitrate, State == "NE")
colnames(NEnitrate)
unique(NEnitrate$State)
sum(NEnitrate$Total.Pounds..lb.yr.)

# How much nitrate in TX?
TXnitrate <- subset(nitrate, State == "TX")
colnames(TXnitrate)
unique(TXnitrate$State)
sum(TXnitrate$Total.Pounds..lb.yr.)

############ clean data: pass 2 ############

### Remove duplicate pollutants ###

# Per EPA, some chemicals are represented in more than one pollutant category
# in DMR:
# (1) the "nitrogen" pollutant category also includes data for the following categories:
# Ammonia as N, Inorganic nitrogen, Nitrite nitrogen, Nitrate nitrogen, Organic nitrogen, 
# Total Kjeldahl nitrogen
# (2) the “phosphorous" category also includes data for: total phosphate
# (3) the "chromium" category also includes data for: hexavalent chromium

# create new dataset, removing the "also includes" listed above

colnames(DMR1822v1)
unique(DMR1822v1$Pollutant.Name)

DMR1822v2 <- subset(DMR1822v1, Pollutant.Name!= "Ammonia as N" & Pollutant.Name!= "Inorganic Nitrogen" 
                    & Pollutant.Name!="Nitrite nitrogen, dissolved (as N)" & Pollutant.Name!= "Nitrogen, nitrate dissolved"
                      & Pollutant.Name!= "Organic Nitrogen" & Pollutant.Name!= "Total Kjeldahl Nitrogen" & 
                      Pollutant.Name!= "Phosphate, total (as PO4)" & Pollutant.Name!=  "Chromium, Hexavalent")

colnames(DMR1822v2)
nrow(DMR1822v2)

# States?
unique(DMR1822v2$State)

# How many unique facilities?
unique(DMR1822v2$fac.ci.co.st)

# How many unique pollutants?
unique(DMR1822v2$Pollutant.Name)

getwd()
write.csv(DMR1822v2,'DMR1822cleanpollutant.csv')

#################################
###  reorg and summarize data ###
#################################

######## Long to wide format - total by pollutant

colnames(DMR1822v2)
nrow(DMR1822v2)

# Create df with only columns of interest
mypolldf <- DMR1822v2[c("Year", "Pollutant.Name", "Total.Pounds..lb.yr.", "Total.TWPE..lb.eq.yr.")]

colnames(mypolldf)

# Pivot long to wide, summing total lbs & tot TWPE

mypolldfwd <- mypolldf %>%
  pivot_wider(
    names_from = Pollutant.Name,
    values_from = c(Total.Pounds..lb.yr., Total.TWPE..lb.eq.yr.),
    values_fn = sum
  )

nrow(mypolldfwd)
colnames(mypolldfwd)
str(mypolldfwd)

getwd()
write.csv(mypolldfwd,'pollutant totals by year.csv')

######## Long to wide format - facility total

# Create df with only columns of interest

colnames(DMR1822v2)

mydf <- DMR1822v2[c("Year", "fac.ci.co.st", "Facility.Name", "City", "County" , "State",
                    "HUC.12.Code" , "Watershed.Name", "Facility.Latitude",  "Facility.Longitude",
                    "Pollutant.Name", "Contains.Potential.Outliers." ,
                    "Total.Pounds..lb.yr.", "Total.TWPE..lb.eq.yr.")]

colnames(mydf)
nrow(DMR1822v2)
nrow(mydf)

## Create a wide dataframe where if multiple values then sum ##
# Use values_fn

# First convert col "Contains Potential Outliers?" to Outliers 1/0

unique(mydf$Contains.Potential.Outliers.)
# frequency table for different unique values of Potential outliers
table(mydf$Contains.Potential.Outliers.)

mydf$PotOutlier <- case_when(mydf$Contains.Potential.Outliers.=="Y" ~ 1, 
                             mydf$Contains.Potential.Outliers.=="" ~ 0)
unique(mydf$PotOutlier)
table(mydf$PotOutlier)
# 12 outliers, 1504 non-outliers

# Create new df that drops the text "Contains.Potential.Outliers." column but retains
# the numeric "PotOutlier" column

mydf2 <- mydf[c("Year", "fac.ci.co.st", "Facility.Name", "City", "County" , "State",
                  "HUC.12.Code" , "Watershed.Name", "Facility.Latitude",  "Facility.Longitude",
                  "Pollutant.Name", "PotOutlier" ,
                  "Total.Pounds..lb.yr.", "Total.TWPE..lb.eq.yr.")]

# Pivot long to wide, summing PotOutliers as well as total lbs

colnames(mydf2)

mydfwd <- mydf2 %>%
  pivot_wider(
    names_from = Pollutant.Name,
    values_from = c(Total.Pounds..lb.yr., Total.TWPE..lb.eq.yr., PotOutlier),
    values_fn = sum
  )

nrow(mydfwd)
colnames(mydfwd)
str(mydfwd)

getwd()
write.csv(mydfwd,'DMR1822wideV1.csv')

colnames(mydfwd)

# create summary columns for Total pounds per year
# using tidyverse mutate

mydfwd2 <- mydfwd %>%
  mutate(SumTotLbsYr = select(., "Total.Pounds..lb.yr._Solids, total dissolved":"Total.Pounds..lb.yr._Naphthalene") %>% rowSums(na.rm = TRUE))
colnames(mydfwd2)
str(mydfwd2$SumTotLbsYr)

# create summary column for Total TWPE pounds per year

colnames(mydfwd2)
mydfwd3 <- mydfwd2 %>%
  mutate(SumTotTWPELbsYr = select(., "Total.TWPE..lb.eq.yr._Solids, total dissolved":"Total.TWPE..lb.eq.yr._Naphthalene") %>% rowSums(na.rm = TRUE))
colnames(mydfwd3)
str(mydfwd3$SumTotTWPELbsYr)

# create summary column for potential outliers per facility per year

colnames(mydfwd3)
mydfwd4 <- mydfwd3 %>%
  mutate(SumPotOutlierYr = select(., "PotOutlier_Solids, total dissolved":"PotOutlier_Naphthalene") %>% rowSums(na.rm = TRUE))
colnames(mydfwd4)
str(mydfwd4$SumPotOutlierYr)

# Simplify data to columns of interest (summary columns)

colnames(mydfwd4)

DMR1822wd<- mydfwd4[c("Year", "fac.ci.co.st", "Facility.Name", "City", "County" , "State",
                   "HUC.12.Code" , "Facility.Latitude",  
                   "Facility.Longitude", "SumTotLbsYr" , "SumTotTWPELbsYr", "SumPotOutlierYr" )]  
colnames(DMR1822wd)

# Pivot wider:
# summing PotOutliers as well as total lbs so that each facility has one only row, and years are represented by columns
# with yr attached to (1) SumTotLbsYr, (2) SumTotTWPELbsYr, (3) SumPotOutlierYr

DMR1822wdr <- DMR1822wd %>%
  pivot_wider(
    names_from = Year,
    values_from = c(SumTotLbsYr, SumTotTWPELbsYr, SumPotOutlierYr),
    values_fn = sum
  )

nrow(DMR1822wdr)
colnames(DMR1822wdr)
str(DMR1822wdr)

getwd()
write.csv(DMR1822wdr,'DMR1822widerV1.csv')

# create summary column for Total pounds 
# using tidyverse mutate

colnames(DMR1822wdr)

mydfwdr <- DMR1822wdr %>%
  mutate(SumTotLbs1822 = select(., "SumTotLbsYr_2018":"SumTotLbsYr_2022") %>% rowSums(na.rm = TRUE))
colnames(mydfwdr)
str(mydfwdr$SumTotLbs1822)

# create summary column for Total TWPE pounds

mydfwdr2 <- mydfwdr %>%
  mutate(SumTotTWPELbs1822 = select(., "SumTotTWPELbsYr_2018":"SumTotTWPELbsYr_2022") %>% rowSums(na.rm = TRUE))
colnames(mydfwdr2)
str(mydfwdr2$SumTotTWPELbs1822)

# create a summary column for total outliers

mydfwdr3 <- mydfwdr2 %>%
  mutate(SumPotOutliers1822 = select(., "SumPotOutlierYr_2018":"SumPotOutlierYr_2022") %>% rowSums(na.rm = TRUE))
colnames(mydfwdr3)
str(mydfwdr3$SumPotOutliers1822)


#########################################################
##### ANALYSIS, all years data combined #####
#########################################################

# Note: data of interest = total pounds per year

### Top 5 polluters, all pollutants combined, 2018-2022 cumulative, by amount

## To return top & bottom 10:

head(mydfwdr3[order(-mydfwdr3$SumTotLbs1822),],10)
tail(mydfwdr3[order(-mydfwdr3$SumTotLbs1822),],10)                                

# export as csv

getwd()
write.csv(mydfwdr3,'DMR1822summaries by plant.csv')

###############################################################
###############################################################
###
###               DMR SUMMARY                             ###
###
###############################################################
###############################################################

DMR1822sumbyplant <- read.csv(file="DMR1822summaries by plant.csv", header=TRUE, sep=",")
colnames(DMR1822sumbyplant)
nrow(DMR1822sumbyplant)
str(DMR1822sumbyplant)

# Count of unique facilities
length(unique(DMR1822sumbyplant$fac.ci.co.st))

# Count number of unique states 
length(unique(DMR1822sumbyplant$State))

# which states
unique(DMR1822sumbyplant$State)

# which pollutants
# use older df, removing duplicates
unique(DMR1822v2$Pollutant.Name)

# how many outliers
sum(DMR1822sumbyplant$SumPotOutliers1822)

#total pounds discharge, all plants
sum(DMR1822sumbyplant$SumTotLbs1822)

# total pounds hazard weighted discharge, all plants
sum(DMR1822sumbyplant$SumTotTWPELbs1822)

##############
### Worst offender facilities  ###
#############

# Focus on Total pounds, not hazard weighted pounds, for simplicity in messaging

colnames(DMR1822sumbyplant)
nrow(DMR1822sumbyplant)

# Create simple dataframe w data of interest
facilitydf <- DMR1822sumbyplant[c("Facility.Name", "City" ,  "County" , "State" , 
                               "Facility.Latitude" , "Facility.Longitude" , "SumTotLbs1822")]
nrow(facilitydf)

colnames(facilitydf)
str(facilitydf)

getwd()
write.csv(facilitydf,'Facility summary_DMR total lbs_1822.csv')

##############
### States w worst offenders ###
#############

colnames(DMR1822sumbyplant)
nrow(DMR1822sumbyplant)

# Create simple dataframe w data of interest
tempdf <- DMR1822sumbyplant[c("State" , "SumTotLbs1822" )]
nrow(tempdf)

# Combine rows w same column value for states, sum other column values

colnames(tempdf)

statesdf <- tempdf %>%
  group_by(State) %>%
  summarise(across(c(SumTotLbs1822), sum))

nrow(statesdf)
colnames(statesdf)
str(statesdf)

getwd()
write.csv(statesdf,'State summary_DMR total lbs_1822.csv')

### END ###

