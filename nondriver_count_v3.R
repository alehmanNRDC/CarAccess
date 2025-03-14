# non driver count 
# by Ali Lehman
# Written March 2025

rm(list = ls()) #This clears the work space
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #sets working directory to active document

# package upload ---- 

packages <- c(
  'quarto',
  'sf',
  'tidyverse',
  'mapview',
  'dplyr',
  'rjson',  
  'jsonlite',
  'tigris',
  'tidycensus',
  'data.table',
  'viridis',
  'fs',
  'here',
  'osmdata',
  'sfnetwork',
  'ggplot2',
  "readr", 
  "osmdata",
  'dodgr'
)

if(!require(pacman)) install.packages('pacman')
pacman::p_load(packages,character.only = T)

# Data upload -----
## NHTS person and trip report
NHTS_personReport<-read.csv(here("inputs/perv2pub_NHTS.csv"))
NHTS_tripReport<-read.csv(here("inputs/tripv2pub_NHTS.csv"))

## Census tract file with geometries 
geometries <- st_read(here("inputs/USA_Census_Tract_Boundaries.shp"))

## 2020 Census (Demographic and Housing Characteristics File) with rural and urban population
census2020<-read.csv(here("inputs/USA Census 2020 Urban Population.csv"), 
                     colClasses = c(Geographic.Identifier = "character"))

## FHWA Annual state VMT
VMTFHWA<-read.csv(here("inputs/FHWA2022VMT.csv"))


## ACS B08201
## Census API to pull household car ownership
vars = load_variables( 2020,'acs5') |> 
  as.data.table()

B08201 <- vars %>% 
  filter(grepl('B08201', name)) %>% 
  slice(7:30) %>%  
  pull(name)       # Creates a list of all the B08201 sub varibales 

# add, B01003_001, total population
B08201 <- c(B08201, "B01003_001")

# Create a named vector where P= the number of people in a household and V= the number of vehicles  
variable_names <- set_names(
  B08201,
  c('TP1', 'P1V0', 'P1V1', 'P1V2', 'P1V3', 'P1V4', 
    'TP2', 'P2V0', 'P2V1', 'P2V2', 'P2V3', 'P2V4', 
    'TP3', 'P3V0', 'P3V1', 'P3V2', 'P3V3', 'P3V4', 
    'TP4', 'P4V0', 'P4V1', 'P4V2', 'P4V3', 'P4V4', 
    'TotalPopulation')
)


# Loop through the states, pulling data at the census tract level from census API 
state_codes <- unique(fips_codes$state_code)[1:51] 
pop <- get_acs(
  geography = "tract", 
  variables = variable_names,  
  state = state_codes,         # Must specify states
  geometry = FALSE             
)


## ACS B01001, B11005_001, and B11005_002    -----
## ACS age data. B11005_001, and B11005_002 are the number of households and number of households with kids, respectively 

age <-  #ACS reports age by gender so we have to pull age groups for "men" and "women" then consolidate
  get_acs(geography = "tract", 
          variables = 	c("B01001_003","B01001_027",
                         "B01001_004","B01001_028", 
                         "B01001_005", "B01001_029", 
                         "B01001_023", "B01001_024",
                         "B01001_047", "B01001_048",
                         "B01001_025", "B01001_049", 
                         "B01001_001", "B11005_001",
                         "B11005_002"),
          state = state_codes,
          geometry = FALSE)


# Analysis ----
### 1. Number of people in  ---- 

# reformat ACS API output
ACS2021 = 
  pivot_wider(
    select(pop,NAME, GEOID, variable, estimate),
    names_from = 'variable',
    values_from = 'estimate'
  )%>%
  mutate(NAME = sub(".*,\\s*.*,\\s*", "", NAME))


# A. Develop factor to account for the fact that 4+ people households might have more than 4 people 

hh_size<- ACS2021%>% 
  mutate(pop_3orless=TP1 + 2*TP2+ 3*TP3)%>% # ppl in 4 or more p hh = [average size] * [# 4+ p hh] = state pop - [# 1p hh] - 2* [# 2 p hh] - 3* [# 3 p hh] 
  mutate(pop_4andmore= TotalPopulation - pop_3orless)%>%
  mutate(avgsize4and= pop_4andmore/  TP4)%>%
  select(GEOID, avgsize4and)

ACS2021<-merge(ACS2021,hh_size, by="GEOID") #add the adjustment factor into ACS dataframe 
ACS2021$avgsize4and <- ifelse(is.na(ACS2021$avgsize4and) | 
                                ACS2021$avgsize4and < 4 | 
                                ACS2021$avgsize4and > 10 | 
                                is.infinite(ACS2021$avgsize4and), 
                              4, 
                              ACS2021$avgsize4and)  # Replace values in avgsize4and that are less than 4, greater than 10, or invalid (NA/Inf)

# B. total people in households without any cars
ACS2021<- ACS2021%>%
  mutate(ZeroCarPPL= 
  P1V0+
  2*P2V0+
  3*P3V0+ 
  avgsize4and*P4V0) 

# C. person hh and one or more vehicles
ACS2021<- ACS2021%>%
  mutate(singleWcar= 
  P1V1+
  P1V2+
  P1V3+
  P1V4)


# D.i. householders in a house w one or more cars
# count 1 householders per house with a car
ACS2021<- ACS2021%>%
  mutate(hoseholder_car= 
  P2V1+
  P2V2+
  P3V1+
  P3V2+
  P3V3+
  P4V1+
  P4V2+
  P4V3+
  P4V4)

# C.ii. non-householders but 1 to 1 ratio, assuming they are all driving
ACS2021<- ACS2021%>%
  mutate(nonhoseholder_car_maybe=
  #if 2 cars, only one non householder can have a car, since householder has the 1st car
  P2V2+ # 1 non hh has car 
  P2V3+ # if more cars than ppl, still only one driver
  P2V4+ # if more cars than ppl, still only one driver
  P3V2+ # 1 non hh has car
  P4V2+ # 1 non hh has car+
  # if 3 cars, 2 non hh can have cars 
  2*P3V3+# 2 non hh has car+
  2*P3V4+# if more cars than ppl, still only 2 drivers
  2*P4V3+ # 2 non hh has car+
  # if 4 cars, 3 non hh can have car 
  3*P4V4)



# C.iii. non-householders where there are less cars than people
ACS2021<- ACS2021%>%
  mutate( nonhouseholder_nocar=
  P2V1+
  2*P3V1+
  (avgsize4and-1)*P4V1+ # 1 person doesnt have car, the rest do
  
  P3V2+
  (avgsize4and-2)* P4V2 +
  (avgsize4and-3)* P4V3 +
  (avgsize4and-4)* P4V4)


## 2. Summarize ACS age output -----
ACSage =
  pivot_wider(
    select(age,GEOID, variable, estimate),
    names_from = 'variable',
    values_from = 'estimate'
  )%>% 
  rename(hh_totalcount = 15, hh_w_kids = 16)

ACSage<-  ACSage %>%
  mutate(over80 = B01001_025+ B01001_049+ B01001_024+ B01001_048)%>%
  mutate(under5 = B01001_003+ B01001_027)%>%
  mutate(young5to15=B01001_004+ B01001_005+ B01001_028+ B01001_029)%>%
  mutate(late70s=B01001_023+B01001_047)%>%
  mutate(over75=B01001_023+B01001_047 +B01001_025+ B01001_049+ B01001_024+ B01001_048)%>%
  mutate(pHHwKids=100*(hh_w_kids/hh_totalcount))%>%
  select(1,15:22)

workingDF<-merge(ACS2021,ACSage, by="GEOID")%>%
  mutate(pkid=(young5to15+under5)/TotalPopulation)


## 3. Rural area flag -----
# Using 2020 census rural and urban population. If 50% or more of the population in a census tract is rural, it is flagged as a rural tract 

ruralCensus2020<-census2020%>%
  select(Geographic.Identifier,Urban.population,Rural.population)%>%
  mutate(pRural=Rural.population/(Urban.population+Rural.population))

ruralCensus2020$ruralFlag <- ifelse(ruralCensus2020$pRural > 0.5, 1, 0)

workingDF<- merge(workingDF,ruralCensus2020, by.x="GEOID", by.y="Geographic.Identifier")



## 4. People that don't have a car  ----
#  adjust the conditional driver total to account for the fact some might be kids 
workingDF<- workingDF%>%
  mutate( noCar=(pkid * nonhoseholder_car_maybe) + nonhouseholder_nocar + ZeroCarPPL)%>%
## 5. Car owners ----
  mutate( ownCar= ((1-pkid) * nonhoseholder_car_maybe)+
  hoseholder_car +
  singleWcar)


## 6. Drivers with travel limiting disabilities----

drivers_TL = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$CONDNONE == "2"], 
  na.rm = TRUE)

drivers_all = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1"], 
  na.rm = TRUE)

 # PTLD= [drivers with travel limiting disabilities] / [all drivers]
PTLD= drivers_TL/drivers_all

workingDF$TLdisability <- workingDF$ownCar *PTLD


## 7.  mental illness considerations -----
# Substance Abuse and Mental Health Services Administration & U.S. Department of Health and Human Services; https://www.samhsa.gov/data/sites/default/files/reports/rpt39443/2021NSDUHFFRRev010323.pdf
# 5.5 percent of adults aged 18 or older had serious mental illness (SMI) in the past year
americanMI<-0.055

# Journal of Transport & Health;  https://www.sciencedirect.com/science/article/abs/pii/S2214140521001730#sec12	
pcantdrivesometimes = 0.228 

pMentalIllTL= pcantdrivesometimes * americanMI # percent to apply
workingDF$TLmentalill<- workingDF$ownCar *pMentalIllTL

## 8. People 75+ and NHTS ----
drivers_over75 = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1" & 
    NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)

all_over75 = sum(NHTS_personReport$WTPERFIN[
    NHTS_personReport$R_AGE >= 75], 
  na.rm = TRUE)

drivers_over75_TLD = sum(NHTS_personReport$WTPERFIN[
  NHTS_personReport$DRIVER == "1"& 
    NHTS_personReport$R_AGE >= 75& 
    NHTS_personReport$CONDNONE == "2"], 
  na.rm = TRUE)

pTLD75= drivers_over75_TLD / drivers_TL
pdrivers_over75_notTL=(drivers_over75*(1-pTLD75))/drivers_all

pDrivingover75 = drivers_over75 /all_over75 
pelderlyTLD = drivers_over75_TLD / all_over75 

workingDF$elderlyDrivers<- pDrivingover75 * workingDF$over75
workingDF$elderly_driver_TLD<- pelderlyTLD * workingDF$over75
workingDF$elderlyNotDriving<- (1-pDrivingover75)* workingDF$over75

## 9. Burden of chauffeuring-----
chauferTrips = sum(NHTS_tripReport$WTTRDFIN[
  NHTS_tripReport$WHYTRP1S == "70"], 
  na.rm = TRUE)
allTypesTrips = sum(NHTS_tripReport$WTTRDFIN, 
  na.rm = TRUE)

pChauferedTrip<-as.numeric(chauferTrips / allTypesTrips)

averageSL=25 #
#
costpMile=0.81 # USDOT BTS  	National maintence cost per mile (2022)
# https://data.bts.gov/stories/s/Transportation-Economic-Trends-Transportation-Spen/bzt6-t8cd/#:~:text=2022%20Year%2Din%2DReview,2020%20%E2%80%93%20at%20%240.44%20per%20mile.

chauffeuringDF<- VMTFHWA%>%
  mutate(chaufferedVMT=as.numeric(TOTAL)*pChauferedTrip)%>%
  mutate(chaufferedcost=chaufferedVMT)%>%
  mutate(chaufferedhours=chaufferedVMT/averageSL)%>%
  select(STATE,chaufferedVMT,chaufferedhours,chaufferedcost)

## 10. Gendered  chauffeuring
# create a ID crosswalk and combine the trip and person reports
NHTS_tripReport$FULLID<- NHTS_tripReport$HOUSEID +NHTS_tripReport$PERSONID
NHTS_personReport$FULLID<- NHTS_personReport$HOUSEID + NHTS_personReport$PERSONID

NHTS_chauferANDtripReport<-merge(NHTS_tripReport,NHTS_personReport,by="FULLID")


chauferTrips = sum(NHTS_tripReport$WTTRDFIN[
  NHTS_tripReport$WHYTRP1S == "70"], 
  na.rm = TRUE)
allTypesTrips = sum(NHTS_tripReport$WTTRDFIN, 
                    na.rm = TRUE)

# Filter the dataset for chauffeured trips and gender 
chauffered_trips_df <- NHTS_chauferANDtripReport[NHTS_chauferANDtripReport$WHYTRP1S == "70", ]

aggregated_data_chauf <- aggregate(WTTRDFIN ~ R_SEX.x, data = chauffered_trips_df, sum)
aggregated_data_all <- aggregate(WTTRDFIN ~ R_SEX.x, data = NHTS_chauferANDtripReport, sum)

aggregated_data_chauf$TripType <- "Chauffeured"
aggregated_data_all$TripType <- "All"

sex_trips <- rbind(aggregated_data_chauf, aggregated_data_all)%>%
  subset( R_SEX.x %in% c(1, 2))

total_weights <- aggregate(WTTRDFIN ~ TripType, data = sex_trips, sum)
sex_trips <- merge(sex_trips, total_weights, by = "TripType", suffixes = c("", "_Total"))

sex_trips$Percentage <- (sex_trips$WTTRDFIN / sex_trips$WTTRDFIN_Total) * 100




#Add geometries ----
geofile <- geometries %>%
  select(FIPS, geometry)%>%
  merge(workingDF, by.x="FIPS", by.y = "GEOID", all.x=TRUE)

# Summary calculations -----
geofile<-geofile%>%
# Normalized (by population) car-deficit count, used in map 
  select(1:3,29,34,35:54)%>%
  mutate(normaltl=(noCar+TLdisability+ 
                     TLmentalill+
                     elderlyDrivers - 
                     elderly_driver_TLD) / TotalPopulation)%>%
    
  # Total deficient (travel limiting disability and  75+)
    mutate( carconstrained = TLdisability+ TLmentalill+ elderlyDrivers - elderly_driver_TLD ) %>%
  
  # Fully car-equipped 
    mutate( unlimitedcar =ownCar-carconstrained)%>%
    
  # Rural car-deficit 
    mutate(RURALtlandnocar=ruralFlag*(noCar+TLdisability+ TLmentalill+ elderlyDrivers - elderly_driver_TLD))%>%
             
  # Rural travel limited 
    mutate(RURALtl=ruralFlag*(TLdisability+
                                      TLmentalill+
                                elderlyDrivers - 
                                      elderly_driver_TLD))%>%
  # Percent of rural no car
    mutate(RURALnoCar=ruralFlag*(noCar))%>%
  # Percent of rural and urban/suburban car deficit   
    mutate(pruralnocar= 100*(RURALtlandnocar/TotalPopulation))%>%
    mutate(pmetronocar = ifelse(ruralFlag == 0, 
                              100 * (carconstrained + noCar) / TotalPopulation, 
                              NA))%>%
  # kids adjustments
  mutate(kids=under5+young5to15)%>%
  mutate(noCarAdults=noCar-kids)%>%
  mutate(carConstrainedandDeficit=carconstrained+noCar)
  
write_sf(geofile, r"(C:\Users\alehman\Desktop\scripts\nondriver\workingDF2.shp)")



# Cost of Housing and Transportation Index Data ------
# Here I pull data from the H&T Index website  
library(dplyr)
library(readr)
library(httr)
library(zip)


# Use a loop of state FIPS to edit the base download link for the state files
base_url <- "https://htaindex.cnt.org/download/download.php?data_yr=2022&focus=tract&geoid="

state_fips <- sprintf("%02d", c(1:56))
state_fips[state_fips == "11"] <- "11"  # Ensure DC is included properly


temp_dir <- tempdir()
dir.create(file.path(temp_dir, "hta_downloads"), showWarnings = FALSE)
df_list <- list()

# Loop over states to download, unzip, and read the CSVs
for (fips in state_fips) {
  # Construct file URL
  file_url <- paste0(base_url, fips)
  
  # Define file paths
  zip_file <- file.path(temp_dir, paste0("state_", fips, ".zip"))
  extract_folder <- file.path(temp_dir, paste0("extracted_", fips))
  dir.create(extract_folder, showWarnings = FALSE)
  
  # Download the file
  tryCatch({
    GET(file_url, write_disk(zip_file, overwrite = TRUE))
    
    # Unzip the files
    unzip(zip_file, exdir = extract_folder)
    
    # Identify the CSV file inside
    csv_file <- list.files(extract_folder, pattern = "\\.csv$", full.names = TRUE)
    
    # Read CSV if found
    if (length(csv_file) > 0) {
      df <- read_csv(csv_file[1], show_col_types = FALSE)
      df$state_fips <- fips  # Add state identifier
      df_list[[fips]] <- df
    }
    
    message("Processed: ", fips)
    
  }, error = function(e) {
    message("Error processing: ", fips, " - ", e$message)
  })
}


HandTdata <- bind_rows(df_list)%>%
  select(auto_ownership_cost_ami,
        vmt_cost_80ami,
        h_cost,
        emp_gravity,
        tract)%>%
  mutate(tract = substr(as.character(tract), 2, nchar(as.character(tract))-1))%>%
  mutate(transitCost=auto_ownership_cost_ami+vmt_cost_80ami)


costdata<-census2020%>%
  mutate(pRural=Rural.population/(Urban.population+Rural.population))%>%
  mutate(ruralFlag=ifelse(pRural > 0.5, 1, 0))%>%
  select(Geographic.Identifier,ruralFlag,State)%>%
  merge(HandTdata, by.x="Geographic.Identifier", by.y = "tract")







