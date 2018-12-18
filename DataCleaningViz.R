
## Import data 
setwd(dir='Documents/Penn/SeniorFall/Practicum Project/')
setwd(dir='Users/adityasrivatsan/')
load(file = 'crime.rdata')
load(file='landuse.rdata')
load(file='in_county_dens.rdata')

install.packages("xlsx")
library(xlsx)
write.table(crime, "PCrime_Original.txt",sep='\t')
write.table(landuse, "PLanduse_Original.txt",sep='\t')
landUseClean = landuse 
landUseClean$geometry <- NULL
landUseClean$block_overlap <- NULL

write.table(in_county, "PCounty_Dens.txt",sep='\t')

## converting all tables to excel to bring it into pandas 

write.table(landUseClean, "LanduseFiltered.txt",sep='\t')

# data cleaning with land use 
## geometry and block_overlap 

dataTract = read.csv('DataYearTract.csv')
dataYM = read.csv('DataYearMonthMax.csv')
## sort 
dataYM = dataYM[order(dataYM$year,dataYM$month),]

removeCol = c('X','YearMonth','tract')
## split training and testing
n = nrow(dataYM)
Xtrain = dataYM[1:round(n * 0.7),!(names(dataYM) %in% removeCol )]
Ytrain = dataYM[1:round(n * 0.7),c('Y')]
Xtest = dataYM[round(n * 0.7):n,!(names(dataYM) %in% removeCol)]
Ytest = dataYM[round(n * 0.7):n,c('Y')]


## 3 models

#totalModel = lm(Y ~.,data = Xtrain)
#summary(totalModel)
#head(Xtrain)
crimeModel = lm(Y ~ year + month, data = Xtrain)
summary(crimeModel)


demographicModel = lm(Y ~ year + month  + white + black + us_indian + 
                        asian + pacific + other_x + two_or_more + 
                        hispanic, data = Xtrain)
summary(demographicModel)

demLandUse = lm(Y ~ year + month  + white + black + us_indian + 
                  asian + pacific + other_x + two_or_more + 
                  hispanic + industrial + civic_institution +  area, data = Xtrain)

landUse= lm(Y ~ year + month + industrial + civic_institution + residential + 
              area, data = Xtrain)
summary(landUse)



## visual
plot(Xtrain$Y,main='Prediction of Crime Rates over time 3 Models',
     xlab = 'Time',ylab='Frequency')
lines(predict(crimeModel),col='red')
lines(predict(demographicModel),col='blue')
lines(predict(landUse),col='yellow')
legend(10,legend=c("Crime", "Demographic","Landuse"),
       col=c("red", "blue","yellow"))


####











####################  census data ###################################################

library(tidyverse)
install.packages('censusapi')
library(censusapi)
install.packages('tidycensus')
library(tidycensus)
library(tidyverse)
install.packages('viridis')
library(viridis)
library(dplyr)

source('setup_demographics.R')


rel_state = 'PA'
rel_county = c('Philadelphia')
## if the above doesn't work in your code, check the FIPS codes.
## can get from data(fips_codes)
your_api_key = 'b85f900174babe3fc5edf61618eb05e6461c1154'
Sys.setenv(CENSUS_KEY=your_api_key)
# Reload .Renviron
readRenviron("~/.Renviron")
# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")


## get a key here: https://api.census.gov/data/key_signup.html
census_api_key(your_api_key,install = T,overwrite = T)


source('setup_demographics.R')

demo_stuff = GetDemoDataGeom(rel_state,
                             rel_county,
                             zero_block = c(18860, 18872), ## prisons
                             years = c('sf1' = 2010, 'acs' = 2017)) ## you should use 2016, or latest
sf1_Data = get_decennial(state = rel_state,
              county = rel_county,
              geography = "block",
              variables = SF1_vars,
              year = 2010,
              output = "wide",
              geometry = TRUE)

block_data = demo_stuff$block_data
block_geom = demo_stuff$block_geom
blockgroup_data = demo_stuff$blockgroup_data
blockgroup_geom = demo_stuff$blockgroup_geom

## for an example:
## ggplot(block_geom %>% filter(censustract < 300)) + geom_sf(aes(fill = as.character(blockgroup)))
## ggplot(blockgroup_geom %>% filter(censustract < 300)) + geom_sf(aes(fill = as.character(blockgroup)))

## add area:
block_geom <- block_geom %>%
  mutate(area = st_area(geometry))
blockgroup_geom <- blockgroup_geom %>%
  mutate(area = st_area(geometry))
## blockgroup loses its unit?

## create a single county shape, for purposes of containment etc
County_geom <- st_union(blockgroup_geom)

## you can adjust how you save all this data

save_location = paste0(data_folder, '/block_and_group.rdata')
save(block_data,
     blockgroup_data,
     block_geom,
     blockgroup_geom,
     County_geom,
     file = save_location)