# Load data and packages
library(tidyverse)
library(readr)
library(ggplot2)
library(anytime)
library(readxl)

update_acled <- F
if(update_acled == T){
# Load data from ACLED
acled <- data.frame()
for(i in dir('source-data/acled')){
  acled <- rbind(acled, read_csv(paste0('source-data/acled/', i)))
}

acled$date <- anytime(acled$event_date, 
                      calcUnique = T)

# Get list of deleted ACLED events:
deleted_acled <- read_xlsx('source-data/acled-deleted/Deleted_Events_Dec02.xlsx')
acled <- acled[!acled$event_id_cnty %in% deleted_acled$EVENT_ID_CNTY, ]

# This ensures we use the most recently updated entry for a given data_id (as files were loaded in order).
acled <- acled[nrow(acled):1, ] 
acled <- acled[!duplicated(acled$event_id_cnty), ]

# We include violent demonstrations in our protest category (note, some move back and forth between Protests and Riots / Violent demonstration categories)
acled$event_type[acled$event_type == 'Riots' & acled$sub_event_type == 'Violent demonstration'] <- 'Protests'

# Get date
acled$date <- anytime(acled$event_date, 
                      calcUnique = T)
library(lubridate)
acled$week_and_year <- paste0(year(acled$date), '_', week(acled$date))
acled$month <- month(acled$date)
acled$week <- week(acled$date)

library(WDI)
pops <- WDI(indicator = c('total_pop' = 'SP.POP.TOTL'))
library(countrycode)
pops$iso3c <- countrycode(pops$iso2c, 'iso2c', 'iso3c')
pops$country <- NULL
pops <- pops[!is.na(pops$iso3c), ]
# Add in 2022
pops_2022 <- pops[pops$year == 2021,] 
pops_2022$year <- 2022 
pops <- rbind(pops, pops_2022)
pops <- unique(pops)

# Assume forward fill populations starting in 2019 if missing:
pops <- pops[order(pops$year), ]
pops$total_pop[pops$year >= 2019] <- ave(pops$total_pop[pops$year >= 2019], pops$iso3c[pops$year >= 2019], FUN = 
                        function(x){
                          for(i in 2:length(x)){
                             if(is.na(x[i])){
                               x[i] <- x[i-1]
                             }
                          }
                          x
                        })

acled$iso3c <- countrycode(acled$country, 'country.name', 'iso3c')
acled <- merge(acled, pops, by = c('year', 'iso3c'), all.x=T)

# Generating / loading cache to speed up process:
saveRDS(acled, 'output-data/acled_cache.RDS')} else {
acled <- readRDS('output-data/acled_cache.RDS')}

# See what current protests in Iran are about:
inspect <- F
if(inspect){
# Subset to Iran:
iran <- acled[acled$country == 'Iran', ]

# Generate current protest column:
iran$current_protests <- iran$date >= as.Date('2022-09-16')

#set.seed(123)
#protest_sample <- sample#(iran$notes[iran$current_protests & #iran$event_type == 'Protests'], 100)
#View(protest_sample)
# 99% were explicitly anti-government and connected to the current national movement

# Confirm that all deadly protests have been anti-regime protests:
unlist(iran[iran$event_type == 'Protests' & 
       iran$year == 2022 & 
       iran$date >= as.Date('2022-09-16') &
       iran$fatalities > 0, 'notes'])
}
