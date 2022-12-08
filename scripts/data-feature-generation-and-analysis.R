# Load data and packages:
library(tidyverse)
library(readr)
acled <- readRDS('output-data/acled_cache.RDS')
library(lubridate)
acled$date <- date(acled$date)
acled <- acled[order(acled$date), ]
acled <- acled[!is.na(acled$iso3c), ]

# 1.  Export map data ------------------------
iran <- acled[acled$country == 'Iran', ]
write_csv(iran, 'output-data/iran_map_data.csv')

# Restrict to last day of complete data:
acled <- acled[acled$date < max(acled$date), ]

# 2. Generate data for side charts ------------------------
# Expand to country-day dataset:
iso_date_grid <- data.frame()
for(i in na.omit(unique(acled$iso3c))){
  dates <- min(acled$date[acled$iso3c == i], na.rm = T):Sys.Date()
  dates <- as.Date(dates, origin = '1970-01-01')
  iso_date_grid <- rbind(iso_date_grid, data.frame(iso3c = rep(i, length(dates)),
                                                   date = dates))
}

# Restrict to those not present in original data
acled$iso3c_date <- paste0(acled$iso3c, '-', as.Date(acled$date))
iso_date_grid$iso3c_date <- paste0(iso_date_grid$iso3c, '-', as.Date(iso_date_grid$date))
iso_date_grid <- iso_date_grid[!iso_date_grid$iso3c_date %in% acled$iso3c_date, ]
iso_date_grid$iso3c_date <- NULL

# Merge in the relevant monthly and time-invariant values
library(lubridate)
iso_date_grid$month <- month(iso_date_grid$date)
iso_date_grid$year <- year(iso_date_grid$date)
iso_date_grid <- merge(iso_date_grid, 
                       unique(acled[, c("year", "iso3c", "iso", "region", 
                                        "country", "iso3",
                                        "month", "iso2c", "total_pop")]),
                       by = c('year', 'iso3c', 'month'),
                       all.x = T)

# Set values which by definition are zero (since these dates do not figure in the original data) to zero
for(i in c('fatalities',
           "country_date_deaths_in_all_events",
           "country_date_protest_deaths",
           "country_date_protest_deadly_events_n")){
  iso_date_grid[, i] <- 0
}

# Generate appropriate week values and the iso3c_date column
iso_date_grid$week <- week(iso_date_grid$date)
iso_date_grid$week_and_year <- paste0(iso_date_grid$year, '_', iso_date_grid$week)
iso_date_grid$iso3c_date <- paste0(iso_date_grid$iso3c, '_', iso_date_grid$date)

# Set the rest of missing columns to NA
for(i in setdiff(colnames(acled), colnames(iso_date_grid))){
  iso_date_grid[, i] <- NA
}

# Combine the two for a data set without missing country-days
acled <- rbind(acled, iso_date_grid[, colnames(acled)])

# Construct time interval and unit variables
# Generate ids
acled$month_and_year <- paste0(acled$year, '_', acled$month)
acled$country_month_year <- paste0(acled$month_and_year, '-', acled$country)
acled$country_date <- paste0(acled$iso3c, '_', acled$date)

# Generate total killed across events per month and day
acled$country_month_year_deaths_in_all_events <- ave(acled$fatalities, 
                                                     acled$country_month_year, FUN = function(x) sum(x, na.rm = T))
acled$country_date_deaths_in_all_events <- ave(acled$fatalities, acled$country_date, FUN = function(x) sum(x, na.rm = T))

# Generate total killed in protests per month and date
acled$protest_deaths <- ifelse(acled$event_type == 'Protests', acled$fatalities, 0)
acled$country_month_year_protest_deaths <- ave(acled$protest_deaths, 
                                               acled$country_month_year, FUN = function(x) sum(x, na.rm = T))
acled$country_date_protest_deaths <- ave(acled$protest_deaths, 
                                         acled$country_date, FUN = function(x) sum(x, na.rm = T))
acled$country_date_protests_n <- ave(acled$event_type, 
                                         acled$country_date, FUN = function(x) sum(x == 'Protests', na.rm = T))
acled$country_date_protests_n <- as.numeric(acled$country_date_protests_n)

# Generate deadly protests per month and date number column
acled$country_month_year_protest_deadly_events_n <- ave(acled$protest_deaths, acled$country_month_year, FUN = function(x) sum(x > 0, na.rm = T))
acled$country_date_protest_deadly_events_n <- ave(acled$protest_deaths, acled$country_date, FUN = function(x) sum(x > 0, na.rm = T))


# Get in the population affected % (and cities/towns over 1,000 population)
# Source: https://public.opendatasoft.com/explore/dataset/geonames-all-cities-with-a-population-1000/export/?disjunctive.cou_name_en&sort=name
# acled <- acled_bc

cities <- read_delim('source-data/geonames-all-cities-with-a-population-1000.csv')
coords <- data.frame(do.call('rbind', strsplit(as.character(cities$Coordinates),',',fixed=TRUE)))
colnames(coords) <- c('lat', 'lng')
coords[, 1] <- as.numeric(coords[, 1])
coords[, 2] <- as.numeric(coords[, 2])
cities$Coordinates <- NULL
cities <- cbind(cities, coords)
# plot(coords[, 2], coords[, 1]) # Uncomment this to see geographical coverage

# Combine smaller cities into larger ones if they are very close together
cities$rounded_lat <- round(cities$lat, 1)
cities$rounded_lng <- round(cities$lng, 1)
cities <- cities[rev(order(cities$Population)), ]
cities$Population <- ave(cities$Population, paste0(cities$lat, '_', cities$lng), FUN = function(x) sum(x, na.rm = T))
cities <- cities[!duplicated(paste0(cities$lat, '_', cities$lng)), ]
library(countrycode)
cities$iso3c <- countrycode(cities$`Country Code`, 'iso2c', 'iso3c')
cities <- cities[!is.na(cities$iso3c), ]
cities$country_urban_pop <- ave(cities$Population, cities$iso3c, FUN = function(x) sum(x, na.rm = T))

acled$rounded_lat <- round(as.numeric(acled$latitude), 1)
acled$rounded_lng <- round(as.numeric(acled$longitude), 1)

# Merge this into ACLED
acled <- merge(acled, 
               cities[, c('Geoname ID', 'ASCII Name', 'Population', 'rounded_lat', 'rounded_lng', 'iso3c')], 
               by = c('rounded_lat', 'rounded_lng', 'iso3c'), all.x = T)

acled <- merge(acled, unique(cities[, c('iso3c', 'country_urban_pop')]), 
               by = 'iso3c', all.x= T)             
               
# Number of people affected by protest in past 30 days
cache_geographical_spread <- T
if(!cache_geographical_spread){
  selection_frame <- acled[acled$event_type == 'Protests', 
                           c('country_date', 'date', 'iso3c', 
                             'Population', 'Geoname ID', 'fatalities')]
  selection_frame <- selection_frame[!is.na(selection_frame$iso3c), ]
  check <- unique(acled[, c('iso3c', 'date')])
  acled$population_affected_by_protest_last_30_days <- NA
  acled$population_affected_by_deadly_protest_last_30_days <- NA
  last <- Sys.time()
  result <- data.frame(matrix(nrow = nrow(check), ncol=6))
  ticker <- 1
  total <- nrow(check)
#  for(j in unique(check$iso3c)){
  for(j in c("IRN")){
  country_data <- selection_frame[selection_frame$iso3c == j, ]
    for(i in unique(acled$date[acled$iso3c == j])){
      # 90-day
      temp <- country_data[as.numeric(country_data$date) %in% 
                        (as.numeric(i)-89):as.numeric(i), ]
      temp_result_1 <- na.omit(c(sum(temp$Population[!duplicated(temp$`Geoname ID`)], na.rm = T), 0))[1]
      temp <- temp[temp$fatalities > 0, ]
      if(nrow(temp) > 0){
        temp_result_2 <- na.omit(c(sum(temp$Population[!duplicated(temp$`Geoname ID`)], na.rm = T), 0))[1]
      } else {
        temp_result_2 <- 0
      }
      # 30-day
      temp <- country_data[as.numeric(country_data$date) %in% (as.numeric(i)-29):as.numeric(i), ]
      temp_result_3 <- na.omit(c(sum(temp$Population[!duplicated(temp$`Geoname ID`)], na.rm = T), 0))[1]
      temp <- temp[temp$fatalities > 0, ]
      if(nrow(temp) > 0){
        temp_result_4 <- na.omit(c(sum(temp$Population[!duplicated(temp$`Geoname ID`)], na.rm = T), 0))[1]
      } else {
        temp_result_4 <- 0
      }
      result[ticker, ] <- c(temp_result_1, temp_result_2, temp_result_3, temp_result_4, j, i)
      cat(paste0( '\r\r\r\r   ', round(100*ticker/total, 3), ' % complete - ', Sys.time()-last))
      ticker <- ticker + 1
      last <- Sys.time()
    }
  }
  
  # Make this into a data frame:
  library(data.table)
  all_results <- data.frame(result)
  colnames(all_results) <- c('population_affected_by_protest_last_90_days', 'population_affected_by_deadly_protest_last_90_days', 'population_affected_by_protest_last_30_days', 'population_affected_by_deadly_protest_last_30_days', 'iso3c', 'date')
  all_results$date <- as.Date(as.numeric(all_results$date), origin = '1970-01-01')
  all_results[, 1] <- as.numeric(all_results[, 1])
  all_results[, 2] <- as.numeric(all_results[, 2])
  all_results[, 3] <- as.numeric(all_results[, 3])
  all_results[, 4] <- as.numeric(all_results[, 4])
  
  saveRDS(all_results, 'output-data/geographical_spread_cache.RDS')
  } else {
    all_results <- readRDS('output-data/geographical_spread_cache.RDS')
  }

# Merge it back into the data:
acled <- merge(acled, all_results, by = c('iso3c', 'date'), all.x=T)
colnames(acled)

# Doing this as % of population
acled$percent_population_affected_by_protest_last_90_days <- acled$population_affected_by_protest_last_90_days/acled$country_urban_pop
acled$percent_population_affected_by_deadly_protest_last_90_days <- acled$population_affected_by_deadly_protest_last_90_days/acled$country_urban_pop
acled$percent_population_affected_by_protest_last_30_days <- acled$population_affected_by_protest_last_30_days/acled$country_urban_pop
acled$percent_population_affected_by_deadly_protest_last_30_days <- acled$population_affected_by_deadly_protest_last_30_days/acled$country_urban_pop

inspect <- F
if(inspect){
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=population_affected_by_protest_last_30_days))+geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=percent_population_affected_by_protest_last_30_days))+
  geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=population_affected_by_deadly_protest_last_30_days))+
  geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=percent_population_affected_by_deadly_protest_last_30_days))+
  geom_line()

ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=population_affected_by_protest_last_90_days))+geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=percent_population_affected_by_protest_last_90_days))+
  geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=population_affected_by_deadly_protest_last_90_days))+
  geom_line()
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=percent_population_affected_by_deadly_protest_last_90_days))+
  geom_line()
}

# Restrict to unique country-days:
acled <- acled[!duplicated(acled$country_date), ]
acled$date <- as.Date(acled$date)

# Generate day-to-day-variable monthly and 3-month backward counts
acled <- acled[order(acled$date), ]

# Define 30-day backward count function:
backward_count_30 <- function(x){
  temp <- x
    for(i in 1:length(x)){
        x[i] <- sum(temp[max(c(1, i-29)):i], na.rm = T)
    }
  x
}

# Calculate for each relevant variable
acled$country_protest_deadly_events_n_in_past_30_days <- 
  ave(acled$country_date_protest_deadly_events_n, acled$iso3c, FUN = backward_count_30)
acled$country_protest_deaths_in_past_30_days <- 
  ave(acled$country_date_protest_deaths, acled$iso3c, FUN = backward_count_30)
acled$country_deaths_in_all_events_in_past_30_days <- 
  ave(acled$country_date_deaths_in_all_events, acled$iso3c, FUN = backward_count_30)

# Define 90-day backward count function:
backward_count_90 <- function(x){
  temp <- x
  for(i in 1:length(x)){
    x[i] <- sum(temp[max(c(1, i-89)):i], na.rm = T)
  }
  x
}

# Calculate for each variable:
acled$country_protest_deadly_events_n_in_past_90_days <- 
  ave(acled$country_date_protest_deadly_events_n, acled$iso3c, FUN = backward_count_90)
acled$country_protest_deaths_in_past_90_days <- 
  ave(acled$country_date_protest_deaths, acled$iso3c, FUN = backward_count_90)
acled$country_deaths_in_all_events_in_past_90_days <- 
  ave(acled$country_date_deaths_in_all_events, acled$iso3c, FUN = backward_count_90)

# Inspect that this worked:
for(i in 1:10){
test <- acled[sample(1:nrow(acled), 1), ]
  if(test$country_deaths_in_all_events_in_past_90_days == 
     na.omit(c(sum(acled$country_date_deaths_in_all_events[
       as.numeric(acled$date) %in% (as.numeric(test$date)-89):as.numeric(test$date) 
       & acled$iso3c == test$iso3c]), 0))[1]){
    cat('Test '); cat(i); cat(' A - passed. ')
  } else {
    cat('Test '); cat(i); cat(' A - failed')
    stop('')
  }

  if(test$country_protest_deadly_events_n_in_past_30_days == na.omit(c(sum(acled$country_date_protest_deadly_events_n[as.numeric(acled$date) %in% (as.numeric(test$date)-29):as.numeric(test$date) & acled$iso3c == test$iso3c]), 0))[1]){
    cat('Test '); cat(i); cat(' B - passed. ')
  } else {
    cat('Test '); cat(i); cat(' B - failed')
    stop()
  }
 cat('\n')
}
cat('-ALL TESTS PASSED-')

# Export as country-day dataset:
acled <- acled[order(acled$date), ]
write_csv(acled[!duplicated(acled$country_date), 
                c("year", "iso3c", "region", "country", 
                  "date", "week_and_year",
                  "month", "week", "iso2c", 
                  "total_pop", "month_and_year",
                  "country_month_year",
                  "country_month_year_deaths_in_all_events",                    "country_month_year_protest_deaths",        "country_month_year_protest_deadly_events_n",
                "country_date",                             "country_date_deaths_in_all_events", "country_date_protest_deaths",                "country_date_protest_deadly_events_n","country_protest_deadly_events_n_in_past_30_days", "country_protest_deaths_in_past_30_days",          "country_deaths_in_all_events_in_past_30_days", "country_protest_deadly_events_n_in_past_90_days", "country_protest_deaths_in_past_90_days", "country_deaths_in_all_events_in_past_90_days", "country_date_protests_n", "country_urban_pop", "population_affected_by_protest_last_90_days", "population_affected_by_deadly_protest_last_90_days", "population_affected_by_protest_last_30_days",                "population_affected_by_deadly_protest_last_30_days", "percent_population_affected_by_protest_last_90_days",        "percent_population_affected_by_deadly_protest_last_90_days", "percent_population_affected_by_protest_last_30_days",        "percent_population_affected_by_deadly_protest_last_30_days")], 'output-data/country_date_data.csv')
saveRDS(acled[!duplicated(acled$country_date), 
                c("year", "iso3c", "region", "country", 
                  "date", "week_and_year",
                  "month", "week", "iso2c", 
                  "total_pop", "month_and_year",
                  "country_month_year",
                  "country_month_year_deaths_in_all_events",                    "country_month_year_protest_deaths",        "country_month_year_protest_deadly_events_n",
                  "country_date",                             "country_date_deaths_in_all_events", "country_date_protest_deaths",                "country_date_protest_deadly_events_n","country_protest_deadly_events_n_in_past_30_days", "country_protest_deaths_in_past_30_days",          "country_deaths_in_all_events_in_past_30_days", "country_protest_deadly_events_n_in_past_90_days", "country_protest_deaths_in_past_90_days", "country_deaths_in_all_events_in_past_90_days", "country_date_protests_n", "country_urban_pop", "population_affected_by_protest_last_90_days", "population_affected_by_deadly_protest_last_90_days", "population_affected_by_protest_last_30_days",                "population_affected_by_deadly_protest_last_30_days", "percent_population_affected_by_protest_last_90_days",        "percent_population_affected_by_deadly_protest_last_90_days", "percent_population_affected_by_protest_last_30_days",        "percent_population_affected_by_deadly_protest_last_30_days")], 'output-data/country_date_data.RDS')
write_csv(acled[!duplicated(acled$country_month_year), 
                c("year", "iso3c", "region", "country", 
                  "date", "week_and_year",
                  "month", "week", "iso2c", 
                  "total_pop", "month_and_year",
                  "country_month_year",
                  "country_month_year_deaths_in_all_events",                    "country_month_year_protest_deaths",        "country_month_year_protest_deadly_events_n",
                  "country_date",                             "country_date_deaths_in_all_events", "country_date_protest_deaths",                "country_date_protest_deadly_events_n", "country_protest_deadly_events_n_in_past_30_days",  "country_protest_deaths_in_past_30_days", "country_deaths_in_all_events_in_past_30_days", "country_protest_deadly_events_n_in_past_90_days", "country_protest_deaths_in_past_90_days", "country_deaths_in_all_events_in_past_90_days", "country_urban_pop", "population_affected_by_protest_last_90_days", "population_affected_by_deadly_protest_last_90_days", "population_affected_by_protest_last_30_days",                "population_affected_by_deadly_protest_last_30_days", "percent_population_affected_by_protest_last_90_days",        "percent_population_affected_by_deadly_protest_last_90_days", "percent_population_affected_by_protest_last_30_days",        "percent_population_affected_by_deadly_protest_last_30_days")], 'output-data/country_month_data.csv')