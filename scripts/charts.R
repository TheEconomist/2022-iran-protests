# Charts:
library(readr)
library(tidyverse)

# Load data
acled <- readRDS('output-data/country_date_data.RDS')
acled <- acled[!is.na(acled$iso3c), ]
iran <- read_csv('output-data/iran_map_data.csv')

# Big map:
ggplot(iran[iran$event_type == 'Protests' & 
              iran$year == 2022 & 
              iran$date >= as.Date('2022-09-16'), ], 
       aes(x=longitude, y=latitude, 
           shape = fatalities > 0,
           size = fatalities,
           col=as.factor(month)))+
  geom_point(alpha = 0.5)+theme_void()+theme(panel.background = element_rect(color = 'white'))+coord_equal()+ggtitle("Iran events, Sep-Nov, 2022")
ggsave('plots/iran_map.png', height = 10, width= 10)

# Iran by day:
iran_by_day <- acled[acled$country == 'Iran', ]
ggplot(iran_by_day[iran_by_day$year == 2022 & 
                     iran_by_day$date >= as.Date('2022-09-16') & iran_by_day$date <= max(iran_by_day$date[iran_by_day$country_date_protests_n > 0], na.rm = T), ], 
       aes(x=date))+geom_line(aes(y=country_date_protest_deaths, col = 'Deaths in protests by day'))+geom_line(aes(y=country_date_protest_deadly_events_n, col = 'Number of deadly protests per day'))+geom_line(aes(y=country_date_protests_n, col = 'Number of protests per day'))+
ggtitle("Iran events, Sep-Nov, 2022")+xlab('')+ylab('')+theme_minimal()+theme(legend.title = element_blank())
ggsave('plots/iran_over_time.png', height = 10, width= 10)


# Geographical spread:
ggplot(iran_by_day[iran_by_day$year == 2022 & 
                     iran_by_day$date >= as.Date('2022-09-16') & iran_by_day$date <= max(iran_by_day$date[iran_by_day$country_date_protests_n > 0], na.rm = T), ], 
       aes(x=date))+geom_line(aes(y=population_affected_by_protest_last_90_days/country_urban_pop, 
                                  col = 'people living in  place with protest in last 90 days'))+
  geom_line(aes(y=population_affected_by_protest_last_30_days/country_urban_pop, 
                col = 'people living in place with protest in last 30 days'))+geom_line(aes(y=population_affected_by_deadly_protest_last_90_days/country_urban_pop, col = 'people living in place with deadly protest in last 90 days'))+
  geom_line(aes(y=population_affected_by_deadly_protest_last_30_days/country_urban_pop, 
                col = 'people living in place with deadly protest in last 30 days'))+
  ggtitle("Iran events, Sep-Nov, 2022, proportion of population in cities and towns")+xlab('')+ylab('')+theme_minimal()+theme(legend.title = element_blank())
ggsave('plots/iran_geography_over_time.png', height = 10, width= 10)


# Bar chart 1:
acled <- acled[order(acled$date), ]
acled$days_of_deadly_protest_in_past_90_days <- ave(acled$country_date_protest_deaths, 
                                    acled$iso3c, FUN = function(x){
                            x <- ifelse(x > 0, 1, 0)
                            temp <- x
                            for(i in 1:length(x)){
                              x[i] <- sum(temp[max(c(1,i-91)):(i-1)])
                            }
                            x
                            })
ggplot(acled[acled$iso3c == 'IRN', ], aes(x=date, y=days_of_deadly_protest_in_past_90_days))+geom_line()+geom_line(aes(y=country_date_protest_deaths, col = 'deaths'))

acled$deadly_events_in_past_90_days_per_m_total_pop <- 1e6*acled$country_protest_deadly_events_n_in_past_90_days/acled$total_pop


bar_chart_1 <- na.omit(acled[acled$total_pop >= 10e6 & acled$days_of_deadly_protest_in_past_90_days > 10, 
                             c('country',
                               'year',
                               'deadly_events_in_past_90_days_per_m_total_pop',
                               'total_pop')])
bar_chart_1 <- bar_chart_1[rev(order(bar_chart_1$deadly_events_in_past_90_days_per_m_total_pop)), ]
bar_chart_1 <- bar_chart_1[!duplicated(paste0(bar_chart_1$country, '_', bar_chart_1$year)), ]

# Bar chart 2
acled$protest_deaths_in_past_90_days_per_m <- 1e6*acled$country_protest_deaths_in_past_90_days/acled$total_pop
bar_chart_2 <- na.omit(acled[acled$total_pop >= 10e6 & acled$days_of_deadly_protest_in_past_90_days > 10, 
                             c('country',
                               'year',
                               'protest_deaths_in_past_90_days_per_m',
                               'total_pop')])
bar_chart_2 <- bar_chart_2[rev(order(bar_chart_2$protest_deaths_in_past_90_days_per_m)), ]
bar_chart_2 <- bar_chart_2[!duplicated(paste0(bar_chart_2$country, '_', bar_chart_2$year)), ]

# Bar chart 3
bar_chart_3 <- na.omit(acled[acled$total_pop >= 10e6 & acled$days_of_deadly_protest_in_past_90_days > 10, 
                             c('country',
                               'year',
                               'country_protest_deadly_events_n_in_past_90_days',
                               'total_pop')])
bar_chart_3 <- bar_chart_3[rev(order(bar_chart_3$country_protest_deadly_events_n_in_past_90_days)), ]
bar_chart_3 <- bar_chart_3[!duplicated(paste0(bar_chart_3$country, '_', bar_chart_3$year)), ]

# Bar chart 3
bar_chart_4 <- na.omit(acled[acled$total_pop >= 10e6 & acled$days_of_deadly_protest_in_past_90_days > 10, 
                             c('country',
                               'year',
                               'country_protest_deaths_in_past_90_days',
                               'total_pop')])
bar_chart_4 <- bar_chart_4[rev(order(bar_chart_4$country_protest_deaths_in_past_90_days)), ]
bar_chart_4 <- bar_chart_4[!duplicated(paste0(bar_chart_4$country, '_', bar_chart_4$year)), ]


# Show data for these (and list 5+ in top ten of these which have ended in civil war, coup, new government or revolution - note that movements which span multiple years are only counted once):
bar_chart_1[1:20, ] # Myanmar, Burundi, Haiti, Iraq (new government), Kenya (new government)
bar_chart_2[1:20, ] # Egypt, Iraq (new gov), Myanmar, Egypt 2, Kenya (new gov), Haiti (coup), Burundi (coup attempt)
bar_chart_3[1:20, ] # Myanmar, Iraq, Egypt, Kenya, Ethiopia,     
bar_chart_4[1:20, ] # Egypt, Egypt, Myanmar, Iraq, Kenya, Ethiopia

# Text claims:
sum(iran_by_day$country_date_protest_deadly_events_n[iran_by_day$date %in% as.Date('2022-09-17'):as.Date('2022-09-23')]) # Deadly protests in first week
sum(iran_by_day$country_date_protests_n[iran_by_day$date %in% as.Date('2022-09-17'):as.Date('2022-09-23')]) # Total protests in first week
iran_by_day$country_date_protest_deaths[iran_by_day$date == as.Date("2022-09-30") & !is.na(iran_by_day$date)] # Number killed in Baluchistan protest, see also iran[iran$date == as.Date('2022-09-30'), ]:
iran_by_day$country_date_protest_deaths[iran_by_day$date == as.Date("2022-09-30") & !is.na(iran_by_day$date)] == max(iran_by_day$country_date_protest_deaths[iran_by_day$date >= as.Date("2022-09-16")], na.rm = T) # Check this is indeed peak

table(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-09-23') & iran_by_day$date <= as.Date('2022-12-01')]) # At least one protest per day after first week
mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-09-23') & iran_by_day$date <= as.Date('2022-12-01')], na.rm = T) # Average per day
mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-10-20') & iran_by_day$date < as.Date('2022-12-01')], na.rm = T) # Average per day in last 6 weeks

mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-10-20')-5*7 & iran_by_day$date < as.Date('2022-12-01') -5*7], na.rm = T) # Average per day in 6 weeks before the past 6 weeks

mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-11-16') & iran_by_day$date < as.Date('2022-12-02')], na.rm = T) # Average per day in 6 weeks before the past 6 weeks
mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-10-01') & iran_by_day$date < as.Date('2022-11-16')], na.rm = T) # Average per day in 6 weeks before the past 6 weeks

mean(iran_by_day$country_date_protest_deadly_events_n[iran_by_day$date >= as.Date('2022-09-23') & iran_by_day$date <= as.Date('2022-12-01')], na.rm = T)/mean(iran_by_day$country_date_protests_n[iran_by_day$date >= as.Date('2022-09-23') & iran_by_day$date <= as.Date('2022-12-01')], na.rm = T) # Average share of protests which are deadly

sum(iran_by_day$country_date_protest_deaths[iran_by_day$date %in% as.Date('2022-09-16'):as.Date('2022-12-01')]) # Total killed in ACLED protest events

