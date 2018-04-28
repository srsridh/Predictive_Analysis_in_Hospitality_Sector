old.dir <- getwd()
setwd("C:/Users/Neha Rawat/Desktop/IU-Data Science/Data Mining/Project/Visitor Forecasting")

#Contributions from: Be My Guest Kaggle
#EDA for the Customer Forecasting analysis
#Loading essential libraries
library('ggplot2') # visualisation
library('scales') # visualisation
library('grid') # visualisation
library('gridExtra') # visualisation
library('RColorBrewer') # visualisation
library('corrplot') # visualisation
library('dplyr') # data manipulation
library('readr') # input/output
library('data.table') # data manipulation
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('ggrepel') # visualisation
library('ggridges') # visualisation
library('ggExtra') # visualisation
library('ggforce') # visualisation
library('viridis') # visualisation
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('purrr') # string manipulation
library('lubridate') # date and time
library('timeDate') # date and time
library('tseries') # time series analysis
library('forecast') # time series analysis
library('prophet') # time series analysis
library('timetk') # time series analysis
library('geosphere') # geospatial locations
library('leaflet') # maps
library('leaflet.extras') # maps
library('maps') # maps

#Helper function for multiplots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#Loading datasets
air_visits <- as.tibble(fread('air_visit_data.csv'))
air_reserve <- as.tibble(fread('air_reserve.csv'))
hpg_reserve <- as.tibble(fread('hpg_reserve.csv'))
air_store <- as.tibble(fread('air_store_info.csv'))
hpg_store <- as.tibble(fread('hpg_store_info.csv'))
holidays <- as.tibble(fread('date_info.csv'))
store_ids <- as.tibble(fread('store_id_relation.csv'))
submission <- as.tibble(fread('sample_submission.csv'))

#Summary and overview of datasets
summary(air_visits)
glimpse(air_visits)
summary(air_reserve)
glimpse(air_reserve)
summary(hpg_reserve)
glimpse(hpg_reserve)
summary(air_store)
glimpse(air_store)
summary(hpg_store)
glimpse(hpg_store)
summary(holidays)
glimpse(holidays)
summary(store_ids)
glimpse(store_ids)
summary(submission)

#Check for nulls and na values
sum(is.na(air_visits))
sum(is.na(air_reserve))
sum(is.na(hpg_reserve))
sum(is.na(air_store))
sum(is.na(hpg_store))
sum(is.na(holidays))
sum(is.na(store_ids))
sum(is.na(submission))
#0

#Reformatting features for visualization
air_visits <- air_visits %>%
  mutate(visit_date = ymd(visit_date))

air_reserve <- air_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

hpg_reserve <- hpg_reserve %>%
  mutate(visit_datetime = ymd_hms(visit_datetime),
         reserve_datetime = ymd_hms(reserve_datetime))

air_store <- air_store %>%
  mutate(air_genre_name = as.factor(air_genre_name),
         air_area_name = as.factor(air_area_name))

hpg_store <- hpg_store %>%
  mutate(hpg_genre_name = as.factor(hpg_genre_name),
         hpg_area_name = as.factor(hpg_area_name))

holidays <- holidays %>%
  mutate(holiday_flg = as.logical(holiday_flg),
         date = ymd(calendar_date))

#Air Visits visualizations
p1 <- air_visits %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line(col = "black") +
  labs(y = "All visitors", x = "Date")

p2 <- air_visits %>%
  ggplot(aes(visitors)) +
  geom_vline(xintercept = 20, color = "red") +
  geom_histogram(color="darkblue", fill="lightblue", bins = 40) +
  scale_x_log10()

p3 <- air_visits %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(wday, visits, fill = wday)) +
  geom_col() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  labs(x = "Day of the week", y = "Average visitors")

p4 <- air_visits %>%
  mutate(month = month(visit_date, label = TRUE)) %>%
  group_by(month) %>%
  summarise(visits = median(visitors)) %>%
  ggplot(aes(month, visits, fill = month)) +
  geom_col() +
  theme(legend.position = "none") +
  labs(x = "Month", y = "Average visitors")

layout <- matrix(c(1,1,1,1,2,3,4,4),2,4,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

air_visits %>%
  filter(visit_date > ymd("2016-04-25") & visit_date < ymd("2016-06-25")) %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(visitors)) %>%
  ggplot(aes(visit_date,all_visitors)) +
  geom_line() +
  geom_smooth(method = "loess", color = "green", size=1.5, span = 1/7) +
  labs(y = "All visitors", x = "Date")

#Air Reservations visualizations
air_break <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p1 <- air_break %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "air visit date")

p2 <- air_break %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(color="darkblue", fill="lightblue")

p3 <- air_break %>%
  filter(diff_hour < 24*7) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(color="darkblue", fill="lightblue") +
  labs(x = "Time from reservation to visit in hours")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)


air_break %>%
  arrange(desc(diff_day)) %>%
  select(reserve_datetime, visit_datetime, diff_day, air_store_id) %>%
  head(10)

#HPG Reservations visualizations
hpg_break <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day")
  )

p1 <- hpg_break %>%
  group_by(visit_date) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_date, all_visitors)) +
  geom_line() +
  labs(x = "'hpg' visit date")

p2 <- hpg_break %>%
  group_by(visit_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(visit_hour, all_visitors)) +
  geom_col(color="maroon", fill="lightpink")

p3 <- hpg_break %>%
  filter(diff_hour < 24*7) %>%
  group_by(diff_hour) %>%
  summarise(all_visitors = sum(reserve_visitors)) %>%
  ggplot(aes(diff_hour, all_visitors)) +
  geom_col(color="maroon", fill="lightpink") +
  labs(x = "Time from reservation to visit in hours")

layout <- matrix(c(1,1,2,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)

#Air and Hpg Stores - genre and spatial visualizations
leaflet(air_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~air_store_id, label = ~air_genre_name,
             clusterOptions = markerClusterOptions())

p1 <- air_store %>%
  group_by(air_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(air_genre_name, n, FUN = min), n, fill = air_genre_name, alpha = 0.1)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine", y = "Number of air restaurants")

p2 <- air_store %>%
  group_by(air_area_name) %>%
  count() %>%
  ungroup() %>%
  top_n(10,n) %>%
  ggplot(aes(reorder(air_area_name, n, FUN = min) ,n, fill = air_area_name, alpha = 0.1)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 10 areas", y = "Number of air restaurants")

layout <- matrix(c(1,2),2,1,byrow=TRUE)
multiplot(p1, p2, layout=layout)

leaflet(hpg_store) %>%
  addTiles() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addMarkers(~longitude, ~latitude,
             popup = ~hpg_store_id, label = ~hpg_genre_name,
             clusterOptions = markerClusterOptions())

p1 <- hpg_store %>%
  group_by(hpg_genre_name) %>%
  count() %>%
  ggplot(aes(reorder(hpg_genre_name, n, FUN = min), n, fill = hpg_genre_name, alpha=0.1)) +
  geom_col() +
  coord_flip() +
  theme(legend.position = "none") +
  labs(x = "Type of cuisine", y = "Number of hpg restaurants")

p2 <- hpg_store %>%
  mutate(area = hpg_area_name) %>%
  group_by(area) %>%
  count() %>%
  ungroup() %>%
  top_n(10,n) %>%
  ggplot(aes(reorder(area, n, FUN = min) ,n, fill = area, alpha=0.1)) +
  geom_col() +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "Top 10 areas", y = "Number of hpg restaurants")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)

#Holidays visualization
holiday_break <- holidays %>%
  mutate(wday = wday(date))

p1 <- holiday_break %>%
  ggplot(aes(holiday_flg, fill = holiday_flg, alpha=0.1)) +
  scale_fill_brewer(palette="Set1")+
  geom_bar() +
  theme(legend.position = "none")

p2 <- holiday_break %>%
  filter(date > ymd("2016-04-25") & date < ymd("2016-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg, alpha=0.1)) +
  scale_color_brewer(palette="Set1")+
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2016 date")

p3 <- holiday_break %>%
  filter(date > ymd("2017-04-25") & date < ymd("2017-06-01")) %>%
  ggplot(aes(date, holiday_flg, color = holiday_flg, alpha=0.1)) +
  scale_color_brewer(palette="Set1")+
  geom_point(size = 2) +
  theme(legend.position = "none") +
  labs(x = "2017 date")

layout <- matrix(c(1,1,2,3),2,2,byrow=FALSE)
multiplot(p1, p2, p3, layout=layout)

#Visitors impact by genre
air_vis_store <- air_visits %>%
  left_join(air_store, by = "air_store_id")

air_vis_store %>%
  group_by(visit_date, air_genre_name) %>%
  summarise(mean_visitors = mean(visitors)) %>%
  ungroup() %>%
  ggplot(aes(visit_date, mean_visitors, color = air_genre_name, alpha=0.1)) +
  geom_line() +
  labs(y = "Average number of visitors to air restaurants", x = "Date") +
  theme(legend.position = "none") +
  scale_y_log10() +
  facet_wrap(~ air_genre_name)

p1 <- air_vis_store %>%
  mutate(wday = wday(visit_date, label = TRUE)) %>%
  group_by(wday, air_genre_name) %>%
  summarise(avg_visitors = mean(visitors)) %>%
  ggplot(aes(air_genre_name, avg_visitors, color = wday)) +
  geom_point(size = 5, alpha=0.6) +
  theme(legend.position = "left", axis.text.y = element_blank(),
        plot.title = element_text(size = 12)) +
  coord_flip() +
  labs(x = "") +
  scale_x_discrete(position = "top") +
  ggtitle("Air Restaurant Genres") +
  scale_color_hue()

p2 <- air_vis_store %>%
  ggplot(aes(visitors, air_genre_name, fill = air_genre_name, alpha=0.1)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(y = "") +
  scale_fill_cyclical(values = c("darkblue", "lightpink"))

layout <- matrix(c(1,1,2,2,2),1,5,byrow=TRUE)
multiplot(p1, p2, layout=layout)

#Visitors impact by holidays
air_vis_hol <- air_visits %>%
  mutate(calendar_date = as.character(visit_date)) %>%
  left_join(holidays, by = "calendar_date")

p1 <- air_vis_hol %>%
  ggplot(aes(holiday_flg, visitors, color = holiday_flg)) +
  geom_boxplot() +
  scale_y_log10() +
  theme(legend.position = "none")

p2 <- air_vis_hol %>%
  mutate(wday = wday(date, label = TRUE)) %>%
  group_by(wday, holiday_flg) %>%
  summarise(avg_visitors = mean(visitors)) %>%
  ggplot(aes(wday, avg_visitors, color = holiday_flg)) +
  geom_point(size = 4, alpha=0.6) +
  theme(legend.position = "none") +
  labs(y = "Average number of visitors")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)

#Impact on visitors due to genres per area
#See how many genres per area

air_store %>%
  mutate(area = str_sub(air_area_name, 1,12)) %>%
  ggplot(aes(area, air_genre_name)) +
  geom_count(colour = "purple", alpha=0.6) +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

hpg_store %>%
  mutate(area = str_sub(hpg_area_name, 1, 12)) %>%
  ggplot(aes(area, hpg_genre_name)) +
  geom_count(colour = "lightpink") +
  theme(legend.position = "bottom", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))


air_genre_counts <- air_store %>%
  group_by(air_genre_name, air_area_name) %>%
  count()

hpg_genre_counts <- hpg_store %>%
  group_by(hpg_genre_name, hpg_area_name) %>%
  count()

p1 <- air_genre_counts %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "blue", binwidth = 1, alpha=0.6) +
  labs(x = "Air genres per area")

p2 <- hpg_genre_counts %>%
  ggplot(aes(n)) +
  geom_histogram(fill = "red", binwidth = 1, alpha=0.6) +
  labs(x = "HPG genres per area")

p3 <- air_vis_store %>%
  group_by(air_genre_name, air_area_name) %>%
  summarise(avg_log_visit = mean(log1p(visitors))) %>%
  left_join(air_genre_counts, by = c("air_genre_name","air_area_name")) %>%
  group_by(n) %>%
  summarise(avg_mlv = mean(avg_log_visit),
            sd_mlv = sd(avg_log_visit)) %>%
  replace_na(list(sd_mlv = 0)) %>%
  ggplot(aes(n, avg_mlv)) +
  geom_point(color = "green", size = 4, alpha=0.6) +
  geom_errorbar(aes(ymin = avg_mlv - sd_mlv, ymax = avg_mlv + sd_mlv), width = 0.5, size = 0.7, color = "green") +
  labs(x = "Cases of identical Air genres per area", y = "Mean +/- SD of\n mean log visitors")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)

#Reservations vs Visitors
air_reserves <- air_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(air_store_id,visit_date) %>%
  summarise(reserve_air_visitors = sum(reserve_visitors))

hpg_reserves <- hpg_reserve %>%
  mutate(visit_date = date(visit_datetime)) %>%
  group_by(hpg_store_id,visit_date) %>%
  summarise(reserve_hpg_visitors= sum(reserve_visitors)) %>%
  inner_join(store_ids, by = "hpg_store_id")

all_reserves <- air_visits %>%
  inner_join(air_reserves, by = c("air_store_id", "visit_date")) %>%
  inner_join(hpg_reserves, by = c("air_store_id", "visit_date")) %>%
  mutate(reserve_visitors = reserve_air_visitors + reserve_hpg_visitors)

p <- all_reserves %>%
  filter(reserve_visitors < 200) %>%
  ggplot(aes(reserve_visitors, visitors)) +
  geom_point(color = "black", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "grey60") +
  geom_smooth(method = "lm", color = "purple", alpha=0.6)

ggMarginal(p, type="histogram", fill = "purple", bins=50, alpha=0.6)

#Feature engineering and related visualizations
air_visits <- air_visits %>%
  mutate(wday = wday(visit_date, label=TRUE),
         wday = fct_relevel(wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         month = month(visit_date, label=TRUE))

air_reserve <- air_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

hpg_reserve <- hpg_reserve %>%
  mutate(reserve_date = date(reserve_datetime),
         reserve_hour = hour(reserve_datetime),
         reserve_wday = wday(reserve_datetime, label = TRUE),
         reserve_wday = fct_relevel(reserve_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         visit_date = date(visit_datetime),
         visit_hour = hour(visit_datetime),
         visit_wday = wday(visit_datetime, label = TRUE),
         visit_wday = fct_relevel(visit_wday, c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")),
         diff_hour = time_length(visit_datetime - reserve_datetime, unit = "hour"),
         diff_day = time_length(visit_datetime - reserve_datetime, unit = "day"))

# count stores in area
air_count <- air_store %>%
  group_by(air_area_name) %>%
  summarise(air_count = n())

hpg_count <- hpg_store %>%
  group_by(hpg_area_name) %>%
  summarise(hpg_count = n())

# distances
med_coord_air <- air_store %>%
  summarise_at(vars(longitude:latitude), median)
med_coord_hpg <- hpg_store %>%
  summarise_at(vars(longitude:latitude), median)

air_coords <- air_store %>%
  select(longitude, latitude)
hpg_coords <- hpg_store %>%
  select(longitude, latitude)

air_store$dist <- distCosine(air_coords, med_coord_air)/1e3
hpg_store$dist <- distCosine(hpg_coords, med_coord_hpg)/1e3

#counts, dist; add prefecture
air_store <- air_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(air_count, by = "air_area_name") %>%
  separate(air_area_name, c("prefecture"), sep = " ", remove = FALSE)

hpg_store <- hpg_store %>%
  mutate(dist_group = as.integer(case_when(
    dist < 80 ~ 1,
    dist < 300 ~ 2,
    dist < 500 ~ 3,
    dist < 750 ~ 4,
    TRUE ~ 5))) %>%
  left_join(hpg_count, by = "hpg_area_name") %>%
  separate(hpg_area_name, c("prefecture"), sep = " ", remove = FALSE)

#Days and Months
p1 <- air_visits %>%
  group_by(wday) %>%
  summarise(avg_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(wday, avg_log_visitors, color = wday)) +
  geom_point(size = 4, alpha=0.6) +
  geom_errorbar(aes(ymin = avg_log_visitors - sd_log_visitors,
                    ymax = avg_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = "none")

p2 <- air_visits %>%
  mutate(visitors = log1p(visitors)) %>%
  ggplot(aes(visitors, wday, fill = wday, alpha=0.6)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(x = "log_visitors", y = "")

p3 <- air_visits %>%
  group_by(month) %>%
  summarise(avg_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(month, avg_log_visitors, color = month)) +
  geom_point(size = 4, alpha=0.6) +
  geom_errorbar(aes(ymin = avg_log_visitors - sd_log_visitors,
                    ymax = avg_log_visitors + sd_log_visitors,
                    color = month), width = 0.5, size = 0.7) +
  theme(legend.position = "none")

p4 <- air_visits %>%
  mutate(visitors = log1p(visitors)) %>%
  ggplot(aes(visitors, month, fill = month, alpha=0.6)) +
  geom_density_ridges(bandwidth = 0.1) +
  scale_x_log10() +
  theme(legend.position = "none") +
  labs(x = "log_visitors", y = "")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(wday, air_genre_name) %>%
  summarise(avg_log_visitors = mean(log1p(visitors)),
            sd_log_visitors = sd(log1p(visitors))) %>%
  ggplot(aes(wday, avg_log_visitors, color = wday)) +
  geom_point(size = 3, alpha=0.6) +
  geom_errorbar(aes(ymin = avg_log_visitors - sd_log_visitors,
                    ymax = avg_log_visitors + sd_log_visitors,
                    color = wday), width = 0.5, size = 0.7) +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9)) +
  facet_wrap(~ air_genre_name)

#Restaurants per area
p1 <- air_count %>%
  ggplot(aes(air_count)) +
  geom_histogram(binwidth = 2, fill = "purple", alpha=0.6)

p2 <- hpg_count %>%
  ggplot(aes(hpg_count)) +
  geom_histogram(binwidth = 5, fill = "lightpink", alpha=0.7)

p3 <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, air_count) %>%
  summarise(avg_store_visit = mean(log1p(visitors))) %>%
  group_by(air_count) %>%
  summarise(avg_log_visitors = mean(avg_store_visit),
            sd_log_visitors = sd(avg_store_visit)) %>%
  ggplot(aes(air_count, avg_log_visitors)) +
  geom_point(size = 4, color = "purple") +
  geom_errorbar(aes(ymin = avg_log_visitors - sd_log_visitors,
                    ymax = avg_log_visitors + sd_log_visitors),
                color = "purple", width = 0.5, size = 0.7) +
  geom_smooth(method = "lm", color = "black") +
  labs(x = "Air restaurants per area")

layout <- matrix(c(1,2,3,3),2,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)

#Distance from busiest area and prefecture plots
p1 <- air_store %>%
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = "purple", alpha=0.6) +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = "Linear distance in km from center")+
  ggtitle("Air Restaurants")

p2 <- hpg_store %>%
  ggplot(aes(dist)) +
  geom_histogram(bins = 30, fill = "maroon", alpha=0.5) +
  geom_vline(xintercept = c(80, 300, 500, 750)) +
  labs(x = "Linear distance in km from center")+
  ggtitle("Hpg Restaurants")

layout <- matrix(c(1,2),1,2,byrow=TRUE)
multiplot(p1, p2, layout=layout)

p1 <- air_store %>%
  ggplot(aes(prefecture)) +
  geom_bar(fill = "purple", alpha=0.6) +
  coord_flip() +
  ggtitle("air prefectures - #restaurants") +
  labs(x = "")

p2 <- hpg_store %>%
  ggplot(aes(prefecture)) +
  geom_bar(fill = "maroon", alpha=0.6) +
  coord_flip() +
  ggtitle("hpg prefectures- #restaurants") +
  labs(x = "")

p3 <- air_visits %>%
  left_join(air_store, by = "air_store_id") %>%
  group_by(air_store_id, prefecture) %>%
  summarise(mean_store_visit = mean(log1p(visitors))) %>%
  group_by(prefecture) %>%
  summarise(avg_log_visitors = mean(mean_store_visit),
            sd_log_visitors = sd(mean_store_visit)) %>%
  ggplot(aes(prefecture, avg_log_visitors)) +
  geom_point(size = 4, color = "black") +
  geom_errorbar(aes(ymin = avg_log_visitors - sd_log_visitors,
                    ymax = avg_log_visitors + sd_log_visitors),
                color = "black", width = 0.5, size = 0.7) +
  labs(x = "prefecture") +
  theme(axis.text.x  = element_text(angle=15, hjust=1, vjust=0.9))

layout <- matrix(c(1,2,1,2,1,2,3,3,3,3),5,2,byrow=TRUE)
multiplot(p1, p2, p3, layout=layout)