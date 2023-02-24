library(dplyr)
library(ggplot2)
library(stringr)

# wd() is /SOC/project

# Questions that need to be answered are:
#    What time are most bikes in use
#    Casual users weekdays vs. weekends/pop. day of the week
#    Does weather have an effect on users? (popular weather)
day <- read.csv("day.csv")
     # weathersit = "weather situation"; 1 is Clear, Few clouds, Partly cloudy, Partly cloudy
     #                                   2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
     #                                   3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
     #                                   4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

hour <- read.csv("hour.csv")
#-------------------------------------------------------------------------------
# What times are bikes being used the most?
popular_time <- hour %>%
  select(hr, cnt) %>%
  group_by(hr) %>%
  summarize(round(mean(cnt), 2)) %>%
  rename(average = "round(mean(cnt), 2)")

# make a bar graph to show most pop. hour
column_time <- popular_time %>%
  ggplot(aes(x = hr,
             y = average)) + geom_col() + ggtitle("Average # of Users vs. Hour of Day") + ylab("Average # of Users") + xlab("Hour of Day")

column_time
#-------------------------------------------------------------------------------
# Casual users weekdays vs. weekends
# $weekday: 0 = Sunday, 6 = Saturday
cas_day <- day %>%
  select(dteday, holiday, weekday, casual) %>%
  mutate(weekday = str_replace_all(weekday, "[12345]", "weekday")) %>%
  mutate(weekday = str_replace_all(weekday, "[06]", "weekend")) %>%
  mutate(weekday = ifelse(holiday == 1, "holiday", weekday))

# make a pie chart
