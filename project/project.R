library(dplyr)
library(ggplot2)

# wd() is /SOC/project
day <- read.csv("day.csv")
     # weathersit = "weather situation"; 1 is Clear, Few clouds, Partly cloudy, Partly cloudy
     #                                   2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
     #                                   3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
     #                                   4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog

hour <- read.csv("hour.csv")

# What times are bikes being used the most?
popular_time <- hour %>%
  select(hr, cnt) %>%
  group_by(hr) %>%
  summarize(round(mean(cnt), 2)) %>%
  rename(average = "round(mean(cnt), 2)")

# make a pie chart
pie_time <- popular_time %>%
  ggplot(aes(x = "",
         y = average,
         fill = hr)) + geom_bar(stat = "identity",
                                width = 1) +
  coord_polar("y", start = 0)

pie_time
