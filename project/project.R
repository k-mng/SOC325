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
  group_by(weekday) %>%
  summarize(casual = sum(casual))

# make a pie chart
ggplot(cas_day,
       aes(x = "",
           y = casual,
           fill = weekday)) + geom_bar(stat = "identity",
                                       width = 1) + coord_polar("y", 
                                                                start = 0) + theme_void() + geom_label(aes(label = casual),
                                                                                                       position = position_stack(vjust = 0.5)) +
  scale_fill_brewer() + ggtitle("# Casual Bikers Weekdays vs. Weekends")

###
# first one is too inconclusive so lets break it down by day
cas2_day <- day %>%
  select(dteday, holiday, weekday, casual) %>%
  mutate(weekday = str_replace_all(weekday, c("0" = "Sunday",
                                              "1" = "Monday",
                                              "2" = "Tuesday",
                                              "3" = "Wednesday",
                                              "4" = "Thursday",
                                              "5" = "Friday",
                                              "6" = "Saturday"))) %>%
  group_by(weekday) %>%
  summarize(casual = sum(casual))

# make the pie chart
ggplot(cas2_day,
       aes(x = "",
           y = casual,
           fill = weekday)) + geom_bar(stat = "identity",
                                       width = 1) + coord_polar("y", 
                                                                start = 0) + theme_void() + geom_label(aes(label = casual),
                                                                                                       position = position_stack(vjust = 0.5)) +
  scale_fill_manual(values = c("#b8d8ba", "#d9dbbc",
                               "#fcddbc", "#ef959d",
                               "#d1aca5", "#69585f",
                               "#82ac85")) + ggtitle("# Casual Bikers on Each Day of the Week")

#-------------------------------------------------------------------------------
# In what weather do people tend to bike in?
weather <- day %>%
  select(weathersit, cnt) 

weather$weathersit <- as.factor(weather$weathersit)

# make a boxplot
ggplot(weather,
       aes(x = weathersit,
           y = cnt)) + geom_boxplot() +
  ggtitle("Frequency of Bikers During Various Weather Situations") +
  xlab("Weather Situation") +
  ylab("# of Bikers") +
  scale_x_discrete(breaks = c("1", "2", "3"),
                   labels = c("Clear/Few Clouds", 
                              "Misty/Cloudy",
                              "Light Snow/Rain, 
                              Scattered Clouds, 
                              Thunderstorm"))
