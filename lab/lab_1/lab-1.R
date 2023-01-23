library(dplyr)
library(seperate)
library(ggplot2)

library(stringr)

testing <- airbnb
testing <- testing %>%
  select(host_neighbourhood, neighbourhood_cleansed)

testing <- str_replace(place_host$neighbourhood, ", Washington, United States", "")
# You have to use the $ instead of indexing because indexing creates an unworkable
# "vector" or something idk

place_host$neighbourhood <- testing # replaces the string with just the city

View(testing)

###########################
### making empty cells = NA
###########################

testing <- replace(testing, testing == '', NA) # removed a couple cases
place_host <- replace(place_host, place_host == '', NA)

testing <- testing[complete.cases(testing), ]
full_host <- place_host[complete.cases(place_host), ]

###############################
### Host Neighborhood = Seattle
###############################

# the problem is that it doesn't just say "Seattle", it also says "downtown"
# I will filter down the data until it just has the seattle problem

testing2 <- full_host %>%
  select(host_neighbourhood) # only one column so I can focus better 

full_host <- full_host %>%
  mutate(host_neighbourhood = str_replace(host_neighbourhood, ".*Seattle", "Seattle"))
# basically transformed the entire column so that the strings with the word "Seattle" 
# behind it would just have the city

testing <- testing %>%
  mutate(same = (host_neighbourhood == neighbourhood_cleansed))

################################
### Making Dataset for Summaries
################################

full_host <- full_host %>%
  mutate(same_city = (neighbourhood == host_neighbourhood))
# makes another column that evaluates weather the host lives in the same city

summary <- full_host %>%
  group_by(host_id, same_city) %>% # by grouping, it allows us to count the 
  # occurences of unique host ids
  summarize(number_listings = length(host_id))

##########################
### Making Scatter Plots!!
##########################

nyc2<- nyc

nyc2$price <- as.factor(nyc$price)
nyc2$prop_white <- as.factor(nyc$prop_white)

scatter <- ggplot(
  data = nyc2,
  aes()) + geom_point()

# Adding labels to the axis + title
scatter <- scatter + labs(
  title = "Price of AirBNB vs. Prop. of Race (Black and White)",
  y = "Price of AirBNB",
  x = "Race Proportions"
)

######################
### Making Box Plots!!
######################

nyc_boxplot <- data_boxplot %>%
  ggplot(aes(modal_race, # x value
             price, # y value
  )) + geom_boxplot() +
  stat_summary(fun = "mean")

testing <- boxplot(
  price~modal_race,
  data = data_boxplot,
  main = "Testing Boxplot",
  xlab = "Modal Race",
  ylab = "Price of Airbnb"
)

# these two do the exact same thing...

#########################
### Making New Data Sets!
#########################

library(dplyr)

testing <- nyc_full %>%
  select(total_pop, city)

testing <- testing[complete.cases(testing), ]

summary <- testing %>%
  group_by(city) %>%
  summarize(airbnb = n(),
            total_pop = max(total_pop))