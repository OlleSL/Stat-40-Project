#Clear work space:
rm (list = ls())
#Load library:
library(tidyverse)

#Load data sets:
wine130 <- read.csv("Data/winemag-data-130k-v2.csv")
wine <- read.csv("Data/wine.csv")

#Make some changes to the data set. Remove column and change the way two variables
# are structured:
wine <- wine %>% subset(select = -c(review)) %>%
  mutate(price = as.numeric( gsub("\\$", "", price)),
         alcohol = as.numeric(gsub("\\%", "", alcohol)))

#Remove two columns from the wine130 data set:
wine130 <- wine130 %>% 
  subset(select = -c(X, description, taster_twitter_handle))

#create a new data frame from the wine data set that only consists of:
# "reviewer", "wine" and "count" and arrange it descending by the count:
wn <- wine %>%
  group_by(reviewer, wine) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#create a new data frame from the wine130 data set that only consists of:
# "taster_name", "title" and a new variable "count" and arrange it descending by the count:
wn130 <- wine130 %>%
  group_by(taster_name, title) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

#Checking if two logical statements are true to then display these rows where it equals TRUE.
#This is used to look into specific tasting occasions:
wine130[wine130$taster_name == "Michael Schachner" & 
          wine130$title == "Segura Viudas NV Extra Dry Sparkling (Cava)",]
wine[wine$reviewer == "Michael Schachner" & 
       wine$wine == "Segura Viudas NV Extra Dry Sparkling (Cava)",]


#new_wine = merge(wine130, wine, by.x = "title", by.y = "wine")

#add a new column showcasing level of wine ratings (low, moderate, high)
#new_wine = new_wine %>%
#  mutate(level_rating = case_when(rating >= 80 && rating <= 86 ~ "Low",
#                                  rating >= 87 && rating <= 93 ~ "Moderate",
#                                  rating >= 94 && rating <= 100 ~ "High",))

#There were 8996 observations out of the ~ 130k rows with a NA value in the column "price". To be able to do an analys of the data, we need to change that.
#   Here we change the NA values to be the average of the rest of the values in the column:
mean_price = mean(wine130$price, na.rm = TRUE)
wine130$price[is.na(wine130$price)] = mean_price
