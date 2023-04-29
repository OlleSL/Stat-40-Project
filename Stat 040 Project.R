#Clear work space:
rm (list = ls())
#Load library:
library(tidyverse)

#Load data sets:
wine130 <- read.csv("project/winemag-data-130k-v2.csv")
wine <- read.csv("project/wine.csv")

#Make some changes to the data set. Remove column and change the way two variables
# are structured:
wine <- wine %>% subset(select = -c(review)) %>%
  mutate(price = as.numeric( gsub("\\$", "", price) ),
         alcohol = as.numeric( gsub("\\%", "", alcohol) ),
         alcohol = ceiling(alcohol) )

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

#There are multiple rows for many wine taster and wine pairings

#Checking if two logical statements are true to then display these rows where it equals TRUE.
#This is used to look into specific tasting occasions:
wine130[wine130$taster_name == "Michael Schachner" & 
          wine130$title == "Segura Viudas NV Extra Dry Sparkling (Cava)",]
wine[wine$reviewer == "Michael Schachner" & 
       wine$wine == "Segura Viudas NV Extra Dry Sparkling (Cava)",]

#This shows that the ratings for a specific wine will vary each instance the taster rates it
#For Michael Schachner the ratings of this wine range from 83 to 89

#new_wine = merge(wine130, wine, by.x = "title", by.y = "wine")

#add a new column showcasing level of wine ratings (low, moderate, high)
#new_wine = new_wine %>%
#  mutate(level_rating = case_when(rating >= 80 && rating <= 86 ~ "Low",
#                                  rating >= 87 && rating <= 93 ~ "Moderate",
#                                  rating >= 94 && rating <= 100 ~ "High",))

#There were 8996 observations out of the ~ 130k rows with a NA value in the column "price". To be able to do an analys of the data, we need to change that.
#   Here we change the NA values to be the average of the rest of the values in the column:

#We should group by the type of wine before we do this
#mean_price = mean(wine130$price, na.rm = TRUE)
#wine130$price[is.na(wine130$price)] = mean_price


###### Which country produces the most wine? #####
wine130 %>%
  ggplot()+
  geom_bar(aes(x = fct_infreq(country)), fill = "#722F37") +
  coord_flip()+
  labs( x = "Country (Ordered from least to most common)", y = "Count")


##### Which wine taster has tasted the most wines? #####
taster_name_count = wine130%>%
  group_by(taster_name)%>%
  summarise(count = n())%>%
  arrange(desc(count))

ggplot(data = taster_name_count)+
  geom_col(aes(x = fct_reorder(taster_name, -count), y = count, fill = count))+
  coord_flip()+
  scale_fill_gradient(low = "grey", high = "blue")+
  labs(x = "Taster Name",
       y = "Count",
       title = "Count of Wine Reviews by Taster Name")+
  theme(legend.position = "none")


###### How does alcohol percentage affect the price of wine? #####
#Replacing the NA with the mean:
mean_alcohol = mean(wine$alcohol, na.rm = TRUE)
wine$alcohol[is.na(wine$alcohol)] = mean_alcohol
mean_wine_price = mean(wine$price, na.rm = TRUE)
wine$price[is.na(wine$price)] = mean_wine_price

#Calculating the standard deviation for the alcohol and price in order to create a scatterplot:
sd_alcohol <- sd(wine$alcohol, na.rm = TRUE)
sd_price <- sd(wine$price, na.rm = TRUE)

#Plotting the relationship between the alcohol and price variable within 3 standard deviations from the mean:
ggplot(data = head(wine, 50000))+
  geom_point(aes(x = price, y = alcohol))+
  scale_x_continuous(limits = c(0, mean_wine_price + 3 * sd_price))+
  scale_y_continuous(limits = c(0, mean_alcohol + 3 * sd_alcohol))+
  labs(x = "Price of Wine (USD $)",
       y = "Alcohol Percentage of Wine",
       title = "Relationship Between Price and Alcohol of Wines")


##### Which wines have the highest ratings? #####

