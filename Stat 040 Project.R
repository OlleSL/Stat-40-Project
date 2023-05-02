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

#Replacing the NA with the mean in the two data sets:
mean_alcohol = mean(wine$alcohol, na.rm = TRUE)
wine$alcohol[is.na(wine$alcohol)] = mean_alcohol
mean_price = mean(wine$price, na.rm = TRUE)
wine$price[is.na(wine$price)] = mean_price
mean_wine_price = mean(wine130$price, na.rm = TRUE)
wine130$price[is.na(wine130$price)] = mean_wine_price

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

###### Which country produces the most wine? #####
wine130 %>%
  group_by(country)%>%
  summarise(count = n())%>%
  ggplot()+
  geom_col(aes(x = fct_reorder(country, -count), y = count, fill = count)) +
  coord_flip()+
  labs( x = "Country (Ordered from least to most common)", 
        y = "Count",
        title = "Wine Produced per Country",
        fill = "Count") +
  scale_fill_gradient(low = "#808080", high = "#722F37")+
  geom_text(aes(x = fct_reorder(country, -count), y = count, label = count), size = 2.5, hjust = -0.05) +
  theme(legend.position = "none")

##### Which wine taster has tasted the most wines? #####
wine %>%
  group_by(reviewer) %>%
  summarise(count = n()) %>%
  mutate(reviewer = case_when(reviewer == "" ~ NA_character_,
                              TRUE ~ reviewer)) %>%
  drop_na() %>%
  arrange(desc(count)) %>%
  ggplot()+
  geom_col(aes(x = fct_reorder(reviewer, -count), y = count, fill = count))+
  coord_flip()+
  scale_fill_gradient(low = "#FFFFFF", high = "#722F37") +
  labs(x = "Taster Name",
       y = "Count",
       title = "Count of Wine Reviews by Taster Name",
       fill = "Count")


###### How does alcohol percentage affect the price of wine? #####
wine %>%
  mutate(sd_alcohol = sd(alcohol, na.rm = TRUE),
         sd_price = sd(price, na.rm = TRUE),
         mean_alcohol = mean(alcohol),
         mean_price = mean(price)) %>%
  ggplot() +
  geom_point(aes(x = price, y = alcohol), alpha = 0.1) +
  labs(x = "Price of Wine (USD $)",
       y = "Alcohol Percentage of Wine",
       title = "Relationship Between Price and Alcohol of Wines",
       subtitle = "Within Three Standard Deviations from Mean") +
  scale_x_continuous(limits = c(0, mean(wine$price, na.rm = TRUE) + 3 * sd(wine$price, na.rm = TRUE))) +
  scale_y_continuous(limits = c(0, mean(wine$alcohol, na.rm = TRUE) + 3 * sd(wine$alcohol, na.rm = TRUE)))

#add a new column showcasing level of wine ratings (low, moderate, high)
wine <- wine %>% 
  mutate(level_rating = case_when(rating <= 86 ~ "Low",
                                  rating <= 93 ~ "Moderate",
                                  TRUE ~ "High",))
#Need to work more on this, not sure if we even need this.
wine <- wine%>% 
 factor(level_rating, levels = c("Low", "Moderate", "High"))

#Calculating the average price of wine for each level of rating
price_by_rating_level <- wine%>%
      group_by(level_rating) %>%
      summarise(count = n(),
      mean_price = mean(price, na.rm=TRUE))

##### Which wines have the highest ratings? #####
#Results for the wine data set
#Top 5 Wine, Alcohol, Price, Rating, Reviewer
highest_wine_5 <- wine%>%
  arrange(desc(rating)) %>%
  subset(select = c(wine, alcohol, price, rating, reviewer)) %>%
  head(5) 

highest_wine_5
#Results for the wine130 data set
#Top 5 Title, Points, Price,Country
highest_wine130_5 <- wine130%>%
  arrange(desc(points)) %>% 
  subset(select = c(title, points, price, taster_name)) %>%
  head(5) 

highest_wine130_5


#Plotting the relationship between price and rating of wines:                                           
wine %>%
  mutate(price_range = case_when(
    price <= 15 ~ "0-15$",
    price <= 30 ~ "15-30$",
    price <= 45 ~ "30-45$",
    price <= 60 ~ "45-60$",
    price <= 100 ~ "60-100$",
    TRUE ~ "100$+")) %>%
  group_by(price_range) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = reorder(price_range, mean_rating), y = mean_rating, fill = price_range)) +
  scale_fill_manual(values = c("0-15$" = "#9400D3", "15-30$" = "#4B0082",
                               "30-45$" = "#B22222", "45-60$" = "#722F37",
                               "60-100$" = "violet", "100$+" = "#FF4433")) +
  labs(x = "Price Range", 
       y = "Mean Rating",
       title = "Mean Rating per Price Range of Wine") +
  theme(legend.position = "none")+
  geom_text(aes(x = reorder(price_range, mean_rating), y = mean_rating,
                label = round(mean_rating, 2)), vjust = -0.4)

#Plotting the relationship between price and points of wines similar to the geom_col above but with the second data set:                                           
wine130 %>%
  mutate(price_range = case_when(
    price <= 15 ~ "0-15$",
    price <= 30 ~ "15-30$",
    price <= 45 ~ "30-45$",
    price <= 60 ~ "45-60$",
    price <= 100 ~ "60-100$",
    TRUE ~ "100$+")) %>%
  group_by(price_range) %>%
  summarise(mean_points = mean(points, na.rm = TRUE)) %>%
  ggplot() +
  geom_col(aes(x = reorder(price_range, mean_points), y = mean_points, fill = price_range)) +
  scale_fill_manual(values = c("0-15$" = "#9400D3", "15-30$" = "#4B0082",
                               "30-45$" = "#B22222", "45-60$" = "#722F37",
                               "60-100$" = "violet", "100$+" = "#FF4433")) +
  labs(x = "Price Range", 
       y = "Mean Rating",
       title = "Mean Rating per Price Range of Wine") +
  theme(legend.position = "none")+
  geom_text(aes(x = reorder(price_range, mean_points), y = mean_points,
                label = round(mean_points, 2)), vjust = -0.4)


wine %>%
  ggplot()+
  geom_boxplot(aes(x = fct_reorder(category, -rating), y = rating, fill = category))+
  labs(x = "Category",
       y = "Rating",
       title = "Relationship Between Category and Rating")+
  theme(legend.position = "none") +
  scale_fill_manual(values = c("Red" = "#9400D3", "White" = "#4B0082",
                               "Sparkling" = "#B22222", "Dessert" = "#722F37",
                               "Port/Sherry" = "violet", "Rose" = "#FF4433",
                               "Fortified" = "skyblue")) 
