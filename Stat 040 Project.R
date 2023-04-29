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

#Replacing the NA with the mean:
mean_alcohol = mean(wine$alcohol, na.rm = TRUE)
wine$alcohol[is.na(wine$alcohol)] = mean_alcohol
mean_price = mean(wine$price, na.rm = TRUE)
wine$price[is.na(wine$price)] = mean_price

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
  ggplot()+
  geom_bar(aes(x = fct_infreq(country)), fill = "#722F37") +
  coord_flip()+
  labs( x = "Country (Ordered from least to most common)", y = "Count")


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
#Calculating the standard deviation for the alcohol and price in order to appropriately filter out outliers from scatterplot:
sd_alcohol <- sd(wine$alcohol, na.rm = TRUE)
sd_price <- sd(wine$price, na.rm = TRUE)

#Plotting the relationship between the alcohol and price variable within 3 standard deviations from the mean:
ggplot(data = wine)+
  geom_point(aes(x = price, y = alcohol))+
  scale_x_continuous(limits = c(0, mean_price + 3 * sd_price))+
  scale_y_continuous(limits = c(0, mean_alcohol + 3 * sd_alcohol))+
  labs(x = "Price of Wine (USD $)",
       y = "Alcohol Percentage of Wine",
       title = "Relationship Between Price and Alcohol of Wines")

#add a new column showcasing level of wine ratings (low, moderate, high)
wine <- wine %>% 
  mutate(level_rating = case_when(rating >= 80 & rating <= 86 ~ "Low",
                                  rating >= 87 & rating <= 93 ~ "Moderate",
                                  rating >= 94 & rating <= 100 ~ "High",))

wine <- wine%>% mutate(new_wine = factor(new_wine, levels = c("Low", "Moderate", "High")

#Calculating the average price of wine for each level of rating
price_by_rating_level <- wine%>%group_by(level_rating) %>%summarise(count = n(),mean_price = mean(price, na.rm=TRUE))

##### Which wines have the highest ratings? #####
#Results for the wine data set
wine %>% arrange(desc(rating)) %>% head(5) 
#Results for the wine130 data set
wine130 %>% arrange(desc(points)) %>% head(5) 

#Graph showing level rating vs price                                        
ggplot(data = wine) +
geom_point(aes(x = level_rating, y = price),colour = "#722F37", alpha = I(.3)) +
facet_wrap(~level_rating) +
labs(x = "Rating", y = "Price"

     
# there is something here that is not working, whatever I change, the bars remains the same...
wine%>%
  mutate(price_range = cut(price, 
                            breaks = c(-Inf, 15, 30, 45, 200 ,Inf), 
                            labels = c("0-15$", "15-30$", "30-45$", "45-60", "200+"), 
                            include.lowest = TRUE)) %>%
  group_by(price_range, rating) %>%
  summarise(mean_rating = mean(rating, na.rm = TRUE)) %>%
  ggplot()+
  geom_col(aes(x = price_range, y = rating, fill = price_range))+
  scale_fill_manual(values = c("0-15$" = "#9400D3", "15-30$" = "#4B0082", 
                               "30-45$" = "#B22222", "45-60" = "#722F37",
                               "200+" = "violet"))+
  labs(x = "Price Range", 
       y = "Mean Rating",
       title = "Mean Rating per Price Range of Wine")+
  theme(legend.position = "none") 
