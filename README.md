# Stat-40-Project
R analysis project on a wine dataset consisting of data cleaning and a lot of visualizations.

#Summary of Datasets
For our project we will be utilizing two data sets on wine reviews. The data sets we will be using are from Kaggle and these are their respective links: Wine Reviews | Kaggle (winemag-data-130k-v2.csv) and Wine Reviews Data | Kaggle. Below is a summary of each column and what it contains.

#Wine Reviews Data, Columns & Descriptions:
Wine: Name of wine
Winery: Name of winery
Category: Type of wine
Designation: Designation of wine
Varietal: Type of grape
Appellation: Region of wine
Alcohol: Alcohol content
Price: Price in dollars $
Rating: Rating from reviews
Reviewer: Name of reviewer
Review: Description of review

#Wine Reviews, Columns & Descriptions:
Country: The country that the wine is from
Description: Description of the wine
Designation: The vineyard within the winery where the grapes that made the wine are from
Points: The number of points wine enthusiast rated the wine on a scale of 1-100
Price: The cost for a bottle of wine
Province: The province or state that the wine is from
Region_1: The wine growing area in a province or state (ie Napa)
Region_2: Sometimes there are more specific regions specified within a wine growing area (ie Rutherford inside the Napa Valley) this value is often blank
Taster_name: Name of the wine enthusiast tasting the wine
Taster_twitter_handle: Twitter related to the taster
Title: The title of the wine review, which often contains the vintage
Variety: The type of grapes used to make the wine 
Winery: The winery that made the wine

#Research Questions
Which country produces the most wine?
How does alcohol percentage affect the price of wine?
Which wine taster has tasted the most wines?
Which wines have the highest ratings?
 


#Data Cleaning/Manipulation 
So far, we removed unnecessary rows such as review and description, which both take up a lot of space. We removed the characters in price and alcohol percentage to convert them to numeric columns. Some of the price entries were not numbers (e.g. "DrizlyVivino") and were replaced as NA. Grouping the data by the taster/reviewer and the wine shows that the only thing stopping this pair of columns from being a primary key is that the alcohol percentage is only rounded sometimes and the tasters give a different review each time they taste a wine.  
