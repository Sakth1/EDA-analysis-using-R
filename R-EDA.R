#importing data set

setwd("E:\\Users\\Desktop\\Professional\\EDA")
data1<-read.csv("DATASETC.csv")

#activating libraries
library("dplyr")
library("tidyr")
library("corrplot")
library("ggplot2")

#data manipulation
mutate(data1)
summary(data1)
outliers<-ggplot(data1, aes(y = PRICE_IN_LAKHS))+ geom_boxplot()
outliers

#What is the distribution of property ages in the dataset?
h2<-ggplot(data1, aes(x = AGE_OF_BUILDING, color=factor(NO_OF_BEDROOMS))) + 
  geom_histogram(fill="grey", alpha=0.5, position="identity")  +
  labs(title = "Distribution of Property Ages", x = "Age of Building", y = "NO. of bedrooms")
h2

#What is the distribution of property prices in the dataset?
histo<-ggplot(data1, aes(x = PRICE_IN_LAKHS)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "black") +
  labs(x = "Price in Lakhs", y = "Frequency", title = "Distribution of Property Prices")
histo

#How does the distribution of property prices differ between newly constructed and old properties?
d1<-ggplot(data1, aes(x = PRICE_IN_LAKHS, fill = CONSTRUCTION_STATUS)) +
  geom_density(alpha = 0.5) +
  labs(x = "Price in Lakhs", fill = "Construction Status", title = "Distribution of Property Prices by Construction Status")
d1

#What is the construction status of properties based and their total floor? Chart type: Bar chart Variables: PRICE_IN_LAKHS, CONSTRUCTION_STATUS
barchart<-ggplot(data1, aes(x = CONSTRUCTION_STATUS, y = TOTAL_FLOOR)) +
  geom_bar(stat = "summary", fun = "mean", fill = "skyblue", color = "black") +
  labs(x = "Construction Status", y = "Average Floors", title = "Average Property Price by Construction Status")
barchart

#What is the distribution of property prices and NO. of bedrooms based on construction status?
v1<-ggplot(data1, aes(x = NO_OF_BEDROOMS, y = PRICE_IN_LAKHS, fill = CONSTRUCTION_STATUS)) +
  geom_violin() + labs(title = "Distribution of Property Prices and NO. of bedrooms by Construction Status", x = "NO. of bedrooms", y = "Price in Lakhs")
v1

#is there a relationship between the age of the building and its price, and does this relationship differ for different property types?
lin<-ggplot(data1, aes(x = AGE_OF_BUILDING, y = PRICE_IN_LAKHS, group = PROPERTY_TYPE, color = PROPERTY_TYPE)) +
  geom_line() +
  labs(x = "Age of Building", y = "Price in Lakhs", color = "Property Type", title = "Property Price vs Age of Building")
lin

#How does property type influence valuation?
b1<-ggplot(data1, aes(x = PROPERTY_TYPE, y = PRICE_IN_LAKHS)) + geom_boxplot() +
  labs(title = "Distribution of Property Prices by Type", x = "Property Type", y = "Price (Lakhs)") +
  theme(axis.text.x = element_text(angle =45, hjust = 1))
b1

#Is there a correlation between Total floors in the property age and price?
p1<-ggplot(data1, aes(x = TOTAL_FLOOR, y = PRICE_IN_LAKHS)) +
  geom_point() +
  labs(x = "Age of Building", y = "Price in Lakhs", title = "Property Price vs Age of Building Across Localities")
p1

#How do property prices vary across different numbers of bedrooms and total floors?
b2<-ggplot(data1, aes(x = factor(NO_OF_BEDROOMS), y = PRICE_IN_LAKHS)) +
  geom_boxplot() +
  labs(x = "Number of Bedrooms", y = "Price in Lakhs", fill = "Total Floors", title = "Property Price Variation Across Bedrooms and Total Floors")
b2

#Is there a correlation between property age and price?
bar2<-ggplot(data1, aes(x = AGE_OF_BUILDING, y = PRICE_IN_LAKHS)) +
  geom_point(alpha = 0.5, color = "purple") +
  labs(title = "Correlation Between Property Age and Price", x = "Age (Years)", y = "Price (Lakhs)") +
  geom_smooth(method = lm)
bar2

