# Lim Zhi Sheng TP069606
# Khoo Jie Cheng TP065193
# Lee Jia Chyi TP068571 
# Tan Shu Jing TP069265

############################## Data Exploration Line 6 --> Line 174 
install.packages("ggplot2")
install.packages("dplyr")
install.packages("rpart.plot")
install.packages("maps")
install.packages("car")
install.packages("randomForest")
library(randomForest)
library(car)
library(rpart.plot)
library(ggplot2)
library(dplyr)
library(maps)

Default_Data <- read.csv("C:/Users/jckho/Downloads/House_Rent_Dataset.csv")

# Check the first few rows
head(Default_Data)

# See the data sets
View(Default_Data)

# Explore the data set
class(Default_Data)

# Explore the data types
for (col in names(Default_Data))
{
  print(class(Default_Data[[col]]))
}  

# Explore the basic characteristics of column name 
for (col in names(Default_Data))
{
  print(col)
}

# Explore Null Values 
colSums(is.na(Default_Data)) # colSums functions will find missing values

# Check for Duplicated Values
duplicated(Default_Data)

# Total up the Duplicated Values for all columns 
sum(duplicated(Default_Data))

# See column Names
colnames(Default_Data)

# BHK RENT SIZE BATHROOM is in Integer format
library(dplyr)
IntegerData <- Default_Data %>% select(BHK, Rent, Size, Bathroom)
for (col in names(IntegerData))
{
  print(col) # See range 
  print(range(IntegerData[[col]]))
}


# To see the correlation of the data
cor_Data = cor(Default_Data[c(2,3,4,11)]) # Sub Column BHK rent Size Bathroom

cor_Data_df <- data.frame(as.table(cor_Data)) # Convert to data frame

install.packages("corrplot")
library("corrplot")

corrplot(cor_Data,method="number",
         # Choose Colour
         tl.col = "black",
         
         # Limit decimals to 4
         number.digits = 4,
         
         # Choose a bottom left orientation 
         type="lower",
         
         # Select colors
         col=colorRampPalette(c("green","yellow","red"))(100)
) 

#Variance
var_Data= var(Default_Data[c(2,3,4,11)])#column data

var_df = data.frame(as.table(var_Data))

ggplot(var_df, aes(x = Var1, y = Var2, fill = Freq)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  scale_alpha_continuous(range = c(0, 1), breaks = seq(0, 1, by = 0.1)) +
  theme_minimal() +
  labs(title = "Variance on house rentvariables") 

# Uniqueness of each cities 
unique(Default_Data$City)

# Locate India as Region
india_map <- map_data("world") %>% filter(region == "India")

# Define the variables for cities and assign longitude and latitude
cities <- data.frame(
  City = c("Kolkata", "Mumbai", "Chennai", "Delhi", "Bangalore", "Hyderabad"),
  latitude = c(22.5726, 19.0760, 13.0827, 28.6139, 12.9716, 17.3850),
  longitude = c(88.3639, 72.8777, 80.2707, 77.2090, 77.5946, 78.4867)
)

# Create a map of India with the cities plotted on it and city names displayed
ggplot() +
  
  # Allocate variables based on chosen Map
  geom_polygon(data = india_map, aes(x = long, y = lat, group = group), fill = "pink", color = "purple") +
  
  geom_point(data = cities, aes(x = longitude, y = latitude), color = "black") +
  
  # V-Just to adjust wording upwards & downwards
  geom_text(data = cities, aes(x = longitude, y = latitude, label = City), vjust = -1, color = "Red") +  
  
  # Add labels
  labs(x = "Longitude of World Map", y = "Latitude of World Map", title = "India Map") + 
  
  # Reduce background grey
  theme_minimal()

  ggplot(Default_Data, aes(x = City)) + geom_bar(fill = 'grey') +
  geom_text(stat = 'count', aes(label = after_stat(count)), color = 'black') + 
  labs(x = 'City / Region', y = 'House Count', title = 'Number of Houses In Different Cities')

# Explore Furnishing Status
unique(Default_Data$Furnishing.Status)

# Explore the Percentage
Percentage <- c(sum(Default_Data$Furnishing.Status == "Unfurnished"), 
                sum(Default_Data$Furnishing.Status == "Semi-Furnished"),
                sum(Default_Data$Furnishing.Status == "Furnished"))
Percentage_Labels <- c("Unfurnished", "Semi-Furnished", "Furnished")
piepercent<- round(100*Percentage/sum(Percentage), 0)
pie(Percentage, labels = piepercent, main = "Data Set pie chart",col = rainbow(length(Percentage)))
legend(x = "topright", c("Unfurnished", "Semi-Furnished", "Furnished"), cex = 0.6,
       fill = rainbow(length(Percentage)))

# BHK GRAPH to see distribution of Frequency against BHK
ggplot(Default_Data, aes(x = factor(BHK))) +
  geom_bar(fill = 'grey') +
  geom_text(stat = 'count', aes(label = after_stat(count)), color = 'black') +
  labs(x = 'BHK', y = 'Count') +
  theme_minimal()

# Show the average mean of Rental Fees based on BHK value 1 - 6
ggplot(Default_Data, aes(x = factor(BHK), y = Rent)) +
  stat_summary(fun = "mean", geom = "bar", fill = "grey", color = "black") +
  geom_text(stat = 'summary', fun = 'mean', aes(label = scales::comma(..y..)), color = 'black') +
  labs(x = 'Number of BHK', y = 'Average Rental Fees') +
  scale_y_continuous(labels = scales::comma)

ggplot(Default_Data, aes(x = Size)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "black") +
  labs(title = "Histogram of Property Size", x = "Size", y = "Frequency")

ggplot(Default_Data, aes(x = Size, y = Rent)) +
  
  # Provide dotted indication
  geom_point() +
  
  # Set labels
  labs(title = "Rental Fee vs Rental Size", x = "Size", y = "Rent") + 
  
  # Remove scientific notations
  scale_y_continuous(labels = scales::comma) +
  
  # Remove background grey
  theme_minimal()


# Bar Plot for Bathroom
barplot(sort(table(Default_Data$Bathroom), decreasing = T))

############################## Data Cleaning Line 177  --> Line 233

# Convert the Date into a date format
Default_Data$Posted.On = as.Date(Default_Data$Posted.On, "%m/%d/%Y")
for (i in 1:nrow(Default_Data)) 
{
  if (is.na(Default_Data$Posted.On[i])) 
  {
    row_data <- Default_Data[i, ]
    print(row_data)
  }
}

# Plot to see the Rent over Rows
plot(Default_Data$Rent, main="Rental Price of Data Sets", xlab="Rows", ylab="Rental Amount")

# Check for any value lower than 2000 because assume 2000 is bare minimum for house
min(Default_Data$Rent)
smalldata <- Default_Data[Default_Data$Rent < 2000, ]
View(smalldata)

# Check for highest value
max(Default_Data$Rent)
bigdata <- Default_Data[Default_Data$Rent >= 3500000, ]
View(bigdata)

# Remove Highest Rent
Default_Data <- Default_Data[Default_Data$Rent != 3500000, ]

# Remove Lowest Rent 
Default_Data <- Default_Data[!(Default_Data$Rent == 1200 
                               & Default_Data$Size == 2100 & Default_Data$City == "Hyderabad"), ]

# Plot to see Size over Rows
plot(Default_Data$Size, main="Size of houses", xlab="Rows", ylab="Size")

# Check highest Size
max(Default_Data$Size)
bigSize <- Default_Data[Default_Data$Size >= 6000,]
View(bigSize)

# Remove highest Size
Default_Data <- Default_Data[!Default_Data$Bathroom == 10, ]

# Check lowest Size
min(Default_Data$Size)
smallSize <- Default_Data[Default_Data$Size < 30, ]
View(smallSize)

# Remove Lowest size
Default_Data <- Default_Data[Default_Data$Size >= 30, ]


# Now our group drop unwanted columns that wouldn't be analyse
Default_Data <- Default_Data %>% select(-Point.of.Contact)

Default_Data <- Default_Data %>% select(-Area.Locality)

############################## Objective 1: To identify the relationship between House Sizes and rental prices Line 235 --> Line 304
# Tan Shu Jing 
summary(Default_Data$Size)
summary(Default_Data$Rent)

# Sub Question 1 : Is there any relationship between House Size and Rental Prices
# Create a scatter plot with a regression line
ggplot(Default_Data, aes(x = Size, y = Rent)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") + #Adds a linear regression line
  labs(x = "House Size", y = "Rental Price") +
  ggtitle("Relationship between House Size and Rental Prices") +
  ylim(0, NA) #Set the lower limit to 0


# Sub Question 2 : Does house size in different cities effect the rental prices 
# The co-variation between House Size and City
ggplot(Default_Data, aes(x = City, y = Size, fill = City)) +
  geom_boxplot() +
  facet_wrap(~ City, scales = "free") +
  labs(title = "The co-variation between House Size and City", x = "City", y = "Size")

# The co-variation between Rental Price and City
ggplot(Default_Data, aes(x = City, y = Rent, fill = City)) +
  geom_boxplot() +
  facet_wrap(~ City, scales = "free") +
  labs(title = "The co-variation between Rental Price and City", x = "Rent", y = "Size") +
  scale_y_continuous(labels = scales::comma_format()) #Format y-axis labels

# House Size and Rental Prices in Different Cities
ggplot(Default_Data, aes(x = Size, y = Rent, color = City)) +
  geom_point() +
  facet_wrap(~ City, scales = "free") +
  labs(title = "House Size and Rental Prices in Different Cities", x = "House Size", y = "Rental Price") +
  scale_y_continuous(labels = scales::comma_format()) + #Format y-axis labels
  theme_minimal()

# Effect of House Size in Different Cities on Rental Price
city_rent_summary <- Default_Data %>%
  group_by(City) %>%
  summarise(avg_rent = mean(Rent), avg_size = mean(Size))

ggplot(data = city_rent_summary, aes(x = avg_size, y = avg_rent, color = City)) +
  geom_point() +
  labs(title = "Effect of House Size in Different Cities on Rental Prices", 
       x = "Average House Size", y = "Average Rental Price", color = "City")

# Sub Question 3 : Relationship between House Size and Rental Prices by Furnishing Status

# The distribution of House Size on Furnishing Status
ggplot(Default_Data, aes(x = Size, fill = Furnishing.Status)) +
  geom_histogram() +
  facet_wrap(~ Furnishing.Status) +
  ggtitle("The distribution of House Size on Furnishing Status")

# The distribution of Rental Price on Furnishing Status
ggplot(Default_Data, aes(x = Rent, fill = Furnishing.Status)) +
  geom_histogram() +
  facet_wrap(~ Furnishing.Status) +
  ggtitle("The distribution of Rental Price on Furnishing Status")

# Relationship between House Size and Rental Prices by Furnishing Status
ggplot(Default_Data, aes(x = Size, y = Rent, color = Furnishing.Status, alpha = 0.7)) +
  geom_jitter() +
  labs(x = "House Size", y = "Rental Price", color = "Furnishing Status") +
  scale_color_manual(values = c("Furnished" = "blue", "Semi-Furnished" = "green", "Unfurnished" = "red")) +
  ggtitle("Relationship between House Size and Rental Prices by Furnishing Status") +
  facet_wrap(~ Furnishing.Status, nrow = 1)


############################## Objective 2 : To identify how furnishing status affects Rental Price Line 306 --> Line 481
#Sub Question 1: Does furnished houses the same rent price as semi-furnished and unfurnished houses?

#Analysis1
# This Pie Chart is used to show the frequency of the furnishing status
ggplot(Default_Data, aes(x = "", fill = Furnishing.Status)) +
  geom_bar(width = 1) +
  coord_polar(theta = "y") +
  geom_text(stat = "count", aes(label = stat(count)), position = position_stack(vjust = 0.5)) +
  labs(title = "Frequency of Furnishing Status",
       fill = "Furnishing Status",
       x = NULL,
       y = NULL) +
  scale_fill_manual(values = c("Furnished" = "dodgerblue", 
                               "Semi-Furnished" = "limegreen", 
                               "Unfurnished" = "coral")) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank())


# This bar plot is used to show the frequency of the furnishing status
ggplot(Default_Data, aes(x = `Furnishing.Status`, fill = `Furnishing.Status`)) +
  geom_bar() +
  geom_text(stat = "count", aes(label=stat(count)), vjust = 1)+
  labs(title = "Frequency of Furnishing Status",
       x = "Furnishing Status",
       y = "Frequency") +
  scale_fill_manual(values = c("Furnished" = "dodgerblue", 
                               "Semi-Furnished" = "limegreen", 
                               "Unfurnished" = "coral")) +
  theme_minimal()





#Analysis2
# Create a scatter plot
ggplot(Default_Data, aes(x = Furnishing.Status, y = Rent, color = Furnishing.Status)) +
  geom_point() +
  labs(title = "Scatter Plot of Rental Prices vs Furnishing Status",
       x = "Furnishing Status",
       y = "Rental Price") +
  scale_color_manual(values = c("Furnished" = "dodgerblue", 
                                "Semi-Furnished" = "limegreen", 
                                "Unfurnished" = "coral")) +
  theme_minimal()


# This bar plot is used to compare rental prices for each furniture state
ggplot(Default_Data, aes(x = factor(Furnishing.Status, levels = c("Furnished", "Semi-Furnished", "Unfurnished")),
                         y = Rent, fill = Furnishing.Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Rental Prices vs Furnishing Status",
       x = "Furnishing Status",
       y = "Rental Price") +
  scale_fill_manual(values = c("Furnished" = "dodgerblue", "Semi-Furnished" = "limegreen", "Unfurnished" = "coral")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma)



#This violin plot is used to compare total rental prices for each furniture status
ggplot(Default_Data, aes(x = Furnishing.Status, y = Rent, fill = Furnishing.Status)) +
  geom_violin(trim = FALSE) +
  geom_text(stat = "summary", fun = median, aes(label = round(..y.., 2)),
            position = position_nudge(x = 0.2), vjust = 0) +
  geom_text(stat = "summary", fun = sum, aes(label = paste("Total:", scales::comma(round(sum(..y..), 2)))),
            position = position_nudge(x = -0.2), vjust = 0) +
  labs(title = "Violin Plot of Rental Prices vs Furnishing Status",
       x = "Furnishing Status",
       y = "Rental Price") +
  scale_fill_manual(values = c("Furnished" = "dodgerblue", 
                               "Semi-Furnished" = "limegreen", 
                               "Unfurnished" = "coral")) +
  theme_minimal() +
  scale_y_continuous(labels = scales::comma) +
  coord_cartesian(ylim = c(0, max(Default_Data$Rent) + 10000)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



#Analysis3
# This bar plot is used to compare the average rental price for each furniture status
ggplot(Default_Data, aes(x = factor(Furnishing.Status, levels = c("Furnished", "Semi-Furnished", "Unfurnished")),
                         y = Rent, fill = Furnishing.Status)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Average Rental Prices vs Furnishing Status",
       x = "Furnishing Status",
       y = "Average Rent") +
  scale_fill_manual(values = c("Furnished" = "dodgerblue", "Semi-Furnished" = "limegreen", "Unfurnished" = "coral")) +
  theme_minimal()

#-------------------------------------------------------------------------------------------

#Sub Question 2: Does average rental prices vary based on the furnishing status of the houses and the city?


#Analysis 1: Average Rental Prices by Furnishing Status VS City
# This grouped bar plot is show the Average Rental Prices by Furnishing Status VS City
ggplot(Default_Data, aes(x = factor(Furnishing.Status),
                         y = Rent,
                         fill = factor(City))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Rental Prices by Furnishing Status in Different Cities",
       x = "Furnishing Status",
       y = "Average Rent") +
  scale_fill_manual(values = c("Bangalore" = "dodgerblue",
                               "Chennai" = "limegreen",
                               "Delhi" = "orange",
                               "Hyderabad" = "purple",
                               "Kolkata" = "red",
                               "Mumbai" = "blue")) +
  theme_minimal()

# This grouped bar plot with facets is show the Average Rental Prices by Furnishing Status VS City
ggplot(Default_Data, aes(x = factor(Furnishing.Status),
                         y = Rent,
                         fill = factor(City))) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Average Rental Prices by Furnishing Status in Different Cities",
       x = "Furnishing Status",
       y = "Average Rent") +
  scale_fill_manual(values = c("Bangalore" = "dodgerblue",
                               "Chennai" = "limegreen",
                               "Delhi" = "orange",
                               "Hyderabad" = "purple",
                               "Kolkata" = "red",
                               "Mumbai" = "blue")) +
  theme_minimal() +
  facet_wrap(~ City, ncol = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(margin = margin(t = 10)))  # Add margin to facet labels




# This facetted violin plot is show the Average Rental Prices by Furnishing Status VS City
ggplot(Default_Data, aes(x = factor(Furnishing.Status),
                         y = Rent,
                         fill = factor(City))) +
  geom_violin(trim = FALSE, scale = "width", alpha = 0.8) +
  geom_jitter(aes(color = factor(City)), width = 0.2, alpha = 0.6) +
  labs(title = "Rental Prices Distribution by Furnishing Status in Different Cities",
       x = "Furnishing Status",
       y = "Rent") +
  scale_fill_manual(values = c("Bangalore" = "dodgerblue",
                               "Chennai" = "limegreen",
                               "Delhi" = "orange",
                               "Hyderabad" = "purple",
                               "Kolkata" = "red",
                               "Mumbai" = "blue")) +
  scale_color_manual(values = c("Bangalore" = "dodgerblue",
                                "Chennai" = "limegreen",
                                "Delhi" = "orange",
                                "Hyderabad" = "purple",
                                "Kolkata" = "red",
                                "Mumbai" = "blue")) +
  theme_minimal() +
  facet_wrap(~ City, ncol = 2, strip.position = "bottom") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        strip.text = element_text(margin = margin(t = 10))) +
  theme(legend.position = "top",
        legend.direction = "horizontal",
        legend.title = element_blank()) +
  guides(fill = guide_legend(title = "City"),
         color = guide_legend(title = "City")) +
  scale_y_continuous(labels = scales::comma)

############################## Objective 3 : To determine whether House Size is affected by BHK and Bathroom.
# Anova 
summary(aov(Bathroom ~ BHK, data = Default_Data))

# Take Bathroom as X-Axis and BHK as Y-Axis
ggplot(Default_Data, aes(x = factor(BHK), y = Bathroom)) +
  
  # Use bar to display 
  geom_bar(stat = "summary", fill = "purple") +
  
  # Set title 
  labs(title = "Relationship of Bathroom and BHK",
       x = "BHK frequency", y = "Bathroom Frequency") + 
  
  theme_light() 

# Take BHK as X-Axis and Bathroom as Y-Axis
ggplot(Default_Data, aes(x = (Bathroom), y = (BHK))) + 
  
  # Scatter Plot
  geom_count() +
  
  # Add a line through linear model 
  geom_smooth(method = "lm", se = F) + 
  
  # Add labels
  labs(title = "The covariance between BHK and Bathroom", x = "Bathroom",y="BHK")

#Sub Question 2 Does the number of BHK effect the Sizes of house.
# To show Box Plot
ggplot(Default_Data, aes(x = factor(BHK), y = Size, fill = factor(BHK))) +
  
  # Use Blox Plot
  geom_boxplot() +
  
  # Add labels
  labs(title = "Box Plot of House Size vs BHK", x = "BHK", y = "House Size")

# Violin graph + Box Plot
ggplot(Default_Data, aes(x = factor(BHK), y = Size, fill = factor(BHK))) +
  
  # Plot Violin
  geom_violin(scale = "width", trim = FALSE) +
  
  # Select Fill
  geom_boxplot(fill = "grey") +
  
  # Add labels
  labs(title = "Violin Box Plot for Size vs BHK", x = "BHK Number", y = "Rental Size") 

# Sub Question 3 Does the number of Bathroom effect the Sizes of house. 
# Select Size as X-Axis Bathroom as Y-Axis
ggplot(Default_Data, aes(x = Size, y = Bathroom)) +
  # Jitter Plot
  geom_jitter() +
  
  # Add a line
  geom_smooth() +
  
  # Add labels
  labs(title = "Jittle Plot of Bathroom vs Size")

# Sub Question 4 What differs among the cities based on BHK and Bathrooms
ggplot(Default_Data, aes(x = factor(Bathroom), fill = City)) +
  # Use Bar Chart
  geom_bar() +
  
  # Divide into different graphs
  facet_wrap(~ City, ncol = 2) +
  
  # Add Tittle
  labs(title = "Frequency of Houses vs Bathroom in different City", x = "Bathroom", y = "Frequency of House")

ggplot(Default_Data, aes(x = factor(BHK), fill = City)) +
  # User Bar Chart
  geom_bar() +
  
  # Divide into 6 graphs
  facet_wrap(~ City, ncol = 2) +
  
  # Add labels
  labs(title = "Frequency of Houses vs BHK in Different City ", x = "BHK", y = "Frequency")

# Decision Tree
DecisionTree <- rpart(Size ~ BHK + Bathroom, data = Default_Data)

# Plot Decision Tree
rpart.plot(DecisionTree)

############################## Objective 4 : To Predict Factor Influencing Rent Price

#t-test
#H0: The mean of Rent  for both "Furnished" and "semi-furning or unfurning" species are equal.
#H1: The mean of Rent  for both "Furnished" and "semi-furning or unfurning" species are not equal.
furnishingstate=Default_Data[Default_Data$Furnishing.Status=="Furnished",]
nofurnishingstate=Default_Data[!Default_Data$Furnishing.Status=="Furnished",]
t.test(furnishingstate$Rent,nofurnishingstate$Rent)
#p-value < 5.259e-15 (very small)
#conclusion : reject H0
#Simple linear regresion one to one to Rent
summary(lm(Default_Data$Rent~Default_Data$BHK))
summary(lm(Default_Data$Rent~Default_Data$Size))
summary(lm(Default_Data$Rent~Default_Data$City))
summary(lm(Default_Data$Rent~Default_Data$Bathroom))
summary(lm(Default_Data$Rent~Default_Data$Furnishing.Status))
#Multiple Regression
summary(lm(Rent~Size+Furnishing.Status+City+Bathroom,data=Default_Data))

#Variance Inflation Factor
vif(lm(Rent~Size+Furnishing.Status+City+Bathroom,data=Default_Data))
#Rent Distribution
qqnorm(Default_Data$Rent)
qqline(Default_Data$Rent)
#GLM
summary(glm(Rent~Size+Furnishing.Status+City+Bathroom,data=Default_Data,family=Gamma(link = "log")))
#Random Forest betwenn Rent and another factor

set.seed(120) #set random seed make every random sequence same
dt <- sample(nrow(Default_Data), nrow(Default_Data) * 0.7) # training set 70%
train <- Default_Data[dt, ]
test <- Default_Data[-dt, ] #test set 30%
rf <- randomForest(Rent ~ Size + Furnishing.Status + City + Bathroom + BHK, data = train, importance = TRUE)
print(rf)

rf_pred_test <- predict(rf, test, type = 'response') # Use 'response' for regression to the test data
rmse <- sqrt(mean((rf_pred_test - test$Rent)^2)) #the differences between each predicted value and the corresponding actual rent value
mae <- mean(abs(rf_pred_test - test$Rent)) #absolute differences between each predicted value and the corresponding actual rent value
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")

#plot graph
options(scipen = 999)#show the x and y not in scient
plot(test$Rent, rf_pred_test, xlab = "Actual Rent", ylab = "Predicted Rent")


############################## Additional Research
#Date with Rental Price with geom_point
ggplot(Default_Data, aes(x = Posted.On, y =Rent)) + 
  geom_point() +
  labs(title = "Date posted with Rent price",  x = "Date posted", y = "Total_Rent")
ggplot(Default_Data, aes(x = Posted.On, y =Rent)) + 
  geom_point() + coord_cartesian(ylim=c(0,150000)) + 
  labs(title = "Date posted with Rent price",  x = "Date posted", y = "Total_Rent")
# total rent time series
TotalRent_Date = Default_Data  %>% mutate(month = lubridate::floor_date(Default_Data$Posted.On, 'month'), Rent) %>% group_by(month) %>% summarise(Total_Rent = sum(Rent))
ggplot(TotalRent_Date, aes(x = month, y = Total_Rent)) + 
  geom_line() + 
  labs(title = "Total Rent with month", x = "Month", y = "Total Rent")+
  scale_y_continuous(labels = scales::comma)
#Total rent distributed by Furnishing Status over the month
TotalRent_DatewithFurnishing <- Default_Data %>% 
  mutate(month = lubridate::floor_date(Default_Data$Posted.On, 'month')) %>% 
  group_by(month, Furnishing.Status) %>%  
  summarise(Total_Rent = sum(Rent)) 
ggplot(TotalRent_DatewithFurnishing, aes(x = month, y = Total_Rent, fill = Furnishing.Status)) + 
  geom_col(position = "dodge") +  
  #geom_text(aes(label = scales::comma(Total_Rent), y = Total_Rent), position = position_dodge(width = 0.9), vjust = -0.5)+
  labs(title = "Total Rent by Furnishing Status in month",
       x = "Month",
       y = "Total Rent") +
  scale_fill_brewer(palette = "Set3")+
  scale_y_continuous(labels = scales::comma)

#Total Furnishing Status frequency over the month
TotalFrequency_DatewithFurnishing <- Default_Data %>% 
  mutate(month = lubridate::floor_date(Default_Data$Posted.On, 'month')) %>% 
  group_by(month, Furnishing.Status) %>%  
  summarise(Frequency = n())
ggplot(TotalFrequency_DatewithFurnishing, aes(x = month, y = Frequency, fill = Furnishing.Status)) + 
  geom_col(position = "dodge") +  
  labs(title = "Frequency of Rentals by Furnishing Status in month",
       x = "Month",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma)
#lollipop chart
ggplot(TotalFrequency_DatewithFurnishing, aes(x = month, y = Frequency)) +
  geom_segment(aes(xend = month, yend = 0), color = "gray") +
  geom_point(aes(fill = Furnishing.Status), shape = 21, size = 4) +
  labs(title = "Frequency of Rentals by Furnishing Status in Month",
       x = "Month",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# Frequency of rental by furnishing status over the month in different city
TotalFrequency_DatewithFurnishinginCity <- Default_Data %>% 
  mutate(month = lubridate::floor_date(Posted.On, 'month')) %>% 
  group_by(month, Furnishing.Status, City) %>%  
  summarise(Frequency = n())
TotalFrequency_DatewithFurnishinginCity
ggplot(TotalFrequency_DatewithFurnishinginCity, aes(x = month, y = Frequency, fill = Furnishing.Status)) +
  geom_segment(aes(xend = month, yend = 0), color = "gray") +
  geom_point(shape = 21, size = 4) +
  labs(title = "Frequency of Rentals by Furnishing Status in Month",
       x = "Month",
       y = "Frequency") +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~ City, nrow = 2, scales = "free_x")


