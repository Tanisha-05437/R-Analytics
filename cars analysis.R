#load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

#load the dataset
cars_data<-read.csv("C:/Users/tanisha singhania/Desktop/used_cars_data.csv")
View(cars_data)

##View the first few rows of dataset
View(head(cars_data,10))

#View the last few rows of dataset
View(tail(cars_data,10))

#Check the structure of dataset
str(cars_data)

#summary statstics of dataset
print(summary(cars_data))

#check for missing values
colSums(is.na(cars_data))

#remove duplicate rows,if any
cars_data<-cars_data%>%  distinct()

#remove rows with missing values using na.omit()
cars_data<-na.omit(cars_data)
View(cars_data)

#ANALYSIS AND VISUALISATION

#1. Average car price by location
avg_price_by_location <- cars_data %>%
  group_by(Location) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))
View(avg_price_by_location)

#2. Bar plot of avg car price by location
ggplot(avg_price_by_location, aes(x = reorder(Location, -avg_price), y = avg_price, fill = Location)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Car Price by Location", x = "Location", y = "Average Price") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#3. Most common fuel type
fuel_counts <- cars_data %>%
  count(Fuel_Type) %>%
  arrange(desc(n))
View(fuel_counts)

#4. Bar plot for most common fuel type
ggplot(fuel_counts, aes(x = reorder(Fuel_Type, -n), y = n, fill = Fuel_Type)) +
  geom_bar(stat = "identity") +
  labs(title = "Most Common Fuel Types", x = "Fuel Type", y = "Count") 

#5. Transmission type and calculate the average price
transmission_impact <- cars_data %>%
  group_by(Transmission) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE))
View(transmission_impact)

#6. Create a box plot to visualize the impact
ggplot(cars_data, aes(x = Transmission, y = Price, fill = Transmission)) +
  geom_boxplot(outlier.colour = "red", outlier.size = 3, notch = TRUE) +
  labs(
    title = "Impact of Transmission Type on Car Prices",
    x = "Transmission Type",
    y = "Car Price"
  ) +
  theme_minimal() 

#7. Group data by Owner_Type and calculate the average price
owner_price <- cars_data %>%
  group_by(Owner_Type) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE))
View(owner_price)

# Add a percentage column for the pie chart
owner_price <- owner_price %>%
  mutate(percentage = round((avg_price / sum(avg_price)) * 100, 1))

#8. Create a pie chart
ggplot(owner_price, aes(x = "", y = avg_price, fill = Owner_Type)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  labs(
    title = "Impact of Owner Type on Average Car Price",
    fill = "Owner Type"
  ) +
  geom_text(aes(label = paste0(percentage, "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() 

#9. Group data by Seats and calculate the average price
seats_avg_price <-cars_data %>%
  group_by(Seats) %>%
  summarize(avg_price = mean(Price, na.rm = TRUE)) %>%
  arrange(desc(avg_price))
View(seats_avg_price)

#10. Create a density plot
ggplot(cars_data, aes(x = Price, fill = factor(Seats))) +
  geom_density(alpha = 0.6) +
  labs(
    title = "Density Plot: Car Price Distribution by Number of Seats",
    x = "Car Price",
    y = "Density",
    fill = "Number of Seats"
  ) +
  theme_minimal() 

#11. Group data into mileage categories (e.g., low, medium, high)
mileage_analysis <- cars_data %>%
  mutate(
    mileage_category = case_when(
      Mileage < 15 ~ "Low Mileage",
      Mileage >= 15 & Mileage < 25 ~ "Medium Mileage",
      Mileage >= 25 ~ "High Mileage"
    )
  ) %>%
  group_by(mileage_category) %>%
  summarize(
    avg_price = mean(Price, na.rm = TRUE),
    count = n()
  )

#12. Scatter plot between price vs. mileage
ggplot(cars_data, aes(x = Mileage, y = Price)) +
  geom_point(color = "blue",alpha=0.7) +
  labs(title="Price vs. Mileage", x="Mileage", y="Price")+
  theme_minimal()

#13. Most Popular Car Models
top_models <- cars_data %>%
  count(Name, sort = TRUE) %>%
  top_n(10)
View(top_models)

#14.Faceted Plot: Price vs. Mileage by Fuel Type
ggplot(cars_data, aes(x = Mileage, y = Price)) +
  geom_point(alpha = 0.5, color = "green") +
  facet_wrap(~ Fuel_Type) +
  labs(title = "Price vs. Mileage Faceted by Fuel Type", x = "Mileage", y = "Price")

#15. Histogram of car prices
ggplot(cars_data, aes(x = Price)) +
  geom_histogram(binwidth = 10, fill = "blue", color = "white") +
  theme_minimal() +
  labs(title = "Distribution of Car Prices", x = "Price", y = "Count")

#16. Distribution of cars by year of manufacture
ggplot(cars_data, aes(x = Year)) +
  geom_bar(fill = "purple") +
  theme_minimal() +
  labs(title = "Distribution of Cars by Year", x = "Year", y = "Count")

#17. Trend in average price by year
avg_price_by_year <- cars_data %>%
  group_by(Year) %>%
  summarise(Average_Price = mean(Price, na.rm = TRUE))
View(avg_price_by_year)

#18. Count the number of listings by year
listings_by_year <- cars_data %>%
      group_by(Year) %>%
      summarise(Count = n(), .groups = "drop")
View(listings_by_year)
    
#19. Line chart for the trend in listings
ggplot(listings_by_year, aes(x = Year, y = Count)) +
      geom_line(color = "darkgreen", size = 1) +
      geom_point(color = "orange", size = 2) +
      theme_minimal() +
      labs(
        title = "Trend of Listings Over the Years",
        x = "Year of Manufacture",
        y = "Number of Listings"
      )

#20. Find the most common fuel type by year
common_fuel_by_year <- cars_data %>%
  group_by(Year, Fuel_Type) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(Year, desc(Count)) %>%
  filter(row_number() == 1)
View(common_fuel_by_year)


    


  