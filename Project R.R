# Load necessary libraries
library(tidyverse)
library(dplyr)
library(ggplot2)

agriculture_data <- read.csv("E:/FInal Project/FAO.csv")

# Display the structure of the dataframe
str(agriculture_data)

# Convert from a wide dataset to a long dataset using pivot_longer
agriculture_data_long <- agriculture_data %>%
  pivot_longer(cols = starts_with("Y"),
               names_to = "Year",
               values_to = "Production")

summary(agriculture_data_long)

# Convert years to integers
agriculture_data_long$Year <- as.integer(gsub("Y", "", agriculture_data_long$Year))

agriculture_data_long <- agriculture_data_long %>%
  drop_na(Production)

summary(agriculture_data_long)

rows_with_negative_production <- which(agriculture_data_long$Production < 0)
print(rows_with_negative_production)

agriculture_data_filtered <- agriculture_data_long %>%
  filter(Production >= 0)

summary(agriculture_data_filtered)

agriculture_analysis_data <- agriculture_data_filtered %>%
  select(Area,Item,Element,Unit,Year,Production)

summary(agriculture_analysis_data)

unique(agriculture_data_filtered$Element)

agriculture_analysis_data<- agriculture_analysis_data %>%
  mutate(Element = factor(Element))

# Assuming 'agriculture_analysis_data' is your dataset
aggregated_data <- agriculture_analysis_data %>%
  group_by(Area, Item, Unit, Year, Element) %>%
  summarise(Production = sum(Production), .groups = "drop")

summary(aggregated_data)

aggregated_data <- pivot_wider(
  data = aggregated_data,
  names_from = Element,
  values_from = Production,
  values_fill = 0
)

#################### EXPLORATORY DATA ANALYSIS ###########################

# Visualize the trends over time
ggplot(aggregated_data, aes(x = Year)) +
  geom_line(aes(y = Food, color = "Total Food")) +
  geom_line(aes(y = Feed, color = "Total Feed")) +
  labs(title = "Total Food and Feed Production Over Time",
       x = "Year", y = "Production Quantity")

#Top 5 food producers since 1961

# Calculate top 5  food producers

largest_food_producers <- aggregated_data %>%
  group_by(Area) %>%
  summarize(Food = sum(Food, na.rm = TRUE)) %>%
  arrange(desc(Food))

# Display the top N largest food producers (e.g., top 5)
top_n_producers <- 5
top_food_producers <- head(largest_food_producers, n = top_n_producers)

# Convert production to million tons
top_food_producers$Total_Food_Million_Tons <- top_food_producers$Food / 1e6

# Visualization (bar chart)
ggplot(top_food_producers, aes(x = reorder(Area, Total_Food_Million_Tons), y = Total_Food_Million_Tons)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = paste("Top", top_n_producers, "Largest Food Producers"),
       x = "Country/Region", y = "Total Food Production(in  millon Tonnes)")


largest_feed_producers <- aggregated_data %>%
  group_by(Area) %>%
  summarize(Feed = sum(Feed, na.rm = TRUE)) %>%
  arrange(desc(Feed))

# Display the top N largest food producers (e.g., top 5)
top_n_producers <- 5
top_feed_producers <- head(largest_feed_producers, n = top_n_producers)

# Convert production to million tons
top_feed_producers$Total_Feed_Million_Tons <- top_feed_producers$Feed / 1e6

# Visualization (bar chart)
ggplot(top_feed_producers, aes(x = reorder(Area, Total_Feed_Million_Tons), y = Total_Feed_Million_Tons)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = paste("Top", top_n_producers, "Largest Feed Producers"),
       x = "Country/Region", y = "Total Feed Production(in  millon Tonnes)")

ggplot(agriculture_analysis_data, aes(x = Element, y = Production/1e6, fill = Element)) +
  geom_boxplot() +
  labs(title = "Box Plot of Food and Feed Production",
       x = "Element", y = "Production (Millions of Tons)") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) 

library(plotly)

# Example: Interactive plot for each item

p <- ggplot(aggregated_data, aes(x = Year, y = Food/1e6, color = Item)) +
  geom_line() +
  labs(title = "Production Trends for Each Item",
       x = "Year", y = "Production (Millions of Tons)")

# Convert to interactive plot
ggplotly(p)   

# Filter data for 'Food' element
food_data <- agriculture_analysis_data %>% filter(Element == "Food")

# Group by item, calculate the total production for each item
item_production <- food_data %>%
  group_by(Item) %>%
  summarize(Total_Production = sum(Production, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  head(5)  # Select the top 10 items

# Filter the original data for the top 10 food items
top_food_data <- food_data %>% filter(Item %in% item_production$Item)

top_food_data_aggregated <- top_food_data %>%
  group_by(Year, Item) %>%
  summarize(Total_Production = sum(Production, na.rm = TRUE))

# Create a line plot for the aggregated production of top food items
ggplot(top_food_data_aggregated, aes(x = Year, y = Total_Production/1e6, color = Item)) +
  geom_line() +
  labs(title = "Aggregated Production Trends of Top 10 Food Items",
       x = "Year", y = "Total Production (Millions of Tons)",
       color = "Food Item") +
  theme(legend.position = "right")


# Filter data for 'Feed' element
feed_data <- agriculture_analysis_data %>% filter(Element == "Feed")

# Group by item, calculate the total production for each item
item_production_feed <- feed_data %>%
  group_by(Item) %>%
  summarize(Total_Production = sum(Production, na.rm = TRUE)) %>%
  arrange(desc(Total_Production)) %>%
  head(5)  # Select the top 10 items

# Filter the original data for the top 10 feed items
top_feed_data <- feed_data %>% filter(Item %in% item_production_feed$Item)

top_feed_data_aggregated <- top_feed_data %>%
  group_by(Year, Item) %>%
  summarize(Total_Production = sum(Production, na.rm = TRUE))

# Create a line plot for the aggregated production of top feed items
ggplot(top_feed_data_aggregated, aes(x = Year, y = Total_Production/1e6, color = Item)) +
  geom_line() +
  labs(title = "Aggregated Production Trends of Top 10 Feed Items",
       x = "Year", y = "Total Production (Millions of Tons)",
       color = "Feed Item") +
  theme(legend.position = "right")