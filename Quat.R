library(tidyverse)
library(ggplot2)
library(maps)
indicator_1 <- read.csv("D:/unicef_indicator_1.csv")
indicator_2 <- read.csv("D:/unicef_indicator_2.csv")
metadata <- read.csv("D:/unicef_metadata.csv")
# Visualization 2: Bar Chart
# Visualizing proportion of students achieving minimum proficiency in reading by country
bar_chart <- ggplot(data = indicator_1, aes(x = reorder(country, obs_value), y = obs_value, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Proportion of Students Achieving Minimum Proficiency in Reading",
       x = "country",
       y = "obs_value",
       fill = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
print(bar_chart)
# Visualization 4: Time-Series Chart
# Example: Institutional deliveries percentage over time
time_series <- ggplot(data = indicator_2, aes(x = time_period, y = obs_value, color = sex)) +
  geom_line() +
  labs(title = "Institutional Deliveries Percentage Over Time",
       x = "Time Period",
       y = "Observed Value",
       color = "sex") +
  theme_minimal()
print(time_series)
## Scatter Plot visualising Literacy Rates Among Lower Secondary Students
# Merge the datasets based on common columns
merged_data <- merge(indicator_1, metadata, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code"))
scatterplot_data <- merged_data %>%
  filter(indicator == "Proportion of students at the end of lower secondary achieving at least a minimum proficiency level in reading") %>%
  select(country, obs_value)
scatterplot <- ggplot(scatterplot_data, aes(x = country, y = obs_value)) +
  geom_point() +      # Add points for each country
  geom_smooth(method = "lm", se = FALSE) +  # Add linear regression line
  theme_minimal() +   # Apply a minimal theme
  labs(
    title = "Literacy Rates Among Lower Secondary Students",
    x = "country",
    y = "Proportion of Students (%)"
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))  # Rotate x-axis labels for better readability
print(scatterplot)
##World Map
# Merge the datasets based on common columns
merged_data <- merge(indicator_1, metadata, by = c("country", "alpha_2_code", "alpha_3_code", "numeric_code"))
# Aggregate data to country level
country_data <- merged_data %>%
  group_by(country) %>%
  summarize(Average_obs_Value = mean(obs_value, na.rm = TRUE))
# Downloading world map data
world_map <- map_data("world")
# Merging country data with world map data
map_data <- merge(world_map, country_data, by.x = "region", by.y = "country", all.x = TRUE)
# Plotting the world map chart
world_map_chart <- ggplot(map_data, aes(x = long, y = lat, group = group, fill = Average_obs_Value)) +
  geom_polygon(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "grey") +
  labs(title = "Average Observed Value by Country",
       fill = "Average Observed Value") +
  theme_void()
# Displaying the world map chart
print(world_map_chart)


