#ASSIGNMENT OF 22-05-2025

library(tidyverse)
library(janitor)
library(ggplot2)

# 1. Trend Analysis Plot - PM10 for 4 cities
cities <- c("Delhi", "Mumbai", "Kolkata", "Chennai")
pm10_data <- df %>% filter(City %in% cities)

ggplot(pm10_data, aes(x = Date, y = PM10, color = City)) +
  geom_line() +
  facet_wrap(~City, scales = "free_y") +
  theme_linedraw(base_size = 14) +
  labs(title = "PM10 Trend Over Time", y = "PM10", x = "Date") ->plota

ggsave("pm_10 Polutant Trends over Time.pdf",
       plot = plota,
       units = "in",
       width = 10,
       height = 6)

# 2. Category-wise AQI Histogram
ggplot(df, aes(x = AQI, fill = AQI_Bucket)) +
  geom_histogram(binwidth = 50, color = "black") +
  scale_x_continuous(breaks = seq(0, 500, 50), limits = c(0, 500)) +
  theme_linedraw(base_size = 14) +
  labs(title = "AQI Distribution by Category", x = "AQI", y = "Count")-> plotb
ggsave("AQI Distribution by Category.pdf",
       plot = plotb,
       units = "in",
       width = 10,
       height = 6)


# 3. Pollutant Relationship Scatter Plot

ggplot(df, aes(x = PM2.5, y = NO, color = AQI_Bucket, size = O3, shape = City))+
  geom_point(alpha = 0.6) +
  scale_x_continuous(breaks = seq(0, max(df$`PM2.5`, na.rm = TRUE), by = 100),limits = c(0, 500)) +
  scale_y_continuous(breaks = seq(0, max(df$NO, na.rm = TRUE), by = 100)) +
  theme_linedraw(base_size = 7.5) +
  labs(title = "PM2.5 vs NO Across States", x = "PM2.5", y = "NO")
  

  
# 4. Faceted Histogram of CO by AQI_Bucket
df_clean <- df %>%
  filter(!is.na(CO), !is.na(AQI_Bucket))

# Plot
ggplot(data = df_clean, mapping = aes(x = CO, fill = AQI_Bucket)) +
  geom_histogram(binwidth = 0.5, color = "green") +
  facet_wrap(~ AQI_Bucket, scales = "free_y") +
  scale_fill_brewer(palette = "Set2") +
  theme_linedraw(base_size = 10)+
  labs(
    title = "CO Distribution by AQI Bucket",
    x = "CO Concentration (mg/m³)",
    y = "Frequency"
  )

#5. SO2 Line Type Comparison

df <- read_csv("city_day.csv")

# Convert Date column to Date format
df$Date <- as.Date(df$Date)

# Filter for Delhi and Chennai
so2_data <- df %>% filter(City %in% c("Delhi", "Chennai"))

ggplot(so2_data, aes(x = Date, y = SO2, color = City)) +
  geom_line(size = 1) +
  facet_wrap(~City, scales = "free_y") +
  theme_linedraw(base_size = 14) +
  labs(
    title = "SO₂ Levels Over Time: Delhi vs Chennai.p",
    x = "Date",
    y = "SO₂ Concentration (µg/m³)"
  )-> plote

ggsave("SO₂ Levels Over Time: Delhi vs Chennai.pdf",
       plot = plote,
       units = "in",
       width = 10,
       height = 6)


#6. Annotate a Special Observation
max_aqi_row <- df %>% filter(!is.na(AQI)) %>% arrange(desc(AQI)) %>% slice(1)

ggplot(df, aes(x = PM2.5, y = PM10)) +
  geom_point(alpha = 0.4, color = "red") +
  geom_point(data = max_aqi_row, aes(x = PM2.5, y = PM10), color = "blue", size = 4) +
  geom_text(data = max_aqi_row, aes(x = PM2.5, y = PM10, label = paste("Max AQI:", AQI)), 
            vjust = -1, color = "blue", fontface = "bold") +
  theme_classic(base_size = 14) +
  labs(title = "Highlight of Maximum AQI Observation", x = "PM2.5", y = "PM10")
