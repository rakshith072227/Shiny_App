# Assignment of 28/05/2025

#1 . Air quality trends for bengaluru
pol_data %>%
  filter(City == "Bengaluru") %>%
  group_by(Year, pollutant) %>%
  summarise(mean_values = mean(values, na.rm = T)) -> bengaluru_trends

bengaluru_trends

bengaluru_trends %>%
  ggplot(aes(x = Year, y = mean_values)) +
  geom_line(color = "red") +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(
    title = "Air Quality Trends for Bengaluru",
    subtitle = "2015–2020",
    x = NULL,
    y = "Pollutant Values",
    caption = "Source: city_day"
  ) +
  theme_linedraw()-> plot2

ggsave("Bengaluru Polutants Trends.pdf",
       plot = plot2,
       units = "in",
       width = 10,
       height = 6)

#2. co trends for all cities
pol_data %>%
  filter(pollutant == "CO") %>%
  group_by(Year, City) %>%
  summarise(mean_CO = mean(values, na.rm = T)) -> co_trends

co_trends

co_trends %>%
  ggplot(aes(x = Year, y = mean_CO, color = City)) +
  geom_line() +
  facet_wrap(~City, scales="free_y")+
  labs(
    title = "CO Trends Across All Cities",
    subtitle = "2015–2020",
    x = NULL,
    y = "CO Levels",
    caption = "Source: city_day"
  ) +
  theme_linedraw()-> plot3

ggsave("CO Trends Across All cities.pdf",
       plot = plot3,
       units = "in",
       width = 10,
       height = 6)

#3. Air quality trends for bengaluru , Chennai , Mumbai , Hyderabad
pol_data %>%
  filter(City %in% c("Bengaluru", "Chennai", "Mumbai", "Hyderabad")) %>%
  group_by(Year, pollutant, City) %>%
  summarise(mean_values = mean(values, na.rm = T)) -> metro_trends

metro_trends

metro_trends %>%
  ggplot(aes(x = Year, y = mean_values, color = City)) +
  geom_line() +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(
    title = "Air Quality Trends: Bengaluru, Chennai, Mumbai, Hyderabad",
    subtitle = "2015–2020",
    x = NULL,
    y = "Pollutant Values",
    caption = "Source: city_day"
  ) +
  theme_linedraw()->plot4

ggsave("Selected Cities Polutants Trends.pdf",
       plot = plot4,
       units = "in",
       width = 10,
       height = 6)

#4. pm2.5 trend for bengaluru for 2015-2020
pol_data %>%
  filter(City == "Bengaluru", pollutant == "PM2.5") %>%
  group_by(Year) %>%
  summarise(mean_pm25 = mean(values, na.rm = T))

pm25_bangalore

pm25_bangalore %>%
  ggplot(aes(x = Year, y = mean_pm25)) +
  geom_line(color = "darkred") +
  labs(
    title = "PM2.5 Trend in Bengaluru",
    subtitle = "2015–2020",
    x = NULL,
    y = "PM2.5 Levels",
    caption = "Source: city_day"
  ) +
  theme_linedraw()

ggsave("pm_2.5 Polutant Trends in Bengaluru.pdf",
       plot = plot5,
       units = "in",
       width = 10,
       height = 6)
 