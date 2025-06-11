library(tidyverse)
library(janitor)
library(lubridate)


"city_day.csv" %>% 
  read_csv() %>% 
  clean_names()-> aqidf

aqidf %>% 
  mutate(year = date %>% year(),
         month = date %>% month(),
         day = date %>% day(),
         week = date %>%  week(),
         weekend = date %>% wday(label = T)) -> aqidf1

colnames(aqidf1)

unique(aqidf1$city)

view(aqidf1)


# Pivot longer: reshape wide pollutant columns into long format
aqidf1 %>%
  pivot_longer(cols = 3:14,  # make sure these are your pollutant columns
               names_to = "pollutants",
               values_to = "values") -> aqidf2
pol_data

pol_data %>% 
  group_by(Year,pollutant) %>% 
  summarise(mean_values = mean(values, na.rm = T)) -> aqi_yearwise

aqi_yearwise 

# Optional: View reshaped data
View(aqidf2)  # Not necessary for code to run

# Year-wise average pollutant trends
aqidf2 %>%
  group_by(year, pollutants) %>%
  summarise(mean_value = mean(values, na.rm = TRUE), .groups = "drop") -> aqi_yearwise

# Optional: View results
print(aqi_yearwise)
## line graph
aqi_yearwise %>%
  ggplot(aes(x = Year, y = mean_values)) +
  geom_line(color = "red") +
  facet_wrap(~pollutant, scales = "free_y") +
  labs(
    title = "Air pollutants trends",
    subtitle = "from 2015 to 2020",
    x = NULL,
    y = "pollutant values",
    caption = "Source: city_day"
  ) +
  theme_linedraw()-> plot1

ggsave("Air Polutants Trends.pdf",
       plot = plot1,
       units = "in",
       width = 10,
       height = 6)

#Heat Map
# aqidf2 %>% 
filter(pollutants=="co") %>% 
  group_by(week,weekend,month) %>% 
  summarise(mean_values= mean(values,na.rm = TRUE)) %>% 
  ggplot(aes(x = week,
             y = weekend,
             fill = mean_values))+
  geom_tile()+
  facet_wrap(~month, scales = "free_x")+
  scale_fill_gradientn(colours = c("darkgreen","yellow","red"))+
  theme_minimal()+
  labs(title = "CO Heat Map",
       subtitle = "For All cities",
       x= NULL,
       Y=NULL)

