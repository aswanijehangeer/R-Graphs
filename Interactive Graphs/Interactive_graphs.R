# Installing Packages

# install.packages("tidyverse")
# install.packages("plotly")
# install.packages("extrafont")
# install.packages("ggthemes")

library(tidyverse)
library(plotly)
library(extrafont)
library(ggthemes)
library(lubridate)

# Importing fonts
extrafont::font_import()

# Import data set - from Kaggle
space_data <- read_csv("Space_Corrected.csv") %>% 
  rename(MissionCost = Rocket) %>% 
  mutate(Datetime = as_datetime(as.POSIXct(Datum, format = "%a %b %d, %Y %H:%M UTC")))


# Structure
str(space_data)

# Data Manipulation

space_emissions <- space_data %>% 
  mutate(Location = str_extract(Location, "(USA)|(Russia)")) %>% 
  filter(!is.na(Location)) %>% 
  count(Location, Day = wday(Datetime, label = TRUE), Month = month(Datetime, label = TRUE)) %>% 
  filter(!is.na(Day), !is.na(Month))

# Interactive Graph

space_time_graph <- space_emissions %>% 
  ggplot(aes(x = Day, y = Month, fill = n,
             text = paste0("Missions: ", n))) + 
  geom_tile() +
  facet_wrap(~ Location) +
  theme_tufte() +
  scale_fill_gradient(low = "purple", high = "black") +
  labs(title = "Times of Space Missions",
       caption = "Source: All Space Missions from 1957 - Kaggle") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "DM Sans"),
        legend.position = "none")

# Aesthetics
font <- list(
  family = "DM Sans",
  size = 15,
  color = "white"
)

label <- list(
  bgcolor = "#232F34",
  bordercolor = "transparent",
  font = font
)

# Plotly
space_time_ggplotly <- ggplotly(space_time_graph, tooltip = c("x", "y", "text")) %>% 
  style(hoverlabel = label) %>%
  layout(font = font) %>% 
  config(displayModeBar = FALSE)






# Second Graph

space_mission_cost <- space_data %>% 
  ggplot(aes(x = year(Datetime), 
             y = MissionCost, 
             color = `Status Mission`,
             text = paste0("Location: ", Location,
                           "\nCompany: ", `Company Name`,
                           "\nRocket: ", Detail,
                           "\nTime: ", Datum))) +
  geom_point() +
  theme_minimal() +
  labs(title = "Rocket Launches by Cost",
       x = "Year",
       y = "Missions Cost ($ USD)",
       caption = "Source: All Space Missions from 1957 - Kaggle") +
  theme(plot.title = element_text(hjust = 0.5),
        text = element_text(family = "DM Sans"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())


# ggplotly

ggplotly(space_mission_cost,
         tooltip = c("y", "text")) %>% 
  style(hoverlabel = label) %>%
  layout(font = font) %>%
  config(displayModeBar = FALSE, showTips = FALSE)



















