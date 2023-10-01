# Loading packages

library(tidyverse)
library(plotly)
library(readxl)
library(janitor)
library(sf)


# Data importing
coal_data <- read_xlsx("Indian Coal Mines Data.xlsx") |> 
  clean_names()

# Glimpse
glimpse(coal_data)

# Checking missing values
sum(is.na(coal_data))

# Importing shp file

shp1 <- read_sf("india_shp_files/IND_adm1.shp")

# Changing name of Uttrarachal to Uttarakhand
shp1 <- shp1 |> 
  mutate(NAME_1 = if_else(NAME_1 == 'Uttaranchal',
                          'Uttarakhand',
                          NAME_1))

# Aggregate coal production data by state
coal_data_agg <- coal_data %>%
  group_by(state_ut_name) %>%
  summarize(coal_production_total = sum(coal_lignite_production_mt_2019_2020, 
                                        na.rm = TRUE))

# Merge the Indian map data with the aggregated coal production data based on state names
merged_data <- shp1 %>%
  left_join(coal_data_agg, by = c("NAME_1" = "state_ut_name"))


# Plot
ggplot(merged_data) +
  geom_sf(aes(fill = coal_production_total)) +
  scale_fill_gradient(low = "white", high = "black",
                      na.value = NA,
                      name = "Coal Produced") +
  labs(title = "Coal and Lignite Production (2019-2020)") + 
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "bottom",
    legend.text.align = 0.5,
    legend.key.height = unit(0.1, "in"),
    legend.key.width = unit(0.3, "in"),
    legend.title = element_text(vjust = 1, face = "bold")
  )