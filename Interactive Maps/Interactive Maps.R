# install.packages("tidyverse")
# install.packages("plotly")



library(tidyverse)
library(plotly)


# Import data set - Kaggle

states_code <- read_csv("states_codes.csv")

min_wage <- read_csv("Minimum Wage Data.csv") %>% 
  select(Year, state = State, Wage = State.Minimum.Wage)

wage_df <- merge(min_wage, states_code) %>% 
  select(Year, state, Wage, code) %>% 
  mutate(hover = paste0(state, "\n$", Wage))

# Structure
str(wages)


# Aesthetics

fontStyle <- list(
  family = "DM Sans",
  size = 15,
  color = "black"
)

label <- list(
  bgcolor = "#EEEEEE",
  bordercolor = "transparent",
  font = fontStyle
)




# Plot

wage_df %>% 
  plot_geo(locationmode = "USA-states",
           frame = ~Year) %>% 
  add_trace(locations = ~code,
          z = ~Wage,
          zmin = 0,
          zmax = max(wage_df$Wage),
          color = ~Wage,
          colorscale = "Electric",
          text = ~hover,
          hoverinfo = 'text') %>% 
  layout(geo = list(scope = "usa"),
         font = list(family = "DM Sans"),
         title = "Minimum Wage in US\n1968 - 2017") %>% 
  style(hoverlabel = label) %>% 
  config(displayModeBar = FALSE) %>% 
  colorbar(tickprefix = "$")
























































