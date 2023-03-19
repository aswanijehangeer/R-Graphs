# Installing packages

# install.packages("tidyverse")
# install.packages("gganimate")
# install.packages("gapminder")
# install.packages("ggthemes")


# Loading 
library(tidyverse)
library(gganimate)
library(gapminder)
library(ggthemes)


# Graph - 01: Transition through time

# Life Expectancy vs GDP Capita by Country

graph1 <- gapminder %>% 
  ggplot(aes(x = gdpPercap, y = lifeExp, color = continent, size = pop)) +
  geom_point(alpha = 0.7, stroke = 0) +
  theme_fivethirtyeight() +
  scale_size(range = c(2, 12), guide = "none") +
  scale_x_log10() +
  labs(title = "Life Expectancy vs GDP Capita by Country",
       x = "Income per person (GDP / capita)",
       y = "Life expectancy (years)",
       color = "Continent",
       caption = "Source: Gapminder") +
  theme(axis.title = element_text(),
         text = element_text(family = "Rubik"),
        legend.text = element_text(size = 10))

# Little more adjustments

animated_graph <- graph1 +
  transition_time(year) +
  labs(subtitle = "Year: {frame_time}") +
  shadow_wake(wake_length = 0.1)

animate(animated_graph, height = 500, width = 800,
        fps = 30, duration = 10,
        end_pause = 60, res = 100)

# Save final graph in gif

anim_save("Gapminder Animation.gif")



# Graph - 02: Data appears gradually.

# import data
vd_games_sales <- read_csv("video_games_sales.csv")

# data manipulation

game_sales <- vd_games_sales %>% 
  mutate(Year = as.numeric(Year)) %>% 
  filter(Platform == "PS3",
         Genre %in% c("Action", "Shooter", "Sports", "Racing", "Simulation")) %>% 
  drop_na() %>% 
  group_by(Year, Genre) %>% 
  summarize(Sales = sum(Global_Sales, na.rm = TRUE))
  
# Static graph
graph2 <- game_sales %>% 
  ggplot(aes(x = Year, y = Sales, color = Genre)) +
  geom_line(linewidth = 1.5, alpha = 0.75) +
  theme_fivethirtyeight() +
  labs(title = "PS3 Video Games Sales",
       y = "Global Sales ($ Millions USD)") +
  theme(legend.position = "bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_text(),
        panel.background = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5)) +
  scale_color_brewer(palette = "Set1") +
  geom_point() +
  scale_x_continuous(breaks = 0:2100)

# Animated Graph
animated_graph2 <- graph2 +
  transition_reveal(Year) +
  view_follow(fixed_y = TRUE)

animate(animated_graph2, height = 500, width = 800,
        fps = 30, duration = 10,
        end_pause = 60, res = 100)

# Save final graph in gif

anim_save("PS3 Video Games Animation.gif")


# Graph - 03 Transition through states

# install.packages("ggdist")

library(ggdist)

reviews <- read_csv("googleplaystore.csv") %>% 
  filter(Category %in% c("GAME", "SOCIAL", "PRODUCTIVITY", "EDUCATION", "TOOLS"))


graph3 <- reviews %>% 
  ggplot(aes(x = Category, y = Rating, fill = Category)) + 
  stat_halfeye(adjust = 0.5, justification = -0.2) +
  geom_boxplot(width = 0.2, alpha = 0.2, outlier.colour = NA) +
  theme_fivethirtyeight() + 
  labs(title = "Google Play Store Ratings",
       y = "Rating (out of 5)",
       caption = "Source: Kaggle") +
  theme(legend.position = "none",
        axis.title.y = element_text(),
        text = element_text(family = "Poppins SemiBold"),
        panel.grid.major.x = element_blank(),
        axis.title.x = element_blank()) +
  scale_fill_tableau()

# Animated graph
animated_graph3 <- graph3 +
  transition_states(Category, wrap = FALSE) +
  shadow_mark(alpha = 0.5) +
  enter_grow() +
  exit_fade() +
  ease_aes("back-out")

animate(animated_graph3, height = 500, width = 800,
        fps = 30, duration = 10,
        end_pause = 60, res = 100)

# Save final graph in gif

anim_save("Google Play Store Ratings Animation.gif")
















