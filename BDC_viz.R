# Viz for locations of passes / incomplete passes
library(ggforce)

# the coords in the data start at 0 but the coords in the code 

scout2 <- scout %>%
  mutate(x_coordinate = x_coordinate - 100,
         y_coordinate = y_coordinate - 42.5,
         x_coordinate_2 = x_coordinate_2 - 100,
         y_coordinate_2 = y_coordinate_2 - 42.5)

draw_nhl() + 
  geom_point(data = scout2 %>%
           filter(event == "Play" | event == "Incomplete Play"),
         aes(x = x_coordinate_2, y = y_coordinate_2, color = event)) + 
  coord_fixed() +
  scale_discrete_manual("color", values = c("red2", "dodgerblue1")) +
  theme_minimal() +
  labs(color = "Event Type") +
  theme_void() -> pass_outcomes

pass_outcomes

ggsave("pass_outcomes.jpeg", device = "jpeg", pass_outcomes, width = 10, units = "in",
       dpi = 320)

# Viz for shots on goal location

draw_nhl() +
  geom_point(data = scout2 %>%
           filter(event == "Goal"),
         aes(x = x_coordinate, y = y_coordinate, color = event)) + 
  coord_fixed() +
  scale_discrete_manual("color", values = c("dodgerblue1")) +
  theme_minimal() +
  labs(color = "Outcome") +
  theme_void() +
  theme(legend.position = "none") -> shot_outcomes

shot_outcomes

ggsave("shot_outcomes.jpeg", device = "jpeg", shot_outcomes, width = 10, units = "in",
       dpi = 320)

# Data Art for title page - minimalist so no rink needed for this one!

scout %>%
  filter(!is.na(x_coordinate_2) & !is.na(y_coordinate_2)) %>%
  ggplot() +
  geom_segment(aes(x = x_coordinate, y = y_coordinate,
                   xend = x_coordinate_2, yend = y_coordinate_2,
                   color = event)) +
  coord_fixed() +
  theme_minimal() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        line = element_blank()) +
  scale_discrete_manual("color", values = c("red2", "dodgerblue1")) -> title_art

ggsave("title_art.jpeg", device = "jpeg", title_art, width = 10, units = "in")