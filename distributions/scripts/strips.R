library(dplyr)
library(ggplot2)
library(lubridate)

mpv = rio::import(here::here("distributions", "data", 
                             "MPVDatasetDownload.xlsx")) %>%
  janitor::clean_names() %>%
  filter(year(date_of_incident_month_day_year) == 2021) %>%
  mutate(month = month(date_of_incident_month_day_year, label = TRUE,
                       abbr = FALSE)) %>%
  select(victims_name, month)


mpv_manip = mpv %>%
  group_by(month) %>%
  mutate(y = row_number() * 1.5) %>%
  mutate(x = case_when(
    month == "January" ~ 1,
    month == "February" ~ 2,
    month == "March" ~ 3
  ))



ggplot() +
  geom_text(data = mpv_manip,
            aes(x = x, y = y, label = victims_name),
            size = 4.5,
            hjust = 0,
            family = "Lora",
            color = "white") +
  geom_segment(aes(x = 1, xend = 1.7, y = 0, yend = 0),
               color = "white") +
  geom_text(aes(x = 1, y = -2, label = "JANUARY, 2021"),
            size = 6,
            hjust = 0,
            family = "Lora",
            color = "white") +
  geom_segment(aes(x = 2, xend = 2.7, y = 0, yend = 0),
               color = "white") +
  geom_text(aes(x = 2, y = -2, label = "FEBRUARY, 2021"),
            size = 6,
            hjust = 0,
            family = "Lora",
            color = "white") +
  geom_segment(aes(x = 3, xend = 3.7, y = 0, yend = 0),
               color = "white") +
  geom_text(aes(x = 3, y = -2, label = "MARCH, 2021"),
            size = 6,
            hjust = 0,
            family = "Lora",
            color = "white") +
  xlim(0.5, 4.5) +
  labs(y = NULL, x = NULL,
       caption = "Source: mappingpoliceviolence.org",
       title = "Lives Lost to Police Violence in 2021") +
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    plot.caption = element_text(color = "white",
                                family = "Open Sans"),
    plot.title = element_text(color = "white",
                              size = 35,
                              family = "Lora",
                              hjust = 0.5,
                              margin = margin(t = 10, l = 5, b = 5)),
    panel.background = element_rect(fill = "#232b2b",
                                    color = "#232b2b"),
    plot.background = element_rect(fill = "#232b2b",
                                   color = "#232b2b"),
    panel.grid = element_blank()
  )

ggsave("strips.png", plot = last_plot(), device = "png", 
       path = here::here("distributions", "outputs"), 
       width = 12, height = 20, units = "in")
