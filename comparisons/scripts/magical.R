library(dplyr)
library(ggplot2)
library(ragg)
library(systemfonts)


octavia_b = rio::import(here::here("comparisons", "data", 
                                               "octavia_butler.xlsx")) %>%
  janitor::clean_names()

data_source = tibble(year = seq(1976, 2014, by = 1)) %>%
  left_join(., octavia_b, by = c("year" = "year")) %>%
  mutate(duplicate = duplicated(year))

series_labels = data_source %>%
  filter(is.na(work_type) == FALSE) %>%
  select(year, work_type, duplicate) %>%
  group_by(year) %>%
  mutate(id = row_number(), 
         y = 21 - id*2)

year_segments_labels = data_source %>%
  select(year, duplicate, work_type) %>%
  filter(duplicate == FALSE) %>%
  mutate(angle = c(seq(90, 0, length.out = 10),
                   seq(360, 270, length.out = 10),
                   seq(90, 0, length.out = 10),
                   seq(360, 270, length.out = 9)
                   ),
         yend = if_else(year %in% c(1989), 13, 
                        if_else(year %in% c(1980, 1993, 1998, 2000, 2005), 15,
                                if_else(is.na(work_type) == TRUE, 21, 
                                        17))),
         y = 1.5)


file = here::here("comparisons", "outputs", "magical.png")
agg_png(file, width = 9, height = 9.5, units = "in", res = 144)

ggplot() + 
  # Vertical Segments
  geom_segment(data = year_segments_labels,
               aes(x = year, xend = year,
                   y = y, yend = yend),
               color = "#f6d087",
               linetype = "dotted",
               size = .25) +
  # Points
  geom_point(data = series_labels,
             aes(x = year, y = y, 
                 shape = work_type,
                 color = work_type),
             size = 5) +
  # Year labels 
  geom_text(data = year_segments_labels,
            aes(x = year, y = -1, 
                label = year, angle = angle),
            family = "Open Sans",
            color = "#f6d087",
            size = 3) +
  # Middle label
  geom_text(aes(x = 1996, y = -16, label = "Octavia Butler\n"),
            family = "Aleo",
            hjust = 0.5,
            size = 7.9,
            color = "white") +
  geom_text(aes(x = 1996, y = -13, label = "selected works"),
            family = "Aleo",
            hjust = 0.5,
            size = 5.5,
            color = "white") +
  labs(caption = "Viz: @ijeamaka_a | Data: Wikipedia") + 
  scale_shape_manual(values = c(16, 21, 1, 20)) +
  scale_color_manual(values = c("#df9a72", "#ffc12b",
                                "#df9072", "#eea110")) +
  ylim(-16, 25) +
  xlim(1976, 2015) +
  coord_polar() +
  theme(legend.position = "bottom",
        legend.background = element_rect(fill = "#2d2d2d"),
        legend.key = element_rect(fill = "#2d2d2d"),
        legend.text = element_text(color = "white",
                                   family = "Aleo",
                                   size = 11),
        legend.title = element_blank(),
        plot.caption = element_text(family = "Open Sans",
                                    color = "white",
                                    size = 8,
                                    hjust = 1),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "#2d2d2d", 
                                        color = "#2d2d2d"),
        plot.background = element_rect(fill = "#2d2d2d", 
                                        color = "#2d2d2d")) 

invisible(dev.off())

  #"#e5e5e4"
