library(dplyr)
library(ggplot2)


timeline_data = rio::import(here::here("distributions", 
                            "data",
                            "Timeline.xlsx")) %>%
  janitor::clean_names() %>%
  mutate(project_num = row_number(),
         started = lubridate::as_date(started),
         ended = lubridate::as_date(ended),
         start_date_label = paste0(lubridate::month(started, label = TRUE),
                                   " ",
                                   lubridate::day(started),
                                   ", ",
                                   lubridate::year(started)),
         end_date_label = paste0(lubridate::month(ended, label = TRUE),
                                 " ",
                                 lubridate::day(ended),
                                 ", ",
                                 lubridate::year(ended)),
         project = forcats::fct_reorder(project, started, .desc = FALSE)) %>%
  arrange(project)

# Creating vector for creating angles for labels
max_days = lubridate::ymd("2021-05-01") - lubridate::ymd("2020-11-30")
angles = c(seq(90, 0, length.out = 38),
           seq(360, 270, length.out = 38),
           seq(90, 0, length.out = 38),
           seq(360, 270, length.out = 37))

date_labels = timeline_data %>%
  select(project_num, started, ended) %>%
  mutate(start_day_seq = started - lubridate::ymd("2020-11-30"),
         end_day_seq = ended - lubridate::ymd("2020-11-30"))

date_labels$start_angle = angles[date_labels$start_day_seq]
date_labels$end_angle = angles[date_labels$end_day_seq]

date_labels = date_labels %>%
  select(project_num, start_angle, end_angle)


# joining + manual tweaks for overlapping labels
final_timeline_data = left_join(timeline_data, date_labels, 
                          by = c("project_num" = "project_num")) %>%
  mutate(duplicate_ends = duplicated(end_date_label))

final_timeline_data$end_date_label[final_timeline_data$duplicate_ends == TRUE] = NA
final_timeline_data$end_date_label[final_timeline_data$end_date_label == "Feb 28, 2021"] = NA


# Creating viz
palette = c("#902929", "#eaae64", "#4a4e65")

ggplot(data = final_timeline_data) +
  # Date segment and labels
  geom_segment(aes(x = started + .2, xend = started + .2,
                   y = project_num, yend = 15),
               linetype = "dashed") +
  geom_segment(aes(x = ended - .2, xend = ended - .2,
                   y = project_num, yend = 15),
               linetype = "dashed") +
  geom_text(aes(x = started, y = 19,
                label = start_date_label,
                angle = start_angle),
            family = "Nunito",
            size = 3) +
  geom_text(aes(x = ended, y = 19, 
                label = end_date_label,
                angle = end_angle),
            family = "Nunito",
            size = 3) +
  # Timeline segments
  geom_segment(aes(x = started, xend = ended,
                   y = project_num, yend = project_num,
                   color = project_type),
               size = 2) +
  labs(color = NULL,
       title = "Timeline of Personal Projects",
       subtitle = "All personal projects started, but not always ended,\nwithin the past few months",
       caption = "Viz: @ijeamaka_a  \n") +
  scale_alpha_identity() +
  scale_x_date(limits = c(lubridate::ymd("2020-12-01"), 
                          lubridate::ymd("2021-05-01"))) +
  scale_y_continuous(limits = c(-5, 20)) +
  scale_color_manual(values = palette) +
  coord_polar() +
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = "#f0f0f2",
                                     color = "#f0f0f2"),
    legend.key = element_rect(fill = "#f0f0f2",
                              color = "#f0f0f2"),
    legend.text = element_text(size = 12,
                               family = "Nunito"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#f0f0f2",
                                    color = "#f0f0f2"),
    plot.background = element_rect(fill = "#f0f0f2",
                                   color = "#f0f0f2"),
    plot.title = element_text(family = "Nunito",
                              size = 25,
                              hjust = 0.5,
                              margin = margin(t = 10, b = 5)),
    plot.subtitle = element_text(family = "Nunito",
                                 size = 15,
                                 hjust = 0.5,
                                 lineheight = 0.75),
    plot.caption = element_text(size = 8,
                                family = "Nunito",
                                hjust = 1)
  ) 

ggsave("circular.png", plot = last_plot(), device = "png", 
       path = here::here("distributions", "outputs"), 
       width = 7, height = 9, units = "in")
