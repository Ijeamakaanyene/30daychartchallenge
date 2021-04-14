library(dplyr)
library(ggplot2)

tweets = rio::import(here::here("relationships", "data", "geokaramanis_tweets.xlsx")) %>%
  janitor::clean_names()

median_comp = tweets %>%
  filter(challenge == "Comparisons") %>%
  summarise(median_comp = median(likes)) %>%
  pull(median_comp)

median_dist = tweets %>%
  filter(challenge == "Distributions") %>%
  summarise(median_dist = median(likes)) %>%
  pull(median_dist)



ggplot(data = tweets) +
  geom_violin(aes(x = challenge, y = likes),
              color = NA, fill = "#CBC3E3",
              trim = FALSE) +
  geom_point(aes(x = challenge, y = likes), 
                 fill = "orange",
             shape = 21) +
  # comp labels
  geom_segment(aes(y = median_comp, yend = median_comp,
                   x = 0.75, xend = 1.25)) +
  geom_text(aes(x = 0.6, y = median_comp - 20, label = "Median: \n56.5"),
            family = "Fira Sans Bold",
            lineheight = 0.75) +
  geom_curve(aes(x = 0.6, xend = 0.73,
                   y = median_comp - 15, yend = median_comp),
             size = .25,
             curvature = -.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  # dist labels
  geom_segment(aes(y = median_dist, yend = median_dist,
                   x = 1.75, xend = 2.25)) +
  geom_text(aes(x = 1.6, y = median_dist - 20, label = "Median: \n51"),
            family = "Fira Sans Bold",
            lineheight = 0.75) +
  geom_curve(aes(x = 1.6, xend = 1.73,
                 y = median_dist - 15, yend = median_dist),
             size = .25,
             curvature = -.5,
             arrow = arrow(length = unit(0.03, "npc"))) +
  scale_y_continuous(breaks = seq(0, 120, by = 10),
                     sec.axis = dup_axis()) +
  labs(title = "Number of likes by #30DayChartChallenge categories \nfrom @geokaramanis tweets",
       caption = "data: Twitter (specifically @geokaramanis account) |  viz: @ijeamaka_a") +
  theme_minimal(base_family = "Fira Sans Bold") +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(color = "grey56"),
    plot.title = element_text(hjust = 0.5, margin = margin(0, 0, 10, 0), lineheight = 1, size = 16),
    plot.caption = element_text(hjust = 0.5, margin = margin(20, 0, 0, 0), size = 8, color = "grey70"),
    plot.background = element_rect(fill = "grey97", color = NA),
    panel.background = element_rect(fill = "grey97", color = NA),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )


ggsave("correlation.png", plot = last_plot(),
       device = "png", path = here::here("relationships", "outputs"), 
       dpi = 320, width = 7, height = 7)

