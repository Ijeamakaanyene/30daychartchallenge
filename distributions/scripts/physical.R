library(dplyr)
library(ggplot2)
library(tidyr)
library(ggstream)

palette = c("#d7d7d7", "#b3bebf", "#96aaa9",
            "#59807c", "#12423f")

length = 100

waves = tibble(
  x = 1:length,
  color = sample(LETTERS[1:5], size = length,
                 replace = TRUE)
)

waves_types = waves %>%
  mutate(y_beta = rbeta(length, shape1 = 0.5, shape2 = 0.5),
         y_norm = rnorm(length, mean = 0, sd = 2),
         y_pois = rpois(length, 2))

pivot_waves = waves_types %>%
  pivot_longer(cols = c(y_beta, y_norm, y_pois)) %>%
  mutate(labels = case_when(
    name == "y_beta" ~ "Beta Distribution",
    name == "y_norm" ~ "Normal Distribution",
    name == "y_pois" ~ "Poisson Distribution"
  ))

ggplot(data = pivot_waves) +
  geom_stream(aes(x = x, y = value, color = color, fill = color),
              type = "proportional") +
  labs(caption = "Viz: @ijeamaka_a",
       title = "The Hills are Alive\nwith Probability Distributions") +
  scale_color_manual(values = palette) +
  scale_fill_manual(values = palette) +
  coord_cartesian(expand = c(0)) +
  facet_wrap(~labels, 
             ncol = 1,
             strip.position = "bottom") +
  theme(legend.position = "none",
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        plot.caption = element_text(color = "#d7d7d7",
                                    family = "Cutive",
                                    size = 8),
        plot.title = element_text(color = "#d7d7d7",
                                  hjust = 0.5,
                                  margin = margin(t = 10, b = 10),
                                  family = "Cutive",
                                  size = 20),
        strip.text = element_text(hjust = 1,
                                  family = "Cutive"),
        strip.background = element_rect(fill = "#d7d7d7", 
                                        color = "#d7d7d7"),
        panel.background = element_rect(color = "#d7d7d7"),
        plot.background = element_rect(fill = "#0c2e2c",
                                        color = "#0c2e2c"))

ggsave("physical.png", plot = last_plot(), device = "png", 
       path = here::here("distributions", "outputs"), 
       width = 6, height = 9, units = "in")
