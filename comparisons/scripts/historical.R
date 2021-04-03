library(dplyr)
library(ggplot2)
library(ragg)
library(ggstream)
library(ggtext)


private_prisons = rio::import(here::here("comparisons", "data", 
                                         "facilitydatabase.xlsx"))

state_year = private_prisons %>%
  count(state, year) %>%
  filter(is.na(state) == FALSE) %>%
  group_by(state) %>%
  mutate(max_n = max(n)) %>%
  ungroup %>%
  mutate(state = forcats::fct_reorder(state, max_n, .desc = TRUE))


max_states = state_year %>%
  filter(year == 2016) %>%
  arrange(-n) %>%
  mutate(perc = round(n / sum(n) * 100, 0),
         perc_cum_sum = cumsum(perc)) %>%
  slice(1:6)
  

palette = rep("#c0c0c0", 35)

names(palette) = unique(state_year$state)

palette[c(31, 4, 5, 2, 7, 8)] = 
  c("#cd3122", "#f4c623", "#d36328", "#6c905e", "#2f533c", "#b8c9dc")
  

# Creating graphic
file = here::here("comparisons", "outputs", "historical.png")
agg_png(file, width = 12, height = 9, units = "in", res = 144)

ggplot(data = state_year,
       aes(x = year, y = n, fill = state)) +
  geom_stream() +
  scale_fill_manual(values = palette) +
  coord_cartesian(expand = c(0),
                  clip = "off") +
  labs(y = NULL, x = NULL,
       caption = "Viz: @ijeamaka_a | Data: Anna Gunderson, 2020 | Palette inspired by {feather}",
       title = "The Growth of U.S. Private Prisons, 1986 - 2016",
       subtitle = paste0("<br>By 2016, over 60% of private prisons could be found in five states:<br>",
                         "<span style = 'color:#cd3122'>Texas</span>, 
                         <span style = 'color:#f4c623'>California</span>, 
                         <span style = 'color:#d36328'>Colorado</span>, 
                         <span style = 'color:#6c905e'>Arizona</span>, 
                         <span style = 'color:#b8c9dc'>Georgia</span>, and
                         <span style = 'color:#2f533c'>Florida</span>")) +
  theme(legend.position = "none",
        plot.background = element_rect(fill = "#2f2f2f", 
                                        color = "#2f2f2f"),
        plot.caption = element_text(family = "Open Sans",
                                    color = "#f2f2f2",
                                    size = 8,
                                    hjust = 1),
        plot.subtitle = element_markdown(family = "Open Sans",
                                    color = "#f2f2f2",
                                    size = 20,
                                    hjust = 0),
        plot.title = element_text(family = "Libre Franklin", 
                                  color = "#f2f2f2",
                                  size = 35,
                                  margin = margin(t = 10)),
        panel.background = element_rect(fill = "#2f2f2f", 
                                        color = "#2f2f2f"),
        panel.grid = element_blank(),
        
        axis.text.x = element_text(color = "#f2f2f2",
                                   family = "Open Sans",
                                   size = 12),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color = "#f2f2f2"))

invisible(dev.off())



