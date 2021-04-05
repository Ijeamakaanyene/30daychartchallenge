library(dplyr)
library(ggplot2)
library(ggbump)
library(tidyr)

books = tribble(
  ~books, ~num_pages, ~pages_read,
  "deem: Pedagogy for a New World", 159,       32,
  "Get a Life, Chloe Bown", 255, 255,
  "The Vanishing Half", 285, 71,
  "Better Data Visualizations", 411, 0, 
  "The Honey-Don't List", 308, 308,
  "Parable of the Sower", 329, 221
) %>%
  mutate(perc_label = round(pages_read / num_pages * 100, 0),
         perc_scale = round(pages_read / num_pages * 10, 0)) %>%
  pivot_longer(cols = c(num_pages, pages_read)) %>%
  mutate(x = if_else(name == "num_pages", 0, 10),
         book_label = glue::glue("{books}, {perc_label}%"),
         book_pages_label = if_else(value == 411, 
                                    glue::glue("{value},\npages"),
                                    as.character(value)))

palette = c("#d7ab85", "#AA9486", "#B6854D", "#39312F", "#1C1718", "#79402E")

ggplot() +
  # Slope chart
  geom_bump(data = books,
            aes(x = x, y = value, color = books),
            size = 2) +
  # Bar graph background
  geom_segment(data = books %>% filter(name == "pages_read"),
               aes(x = x + .1, y = value, 
                   yend = value, xend = (x + .1) + 10),
               color = "#cbc5b8",
               size = 2) +
  # Bar graph
  geom_segment(data = books %>% filter(name == "pages_read"),
               aes(x = x + .1, y = value, 
                   yend = value, xend = (x + .1) + perc_scale,
                   color = books),
               size = 2) +
  # Book number 
  geom_text(data = filter(books, name == "num_pages"),
            aes(x = x - .5, y = value, color = books, 
                label = book_pages_label),
            family = "Roboto Condensed",
            lineheight = 0.75,
            hjust = 1) +
  # Book name
  geom_text(data = filter(books, name == "pages_read"),
            aes(x = x, y = value + 12, color = books, 
                label = book_label),
            size = 5,
            family = "Roboto Condensed",
            hjust = 0) +
  scale_color_manual(values = palette) +
  labs(title = "I Am Atrocious at Finishing Books",
       subtitle = "For each book on my bedside table,\nthe size of the book and how much of it I actually read",
       caption = "Viz: @ijeamaka_a | Palette: modified {wesanderson}  \n") +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.9,
                                  size = 30,
                                  family = "Roboto Condensed",
                                  margin = margin(t = 10),
                                  color = "#b05d43"),
        plot.subtitle = element_text(hjust = 0.9,
                                     size = 20,
                                     family = "Roboto Condensed",
                                     color = "black"),
        plot.caption = element_text(family = "Roboto Condensed",
                                    size = 8,
                                    hjust = 1),
        plot.background = element_rect(fill = "#f5f4f3",
                                       color = "#f5f4f3"))


ggsave("slope.png", plot = last_plot(), device = "png", 
       path = here::here("comparisons", "outputs"), 
       width = 12, height = 9, units = "in")


