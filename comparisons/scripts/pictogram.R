library(dplyr)
library(magick)
library(ggimage)
library(ggplot2)
library(ragg)


# Change images
image_happy = list.files("comparisons/data/", 
                         pattern = "^noun_happy")

image_sad = list.files("comparisons/data/", 
                       pattern = "^noun_sad")


image_read(paste0("./comparisons/data/", 
                           image_happy)) %>%
  image_colorize(100, "#828e85") %>%
  image_write(path = here::here("comparisons", "data", 
                                paste0("color_", image_happy)),
              format = "png")

image_read(paste0("./comparisons/data/", 
                  image_sad)) %>%
  image_colorize(100, "black") %>%
  image_write(path = here::here("comparisons", "data", 
                                paste0("color_", image_sad)),
              format = "png")

# Creating calendar data
vertical_segments = tibble(
  x = seq(1, by = 5, length.out = 8),
  xend = x,
  y = 1,
  yend = 26
)

horizontal_segments = tibble(
  x = rep(1, 6),
  xend = 36,
  y = seq(1, by = 5, length.out = 6),
  yend = y
)

days = tibble(
  days = seq(1, 30),
  x = c(c(16, 21, 26, 31),
        rep(seq(1, by = 5, length.out = 7), 4))[1:30],
  x_final = x + .75,
  y = c(rep(26, 4),
        rep(21, 7),
        rep(16, 7),
        rep(11, 7),
        rep(6, 5)),
  y_final = y - .5
)

day_labels = tibble(
  days = c("Monday", "Tuesday", "Wednesday", "Thursday",
           "Friday", "Saturday", "Sunday"),
  x = seq(1, by = 5, length.out = 7) + 2.5,
  y = 27
)


# Reloading images
list_images = list.files("comparisons/data/", 
                         pattern = "color_noun_")
pet_imgs = here::here("comparisons", "data", paste0(list_images))


image_labels = tibble(
  days = 1:8,
  image = ifelse(days < 8, pet_imgs[2], pet_imgs[1]),
  y = c(rep(26, 4),
        rep(21, 4)) - 2.5,
  x = c(16, 21, 26, 31,
        1, 6, 11, 16) + 2.5
)

treats_labels = tibble(
  labels = c(rep("no treats", 7), "TREATS!!!"),
  x = c(16, 21, 26, 31,
        1, 6, 11, 16) + 3,
  y = c(rep(26, 4),
        rep(21, 4)) - 4.5,
  color = c(rep("black", 7), "#828e85")
)

file = here::here("comparisons", "outputs", "pictogram.png")
agg_png(file, width = 10, height = 8, units = "in", res = 144)

ggplot() +
  geom_segment(data = vertical_segments,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_segment(data = horizontal_segments,
               aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = days,
            aes(x = x_final, y = y_final, label = days),
            hjust = 0.5,
            family = "Darker Grotesque",
            fontface = "bold",
            color = "white",
            size = 6) +
  geom_rect(aes(xmin = 1, xmax = 36,
                ymin = 26.5, ymax = 27.5), 
            fill = "#a2605a") +
  geom_text(data = day_labels,
            aes(x = x, y = y, label = days),
            hjust = 0.5,
            fontface = "bold",
            color = "white",
            size = 7,
            family = "Darker Grotesque") +
  geom_image(data = image_labels,
             aes(x = x, y = y, image = image),
             size = .17,
             asp = 1.26,
             by = "width") +
  geom_text(data = treats_labels,
            aes(x = x, y = y, label = labels, 
                color = color),
            hjust = 0.5,
            fontface = "bold",
            size = 5,
            family = "Darker Grotesque") +
  scale_color_identity() +
  coord_equal() +
  theme(
    aspect.ratio = 1/1.26,
    panel.grid = element_blank(),
    plot.background = element_blank(),
    panel.background = element_rect(fill = "#e3d9d3",
                                    color = "#e3d9d3"),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )

invisible(dev.off())

#baccbf green
# #b58b81 reddish
# By Gregor Cresnar 
