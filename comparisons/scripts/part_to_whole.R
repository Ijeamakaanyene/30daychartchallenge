library(dplyr)
library(magick)
library(ggimage)
library(ggplot2)
library(ragg)

# Run once!
image_files = list.files(here::here("comparisons", "data"))

turn_to_grey = c(2, 3)


for(i in seq_along(image_files)){
  
  if(i %in% turn_to_grey){
    images = image_read(paste0("./comparisons/data/", 
                               image_files[i])) %>%
      image_trim() %>%
      image_convert(colorspace = "gray")
    
    image_write(images, 
                path = here::here("comparisons", "data", 
                                  paste0("grey_", image_files[i])),
                format = "png")
  } else {
    images = image_read(paste0("./comparisons/data/", 
                               image_files[i])) %>%
      image_trim()
    
    image_write(images, 
                path = here::here("comparisons", "data", 
                                  image_files[i]),
                format = "png")
  }
}

# Pulling images back in 
list_images = list.files(here::here("comparisons", "data"))[-c(2, 3)]
fruitable_imgs = here::here("comparisons", "data", paste0(list_images))

fruitables = tibble(
  x = seq(1.5, by = 2.5, length.out = 5),
  y = 3.5,
  images = fruitable_imgs
)

# Creating graphic
file = here::here("comparisons", "outputs", "part-to-whole.png")
agg_png(file, width = 10, height = 8, units = "in", res = 144)

ggplot(data = fruitables) +
  geom_rect(aes(xmin = 0, xmax = 13,
                ymin = 2.5, ymax = 4.5),
            fill = "#FCE7FF",
            color = "#FCE7FF") +
  geom_image(aes(x = x, y = y, image = images),
             size = 0.25, asp = 1.26,
             by = "width") +
  geom_rect(aes(xmin = 0, xmax = 13,
                ymin = 1.5, ymax = 2.4),
            fill = "#590B65",
            color = "#590B65") +
  geom_text(aes(x = 6.5, y = 2.1, label = "Fruitables Whole Jerky Bites Purchases"),
            hjust = 0.5,
            color = "white",
            size = 13,
            family = "Nunito SemiBold") +
  geom_text(aes(x = 6.25, y = 1.9, label = "for my dog, Waffles"),
            hjust = 0.5,
            color = "white",
            size = 10,
            family = "Nunito Light") +
  ylim(1.5, 4.5) +
  xlim(0, 13) +
  coord_fixed(clip = "off", expand = FALSE) +
  theme(aspect.ratio = 1/1.26,
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white", color = "white", 
                                       size = 2.5),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank()
        )

invisible(dev.off())

