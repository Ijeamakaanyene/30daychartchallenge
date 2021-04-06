library(dplyr)
library(ggplot2)
library(tidyr)
library(colorspace)
library(purrr)

tidytuesday = c("#d7d9e3", "#fefdfd", "#e4d2c1", "#eaafa6", "#9da0b0", "#c4a58f", "#ecb025", "#d8c7b3", "#dc354a",
                "#cdcdc0", "#0c1931", "#626d71", "#f4f2f2", "#f2a70c", "#007a4d","#d5d8c2", "#173045",
                "#fce9db", "#3f3e4b", "#52635d", "#817141", "#fdb66e",
                "#a68974", "#8c4c46", "#caccc2", "#fdfcf3", "#2d4059",
                "#008891", "#184d47", "#939b62","#ea5455", "#f07b3f", "#ffd460",
                "#454545", "#b0c5d0", "#613318", "#d57500", 
                "#ae431f", "#668d3c",
                "#4e6172", "#f9f9f9",
                "#052522", "#052522", 
                "#052522", "#385132", 
                "#5f3a34", "#230801",
                "#f9f9f9", "#b84d2b", "#076733","#fcd0ac", 
                "#08705f", "#ffdc33", "#670000", "#dfdacd", 
                "#e1d2c0", "#0f0d0d", "#303030", "#22773f",
                "#bd645e", "#2e4045", "#bfb5b2", "#5e3c58", 
                "#2e4045", "#202736", "#efcd00", "#dbdce1",
                "#dbdce1", "#97bbc7", "#2348a3", "#46467a", "#fbc213",
                "#303030", "#aaaaaa", "#ffcf40", "#999999", "#ffbf00",
                "#eeeeee",
                "#303030", "#B3B3B3", "#fff4e6", "#D9AF6B",
                "#AF6458", "#736F4C", "#526A83", "#625377",
                "#68855C", "#7C7C7C",
                "#f1f1f1")

artistry = c("#75926f", "#8a484a", "#ba6763", "#8d667e", "#39355c",
             "#fffcef", "#1e2539", "#743c52", "#f29544", "#005555",
             "#ededed", "#fbca03", "#1b1626", "#2a2640", "#734656",
             "#a64e46", "#f27d52", "#f29544", "#a64e46", "#f29544",
             "#14232a", "#aaaaaa", "#0c0507", "#c74a00", "#f39200",
             "#2e2137", "#1a2656", "#507992", "#2a534a", "#2d4159",
             "#2a5c4d", "#cbac9a", "#3c5551", "#902018", "#e4cdc3",
             "#f7f7f7", "#6b7f8c", "#8397a5", "#f27d52", "#f2a341",
             "#ffffff", "#040404", "#1c2253", "#c48831", "#013237",
             "#13361c", "#354151", "#e4b453", "#2a2640", "#a64e46", 
             "#f29544", "#ded5c9", "#f97d4e", "#2e2137", "#312f39", 
             "#38394c", "#ededed", "#141419", "#000000", "#8c8c94",
             "#e7e8ec", "#0b0624", "#ab3a3a", "#344b33", "#7f886e",
             "#b78d6a", "#c5ae96",
             "#ffffff", "#ffffff", "#6d696a",
             "#cac9bb", "#aab79f",
             "#13383e", "#f3f0e7", "#eeddcc", "#dbc5a5", "#e7c09f",
             "#cc9a86", "#bd806d", "#895f46",
             "#99582a", "#8c756a", "#50291b", "#090a0e", "#52584d", 
             "#9ab19a", "#d9c1bb",
             "#d0aca3", "#507992", "#ddbca6", "#cea589", "#c17d6b", "#946956",
             "#033649", "#036564", "#cdb380", "#e8ddcb", "#031634",
             "#f1eee9", "#e5e5e4", "#87a3b2", "#000033", "#992c27", "#104a65",  "#f4ba32",
             "#000033", "#992c27", "#104a65", "#f4ba32", "#001b54",
             "#ff9a56", "#ff9a56", "#001b54", "#2a2640", "#f29544", 
             "#000033", "#f4ba32", "#fbcb5a", "#25273c",
             "#fb6d25", "#fca718", "#242424", "#f7f7f7", 
             "#b64132", "#28262f", "#5b0e0e", "#497570", "#f2ddc3",
             "#dd8d78", "#ae625f",
             "#544a57", "#26171c",
             "#86423e", "#f2ddc3", "#b85a5b")


all_shades = tibble(
 hex = c(tidytuesday, artistry),
 source = c(rep("tidytuesday", length(tidytuesday)),
            rep("artistry", length(artistry))))

# shoutout to @jakekaupp for this code!
convert_shades = all_shades %>%
  mutate(rgb = map(hex, hex2RGB),
         hcl = map(rgb, ~as(.x, "polarLUV")),
         hcl = map(hcl, ~as_tibble(attr(.x, "coords")))) %>%
  unnest(c(hcl)) %>%
  mutate() %>%
  arrange(H) %>%
  mutate(hue_group = case_when(
    H >= 0 & H < 30 ~ 0.1,
    H >= 30 & H < 60 ~ 1.1,
    H >= 60 & H < 90 ~ 2.1,
    H >= 90 & H < 120 ~ 3.1,
    H >= 120 & H < 150 ~ 4.1,
    H >= 150 & H < 180 ~ 5.1,
    H >= 180 & H < 210 ~ 6.1,
    H >= 210 & H < 240 ~ 7.1,
    H >= 240 & H < 270 ~ 8.1,
    H >= 270 & H < 300 ~ 9.1,
    H >= 300 & H < 330 ~ 10.1,
    H >= 330 & H <= 360 ~ 11.1
  ))

artistry_shades = convert_shades %>%
  filter(source == "artistry") %>%
  arrange(hue_group, L) %>%
  group_by(hue_group) %>%
  mutate(y = row_number())

tidytuesday_shades = convert_shades %>%
  filter(source == "tidytuesday") %>%
  arrange(hue_group, L) %>%
  group_by(hue_group) %>%
  mutate(y = row_number()) %>%
  ungroup() %>%
  mutate(hue_group = -hue_group)


# FIXED
outline_vert = tibble(
  x = seq(-12, 12.0, by = 1),
  xend = x,
  y = 0, 
  yend = max(artistry_shades$y) + 1
)

outline_horz = tibble(
  x = seq(-11.9, 11.9, by = 1),
  xend = x + .8,
  y = max(artistry_shades$y) + 1,
  yend = max(artistry_shades$y) + 1,
  label = c(seq(360, 120, by = -30),
            paste0("0", seq(90, 30, by = -30)),
            paste0("0", seq(30, 90, by = 30)),
            seq(120, 360, by = 30)),
    #seq(12, 1, by = -1),
            #seq(1, 12, by = 1)),
  label_x = x + .4,
  angle = seq(360, 0, length.out = 24)
)



inner_vert = tibble(
  x = c(-12, 0),
  xend = x,
  y = -10,
  yend = 0
)

systemfonts::register_variant(name = "Aleo Regular", family = "Aleo", weight = "normal")
#temp = systemfonts::font_info(family = "Aleo")

ggplot() +
  # Right shades
  geom_segment(data = artistry_shades,
               aes(x = hue_group, xend = hue_group + 0.8,
                   y = y, yend = y, color = hex),
               size = 2) +
  # Left shades
  geom_segment(data = tidytuesday_shades,
               aes(x = hue_group, xend = hue_group - 0.8,
                   y = y, yend = y, color = hex),
               size = 2) +
  # Top Labels
  geom_segment(data = outline_horz,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "#272726",
               size = 0.25) +
  geom_text(data = outline_horz,
            aes(x = label_x, y = y + 1.75, label = label, angle = angle),
            family = "Cutive",
            color = "#272726") +
  # Outline
  geom_segment(data = outline_vert,
               aes(x = x, y = y, 
                   xend = xend, yend = yend),
               color = "#272726",
               size = 0.25) +
  geom_segment(data = outline_horz,
               aes(x = x, xend = xend,
                   y = 0, yend = 0),
               color = "#272726",
               size = 0.25) +
  geom_segment(data = inner_vert,
               aes(x = x, xend = xend,
                   y = y, yend = yend),
               color = "#272726",
               size = 0.25) +
  annotate("text", x = -5, y = -8.25, label = "tidytuesday", 
           lineheight = 0.75,
           family = "Cutive",
           color = "#272726",
           angle = 270) +
  annotate("text", x = 5, y = -8.25, label = "rtistry", 
           lineheight = 0.75,
           family = "Cutive",
           color = "#272726",
           angle = 90) +
  # The extras
  labs(title = "Colors Featured: #rtistry vs. #TidyTuesday",
       subtitle = "The hex codes used in all pieces are grouped by \nsimilar hue and arranged with increasing lightness",
       caption = "Viz: @ijeamaka_a  \n") +
  scale_color_identity() +
  xlim(-12, 12) +
  ylim(-10, NA) +
  coord_polar(clip = "off") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#f2f2f2",
                                        color = "#f2f2f2"),
        plot.background = element_rect(fill = "#f2f2f2",
                                        color = "#f2f2f2"),
        plot.title = element_text(hjust = 0.5,
                                  family = "Aleo",
                                  size = 25),
        plot.subtitle = element_text(hjust = 0.5,
                                  family = "Aleo",
                                  size = 15),
        plot.caption = element_text(size = 8,
                                    family = "Aleo",
                                    hjust = 1))


ggsave("experimental.png", plot = last_plot(), device = "png", 
       path = here::here("comparisons", "outputs"), 
       width = 7.8, height = 9, units = "in")


