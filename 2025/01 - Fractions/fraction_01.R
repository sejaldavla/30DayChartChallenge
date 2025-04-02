# load packages

library(tidyverse)
library(showtext)
library(ggwaffle)
library(fontawesome)
library(emojifont)
library(extrafont)
library(ggtext)

# data from: https://abacusdata.ca/2025-federal-election-poll-liberals-conservatives-tied/

df <- tibble(party = c(rep("Liberal", 55),rep("other", 45), rep("Conservative", 52), rep("other", 48)),
             topic = c(rep("55% will vote <span style='color:#D71920'>**Liberal**</span> on the issue of<br><span style='font-size:35pt'>**Dealing with Trump**</span>", 100), rep("52% will vote <span style='color:#1A4782'>**Conservative**</span> on the issue of<br><span style='font-size:35pt'>**Growing the economy**</span>",100)))

waffle_data <- expand.grid(x = rep(1:10), y = rep(1:10), topic = unique(df$topic)) |>
  mutate(party = df$party,
         label = fontawesome('fa-user'))

# fonts

font_add_google("Outfit","Outfit")
showtext_auto()

# colors

col_palette <- c("Liberal" = "#D71920",
                 "Conservative" = "#1A4782",
                 "other" = "grey90")

# text

title_text <- "What's on the ballot, Canada?"

caption <- "Source: Abacus Data • Graphic: Sejal Davla, PhD"

# plot

fraction_01 <- waffle_data |>
  ggplot(aes(x, y, fill = party)) +
  geom_text(aes(label = label,
                color = party), family='fontawesome-webfont', size = 8) +
  scale_color_manual(values = col_palette) +
  facet_wrap(~topic) +
  scale_y_reverse() +
  coord_equal() +
  labs(title = title_text,
       caption = caption) +
  theme_void(base_family = "outfit") +
  theme(legend.position = "none",
        panel.spacing = unit(2,"lines"),
        plot.title = element_markdown(color = "gray80",
                                      size = 50,
                                      face = "bold",
                                      hjust = 0.5),
        plot.caption = element_text(color = "gray80",
                                    size = 18,
                                    hjust = 0.5,
                                    vjust = -5),
        strip.text = element_markdown(size = 22,
                                      color = "gray80",
                                      lineheight = 0.75,
                                      hjust = 0.5,
                                      margin = margin(0.5,0,0.5,0, "cm")))

ggsave("fraction_01.png", width = 4, height = 4, dpi = 300, bg = "black")
