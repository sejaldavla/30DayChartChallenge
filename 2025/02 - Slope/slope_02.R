
# load packages

library(rvest)
library(tidyverse)
library(janitor)
library(ggimage)
library(ggtext)
library(sysfonts)
library(showtext)
library(grid)

# data import

ovechkin <- read_html("https://www.espn.com/nhl/player/stats/_/id/3101/alex-ovechkin")

tables <- ovechkin |> 
  html_table(fill = TRUE)

df_o <- tables[[1]] |>
  bind_cols(tables[[2]]) |>
  filter(season != "Career") |>
  mutate(player = rep("Ovechkin", 20),
         season = row_number())

gretzky <- read_html("https://www.espn.com/nhl/player/stats/_/id/4128/wayne-gretzky")

tables <- gretzky |> 
  html_table(fill = TRUE)

df_g <- tables[[1]] |>
  bind_cols(tables[[2]]) |>
  filter(season != "Career") |>
  mutate(player = rep("Gretzky", 21),
         season = row_number())


df <- df_o |>
  bind_rows(df_g) |>
  clean_names() |>
  select(c(season, player, gp, g, a, pts)) |>
  rename(games = gp,
         goals = g,
         assists = a,
         points = pts) |>
  mutate(image = "hockey.png")


# fonts

font_add_google("Bungee Tint", db_cache = FALSE)
font_add_google("Inter", db_cache = FALSE)
showtext_auto()

# colors

col_pal <- c("Gretzky" = "#FF4C00",
             "Ovechkin" = "#041E42")

# text

st_text <- "<span style='color:#041E42'>**Alex Ovechkin**</span>'s goal scoring pace remained consistent with each advancing season <br>compared to <span style='color:#FF4C00'>**Wayne Gretzky**</span> whose performance declined in the later half of his career."
  
caption <- "Source: NHL Stats • Graphic: Sejal Davla, PhD"


slope_02 <- df |>
  mutate(player = factor(player, levels = c("Ovechkin","Gretzky"))) |>
  ggplot(aes(x = season, y = goals, color = player)) +
  geom_image(aes(image = image), size = 0.02, asp = 6/4, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = col_pal) +
  scale_y_continuous(limits = c(0, 100),
                     breaks = seq(0,100,20)) +
  facet_wrap(~player) +
  labs(title = "The Road to 894: Season Performance",
       subtitle = st_text,
       caption = caption) +
  theme_minimal(base_family = "Inter") +
  theme(legend.position = "none",
        plot.title = element_markdown(family = "Bungee Tint",
                                      size = 24,
                                      hjust = 0.5,
                                      margin = margin(1,0,0.5,0,"cm")),
        plot.subtitle = element_textbox_simple(size = 18,
                                               hjust = 0.5,
                                               lineheight = 0.5,
                                               margin = margin(0,0,0.5,0,"cm")),
        plot.caption = element_markdown(size = 12,
                                        hjust = 0.5),
        axis.line = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last"),
                                 color = "grey50"),
        panel.grid.minor = element_blank(),
        strip.text = element_markdown(family = "Bungee Tint",
                                      size = 12),
        panel.spacing.x = unit(3,"lines"))

ggsave("slope_02.png", width = 6, height = 4, dpi = 300, bg="white")





