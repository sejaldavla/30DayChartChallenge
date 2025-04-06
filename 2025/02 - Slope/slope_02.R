
# load packages

library(rvest)
library(tidyverse)
library(janitor)

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
  mutate(player = rep("Gretzsky", 21),
         season = row_number())


df <- df_o |>
  bind_rows(df_g) |>
  clean_names() |>
  select(c(season, player, gp, g, a, pts)) |>
  rename(games = gp,
         goals = g,
         assists = a,
         points = pts)


df |>
  ggplot(aes(x = season, y = goals, shape = player, color = player)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_void()
  
