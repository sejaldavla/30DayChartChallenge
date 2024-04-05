# required packages

library(tidyverse)
library(ggtext)
library(showtext)
library(janitor)
library(here)

# data import and cleaning

df <- read.csv(file = "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv", skip = 1, header = TRUE, sep = ",")

write.csv(df, "global_temp.csv")

mean_temp <- df |>
  select(Year, J.D) |>
  clean_names() |>
  rename("temp" = "j_d") |>
  mutate(temp = as.numeric(temp)) |>
  na.omit()

write.csv(mean_temp,"mean_temp.csv")

# text

font_add_google(name = "Open Sans", family = "opensans")
font_add_google(name = "Cabin Sketch", family = "Cabin Sketch")
showtext_auto()

title_text <- "Is the planet getting hotter?"
subtitle_text <- "<span style='font-size:40pt; color:red'>**2023 WAS THE HOTTEST YEAR**</span> with a 1.17 °C change in average global land and surface temperature."
caption_text <- "Source: GISS Surface Temperature Analysis (GISTEMPv4), NASA • Graphic: Sejal Davla, PhD"

# plot

global_temperature <- mean_temp |>
  ggplot(aes(x = year, y = temp, fill = temp)) +
  geom_col(width = 0.5) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       name = "Global Temperature (°C)",
                       limits = c(-0.5,1.2)) +
  scale_x_continuous(breaks = seq(1880, 2023, 20)) +
  scale_y_continuous(limits = c(-0.48, 1.3),
                     breaks = seq(-0.48, 1.3, 0.5))+
  coord_cartesian(expand = FALSE) +
  labs(x = NULL,
       y = "",
       title = title_text,
       subtitle = subtitle_text,
       caption = caption_text) +
  theme_classic(base_family = "sans", 
                base_size = 12) +
  theme(plot.title = element_text(family = "Cabin Sketch", 
                                  size = 80, 
                                  color = "white",
                                  vjust = 0.05,
                                  margin = margin(0,0,0.6,0, "cm")),
        plot.title.position = "plot",
        plot.subtitle = element_textbox_simple(family = "opensans",
                                         size = 33,
                                         color = "white",
                                         lineheight = 0.5),
        plot.caption = element_text(size = 20,
                                    color = "#e1e1e1",
                                    hjust = 0.5,
                                    vjust = -15,
                                    lineheight = 0.4),
        panel.background = element_rect(fill = "#212529"),
        plot.background = element_rect(fill = "#212529"),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(color = "white", 
                                   size = 30,
                                   vjust = -1.5),
        axis.text.y = element_blank(),
        legend.direction = "horizontal",
        legend.position = "None",
        legend.background = element_rect(fill = "#252321"),
        legend.text = element_text(color = "white", size = 40),
        legend.title = element_text(color = "white", size = 40),
        legend.text.align = 0.5,
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(0.2, "cm"),
        plot.margin = margin(0, 0.5, 1.5, 0.5, unit = "cm")
  ) + 
  geom_hline(yintercept = -0.48, 
             linetype = 3, 
             color = "#9567E0", 
             alpha = 0.7, 
             linewidth = 0.75) +
  annotate(geom = "text", 
           x = 1950, 
           y = -0.45, 
           label = "-0.48 °C", 
           size = 10, 
           color = "#f6f6f6") +
  geom_hline(yintercept = 0.5, 
             linetype = 3, 
             color = "#f2a62c", 
             alpha = 0.7, 
             linewidth = 0.75) +
  annotate(geom = "text", 
           x = 1950, 
           y = 0.53, 
           label = "0.50 °C", 
           size = 10, 
           color = "#f6f6f6") +
  geom_hline(yintercept = 1.17, 
             linetype = 3, 
             color = "#f21d0a", 
             alpha = 0.7, 
             linewidth = 0.75) +
  annotate(geom = "text", 
           x = 1950, 
           y = 1.20, 
           label = "1.17 °C", 
           size = 10, 
           color = "#f6f6f6")

ggsave("global_temperature.png", width = 6.5, height = 8, units = "in", dpi = 300)

