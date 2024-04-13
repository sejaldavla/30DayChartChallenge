# required packages

library(tidyverse)
library(ggtext)
library(showtext)
library(patchwork)

# data wrangling

## publications
AI_pub <- tibble(publication = c("other", "co"),
                 number = c(1827, 13343))

AI_pub$fraction <- AI_pub$number / sum(AI_pub$number) # calculate fraction (percentage)
AI_pub$ymax <- cumsum(AI_pub$number) 
AI_pub$ymin <- c(0, head(AI_pub$ymax, n = -1))
AI_pub$labelPosition <- (AI_pub$ymax + AI_pub$ymin) / 2
AI_pub$label <- paste0(round(AI_pub$fraction * 100), "%")

## patents
AI_patent <- tibble(patent = c("other", "co"),
                    number = c(34715, 518))


AI_patent$fraction <- AI_patent$number / sum(AI_patent$number) # calculate fraction (percentage)
AI_patent$ymax <- cumsum(AI_patent$number) 
AI_patent$ymin <- c(0, head(AI_patent$ymax, n = -1))
AI_patent$labelPosition <- (AI_patent$ymax + AI_patent$ymin) / 2
AI_patent$label <- paste0(round(AI_patent$fraction * 100), "%")

# fonts
font_add_google("Montserrat", db_cache = FALSE)
showtext_auto()

# colors
col_pal <- c("other" = "#2596be", 
              "co" = "#be2587")

# plot

P1 <- ggplot(AI_pub, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = publication)) +
  geom_rect(color = "#F2F2F2", alpha = 0.7) +
  geom_text(x = 3.5, aes(y = labelPosition, label = label),
            size = 5,
            color = "black",
            family = "Montserrat") +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(1,4)) +
  scale_fill_manual(values = col_pal,
                    name = "",
                    labels = c("co-authors from academia",
                               "authors from Microsoft")) +
  theme_void(base_family = "Montserrat") +
  theme(legend.text = element_text(size = 15),
        legend.position = "bottom",
        legend.margin = margin(-25, 0, 0, 0),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm")) +
  guides(fill = guide_legend(nrow = 2)) +
  annotate(geom = "text",
           label = "Microsoft publications",
           family = "Montserrat",
           x = 1,
           y = 0,
           color = "#727272",
           size = 8)

P2 <- ggplot(AI_patent, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = patent)) +
  geom_rect(color = "#F2F2F2", alpha = 0.7) +
  geom_text(x = 3.5, aes(y = labelPosition, label = label),
            size = 5,
            color = "black",
            family = "Montserrat") +
  coord_polar(theta = "y", clip = "off") +
  xlim(c(1,4)) +
  scale_fill_manual(values = col_pal,
                    name = "",
                    labels = c("co-owned with academia",
                               "owned by Microsoft")) +
  theme_void(base_family = "Montserrat") +
  theme(legend.text = element_text(size = 15),
        legend.position = "bottom",
        legend.margin = margin(-25, 0, 0, 0),
        legend.key.height = unit(0.25, "cm"),
        legend.key.width = unit(0.5, "cm")) +
  guides(fill = guide_legend(nrow = 2)) +
  annotate(geom = "text",
           label = "Microsoft patents",
           family = "Montserrat",
           x = 1,
           y = 0,
           color = "#727272",
           size = 8)

tech_monopoly <- P1   +   P2 +
  plot_annotation(title = "Intellectual Monopoly Capitalism",
                  subtitle = "**Patent monopoly:** Authors analysis based on data from Web of Science and Derwent Innovation shows the majority of Microsoft publications have co-authors from academia; however, 99% of patents generated using knowledge from academic researchers are exclusively owned by Microsoft.",
                  caption = "Source: Cecilia Rikap, NLR 139, 2023 • Graphic: Sejal Davla, PhD",
                  theme = theme(plot.title = element_text(size = 30, family = "Montserrat"),
                                plot.subtitle = element_textbox_simple(size = 15, 
                                                                       family = "Montserrat",
                                                                       lineheight = 0.5,
                                                                       margin = margin(3,0,0,0)),
                                plot.caption = element_text(size = 12,
                                                            family = "Montserrat",
                                                            hjust = 0.5,
                                                            margin = margin(0.2,0,0,0, unit = "cm")),
                                panel.background = element_rect(fill = "#F2F2F2"),
                                plot.background = element_rect(fill = "#F2F2F2"))) 

ggsave("tech_monopoly.png", height = 3, width = 5, dpi = 300)








