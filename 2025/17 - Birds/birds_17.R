# load packages

library(tidyverse)
library(packcircles)
library(ggimage)
library(ggtext)
library(cropcircles)
library(cowplot)
library(sysfonts)
library(ragg)
library(showtext)


# data source: https://www.forestandbird.org.nz/resources/bird-century-winner-announced-puteketeke-pandemonium-prevails

df <- tibble(
  birds = c("Pūteketeke",
            "Kiwi",
            "Kea",
            "Kākāpō",
            "Fantail",
            "Eastern rockhopper penguin",
            "Black robin",
            "Huia",
            "Tūī",
            "Takahē"),
  label = c("Pūteketeke Australasian crested grebe",
            "North Island brown kiwi",
            "Kea",
            "Kākāpō",
            "Pīwakawaka Fantail",
            "Tawaki piki toka Eastern rockhopper penguin",
            "Kakaruia Black robin",
            "Huia",
            "Tūī",
            "Takahē"),
  votes = c(290374,
            12904,
            12060,
            10889,
            7857,
            6763,
            6753,
            6467,
            6457,
            6292),
  images = c("putkeke.png",
             "kiwi.png",
             "kea.png",
             "kakapo.png",
             "fantail.png",
             "penguin.png",
             "robin.png",
             "huia.png",
             "tui.png",
             "takahe.png")
)

# fonts

font_add_google("Unkempt","unkempt", db_cache = FALSE)
font_add_google("Raleway","raleway", db_cache = FALSE)
showtext_auto()

# text

title_text <- "<span style='font-size:40pt'>**Bird of the Century Winner**</span><br><span style='font-size:35pt'>a little nudge from John Oliver</span>"

st_text <- str_wrap("After British-American comedian John Oliver launched his aggressive campaign 
                    supporting Pūteketeke—an Australasian crested grebe—won the Bird of the Century competition with 
                    nearly 300,000 votes in 2023. Forest & Bird, the organization that has been 
                    running annual Bird of the Year competitions since 2005 reported an unprecedented 
                    surge in voting for Pūteketeke from 193 countries. Before that, the competition's 
                    highest-ever vote count was 56,733 in 2021, mostly from New Zealand.", 65)
  
caption <- "Source: Forest & Bird • Graphic: Sejal Davla, PhD"

st2_text <- "All images are sourced from Wikipedia and credits are included in the GitHub code"


# plot

packing <- circleProgressiveLayout(df$votes, sizetype='area') 
data <- cbind(df, packing)
dat.gg <- circleLayoutVertices(packing, npoints=10000)


p1 <- ggplot() + 
  #geom_text(data = data, aes(x, y, size=votes, label = birds)) +
  geom_image(data = data, aes(x, y, image = circle_crop(images)),
             size = ifelse(data$birds %in% c("Kiwi", "Kea"), 0.175,
                           ifelse(data$birds == "Kākāpō", 0.165, 
                                  ifelse(data$birds %in% c("Eastern rockhopper penguin","Black robin"), 0.126,
                                         ifelse(data$birds %in% c("Huia","Tūī","Takahē"), 0.124, 
                                                ifelse(data$birds == "Fantail",0.135, 0.89)))))) +
  geom_polygon(data = dat.gg, aes(x, y, group = id),
               color = "#6fb14a",
               fill = NA,
               linewidth = 1.5,
               position = position_jitter()) +
  scale_size_continuous(range = c(2,5)) +
  coord_equal() +
  labs(title = title_text) +
  theme_void() +
  theme(legend.position="none",
        plot.title = element_textbox(family = "unkempt",
                                     margin = margin(2,0,0,-15,"cm")),
        plot.background = element_rect(fill = "#ccd2ca"),
        plot.margin = margin(0,0,1,18,"cm"))


p2 <- data |>
  mutate(label_new = paste0(label, " - ", scales::comma(votes), " votes"),
         label_new = forcats::fct_reorder(label_new, desc(votes))) |> 
  ggplot(aes(x = votes, y = label)) +
  geom_col(fill = "#6fb14a",
           width = 1.5) +
  coord_cartesian(expand = FALSE) +
  facet_wrap(~label_new, 
             ncol = 1,
             scales = "free_y") +
  theme_void(base_family = "raleway") +
  theme(plot.background = element_rect(fill = "#ccd2ca",
                                       margin(10,10,10,10)),
        strip.text = element_textbox(hjust = 0,
                                     vjust = 0.5,
                                     margin = margin(1, 0, 1, 0),
                                     face = "bold"),
        plot.margin = margin(0.1,0.1,0.1,0.1,"cm"))


birds_17 <- ggdraw() +
  draw_plot(p1) +
  draw_plot(p2, x = 0.05, y = 0.15, width = 0.35, height = 0.25) +
  draw_plot_label(st_text,
                  hjust = 0,
                  x = 0.07,
                  y = 0.87,
                  size = 15,
                  color = "#6fb14a",
                  family = "raleway") +
  draw_plot_label(caption, 
                  x = 0.5, 
                  y = 0.03,
                  hjust = 0.5,
                  size = 12,
                  family = "raleway") +
  draw_plot_label(st2_text,
                  x = 0.5, 
                  y = 0.06,
                  hjust = 0.5,
                  size = 10,
                  family = "raleway")


