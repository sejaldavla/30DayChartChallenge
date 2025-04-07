
# load packages

library(rvest)
library(tidyverse)
library(janitor)
library(ggbump)
library(ggtext)
library(glue)
library(sysfonts)
library(showtext)


# data

biotech <- read_html("https://en.wikipedia.org/wiki/List_of_largest_biomedical_companies_by_revenue")

tables <- biotech |>
  html_table(fill = TRUE)
  
df <- tables[[1]] |>
  clean_names()

df_clean <- df |>
  select(-c(rank, chg, traded_on)) |>
  mutate(across(starts_with("x"), ~ str_remove_all(.,"\\[(\\d+)\\]"))) 

names(df_clean) <- str_remove_all(names(df_clean), "x")

df_rank <- df_clean |>
  pivot_longer(cols = !c(company,country),
               names_to = "year",
               values_to = "revenue") |>
  mutate(year = as.numeric(year),
         revenue = as.numeric(revenue),
         revenue = round(revenue),
         country = str_sub(country, 1,3)) |>
  na.omit() |>
  filter(year %in% c("2019","2020","2021","2022","2023","2024")) |>
  group_by(year) |>
  arrange(year, desc(revenue)) |>
  mutate(rank = row_number(desc(revenue))) |>
  filter(rank <= 10) |>
  distinct() |>
  mutate(color = case_when(company == "Pfizer" ~ '#000485',
                           .default = "#E7E7E7"))

points_data <- df_rank |>
  filter(company == "Pfizer")

label_2019 <- df_rank |>
  filter(year == 2019)

label_2024 <- df_rank |>
  filter(year == 2024)


# fonts
font_add_google("Noto Sans")
showtext_auto()

# text

title_text <- "COVID-19 Pandemic Made Pfizer the Largest Pharma Company in Revenue"

subtitle_text <- "This chart highlights how responding to the pandemic made <span style='color:#000485'>**Pfizer**</span> the top-ranking biomedical company by <span style='color:#000485'>revenue (in billion dollars)</span> in 2022. The 2020 COVID-19 pandemic doubled Pfizer’s earnings in just one year. Within two years, the company recorded $100 billion in revenue, 57% of which came from the <strong><em>COVID-19 vaccine and antiviral Paxlovid</em></strong>."

caption <- "Source: Wikipedia • Graphic: Sejal Davla, PhD"

# plot

label_padding = 0.2

ranking_05 <- df_rank |>
  ggplot(aes(year, rank)) +
  geom_bump(aes(group = company, color = I(color)),
            linewidth = 1.2) +
  geom_point(data = points_data,
             aes(size = revenue, color = I(color)),
             shape = 19) +
  geom_text(data = points_data,
            aes(label = revenue),
            color = "white",
            size = 10) +
  geom_richtext(data = label_2019,
                aes(x = year - label_padding, 
                    y = rank, 
                    label = glue("{company}"," ","**{rank}**")),
                hjust = 1,
                size = 10,
                label.color = NA) +
  geom_richtext(data = label_2024,
                aes(x = year + label_padding, 
                    y = rank, 
                    label = glue("**{rank}**", "   ","{company}")),
                hjust = 0,
                size = 10,
                label.color = NA) +
  scale_x_continuous(limits=c(2018,2025), breaks=2019:2024) +
  scale_y_reverse() +
  scale_size(range = c(5, 10)) +
  labs(x = "",
       y = "",
       size = "",
       title = title_text,
       subtitle = subtitle_text,
       caption = caption) +
  theme_minimal(base_family = "Noto Sans") +
  theme(legend.position = "none",
        axis.text.y = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 28),
        plot.title = element_textbox_simple(color = '#000485',
                                      size = 60, 
                                      hjust = 0.5,
                                      margin = margin(0.5,0,1,0,"cm")),
        plot.subtitle = element_textbox_simple(size = 40,
                                               hjust = 0.5,
                                               lineheight = 0.5,
                                               margin = margin(0,0,0.5,0,"cm")),
        plot.caption = element_text(size = 24,
                                    hjust = 0.5))


ggsave("ranking_05.png", width = 10, height = 8, dpi = 300, bg = "white")

  
  
  
  
  
  
