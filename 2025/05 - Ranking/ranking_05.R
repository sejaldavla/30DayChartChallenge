
# load packages

library(rvest)
library(tidyverse)
library(janitor)
library(ggbump)

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
  group_by(year) |>
  arrange(year, desc(revenue)) |>
  mutate(rank = row_number(desc(revenue))) |>
  filter(rank <= 10) |>
  mutate(rank = rank *(-1)) |>
  distinct()

df_rank |>
  ggplot(aes(factor(year), rank)) +
  geom_point(aes(size = revenue, color = country)) +
  geom_bump(aes(color = company)) 






  
  
  
  
  
  
