# Como fazer alguns gráficos diferentes com ggplot2?

library(tidyverse)


imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")


# 1. Gráfico de setores (pizza)

# Categorizar por nota:
imdb_nota_categorizada <- imdb |>
  mutate(nota_cat = case_when(
    nota_imdb > 9 ~ "4 - Ótimo",
    nota_imdb > 7 ~ "3 - Médio",
    nota_imdb > 5 ~ "2 - Regular",
    TRUE ~ "1 - Ruim"
  ))

imdb_nota_categorizada |>
  count(nota_cat) |>
  mutate(nota_cat_porc = (n / sum(n))* 100) |>
  ggplot() +
  aes(x = "", y = nota_cat_porc, fill = nota_cat) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  theme_void()

