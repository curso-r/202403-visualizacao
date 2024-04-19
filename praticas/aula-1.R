# Carregando pacotes -----
library(tidyverse)

# Carregando os dados ----

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# Salvando um backup em .rds
# write_rds(imdb, "dados/imdb.rds")

# writexl::write_xlsx()


# ------------------------------

# Gráfico de barras
# Linhas
# Dispersão
# Histograma
# Boxplot
# Pizza, Circular
# Correlação
# PCA *** Gabrielle e Fabiana
# Heatmap
# Dendograma
# Pirâmide etária por gênero
# Pesquisar: gráfico rosa dos ventos

# Gráfico de dispersão (pontos) -----

imdb |>
  ggplot()

imdb |>
  ggplot() +
  # colocamos os atributos que precisamos buscar elementos na base de dados
  # dentro do aes
  aes(x = orcamento, y = receita)

# não usar notação científica
options(scipen = 999)

imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  # adicionar pontos
  geom_point()


# O que significa?

# Warning message:
# Removed 23758 rows containing missing values (`geom_point()`).



imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_point() +
  geom_hline(yintercept = 1000000000)

# dá para colocar a média?  mediana?

media_receita <- mean(imdb$receita, na.rm = TRUE)

media_receita/1000000 # dividi por 1 milhao

media_receita_milhoes <- round(media_receita/1000000, 1)

texto_receita_milhoes <- paste0("U$", media_receita_milhoes, " milhões")


imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_point() +
  geom_hline(yintercept = media_receita,
             color = "red",
             linetype = 2) +
  geom_text(label = texto_receita_milhoes,
            x = 300000000,
            y = media_receita*1.5,
            color = "red")



# mediana

imdb_sem_na <- imdb |>
  drop_na(orcamento, receita)


imdb |>
  # apenas linhas completas
  drop_na() |>
  View()


mediana_receita <- median(imdb_sem_na$receita, na.rm = TRUE)



# A ordem das camadas de geom importa!
imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_point() +
  geom_hline(yintercept = mediana_receita, color = "blue")

imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_hline(yintercept = mediana_receita, color = "blue") +
  geom_point()

imdb |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_point() +
  geom_text(label = "olá!",
            x = 300000000,
            y = mediana_receita,
            size = 5)


# Bia, se eu quiser selecionar a variavel y,
# no caso receita a partir de determinado valor para plotar, como faz?

filmes_filtrado <- imdb |>
  filter(
    receita > 1000000000,
    !is.na(orcamento)
  )

filmes_filtrado |>
  ggplot() +
  aes(x = orcamento, y = receita) +
  geom_point()



