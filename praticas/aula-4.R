# Carregando pacotes -----
library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter)
library(dados)

# Carregando os dados ----

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# Dúvida Luzi --- Gráfico de Pizza / Gráfico de setores

imdb_nota_categorizada <- imdb |>
  mutate(nota_cat = case_when(
    nota_imdb >= 8 ~ "Ótimo",
    nota_imdb >= 5 ~ "Médio",
    nota_imdb < 5 ~ "Ruim"
  ),
  .after = nota_imdb)


imdb_nota_categorizada |>
  count(nota_cat) |>
  mutate(porc = n/sum(n),
         porc_label = scales::percent(porc)) |>
  ggplot() +
  # Atenção com o valor do aes Y aqui!
  aes(y = porc, fill = nota_cat, x = "") +
  geom_bar(stat = "identity", width = 1) +

  coord_polar(theta = "y") +
  theme_void() +
  # Salva pela Karina
  geom_label(aes(label = porc_label),
            position = position_stack(vjust = 0.5),
            colour = "white")

# Expansões do ggplot2 -------------------------------
# patchwork ----
# ggrepel ---
# gghighlight ---

# Patchwork ------------------------------------

g1 <- dados_starwars |>
  ggplot() +
  aes(y = massa, x = altura) +
  geom_point()


g2 <- dados_starwars |>
  ggplot() +
  aes(x = massa) +
  geom_histogram()

g3 <- dados_starwars |>
  ggplot() +
  aes(x = altura) +
  geom_histogram()

g4 <- dados_starwars |>
  count(genero) |>
  ggplot() +
  aes(x = genero, y = n) +
  geom_col()

g1 + g2

library(patchwork)

g1 + g2

g1 + g2 + g3 + g4

g1 / g2


# plot_layout permite que a gente defina o numero de colunas
# ou numero de linhas
g1 + g2 + g3 + g4 + plot_layout(ncol = 2)

g1 + g2 + g3 + g4 + plot_layout(nrow = 4)

# composições diferentes

(g3 + g2) / g1

# anotação
grafico_com_anotacao <- (g3 + g2) / g1 + plot_annotation(
  title = "Qual é a relação entre a massa e a altura?",
  subtitle = "Pensonagens de StarWars",
  # tag_levels = "1"
    # 'a', 'A', '1', ⁠'i⁠, or 'I',
  tag_levels = "a",
  tag_prefix = "Fig 2." ,
  tag_suffix = ")"
)


grafico_com_anotacao & theme_light()

# gghighlight ------------------

dados_starwars |>
  ggplot() +
  aes(y = massa, x = altura) +
  geom_point() +
  gghighlight::gghighlight(massa > 1000)




dados_starwars |>
  ggplot() +
  aes(y = massa, x = altura) +
  geom_point() +
  gghighlight::gghighlight(especie == "Droide")


# útil para destacar outliers

# Exercício com os dados do pacote mananciais

library(readxl)
url <- "https://github.com/beatrizmilz/mananciais/blob/master/inst/extdata/mananciais.xlsx?raw=true"
destfile <- "dados/mananciais.xlsx"
curl::curl_download(url, destfile)
mananciais <- read_excel(destfile)
View(mananciais)


mananciais |>
  ggplot() +
  aes(x = data, y = volume_porcentagem) +
  geom_line(aes(color = sistema))

mananciais |>
  ggplot() +
  aes(x = data, y = volume_porcentagem) +
  geom_hline(yintercept = 0) +
  geom_line(aes(color = sistema)) +
  gghighlight::gghighlight(sistema == "Cantareira")


mananciais |>
  ggplot() +
  aes(x = data, y = volume_porcentagem) +
  geom_hline(yintercept = 0) +
  geom_line(aes(color = sistema)) +
  gghighlight::gghighlight(volume_porcentagem <= 0)


# Bia, pra mim não é mt "instintivo" quando usar o = e quando usar o ==
# pq em sistema vai == e no color  = ?

mananciais |>
  ggplot() +
  aes(x = data, y = volume_porcentagem) +
  geom_hline(yintercept = 0) +
  geom_line(aes(color = sistema)) +
  gghighlight::gghighlight(sistema == "Cantareira")

# == é uma comparação lógica

# filter(sistema == "Cantareira")


# Ana Paula Rocha
# 20:28
# Poderia calcular uma média móvel para o y?
# TO DO!

# Gabrielle Lombardi
# 20:28
# como faz para colorir uma faixa mesmo?
# tipo deixar o fundo cinza escuro, entre 0 e 100

# NÃO DEU CERTO.
# mananciais |>
#   ggplot() +
#   aes(x = data, y = volume_porcentagem) +
#   geom_hline(yintercept = 0) +
#   geom_line(aes(color = sistema)) +
#   geom_rect(aes(
#     xmin = -Inf,
#     xmax = Inf,
#     ymin = 20,
#     ymax = 50
#   ),
#   fill = "red") +
#   theme_minimal()
#  # gghighlight::gghighlight(volume_porcentagem <= 0) +



# como faz para colorir uma faixa mesmo?
# tipo deixar o fundo cinza escuro, entre 0 e 100
dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_rect(
    xmin = 0,
    xmax = 200,
    ymin = -Inf,
    ymax = Inf,
    fill = "lightgray"
  ) +
  geom_point() +
  theme_minimal()

# Intervalo ---

library(ggrepel)


dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_label(aes(label = nome))


dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_label_repel(aes(label = nome))

# Warning message:
# ggrepel: 27 unlabeled data points (too many overlaps).
# Consider increasing max.overlaps

# Dá para forçar colocar as labels, mas não fica bom!
dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_label_repel(aes(label = nome), max.overlaps = 100)


# é possível passar um vetor com os nomes que vc quer? ou algo assim?


starwars_newhope <- dados_starwars |>
    filter(massa < 1000) |>
  filter(str_detect(filmes, "A New Hope"))

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_label_repel(aes(label = nome), data = starwars_newhope)

dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_point(
    aes(color = nome),
    data = starwars_newhope,
    show.legend = FALSE
  ) +
  geom_label_repel(
    aes(label = nome, color = nome),
    data = starwars_newhope,
    box.padding = 2,
    max.overlaps = 100,
    show.legend = FALSE
  )


dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_point(
    aes(color = nome),
    data = starwars_newhope,
    show.legend = FALSE
  ) +
  # geom_label_repel(
  #   aes(label = nome, color = nome),
  #   data = starwars_newhope,
  #   box.padding = 2,
  #   max.overlaps = 100,
  #   show.legend = FALSE
  # )+
  gghighlight::gghighlight(str_detect(filmes, "A New Hope"))


dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  gghighlight::gghighlight(str_detect(filmes, "A New Hope"))



dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  geom_point(
    aes(color = nome),
    data = starwars_newhope,
    show.legend = FALSE
  ) +
  geom_label_repel(
    aes(label = nome, color = nome),
    data = starwars_newhope,
    box.padding = 2,
    max.overlaps = 100,
    show.legend = FALSE,
    size = 5
  )

## ggridges ----------------------------------

library(ggridges)

diamante |>
  ggplot() +
  aes(x = preco, y = corte, fill = corte) +
  geom_boxplot()

diamante |>
  ggplot() +
  aes(x = preco, y = corte, fill = corte) +
  geom_density_ridges(
    show.legend = FALSE,
    alpha = 0.8,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  theme_ridges()


media_geral <- mean(diamante$preco)

diamante |>
  ggplot() +
  aes(x = preco, y = corte, fill = corte) +
  geom_density_ridges(
    show.legend = FALSE,
    alpha = 0.8,
    quantile_lines = TRUE,
    quantiles = 2
  ) +
  geom_vline(xintercept = media_geral,
             linetype = 2,
             color = "gray"
             ) +
  theme_ridges() +
  labs(
    x = "Preço",
    y = "Corte",
    title = "Distribuição dos preços de diamantes segundo o corte",
    caption = "Fonte: Dados da base diamonds"
  )


# Extensão: hrbrthemes

library(hrbrthemes)
install.packages("hrbrthemes")




dados_starwars |>
  filter(massa < 1000) |>
  ggplot() +
  aes(x = massa, y = altura, color = sexo_biologico) +
  geom_point() +
  hrbrthemes::theme_ipsum() +
  hrbrthemes::scale_color_ipsum()


# Extensão: ggalt

jabba <- filter(dados_starwars,
                massa > 1000)

dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  ggalt::geom_encircle(
    data =
      jabba,
    s_shape = 0,
    expand = 0.001,
    color = "red"
  ) +
  theme_ridges()


dados_starwars |>
  ggplot() +
  aes(x = massa, y = altura) +
  geom_point() +
  ggalt::geom_encircle(
    data =
      jabba,
    s_shape = 0,
    expand = 0.001,
    color = "red"
  ) +
  theme_ridges() +
  annotate(geom = "text",
           x = jabba$massa,
           y = jabba$altura*1.18,
           label = jabba$nome,
           color = "red")


# s_shape=0.5, expand=0.1
