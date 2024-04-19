# Carregando pacotes -----
library(conflicted)
library(tidyverse)
conflicts_prefer(dplyr::filter)

# Carregando os dados ----

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")



# Boxplot --------------------------------
imdb |>
  ggplot() +
  geom_boxplot(aes(x = nota_imdb))

# Para a próxima aula:
# Vamos fazer um exemplo pensando em algum grupo da base.
# Os gêneros mais frequentes

# Criando um boxplot dos gêneros dos filmes
# por gênero, mas para os gêneros mais frequentes


# deixando a base no formato longo, apenas 1 gênero por linha
imdb_generos <- imdb |>
  separate_longer_delim(
    cols = generos, delim = ", ")

# quais são os gêneros mais frequentes?
generos_mais_frequentes <- imdb_generos |>
  count(generos, sort = TRUE) |>
  slice_head(n = 5)

# filtrando a base longa para apenas os gêneros mais frequentes
imdb_generos_mais_frequentes <- imdb_generos |>
  filter(generos %in% generos_mais_frequentes$generos)

# Criando o boxplot
imdb_generos_mais_frequentes |>
  ggplot() +
  geom_boxplot(aes(x = generos, y = nota_imdb))

# Está em ordem alfabética!

# Ordenar pela mediana

imdb_generos_mais_frequentes |>
    mutate(generos = fct_reorder(generos, nota_imdb, .fun = median)) |>
  ggplot() +
  geom_boxplot(aes(x = generos, y = nota_imdb))

# Dúvida: o que o .fun quer dizer?

# O .fun do fct_reoder informa para a função qual é a sumarização
# que deve ser feita para descobrir a ordem dos valores.
# Internamente, é similar à:
imdb_generos_mais_frequentes |>
  group_by(generos) |>
  summarise(
    mediana = median(nota_imdb)
  ) |>
  arrange(mediana)

# Repare que a ordem dos gêneros é igual à
# ordem apresentada no último boxplot! :)



# DÚVIDA HELDER: Como fazer algo similar, mas em vez da mediana,
# usar a quantidade de linhas? (n)

# Encontrei a resposta aqui:
# https://r-graph-gallery.com/267-reorder-a-variable-in-ggplot2.html

imdb_generos_mais_frequentes |>
  mutate(generos = fct_reorder(generos, nota_imdb, .fun = "length")) |>
  ggplot() +
  geom_boxplot(aes(y = generos, x = nota_imdb))


# Como fazer a "prova real"?
# colocando o n junto ao nome da categoria, ajuda a gente a ver que deu certo!
imdb_generos_mais_frequentes |>
  group_by(generos) |>
  mutate(numero_filmes = dplyr::n()) |>
  ungroup() |>
  mutate(generos = paste0(generos, " - n = ", numero_filmes)) |>
  mutate(generos = fct_reorder(generos, nota_imdb, .fun = "length")) |>
  ggplot() +
  geom_boxplot(aes(y = generos, x = nota_imdb))


# Dúvida:
# Poderia inserir um ponto com a media? (destacado em vermelho)

# Sim, mas é necessário fazer em duas etapas:

# Primeiro calcular a média
media_por_genero <- imdb_generos_mais_frequentes |>
  group_by(generos) |>
  summarise(media = mean(nota_imdb))

# depois usamos essa média calculada no gráfico:

imdb_generos_mais_frequentes |>
  mutate(generos = fct_reorder(generos, nota_imdb, .fun = median)) |>
  ggplot() +
  geom_boxplot(aes(x = generos, y = nota_imdb)) +
  # o trecho abaixo adiciona o ponto da média calculada:
  geom_point(aes(x = generos, y = media),
             color = "red",
             data = media_por_genero)


# DÚVIDA:
# Pq uso :: ?
# - Conflito
# - me ajuda a buscar
# - no trabalho usamos pacote

imdb |>
# Exemplo de quando algum texto nos eixos
  # é muito grande!!
   mutate(titulo = stringr::str_wrap(titulo, 20)) |>
  slice_sample(n = 10) |>
  ggplot() +
  geom_col(aes(x = nota_imdb, y = titulo))


# DÚVIDA
# É possível fazer esse grafico com dois fatores,
# por exemplo, genero e ano?

imdb_generos_mais_frequentes |>
  filter(ano %in% c(2017, 2018, 2019, 2020)) |>
  ggplot() +
  aes(x = ano, y = nota_imdb) +
  geom_boxplot(aes(group = ano, fill = generos), show.legend = FALSE) +
  scale_x_continuous(breaks = c(2017, 2018, 2019, 2020)) +
  facet_wrap(~generos)

# -----

# Dúvida: como fazer algo similar à:
# https://r-graph-gallery.com/265-grouped-boxplot-with-ggplot2.html ?

imdb_generos_mais_frequentes |>
  filter(ano %in% c(2019, 2020),
         generos %in% c("Drama", "Comedy")) |>
  mutate(ano = as.character(ano)) |>
  ggplot() +
  geom_boxplot(aes(x = ano, y = nota_imdb, fill = generos))


# titulos e labels ------------------------------

imdb |>
  mutate(lucrou = receita - orcamento > 0) |>
  ggplot() +
  aes(x = orcamento, y = receita, color = lucrou) +
  geom_point() +
  # na função labs() tratamos dos textos apenas!
  labs(
    # sempre funcionam
    title = "Orçamento x Receita",
    subtitle = "Filmes do Estados Unidos",
    caption = "Fonte: Dados do IMDB",
    # depende do aes()
    x = "Orçamento",
    y = "Receita",
    color = "Teve lucro?",
    # fill,
    # size,
    # shape
  )


# Temas -------
# criando um gráfico de exemplo que vamos usar depois
exemplo_grafico <- imdb |>
  mutate(lucrou = receita - orcamento > 0) |>
  ggplot() +
  aes(x = orcamento, y = receita, color = lucrou) +
  geom_point()

# temas do próprio ggplot2 -------------

exemplo_grafico +
  theme_light()

exemplo_grafico +
  theme_minimal()

exemplo_grafico +
  theme_void()

exemplo_grafico +
  theme_bw()

# Pacote esquisse ------------------
# install.packages("esquisse")

library(esquisse)

# abrir o esquisse
esquisse::esquisser()
# ou pelo addins -> ggplot2 builder


imdb |>
  mutate(
    nota_cat = case_when(
      nota_imdb >= 8 ~ "4 - Ótimo",
      nota_imdb >= 6 ~ "3 - Médio",
      nota_imdb >= 4 ~ "2 - Regular",
      TRUE ~ "1 - Ruim"
    )
  ) |>
  filter(ano >= 2010L & ano <= 2020L)  |>
  # Bia, no esquisse, tem como arrumar os valores dos eixos?
  mutate(receita_milhoes = receita / 1000000,
         orcamento_milhoes = orcamento / 1000000) |>
  ggplot() +
  aes(
    x = orcamento_milhoes,
    y = receita_milhoes,
    shape = nota_cat,
    color = nota_cat
  ) +
  geom_point(size = 2.7, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d(begin = 0, end = 0.8) +
  labs(title = "Orçamento vs receita",
       y = "Receita (Milhões U$)",
       x = "Orçamento (Milhões U$)",
       color = "Categoria da nota",
       shape = "Categoria da nota") +
  ggthemes::theme_pander() +
  # theme é do ggplot2
  theme(legend.position = "bottom",
        plot.title = element_text(
         # color = "darkgray"
          color = "#545D64",
          hjust = 0.5

        )) +
  facet_wrap( ~ nota_cat)


# reaproveitar o tema -----
aplicar_tema <- theme_minimal(
  base_family = "Comic Sans MS",
  base_size = 15
) +
    theme(legend.position = "bottom",
          plot.title = element_text(color = "#545D64",
                                    hjust = 0.5))


starwars |>
  slice_max(mass, n = 5) |>
  ggplot() +
  geom_col(aes(x = mass, y = name)) +
  labs(title = "Personagens mais pesados de Starwars") +
  aplicar_tema



aplicar_tema <- theme_minimal(
  # Fonte que você tem instalada
  base_family = "Times New Roman",
  base_size = 15
) +
    theme(legend.position = "bottom",
          plot.title = element_text(color = "#545D64",
                                    hjust = 0.5))


starwars |>
  slice_max(mass, n = 5) |>
  ggplot() +
  geom_col(aes(x = mass, y = name)) +
  labs(title = "Personagens mais pesados de Starwars") +
  aplicar_tema

# Cores -------


# Vimos exemplos de funções de escalas de cores anteriormente.

# Como colocar uma cor específica?
# Dá para fazer assim:
imdb |>
  mutate(
    nota_cat = case_when(
      nota_imdb >= 7.5 ~ "3 - Ótimo",
      nota_imdb >= 5 ~ "2 - Regular",
      TRUE ~ "1 - Ruim"
    )
  ) |>
  count(nota_cat) |>
 #  mutate(nota_cat = forcats::fct_reorder(nota_cat, n)) |>
  ggplot() +
  geom_col(aes(x = n, y = nota_cat, fill = nota_cat)) +
  scale_fill_manual(values = c("red", "blue", "green"))


# Como colocar uma cor específica? Eu prefiro assim:
imdb |>
  mutate(
    nota_cat = case_when(
      nota_imdb >= 7.5 ~ "3 - Ótimo",
      nota_imdb >= 5 ~ "2 - Regular",
      TRUE ~ "1 - Ruim"
    ),
    cat_cor = case_when(
      nota_cat == "3 - Ótimo" ~ "green",
      nota_cat == "2 - Regular" ~ "orange",
      nota_cat == "1 - Ruim" ~ "red"
    )
  ) |>
  count(nota_cat, cat_cor) |>
  mutate(nota_cat = forcats::fct_reorder(nota_cat, n)) |>
  ggplot() +
  geom_col(aes(x = n, y = nota_cat, fill = cat_cor)) +
  scale_fill_identity()
