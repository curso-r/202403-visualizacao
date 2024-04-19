# Carregando pacotes -----
library(tidyverse)

# Carregando os dados ----

imdb <- read_csv("https://raw.githubusercontent.com/curso-r/main-r4ds-1/master/dados/imdb.csv")

# Um gráfico com ggplot2, usando o orcamento e a receita dos filmes
# e colorir os pontos a partir da pergunta: o filme lucrou?

imdb_com_lucro <- imdb |>
  mutate(
    lucro = receita - orcamento,
    # if_else(condicao,
    # valor_caso_condicao_seja_verdadeira,
    # valor_caso_condicao_seja_falsa)
    lucrou = if_else(lucro > 0, "Sim", "Não")
  )


imdb_com_lucro |>
  ggplot() +
  aes(x = orcamento, y = receita, color = lucrou) +
  geom_point()



grafico_lucro <- imdb_com_lucro |>
  drop_na(orcamento, receita) |>
  ggplot() +
  aes(x = orcamento, y = receita, color = lucrou) +
  # alpha é um valor de transparencia
  # entre 0 e 1
  # 0 é totalmente transparente
  # 1 é totalmente colorido
  geom_point(alpha = 0.5)

grafico_lucro

# Função para criar uma pasta/diretório
fs::dir_create("graficos_gerados")
# Compar

# Salvar esse gráfico!
ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro
)


ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_alta.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  height = 10, # altura
  width = 5 # largura

)



ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_larga.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  height = 5, # altura
  width = 10 # largura
)


ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_inches.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  height = 5, # altura
  width = 10, # largura
  unit = "in"
)



ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_50-dpi.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  dpi = 50, # resolucao
  # dots per inch
)



ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_100-dpi.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  dpi = 100, # resolucao
  # dots per inch
)


ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_300-dpi.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  dpi = 300, # resolucao
  # dots per inch
)


ggsave(
  filename = "graficos_gerados/imbd_lucro_filmes_1000-dpi.png",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
  dpi = 1000, # resolucao
  # dots per inch
)


ggsave(
  # SVG - Formato de imagem vetorizada
  # Dá para editar em outros software, como o ilustrator
  filename = "graficos_gerados/imbd_lucro_filmes.svg",
  # plot = por padrão, último gráfico gerado
  plot = grafico_lucro,
)

# Patchwork é útil
# https://patchwork.data-imaginist.com/


# Até então vimos 1 tipo de gráfico: scaterplot/gráfico de dispersão/pontos.

# Gráfico de linhas -----------------------------


# Gráfico da quantidade de filmes ao longo tempo
# x = ano
# y = quantidade de filmes naquele ano
imdb |>
  # agrupar
  group_by(ano) |>
  summarise(quantidade_filmes = n())

# Outra opção

imdb |>
  # contagem de linhas por grupo
  count(ano) |>
  drop_na(ano) |>
  filter(ano != 2020) |>
  ggplot() +
  geom_line(aes(x = ano, y = n))


# aes() pode ser usado em 3 lugares
# tudo que vem de colunas dos dados, precisamos
# passar pelo aes()

# forma 1 e 2 funcionam igual! compartilha o aes()
# para todas as geometrias

imdb |>
  count(ano) |>
  ggplot(aes(x = ano, y = n)) +
  geom_line() +
  geom_point()



imdb |>
  count(ano) |>
  ggplot() +
  aes(x = ano, y = n) +
  geom_line() +
  geom_point()

# forma 3 - o aes() só vale para a geometria onde ele é usado
# o código abaixo gera um erro, pois precisamos
# indicar qual é o x e y para o geom_point

imdb |>
  count(ano) |>
  ggplot() +
  geom_line(aes(x = ano, y = n)) +
  geom_point()

# Dúvida Karina:
# daria pra colocarmos o meses no x e diferentes linhas para os anos?
# Como se a gente quisesse ver se existe algum tipo de tendencia ao longo dos anos?

# daria para fazer pela variavel data de lancamento não?

imdb |>
  glimpse()

imdb_data_lancamento <- imdb |>
  mutate(
    data_lancamento_date  = readr::parse_date(
      data_lancamento,
      format = "%Y-%m-%d"
    ),
    mes_lancamento_numero = lubridate::month(data_lancamento_date),
    mes_lancamento_texto = lubridate::month(data_lancamento_date,
                                            label = TRUE)
    # TO DO: ARRUMAR LOCALE
  ) |>
  drop_na(data_lancamento_date)



imdb_data_lancamento |> glimpse()
# mes_lancamento_texto  <ord> Jan, Nov, Mar, May, Apr…


imdb_data_lancamento |>
  filter(ano %in% c(seq(1990, 2020, by = 10))) |>
  mutate(ano_texto = as.character(ano)) |>
  count(ano, ano_texto, mes_lancamento_numero, mes_lancamento_texto) |>
  ggplot() +
  aes(x = mes_lancamento_texto, y = n, color = ano_texto) +
  geom_line(aes(group = ano_texto))

# conseguimos mudar os limites do eixo Y?
imdb_data_lancamento |>
  filter(ano %in% c(2017, 2018, 2019)) |>
  mutate(ano_texto = as.character(ano)) |>
  count(ano, ano_texto, mes_lancamento_numero, mes_lancamento_texto) |>
  ggplot() +
  aes(x = mes_lancamento_texto, y = n, color = ano_texto) +
  geom_line(aes(group = ano_texto)) +
  scale_y_continuous(limits = c(0, 120))



# como buscar sempre os 3 anos mais recentes?

vetor_anos_mais_recentes <- imdb_data_lancamento |>
  distinct(ano) |>
  slice_max(order_by = ano, n = 3) |>
  pull(ano)

imdb_data_lancamento |>
  filter(ano %in% vetor_anos_mais_recentes) |>
  mutate(ano_texto = as.character(ano)) |>
  count(ano, ano_texto, mes_lancamento_numero, mes_lancamento_texto) |>
  ggplot() +
  aes(x = mes_lancamento_texto, y = n, color = ano_texto) +
  geom_line(aes(group = ano_texto))

# Gráfico de barras ----------------------------------

# Quais são os gêneros mais frequentes no filmes dessa base?

# base original - 1 filme por linha
imdb |>
  count(generos)


# 1 genero por linha,
# e consequentemente os filmes com mais de 1 genero
# vao ocupar mais de uma linha!

imdb_generos <- imdb |>
  separate_longer_delim(generos, delim = ", ")



## geom_col ---
# primeira versão
imdb_generos |>
  count(generos, sort = TRUE) |>
  ggplot() +
  aes(x = generos, y = n) +
  geom_col()

# inverter eixos
imdb_generos |>
  count(generos, sort = TRUE) |>
  ggplot() +
  aes(y = generos, x = n) +
  geom_col()

# limitamos para os 10 generos + frequentes
imdb_generos |>
  count(generos, sort = TRUE) |>
  slice_max(order_by = n, n = 10) |>
  ggplot() +
  aes(x = generos, y = n) +
  geom_col()


imdb_generos_fct <- imdb_generos |>
  count(generos, sort = TRUE) |>
  slice_max(order_by = n, n = 10) |>
  mutate(generos_fator_crescente = forcats::fct_reorder(generos, n),
         generos_fator_decrescente = forcats::fct_reorder(generos, -n))




imdb_generos_fct |>
  arrange(generos)


imdb_generos_fct |>
  arrange(generos_fator_crescente)


imdb_generos_fct |>
  ggplot() +
  # do menor para o maior
  aes(x = generos_fator_crescente, y = n) +
  geom_col()


imdb_generos_fct |>
  ggplot() +
  # do maior para o menor
  aes(x = generos_fator_decrescente, y = n) +
  geom_col()



# Outra forma:

imdb_generos |>
  count(generos, sort = TRUE) |>
  slice_max(order_by = n, n=10) |> #seleciona os 10 que apresentam maior quantidade de filmes
  ggplot() +
  # reorder também funciona!
  aes(x = reorder(generos, -n), y = n) + #ordena por maior frequencia
  geom_col()



## geom_bar ----

# geom_bar só recebe o eixo x, e ele faz a contagem!
imdb_generos |>
  ggplot() +
  geom_bar(aes(x = generos))


imdb_generos |>
  ggplot() +
  geom_bar(aes(x = generos)) +
  coord_flip()

# geom_label e geom_text ---------------------------
imdb |>
  separate_longer_delim(generos, delim = ", ") |>
  count(generos, sort = TRUE) |>
  slice_max(order_by = n, n = 10) |>
  mutate(generos_fator_crescente = forcats::fct_reorder(generos, n),
         generos_fator_decrescente = forcats::fct_reorder(generos, -n),
         mil_filmes = round(n/1000, 1),
         texto_mil_filmes = paste0(mil_filmes, "k"))  |>
  ggplot() +
  # do maior para o menor
  aes(y = generos_fator_crescente, x = n) +
  geom_col() +
  # geom_text escreve um texto
  geom_text(aes(label = texto_mil_filmes,
                # meio
                # x = n/2
                # fora da barra
                x = n + 600
                ),
            color = "black") +
  # geom_label coloca uma caixinha em volta do texto
  geom_label(aes(label = texto_mil_filmes,
                # meio
                 x = n/2
                # fora da barra
                #x = n + 600
                ))

# Histograma --------------------------------

imdb |>
  ggplot() +
  geom_histogram(aes(x = nota_imdb),
                 # cor de preenchimento
                 fill = "lightblue",
                 # cor da borda
                 color = "black")
# `stat_bin()` using `bins = 30`. Pick better value with
# `binwidth`.



imdb |>
  ggplot() +
  geom_histogram(aes(x = nota_imdb),
                 # largura da barra: 1
                 binwidth = 1,
                 # cor de preenchimento
                 fill = "lightblue",
                 # cor da borda
                 color = "black")



imdb |>
  ggplot() +
  geom_histogram(aes(x = nota_imdb),
                 # quantidade de barras
                 bins = 20,
                 # cor de preenchimento
                 fill = "lightblue",
                 # cor da borda
                 color = "black")


# Como posso alterar o eixo y, por exemplo,
# ir ate 5000, de 1000 em 1000

imdb |>
  ggplot() +
  geom_histogram(aes(x = nota_imdb),
                 # largura da barra: 1
                 binwidth = 1,
                 # cor de preenchimento
                 fill = "lightblue",
                 # cor da borda
                 color = "black") +
  scale_y_continuous(
   # breaks = c(0, 5000)
    breaks = seq(0, 10000, by = 1000)
  ) +
  scale_x_continuous(
    breaks = seq(0, 10, by = 1)
  )




imdb |>
  ggplot() +
  geom_histogram(aes(x = nota_imdb),
                 # largura da barra: 1
                 binwidth = 1,
                 # cor de preenchimento
                 fill = "lightblue",
                 # cor da borda
                 color = "black") +
  scale_y_continuous(
   # breaks = c(0, 5000)
    # breaks = seq(0, 10000, by = 1000)
     breaks = c(0,5000,10000)
  )

# Boxplot --------------------------------


imdb |>
  ggplot() +
  geom_boxplot(aes(x = nota_imdb))


# Para a próxima aula:
# Vamos fazer um exemplo pensando em algum grupo da base.
# Os gêneros mais frequentes

# Próxima aula

# Poderia inserir um ponto com a media (destacado em vermelho)




