# score -1 a 1 - democracia
# score -1 a 1 - formalidade

# parecido com o gráfico da PCA, mas posicionando o ponto do partido
# em cada quadrante

# 0 é a linha de corte

# pesquisar como deixar os 4 quadrantes iguais da PCA

# https://github.com/kassambara/factoextra/blob/master/R/fviz.R
library(ggplot2)

var_1 <- runif(100, min = -1, max = 1)
var_2 <- runif(100, min = -1, max = 1)

dados_exemplo <- tibble::tibble(
  var_1,
  var_2
)

dados_exemplo |>
  ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0) +
  geom_point(aes(x = var_1, y = var_2), color = "blue") +
  theme_light()


