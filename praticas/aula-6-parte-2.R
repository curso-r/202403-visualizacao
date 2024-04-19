# Gráficos interativos
# plotly
# highcharts
# echarts


# Echarts --------------
library(tidyverse)
library(echarts4r)

starwars |>
  mutate(sex = tidyr::replace_na(sex, "Sem informação")) |>
  group_by(sex) |>
  e_chart(x = height) |>
  e_scatter(serie = mass, symbol_size = 10) |>
  e_tooltip() |>
  e_axis_labels(y = "Massa", x = "Altura")





title_lab = "Contagem de personagens do Starwars"


starwars |>
  mutate(sex = tidyr::replace_na(sex, "Sem informação")) |>
  count(sex, name = "Contagem") |>
  arrange(Contagem) |>
  e_chart(x = sex) |>
  e_bar(serie = Contagem, legend = FALSE) |>
  e_flip_coords() |>
  e_tooltip() |>
  e_axis_labels(x = "Contagem de personagens",
                y = "Sexo") |>
  echarts4r::e_grid(left = "20%") |>
  echarts4r::e_title(text = title_lab |>
                       stringr::str_wrap(width = 60)) |>
  echarts4r::e_y_axis(
        nameLocation = "middle",
        nameGap = 100
      ) |>
      echarts4r::e_x_axis(
        nameLocation = "middle",
        nameGap = 30
      ) |>
    echarts4r::e_theme_custom('{"color":["#01274c","#ffca06"]}') |>
    echarts4r::e_toolbox_feature(feature = c("saveAsImage"))





# Plotly ------------

grafico <- starwars |>
  ggplot() +
  geom_point(aes(x = mass, y = height))

plotly::ggplotly(grafico)


library(plotly)

starwars |>
  plot_ly(
    mode = "markers",
    type = "scatter",
    x = ~ mass,
    y = ~ height,
    color = ~ sex
  )


# highcharts --------------------------------
library(highcharter)


starwars |>
  hchart(type = "scatter",
         mapping = hcaes(x = mass, y = height, color = sex))


starwars |>
  ggplot() +
  geom_point(aes(x = mass, y = height, color = sex)) +
  scale_color_viridis_d()
