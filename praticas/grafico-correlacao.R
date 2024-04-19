library(readr)
library(tidyverse)
mananciais <- read_delim("https://github.com/beatrizmilz/mananciais/raw/master/inst/extdata/mananciais.csv",
    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
        grouping_mark = "."), trim_ws = TRUE)


ultimo_dia_do_mes <- mananciais |>
  dplyr::mutate(
    mes = lubridate::floor_date(data, "month")
  ) |>
  dplyr::group_by(mes, sistema) |>
  dplyr::slice_max(data) |>
  dplyr::ungroup()


ultimo_dia_do_mes |>
  ggplot() +
  aes(x = pluviometria_mensal, y = volume_porcentagem) +
  geom_point() +
  geom_smooth(method = "lm")
