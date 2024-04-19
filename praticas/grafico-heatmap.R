library(readr)
library(tidyverse)
mananciais <- read_delim("https://github.com/beatrizmilz/mananciais/raw/master/inst/extdata/mananciais.csv",
    delim = ";", escape_double = FALSE, locale = locale(decimal_mark = ",",
        grouping_mark = "."), trim_ws = TRUE)


mananciais |>
  ggplot() +
  aes(x = data, y = sistema, fill = volume_porcentagem) +
  geom_tile() +
  theme_minimal() +
  scale_fill_viridis_c()



