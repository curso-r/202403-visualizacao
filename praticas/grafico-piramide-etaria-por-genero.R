# Baixei os dados em: https://censo2022.ibge.gov.br/panorama/
# Baixei os dados para Osasco
library(ggplot2)
piramide_etaria <-
  readr::read_delim(
    "dados/censo-2022/Censo 2022 - Pirâmide etária - Osasco (SP).csv",
    delim = ";",
    escape_double = FALSE,
    trim_ws = TRUE
  ) |>
  janitor::clean_names()


piramide_etaria_preparada <- piramide_etaria |>
  tidyr::pivot_longer(
    cols = c("populacao_feminina_pessoas", "populacao_masculina_pessoas"),
    values_to = "populacao"
  ) |>
  dplyr::mutate(
    populacao = dplyr::case_when(
      name == "populacao_masculina_pessoas" ~ populacao * -1,
      TRUE ~ populacao
    ),
    sexo = dplyr::case_when(
      name == "populacao_feminina_pessoas" ~ "Feminino",
      name == "populacao_masculina_pessoas" ~ "Masculino",
    ),
    grupo_de_idade_fct = factor(
      grupo_de_idade,
      levels = c(
        "0 a 4 anos",
        "5 a 9 anos" ,
        "10 a 14 anos",
        "15 a 19 anos",
        "20 a 24 anos",
        "25 a 29 anos",
        "30 a 34 anos",
        "35 a 39 anos",
        "40 a 44 anos",
        "45 a 49 anos",
        "50 a 54 anos",
        "55 a 59 anos",
        "60 a 64 anos",
        "65 a 69 anos",
        "70 a 74 anos",
        "75 a 79 anos",
        "80 a 84 anos",
        "85 a 89 anos",
        "90 a 94 anos",
        "95 a 99 anos",
        "100 anos ou mais"
      )
    )
  )

#unique(piramide_etaria_preparada$grupo_de_idade) |> sort()


piramide_etaria_preparada |>
  ggplot() +
  geom_col(aes(x = populacao, y = grupo_de_idade_fct, fill = sexo)) +
  geom_vline(xintercept = 0) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2", direction = -1) +
  labs(x = "População",
       y = "Faixa etária",
       title = "Pirâmide Etária de Osasco, SP - Censo 2022",
       fill = "Sexo") +
  scale_x_continuous(labels = abs)


# Material complementar:
# https://epirhandbook.com/en/demographic-pyramids-and-likert-scales.html
