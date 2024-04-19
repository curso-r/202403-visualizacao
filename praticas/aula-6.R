library(tidyverse)

dados_piramide <- readxl::read_excel("dados/censo-2022/Censo 2022 - Pirâmide etária - Brasil.xlsx")

dados_piramide_tratado <- dados_piramide |>
  janitor::clean_names() |>
  select(-x5, -x6)


# eixo Y - grupos etarios
# eixo X - contagem de pessoas no grupo
# fill = sexo


# dados_piramide_tratado |>
#  ggplot() +
#  aes(y = grupo_de_idade, x = ???) +
#  geom_col()


dados_piramide_tratado
# o que temos:
# cada linha representa um grupo de idade.
# temos coluna: grupo de idade, pop fem, pop masc, recorte

# e o que queremos:
# col grupo de idade, sexo, pop, recorte

# pivot_wider - "alargar" a tabela
# pivot_longer - "alongar" a tabela

dados_piramide_longo <- dados_piramide_tratado |>
  pivot_longer(cols = c(
    "populacao_feminina_pessoas",
    "populacao_masculina_pessoas"
  )) |>
  rename(populacao = value, sexo = name)





# eixo Y - grupos etarios
# eixo X - contagem de pessoas no grupo
# fill = sexo


dados_piramide_longo |>
  ggplot() +
  aes(y = grupo_de_idade, x = populacao, fill = sexo) +
  geom_col()



# Coisas que precisam ser corrigidas:
# 1) ok! grupos de idade estão fora da ordem - transformar em fator!
# 2) OK! notação científica na pop, fica difícil de ler
# 3) OK! renomear a variável sexo,
# 4) OK! formato de pirâmide
# 5) cor / padrão/daltonismo

dados_piramide_prep <- dados_piramide_longo |>
  mutate(
    pop = if_else(sexo == "populacao_masculina_pessoas",
                  populacao * -1,
                  populacao),
    pop_milhoes = pop / 1000000,
    sexo_renomeado = case_when(
      sexo == "populacao_masculina_pessoas" ~ "Masculino",
      sexo == "populacao_feminina_pessoas" ~ "Feminino"
    ),
    grupo_de_idade_fct = factor(
      grupo_de_idade,
      levels = c(
        "0 a 4 anos",
        "5 a 9 anos",
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
# dados_piramide_prep$grupo_de_idade |>
#   unique() |>
#   sort() |>
#   paste0(collapse = "', '")



options(scipen = 999)

dados_piramide_prep |>
  ggplot() +
  aes(y = grupo_de_idade_fct, x = pop_milhoes, fill = sexo_renomeado) +
  geom_col() +
  geom_vline(xintercept = 0) +
  scale_x_continuous(labels = abs, n.breaks = 10) +
  # Paleta de cores
  scale_fill_brewer(palette = "Set2", direction = -1) +
  labs(
    x = "População (em milhões)",
    y = "Faixa etária",
    fill = "",
    caption = "Fonte: Dados do Censo 2022 disponibilizado pelo IBGE.",
    title = "Pirâmide etária do Brasil - Censo 2022"
  ) +
  theme_minimal() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5) #,
        #legend.title = element_blank()
        )


