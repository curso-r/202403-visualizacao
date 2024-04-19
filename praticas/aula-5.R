# Mapas

# Elementos matriciais ou raster - tiff, png
# pixel
# imagem de satélite

# elementos vetoriais - shp, kml, kmz, geojson, svg
# pontos
# linhas
# polígonos

# Pacotes
library(tidyverse)
library(geobr) # dados vetoriais oficiais do Brasil
library(sf) # simple feature, principal pacote para trabalhar
# com dados vetoriais
# remotes::install_github("abjur/abjData")
library(abjData)
library(parzer)
# remotes::install_github("curso-r/munifacil")

# -------------------------------------------

# Quais são as funções disponíveis no geobr?
list_geobr() |> View()


delim_brasil <- read_country(year = 2020)

delim_brasil
# Simple feature collection with 27 features and 5 fields
# Geometry type: MULTIPOLYGON
# Dimension:     XY
# Bounding box:  xmin: -73.99045 ymin: -33.75118 xmax: -28.84784 ymax: 5.271841
# Geodetic CRS:  SIRGAS 2000 # Coordinate Reference System
# First 10 features:

# Córrego Alegre,

delim_brasil |>
  ggplot() +
  geom_sf()



delim_brasil |>
  ggplot() +
  geom_sf(aes(fill = name_region))


delim_pb <- geobr::read_state("PB", year = 2020)

delim_muni_pb <- geobr::read_municipality("PB", year = 2020)

muni_br <- geobr::read_municipality(year = "2020")


ggplot() +
  geom_sf(data = delim_brasil) +
  geom_sf(data = delim_pb, color = "red", size = 5, fill = "red")# +
  # geom_sf(data = delim_muni_br)

# Fazer join

# Identificar a coluna chave
# Se tiver código do município a gente fica feliz

# Se não tiver, usamos o nome e UF

dados_paraiba <- abjData::pnud_muni |>
  dplyr::filter(uf == 25) |>
  dplyr::mutate(code_muni = codmun7,
                code_state = uf)

names(dados_paraiba)

delim_muni_pb_sf <- delim_muni_pb |> # no join, inicio com a base sf
  full_join(dados_paraiba)

# Joining with `by = join_by(code_muni, code_state)`

delim_muni_pb_sf |>
  ggplot() +
  geom_sf(aes(fill = espvida)) +
  facet_wrap( ~ ano) +
  scale_fill_viridis_c() +
   # theme_void()
  theme_minimal()


# Coluna: nome do município

muni_destaque <- delim_muni_pb_sf |>
  select(ano, code_muni, name_muni, espvida, geom) |>
  mutate(espvida_meta = espvida >= 65) |>
  group_by(code_muni) |>
  mutate(soma_espvida_meta = sum(espvida_meta)) |>
  ungroup() |>
  filter(soma_espvida_meta == max(soma_espvida_meta))


delim_muni_pb_sf |>
  ggplot() +
  geom_sf(aes(fill = espvida)) +
  geom_sf(fill = "red", data = muni_destaque) +
  facet_wrap( ~ ano) +
  scale_fill_viridis_c() +
   # theme_void()
  theme_minimal()

# Mais facil ---------------------------------------

maior_que_65 <- delim_muni_pb_sf |>
  select(ano, code_muni, name_muni, espvida, geom) |>
  mutate(espvida_meta = espvida >= 65) |>
  filter(espvida_meta == TRUE)

delim_muni_pb_sf |>
  ggplot() +
  geom_sf(fill = "white") +
  geom_sf(fill = "red", data = maior_que_65) +
  facet_wrap( ~ ano) +
   # theme_void()
  theme_minimal()

# Reiniciando a sessão -----------------------------
# pós intervalo
library(tidyverse)
library(readr)
apendice_c_2018 <- read_csv("https://beatrizmilz.github.io/2020-FLS6397/projeto_final/dados/apendice_c_2018.csv",
    locale = locale(encoding = "ISO-8859-2"),
    skip = 4) |>
  janitor::clean_names()

dados_cetesb <- apendice_c_2018 |>
  rename(populacao_urbana = urubana,
         corpo_receptor = x11) |>
  filter(ugrhi != "UGRHI") |>
  drop_na(municipio) |>
  mutate(name_muni = municipio)


municipios_sp <- geobr::read_municipality("SP")


# não deu!
dados_unidos <- municipios_sp |>
  full_join(dados_cetesb)


# remotes::install_github("curso-r/munifacil")

dados_cetesb_cod_ibge <- dados_cetesb |>
  mutate(uf = "SP") |>
  munifacil::limpar_colunas(col_muni = municipio, col_uf = uf) |>
  munifacil::incluir_codigo_ibge() |>
  mutate(code_muni = id_municipio) |>
  select(-name_muni)


dados_unidos <- municipios_sp |>
  mutate(code_muni = as.character(code_muni)) |>
  full_join(dados_cetesb_cod_ibge, by = "code_muni")


# Joining with `by = join_by(code_muni, name_muni)`
# Error in `sf_column %in% names(g)`:
# ! Can't join `x$code_muni` with `y$code_muni` due to
#   incompatible types.
# ℹ `x$code_muni` is a <double>.
# ℹ `y$code_muni` is a <character>.



# leaflet ----------------------------------------------------

library(leaflet)


dados_unidos |>
  leaflet() |>
  addTiles() |>
  addPolygons(
    # fill = TRUE,
    # fillColor = ~populacao_urbana,
    popup = ~name_muni
  )


dados_unidos |>
  leaflet() |>
  addProviderTiles(provider = providers$Esri.WorldImagery) |>
  addPolygons(
    fill = TRUE,
    fillColor = ~populacao_urbana,
    fillOpacity = 0.1,
    popup = ~name_muni
  )


# Ideia: mapa de calor
