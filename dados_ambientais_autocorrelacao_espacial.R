# Pacotes ----

library(geobr)

library(tidyverse)

library(readxl)

library(sf)

library(geobr)

library(terra)

library(tidyterra)

library(writexl)

# Dados ----

## Áreas ----

### estados ----

#### Importando -----

estados <- geobr::read_state(year = 2019) %>%
  dplyr::filter(code_region == 2)

#### Visualizando ----

estados

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold")

### Mata Atlântica do Nordeste ----

#### Importando -----

ma <- geobr::read_biomes(year = 2019) %>%
  dplyr::filter(name_biome == "Mata Atlântica") %>%
  sf::st_intersection(estados %>%
                        sf::st_union())

#### Visualizando ----

ma

ggplot() +
  geom_sf(data = estados, color = "black", fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 0.5)

## Registros ----

### Pristimantis ramagii ----

#### Importando ----

ramagii <- readxl::read_xlsx("Pristimantis_ramagii.xlsx") %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = estados %>% sf::st_crs())

#### Visualizando ----

ramagii

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 0.5) +
  geom_sf(data = ramagii)

### Pristimantis vinhai ----

#### Importando ----

vinhai <- readxl::read_xlsx("Pristimantis_vinhai.xlsx") %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = estados %>% sf::st_crs())

#### Visualizando ----

vinhai

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 0.5) +
  geom_sf(data = vinhai)

### Pristimantis paulodutrai ----

#### Importando ----

paulodutrai <- readxl::read_xlsx("Pristimantis_paulodutrai.xlsx") %>%
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = estados %>% sf::st_crs())

#### Visualizando ----

paulodutrai

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 0.5) +
  geom_sf(data = paulodutrai)

## Dados ambientais ----

### Variáveis bioclimáticas ----

#### Importando ----

bio <- terra::rast("G:/Meu Drive/UFPE/2023.1/TCC/artigo/wc2.1_country/BRA_wc2.1_30s_bio.tif")

#### Tratando ----

names(bio) <- paste0("Bio ", 1:19)

bio <- bio %>%
  terra::crop(estados) %>%
  terra::mask(estados)

#### Visualizando ----

bio

ggplot() +
  tidyterra::geom_spatraster(data = bio) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Altitude ----

#### Importando ----

elev <- terra::rast("G:/Meu Drive/UFPE/projeto mestrado/V CBH/elevation/BRA_elv_msk.tif")

#### Tratando ----

elev <- elev %>%
  terra::crop(estados) %>%
  terra::mask(estados)

#### Visualizando -----

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Uso e Cobertura do solo ----

#### Importando ----

uso <- terra::rast("G:/Meu Drive/UFPE/projeto mestrado/V CBH/brasil_coverage_2023.tif")

#### Tratando ----

uso <- uso %>%
  terra::resample(bio$`Bio 1`,
                  method = "bilinear") %>%
  terra::crop(estados) %>%
  terra::mask(estados)

#### Visualizando -----

ggplot() +
  tidyterra::geom_spatraster(data = uso) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Unindo os dados ----

bio$Elevação <- elev

bio$Uso <- uso

plotar_bio <- function(x){

  plots <- ggplot() +
    tidyterra::geom_spatraster(data = bio[[x]]) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

  print(plots)

}

purrr::walk(1:length(names(bio)),
            plotar_bio)

## Exportando ----

bio %>%
  terra::writeRaster("bio.tif")

# Tabelas ----

## Extrair os valores ----

pristimantis_shp <- dplyr::bind_rows(ramagii[-c(7, 32, 37, 191, 258), ],
                                     paulodutrai[-4, ],
                                     vinhai)

pristimantis_shp

extrair_valores <- function(x){

  valores_amb <- bio %>%
    terra::extract(pristimantis_shp %>%
                     dplyr::filter(species == x) %>%
                     sf::st_transform(crs = bio %>% terra::crs()))

  assign(paste0("tabela_", x),
         valores_amb,
         envir = globalenv())

}

purrr::walk(pristimantis_shp %>%
              dplyr::pull(species) %>%
              unique,
            extrair_valores)

ls(pattern = "tabela_P") %>%
  mget(envir = globalenv())

# Autocorrelação espacial ----

## Teste de Moran ----

### Pristimantis paulodutrai ----





























## Exportando ----

lista_tabelas <- ls(pattern = "tabela_P") %>%
  mget(envir = globalenv())

lista_tabelas

exportar_valores <- function(x, y){

  x %>%
    writexl::write_xlsx(paste0("tabela_", y, ".xlsx"))

}

purrr::walk2(lista_tabelas,
             c("ramagii",
               "paulodutrai",
               "vinhai"),
              exportar_valores)
