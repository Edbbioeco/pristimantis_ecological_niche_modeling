# Pacotes ----

library(geobr)

library(tidyverse)

library(readxl)

library(parzer)

library(spThin)

library(sf)

library(writexl)

# Dados ----

## Estados do Nordeste ----

### Importando -----

estados <- geobr::read_state(year = 2019) %>%
  dplyr::filter(code_region == 2)

### Visualizando ----

estados

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold")

## Mata Atlântica do Nordeste ----

### Importando ----

ma <- geobr::read_biomes(year = 2019) %>%
  dplyr::filter(name_biome == "Mata Atlântica") %>%
  sf::st_intersection(estados %>%
                        sf::st_union())

### Visualizando ----

ma

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 1) +
  theme_bw()

## GBIF ----

### importando ----

gbif <- readxl::read_xlsx("gbif.xlsx")

gbif

gbif %>% dplyr::glimpse()

### Tratando ----

gbif_trat <- gbif %>%
  dplyr::select(species, decimalLatitude:decimalLongitude) %>%
  dplyr::rename("Latitude" = decimalLatitude,
                "Longitude" = decimalLongitude) %>%
  dplyr::mutate(Longitude = Longitude %>%
                  stringr::str_replace("^(-?\\d{2})(\\d+)$", "\\1.\\2") %>%
                  as.numeric(),
                Latitude = case_when(stringr::str_detect(as.character(Latitude), "^(-?[1-2])") ~ str_replace(as.character(Latitude), "^(-?\\d{2})(\\d+)$", "\\1.\\2"),
                                     stringr::str_detect(as.character(Latitude), "^(-?[3-9])") ~ stringr::str_replace(as.character(Latitude), "^(-?\\d{1})(\\d+)$", "\\1.\\2"),
                                     TRUE ~ as.character(Latitude)) %>%
                  as.numeric()) %>%
  dplyr::filter(!is.na(species) &
                  !is.na(Latitude) &
                  !is.na(Longitude) &
                  !species %>% stringr::str_detect(" sp| cf| af| relictus") &
                  species %>% stringr::word(2) != "NA") %>%
  dplyr::distinct(species, Longitude, Latitude, .keep_all = TRUE)

gbif_trat$species %>% unique()

gbif_trat

## SpeciesLink ----

### Importando ----

specieslink <- readxl::read_xlsx("speciesLink.xlsx")

### Tratando ----

specieslink_trat <- specieslink %>%
  dplyr::select(species, scientificname, longitude, latitude) %>%
  dplyr::filter(!species %>% is.na() &
                  !species %>% stringr::str_detect("sp|aff")) %>%
  tidyr::drop_na() %>%
  dplyr::select(-species) %>%
  dplyr::rename("species" = scientificname,
                "Longitude" = longitude,
                "Latitude" = latitude) %>%
  dplyr::mutate(Longitude = Longitude %>% as.numeric(),
                Latitude = Latitude %>% as.numeric())

specieslink_trat %>% dplyr::glimpse()

specieslink_trat

## SiBBr ----

### Importando ----

sibbr <- readr::read_csv("sibbr.csv")

### Tratando ----

sibbr_trat <- sibbr %>%
  dplyr::select(Species, decimalLatitude:decimalLongitude) %>%
  tidyr::drop_na() %>%
  dplyr::rename("species" = Species,
                "Latitude" = decimalLatitude,
                "Longitude" = decimalLongitude) %>%
  dplyr::filter(species %in% unique(gbif_trat$species))

sibbr_trat

## Literatura ----

### Importando ----

literatura <- read_xlsx("literatura.xlsx")

### Tratando ----

literatura_trat <- literatura %>%
  dplyr::rename("species" = Espécie) %>%
  dplyr::select(species, Latitude, Longitude) %>%
  dplyr::mutate(Latitude = Latitude %>% parzer::parse_lat(),
                Longitude = Longitude %>% parzer::parse_lon())

literatura_trat

# Separando por espécies ----

## Unindo os dataframes ----

pristimantis <- ls(pattern = "trat") %>%
  mget(envir = globalenv()) %>%
  dplyr::bind_rows()

pristimantis

## Separando as espécies ----

separar_especies <- function(x){

  sp <- pristimantis %>%
    dplyr::filter(species == x)

  assign(paste0("registro_", x),
         sp,
         envir = globalenv())

}

especies <- unique(gbif_trat$species)

especies

purrr::walk(especies,
            separar_especies)

# Exportando para cada espécie ----

## Criando a lista ----

lista_registros <- ls(pattern = "registro_") %>%
  mget(envir = globalenv())

lista_registros <- lista_registros[-3]

lista_registros

## Exportando ----

exportar_especies <- function(x, y){

  coords <- x %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = ma %>% sf::st_crs()) %>%
    sf::st_intersection(ma) %>%
    sf::st_coordinates() %>%
    as.data.frame()

  df <- x %>%
    sf::st_as_sf(coords = c("Longitude", "Latitude"),
                 crs = ma %>% sf::st_crs()) %>%
    sf::st_intersection(ma) %>%
    as.data.frame() %>%
    dplyr::mutate(Longitude = coords$X,
                  Latitude = coords$Y) %>%
    dplyr::select(species, Longitude, Latitude)

  df %>%
    writexl::write_xlsx(paste0(y, ".xlsx"))

}

purrr::walk2(lista_registros,
             paste0("Pristimantis_",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             exportar_especies)

