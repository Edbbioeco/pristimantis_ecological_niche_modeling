# Pacotes ----

library(readxl)

library(tidyverse)

library(geobr)

library(sf)

library(terra)

library(tidyterra)

library(reshape2)

# Dados ----

## Registro de ocorrência ----

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

### Unindo os dados ----

bio$Elevação <- elev

ggplot() +
  tidyterra::geom_spatraster(data = bio) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Valores extraídos ----

lista_para_importar <- list.files(pattern = "tabela_")

lista_para_importar

importar_tabelas <- function(x, y){

  tabela <- readxl::read_xlsx(x)

  assign(y,
         tabela,
         envir = globalenv())

}

purrr::walk2(lista_para_importar,
             lista_para_importar %>% stringr::str_remove(".xlsx"),
             importar_tabelas)

# Multicolinearidade ----

## Analisando ----

lista_de_tabelas <- ls(pattern = "tabela_") %>%
  mget(envir = globalenv())

lista_de_tabelas

criar_multicolinearidade_tabelas <- function(x, y){

  tabela <- x %>%
    cor(method = "spearman")

  tabela[upper.tri(tabela)] <- NA

  tabela <- tabela %>%
    reshape2::melt() %>%
    tidyr::drop_na() %>%
    dplyr::mutate(igual = dplyr::case_when(Var1 == Var2 ~ "Sim",
                                           .default = "Não"),
                  Espécie = paste0("Pristimantis ", y)) %>%
    dplyr::filter(igual == "Não") %>%
    dplyr::select(-igual)

  assign(paste0("Multicolinearidade_", y),
         tabela,
         envir = globalenv())

}

purrr::walk2(lista_de_tabelas,
             ls(pattern = "tabela_") %>% stringr::str_remove("tabela_"),
             criar_multicolinearidade_tabelas)

ls(pattern = "Multicolinearidade_") %>%
  mget(envir = globalenv()) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(value = value %>% round(2)) %>%
  dplyr::rename("Spearman Correlation Index" = value) %>%
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 1) +
  geom_text(color = "black", size = 2.5) +
  coord_equal() +
  scale_fill_gradientn(breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1),
                       colours = c(viridis::viridis(n = 10) %>% rev(), viridis::viridis(n = 10))) +
  labs(x = NULL,
       y = NULL) +
  guides(fill = guide_colourbar(title.position = "top",
                                title.hjust = 0.5,
                                barwidth = 20,
                                barheight = 1.5,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  facet_wrap(~Espécie) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.text.x = element_text(color = "black", size = 12, angle = -90),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom")

## Excluindo ----

### Pristimantis paulodutrai ----

paulodutraai_pca1 <- bio[[2]]
