# Pacotes ----

library(geobr)

library(tidyverse)

library(readxl)

library(sf)

library(geobr)

library(terra)

library(tidyterra)

library(dbscan)

library(vegan)

library(writexl)

# Dados ----

## Áreas ----

### estados ----

#### Importando -----

estados <- geobr::read_state(year = 2019) |>
  dplyr::filter(code_region == 2)

#### Visualizando ----

estados

ggplot() +
  geom_sf(data = estados,
          color = "black",
          fill = "gold")

### Mata Atlântica do Nordeste ----

#### Importando -----

ma <- geobr::read_biomes(year = 2019) |>
  dplyr::filter(name_biome == "Mata Atlântica") |>
  sf::st_intersection(estados |>
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

ramagii <- readxl::read_xlsx("Pristimantis_ramagii.xlsx")

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
  geom_sf(data = ramagii |>
            sf::st_as_sf(coords = c("Longitude", "Latitude"),
                         crs = estados |> sf::st_crs()))

### Pristimantis vinhai ----

#### Importando ----

vinhai <- readxl::read_xlsx("Pristimantis_vinhai.xlsx")

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
  geom_sf(data = vinhai |>
            sf::st_as_sf(coords = c("Longitude", "Latitude"),
                         crs = estados |> sf::st_crs()))

### Pristimantis paulodutrai ----

#### Importando ----

paulodutrai <- readxl::read_xlsx("Pristimantis_paulodutrai.xlsx")

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
  geom_sf(data = paulodutrai |>
            sf::st_as_sf(coords = c("Longitude", "Latitude"),
                         crs = estados |> sf::st_crs()))

## Dados ambientais ----

### Variáveis bioclimáticas ----

#### Importando ----

bio <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/artigo/wc2.1_country/BRA_wc2.1_30s_bio.tif")

#### Tratando ----

names(bio) <- paste0("Bio ", 1:19)

bio <- bio |>
  terra::crop(ma) |>
  terra::mask(ma)

#### Visualizando ----

bio

ggplot() +
  tidyterra::geom_spatraster(data = bio) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Altitude ----

#### Importando ----

elev <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/V CBH/elevation/BRA_elv_msk.tif")

#### Tratando ----

elev <- elev |>
  terra::crop(ma) |>
  terra::mask(ma)

#### Visualizando -----

ggplot() +
  tidyterra::geom_spatraster(data = elev) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Uso e Cobertura do solo ----

#### Importando ----

uso <- terra::rast("C:/Users/LENOVO/OneDrive/Documentos/projeto mestrado/V CBH/brasil_coverage_2023.tif")

#### Tratando ----

uso <- uso |>
  terra::resample(bio$`Bio 1`,
                  method = "bilinear") |>
  terra::crop(ma) |>
  terra::mask(ma)

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

bio |>
  terra::writeRaster("bio.tif", overwrite = TRUE)

# Tabelas ----

## Extrair os valores ----

pristimantis_shp <- dplyr::bind_rows(ramagii[-c(7, 32, 37, 191, 258), ] |>
                                       sf::st_as_sf(coords = c("Longitude", "Latitude"),
                                                    crs = estados |> sf::st_crs()),
                                     paulodutrai[-4, ] |>
                                       sf::st_as_sf(coords = c("Longitude", "Latitude"),
                                                    crs = estados |> sf::st_crs()),
                                     vinhai |>
                                       sf::st_as_sf(coords = c("Longitude", "Latitude"),
                                                    crs = estados |> sf::st_crs()))

pristimantis_shp

extrair_valores <- function(x, y){

  valores_amb <- bio |>
    terra::extract(pristimantis_shp |>
                     dplyr::filter(species == x) |>
                     sf::st_transform(crs = bio |> terra::crs()))

  assign(paste0("tabela_", y),
         valores_amb,
         envir = globalenv())

}

purrr::walk2(pristimantis_shp |>
              dplyr::pull(species) |>
              unique(),
            pristimantis_shp |>
              dplyr::pull(species) |>
              unique() |>
              stringr::str_replace(" ", "_"),
            extrair_valores)

ls(pattern = "tabela_P") |>
  mget(envir = globalenv())

# Autocorrelação espacial ----

## Criando as matrizes de distância para cada espécie

pristimantis <- dplyr::bind_rows(ramagii[-c(7, 32, 37, 191, 258), ] |>
                                   dplyr::mutate(id = 1:nrow(ramagii[-c(7, 32, 37, 191, 258), ])),
                                 paulodutrai[-4, ] |>
                                   dplyr::mutate(id = 1:nrow(paulodutrai[-4, ])),
                                 vinhai |>
                                   dplyr::mutate(id = 1:nrow(vinhai)))

pristimantis

criar_matrizes_distancia <- function(x, y){

  matriz_dist <- pristimantis |>
    dplyr::filter(species == x) |>
    dplyr::select(-1) |>
    as.data.frame() |>
    fields::rdist.earth(miles = FALSE) |>
    as.dist()

  assign(paste0("distancia_", y),
         matriz_dist,
         envir = globalenv())

}

purrr::walk2(pristimantis |>
               dplyr::pull(species) |>
               unique(),
             pristimantis |>
               dplyr::pull(species) |>
               unique() |>
               stringr::str_replace(" ", "_"),
             criar_matrizes_distancia)

ls(pattern = "distancia_") |>
  mget(envir = globalenv())

## Testar diferentes distâncias ----

### Pristimantis paulodutrai ----

testar_distancias_paulodutrai <- function(y) {

  distancia <- c()

  variavel <- c()

  significativo <- c()

  for (x in seq(5, 75, 1)) {

    p_paulodutrai <- pristimantis |>
      dplyr::filter(species == "Pristimantis paulodutrai")

    distancia_cluster <- distancia_Pristimantis_paulodutrai |>
      dbscan::dbscan(eps = x, minPts = 2)

    pontos_manter <- tibble::tibble(
      pontos = 1:nrow(p_paulodutrai),
      Cluster = dplyr::if_else(distancia_cluster$cluster == 0,
                               "Sem Cluster",
                               paste0("Cluster ", distancia_cluster$cluster))
    ) |>
      dplyr::group_by(Cluster) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(pontos)

    p_paulodutrai_geodist <- p_paulodutrai[pontos_manter, ] |>
      dplyr::select(-1) |>
      as.data.frame() |>
      fields::rdist.earth(miles = FALSE) |>
      as.dist() |>
      as.numeric()

    p_paulodutrai_ambdist <- tabela_Pristimantis_paulodutrai[pontos_manter, ] |>
      dplyr::select(-1, y) |>
      vegan::vegdist(method = "euclidean") |>
      as.numeric()

    correlacao <- cor.test(p_paulodutrai_geodist, p_paulodutrai_ambdist, method = "pearson")

    sig <- if(correlacao$estimate <= 0.7 &
              correlacao$statistic > qt(p = 0.05,
                                        df = correlacao$parameter,
                                        lower.tail = FALSE)){

      "Não"

    } else {

      "Sim"

    }

    distancia <- c(distancia, x)

    variavel <- c(variavel, names(tabela_Pristimantis_paulodutrai)[y])

    significativo <- c(significativo, sig)

  }

  tibble::tibble(Distância = distancia,
                 Variável = variavel,
                 Significância = significativo) |>
    dplyr::filter(Significância == "Não") |>
    dplyr::group_by(Variável) |>
    dplyr::slice_min(Distância) |>
    as.data.frame()
}

df_distancias_paulodutrai <- purrr::map_dfr(2:22,
                                            testar_distancias_paulodutrai)

df_distancias_paulodutrai

## Pristimantis ramagii ----

testar_distancias_ramagii <- function(y) {

  distancia <- c()

  variavel <- c()

  significativo <- c()

  for (x in seq(5, 75, 1)) {

    p_ramagii <- pristimantis |>
      dplyr::filter(species == "Pristimantis ramagii")

    distancia_cluster <- distancia_Pristimantis_ramagii |>
      dbscan::dbscan(eps = x, minPts = 2)

    pontos_manter <- tibble::tibble(
      pontos = 1:nrow(p_ramagii),
      Cluster = dplyr::if_else(distancia_cluster$cluster == 0,
                               "Sem Cluster",
                               paste0("Cluster ", distancia_cluster$cluster))
    ) |>
      dplyr::group_by(Cluster) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(pontos)

    p_ramagii_geodist <- p_ramagii[pontos_manter, ] |>
      dplyr::select(-1) |>
      as.data.frame() |>
      fields::rdist.earth(miles = FALSE) |>
      as.dist() |>
      as.numeric()

    p_ramagii_ambdist <- tabela_Pristimantis_ramagii[pontos_manter, ] |>
      dplyr::select(-1, y) |>
      vegan::vegdist(method = "euclidean") |>
      as.numeric()

    correlacao <- cor.test(p_ramagii_geodist, p_ramagii_ambdist, method = "pearson")

    sig <- if(correlacao$estimate <= 0.7 &
              correlacao$statistic > qt(p = 0.05,
                                        df = correlacao$parameter,
                                        lower.tail = FALSE)){

      "Não"

    } else {

      "Sim"

    }

    distancia <- c(distancia, x)

    variavel <- c(variavel, names(tabela_Pristimantis_ramagii)[y])

    significativo <- c(significativo, sig)

  }

  tibble::tibble(Distância = distancia,
                 Variável = variavel,
                 Significância = significativo) |>
    dplyr::filter(Significância == "Não") |>
    dplyr::group_by(Variável) |>
    dplyr::slice_min(Distância) |>
    as.data.frame()
}

df_distancias_ramagii <- purrr::map_dfr(2:22, testar_distancias_ramagii)

df_distancias_ramagii

## Pristimantis vinhai ----

testar_distancias_vinhai <- function(y) {

  distancia <- c()

  variavel <- c()

  significativo <- c()

  for (x in seq(5, 75, 1)) {

    p_vinhai <- pristimantis |>
      dplyr::filter(species == "Pristimantis vinhai")

    distancia_cluster <- distancia_Pristimantis_vinhai |>
      dbscan::dbscan(eps = x, minPts = 2)

    pontos_manter <- tibble::tibble(
      pontos = 1:nrow(p_vinhai),
      Cluster = dplyr::if_else(distancia_cluster$cluster == 0,
                               "Sem Cluster",
                               paste0("Cluster ", distancia_cluster$cluster))
    ) |>
      dplyr::group_by(Cluster) |>
      dplyr::slice_head(n = 1) |>
      dplyr::pull(pontos)

    p_vinhai_geodist <- p_vinhai[pontos_manter, ] |>
      dplyr::select(-1) |>
      as.data.frame() |>
      fields::rdist.earth(miles = FALSE) |>
      as.dist() |>
      as.numeric()

    p_vinhai_ambdist <- tabela_Pristimantis_vinhai[pontos_manter, ] |>
      dplyr::select(-1, y) |>
      vegan::vegdist(method = "euclidean") |>
      as.numeric()

    correlacao <- cor.test(p_vinhai_geodist, p_vinhai_ambdist, method = "pearson")

    sig <- if(correlacao$estimate <= 0.7 &
              correlacao$statistic > qt(p = 0.05,
                                        df = correlacao$parameter,
                                        lower.tail = FALSE)){

      "Não"

    } else {

      "Sim"

    }

    distancia <- c(distancia, x)

    variavel <- c(variavel, names(tabela_Pristimantis_vinhai)[y])

    significativo <- c(significativo, sig)

  }

  tibble::tibble(Distância = distancia,
                 Variável = variavel,
                 Significância = significativo) |>
    dplyr::filter(Significância == "Não") |>
    dplyr::group_by(Variável) |>
    dplyr::slice_min(Distância) |>
    as.data.frame()
}

df_distancias_vinhai <- purrr::map_dfr(2:22, testar_distancias_vinhai)

df_distancias_vinhai

## Excluindo os pontos ----

### Paulodutrai ----

distancia_cluster_paulodutrai <- distancia_Pristimantis_paulodutrai |>
  dbscan::dbscan(eps = 5,
                 minPts = 2)

pontos_manter_paulodutrai <- tibble::tibble(pontos = 1:nrow(pristimantis |>
                                                              dplyr::filter(species == "Pristimantis paulodutrai")),
                                            Cluster = dplyr::if_else(distancia_cluster_paulodutrai$cluster == 0,
                                                                     "Sem Cluster",
                                                                     paste0("Cluster ", distancia_cluster_paulodutrai$cluste))) |>
  dplyr::group_by(Cluster) |>
  dplyr::slice_head(n = 1) |>
  dplyr::pull(pontos)

pontos_manter_paulodutrai

pr_paulodutrai <- pristimantis |>
  dplyr::filter(species == "Pristimantis paulodutrai") |>
  dplyr::filter(id %in% pontos_manter_paulodutrai) |>
  dplyr::select(-4)

pr_paulodutrai

tabela2_Pristimantis_paulodutrai <- tabela_Pristimantis_paulodutrai |>
  dplyr::filter(ID %in% pontos_manter_paulodutrai) |>
  dplyr::mutate(Uso = Uso |> round(0))

tabela2_Pristimantis_paulodutrai

### Ramagii ----

distancia_cluster_ramagii <- distancia_Pristimantis_ramagii |>
  dbscan::dbscan(eps = 0, minPts = 2)

pontos_manter_ramagii <- tibble::tibble(pontos = 1:nrow(pristimantis |>
                                                              dplyr::filter(species == "Pristimantis ramagii")),
                                            Cluster = dplyr::if_else(distancia_cluster_ramagii$cluster == 0,
                                                                     "Sem Cluster",
                                                                     paste0("Cluster ", distancia_cluster_ramagii$cluste))) |>
  dplyr::group_by(Cluster) |>
  dplyr::slice_head(n = 1) |>
  dplyr::pull(pontos)

pontos_manter_ramagii

pr_ramagii <- pristimantis |>
  dplyr::filter(species == "Pristimantis ramagii") |>
  dplyr::filter(id %in% pontos_manter_ramagii) |>
  dplyr::select(-4)

pr_ramagii

tabela2_Pristimantis_ramagii <- tabela_Pristimantis_ramagii |>
  dplyr::filter(ID %in% pontos_manter_ramagii) |>
  dplyr::mutate(Uso = Uso |> round(0))

tabela2_Pristimantis_ramagii

### Vinhai ----

distancia_cluster_vinhai <- distancia_Pristimantis_vinhai |>
  dbscan::dbscan(eps = 5, minPts = 2)

pontos_manter_vinhai <- tibble::tibble(pontos = 1:nrow(pristimantis |>
                                                          dplyr::filter(species == "Pristimantis vinhai")),
                                        Cluster = dplyr::if_else(distancia_cluster_vinhai$cluster == 0,
                                                                 "Sem Cluster",
                                                                 paste0("Cluster ", distancia_cluster_vinhai$cluste))) |>
  dplyr::group_by(Cluster) |>
  dplyr::slice_head(n = 1) |>
  dplyr::pull(pontos)

pontos_manter_vinhai

pr_vinhai <- pristimantis |>
  dplyr::filter(species == "Pristimantis vinhai") |>
  dplyr::filter(id %in% pontos_manter_vinhai) |>
  dplyr::select(-4)

pr_vinhai

tabela2_Pristimantis_vinhai <- tabela_Pristimantis_vinhai |>
  dplyr::filter(ID %in% pontos_manter_vinhai) |>
  dplyr::mutate(Uso = Uso |> round(0))

tabela2_Pristimantis_vinhai

# Exportando ----

## Registros ----

exportando_registros <- function(x, y){

  x |>
    writexl::write_xlsx(paste0("registro_", y, ".xlsx"))

}

lista_registros2 <- ls(pattern = "pr_") |>
  mget(envir = globalenv())

lista_registros2

purrr::walk2(lista_registros2,
             paste0("Pristimantis_",
                    c("paulodutrai", "ramagii", "vinhai")),
             exportando_registros)

## Variáveis ambientais ----

exportando_tabelas <- function(x, y){

  x |>
    writexl::write_xlsx(paste0("tabela_", y, ".xlsx"))

}

lista_tabelas <- ls(pattern = "tabela2_") |>
  mget(envir = globalenv())

lista_tabelas

purrr::walk2(lista_tabelas,
             paste0("Pristimantis_",
                    c("paulodutrai", "ramagii", "vinhai")),
             exportando_tabelas)


