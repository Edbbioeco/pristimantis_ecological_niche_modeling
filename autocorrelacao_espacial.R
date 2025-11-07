# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(terra)

library(tidyterra)

library(readxl)

library(reshape2)

library(spdep)

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

## Dados ambientais ----

### Importando ----

bio <- terra::rast("bio.tif")

### Visualizando ----

bio

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

purrr::walk2(lista_para_importar[3:5],
             lista_para_importar[3:5] %>% stringr::str_remove(".xlsx"),
             importar_tabelas)

# Multicolinearidade ----

## Analisando ----

lista_de_tabelas <- ls(pattern = "tabela_") %>%
  mget(envir = globalenv())

lista_de_tabelas

criar_multicolinearidade_tabelas <- function(x, y){

  tabela <- x %>%
    tidyr::drop_na() %>%
    dplyr::select(-1) %>%
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
             ls(pattern = "tabela_") %>%
               stringr::str_remove("tabela_Pristimantis_"),
             criar_multicolinearidade_tabelas)

ls(pattern = "Multicolinearidade_") %>%
  mget(envir = globalenv()) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(value = value %>% round(2)) %>%
  dplyr::rename("Spearman Correlation Index" = value) %>%
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(color = "black", size = 2) +
  coord_equal() +
  scale_fill_gradientn(breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1),
                       colours = c(viridis::turbo(n = 10) %>% rev(),
                                  viridis::turbo(n = 10))) +
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
        axis.text.x = element_text(color = "black", size = 12, angle = -90, hjust = 0),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(face = "italic", size = 10))

ggsave(filename = "multicolinearidade.png", height = 12, width = 14)

ls(pattern = "Multicolinearidade_") %>%
  mget(envir = globalenv()) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(value = value %>% round(2)) %>%
  dplyr::rename("Spearman Correlation Index" = value) %>%
  dplyr::filter(`Spearman Correlation Index` < -0.7 |
                  `Spearman Correlation Index` > 0.7) %>%
  ggplot(aes(Var1, Var2, fill = `Spearman Correlation Index`, label = `Spearman Correlation Index`)) +
  geom_tile(color = "black", linewidth = 0.5) +
  geom_text(color = "black", size = 2) +
  coord_equal() +
  scale_fill_gradientn(breaks = seq(-1, 1, 0.2),
                       limits = c(-1, 1),
                       colours = c(viridis::turbo(n = 10) %>% rev(),
                                   viridis::turbo(n = 10))) +
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
        axis.text.x = element_text(color = "black", size = 12, angle = -90, hjust = 0),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 15),
        legend.position = "bottom",
        strip.text = element_text(face = "italic", size = 10))

ggsave(filename = "multicolinearidade2.png", height = 12, width = 14)

## Excluindo ----

### Pristimantis paulodutrai ----

# Manter: Uso, Elevação, Bio 18, Bio 16, Bio14, Bio 8, Bio 7, Bio 5

tabela_Pristimantis_paulodutrai_trat <- tabela_Pristimantis_paulodutrai %>%
  dplyr::select(6, 8, 9, 15, 17, 19, 21, 22)

tabela_Pristimantis_paulodutrai_trat

### Pristimantis ramagii ----

# Manter: Uso, elevação, Bio 19, Bio 17, Bio 15, Bio 7, Bio 4

tabela_Pristimantis_ramagii_trat <- tabela_Pristimantis_ramagii %>%
  dplyr::select(5, 8, 16, 18, 20:22)

tabela_Pristimantis_ramagii_trat

### Pristimantis vinhai ----

# Manter: Uso, elevação, Bio 19, Bio 15, Bio 8, Bio 5, Bio 3

tabela_Pristimantis_vinhai_trat <- tabela_Pristimantis_vinhai %>%
  dplyr::select(4, 6, 9, 16, 20:22)

tabela_Pristimantis_vinhai_trat

# Exportando as novas tabelas ----

lista_tabelas_multicol <- ls(pattern = "trat") %>%
  mget(envir = globalenv())

lista_tabelas_multicol

exportar_novas_tabelas <- function(x, y){

  x %>%
    writexl::write_xlsx(y)

}

purrr::walk2(lista_tabelas_multicol,
            paste0("tabela_",
            c("paulodutrai",
              "ramagii",
              "vinhai"),
            "_trat.xlsx"),
            exportar_novas_tabelas)

list.files(pattern = "_trat.xlsx")
