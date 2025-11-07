# pacotes ----

library(geobr)

library(tidyverse)

library(terra)

library(tidyterra)

library(readxl)

library(dismo)

library(sdm)

library(usdm)

library(rJava)

library(cowplot)

library(ggview)

# Dados ----

## Áreas ----

### Brasil ----

#### Importando ----

br <- geobr::read_state(year = 2019)

#### Visualizando ----

ggplot() +
  geom_sf(data = br, color = "black")

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

## Dados ambientais ----

### Importando ----

bio <- terra::rast("bio.tif")

### Visualizando ----

bio

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

## Dados de ocorrência ----

### Importando ----

lista_para_importar_registros <- list.files(pattern = "registro_Pristimantis_")

lista_para_importar_registros

lista_para_importar_registros

importar_registros <- function(x, y){

  registro <- readxl::read_xlsx(x)

  assign(y,
         registro,
         envir = globalenv())

}

purrr::walk2(lista_para_importar_registros,
             paste0("registro_",
                    lista_para_importar_registros |>
                      stringr::str_remove_all("registro_Pristimantis_|.xlsx")),
             importar_registros)

### Visualizando ----

ls(pattern = "registro_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  ggplot() +
  geom_sf(data = estados, color = "black", fill = "gold") +
  geom_sf(data = ma,
          color = "darkgreen",
          fill = "transparent",
          linewidth = 0.5) +
  geom_point(aes(Longitude, Latitude)) +
  facet_wrap(~species)

## Valores extraídos ----

lista_para_importar_valores <- list.files(pattern = "_trat.xlsx")

lista_para_importar_valores

importar_valores <- function(x, y){

  tabela <- readxl::read_xlsx(x)

  assign(y,
         tabela,
         envir = globalenv())

}

purrr::walk2(lista_para_importar_valores,
             lista_para_importar_valores |> stringr::str_remove("_trat.xlsx"),
             importar_valores)

ls(pattern = "tabela_") |>
  mget(envir = globalenv())

# Modelagem ----

## Pristimantis paulodutrai ----

### Tratando as variáveis ambientais ----

names(bio) <- names(bio) |> stringr::str_remove(" ")

nomes_var <- tabela_paulodutrai |> names() |> stringr::str_remove(" ")

nomes_var

bio_paulodutrai <- bio[[nomes_var]] |>
  terra::crop(ma) |>
  terra::mask(ma)

bio_paulodutrai

ggplot() +
  tidyterra::geom_spatraster(data = bio_paulodutrai) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Criando o objeto sdmData ----

registro_paulodutrai_sf <- registro_paulodutrai |>
  dplyr::mutate(sp = 1) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = bio_paulodutrai |> terra::crs()) |>
  terra::vect()

sdm_data_paulodutrai <- sdm::sdmData(sp ~ .,
                         train = registro_paulodutrai_sf,
                         predictors = bio_paulodutrai,
                         bg = list(method = "gRandom", n = 1000))

sdm_data_paulodutrai

### Ajustando o modelo ----

sdm::getmethodNames()

sdm_modelo_paulodutrai <- sdm::sdm(sp ~ .,
                                   data = sdm_data_paulodutrai,
                                   methods = c("rf",
                                               "glmpoly",
                                               "maxNet"),
                                   replication = "sub",
                                   test.percent = 30,
                                   n = 10)

sdm_modelo_paulodutrai

sdm_modelo_paulodutrai |>
  sdm::write.sdm("modelo_pauodutrai.sdm", overwrite = TRUE)

### Predição ----

sdm_modelo_paulodutrai <- sdm::read.sdm("modelo_pauodutrai.sdm")

sdm_modelo_paulodutrai

predict_paulodutrai <- terra::predict(sdm_modelo_paulodutrai,
                                      bio_paulodutrai,
                                      overwrite = TRUE)

predict_paulodutrai

plotar_pred <- function(x){

  plots <- ggplot() +
    tidyterra::geom_spatraster(data = predict_paulodutrai[[x]]) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(option = "turbo",
                         na.value = "transparent")

  print(plots)

}

purrr::walk(1:length(names(predict_paulodutrai)),
            plotar_pred)

### Ensemble dos modelos ----

ensemble_paulodutrai <- sdm_modelo_paulodutrai |>
  sdm::ensemble(newdata = predict_paulodutrai,
                setting = list(method = "weighted",
                               stat = "AUC"))

ensemble_paulodutrai

ggplot() +
  tidyterra::geom_spatraster(data = ensemble_paulodutrai) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Pristimantis ramagii ----

### Trtando as variáveis ambientais ----

nomes_var <- tabela_ramagii |> names() |> stringr::str_remove(" ")

nomes_var

bio_ramagii <- bio[[nomes_var]]

bio_ramagii

ggplot() +
  tidyterra::geom_spatraster(data = bio_ramagii) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Criando o objeto sdmData ----

registro_ramagii_sf <- registro_ramagii |>
  dplyr::mutate(sp = 1) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = bio_ramagii |> terra::crs()) |>
  terra::vect()

sdm_data_ramagii <- sdm::sdmData(sp ~ .,
                                     train = registro_ramagii_sf,
                                     predictors = bio_ramagii,
                                     bg = list(method = "gRandom", n = 1000))

sdm_data_ramagii

### Ajustando o modelo ----

sdm::getmethodNames()

sdm_modelo_ramagii <- sdm::sdm(sp ~ .,
                                   data = sdm_data_ramagii,
                                   methods = c("rf",
                                               "glmPoly",
                                               "maxent"),
                                   replication = "sub",
                                   test.percent = 30,
                                   n = 10)

sdm_modelo_ramagii

sdm_modelo_ramagii |>
  sdm::write.sdm("modelo_ramagii.sdm", overwrite = TRUE)

### Predição ----

sdm_modelo_ramagii <- sdm::read.sdm("modelo_ramagii.sdm")

sdm_modelo_ramagii

predict_ramagii <- terra::predict(sdm_modelo_ramagii,
                                      bio_ramagii,
                                      overwrite = TRUE)

predict_ramagii

plotar_pred <- function(x){

  plots <- ggplot() +
    tidyterra::geom_spatraster(data = predict_ramagii[[x]]) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(option = "turbo",
                         na.value = "transparent")

  print(plots)

}

purrr::walk(1:length(names(predict_ramagii)),
            plotar_pred)

### Ensemble dos modelos ----

ensemble_ramagii <- sdm_modelo_ramagii |>
  sdm::ensemble(newdata = predict_ramagii,
                setting = list(method = "weighted",
                               stat = "AUC"))

ensemble_ramagii

ggplot() +
  tidyterra::geom_spatraster(data = ensemble_ramagii) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Pristimantis vinhai ----

### Trtando as variáveis ambientais ----

nomes_var <- tabela_vinhai |> names() |> stringr::str_remove(" ")

nomes_var

bio_vinhai <- bio[[nomes_var]]

bio_vinhai

ggplot() +
  tidyterra::geom_spatraster(data = bio_vinhai) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

### Criando o objeto sdmData ----

registro_vinhai_sf <- registro_vinhai |>
  dplyr::mutate(sp = 1) |>
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = bio_vinhai |> terra::crs()) |>
  terra::vect()

sdm_data_vinhai <- sdm::sdmData(sp ~ .,
                                 train = registro_vinhai_sf,
                                 predictors = bio_vinhai,
                                 bg = list(method = "gRandom", n = 1000))

sdm_data_vinhai

### Ajustando o modelo ----

sdm::getmethodNames()

sdm_modelo_vinhai <- sdm::sdm(sp ~ .,
                               data = sdm_data_vinhai,
                               methods = c("rf",
                                           "glmpoly",
                                           "maxent"),
                               replication = "sub",
                               test.percent = 30,
                               n = 10)

sdm_modelo_vinhai

sdm_modelo_vinhai |>
  sdm::write.sdm("modelo_vinhai.sdm", overwrite = TRUE)

### Predição ----

sdm_modelo_vinhai <- sdm::read.sdm("modelo_vinhai.sdm")

predict_vinhai <- terra::predict(sdm_modelo_vinhai,
                                  bio_vinhai,
                                  overwrite = TRUE)

predict_vinhai

plotar_pred <- function(x){

  plots <- ggplot() +
    tidyterra::geom_spatraster(data = predict_vinhai[[x]]) +
    facet_wrap(~lyr) +
    scale_fill_viridis_c(option = "turbo",
                         na.value = "transparent")

  print(plots)

}

purrr::walk(1:length(names(predict_vinhai)),
            plotar_pred)

### Ensemble dos modelos ----

ensemble_vinhai <- sdm_modelo_vinhai |>
  sdm::ensemble(newdata = predict_vinhai,
                setting = list(method = "weighted",
                               stat = "AUC"))

ensemble_vinhai

ggplot() +
  tidyterra::geom_spatraster(data = ensemble_vinhai) +
  facet_wrap(~lyr) +
  scale_fill_viridis_c(option = "turbo",
                       na.value = "transparent")

## Mapa unido ----

### Unindo os rasters ----

ensemble <- ls(pattern = "ensemble_") |>
  mget(envir = globalenv()) |>
  terra::rast()

names(ensemble) <- paste0("Pristimantis ",
                          ls(pattern = "ensemble_") |>
                            stringr::str_remove("ensemble_"))

names(ensemble)

ensemble

### Insert map ----

insert_map <- ggplot() +
  geom_sf(data = br, color = "black", linewidth = 0.5) +
  geom_sf(data = estados, color = "black", fill = "white", linewidth = 0.5) +
  geom_rect(aes(xmin = -48.76,
                xmax = -32.38,
                ymin = -18.35,
                ymax = -1.05),
            color = "darkred",
            fill = "red",
            alpha = 0.5) +
  theme_void()

insert_map

### Mapa principal ----

mapa <- ggplot() +
  geom_sf(data = br, color = "black", linewidth = 0.5) +
  geom_sf(data = estados, color = "black", fill = "white", linewidth = 0.5) +
  tidyterra::geom_spatraster(data = ensemble) +
  geom_sf(data = ma, color = "black", fill = "transparent", linewidth = 0.5) +
  facet_wrap(~lyr, ncol = 2) +
  scale_fill_viridis_c(na.value = "transparent",
                       limits = c(0, 1),
                       breaks = seq(0, 1, 0.2)) +
  coord_sf(xlim = c(-48.76, -32.38),
           ylim = c(-18.35, -1.05),
           label_graticule = "ESW") +
  guides(fill = guide_colourbar(title.hjust = 0.5,
                                barwidth = 2.5,
                                barheight = 25,
                                framelinewidth = 1,
                                frame.colour = "black",
                                ticks.colour = "black",
                                ticks.linewidth = 0.5)) +
  labs(fill = "Environmental suitability") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 17.5, face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))

mapa

### Unindo os mapas ----

mapa |>
  cowplot::ggdraw() +
  cowplot::draw_plot(insert_map,
                     x = 0.4,
                     y = 0.1,
                     width = 0.35,
                     height = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_modelos_ensemble.png", height = 10, width = 12)

# Avaliando os modelos ----

## Lista de modelos ----

lista_modelos <- ls(pattern = "sdm_modelo_") |>
  mget(envir = globalenv())

lista_modelos

## Criando as avaliações ----

avaliar_modelos <- function(x, y){

  message(paste0("avaliação para ", y))

  avaliacao <- x |>
    sdm::getEvaluation(stat = c("AUC", "TSS"),
                       opt = 2) |>
    dplyr::summarise(`Mean AUC` = AUC |> mean(),
                     `SD AUC` = AUC |> sd(),
                     `Mean TSS` = TSS |> mean(),
                     `SD TSS` = TSS |> sd())

  print(avaliacao)

  message("")

}

purrr::walk2(lista_modelos,
             paste0("Pristimantis ",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             avaliar_modelos,
            .progress = FALSE)

## AUC ----

auc_modelos <- function(x, y){

  message(paste0("avaliação para ", y))

  avaliacao <- x |>
    sdm::roc()

  print(avaliacao)

  message(" ")

}

purrr::walk2(lista_modelos,
             paste0("Pristimantis ",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             auc_modelos)

## Lista de ensembles ----

lista_ensemble <- ls(pattern = "ensemble_") |>
  mget(envir = globalenv())

lista_ensemble

## Histograma de valores ----

histo_modelos <- function(x, y){

  histograma <- lista_ensemble[2] |>
    as.data.frame(xy = TRUE) |>
    tidyr::drop_na() |>
    dplyr::mutate(Species = y) |>
    dplyr::rename("Environmental suitability" = ensemble_weighted)

  assign(paste0("histo_df_", y),
         histograma,
         envir = globalenv())

}

purrr::walk2(lista_modelos,
             paste0("Pristimantis ",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             histo_modelos)

ls(pattern = "histo_df_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows() |>
  ggplot(aes(`Environmental suitability`)) +
  geom_histogram(color = "black") +
  facet_wrap(~ Species,
             ncol = 2) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 17.5,
                                  face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))



ggsave(filename = "histo_modelos.png", height = 10, width = 12)

## Variáveis mais importantes ----

var_imp_modelos <- function(x, y){

  df_varimp <- x |>
    sdm::getVarImp() |>
    plot()

  df_varimp <- df_varimp$data |>
    dplyr::mutate(variables = dplyr::case_when(variables == "Elevação" ~ "Elevation",
                                               variables == "Uso" ~ "Land use",
                                               .default = variables),
                  variables = variables |>
                    forcats::fct_relevel(rownames(df_varimp$data)),
                  Species = y) |>
    dplyr::rename("Variable" = variables,
                  "Correlation" = corTest)

  assign(paste0("varimp_", y),
         df_varimp,
         envir = globalenv())

}

purrr::walk2(lista_modelos,
             paste0("Pristimantis ",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             var_imp_modelos)

var_imp_df <- ls(pattern = "varimp_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

var_imp_df

var_imp_df|>
  dplyr::mutate(Variable = Variable |> forcats::fct_relevel(c(paste0("Bio",
                                                                     c(3:5,
                                                                       7,
                                                                       8,
                                                                       14:19)),
                                                              "Elevation",
                                                              "Land Use"))) |>
  ggplot(aes(Correlation, Variable)) +
  geom_col(color = "black") +
  facet_wrap(~ Species, ncol = 2, scale = "free_y") +
  scale_x_continuous(breaks = seq(0, 0.65, 0.05)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 17.5,
                                  face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))

ggsave(filename = "var_imp_modelos.png", height = 10, width = 12)

'## Curvas de resposta ----

var_curva_modelos <- function(x, y){

  rcurva <- x |>
  sdm::rcurve()

  rcurva <- rcurva$data |>
    dplyr::mutate(Species = y)

  assign(paste0("resposta_curva_", y),
         rcurva,
         envir = globalenv())

}

purrr::walk2(lista_modelos,
             paste0("Pristimantis ",
                    c("paulodutrai",
                      "ramagii",
                      "vinhai")),
             var_curva_modelos)

df_resposta_curva <- ls(pattern = "resposta_curva_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

df_resposta_curva

r_c_paulodutrai <- df_resposta_curva |>
  dplyr::mutate(variable = dplyr::case_when(variable == "Elevação" ~ "Elevation",
                                             variable == "Uso" ~ "Land use",
                                             .default = variable)) |>
  dplyr::rename("Variable" = variable,
                "Variable value" = Value,
                "Model response" = Response) |>
  dplyr::mutate(Variable = Variable |> forcats::fct_relevel(c(paste0("Bio",
                                                                     c(3:5,
                                                                       7,
                                                                       8,
                                                                       14:19)),
                                                              "Elevation",
                                                              "Land Use"))) |>
  dplyr::filter(Species == "Pristimantis paulodutrai") |>
  ggplot(aes(`Variable value`, `Model response`)) +
  geom_ribbon(aes(x = `Variable value`, ymin = lower, ymax = upper),
              alpha = 0.3) +
  geom_line() +
  facet_grid(Species ~ Variable, scales = "free") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        strip.text = element_text(color = "black", size = 17.5),
        strip.text.y.right = element_text(color = "black", size = 15,
                                          face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))

r_c_paulodutrai

r_c_ramagii <- df_resposta_curva |>
  dplyr::mutate(variable = dplyr::case_when(variable == "Elevação" ~ "Elevation",
                                            variable == "Uso" ~ "Land use",
                                            .default = variable)) |>
  dplyr::rename("Variable" = variable,
                "Variable value" = Value,
                "Model response" = Response) |>
  dplyr::mutate(Variable = Variable |> forcats::fct_relevel(c(paste0("Bio",
                                                                     c(3:5,
                                                                       7,
                                                                       8,
                                                                       14:19)),
                                                              "Elevation",
                                                              "Land Use"))) |>
  dplyr::filter(Species == "Pristimantis ramagii") |>
  ggplot(aes(`Variable value`, `Model response`)) +
  geom_ribbon(aes(x = `Variable value`, ymin = lower, ymax = upper),
              alpha = 0.3) +
  geom_line() +
  facet_grid(Species ~ Variable, scales = "free") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        axis.title.x = element_blank(),
        strip.text = element_text(color = "black", size = 17.5),
        strip.text.y.right = element_text(color = "black", size = 15,
                                          face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))

r_c_ramagii

r_c_vinhai <- df_resposta_curva |>
  dplyr::mutate(variable = dplyr::case_when(variable == "Elevação" ~ "Elevation",
                                            variable == "Uso" ~ "Land use",
                                            .default = variable)) |>
  dplyr::rename("Variable" = variable,
                "Variable value" = Value,
                "Model response" = Response) |>
  dplyr::mutate(Variable = Variable |> forcats::fct_relevel(c(paste0("Bio",
                                                                     c(3:5,
                                                                       7,
                                                                       8,
                                                                       14:19)),
                                                              "Elevation",
                                                              "Land Use"))) |>
  dplyr::filter(Species == "Pristimantis vinhai") |>
  ggplot(aes(`Variable value`, `Model response`)) +
  geom_ribbon(aes(x = `Variable value`, ymin = lower, ymax = upper),
              alpha = 0.3) +
  geom_line() +
  facet_grid(Species ~ Variable, scales = "free") +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1)) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 17.5),
        strip.text.y.right = element_text(color = "black", size = 15,
                                          face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5))

r_c_vinhai

(r_c_paulodutrai / r_c_ramagii / r_c_vinhai)

ggsave(filename = "curva_resposta_modelos.png", height = 10, width = 12)

# Similaridade de Nicho ----

## Testando múltiplas combinações ----

combos_sobreposicao <- lista_ensemble |> names() |>
  combn(2,
        simplify = FALSE)

combos_sobreposicao

similaridade_nicho <- function(x){

  sp1 <- combos_sobreposicao[[x]][1]

  sp2 <- combos_sobreposicao[[x]][2]

  message(paste0("Comparação entre: ", sp1, " e ", sp2))

  nicho_similaridade <- sdm::nicheSimilarity(lista_ensemble[[sp1]],
                                             lista_ensemble[[sp2]])[c(1, 7)]

  print(nicho_similaridade)

  message("")

}

purrr::walk(1:length(combos_sobreposicao),
            similaridade_nicho)

## Mapa ----

### Criando os rasters ----

criar_pa <- function(x, y){

  threshold_modelo <- sdm::threshold(lista_modelos[[x]],
                                     id = "ensemble",
                                     opt = 1)

  pa_sf <- sdm::pa(lista_ensemble[[x]],
                       lista_modelos[[x]],
                       id = "ensemble",
                       z = threshold_modelo) |>
    terra::as.polygons() |>
    sf::st_as_sf() |>
    dplyr::filter(ensemble_weighted == 1) |>
    dplyr::mutate(sps = paste0("Pristimantis ", y))

  assign(paste0("sf_pa_", y),
         pa_sf,
         envir = globalenv())

}

purrr::walk2(1:length(lista_modelos),
            lista_modelos |>
                      names() |>
                      stringr::str_remove("sdm_modelo_"),
             criar_pa)

### Criando o mapa ----

nicho_sf <- ls(pattern = "sf_pa_") |>
  mget(envir = globalenv()) |>
  dplyr::bind_rows()

nicho_sf

gg_pa <- ggplot() +
  geom_sf(data = br, color = "black", linewidth = 0.5) +
  geom_sf(data = estados, color = "black", fill = "white", linewidth = 0.5) +
  geom_sf(data = nicho_sf, aes(fill = sps, color = sps),
          alpha = 0.5, linewidth = 0.5) +
  geom_sf(data = ma, color = "darkgreen", fill = "transparent", linewidth = 0.5) +
  scale_fill_manual(values = c("#FB9A06FF",
                               "#781C6DFF",
                               "#000004FF")) +
  scale_color_manual(values = c("#FB9A06FF",
                                "#781C6DFF",
                                "#000004FF")) +
  facet_wrap(~sps, ncol = 2) +
  coord_sf(xlim = c(-48.76, -32.38),
           ylim = c(-18.35, -1.05),
           label_graticule = "ESW") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12),
        strip.text = element_text(color = "black", size = 17.5, face = "italic"),
        legend.text = element_text(color = "black", size = 12),
        legend.title = element_text(color = "black", size = 17.5),
        legend.position = "none")

gg_pa

### Unindo os mapas ----

gg_pa |>
  cowplot::ggdraw() +
  cowplot::draw_plot(insert_map,
                     x = 0.5,
                     y = 0.1,
                     width = 0.35,
                     height = 0.35) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_modelos_sobreposição.png", height = 10, width = 12)

