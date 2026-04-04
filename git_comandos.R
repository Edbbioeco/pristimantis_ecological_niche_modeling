
# Pacote ----

library(gert)

library(tidyverse)

# Selecionando os arquivos ----

gert::git_status() |>
  as.data.frame() |>
  dplyr::filter(file |> stringr::str_detect(".R$"))

# Selecionando o arquivo ----

gert::git_add(files = "git_comandos.R") |>
  as.data.frame()

# Commitando ----

gert::git_commit("Script para os comandos de git")

# Pushando ----

gert::git_push(remote = "origin", force = TRUE)

# Pullando ----

gert::git_pull(remote = "origin")

# Resetando ----

gert::git_reset_mixed() |>
  as.data.frame()

gert::git_reset_soft("HEAD^")

# Removendo arquivos ----

## Selecionando os arquivos para remover ----

gert::git_rm(list.files(pattern = ".tif$|.png$|.jpeg$|.shp$|.dbf$|.shx$|.prj$|.txt$|.csv$|.xlsx$")) |>
  as.data.frame()

## Commitando ----

gert::git_commit("Remover")

## Pushando ----

gert::git_push(remote = "origin", force = TRUE)

## Pullando ----

gert::git_pull(remote = "origin")
