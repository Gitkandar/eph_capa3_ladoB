# Librerías ----
library(tables)
library(dplyr)
library(tidyr)
library(labelled)
library(purrr)
library(stringr)
library(mltools)

# Carpetas ----
ruta_data <- "C:/oes/capa2/"
setwd(ruta_data)

# Comandos ----
anio <- c(16, rep(17,4), rep(18,4), rep(19,4), rep(20,4), rep(21,2))
trimestre <- c(4, 1:4, 1:4, 1:4, 1:4, 1:2)
periodo <- tibble(anio, trimestre)
# de prueba uso T1 de 2020
i <- 14

# Código ----
file_name <-
  paste0("EPH20",periodo$anio[i], "_T",periodo$trimestre[i],".RData")
exit_name <-
  paste0("Capa3_20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
file <- paste0(ruta_data, file_name)
load(file)

# Cálculo de media, mediana y n por decil para ingreso_capita_familiar ----
deciles <- function(datos, i, p){
  data <- data.frame(rep(i, times = p, na.rm = TRUE))
  colnames(data) <- "ingreso"
  data <- data %>%
    mutate(decil = ntile(ingreso,10)) %>%
    drop_na (decil)
  data <- data %>%
    group_by(decil) %>%
    summarise(
      min = min(ingreso, na.rm = TRUE),
      m = mean(ingreso, na.rm = TRUE),
      md = median(ingreso, na.rm = TRUE),
      max = max(ingreso, na.rm = TRUE),
      n = n())
  }

d_ipc <- deciles(datos, datos$ingreso_capita_familiar, datos$pondih)
d_iop <- deciles(datos, datos$ingreso_ocupacion_principal, datos$pondiio)

# otras medidas a calcular: 
# razon entre decil 1 y 10 de:
ingreso_ocupacion_principal
ingreso_real_ocupacion_principal