# Librerías ----
library(tables)
library(tidyr)
library(fastDummies)
library(readxl)
library(dplyr)
library(labelled)
library(purrr)
library(stringr)

# Rutas ----
ruta_gral <- "C:/oes/"
ruta_data <- paste0(ruta_gral, "capa2/")
ruta_results <- paste0(ruta_gral, "capa3/")
ruta_graf <- paste0(ruta_gral,"graf3/")
ruta_cod <- paste0(ruta_gral, "eph_capa3_ladoB/")

# Funciones----
setwd(ruta_cod)
source("funciones TE.R", encoding = "UTF-8")

# Comandos ----
anio <- c(16,17,17,17,17,18,18,18,18,19,19,19,19,20,20)
trimestre <- c(4,1,2,3,4,1,2,3,4,1,2,3,4,1,2)
periodo <- as.data.frame(cbind(anio,trimestre))

# Iteración ----
for (i in (1:nrow(periodo))) {
  ## Archivo para invocar ####
  file_name <- paste0("EPH20", periodo$anio[i], "_T", periodo$trimestre[i],".RData")
  exit_name <- paste0("Tasas_20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
  ## Carga archivo ####
  file <- paste0(ruta_data, file_name)
  load(file)
  setwd(ruta_cod)
  source("tasas_estimadores.R", encoding = "UTF-8")
  # Unifico períodos ----
  if (i == 1) {
    tasasunif = tasas } 
  else {tasasunif <- rbind(tasasunif, tasas)}
  setwd(ruta_results)
  save(datos, file = file_name)
  save(tasas, file = exit_name)
}

save(tasasunif, file = "tasas_unificado.RData")

rm(variables, variablesS, G, cat_ocupacional, educacion,  etario,  fabs,
   formalidad,  Gini,  sexo,  tipo_empleo,  tasas,  tasasunif,  periodo,
   anio,  exit_name, file_name, file)