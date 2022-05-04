# ARCHIVO: master3 #####

 ## OBJETIVO: Coordinar la creación de variables para capa2_plus y capa3.
 ##           capa2_plus implica que se necesita crear un conjunto de variables a nivel individuo que no van a desecharse
 ##           capa3 es la capa que calcula indicadores a nivel aglomerado
 
 ## ANTECEDENTES: Este archivo solo toma:
 ##               - La invocación de capa2
 ##               - Las librerías invocadas
 ##               - La compilación de bases
 ##               - El guardado de capa3          



# Librerías ####
library(tables)
library(dplyr)
library(tidyr)
library(labelled)
library(stringr)
library(fastDummies)
library(readxl)
library(stringr)


# Recupero de archivos ####

## Estructura de archivos ####
ruta_gral <- "C:/oes/eph_rdos/"
ruta_data <- paste0(ruta_gral, "capa2/")
ruta_results <- paste0(ruta_gral, "capa3_ladoB/")
ruta_graf <- paste0(ruta_gral,"graf3/")
ruta_cod <- "C:/oes/eph_capa3_ladoB/"

## Comandos ####
anio <- c(16,17,17,17,17,18,18,18,18,19,19,19,19,20,20)
trimestre <- c(4,1,2,3,4,1,2,3,4,1,2,3,4,1,2)
periodo <- as.data.frame(cbind(anio,trimestre))

## Carga archivo ####
for (i in 1:nrow(periodo)){
  ## Archivo para invocar ####
  file_name <- paste0("EPH20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
  file <- paste0(ruta_data,file_name)
  exit_name <- paste0("Capa3_20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
  load(file) 
  
  # Scripts ####
  setwd(ruta_cod)
  source("funciones.R", encoding = "UTF-8")
  source("capa2_plus.R", encoding = "UTF-8")
  
  source("test_capa3.R", encoding = "UTF-8")
  
  source("funciones TE.R", encoding = "UTF-8")
  source("tasas y estimadores.R", encoding = "UTF-8")

}

# Guarda el archivo unificado ----
setwd(ruta_results)
save(salida, file = "capa3_unificado.RData")
save(tasasunif, file = "tasas_unificado.RData")