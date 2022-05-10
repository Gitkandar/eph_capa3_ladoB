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
a <- 20
t <- 1
setwd(ruta_data)

## Archivo para invocar ####
file_name <- paste0("EPH20",a,"_T",t,".RData")

## Carga archivo ####
file <- paste0(ruta_data,file_name)
load(file) 




# Scripts ####
setwd(ruta_cod)
source("funciones.R", encoding = "UTF-8")
source("capa2_plus.R", encoding = "UTF-8")

source("capa3.R", encoding = "UTF-8")
