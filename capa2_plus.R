# ARCHIVO: capa2_plus #####

## OBJETIVO: Crear el conjunto de variables a nivel individuo necesarias para procesar capa3

## ANTECEDENTES: Se toman de enfoque2 completo, excluyendo el código de original que figura como texto.

## MODIFICACIONES:
##                1. Las nuevas variables se guardan en datos.
##                2. Se reorganiza la información de manera que las funciones queden agrupadas entre si.
##                3. Se modificaron las funciones para que resuelva el nombrado de nuevas variables de forma automática
##                4. Las nuevas variables se crean por loops
                
library(dplyr)
library(readxl)
library(stringr)


# TABLA DE PONDERADORES ####

## Carga del dataframe que relaciona variables con ponderador distinto a [pondera]
ponderados <- read_excel("C:/oes/eph_capa3_ladoB/diccionario/tabla_ponder.xlsx")


# CONJUNTOS DE VARIABLES ####

## De Dummies ponderadas ----
dum <- c("sexo", "nivel_educ_obtenido2", "edad_categoria_intervalo", 
         "tipo_empleo", "condicion_formalidad", "categoria_ocupacional2")

## De Ingresos ----
###  Variables a ser cruzadas
ingr1 <- c("ingreso_ocupacion_principal", "ingreso_real_ocupacion_principal", 
           "ingreso_otras_ocupaciones", "ingreso_real_otras_ocupaciones",
           "ingreso_total_individual", "ingreso_real_total_individual", 
           "ingreso_no_laborable", "ingreso_real_no_laborable")
ingr2 <- ingr1[1:6]

###  Variables de cruce
ingr1_c <- dum[1:2]
ingr2_c <- dum[3:5]


# PROCESADO ####
## Indicador de posición (corte) ----
corte <- tibble(
  ini_dummies_puras = ncol(datos) + 1,
  fin_dummies_puras = NA,
  ini_dummies_cruzadas = NA,
  fin_dummies_cruzadas = NA,
  ini_ingresos = NA,
  fin_ingresos = NA,
  ini_ingresos_nolab = NA,
  fin_ingresos_nolab = NA
)

## Loop para dummies puras ponderadas ---- 
for (x in dum) { datos <- cruza_var(datos, x)} 

corte$fin_dummies_puras[1] <- ncol(datos)
corte$ini_dummies_cruzadas[1] <- ncol(datos) + 1

## Loops para dummies cruzadas ponderadas ----
for (j in 1:(length(dum) - 1)) {
  for (y in dum[-(1:j)]) {
    datos <- cruza_var(datos, dum[j], y)
  }
}

corte$fin_dummies_cruzadas[1] <- ncol(datos)
corte$ini_ingresos[1] <- ncol(datos) + 1

## Loop para la totalidad de los ingresos seleccionados ---- 
for (x in ingr1) {
  for (y in ingr1_c) {
    datos <- cruza_ingr(datos, x, y)
  }
} 

corte$fin_ingresos[1] <- ncol(datos)
corte$ini_ingresos_nolab[1] <- ncol(datos) + 1

## Loop para la totalidad de los ingresos excluyendo los "no laborales" ----
for (x in ingr2) {
  for (y in ingr2_c) {
    datos <- cruza_ingr(datos, x, y)
  }
}  

corte$fin_ingresos_nolab[1] <- ncol(datos)

# ACTUALIZACION DE LA BASE DE PONDERADORES ####
ponderados <- update_pond(datos, ponderados)


# LIMPIEZA ####
rm(dum, dum1, dum2, dum3, dum4, dum5, ingr1, ingr1_c, ingr2, ingr2_c, x, y, x1, x2, x3, x4, x5, x6)


# PARA PROBAR LOS PUNTOS DE CORTE ####
names(datos[, c(333, 362, 363, 712, 713, 800, 801, 890)])
