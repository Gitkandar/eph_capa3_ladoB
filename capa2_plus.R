# ARCHIVO: capa2_plus #####

## OBJETIVO: Crear el conjunto de variables a nivel individuo necesarias para procesar capa3

## ANTECEDENTES: Se toman de enfoque2 completo, excluyendo el código de original que figura como texto.

## MODIFICACIONES:
##                1. Las nuevas variables se guardan en datos.
##                2. Se reorganiza la información de manera que las funciones queden agrupadas entre si.
##                3. Se modificaron las funciones para que resuelva el nombrado de nuevas variables de forma automática
##                4. Las nuevas variables se crean por loops
                


# TABLA DE PONDERADORES ####

## Carga del dataframe que relaciona variables con ponderador distinto a [pondera]
ponderados <- read_excel("C:/oes/eph_capa3_ladoB/diccionario/tabla_ponder.xlsx")


# CONJUNTOS DE VARIABLES ####

## De Dummies ponderadas ----
dum <- c("sexo", "nivel_educ_obtenido2", "edad_categoria_intervalo", "tipo_empleo", "condicion_formalidad", "categoria_ocupacional2")
dum1 <- dum[2:length(dum)]
dum2 <- dum1[2:length(dum1)]
dum3 <- dum2[2:length(dum2)]
dum4 <- dum3[2:length(dum3)]
dum5 <- dum4[2:length(dum4)]

x1 <- c("sexo")
x2 <- c("nivel_educ_obtenido2")
x3 <- c("edad_categoria_intervalo")
x4 <- c("tipo_empleo")
x5 <- c("condicion_formalidad")
x6 <- c("categoria_ocupacional2")

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
corte <- data.frame(matrix(NA, nrow = 1, ncol = 8)) %>%
          rename(ini_dummies_puras = X1,
                 fin_dummies_puras = X2,
                 ini_dummies_cruzadas = X3,
                 fin_dummies_cruzadas = X4,
                 ini_ingresos = X5,
                 fin_ingresos = X6,
                 ini_ingresos_nolab = X7,
                 fin_ingresos_nolab = X8)


corte$ini_dummies_puras[1] <- length(datos) + 1

## Loop para dummies puras ponderadas ---- 
for (x in dum) { datos <- cruza_var(datos, x)} 

corte$fin_dummies_puras[1] <- length(datos)
corte$ini_dummies_cruzadas[1] <- length(datos) + 1

## Loops para dummies cruzadas ponderadas ----
for (y in dum1) {datos <- cruza_var(datos, x1, y)}                      
for (y in dum2) {datos <- cruza_var(datos, x2, y)}
for (y in dum3) {datos <- cruza_var(datos, x3, y)}
for (y in dum4) {datos <- cruza_var(datos, x4, y)}
for (y in dum5) {datos <- cruza_var(datos, x5, y)} 

corte$fin_dummies_cruzadas[1] <- length(datos)
corte$ini_ingresos[1] <- length(datos) + 1

## Loop para la totalidad de los ingresos seleccionados ---- 
for (x in ingr1) {
  for (y in ingr1_c) {
    datos <- cruza_ingr(datos, x, y)
  }
} 

corte$fin_ingresos[1] <- length(datos)
corte$ini_ingresos_nolab[1] <- length(datos) + 1

## Loop para la totalidad de los ingresos excluyendo los "no laborales" ----
for (x in ingr2) {
  for (y in ingr2_c) {
    datos <- cruza_ingr(datos, x, y)
  }
}  

corte$fin_ingresos_nolab[1] <- length(datos)

# ACTUALIZACION DE LA BASE DE PONDERADORES ####
ponderados <- update_pond(datos, ponderados)


# LIMPIEZA ####
rm(dum, dum1, dum2, dum3, dum4, dum5, ingr1, ingr1_c, ingr2, ingr2_c, x, y, x1, x2, x3, x4, x5, x6)


# PARA PROBAR LOS PUNTOS DE CORTE ####
names(datos[, c(333, 362, 363, 712, 713, 800, 801, 890)])
