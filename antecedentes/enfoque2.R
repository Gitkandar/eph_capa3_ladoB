# DEFINICIONES INICIALES ####

  # Este código busca simplificar enfoque1, utilizando la estrategia de construir funciones que luego se aplican a las variables.
  # En este scrip no hay cálculo de medias y promedios como en enfoque1 (después de <<RESUMEN>>)
  
  # Este scrip tiene tres partes:
  #   - La 1era que abarca desde el inicio hasta <<FUNCION: dummies...>> escrita por Ger, en donde se modifican las variables de ingresos, 
  #   usando funciones.
  #   - La 2da que abarca desde <<FUNCION: dummies...>> hasta lo que quedó como nota, escrita por Diego, que tiene una estrategia interesante
  #   para crear dummies basadas en las variables originales, y sobre eso aplicar los pondera. Esta parte del código, hace lo mismo que la 
  #   siguiente (la 3ra parte).
  #   - La 3ra parte, que es todo lo que figura como nota al final, escrita por Lu, que es la aplicación de la lógica de funciones a las
  #   variables que no son de ingresos, y que Diego reemplaza
  
  # ESTRATEGIAS UTILIZADAS:
  
  # 1. FUNCIONES PROPIAS: muchas veces los cruces entre variables, siguen un mismo patrón. Para eso se crean funciones que al interior 
  # contienen clausulas ifelse (como en enfoque1, solo que dentro de la función) para aplicar a las variables.
  
  # 2. TRANSMUTE_AT: Se usa para crear nuevas bases de datos donde se almacenan las nuevas variables creadas.
  # Se pensó así para que el re-nombramiento de las variables fuera más fácil de realizar. La desventaja es que se crean muchas bases
  
  # 3. FUNCION PROPIA PARA VARIABLES DUMMIES PONDERADAS: Este es el código de Diego. Trabaja en 4 partes.
  
  #   3.a. PULL: Es una simplificación de algo así como datos$x. Es decir: pull(x) = datos$x
  
  #   3.b. FASTDUMMIES::DUMMY_COLS: Crea una matriz de dummies basadas en las variables que se indiquen.
  #   Para ver un ejemplo comcreto, ver el scrip: ej_dummie
  
  # COSAS A REVISAR: nota linea 503  


# Librerías ####
library(tables)
library(dplyr)
library(tidyr)
library(labelled)
library(stringr)
library(fastDummies)

# Estructura de archivos ####
ruta_gral <- "C:/oes/eph_rdos/"
ruta_data <- paste0(ruta_gral, "capa2/")
ruta_results <- paste0(ruta_gral, "capa3/")
ruta_graf <- paste0(ruta_gral,"graf3/")
ruta_cod <- "C:/oes/eph_capa3/"

# Comandos ####
a <- 20
t <- 1
decil <- 1:10
setwd(ruta_data)

# Archivo para invocar ####
file_name <- paste0("EPH20",a,"_T",t,".RData")

# Carga archivo ####
file <- paste0(ruta_data,file_name)
load(file)
#load("C:/Users/diego/Documents/OES/EPHCapa3_Diego/capa2/EPH2020_T3.RData")


# FUNCIONES APLICADAS A INGRESOS ####
isexo <- function(x, y = "Mujeres") {
  if (y == "Mujeres") {
  ifelse(
    test = x > 0 & datos$sexo == "Mujeres",
    yes = x, no = NA)}
  
  else if (y == "Varones") {
    ifelse(
      test = x > 0 & datos$sexo == "Varones",
      yes = x, no = NA)}
}

ieducacion <- function(x, y = "Sin instrucción") {
  if (y == "Sin instrucción") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Sin instrucción",
      yes = x, no = NA)}
  
  else if (y == "Primaria Incompleta") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Incompleta",
      yes = x, no = NA)}
  
  else if (y == "Primaria Completa") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Completa",
      yes = x, no = NA)}
  
  else if (y == "Secundaria Incompleta") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Secundaria Incompleta",
      yes = x, no = NA)}
  
  else if (y == "Secundaria Completa") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Secundaria Completa",
      yes = x, no = NA)}
  
  else if (y == "Terciario Incompleto") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Incompleto",
      yes = x, no = NA)}
  
  else if (y == "Terciario Completo") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Completo",
      yes = x, no = NA)}
  
  else if (y == "Universitaria Incompleta") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Universitaria Incompleta",
      yes = x, no = NA)}
  
  else if (y == "Universitaria Completa") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Universitaria Completa",
      yes = x, no = NA)}
}

iformalidad <- function(x, y = "Formal") {
  if (y == "Formal") {
    ifelse(
      test = x > 0 & datos$condicion_formalidad == "Formal",
      yes = x, no = NA)}
  
  else if (y == "No formal") {
    ifelse(
      test = x > 0 & datos$condicion_formalidad == "No formal",
      yes = x, no = NA)}
}

itipo_empleo <- function(x, y = "Privado") {
  if (y == "Privado") {
    ifelse(
      test = x > 0 & datos$tipo_empleo == "Privado",
      yes = x, no = NA)}
  
  else if (y == "Público") {
    ifelse(
      test = x > 0 & datos$tipo_empleo == "Público",
      yes = x, no = NA)}
  
  else if (y == "Otro tipo") {
    ifelse(
      test = x > 0 & datos$tipo_empleo == "Otro tipo",
      yes = x, no = NA)}
}

ietario <- function(x, y = "No corresponde") {
  if (y == "No corresponde") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "0-9",
      yes = x, no = NA)}
  
  else if (y == "Menor") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "10-13",
      yes = x, no = NA)}
  
  else if (y == "Joven") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "14-18",
      yes = x, no = NA)}
  
  else if (y == "Junior") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "19-25",
      yes = x, no = NA)}
  
  else if (y == "Semi senior") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "26-35",
      yes = x, no = NA)}
  
  else if (y == "Senior") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "36-45",
      yes = x, no = NA)}
  
  else if (y == "Especialista I") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "46-55",
      yes = x, no = NA)}
  
  else if (y == "Especialista II") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "56-65",
      yes = x, no = NA)}
  
  else if (y == "En edad jubilatoria") {
    ifelse(
      test = x > 0 & datos$edad_categoria_intervalo == "66+",
      yes = x, no = NA)}
}


# INGRESOS ####

## Conjuntos de variables ----

ingr1 <- c("ingreso_ocupacion_principal", 
           "ingreso_real_ocupacion_principal",
           
           "ingreso_otras_ocupaciones",
           "ingreso_real_otras_ocupaciones",
           
           "ingreso_total_individual",
           "ingreso_real_total_individual",
           
           "ingreso_no_laborable",
           "ingreso_real_no_laborable")

ingr2 <- ingr1[1:6]


## CRUCE CON SEXO ----
## DESCRIPCION DE ESTRATEGIA
### transmute: crea nuevas variables y solo deja esas. Por ese motivo, se crea un data frame nuevo (con otro nombre)
### Aca se pide: 
###             1. Usa las variables del vector ingr1
###             2. Aplicales la función isexo
###             3. La tercera sentencia (por ej, "varones") depende de cómo fue contruida la función

### Varones ----
ingr_varones <- datos %>% 
  transmute_at(.vars = ingr1, 
               isexo, "Varones")
names(ingr_varones) <- paste(names(ingr_varones), "H", sep = "_")

### Mujeres ----
ingr_mujeres <- datos %>% 
  transmute_at(.vars = ingr1, 
               isexo, "Mujeres")
names(ingr_mujeres) <- paste(names(ingr_mujeres), "M", sep = "_")

### Depura ----
datos <- cbind(datos, ingr_varones, ingr_mujeres)
rm(ingr_varones, ingr_mujeres)


## CRUCE CON NIVEL EDUCATIVO ----

### Sin instrucción ----
ingr_EdNo <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Sin instrucción")
names(ingr_EdNo) <- paste(names(ingr_EdNo), "EdNo", sep = "_")

### Primaria Incompleta ----
ingr_EdPI <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Primaria Incompleta")
names(ingr_EdPI) <- paste(names(ingr_EdPI), "EdPI", sep = "_")

### Primaria Completa ----
ingr_EdPC <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Primaria Completa")
names(ingr_EdPC) <- paste(names(ingr_EdPC), "EdPC", sep = "_")

### Secundaria Incompleta ----
ingr_EdSI <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Secundaria Incompleta")
names(ingr_EdSI) <- paste(names(ingr_EdSI), "EdSI", sep = "_")    

### Secundaria Completa ----
ingr_EdSC <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Secundaria Completa")
names(ingr_EdSC) <- paste(names(ingr_EdSC), "EdSC", sep = "_")  

### Terciario Incompleto ----
ingr_EdTI <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Terciario Incompleto")
names(ingr_EdTI) <- paste(names(ingr_EdTI), "EdTI", sep = "_") 

### Terciario Completo ----
ingr_EdTC <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Terciario Completo")
names(ingr_EdTC) <- paste(names(ingr_EdTC), "EdTC", sep = "_") 

### Universitaria Incompleta ----
ingr_EdUI <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Universitaria Incompleta")
names(ingr_EdUI) <- paste(names(ingr_EdUI), "EdUI", sep = "_")

### Universitaria Completa ----
ingr_EdUC <- datos %>% 
  transmute_at(.vars = ingr1, 
               ieducacion, "Universitaria Completa")
names(ingr_EdUC) <- paste(names(ingr_EdUC), "EdUC", sep = "_")

### Depura ----
datos <- cbind(datos, ingr_EdNo, ingr_EdPI, ingr_EdPC, ingr_EdSI, 
               ingr_EdSC, ingr_EdTI, ingr_EdTC, ingr_EdUI, ingr_EdUC)
rm(ingr_EdNo, ingr_EdPI, ingr_EdPC, ingr_EdSI, 
   ingr_EdSC, ingr_EdTI, ingr_EdTC, ingr_EdUI, ingr_EdUC)


## CRUCE CON CONDICION DE FORMALIDAD ----
### Formales ----
ingr_CF <- datos %>% 
  transmute_at(.vars = ingr2, 
               iformalidad, "Formal")
names(ingr_CF) <- paste(names(ingr_CF), "CF", sep = "_")

### No formales ----
ingr_CI <- datos %>% 
  transmute_at(.vars = ingr2, 
               iformalidad, "No formal")
names(ingr_CI) <- paste(names(ingr_CI), "CI", sep = "_")

### Depura ----
datos <- cbind(datos, ingr_CF, ingr_CI)
rm(ingr_CF, ingr_CI)



## CRUCE CON TIPO DE EMPLEO ----
### Empleo privado ----
ingr_EmPriv <- datos %>% 
  transmute_at(.vars = ingr2, 
               itipo_empleo, "Privado")
names(ingr_EmPriv) <- paste(names(ingr_EmPriv), "EmPriv", sep = "_")

### Empleo Público ----
ingr_EmPubl <- datos %>% 
  transmute_at(.vars = ingr2, 
               itipo_empleo, "Público")
names(ingr_EmPubl) <- paste(names(ingr_EmPubl), "EmPubl", sep = "_")

### Otro tipo de empleo ----
ingr_EmOtro <- datos %>% 
  transmute_at(.vars = ingr2, 
               itipo_empleo, "Otro tipo")
names(ingr_EmOtro) <- paste(names(ingr_EmOtro), "EmOtro", sep = "_")

### Depura ----
datos <- cbind(datos, ingr_EmPriv, ingr_EmPubl, ingr_EmOtro)
rm(ingr_EmPriv, ingr_EmPubl, ingr_EmOtro)

 

## CRUCE CON RANGO ETARIO ----
### Edad que no corresponde - Rango de 0-9 ----
ingr_ReNo <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "No corresponde")
names(ingr_ReNo) <- paste(names(ingr_ReNo), "ReNo", sep = "_")

### Menor - Rango de 10-13 ----
ingr_ReMe <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Menor")
names(ingr_ReMe) <- paste(names(ingr_ReMe), "ReMe", sep = "_")

### Joven - Rango de 14-18 ----
ingr_ReJov <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Joven")
names(ingr_ReJov) <- paste(names(ingr_ReJov), "ReJov", sep = "_")

### Junior - Rango de 19-25 ----
ingr_ReJu <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Junior")
names(ingr_ReJu) <- paste(names(ingr_ReJu), "ReJu", sep = "_")

### Semi-senior - Rango de 26-35 ----
ingr_ReSemi <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Semi senior")
names(ingr_ReSemi) <- paste(names(ingr_ReSemi), "ReSemi", sep = "_")

### Senior - Rango de 36-45 ----
ingr_ReSen <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Senior")
names(ingr_ReSen) <- paste(names(ingr_ReSen), "ReSen", sep = "_")

### Especialista I - Rango de 46-55 ----
ingr_ReEsp1 <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Especialista I")
names(ingr_ReEsp1) <- paste(names(ingr_ReEsp1), "ReEsp1", sep = "_")

### Especialista II - Rango de 56-65 ----
ingr_ReEsp2 <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "Especialista II")
names(ingr_ReEsp2) <- paste(names(ingr_ReEsp2), "ReEsp2", sep = "_")

### En edad jubilatoria - Rango 66+ ----
ingr_ReJub <- datos %>% 
  transmute_at(.vars = ingr2, 
               ietario, "En edad jubilatoria")
names(ingr_ReJub) <- paste(names(ingr_ReJub), "ReJub", sep = "_")

### Depura ----
datos <- cbind(datos, ingr_ReNo, ingr_ReMe, ingr_ReJov, ingr_ReJu, ingr_ReSemi, 
               ingr_ReSen, ingr_ReEsp1, ingr_ReEsp2, ingr_ReJub)
rm(ingr_ReNo, ingr_ReMe, ingr_ReJov, ingr_ReJu, ingr_ReSemi, ingr_ReSen, 
   ingr_ReEsp1, ingr_ReEsp2, ingr_ReJub)






# FUNCION: dummies de una variable x, multiplicadas por su pondera ####

f1 <- function(data, x, peso, nombres) {
  
  # En esta parte se crea una matriz de dummies para la variable x seleccionada
  dummies <- data %>% 
             pull({{x}}) %>% 
             as.character() %>%
             fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE)
  
  # Toma la variable que juega de ponderador y la multiplica en columnas [por eso el 2 en apply] por la matriz de dummies
  ponderas <- apply(dummies, 2, function(x) {x * pull(data, {{peso}})}) 
  
  # Nombra a las columnas de pondera según el orden alfabetico original, que se carga en la función
  colnames(ponderas) <- nombres
  
  # Finalmente une los resultado de pondera renombrados a la matriz original
  bind_cols(data, as.data.frame(ponderas))
  
}


## NOTA: a la función f1 se la debe alimentar con un vector de nombres para cada dummy ponderada



## Obtención de nombres para f1 ----

nom_educ_largo <- datos$nivel_educ_obtenido2 %>% 
                  unique() %>% 
                  str_to_lower() %>% 
                  str_replace_all(" ", "_") %>% 
                  str_replace("a$", "o") %>% 
                  str_replace("a_", "o_") %>% 
                  sort()

nom_educ <- c("PC", "PI", "SC", "SI", "No", "TC", "TI", "UC", "UI")
nom_edad <- c("Jub", "Esp1", "Esp2", "Jov", "Ju", "Me", "No", "Semi", "Sen")

## Obtención de dummies ponderadas por categoría ----
datos_f1 <- datos %>% 
  # Dummies de variables puras, sin ningún tipo de cruce
  f1(condicion_formalidad, pondera, c("formal", "no_corresp_formalidad", "informal")) %>% 
  f1(tipo_empleo, pondera, c("otro_tipo_empleo", "privado", "publico")) %>% 
  f1(nivel_educ_obtenido2, pondera, nom_educ_largo) %>% 
  f1(categoria_ocupacional2, pondera, c("cuenta_propia", "empleado", "familiar", "patron")) %>% 
  f1(sexo, pondera, c("mujer", "hombre")) %>% 
  
  f1(nivel_educ_obtenido2, hombre, paste0("hombre_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, mujer, paste0("mujer_Ed", nom_educ)) %>% 
  f1(edad_categoria, hombre, paste0("hombre_Re", nom_edad)) %>% 
  f1(edad_categoria, mujer, paste0("mujer_Re", nom_edad)) %>% 
  f1(condicion_formalidad, hombre, paste0("hombre_", c("CF", "CFNC", "CI"))) %>% 
  f1(condicion_formalidad, mujer, paste0("mujer_", c("CF", "CFNC", "CI"))) %>% 
  f1(tipo_empleo, hombre, paste0("hombre_Em", c("Otro", "Priv", "Publ"))) %>% 
  f1(tipo_empleo, mujer, paste0("mujer_Em", c("Otro", "Priv", "Publ"))) %>% 
  f1(categoria_ocupacional2, hombre, paste0("hombre_Cat", c("Cta", "Empl", "Fam", "Pat"))) %>% 
  f1(categoria_ocupacional2, mujer, paste0("mujer_Cat", c("Cta", "Empl", "Fam", "Pat"))) %>% 
  f1(nivel_educ_obtenido2, formal, paste0("formal_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, informal, paste0("informal_Ed", nom_educ)) %>% 
  f1(tipo_empleo, formal, paste0("formal_Em", c("Otro", "Priv", "Publ"))) %>% 
  f1(tipo_empleo, informal, paste0("informal_Em", c("Otro", "Priv", "Publ"))) %>% 
  f1(edad_categoria, formal, paste0("formal_Re", nom_edad)) %>% 
  f1(edad_categoria, informal, paste0("informal_Re", nom_edad)) %>% 
  f1(nivel_educ_obtenido2, publico, paste0("publico_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, privado, paste0("privado_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, otro_tipo_empleo, paste0("otro_tipo_empleo_Ed", nom_educ)) %>% 
  f1(edad_categoria, publico, paste0("publico_Re", nom_edad)) %>% 
  f1(edad_categoria, privado, paste0("privado_Re", nom_edad)) %>% 
  f1(edad_categoria, otro_tipo_empleo, paste0("otro_tipo_empleo_Re", nom_edad)) %>% 
  f1(edad_categoria, sin_instrucción, paste0("sin_instrucción_Re", nom_edad)) %>% 
  f1(edad_categoria, primario_incompleto, paste0("primario_incompleto_Re", nom_edad)) %>% 
  f1(edad_categoria, primario_completo, paste0("primario_completo_Re", nom_edad)) %>% 
  f1(edad_categoria, secundario_incompleto, paste0("secundario_incompleto_Re", nom_edad)) %>% 
  f1(edad_categoria, secundario_completo, paste0("secundario_completo_Re", nom_edad)) %>% 
  f1(edad_categoria, terciario_incompleto, paste0("terciario_incompleto_Re", nom_edad)) %>% 
  f1(edad_categoria, terciario_completo, paste0("terciario_completo_Re", nom_edad)) %>% 
  f1(edad_categoria, universitario_incompleto, paste0("universitario_incompleto_Re", nom_edad)) %>% 
  f1(edad_categoria, universitario_completo, paste0("universitario_completo_Re", nom_edad)) %>% 
  f1(nivel_educ_obtenido2, patron, paste0("patron_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, cuenta_propia, paste0("cuenta_propia_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, empleado, paste0("empleado_Ed", nom_educ)) %>% 
  f1(nivel_educ_obtenido2, familiar, paste0("familiar_Ed", nom_educ)) %>% 
  f1(condicion_formalidad, empleado, paste0("empleado_", c("CF", "CFNC", "CI"))) %>% 
  f1(tipo_empleo, empleado, paste0("empleado_Em", c("Otro", "Priv", "Publ"))) %>% 
  
  select(-ends_with("CFNC"), -cta_propia) #cuidado porque el codigo de Ger pisa esta variable

colnames(datos_f1) <- chartr("áéíóú", "aeiou", colnames(datos_f1))
colnames(datos_f1) <- str_replace(colnames(datos_f1), "cuenta_", "cta_")

# lu <- datos %>% replace(. == 0, NA)
# diego <- datos_f1 %>% replace(. == 0, NA)
# all_equal(german, diego)


#### VERSION ORIGINAL LU ####

# ## Aplicadas a condición de formalidad ##
# cfabsolutas <- function(x, y = "Formal") {
#   if (y == "Formal") {
#     case_when(
#       datos$condicion_formalidad == "Formal" ~ datos$pondera,
#       datos$condicion_formalidad != "Formal" ~ 0,
#       is.na(datos$condicion_formalidad) ~ 0)}
#   
#   else if (y == "No formal") {
#     case_when(
#       datos$condicion_formalidad == "No formal" ~ datos$pondera,
#       datos$condicion_formalidad != "No formal" ~ 0,
#       is.na(datos$condicion_formalidad) ~ 0)}
#   
#   else if (y == "No corresponde") {
#     case_when(
#       datos$condicion_formalidad == "No corresponde" ~ datos$pondera,
#       datos$condicion_formalidad != "No corresponde" ~ 0,
#       is.na(datos$condicion_formalidad) ~ 0)}
# } 
# 
# ## Aplicadas a tipo de empleo ##
# teabsolutas <- function(x, y = "Público") {
#   if (y == "Público") {
#     case_when(
#       datos$tipo_empleo == "Público" ~ datos$pondera,
#       datos$tipo_empleo != "Público" ~ 0,
#       is.na(datos$tipo_empleo) ~ 0)}
#   
#   else if (y == "Privado") {
#     case_when(
#       datos$tipo_empleo == "Privado" ~ datos$pondera,
#       datos$tipo_empleo != "Privado" ~ 0,
#       is.na(datos$tipo_empleo) ~ 0)}
#   
#   else if (y == "Otro tipo") {
#     case_when(
#       datos$tipo_empleo == "Otro tipo" ~ datos$pondera,
#       datos$tipo_empleo != "Otro tipo" ~ 0,
#       is.na(datos$tipo_empleo) ~ 0)}
# } 
# 
# ## Aplicadas a nivel educativo ##
# neabsolutas <- function(x, y = "Universitaria Completa") {
#   if (y == "Universitaria Completa") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Universitaria Completa" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Universitaria Completa" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Universitaria Incompleta") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Universitaria Incompleta" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Universitaria Incompleta" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Terciario Completo") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Terciario Completo" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Terciario Completo" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Terciario Incompleto") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Terciario Incompleto" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Terciario Incompleto" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Secundaria Completa") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Secundaria Completa" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Secundaria Completa" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Secundaria Incompleta") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Secundaria Incompleta" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Secundaria Incompleta" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Primaria Completa") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Primaria Completa" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Primaria Completa" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Primaria Incompleta") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Primaria Incompleta" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Primaria Incompleta" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
#   
#   else if (y == "Sin instrucción") {
#     case_when(
#       datos$nivel_educ_obtenido2 == "Sin instrucción" ~ datos$pondera,
#       datos$nivel_educ_obtenido2 != "Sin instrucción" ~ 0,
#       is.na(datos$nivel_educ_obtenido2) ~ 0)}
# } 
# 
# ## Aplicadas a categoría ocupacional ##
# catabsolutas <- function(x, y = "Patrón") {
#   if (y == "Patrón") {
#     case_when(
#       datos$categoria_ocupacional2 == "Patrón" ~ datos$pondera,
#       datos$categoria_ocupacional2 != "Patrón" ~ 0,
#       is.na(datos$categoria_ocupacional2) ~ 0)}
#   
#   else if (y == "Cuenta Propia") {
#     case_when(
#       datos$categoria_ocupacional2 == "Cuenta Propia" ~ datos$pondera,
#       datos$categoria_ocupacional2 != "Cuenta Propia" ~ 0,
#       is.na(datos$categoria_ocupacional2) ~ 0)}
#   
#   else if (y == "Empleado") {
#     case_when(
#       datos$categoria_ocupacional2 == "Empleado" ~ datos$pondera,
#       datos$categoria_ocupacional2 != "Empleado" ~ 0,
#       is.na(datos$categoria_ocupacional2) ~ 0)}
#   
#   else if (y == "Familiar") {
#     case_when(
#       datos$categoria_ocupacional2 == "Familiar" ~ datos$pondera,
#       datos$categoria_ocupacional2 != "Familiar" ~ 0,
#       is.na(datos$categoria_ocupacional2) ~ 0)}
# }
# 
# ## Aplicadas a cruces ##
# educacion <- function(x, y = "Sin instrucción") {
#   if (y == "Sin instrucción") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Sin instrucción",
#       yes = x, no = 0)}
#   
#   else if (y == "Primaria Incompleta") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Incompleta",
#       yes = x, no = 0)}
#   
#   else if (y == "Primaria Completa") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Completa",
#       yes = x, no = 0)}
#   
#   else if (y == "Secundaria Incompleta") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Secundaria Incompleta",
#       yes = x, no = 0)}
#   
#   else if (y == "Secundaria Completa") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Secundaria Completa",
#       yes = x, no = 0)}
#   
#   else if (y == "Terciario Incompleto") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Incompleto",
#       yes = x, no = 0)}
#   
#   else if (y == "Terciario Completo") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Completo",
#       yes = x, no = 0)}
#   
#   else if (y == "Universitaria Incompleta") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Universitaria Incompleta",
#       yes = x, no = 0)}
#   
#   else if (y == "Universitaria Completa") {
#     ifelse(
#       test = x > 0 & datos$nivel_educ_obtenido2 == "Universitaria Completa",
#       yes = x, no = 0)}
# }
# 
# formalidad <- function(x, y = "Formal") {
#   if (y == "Formal") {
#     case_when(
#       x > 0 & datos$condicion_formalidad == "Formal" ~ x,
#       x > 0 & datos$condicion_formalidad != "Formal" ~ 0,
#       x > 0 & is.na(datos$condicion_formalidad) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "No formal") {
#     case_when(
#       x > 0 & datos$condicion_formalidad == "No formal" ~ x,
#       x > 0 & datos$condicion_formalidad != "No formal" ~ 0,
#       x > 0 & is.na(datos$condicion_formalidad) ~ 0,
#       x == 0 ~ 0)}
# }
# 
# tipo_empleo <- function(x, y = "Privado") {
#   if (y == "Privado") {
#     case_when(
#       x > 0 & datos$tipo_empleo == "Privado" ~ x,
#       x > 0 & datos$tipo_empleo != "Privado" ~ 0,
#       x > 0 & is.na(datos$tipo_empleo) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Público") {
#     case_when(
#       x > 0 & datos$tipo_empleo == "Público" ~ x,
#       x > 0 & datos$tipo_empleo != "Público" ~ 0,
#       x > 0 & is.na(datos$tipo_empleo) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Otro tipo") {
#     case_when(
#       x > 0 & datos$tipo_empleo == "Otro tipo" ~ x,
#       x > 0 & datos$tipo_empleo != "Otro tipo" ~ 0,
#       x > 0 & is.na(datos$tipo_empleo) ~ 0,
#       x == 0 ~ 0)}
# }
# 
# cat_ocupacional <- function(x, y = "Patrón") {
#   if (y == "Patrón") {
#     case_when(
#       x > 0 & datos$categoria_ocupacional2 == "Patrón" ~ x,
#       x > 0 & datos$categoria_ocupacional2 != "Patrón" ~ 0,
#       x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Cuenta Propia") {
#     case_when(
#       x > 0 & datos$categoria_ocupacional2 == "Cuenta Propia" ~ x,
#       x > 0 & datos$categoria_ocupacional2 != "Cuenta Propia" ~ 0,
#       x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Empleado") {
#     case_when(
#       x > 0 & datos$categoria_ocupacional2 == "Empleado" ~ x,
#       x > 0 & datos$categoria_ocupacional2 != "Empleado" ~ 0,
#       x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Familiar") {
#     case_when(
#       x > 0 & datos$categoria_ocupacional2 == "Familiar" ~ x,
#       x > 0 & datos$categoria_ocupacional2 != "Familiar" ~ 0,
#       x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
#       x == 0 ~ 0)}
# }
# 
# etario <- function(x, y = "No corresponde") {
#   if (y == "No corresponde") {
#     case_when(
#       x > 0 & datos$edad_categoria == "No corresponde" ~ x,
#       x > 0 & datos$edad_categoria != "No corresponde" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Menor") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Menor" ~ x,
#       x > 0 & datos$edad_categoria != "Menor" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Joven") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Joven" ~ x,
#       x > 0 & datos$edad_categoria != "Joven" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Junior") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Junior" ~ x,
#       x > 0 & datos$edad_categoria != "Junior" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Semi senior") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Semi senior" ~ x,
#       x > 0 & datos$edad_categoria != "Semi senior" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Senior") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Senior" ~ x,
#       x > 0 & datos$edad_categoria != "Senior" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Especialista I") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Especialista I" ~ x,
#       x > 0 & datos$edad_categoria != "Especialista I" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "Especialista II") {
#     case_when(
#       x > 0 & datos$edad_categoria == "Especialista II" ~ x,
#       x > 0 & datos$edad_categoria != "Especialista II" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
#   else if (y == "En edad jubilatoria") {
#     case_when(
#       x > 0 & datos$edad_categoria == "En edad jubilatoria" ~ x,
#       x > 0 & datos$edad_categoria != "En edad jubilatoria" ~ 0,
#       x > 0 & is.na(datos$edad_categoria) ~ 0,
#       x == 0 ~ 0)}
#   
# }

# ## Sexo ##
# datos <- datos %>% 
# ## Variables absolutas ##-  
#     mutate(mujer = ifelse(sexo == "Mujeres", pondera, 0),
#            hombre = ifelse(sexo == "Varones", pondera, 0),
# 
# ## Variables cruzadas ##
# ### Cruce con nivel educativo ##-    
#          hombre_EdNo = educacion(hombre, "Sin instrucción"),
#          hombre_EdPI = educacion(hombre, "Primaria Incompleta"),
#          hombre_EdPC = educacion(hombre, "Primaria Completa"),
#          hombre_EdSI = educacion(hombre, "Secundaria Incompleta"),
#          hombre_EdSC = educacion(hombre, "Secundaria Completa"),
#          hombre_EdTI = educacion(hombre, "Terciario Incompleto"),
#          hombre_EdTC = educacion(hombre, "Terciario Completo"),
#          hombre_EdUI = educacion(hombre, "Universitaria Incompleta"),
#          hombre_EdUC = educacion(hombre, "Universitaria Completa"),
#          
#          mujer_EdNo = educacion(mujer, "Sin instrucción"),
#          mujer_EdPI = educacion(mujer, "Primaria Incompleta"),
#          mujer_EdPC = educacion(mujer, "Primaria Completa"),
#          mujer_EdSI = educacion(mujer, "Secundaria Incompleta"),
#          mujer_EdSC = educacion(mujer, "Secundaria Completa"),
#          mujer_EdTI = educacion(mujer, "Terciario Incompleto"),
#          mujer_EdTC = educacion(mujer, "Terciario Completo"),
#          mujer_EdUI = educacion(mujer, "Universitaria Incompleta"),
#          mujer_EdUC = educacion(mujer, "Universitaria Completa"),
# 
# ### Cruce con condición de formalidad ##
#          hombre_CF = formalidad(hombre, "Formal"),
#          hombre_CI = formalidad(hombre, "No formal"),
# 
#          mujer_CF = formalidad(mujer, "Formal"),
#          mujer_CI = formalidad(mujer, "No formal"),
# 
# ### Cruce con tipo de empleo ##
#          hombre_EmPriv = tipo_empleo(hombre, "Privado"),
#          hombre_EmPubl = tipo_empleo(hombre, "Público"),
#          hombre_EmOtro = tipo_empleo(hombre, "Otro tipo"),
#          
#          mujer_EmPriv = tipo_empleo(mujer, "Privado"),
#          mujer_EmPubl = tipo_empleo(mujer, "Público"),
#          mujer_EmOtro = tipo_empleo(mujer, "Otro tipo"),
#       
#  
# ### Cruce con categoría ocupacional ##
#          hombre_CatPat = cat_ocupacional(hombre, "Patrón"),
#          hombre_CatCta = cat_ocupacional(hombre, "Cuenta Propia"),
#          hombre_CatEmpl = cat_ocupacional(hombre, "Empleado"),
#          hombre_CatFam = cat_ocupacional(hombre, "Familiar"),
#            
#          mujer_CatPat = cat_ocupacional(mujer, "Patrón"),
#          mujer_CatCta = cat_ocupacional(mujer, "Cuenta Propia"),
#          mujer_CatEmpl = cat_ocupacional(mujer, "Empleado"),
#          mujer_CatFam = cat_ocupacional(mujer, "Familiar"),
# 
# 
# ### Cruce con Rango etario ##
#          hombre_ReNo = etario(hombre, "No corresponde"),
#          hombre_ReMe = etario(hombre, "Menor"),
#          hombre_ReJov = etario(hombre, "Joven"),
#          hombre_ReJu = etario(hombre, "Junior"),
#          hombre_ReSemi = etario(hombre, "Semi senior"),
#          hombre_ReSen = etario(hombre, "Senior"),
#          hombre_ReEsp1 = etario(hombre, "Especialista I"),
#          hombre_ReEsp2 = etario(hombre, "Especialista II"),
#          hombre_ReJub = etario(hombre, "En edad jubilatoria"),
#    
#          mujer_ReNo = etario(mujer, "No corresponde"),
#          mujer_ReMe = etario(mujer, "Menor"),
#          mujer_ReJov = etario(mujer, "Joven"),
#          mujer_ReJu = etario(mujer, "Junior"),
#          mujer_ReSemi = etario(mujer, "Semi senior"),
#          mujer_ReSen = etario(mujer, "Senior"),
#          mujer_ReEsp1 = etario(mujer, "Especialista I"),
#          mujer_ReEsp2 = etario(mujer, "Especialista II"),
#          mujer_ReJub = etario(mujer, "En edad jubilatoria"))
# 
# 
# ## Condición de formalidad ##
# datos <- datos %>% 
# 
# ## Variables absolutas ##
#     mutate(formal = cfabsolutas(condicion_formalidad, "Formal"),
#            informal = cfabsolutas(condicion_formalidad, "No formal"),
#            no_corresp_formalidad = cfabsolutas(condicion_formalidad, "No corresponde")) %>%
# 
# ## Variables cruzadas ##
# ### Cruce con nivel educativo ##
#     mutate(formal_EdNo = educacion(formal, "Sin instrucción"),
#            formal_EdPI = educacion(formal, "Primaria Incompleta"),
#            formal_EdPC = educacion(formal, "Primaria Completa"),
#            formal_EdSI = educacion(formal, "Secundaria Incompleta"),
#            formal_EdSC = educacion(formal, "Secundaria Completa"),
#            formal_EdTC = educacion(formal, "Terciario Completo"),
#            formal_EdTI = educacion(formal, "Terciario Incompleto"),
#            formal_EdUI = educacion(formal, "Universitaria Incompleta"),
#            formal_EdUC = educacion(formal, "Universitaria Completa"),
#            
#            informal_EdNo = educacion(informal, "Sin instrucción"),
#            informal_EdPI = educacion(informal, "Primaria Incompleta"),
#            informal_EdPC = educacion(informal, "Primaria Completa"),
#            informal_EdSI = educacion(informal, "Secundaria Incompleta"),
#            informal_EdSC = educacion(informal, "Secundaria Completa"),
#            informal_EdTC = educacion(informal, "Terciario Completo"),
#            informal_EdTI = educacion(informal, "Terciario Incompleto"),
#            informal_EdUI = educacion(informal, "Universitaria Incompleta"),
#            informal_EdUC = educacion(informal, "Universitaria Completa")) %>% 
#            
# ### Cruce con tipo de empleo ##
#     mutate(formal_EmPriv = tipo_empleo(formal, "Privado"),
#            formal_EmPubl = tipo_empleo(formal, "Público"),
#            formal_EmOtro = tipo_empleo(formal, "Otro tipo"),
#            
#            informal_EmPriv = tipo_empleo(informal, "Privado"),
#            informal_EmPubl = tipo_empleo(informal, "Público"),
#            informal_EmOtro = tipo_empleo(informal, "Otro tipo")) %>% 
# 
# 
# ### Cruce con rango etario ##
#     mutate(formal_ReNo = etario(formal, "No corresponde"),
#            formal_ReMe = etario(formal, "Menor"),
#            formal_ReJov = etario(formal, "Joven"),
#            formal_ReJu = etario(formal, "Junior"),
#            formal_ReSemi = etario(formal, "Semi senior"),
#            formal_ReSen = etario(formal, "Senior"),
#            formal_ReEsp1 = etario(formal, "Especialista I"),
#            formal_ReEsp2 = etario(formal, "Especialista II"),
#            formal_ReJub = etario(formal, "En edad jubilatoria"),
#            
#            informal_ReNo = etario(informal, "No corresponde"),
#            informal_ReMe = etario(informal, "Menor"),
#            informal_ReJov = etario(informal, "Joven"),
#            informal_ReJu = etario(informal, "Junior"),
#            informal_ReSemi = etario(informal, "Semi senior"),
#            informal_ReSen = etario(informal, "Senior"),
#            informal_ReEsp1 = etario(informal, "Especialista I"),
#            informal_ReEsp2 = etario(informal, "Especialista II"),
#            informal_ReJub = etario(informal, "En edad jubilatoria"))
#            
#            
# ## Tipo de empleo ##
# ## Variables absolutas ##
# datos <- datos %>% 
#   mutate(publico = teabsolutas(tipo_empleo, "Público"),
#          privado = teabsolutas(tipo_empleo, "Privado"),
#          otro_tipo_empleo = teabsolutas(tipo_empleo, "Otro tipo")) %>%
# 
# ## Variables cruzadas ##      
# ### Cruce con nivel educativo ##
#     mutate(publico_EdUC = educacion(publico,"Universitaria Completa"),
#            publico_EdUI = educacion(publico, "Universitaria Incompleta"),
#            publico_EdTI = educacion(publico, "Terciario Incompleto"),
#            publico_EdTC = educacion(publico, "Terciario Completo"),
#            publico_EdSC = educacion(publico, "Secundaria Completa"),
#            publico_EdSI = educacion(publico, "Secundaria Incompleta"),
#            publico_EdPC = educacion(publico, "Primaria Completa"),
#            publico_EdPI = educacion(publico, "Primaria Incompleta"),
#            publico_EdNo = educacion(publico, "Sin instrucción"),
#            
#            privado_EdUC = educacion(privado,"Universitaria Completa"),
#            privado_EdUI = educacion(privado, "Universitaria Incompleta"),
#            privado_EdTI = educacion(privado, "Terciario Incompleto"),
#            privado_EdTC = educacion(privado, "Terciario Completo"),
#            privado_EdSC = educacion(privado, "Secundaria Completa"),
#            privado_EdSI = educacion(privado, "Secundaria Incompleta"),
#            privado_EdPC = educacion(privado, "Primaria Completa"),
#            privado_EdPI = educacion(privado, "Primaria Incompleta"),
#            privado_EdNo = educacion(privado, "Sin instrucción"),
#            
#            otro_tipo_empleo_EdUC = educacion(otro_tipo_empleo,"Universitaria Completa"),
#            otro_tipo_empleo_EdUI = educacion(otro_tipo_empleo, "Universitaria Incompleta"),
#            otro_tipo_empleo_EdTI = educacion(otro_tipo_empleo, "Terciario Incompleto"),
#            otro_tipo_empleo_EdTC = educacion(otro_tipo_empleo, "Terciario Completo"),
#            otro_tipo_empleo_EdSC = educacion(otro_tipo_empleo, "Secundaria Completa"),
#            otro_tipo_empleo_EdSI = educacion(otro_tipo_empleo, "Secundaria Incompleta"),
#            otro_tipo_empleo_EdPC = educacion(otro_tipo_empleo, "Primaria Completa"),
#            otro_tipo_empleo_EdPI = educacion(otro_tipo_empleo, "Primaria Incompleta"),
#            otro_tipo_empleo_EdNo = educacion(otro_tipo_empleo, "Sin instrucción")) %>% 
#            
# ### Cruce con rango etario ##
#     mutate(publico_ReNo = etario(publico,"No corresponde"),
#            publico_ReMe = etario(publico, "Menor"),
#            publico_ReJov = etario(publico, "Joven"),
#            publico_ReJu = etario(publico, "Junior"),
#            publico_ReSemi = etario(publico, "Semi senior"),
#            publico_ReSen = etario(publico, "Senior"),
#            publico_ReEsp1 = etario(publico, "Especialista I"),
#            publico_ReEsp2 = etario(publico, "Especialista II"),
#            publico_ReJub = etario(publico, "En edad jubilatoria"),
#            
#            privado_ReNo = etario(privado,"No corresponde"),
#            privado_ReMe = etario(privado, "Menor"),
#            privado_ReJov = etario(privado, "Joven"),
#            privado_ReJu = etario(privado, "Junior"),
#            privado_ReSemi = etario(privado, "Semi senior"),
#            privado_ReSen = etario(privado, "Senior"),
#            privado_ReEsp1 = etario(privado, "Especialista I"),
#            privado_ReEsp2 = etario(privado, "Especialista II"),
#            privado_ReJub = etario(privado, "En edad jubilatoria"),
#            
#            otro_tipo_empleo_ReNo = etario(otro_tipo_empleo,"No corresponde"),
#            otro_tipo_empleo_ReMe = etario(otro_tipo_empleo, "Menor"),
#            otro_tipo_empleo_ReJov = etario(otro_tipo_empleo, "Joven"),
#            otro_tipo_empleo_ReJu = etario(otro_tipo_empleo, "Junior"),
#            otro_tipo_empleo_ReSemi = etario(otro_tipo_empleo, "Semi senior"),
#            otro_tipo_empleo_ReSen = etario(otro_tipo_empleo, "Senior"),
#            otro_tipo_empleo_ReEsp1 = etario(otro_tipo_empleo, "Especialista I"),
#            otro_tipo_empleo_ReEsp2 = etario(otro_tipo_empleo, "Especialista II"),
#            otro_tipo_empleo_ReJub = etario(otro_tipo_empleo, "En edad jubilatoria"))
#            
# 
#       
# ## Nivel educativo ##
# ## Variables absolutas ##
# datos <- datos %>% 
#     mutate(universitario_completo = neabsolutas(nivel_educ_obtenido2, "Universitaria Completa"),
#            universitario_incompleto = neabsolutas(nivel_educ_obtenido2, "Universitaria Incompleta"),
#            
#            terciario_completo = neabsolutas(nivel_educ_obtenido2, "Terciario Completo"),
#            terciario_incompleto = neabsolutas(nivel_educ_obtenido2, "Terciario Incompleto"),
#            
#            secundario_completo = neabsolutas(nivel_educ_obtenido2, "Secundaria Completa"),
#            secundario_incompleto = neabsolutas(nivel_educ_obtenido2, "Secundaria Incompleta"),
#            
#            primario_completo = neabsolutas(nivel_educ_obtenido2, "Primaria Completa"),
#            primario_incompleto = neabsolutas(nivel_educ_obtenido2, "Primaria Incompleta"),
#            
#            sin_instruccion = neabsolutas(nivel_educ_obtenido2, "Sin instrucción")) %>%
#       
# 
# ### Cruce con rango etario ##
#     mutate(universitario_completo_ReNo = etario(universitario_completo, "No corresponde"),
#            universitario_completo_ReMe = etario(universitario_completo, "Menor"),
#            universitario_completo_ReJov = etario(universitario_completo, "Joven"),
#            universitario_completo_ReJu = etario(universitario_completo, "Junior"),
#            universitario_completo_ReSemi = etario(universitario_completo, "Semi senior"),
#            universitario_completo_ReSen = etario(universitario_completo, "Senior"),
#            universitario_completo_ReEsp1 = etario(universitario_completo, "Especialista I"),
#            universitario_completo_ReEsp2 = etario(universitario_completo, "Especialista II"),
#            universitario_completo_ReJub = etario(universitario_completo, "En edad jubilatoria"),
#            
#            universitario_incompleto_ReNo = etario(universitario_incompleto, "No corresponde"),
#            universitario_incompleto_ReMe = etario(universitario_incompleto, "Menor"),
#            universitario_incompleto_ReJov = etario(universitario_incompleto, "Joven"),
#            universitario_incompleto_ReJu = etario(universitario_incompleto, "Junior"),
#            universitario_incompleto_ReSemi = etario(universitario_incompleto, "Semi senior"),
#            universitario_incompleto_ReSen = etario(universitario_incompleto, "Senior"),
#            universitario_incompleto_ReEsp1 = etario(universitario_incompleto, "Especialista I"),
#            universitario_incompleto_ReEsp2 = etario(universitario_incompleto, "Especialista II"),
#            universitario_incompleto_ReJub = etario(universitario_incompleto, "En edad jubilatoria"),
#            
#            terciario_completo_ReNo = etario(terciario_completo, "No corresponde"),
#            terciario_completo_ReMe = etario(terciario_completo, "Menor"),
#            terciario_completo_ReJov = etario(terciario_completo, "Joven"),
#            terciario_completo_ReJu = etario(terciario_completo, "Junior"),
#            terciario_completo_ReSemi = etario(terciario_completo, "Semi senior"),
#            terciario_completo_ReSen = etario(terciario_completo, "Senior"),
#            terciario_completo_ReEsp1 = etario(terciario_completo, "Especialista I"),
#            terciario_completo_ReEsp2 = etario(terciario_completo, "Especialista II"),
#            terciario_completo_ReJub = etario(terciario_completo, "En edad jubilatoria"),
#            
#            terciario_incompleto_ReNo = etario(terciario_incompleto, "No corresponde"),
#            terciario_incompleto_ReMe = etario(terciario_incompleto, "Menor"),
#            terciario_incompleto_ReJov = etario(terciario_incompleto, "Joven"),
#            terciario_incompleto_ReJu = etario(terciario_incompleto, "Junior"),
#            terciario_incompleto_ReSemi = etario(terciario_incompleto, "Semi senior"),
#            terciario_incompleto_ReSen = etario(terciario_incompleto, "Senior"),
#            terciario_incompleto_ReEsp1 = etario(terciario_incompleto, "Especialista I"),
#            terciario_incompleto_ReEsp2 = etario(terciario_incompleto, "Especialista II"),
#            terciario_incompleto_ReJub = etario(terciario_incompleto, "En edad jubilatoria"),
#            
#            secundario_completo_ReNo = etario(secundario_completo, "No corresponde"),
#            secundario_completo_ReMe = etario(secundario_completo, "Menor"),
#            secundario_completo_ReJov = etario(secundario_completo, "Joven"),
#            secundario_completo_ReJu = etario(secundario_completo, "Junior"),
#            secundario_completo_ReSemi = etario(secundario_completo, "Semi senior"),
#            secundario_completo_ReSen = etario(secundario_completo, "Senior"),
#            secundario_completo_ReEsp1 = etario(secundario_completo, "Especialista I"),
#            secundario_completo_ReEsp2 = etario(secundario_completo, "Especialista II"),
#            secundario_completo_ReJub = etario(secundario_completo, "En edad jubilatoria"),
#            
#            secundario_incompleto_ReNo = etario(secundario_incompleto, "No corresponde"),
#            secundario_incompleto_ReMe = etario(secundario_incompleto, "Menor"),
#            secundario_incompleto_ReJov = etario(secundario_incompleto, "Joven"),
#            secundario_incompleto_ReJu = etario(secundario_incompleto, "Junior"),
#            secundario_incompleto_ReSemi = etario(secundario_incompleto, "Semi senior"),
#            secundario_incompleto_ReSen = etario(secundario_incompleto, "Senior"),
#            secundario_incompleto_ReEsp1 = etario(secundario_incompleto, "Especialista I"),
#            secundario_incompleto_ReEsp2 = etario(secundario_incompleto, "Especialista II"),
#            secundario_incompleto_ReJub = etario(secundario_incompleto, "En edad jubilatoria"),
#            
#            primario_completo_ReNo = etario(primario_completo, "No corresponde"),
#            primario_completo_ReMe = etario(primario_completo, "Menor"),
#            primario_completo_ReJov = etario(primario_completo, "Joven"),
#            primario_completo_ReJu = etario(primario_completo, "Junior"),
#            primario_completo_ReSemi = etario(primario_completo, "Semi senior"),
#            primario_completo_ReSen = etario(primario_completo, "Senior"),
#            primario_completo_ReEsp1 = etario(primario_completo, "Especialista I"),
#            primario_completo_ReEsp2 = etario(primario_completo, "Especialista II"),
#            primario_completo_ReJub = etario(primario_completo, "En edad jubilatoria"),
#            
#            primario_incompleto_ReNo = etario(primario_incompleto, "No corresponde"),
#            primario_incompleto_ReMe = etario(primario_incompleto, "Menor"),
#            primario_incompleto_ReJov = etario(primario_incompleto, "Joven"),
#            primario_incompleto_ReJu = etario(primario_incompleto, "Junior"),
#            primario_incompleto_ReSemi = etario(primario_incompleto, "Semi senior"),
#            primario_incompleto_ReSen = etario(primario_incompleto, "Senior"),
#            primario_incompleto_ReEsp1 = etario(primario_incompleto, "Especialista I"),
#            primario_incompleto_ReEsp2 = etario(primario_incompleto, "Especialista II"),
#            primario_incompleto_ReJub = etario(primario_incompleto, "En edad jubilatoria"),
#            
#            sin_instruccion_ReNo = etario(sin_instruccion, "No corresponde"),
#            sin_instruccion_ReMe = etario(sin_instruccion, "Menor"),
#            sin_instruccion_ReJov = etario(sin_instruccion, "Joven"),
#            sin_instruccion_ReJu = etario(sin_instruccion, "Junior"),
#            sin_instruccion_ReSemi = etario(sin_instruccion, "Semi senior"),
#            sin_instruccion_ReSen = etario(sin_instruccion, "Senior"),
#            sin_instruccion_ReEsp1 = etario(sin_instruccion, "Especialista I"),
#            sin_instruccion_ReEsp2 = etario(sin_instruccion, "Especialista II"),
#            sin_instruccion_ReJub = etario(sin_instruccion, "En edad jubilatoria"))
#       
# 
# ## Categoría ocupacional ##
# ## Variables absolutas ##
# datos <- datos %>%
#   mutate(patron = catabsolutas(categoria_ocupacional2, "Patrón"),
#          cta_propia = catabsolutas(categoria_ocupacional2, "Cuenta Propia"),
#          empleado = catabsolutas(categoria_ocupacional2, "Empleado"),
#          familiar = catabsolutas(categoria_ocupacional2, "Familiar")) %>%
# 
# ## Variables cruzadas ##
# ### Cruce con nivel educativo ##
#     mutate(patron_EdUC = educacion(patron, "Universitaria Completa"),
#            patron_EdUI = educacion(patron, "Universitaria Incompleta"),
#            patron_EdTI = educacion(patron, "Terciario Incompleto"),
#            patron_EdTC = educacion(patron, "Terciario Completo"),
#            patron_EdSC = educacion(patron, "Secundaria Completa"),
#            patron_EdSI = educacion(patron, "Secundaria Incompleta"),
#            patron_EdPC = educacion(patron, "Primaria Completa"),
#            patron_EdPI = educacion(patron, "Primaria Incompleta"),
#            patron_EdNo = educacion(patron, "Sin instrucción"),
#            
#            cta_propia_EdUC = educacion(cta_propia, "Universitaria Completa"),
#            cta_propia_EdUI = educacion(cta_propia, "Universitaria Incompleta"),
#            cta_propia_EdTI = educacion(cta_propia, "Terciario Incompleto"),
#            cta_propia_EdTC = educacion(cta_propia, "Terciario Completo"),
#            cta_propia_EdSC = educacion(cta_propia, "Secundaria Completa"),
#            cta_propia_EdSI = educacion(cta_propia, "Secundaria Incompleta"),
#            cta_propia_EdPC = educacion(cta_propia, "Primaria Completa"),
#            cta_propia_EdPI = educacion(cta_propia, "Primaria Incompleta"),
#            cta_propia_EdNo = educacion(cta_propia, "Sin instrucción"),
#            
#            empleado_EdUC = educacion(empleado, "Universitaria Completa"),
#            empleado_EdUI = educacion(empleado, "Universitaria Incompleta"),
#            empleado_EdTI = educacion(empleado, "Terciario Incompleto"),
#            empleado_EdTC = educacion(empleado, "Terciario Completo"),
#            empleado_EdSC = educacion(empleado, "Secundaria Completa"),
#            empleado_EdSI = educacion(empleado, "Secundaria Incompleta"),
#            empleado_EdPC = educacion(empleado, "Primaria Completa"),
#            empleado_EdPI = educacion(empleado, "Primaria Incompleta"),
#            empleado_EdNo = educacion(empleado, "Sin instrucción"),
#            
#            familiar_EdUC = educacion(familiar, "Universitaria Completa"),
#            familiar_EdUI = educacion(familiar, "Universitaria Incompleta"),
#            familiar_EdTI = educacion(familiar, "Terciario Incompleto"),
#            familiar_EdTC = educacion(familiar, "Terciario Completo"),
#            familiar_EdSC = educacion(familiar, "Secundaria Completa"),
#            familiar_EdSI = educacion(familiar, "Secundaria Incompleta"),
#            familiar_EdPC = educacion(familiar, "Primaria Completa"),
#            familiar_EdPI = educacion(familiar, "Primaria Incompleta"),
#            familiar_EdNo = educacion(familiar, "Sin instrucción")) %>% 
#            
# ### Cruce de empleado con condición de formalidad ##
#     mutate(empleado_CF = formalidad(empleado, "Formal"),
#            empleado_CI = formalidad(empleado, "No formal")) %>%
# 
# ### Cruce de empleado con tipo de empleo ##
#     mutate(empleado_EmPriv = tipo_empleo(empleado, "Privado"),
#            empleado_EmPubl = tipo_empleo(empleado, "Público"),
#            empleado_EmOtro = tipo_empleo(empleado, "Otro tipo")) 
