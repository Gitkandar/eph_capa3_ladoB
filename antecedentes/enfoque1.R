# NOTA ################################################################################################################ 
#       Este archivo es el primer enfoque realizado para la creación de variables de capa3.
#       
#       Cubre la preparación previa de variables que se necesitan crear en una seudo capa2 (antes de la sección <<RESUMEN>>),
#       para luego calcular los promedios y las medias según corresponda (después de la sección <<RESUMEN>>)
#       
#       Es un trabajo titánico, que tiene la dificultad de presentar un código extenso y caso a caso
#       
#       En este script no se calculan tasas (por lo menos hasta la parte <<RESUMEN>>), solo hay dos tipos de variables:
#       - Variables de ingreso cruzadas
#       - Variables cruzadas dummy que no son de ingreso, con su correspondiente pondera.
#       
#       Después de <<RESUMEN>> hay un enfoque interesante para crear promedios y medias de las variables de ingreso
#       - Básicamente está relacionado con el uso de la función dplyr::across
#       - Una buena explicación en este link: https://willhipson.netlify.app/post/dplyr_across/dplyr_across/
#       - Por otra parte se usa la función summarise_at, que es muy parecida a across
#       - Para mayor referencia: https://dplyr.tidyverse.org/reference/summarise_all.html


      
#       ESTRATEGIAS UTILIZADAS
#       
#       1. IFELSE: Básicamente se toma cada variable y se la va convirtiendo acorde al cruce que se quiere realizar.
#       
#       2. DPLYR::ACROSS: Aplica a una lista de variables selecciona un conjunto de summaries (media, promedio, etc.)
#       
#       3. SUMMARISE_AT: Básicamente hace lo mismo que ACROSS, pero sin basarse en la codificación de purr



# Librerías ####
library(tables)
library(dplyr)
library(tidyr)
library(labelled)

## Carpetas ####
ruta_gral <- "C:/oes-MonitorEPH"
ruta_data <- "C:/oes/capa2/"
ruta_results <- "C:/oes/capa3/"
ruta_graf <- paste0(ruta_gral,"/graf/")
ruta_cod <- paste0(ruta_gral,"/cod/")

setwd(ruta_data)

# Comandos ----
anio <- c(16,17,17,17,17,18,18,18,18,19,19,19,19,20,20)
trimestre <- c(4,1,2,3,4,1,2,3,4,1,2,3,4,1,2)
periodo <- as.data.frame(cbind(anio,trimestre))
decil <- 1:10

i=14

# Iteración ####
for (i in nrow(periodo)){
    ## Archivo para invocar ####
    file_name <- paste0("EPH20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
    exit_name <- paste0("Capa3_20",periodo$anio[i],"_T",periodo$trimestre[i],".RData")
    ## Carga archivo ####
    file <- paste0(ruta_data,file_name)
    load(file)
    ## Individuos ----
    capa3i <- datos %>%
      # INGRESOS ----
      # cruce con sexo ----
    mutate(ingreso_ocupacion_principal_H = ifelse(sexo == "Varones", ingreso_ocupacion_principal, NA),
           ingreso_ocupacion_principal_M = ifelse(sexo == "Mujeres", ingreso_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_H = ifelse(sexo == "Varones", ingreso_real_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_M = ifelse(sexo == "Mujeres", ingreso_real_ocupacion_principal, NA),
           ingreso_otras_ocupaciones_H = ifelse(sexo == "Varones", ingreso_otras_ocupaciones, NA),
           ingreso_otras_ocupaciones_M = ifelse(sexo == "Mujeres", ingreso_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_H = ifelse(sexo == "Varones", ingreso_real_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_M = ifelse(sexo == "Mujeres", ingreso_real_otras_ocupaciones, NA),
           ingreso_total_individual_H = ifelse(sexo == "Varones", ingreso_total_individual, NA),
           ingreso_total_individual_M = ifelse(sexo == "Mujeres", ingreso_total_individual, NA),
           ingreso_real_total_individual_H = ifelse(sexo == "Varones", ingreso_real_total_individual, NA),
           ingreso_real_total_individual_M = ifelse(sexo == "Mujeres", ingreso_real_total_individual, NA),
           ingreso_no_laborable_H = ifelse(sexo == "Varones", ingreso_no_laborable, NA),
           ingreso_no_laborable_M = ifelse(sexo == "Mujeres", ingreso_no_laborable, NA),
           ingreso_real_no_laborable_H = ifelse(sexo == "Varones", ingreso_real_no_laborable, NA),
           ingreso_real_no_laborable_M = ifelse(sexo == "Mujeres", ingreso_real_no_laborable, NA)) %>%
      # cruce con NE (nivel educativo) ----
    mutate( 
      #Sin instruccion
      ingreso_ocupacion_principal_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción", ingreso_real_no_laborable, NA),
      #PI
      ingreso_ocupacion_principal_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", ingreso_real_no_laborable, NA),
      #PC
      ingreso_ocupacion_principal_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa", ingreso_real_no_laborable, NA),
      #SI
      ingreso_ocupacion_principal_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", ingreso_real_no_laborable, NA),
      #SC
      ingreso_ocupacion_principal_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", ingreso_real_no_laborable, NA),
      #TI
      ingreso_ocupacion_principal_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", ingreso_real_no_laborable, NA),
      #TC
      ingreso_ocupacion_principal_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo", ingreso_real_no_laborable, NA),
      #UI
      ingreso_ocupacion_principal_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", ingreso_real_no_laborable, NA),
      #UC
      ingreso_ocupacion_principal_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_ocupacion_principal, NA),
      ingreso_real_ocupacion_principal_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_real_ocupacion_principal, NA),
      ingreso_otras_ocupaciones_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_otras_ocupaciones, NA),
      ingreso_real_otras_ocupaciones_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_real_otras_ocupaciones, NA),
      ingreso_total_individual_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_total_individual, NA),
      ingreso_real_total_individual_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_real_total_individual, NA),
      ingreso_no_laborable_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_no_laborable, NA),
      ingreso_real_no_laborable_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", ingreso_real_no_laborable, NA)) %>%
      # cruce con Cond de formalidad ----
    mutate(ingreso_ocupacion_principal_CF = ifelse(condicion_formalidad == "Formal", ingreso_ocupacion_principal, NA),
           ingreso_ocupacion_principal_CI = ifelse(condicion_formalidad == "No formal", ingreso_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_CF = ifelse(condicion_formalidad == "Formal", ingreso_real_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_CI = ifelse(condicion_formalidad == "No formal", ingreso_real_ocupacion_principal, NA),
           ingreso_otras_ocupaciones_CF = ifelse(condicion_formalidad == "Formal", ingreso_otras_ocupaciones, NA),
           ingreso_otras_ocupaciones_CI = ifelse(condicion_formalidad == "No formal", ingreso_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_CF = ifelse(condicion_formalidad == "Formal", ingreso_real_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_CI = ifelse(condicion_formalidad == "No formal", ingreso_real_otras_ocupaciones, NA),
           ingreso_total_individual_CF = ifelse(condicion_formalidad == "Formal", ingreso_total_individual, NA),
           ingreso_total_individual_CI = ifelse(condicion_formalidad == "No formal", ingreso_total_individual, NA),
           ingreso_real_total_individual_CF = ifelse(condicion_formalidad == "Formal", ingreso_real_total_individual, NA),
           ingreso_real_total_individual_CI = ifelse(condicion_formalidad == "No formal", ingreso_real_total_individual, NA)) %>%
      # cruce con Tipo de empleo ----
    mutate(ingreso_ocupacion_principal_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_ocupacion_principal, NA),
           ingreso_ocupacion_principal_EmPubl = ifelse(tipo_empleo == "Público", ingreso_ocupacion_principal, NA),
           ingreso_ocupacion_principal_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_real_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_EmPubl = ifelse(tipo_empleo == "Público", ingreso_real_ocupacion_principal, NA),
           ingreso_real_ocupacion_principal_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_real_ocupacion_principal, NA),
           ingreso_otras_ocupaciones_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_otras_ocupaciones, NA),
           ingreso_otras_ocupaciones_EmPubl = ifelse(tipo_empleo == "Público", ingreso_otras_ocupaciones, NA),
           ingreso_otras_ocupaciones_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_real_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_EmPubl = ifelse(tipo_empleo == "Público", ingreso_real_otras_ocupaciones, NA),
           ingreso_real_otras_ocupaciones_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_real_otras_ocupaciones, NA),
           ingreso_total_individual_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_total_individual, NA),
           ingreso_total_individual_EmPubl = ifelse(tipo_empleo == "Público", ingreso_total_individual, NA),
           ingreso_total_individual_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_total_individual, NA),
           ingreso_real_total_individual_EmPriv = ifelse(tipo_empleo == "Privado", ingreso_real_total_individual, NA),
           ingreso_real_total_individual_EmPubl = ifelse(tipo_empleo == "Público", ingreso_real_total_individual, NA),
           ingreso_real_total_individual_EmOtro = ifelse(tipo_empleo == "Otro tipo", ingreso_real_total_individual, NA)) %>%
      # cruce con Rango etario ----
    mutate( 
        #No corresponde
        ingreso_ocupacion_principal_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReNo = ifelse(edad_categoria_intervalo == "0-9", ingreso_real_total_individual, NA),
        #Menor
        ingreso_ocupacion_principal_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReMe = ifelse(edad_categoria_intervalo == "10-13", ingreso_real_total_individual, NA),
        #Joven
        ingreso_ocupacion_principal_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReJov = ifelse(edad_categoria_intervalo == "14-18", ingreso_real_total_individual, NA),
        #Junior
        ingreso_ocupacion_principal_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReJu = ifelse(edad_categoria_intervalo == "19-25", ingreso_real_total_individual, NA),
        #Semi Senior
        ingreso_ocupacion_principal_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReSemi = ifelse(edad_categoria_intervalo == "26-35", ingreso_real_total_individual, NA),
        #Senior
        ingreso_ocupacion_principal_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReSen = ifelse(edad_categoria_intervalo == "36-45", ingreso_real_total_individual, NA),
        #Especialista I
        ingreso_ocupacion_principal_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55", ingreso_real_total_individual, NA),
        #Especialista II
        ingreso_ocupacion_principal_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65", ingreso_real_total_individual, NA),
        #En edad jubilatoria
        ingreso_ocupacion_principal_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_ocupacion_principal, NA),
        ingreso_real_ocupacion_principal_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_real_ocupacion_principal, NA),
        ingreso_otras_ocupaciones_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_otras_ocupaciones, NA),
        ingreso_real_otras_ocupaciones_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_real_otras_ocupaciones, NA),
        ingreso_total_individual_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_total_individual, NA),
        ingreso_real_total_individual_ReJub = ifelse(edad_categoria_intervalo == "66+", ingreso_real_total_individual, NA))  %>%
      # sexo ####
    mutate(mujer = ifelse(sexo == "Mujeres", pondera, 0),
           hombre = ifelse(sexo == "Varones", pondera, 0)) %>%
      # Cruce con NE ----
    mutate(hombre_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & sexo == "Varones", pondera, 0),
           hombre_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & sexo == "Varones", pondera, 0),
           hombre_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & sexo == "Varones", pondera, 0),
           hombre_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & sexo == "Varones", pondera, 0),
           hombre_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & sexo == "Varones", pondera, 0),
           hombre_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & sexo == "Varones", pondera, 0),
           hombre_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & sexo == "Varones", pondera, 0),
           hombre_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & sexo == "Varones", pondera, 0),
           hombre_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & sexo == "Varones", pondera, 0),
           mujer_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & sexo == "Mujeres", pondera, 0),
           mujer_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & sexo == "Mujeres", pondera, 0),
           mujer_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & sexo == "Mujeres", pondera, 0),
           mujer_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & sexo == "Mujeres", pondera, 0),
           mujer_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & sexo == "Mujeres", pondera, 0),
           mujer_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & sexo == "Mujeres", pondera, 0),
           mujer_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & sexo == "Mujeres", pondera, 0),
           mujer_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & sexo == "Mujeres", pondera, 0),
           mujer_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & sexo == "Mujeres", pondera, 0)) %>%
      # Cruce con condicion de formalidad ----
    mutate(hombre_CF = ifelse(condicion_formalidad == "Formal" & sexo == "Varones", pondera, 0),
           hombre_CI = ifelse(condicion_formalidad == "No formal" & sexo == "Varones", pondera, 0),
           mujer_CF = ifelse(condicion_formalidad == "Formal" & sexo == "Mujeres", pondera, 0),
           mujer_CI = ifelse(condicion_formalidad == "No formal" & sexo == "Mujeres", pondera, 0)) %>%
      # Cruce con tipo de empleo ----
    mutate(hombre_EmPriv = ifelse(tipo_empleo == "Privado" & sexo == "Varones", pondera, 0),
           hombre_EmPubl = ifelse(tipo_empleo == "Público" & sexo == "Varones", pondera, 0),
           hombre_EmOtro = ifelse(tipo_empleo == "Otro tipo" & sexo == "Varones", pondera, 0),
           mujer_EmPriv = ifelse(tipo_empleo == "Privado" & sexo == "Mujeres", pondera, 0),
           mujer_EmPubl = ifelse(tipo_empleo == "Público" & sexo == "Mujeres", pondera, 0),
           mujer_EmOtro = ifelse(tipo_empleo == "Otro tipo" & sexo == "Mujeres", pondera, 0)) %>%
      # Cruce con categoria ocupacional ----
    mutate(hombre_CatPat = ifelse(categoria_ocupacional2 == "Patrón" & sexo == "Varones", pondera, 0),
           hombre_CatCta = ifelse(categoria_ocupacional2 == "Cuenta Propia" & sexo == "Varones", pondera, 0),
           hombre_CatEmpl = ifelse(categoria_ocupacional2 == "Empleado" & sexo == "Varones", pondera, 0),
           hombre_CatFam = ifelse(categoria_ocupacional2 == "Familiar" & sexo == "Varones", pondera, 0),
           mujer_CatPat = ifelse(categoria_ocupacional2 == "Patrón" & sexo == "Mujeres", pondera, 0),
           mujer_CatCta = ifelse(categoria_ocupacional2 == "Cuenta Propia" & sexo == "Mujeres", pondera, 0),
           mujer_CatEmpl = ifelse(categoria_ocupacional2 == "Empleado" & sexo == "Mujeres", pondera, 0),
           mujer_CatFam = ifelse(categoria_ocupacional2 == "Familiar" & sexo == "Mujeres", pondera, 0)) %>%
      # Cruce con Rango etario ----
    mutate(hombre_ReNo = ifelse(edad_categoria_intervalo == "0-9" & sexo == "Varones", pondera, 0),
           hombre_ReMe = ifelse(edad_categoria_intervalo == "10-13" & sexo == "Varones", pondera, 0),
           hombre_ReJov = ifelse(edad_categoria_intervalo == "14-18" & sexo == "Varones", pondera, 0),
           hombre_ReJu = ifelse(edad_categoria_intervalo == "19-25" & sexo == "Varones", pondera, 0),
           hombre_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & sexo == "Varones", pondera, 0),
           hombre_ReSen = ifelse(edad_categoria_intervalo == "36-45" & sexo == "Varones", pondera, 0),
           hombre_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & sexo == "Varones", pondera, 0),
           hombre_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & sexo == "Varones", pondera, 0),
           hombre_ReJub = ifelse(edad_categoria_intervalo == "66+" & sexo == "Varones", pondera, 0),
           mujer_ReNo = ifelse(edad_categoria_intervalo == "0-9" & sexo == "Mujeres", pondera, 0),
           mujer_ReMe = ifelse(edad_categoria_intervalo == "10-13" & sexo == "Mujeres", pondera, 0),
           mujer_ReJov = ifelse(edad_categoria_intervalo == "14-18" & sexo == "Mujeres", pondera, 0),
           mujer_ReJu = ifelse(edad_categoria_intervalo == "19-25" & sexo == "Mujeres", pondera, 0),
           mujer_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & sexo == "Mujeres", pondera, 0),
           mujer_ReSen = ifelse(edad_categoria_intervalo == "36-45" & sexo == "Mujeres", pondera, 0),
           mujer_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & sexo == "Mujeres", pondera, 0),
           mujer_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & sexo == "Mujeres", pondera, 0),
           mujer_ReJub = ifelse(edad_categoria_intervalo == "66+" & sexo == "Mujeres", pondera, 0)) %>%
      # CONDICIÓN DE FORMALIDAD ####
    mutate(Formal = ifelse(condicion_formalidad == "Formal", pondera, 0),
           NoFormal = ifelse(condicion_formalidad == "No formal", pondera, 0),
           EmpleadoNoCorresponde = ifelse(condicion_formalidad == "No corresponde", pondera, 0)) %>%
      # Cruce con NE ----
    mutate(formal_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & condicion_formalidad == "Formal", pondera, 0),
           formal_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & condicion_formalidad == "Formal", pondera, 0),
           informal_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & condicion_formalidad == "No formal", pondera, 0),
           informal_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & condicion_formalidad == "No formal", pondera, 0)) %>%
      # Cruce con tipo de empleo ----
    mutate(formal_EmPriv = ifelse(tipo_empleo == "Privado" & condicion_formalidad == "Formal", pondera, 0),
           formal_EmPubl = ifelse(tipo_empleo == "Público" & condicion_formalidad == "Formal", pondera, 0),
           formal_EmOtro = ifelse(tipo_empleo == "Otro tipo" & condicion_formalidad == "Formal", pondera, 0),
           informal_EmPriv = ifelse(tipo_empleo == "Privado" & condicion_formalidad == "No formal", pondera, 0),
           informal_EmPubl = ifelse(tipo_empleo == "Público" & condicion_formalidad == "No formal", pondera, 0),
           informal_EmOtro = ifelse(tipo_empleo == "Otro tipo" & condicion_formalidad == "No formal", pondera, 0)) %>%
      # Cruce con Rango etario ----
    mutate(formal_ReNo = ifelse(edad_categoria_intervalo == "0-9" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReMe = ifelse(edad_categoria_intervalo == "10-13" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReJov = ifelse(edad_categoria_intervalo == "14-18" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReJu = ifelse(edad_categoria_intervalo == "19-25" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReSen = ifelse(edad_categoria_intervalo == "36-45" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & condicion_formalidad == "Formal", pondera, 0),
           formal_ReJub = ifelse(edad_categoria_intervalo == "66+" & condicion_formalidad == "Formal", pondera, 0),
           informal_ReNo = ifelse(edad_categoria_intervalo == "0-9" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReMe = ifelse(edad_categoria_intervalo == "10-13" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReJov = ifelse(edad_categoria_intervalo == "14-18" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReJu = ifelse(edad_categoria_intervalo == "19-25" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReSen = ifelse(edad_categoria_intervalo == "36-45" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & condicion_formalidad == "No formal", pondera, 0),
           informal_ReJub = ifelse(edad_categoria_intervalo == "66+" & condicion_formalidad == "No formal", pondera, 0)) %>%
      # TIPO DE EMPLEO ####
    mutate(Publico = ifelse(tipo_empleo == "Público", pondera, 0),
           Privado = ifelse(tipo_empleo == "Privado", pondera, 0),
           OtroTipo = ifelse(tipo_empleo == "Otro tipo", pondera, 0)) %>%
      # Cruce con NE ----
    mutate(publico_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & tipo_empleo == "Público", pondera, 0),
           publico_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & tipo_empleo == "Público", pondera, 0),
           publico_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & tipo_empleo == "Público", pondera, 0),
           publico_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & tipo_empleo == "Público", pondera, 0),
           publico_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & tipo_empleo == "Público", pondera, 0),
           publico_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & tipo_empleo == "Público", pondera, 0),
           publico_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & tipo_empleo == "Público", pondera, 0),
           publico_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & tipo_empleo == "Público", pondera, 0),
           publico_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & tipo_empleo == "Público", pondera, 0),
           privado_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & tipo_empleo == "Privado", pondera, 0),
           privado_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & tipo_empleo == "Privado", pondera, 0),
           privado_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & tipo_empleo == "Privado", pondera, 0),
           privado_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & tipo_empleo == "Privado", pondera, 0),
           privado_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & tipo_empleo == "Privado", pondera, 0),
           privado_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & tipo_empleo == "Privado", pondera, 0),
           privado_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & tipo_empleo == "Privado", pondera, 0),
           privado_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & tipo_empleo == "Privado", pondera, 0),
           privado_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & tipo_empleo == "Privado", pondera, 0),
           otro_tipo_empleo_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & tipo_empleo == "Otro tipo", pondera, 0),
           otro_tipo_empleo_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & tipo_empleo == "Otro tipo", pondera, 0)) %>%
      # Cruce con Rango etario ----
    mutate(Publico_ReNo = ifelse(edad_categoria_intervalo == "0-9" & tipo_empleo == "Público", pondera, 0),
           Publico_ReMe = ifelse(edad_categoria_intervalo == "10-13" & tipo_empleo == "Público", pondera, 0),
           Publico_ReJov = ifelse(edad_categoria_intervalo == "14-18" & tipo_empleo == "Público", pondera, 0),
           Publico_ReJu = ifelse(edad_categoria_intervalo == "19-25" & tipo_empleo == "Público", pondera, 0),
           Publico_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & tipo_empleo == "Público", pondera, 0),
           Publico_ReSen = ifelse(edad_categoria_intervalo == "36-45" & tipo_empleo == "Público", pondera, 0),
           Publico_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & tipo_empleo == "Público", pondera, 0),
           Publico_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & tipo_empleo == "Público", pondera, 0),
           Publico_ReJub = ifelse(edad_categoria_intervalo == "66+" & tipo_empleo == "Público", pondera, 0),
           Privado_ReNo = ifelse(edad_categoria_intervalo == "0-9" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReMe = ifelse(edad_categoria_intervalo == "10-13" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReJov = ifelse(edad_categoria_intervalo == "14-18" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReJu = ifelse(edad_categoria_intervalo == "19-25" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReSen = ifelse(edad_categoria_intervalo == "36-45" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & tipo_empleo == "Privado", pondera, 0),
           Privado_ReJub = ifelse(edad_categoria_intervalo == "66+" & tipo_empleo == "Privado", pondera, 0),
           OtroTipo_ReNo = ifelse(edad_categoria_intervalo == "0-9" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReMe = ifelse(edad_categoria_intervalo == "10-13" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReJov = ifelse(edad_categoria_intervalo == "14-18" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReJu = ifelse(edad_categoria_intervalo == "19-25" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReSen = ifelse(edad_categoria_intervalo == "36-45" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & tipo_empleo == "Otro tipo", pondera, 0),
           OtroTipo_ReJub = ifelse(edad_categoria_intervalo == "66+" & tipo_empleo == "Otro tipo", pondera, 0)) %>%
      # NIVEL EDUCATIVO ----
    mutate(UniversitarioCompleto = ifelse(nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
           UniversitarioIncompleto = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
           TerciarioCompleto = ifelse(nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
           TerciarioIncompleto = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
           SecundarioCompleto = ifelse(nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
           SecundarioIncompleto = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
           PrimarioCompleto = ifelse(nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
           PrimarioIncompleto = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
           SinInstruccion = ifelse(nivel_educ_obtenido2 == "Sin instrucción", pondera, 0)) %>%
      # Cruce con Rango etario ----
    mutate(
      # UC
      UniversitarioCompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      UniversitarioCompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Universitaria Completa", pondera, 0),
      # UI
      UniversitarioIncompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Universitaria Incpleta", pondera, 0),
      UniversitarioIncompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      UniversitarioIncompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Universitaria Incompleta", pondera, 0),
      # TC
      TerciarioCompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      TerciarioCompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Terciario Completo", pondera, 0),
      # TI
      TerciarioIncompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      TerciarioIncompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Terciario Incompleto", pondera, 0),
      # SC
      SecundarioCompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      SecundarioCompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Secundaria Completa", pondera, 0),
      # SI
      SecundarioIncompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      SecundarioIncompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Secundaria Incompleta", pondera, 0),
      # PC
      PrimarioCompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      PrimarioCompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Primaria Completa", pondera, 0),
      # PI
      PrimarioIncompleto_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      PrimarioIncompleto_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Primaria Incompleta", pondera, 0),
      # SI
      SinInstruccion_ReNo = ifelse(edad_categoria_intervalo == "0-9" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReMe = ifelse(edad_categoria_intervalo == "10-13" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReJov = ifelse(edad_categoria_intervalo == "14-18" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReJu = ifelse(edad_categoria_intervalo == "19-25" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReSemi = ifelse(edad_categoria_intervalo == "26-35" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReSen = ifelse(edad_categoria_intervalo == "36-45" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReEsp1 = ifelse(edad_categoria_intervalo == "46-55" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReEsp2 = ifelse(edad_categoria_intervalo == "56-65" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0),
      SinInstruccion_ReJub = ifelse(edad_categoria_intervalo == "66+" & nivel_educ_obtenido2 == "Sin instrucción", pondera, 0)) %>%
      # CATEGORÍA OCUPACIONAL  ####
    mutate(patron = ifelse(categoria_ocupacional2 == "Patrón", pondera, 0),
           cta_propia = ifelse(categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           empleado = ifelse(categoria_ocupacional2 == "Empleado", pondera, 0),
           familiar = ifelse(categoria_ocupacional2 == "Familiar", pondera, 0)) %>%
      # Cruce con NE ----
    mutate(patron_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & categoria_ocupacional2 == "Patrón", pondera, 0),
           patron_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & categoria_ocupacional2 == "Patrón", pondera, 0),
           cta_propia_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           cta_propia_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & categoria_ocupacional2 == "Cuenta Propia", pondera, 0),
           empleado_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & categoria_ocupacional2 == "Empleado", pondera, 0),
           familiar_EdUC = ifelse(nivel_educ_obtenido2 == "Universitaria Completa" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdUI = ifelse(nivel_educ_obtenido2 == "Universitaria Incompleta" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdTI = ifelse(nivel_educ_obtenido2 == "Terciario Incompleto" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdTC = ifelse(nivel_educ_obtenido2 == "Terciario Completo" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdSC = ifelse(nivel_educ_obtenido2 == "Secundaria Completa" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdSI = ifelse(nivel_educ_obtenido2 == "Secundaria Incompleta" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdPC = ifelse(nivel_educ_obtenido2 == "Primaria Completa" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdPI = ifelse(nivel_educ_obtenido2 == "Primaria Incompleta" & categoria_ocupacional2 == "Familiar", pondera, 0),
           familiar_EdNo = ifelse(nivel_educ_obtenido2 == "Sin instrucción" & categoria_ocupacional2 == "Familiar", pondera, 0)) %>%
      # Cruce Empleado con condicion de formaliad ----
    mutate(empleado_CF = ifelse(condicion_formalidad == "Formal" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_CI = ifelse(condicion_formalidad == "No formal" & categoria_ocupacional2 == "Empleado", pondera, 0)) %>%
      # Cruce Empelado con tipo de empleo ----
    mutate(empleado_EmPriv = ifelse(tipo_empleo == "Privado" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EmPubl = ifelse(tipo_empleo == "Público" & categoria_ocupacional2 == "Empleado", pondera, 0),
           empleado_EmOtro = ifelse(tipo_empleo == "Otro tipo" & categoria_ocupacional2 == "Empleado", pondera, 0)) 
    
    # Guardo lo hecho en la base original ----
    datos <- capa3i
    

# RESUMEN ----


### NOTA    
  
    ## INDIVIDUOS ----
    # Ingresos ----
    ingresos <- capa3i %>%
      group_by(aglomerado) %>%
      summarise(
        
        #Ingresos pondiio
        across(
          .cols = c(ingreso_duenio_sin_socio, ingreso_real_duenio_sin_socio,
                    ingreso_duenio_con_socio, ingreso_real_duenio_con_socio,
                    ingreso_ocupacion_principal, ingreso_real_ocupacion_principal,
                    ingreso_otras_ocupaciones, ingreso_real_otras_ocupaciones), 
          .fns = list(
            m = ~weighted.mean(., pondiio, na.rm = TRUE),
            Md = ~median(rep(., times = pondiio), na.rm = TRUE)
          ),
          .names = "{col}_{fn}"
        ),
        
        #Ingresos pondii
        across(
          .cols = c(ingreso_total_individual, ingreso_real_total_individual),
          .fns = list(
            m = ~weighted.mean(., pondii, na.rm = TRUE),
            Md = ~median(rep(., times = pondii), na.rm = TRUE)
          ),
          .names = "{col}_{fn}"
        ),
        
        #Ingresos pondera
        across(
          .cols = c(ingreso_jubilacion, ingreso_real_jubilacion, ingreso_negocio,
                    ingreso_real_negocio, ingreso_renta, ingreso_real_renta, 
                    ingreso_otro, ingreso_real_otro, ingreso_menores, 
                    ingreso_real_menores, ingreso_no_laborable, ingreso_real_no_laborable),
          .fns = list(
            m = ~weighted.mean(., pondera, na.rm = TRUE),
            Md = ~median(rep(., times = pondera), na.rm = TRUE)
          ),
          .names = "{col}_{fn}"
        )
      ) %>% 
      select(aglomerado, ends_with("_m"), ends_with("_Md"))
    
    
    # Cruza de ingresos ----
    #usamos summarise_at porque pisamos variables que ya existen
    cruces_pondera <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(
          starts_with("ingreso_no_laborable_"),
          starts_with("ingreso_real_no_laborable_")
        ),
        .funs = ~weighted.mean(., w = pondera, na.rm = TRUE)
      )
    
    cruces_pondii <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(
          starts_with("ingreso_total_individual_"),
          starts_with("ingreso_real_total_individual_")
        ),
        .funs = ~weighted.mean(., w = pondii, na.rm = TRUE)
      )
    
    cruces_pondiio <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(starts_with("ingreso_ocupacion_principal_"),
                     starts_with("ingreso_otras_ocupaciones_"),
                     starts_with("ingreso_real_ocupacion_principal_"),
                     starts_with("ingreso_real_otras_ocupaciones_")),
        .funs = ~weighted.mean(., w = pondiio, na.rm = TRUE)
      )
    
    # Sexo ----
    sexo1 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise(
        mujer = sum(mujer, na.rm = T),
        mujer_Part = sum(mujer, na.rm = T)/sum(pondera),
        hombre = sum(hombre, na.rm = T),
        hombre_Part = sum(hombre, na.rm = T)/sum(pondera)
      )
    
    sexo2 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(starts_with("hombre_"), starts_with("mujer_")),
        .funs = ~sum(., na.rm = TRUE)
      )
    
    # Formalidad ----
    form1 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise(
        formal = sum(Formal, na.rm = T),
        informal = sum(NoFormal, na.rm = T),
        formal_Part = sum(Formal, na.rm = T)/sum(pondera),
        informal_Part = sum(NoFormal, na.rm = T)/sum(pondera),
      )
    
    form2 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(starts_with("formal_"), starts_with("informal_")),
        .funs = ~sum(., na.rm = TRUE)
      )
    
    # Empleo ----
    empleo1 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise(
        publico = sum(Publico, na.rm = T),
        privado = sum(Privado, na.rm = T),
        otro_tipo_empleo = sum(OtroTipo, na.rm = T), 
        publico_Part = sum(Publico, na.rm = T)/sum(pondera),
        privado_Part = sum(Privado, na.rm = T)/sum(pondera),
        otro_tipo_empleo_Part = sum(OtroTipo, na.rm = T)/sum(pondera), 
      )
    
    empleo2 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(starts_with("publico_", ignore.case = F), 
                     starts_with("privado_", ignore.case = F), 
                     starts_with("otro_tipo_empleo_"), 
                     starts_with("OtroTipo_Re"),
                     starts_with("Publico_Re", ignore.case = F), 
                     starts_with("Privado_Re", ignore.case = F)
        ),
        .funs = ~sum(., na.rm = TRUE)
      )
    
    # Nivel educativo ----
    educ1 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(ends_with("Completo"), SinInstruccion),
        .funs = ~sum(., na.rm = TRUE)
      )
    
    educ2 <- capa3i %>%
      group_by(aglomerado) %>%
      summarise(
        across(
          .cols = c(ends_with("Completo"), SinInstruccion), 
          .fns = list(Part = ~sum(., na.rm = TRUE)/sum(pondera)),
          .names = "{col}_{fn}"
        )
      )
    
    educ3 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(
          starts_with("Universitario") & contains("_"),
          starts_with("Terciario") & contains("_"),
          starts_with("Secundario") & contains("_"),
          starts_with("Primario") & contains("_"),
          starts_with("SinInstruccion") & contains("_")
        ),
        .funs = ~sum(., na.rm = TRUE)
      )
    
    # Categoría ocupacional ----
    catoc1 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise(
        patron = sum(patron, na.rm = T),
        cta_propia = sum(cta_propia, na.rm = T),
        empleado = sum(empleado, na.rm = T),
        familiar = sum(familiar, na.rm = T),
        patron_Part = sum(patron, na.rm = T)/sum(pondera),
        cta_propia_Part = sum(cta_propia, na.rm = T)/sum(pondera),
        empleado_Part = sum(empleado, na.rm = T)/sum(pondera),
        familiar_Part = sum(familiar, na.rm = T)/sum(pondera)
      )
    
    catoc2 <- capa3i %>%
      group_by(aglomerado) %>% 
      summarise_at(
        .vars = vars(
          starts_with("patron_"),
          starts_with("cta_propia_"),
          starts_with("empleado_"),
          starts_with("familiar_")
        ),
        .funs = ~sum(., na.rm = TRUE)
      )
    
  ## HOGARES ----
    hogares <- datos %>%
      filter(!duplicated(id_hogar)) %>% 
      group_by(aglomerado) %>%
      summarise(
        
        across(
          .cols = c(ingreso_familiar, ingreso_real_familiar, 
                    ingreso_capita_familiar, ingreso_real_capita_familiar), 
          .fns = list(
            m = ~weighted.mean(., pondih, na.rm = TRUE),
            Md = ~median(rep(., times = pondih), na.rm = TRUE)
          ),
          .names = "{col}_{fn}"
        )
      )
    
    ## UNION ----
    capa3 <- cruces_pondera %>% 
      full_join(cruces_pondii) %>% 
      full_join(cruces_pondiio) %>% 
      full_join(ingresos) %>% 
      full_join(sexo1) %>% 
      full_join(sexo2) %>% 
      full_join(form1) %>% 
      full_join(form2) %>% 
      full_join(empleo1) %>% 
      full_join(empleo2) %>% 
      full_join(educ1) %>% 
      full_join(educ2) %>% 
      full_join(educ3) %>% 
      full_join(catoc1) %>% 
      full_join(catoc2) %>% 
      full_join(hogares) %>% 
      mutate(
        periodo = paste0("20", periodo$anio[i], "_T", periodo$trimestre[i]),
        aglomerado = as.character(aglomerado)
      )
    
    # Unifico períodos en un archivo ----
    if (i == 1) {
      salida <- capa3
    } else {
        salida <- rbind(salida, capa3)
        }
    setwd(ruta_results)
    save(datos, file = file_name)
    save(capa3, file = exit_name)
}

# Luego, guardo el archivo unificado ----
save(salida, file = "capa3.csv")