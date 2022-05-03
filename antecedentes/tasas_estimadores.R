# https://diegokoz.github.io/R_EPH_bookdown/tidyverse-y-tasas-basicas-del-mercado-de-trabajo.html


# Librerías ----
library(tables)
library(dplyr)
library(tidyr)
library(labelled)
library(purrr)
library(stringr)

## Carpetas ####
ruta_gral <- "C:/oes-MonitorEPH"
ruta_data <- "C:/oes/capa2/"
ruta_results <- "C:/oes/capa3/"
ruta_graf <- paste0(ruta_gral, "/graf/")
ruta_cod <- paste0(ruta_gral, "/cod/")

setwd(ruta_data)

# Comandos ----
anio <- c(16, rep(17,4), rep(18,4), rep(19,4), rep(20,4), rep(21,2))
trimestre <- c(4, 1:4, 1:4, 1:4, 1:4, 1:2)
periodo <- tibble(anio, trimestre)
decil <- 1:10

i <- 14

# Funciones a utilizar ----

fabs <- function (x, y, p) {
  case_when(x == y ~ p,
            x != y ~ 0,
            is.na(x) ~ 0)
}

## Aplicadas a cruces ----
sexo <- function(x, y = "Mujeres") {
  if (y == "Mujeres") {
    case_when(
      x > 0 & datos$sexo == "Mujeres" ~ x,
      x > 0 & datos$sexo != "Mujeres" ~ 0,
      x > 0 & is.na(datos$sexo) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Varones") {
    case_when(
      x > 0 & datos$sexo == "Varones" ~ x,
      x > 0 & datos$sexo != "Varones" ~ 0,
      x > 0 & is.na(datos$sexo) ~ 0,
      x == 0 ~ 0
    )
  }
}

educacion <- function(x, y = "Sin instrucción") {
  if (y == "Sin instrucción") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Sin instrucción",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Primaria Incompleta") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Incompleta",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Primaria Completa") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Primaria Completa",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Secundaria Incompleta") {
    ifelse(
      test = x > 0 &
        datos$nivel_educ_obtenido2 == "Secundaria Incompleta",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Secundaria Completa") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Secundaria Completa",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Terciario Incompleto") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Incompleto",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Terciario Completo") {
    ifelse(
      test = x > 0 & datos$nivel_educ_obtenido2 == "Terciario Completo",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Universitaria Incompleta") {
    ifelse(
      test = x > 0 &
        datos$nivel_educ_obtenido2 == "Universitaria Incompleta",
      yes = x,
      no = 0
    )
  }
  
  else if (y == "Universitaria Completa") {
    ifelse(
      test = x > 0 &
        datos$nivel_educ_obtenido2 == "Universitaria Completa",
      yes = x,
      no = 0
    )
  }
}

etario <- function (x, y = "No corresponde") {
  if (y == "No corresponde") {
    case_when(
      x > 0 & datos$edad_categoria == "No corresponde" ~ x,
      x > 0 & datos$edad_categoria != "No corresponde" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Menor") {
    case_when(
      x > 0 & datos$edad_categoria == "Menor" ~ x,
      x > 0 & datos$edad_categoria != "Menor" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Joven") {
    case_when(
      x > 0 & datos$edad_categoria == "Joven" ~ x,
      x > 0 & datos$edad_categoria != "Joven" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Junior") {
    case_when(
      x > 0 & datos$edad_categoria == "Junior" ~ x,
      x > 0 & datos$edad_categoria != "Junior" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Semi senior") {
    case_when(
      x > 0 & datos$edad_categoria == "Semi senior" ~ x,
      x > 0 & datos$edad_categoria != "Semi senior" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Senior") {
    case_when(
      x > 0 & datos$edad_categoria == "Senior" ~ x,
      x > 0 & datos$edad_categoria != "Senior" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Especialista I") {
    case_when(
      x > 0 & datos$edad_categoria == "Especialista I" ~ x,
      x > 0 & datos$edad_categoria != "Especialista I" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Especialista II") {
    case_when(
      x > 0 & datos$edad_categoria == "Especialista II" ~ x,
      x > 0 & datos$edad_categoria != "Especialista II" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "En edad jubilatoria") {
    case_when(
      x > 0 & datos$edad_categoria == "En edad jubilatoria" ~ x,
      x > 0 & datos$edad_categoria != "En edad jubilatoria" ~ 0,
      x > 0 & is.na(datos$edad_categoria) ~ 0,
      x == 0 ~ 0
    )
  }
}

formalidad <- function (x, y = "Formal") {
  if (y == "Formal") {
    case_when(
      x > 0 & datos$condicion_formalidad == "Formal" ~ x,
      x > 0 & datos$condicion_formalidad != "Formal" ~ 0,
      x > 0 & is.na(datos$condicion_formalidad) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "No formal") {
    case_when(
      x > 0 & datos$condicion_formalidad == "No formal" ~ x,
      x > 0 & datos$condicion_formalidad != "No formal" ~ 0,
      x > 0 & is.na(datos$condicion_formalidad) ~ 0,
      x == 0 ~ 0
    )
  }
}

tipo_empleo <- function (x, y = "Privado") {
  if (y == "Privado") {
    case_when(
      x > 0 & datos$tipo_empleo == "Privado" ~ x,
      x > 0 & datos$tipo_empleo != "Privado" ~ 0,
      x > 0 & is.na(datos$tipo_empleo) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Público") {
    case_when(
      x > 0 & datos$tipo_empleo == "Público" ~ x,
      x > 0 & datos$tipo_empleo != "Público" ~ 0,
      x > 0 & is.na(datos$tipo_empleo) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Otro tipo") {
    case_when(
      x > 0 & datos$tipo_empleo == "Otro tipo" ~ x,
      x > 0 & datos$tipo_empleo != "Otro tipo" ~ 0,
      x > 0 & is.na(datos$tipo_empleo) ~ 0,
      x == 0 ~ 0
    )
  }
}

cat_ocupacional <- function (x, y = "Patrón") {
  if (y == "Patrón") {
    case_when(
      x > 0 & datos$categoria_ocupacional2 == "Patrón" ~ x,
      x > 0 & datos$categoria_ocupacional2 != "Patrón" ~ 0,
      x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Cuenta Propia") {
    case_when(
      x > 0 & datos$categoria_ocupacional2 == "Cuenta Propia" ~ x,
      x > 0 & datos$categoria_ocupacional2 != "Cuenta Propia" ~ 0,
      x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Empleado") {
    case_when(
      x > 0 & datos$categoria_ocupacional2 == "Empleado" ~ x,
      x > 0 & datos$categoria_ocupacional2 != "Empleado" ~ 0,
      x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
      x == 0 ~ 0
    )
  }
  
  else if (y == "Familiar") {
    case_when(
      x > 0 & datos$categoria_ocupacional2 == "Familiar" ~ x,
      x > 0 & datos$categoria_ocupacional2 != "Familiar" ~ 0,
      x > 0 & is.na(datos$categoria_ocupacional2) ~ 0,
      x == 0 ~ 0
    )
  }
}

## Gini ----
Gini <- function(x, peso) {
  z <- as.data.frame(cbind(rta = all_of(x), w = all_of(peso)))
  z <- arrange(z, rta)
  
  aux0 <- (2 * cumsum(z$w) - z$w + 1) / 2
  aux1 <- sum(z$w)
  aux2 <- z$rta * (aux1 - aux0 + 1)
  aux3 <- sum(aux2 * z$w)
  media <- weighted.mean(x = z$rta,
                         w = z$w,
                         na.rm = TRUE)
  
  1 + (1 / aux1) - (2 / (media * aux1 ^ 2)) * aux3
}

# Iteración ----
for (i in nrow(periodo)) {
  ## Archivo para invocar ####
  file_name <-
    paste0("EPH20", periodo$anio[i], "_T", periodo$trimestre[i], ".RData")
  exit_name <-
    paste0("Capa3_20",
           periodo$anio[i],
           "_T",
           periodo$trimestre[i],
           ".RData")
  ## Carga archivo ####
  file <- paste0(ruta_data, file_name)
  load(file)
  # tasas general ----
  datos <- datos %>%
    mutate(
      poblacion_ocupada = fabs(condicion_actividad, "Ocupado", pondera),
      poblacion_desocupada = fabs(condicion_actividad, "Desocupado", pondera),
      #poblacion_inactiva = fabs(condicion_actividad2, "Inactivo", pondera),
      poblacion_subocupada = fabs(intensidad_trabajo, "Subocupado", pondera),
      poblacion_asalariada = fabs(asalariado, "Si", pondera),
      poblacion_no_asalariada = fabs(asalariado, "No", pondera),
      ocupado_demandante = fabs(condicion_actividad2, "Ocupado demandante", pondera),
      ocupado_no_demandante = fabs(condicion_actividad2, "Ocupado no demandante", pondera),
      ocupado_no_demandante_ni_disponible = fabs(
        condicion_actividad2,
        "Ocupado no demandante ni disponible",
        pondera
      ),
      subocupado_demandante = fabs(condicion_actividad3, "Subocupado demandante", pondera),
      otro_ocupado_demandante = fabs(condicion_actividad3, "Otro ocupado demandante", pondera),
      subocupado_no_demandante = fabs(condicion_actividad3, "Subocupado no demandante", pondera),
      otro_ocupado_no_demandante = fabs(condicion_actividad3, "Otro ocupado no demandante", pondera),
      nbi1 = fabs(nbi, "Si", pondera),
      nbi2 = fabs(nbi_2, "Si", pondera)
    )
  
  
  # Resumen ----
  tasasG <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total = sum(pondera),
      poblacion_activa = sum(poblacion_ocupada) + sum(poblacion_desocupada),
      poblacion_inactiva = poblacion_total - poblacion_activa,
      poblacion_ocupada = sum(poblacion_ocupada),
      poblacion_subocupada = sum(poblacion_subocupada),
      poblacion_desocupada = sum(poblacion_desocupada),
      poblacion_asalariada = sum(poblacion_asalariada),
      poblacion_no_asalariada = sum(poblacion_no_asalariada),
      actividad = poblacion_activa / poblacion_total,
      inactividad = poblacion_inactiva / poblacion_total,
      desocupacion = sum(poblacion_desocupada) / poblacion_activa,
      empleo = sum(poblacion_ocupada) / poblacion_total,
      ocupado_demandante = sum(ocupado_demandante) / poblacion_activa,
      subocupado_demandante = sum(subocupado_demandante) / poblacion_activa,
      otro_ocupado_demandante = sum(otro_ocupado_demandante) / poblacion_activa,
      ocupado_no_demandante = sum(ocupado_no_demandante) / poblacion_activa,
      subocupado_no_demandante = sum(subocupado_no_demandante) /
        poblacion_activa,
      subocupado = sum(poblacion_subocupada) / poblacion_activa,
      otro_ocupado_no_demandante = sum(otro_ocupado_no_demandante) /
        poblacion_activa,
      presion_trabajo = (
        sum(poblacion_desocupada) + sum(ocupado_demandante) +
          sum(ocupado_no_demandante)
      ) / poblacion_activa,
      nbi1 = sum(nbi1) / poblacion_total,
      nbi2 = sum(nbi2) / poblacion_total
    )
  # Total pais ----
  tasasP <- datos %>%
    summarise(
      poblacion_total = sum(pondera),
      poblacion_activa = sum(poblacion_ocupada) + sum(poblacion_desocupada),
      poblacion_inactiva = poblacion_total - poblacion_activa,
      poblacion_ocupada = sum(poblacion_ocupada),
      poblacion_subocupada = sum(poblacion_subocupada),
      poblacion_desocupada = sum(poblacion_desocupada),
      poblacion_asalariada = sum(poblacion_asalariada),
      poblacion_no_asalariada = sum(poblacion_no_asalariada),
      actividad = poblacion_activa / poblacion_total,
      inactividad = poblacion_inactiva / poblacion_total,
      desocupacion = sum(poblacion_desocupada) / poblacion_activa,
      empleo = sum(poblacion_ocupada) / poblacion_total,
      ocupado_demandante = sum(ocupado_demandante) / poblacion_activa,
      subocupado_demandante = sum(subocupado_demandante) / poblacion_activa,
      otro_ocupado_demandante = sum(otro_ocupado_demandante) / poblacion_activa,
      ocupado_no_demandante = sum(ocupado_no_demandante) / poblacion_activa,
      subocupado_no_demandante = sum(subocupado_no_demandante) /
        poblacion_activa,
      subocupado = sum(poblacion_subocupada) / poblacion_activa,
      otro_ocupado_no_demandante = sum(otro_ocupado_no_demandante) /
        poblacion_activa,
      presion_trabajo = (
        sum(poblacion_desocupada) + sum(ocupado_demandante) +
          sum(ocupado_no_demandante)
      ) / poblacion_activa,
      nbi1 = sum(nbi1) / poblacion_total,
      nbi2 = sum(nbi2) / poblacion_total
    )
  # variables para cruce con sexo ----
  variablesS <-
    c(
      "pondera",
      "poblacion_ocupada",
      "poblacion_desocupada",
      "poblacion_subocupada",
      
      "poblacion_asalariada",
      "poblacion_no_asalariada",
      
      "ocupado_demandante",
      "ocupado_no_demandante",
      "ocupado_no_demandante_ni_disponible",
      
      "subocupado_demandante",
      "subocupado_no_demandante",
      
      "otro_ocupado_demandante",
      "otro_ocupado_no_demandante",
      
      "nbi1",
      "nbi2"
    )
  
  # Cruce con sexo ----
    # Varones ----
  tasa_varones <- datos %>%
    transmute_at(.vars = variablesS,
                 sexo, "Varones")
  names(tasa_varones) <-
    paste(names(tasa_varones), "H", sep = "_")
  
    # Mujeres ----
  tasa_mujeres <- datos %>%
    transmute_at(.vars = variablesS,
                 sexo, "Mujeres")
  names(tasa_mujeres) <-
    paste(names(tasa_mujeres), "M", sep = "_")
  
  # Depura ----
  datos <- cbind(datos, tasa_varones, tasa_mujeres)
  rm(tasa_varones, tasa_mujeres)
  
  # variables para otros cruces ----
  variables <-
    c(
      "pondera",
      "poblacion_ocupada",
      "poblacion_desocupada",
      "poblacion_subocupada",
      
      "poblacion_asalariada",
      "poblacion_no_asalariada",
      
      "ocupado_demandante",
      "ocupado_no_demandante",
      "ocupado_no_demandante_ni_disponible",
      
      "subocupado_demandante",
      "subocupado_no_demandante",
      
      "otro_ocupado_demandante",
      "otro_ocupado_no_demandante"
    )
  
  # Cruce con nivel educativo ----
    # Sin instrucción ----
  tasa_EdNo <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Sin instrucción")
  names(tasa_EdNo) <- paste(names(tasa_EdNo), "EdNo", sep = "_")
  
    # Primaria Incompleta ----
  tasa_EdPI <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Primaria Incompleta")
  names(tasa_EdPI) <- paste(names(tasa_EdPI), "EdPI", sep = "_")
  
    # Primaria Completa ----
  tasa_EdPC <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Primaria Completa")
  names(tasa_EdPC) <- paste(names(tasa_EdPC), "EdPC", sep = "_")
  
    # Secundaria Incompleta ----
  tasa_EdSI <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Secundaria Incompleta")
  names(tasa_EdSI) <-
    paste(names(tasa_EdSI), "EdSI", sep = "_")
  
    # Secundaria Completa ----
  tasa_EdSC <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Secundaria Completa")
  names(tasa_EdSC) <-
    paste(names(tasa_EdSC), "EdSC", sep = "_")
  
    # Terciario Incompleto ----
  tasa_EdTI <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Terciario Incompleto")
  names(tasa_EdTI) <-
    paste(names(tasa_EdTI), "EdTI", sep = "_")
  
    # Terciario Completo ----
  tasa_EdTC <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Terciario Completo")
  names(tasa_EdTC) <-
    paste(names(tasa_EdTC), "EdTC", sep = "_")
  
    # Universitaria Incompleta ----
  tasa_EdUI <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Universitaria Incompleta")
  names(tasa_EdUI) <- paste(names(tasa_EdUI), "EdUI", sep = "_")
  
    # Universitaria Completa ----
  tasa_EdUC <- datos %>%
    transmute_at(.vars = variables,
                 educacion, "Universitaria Completa")
  names(tasa_EdUC) <- paste(names(tasa_EdUC), "EdUC", sep = "_")
  
  # Depura ----
  datos <-
    cbind(
      datos,
      tasa_EdNo,
      tasa_EdPI,
      tasa_EdPC,
      tasa_EdSI,
      tasa_EdSC,
      tasa_EdTI,
      tasa_EdTC,
      tasa_EdUI,
      tasa_EdUC
    )
  rm(
    tasa_EdNo,
    tasa_EdPI,
    tasa_EdPC,
    tasa_EdSI,
    tasa_EdSC,
    tasa_EdTI,
    tasa_EdTC,
    tasa_EdUI,
    tasa_EdUC
  )
  
  # Cruce con Rango etario ----
    # Edad que no corresponde - Rango de 0-9 ----
  tasa_ReNo <- datos %>%
    transmute_at(.vars = variables,
                 etario, "No corresponde")
  names(tasa_ReNo) <- paste(names(tasa_ReNo), "ReNo", sep = "_")
  
    # Menor - Rango de 10-13 ----
  tasa_ReMe <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Menor")
  names(tasa_ReMe) <- paste(names(tasa_ReMe), "ReMe", sep = "_")
  
    # Joven - Rango de 14-18 ----
  tasa_ReJov <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Joven")
  names(tasa_ReJov) <-
    paste(names(tasa_ReJov), "ReJov", sep = "_")
  
    # Junior - Rango de 19-25 ----
  tasa_ReJu <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Junior")
  names(tasa_ReJu) <- paste(names(tasa_ReJu), "ReJu", sep = "_")
  
    # Semi-senior - Rango de 26-35 ----
  tasa_ReSemi <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Semi senior")
  names(tasa_ReSemi) <-
    paste(names(tasa_ReSemi), "ReSemi", sep = "_")
  
    # Senior - Rango de 36-45 ----
  tasa_ReSen <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Senior")
  names(tasa_ReSen) <-
    paste(names(tasa_ReSen), "ReSen", sep = "_")
  
    # Especialista I - Rango de 46-55 ----
  tasa_ReEsp1 <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Especialista I")
  names(tasa_ReEsp1) <-
    paste(names(tasa_ReEsp1), "ReEsp1", sep = "_")
  
    # Especialista II - Rango de 56-65 ----
  tasa_ReEsp2 <- datos %>%
    transmute_at(.vars = variables,
                 etario, "Especialista II")
  names(tasa_ReEsp2) <-
    paste(names(tasa_ReEsp2), "ReEsp2", sep = "_")
  
    # En edad jubilatoria - Rango 66+ ----
  tasa_ReJub <- datos %>%
    transmute_at(.vars = variables,
                 etario, "En edad jubilatoria")
  names(tasa_ReJub) <-
    paste(names(tasa_ReJub), "ReJub", sep = "_")
  
    # Depura ----
  datos <-
    cbind(
      datos,
      tasa_ReNo,
      tasa_ReMe,
      tasa_ReJov,
      tasa_ReJu,
      tasa_ReSemi,
      tasa_ReSen,
      tasa_ReEsp1,
      tasa_ReEsp2,
      tasa_ReJub
    )
  rm(
    tasa_ReNo,
    tasa_ReMe,
    tasa_ReJov,
    tasa_ReJu,
    tasa_ReSemi,
    tasa_ReSen,
    tasa_ReEsp1,
    tasa_ReEsp2,
    tasa_ReJub
  )
  
  
  # Cruce con condición de formalidad ----
    # Formales ----
  tasa_CF <- datos %>%
    transmute_at(.vars = variables,
                 formalidad, "Formal")
  names(tasa_CF) <- paste(names(tasa_CF), "CF", sep = "_")
  
    # No formales ----
  tasa_CI <- datos %>%
    transmute_at(.vars = variables,
                 formalidad, "No formal")
  names(tasa_CI) <- paste(names(tasa_CI), "CI", sep = "_")
  
    # Depura ----
  datos <- cbind(datos, tasa_CF, tasa_CI)
  rm(tasa_CF, tasa_CI)
  
  # Cruce con Tipo de empleo ----
    # Empleo privado ----
  tasa_EmPriv <- datos %>%
    transmute_at(.vars = variables,
                 tipo_empleo, "Privado")
  names(tasa_EmPriv) <-
    paste(names(tasa_EmPriv), "EmPriv", sep = "_")
  
    # Empleo Público ----
  tasa_EmPubl <- datos %>%
    transmute_at(.vars = variables,
                 tipo_empleo, "Público")
  names(tasa_EmPubl) <-
    paste(names(tasa_EmPubl), "EmPubl", sep = "_")
  
    # Otro tipo de empleo ----
  tasa_EmOtro <- datos %>%
    transmute_at(.vars = variables,
                 tipo_empleo, "Otro tipo")
  names(tasa_EmOtro) <-
    paste(names(tasa_EmOtro), "EmOtro", sep = "_")
  
    # Depura ----
  datos <- cbind(datos, tasa_EmPriv, tasa_EmPubl, tasa_EmOtro)
  rm(tasa_EmPriv, tasa_EmPubl, tasa_EmOtro)
  
  ## Resumen ----
  # Cruce con sexo ----
  tasasS <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      #Hombre ----
      poblacion_total_H = sum(pondera_H),
      poblacion_activa_H = sum(poblacion_ocupada_H) + sum(poblacion_desocupada_H),
      poblacion_inactiva_H = poblacion_total_H - poblacion_activa_H,
      poblacion_ocupada_H = sum(poblacion_ocupada_H),
      poblacion_subocupada_H = sum(poblacion_subocupada_H),
      poblacion_desocupada_H = sum(poblacion_desocupada_H),
      poblacion_asalariada_H = sum(poblacion_asalariada_H),
      poblacion_no_asalariada_H = sum(poblacion_no_asalariada_H),
      actividad_H = poblacion_activa_H / poblacion_total_H,
      inactividad_H = poblacion_inactiva_H / poblacion_total_H,
      desocupacion_H = sum(poblacion_desocupada_H) / poblacion_activa_H,
      empleo_H = sum(poblacion_ocupada_H) / poblacion_total_H,
      ocupado_demandante_H = sum(ocupado_demandante_H) / poblacion_activa_H,
      subocupado_demandante_H = sum(subocupado_demandante_H) / poblacion_activa_H,
      otro_ocupado_demandante_H = sum(otro_ocupado_demandante_H) / poblacion_activa_H,
      ocupado_no_demandante_H = sum(ocupado_no_demandante_H) / poblacion_activa_H,
      subocupado_no_demandante_H = sum(subocupado_no_demandante_H) /
        poblacion_activa_H,
      subocupado_H = sum(poblacion_subocupada_H) / poblacion_activa_H,
      otro_ocupado_no_demandante_H = sum(otro_ocupado_no_demandante_H) /
        poblacion_activa_H,
      presion_trabajo_H = (
        sum(poblacion_desocupada_H) + sum(ocupado_demandante_H) +
          sum(ocupado_no_demandante_H)
      ) / poblacion_activa_H,
      nbi1_H = sum(nbi1_H) / poblacion_total_H,
      nbi2_H = sum(nbi2_H) / poblacion_total_H,
      # Mujer ----
      poblacion_total_M = sum(pondera_M),
      poblacion_activa_M = sum(poblacion_ocupada_M) + sum(poblacion_desocupada_M),
      poblacion_inactiva_M = poblacion_total_M - poblacion_activa_M,
      poblacion_ocupada_M = sum(poblacion_ocupada_M),
      poblacion_subocupada_M = sum(poblacion_subocupada_M),
      poblacion_desocupada_M = sum(poblacion_desocupada_M),
      poblacion_asalariada_M = sum(poblacion_asalariada_M),
      poblacion_no_asalariada_M = sum(poblacion_no_asalariada_M),
      actividad_M = poblacion_activa_M / poblacion_total_M,
      inactividad_M = poblacion_inactiva_M / poblacion_total_M,
      desocupacion_M = sum(poblacion_desocupada_M) / poblacion_activa_M,
      empleo_M = sum(poblacion_ocupada_M) / poblacion_total_M,
      ocupado_demandante_M = sum(ocupado_demandante_M) / poblacion_activa_M,
      subocupado_demandante_M = sum(subocupado_demandante_M) / poblacion_activa_M,
      otro_ocupado_demandante_M = sum(otro_ocupado_demandante_M) / poblacion_activa_M,
      ocupado_no_demandante_M = sum(ocupado_no_demandante_M) / poblacion_activa_M,
      subocupado_no_demandante_M = sum(subocupado_no_demandante_M) /
        poblacion_activa_M,
      subocupado_M = sum(poblacion_subocupada_M) / poblacion_activa_M,
      otro_ocupado_no_demandante_M = sum(otro_ocupado_no_demandante_M) /
        poblacion_activa_M,
      presion_trabajo_M = (
        sum(poblacion_desocupada_M) + sum(ocupado_demandante_M) +
          sum(ocupado_no_demandante_M)
      ) / poblacion_activa_M,
      nbi1_M = sum(nbi1_M) / poblacion_total_M,
      nbi2_M = sum(nbi2_M) / poblacion_total_M
    )
  
  # Cruce con sexo PAIS ----
  tasasSP <- datos %>%
    summarise(
      #Hombre ----
      poblacion_total_H = sum(pondera_H),
      poblacion_activa_H = sum(poblacion_ocupada_H) + sum(poblacion_desocupada_H),
      poblacion_inactiva_H = poblacion_total_H - poblacion_activa_H,
      poblacion_ocupada_H = sum(poblacion_ocupada_H),
      poblacion_subocupada_H = sum(poblacion_subocupada_H),
      poblacion_desocupada_H = sum(poblacion_desocupada_H),
      poblacion_asalariada_H = sum(poblacion_asalariada_H),
      poblacion_no_asalariada_H = sum(poblacion_no_asalariada_H),
      actividad_H = poblacion_activa_H / poblacion_total_H,
      inactividad_H = poblacion_inactiva_H / poblacion_total_H,
      desocupacion_H = sum(poblacion_desocupada_H) / poblacion_activa_H,
      empleo_H = sum(poblacion_ocupada_H) / poblacion_total_H,
      ocupado_demandante_H = sum(ocupado_demandante_H) / poblacion_activa_H,
      subocupado_demandante_H = sum(subocupado_demandante_H) / poblacion_activa_H,
      otro_ocupado_demandante_H = sum(otro_ocupado_demandante_H) / poblacion_activa_H,
      ocupado_no_demandante_H = sum(ocupado_no_demandante_H) / poblacion_activa_H,
      subocupado_no_demandante_H = sum(subocupado_no_demandante_H) /
        poblacion_activa_H,
      subocupado_H = sum(poblacion_subocupada_H) / poblacion_activa_H,
      otro_ocupado_no_demandante_H = sum(otro_ocupado_no_demandante_H) /
        poblacion_activa_H,
      presion_trabajo_H = (
        sum(poblacion_desocupada_H) + sum(ocupado_demandante_H) +
          sum(ocupado_no_demandante_H)
      ) / poblacion_activa_H,
      nbi1_H = sum(nbi1_H) / poblacion_total_H,
      nbi2_H = sum(nbi2_H) / poblacion_total_H,
      # Mujer ----
      poblacion_total_M = sum(pondera_M),
      poblacion_activa_M = sum(poblacion_ocupada_M) + sum(poblacion_desocupada_M),
      poblacion_inactiva_M = poblacion_total_M - poblacion_activa_M,
      poblacion_ocupada_M = sum(poblacion_ocupada_M),
      poblacion_subocupada_M = sum(poblacion_subocupada_M),
      poblacion_desocupada_M = sum(poblacion_desocupada_M),
      poblacion_asalariada_M = sum(poblacion_asalariada_M),
      poblacion_no_asalariada_M = sum(poblacion_no_asalariada_M),
      actividad_M = poblacion_activa_M / poblacion_total_M,
      inactividad_M = poblacion_inactiva_M / poblacion_total_M,
      desocupacion_M = sum(poblacion_desocupada_M) / poblacion_activa_M,
      empleo_M = sum(poblacion_ocupada_M) / poblacion_total_M,
      ocupado_demandante_M = sum(ocupado_demandante_M) / poblacion_activa_M,
      subocupado_demandante_M = sum(subocupado_demandante_M) / poblacion_activa_M,
      otro_ocupado_demandante_M = sum(otro_ocupado_demandante_M) / poblacion_activa_M,
      ocupado_no_demandante_M = sum(ocupado_no_demandante_M) / poblacion_activa_M,
      subocupado_no_demandante_M = sum(subocupado_no_demandante_M) /
        poblacion_activa_M,
      subocupado_M = sum(poblacion_subocupada_M) / poblacion_activa_M,
      otro_ocupado_no_demandante_M = sum(otro_ocupado_no_demandante_M) /
        poblacion_activa_M,
      presion_trabajo_M = (
        sum(poblacion_desocupada_M) + sum(ocupado_demandante_M) +
          sum(ocupado_no_demandante_M)
      ) / poblacion_activa_M,
      nbi1_M = sum(nbi1_M) / poblacion_total_M,
      nbi2_M = sum(nbi2_M) / poblacion_total_M
    )
  # Cruce con nivel educativo ----
      # Sin instrucción ----
  tasa_EdNo <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdNo = sum(pondera_EdNo),
      poblacion_activa_EdNo = sum(poblacion_ocupada_EdNo) + sum(poblacion_desocupada_EdNo),
      poblacion_inactiva_EdNo = poblacion_total_EdNo - poblacion_activa_EdNo,
      poblacion_ocupada_EdNo = sum(poblacion_ocupada_EdNo),
      poblacion_subocupada_EdNo = sum(poblacion_subocupada_EdNo),
      poblacion_desocupada_EdNo = sum(poblacion_desocupada_EdNo),
      poblacion_asalariada_EdNo = sum(poblacion_asalariada_EdNo),
      poblacion_no_asalariada_EdNo = sum(poblacion_no_asalariada_EdNo),
      actividad_EdNo = poblacion_activa_EdNo / poblacion_total_EdNo,
      inactividad_EdNo = poblacion_inactiva_EdNo / poblacion_total_EdNo,
      desocupacion_EdNo = sum(poblacion_desocupada_EdNo) / poblacion_activa_EdNo,
      empleo_EdNo = sum(poblacion_ocupada_EdNo) / poblacion_total_EdNo,
      ocupado_demandante_EdNo = sum(ocupado_demandante_EdNo) / poblacion_activa_EdNo,
      subocupado_demandante_EdNo = sum(subocupado_demandante_EdNo) /
        poblacion_activa_EdNo,
      otro_ocupado_demandante_EdNo = sum(otro_ocupado_demandante_EdNo) /
        poblacion_activa_EdNo,
      ocupado_no_demandante_EdNo = sum(ocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo,
      subocupado_no_demandante_EdNo = sum(subocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo,
      subocupado_EdNo = sum(poblacion_subocupada_EdNo) / poblacion_activa_EdNo,
      otro_ocupado_no_demandante_EdNo = sum(otro_ocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo
    )
  
      # Primaria Incompleta ----
  tasa_EdPI <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdPI = sum(pondera_EdPI),
      poblacion_activa_EdPI = sum(poblacion_ocupada_EdPI) + sum(poblacion_desocupada_EdPI),
      poblacion_inactiva_EdPI = poblacion_total_EdPI - poblacion_activa_EdPI,
      poblacion_ocupada_EdPI = sum(poblacion_ocupada_EdPI),
      poblacion_subocupada_EdPI = sum(poblacion_subocupada_EdPI),
      poblacion_desocupada_EdPI = sum(poblacion_desocupada_EdPI),
      poblacion_asalariada_EdPI = sum(poblacion_asalariada_EdPI),
      poblacion_no_asalariada_EdPI = sum(poblacion_no_asalariada_EdPI),
      actividad_EdPI = poblacion_activa_EdPI / poblacion_total_EdPI,
      inactividad_EdPI = poblacion_inactiva_EdPI / poblacion_total_EdPI,
      desocupacion_EdPI = sum(poblacion_desocupada_EdPI) / poblacion_activa_EdPI,
      empleo_EdPI = sum(poblacion_ocupada_EdPI) / poblacion_total_EdPI,
      ocupado_demandante_EdPI = sum(ocupado_demandante_EdPI) / poblacion_activa_EdPI,
      subocupado_demandante_EdPI = sum(subocupado_demandante_EdPI) /
        poblacion_activa_EdPI,
      otro_ocupado_demandante_EdPI = sum(otro_ocupado_demandante_EdPI) /
        poblacion_activa_EdPI,
      ocupado_no_demandante_EdPI = sum(ocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI,
      subocupado_no_demandante_EdPI = sum(subocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI,
      subocupado_EdPI = sum(poblacion_subocupada_EdPI) / poblacion_activa_EdPI,
      otro_ocupado_no_demandante_EdPI = sum(otro_ocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI
    )
  
      # Primaria Completa ----
  tasa_EdPC <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdPC = sum(pondera_EdPC),
      poblacion_activa_EdPC = sum(poblacion_ocupada_EdPC) + sum(poblacion_desocupada_EdPC),
      poblacion_inactiva_EdPC = poblacion_total_EdPC - poblacion_activa_EdPC,
      poblacion_ocupada_EdPC = sum(poblacion_ocupada_EdPC),
      poblacion_subocupada_EdPC = sum(poblacion_subocupada_EdPC),
      poblacion_desocupada_EdPC = sum(poblacion_desocupada_EdPC),
      poblacion_asalariada_EdPC = sum(poblacion_asalariada_EdPC),
      poblacion_no_asalariada_EdPC = sum(poblacion_no_asalariada_EdPC),
      actividad_EdPC = poblacion_activa_EdPC / poblacion_total_EdPC,
      inactividad_EdPC = poblacion_inactiva_EdPC / poblacion_total_EdPC,
      desocupacion_EdPC = sum(poblacion_desocupada_EdPC) / poblacion_activa_EdPC,
      empleo_EdPC = sum(poblacion_ocupada_EdPC) / poblacion_total_EdPC,
      ocupado_demandante_EdPC = sum(ocupado_demandante_EdPC) / poblacion_activa_EdPC,
      subocupado_demandante_EdPC = sum(subocupado_demandante_EdPC) /
        poblacion_activa_EdPC,
      otro_ocupado_demandante_EdPC = sum(otro_ocupado_demandante_EdPC) /
        poblacion_activa_EdPC,
      ocupado_no_demandante_EdPC = sum(ocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC,
      subocupado_no_demandante_EdPC = sum(subocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC,
      subocupado_EdPC = sum(poblacion_subocupada_EdPC) / poblacion_activa_EdPC,
      otro_ocupado_no_demandante_EdPC = sum(otro_ocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC
    )
  
      # Secundaria Incompleta ----
  tasa_EdSI <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdSI = sum(pondera_EdSI),
      poblacion_activa_EdSI = sum(poblacion_ocupada_EdSI) + sum(poblacion_desocupada_EdSI),
      poblacion_inactiva_EdSI = poblacion_total_EdSI - poblacion_activa_EdSI,
      poblacion_ocupada_EdSI = sum(poblacion_ocupada_EdSI),
      poblacion_subocupada_EdSI = sum(poblacion_subocupada_EdSI),
      poblacion_desocupada_EdSI = sum(poblacion_desocupada_EdSI),
      poblacion_asalariada_EdSI = sum(poblacion_asalariada_EdSI),
      poblacion_no_asalariada_EdSI = sum(poblacion_no_asalariada_EdSI),
      actividad_EdSI = poblacion_activa_EdSI / poblacion_total_EdSI,
      inactividad_EdSI = poblacion_inactiva_EdSI / poblacion_total_EdSI,
      desocupacion_EdSI = sum(poblacion_desocupada_EdSI) / poblacion_activa_EdSI,
      empleo_EdSI = sum(poblacion_ocupada_EdSI) / poblacion_total_EdSI,
      ocupado_demandante_EdSI = sum(ocupado_demandante_EdSI) / poblacion_activa_EdSI,
      subocupado_demandante_EdSI = sum(subocupado_demandante_EdSI) /
        poblacion_activa_EdSI,
      otro_ocupado_demandante_EdSI = sum(otro_ocupado_demandante_EdSI) /
        poblacion_activa_EdSI,
      ocupado_no_demandante_EdSI = sum(ocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI,
      subocupado_no_demandante_EdSI = sum(subocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI,
      subocupado_EdSI = sum(poblacion_subocupada_EdSI) / poblacion_activa_EdSI,
      otro_ocupado_no_demandante_EdSI = sum(otro_ocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI
    )
  
      # Secundaria Completa ----
  tasa_EdSC <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdSC = sum(pondera_EdSC),
      poblacion_activa_EdSC = sum(poblacion_ocupada_EdSC) + sum(poblacion_desocupada_EdSC),
      poblacion_inactiva_EdSC = poblacion_total_EdSC - poblacion_activa_EdSC,
      poblacion_ocupada_EdSC = sum(poblacion_ocupada_EdSC),
      poblacion_subocupada_EdSC = sum(poblacion_subocupada_EdSC),
      poblacion_desocupada_EdSC = sum(poblacion_desocupada_EdSC),
      poblacion_asalariada_EdSC = sum(poblacion_asalariada_EdSC),
      poblacion_no_asalariada_EdSC = sum(poblacion_no_asalariada_EdSC),
      actividad_EdSC = poblacion_activa_EdSC / poblacion_total_EdSC,
      inactividad_EdSC = poblacion_inactiva_EdSC / poblacion_total_EdSC,
      desocupacion_EdSC = sum(poblacion_desocupada_EdSC) / poblacion_activa_EdSC,
      empleo_EdSC = sum(poblacion_ocupada_EdSC) / poblacion_total_EdSC,
      ocupado_demandante_EdSC = sum(ocupado_demandante_EdSC) / poblacion_activa_EdSC,
      subocupado_demandante_EdSC = sum(subocupado_demandante_EdSC) /
        poblacion_activa_EdSC,
      otro_ocupado_demandante_EdSC = sum(otro_ocupado_demandante_EdSC) /
        poblacion_activa_EdSC,
      ocupado_no_demandante_EdSC = sum(ocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC,
      subocupado_no_demandante_EdSC = sum(subocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC,
      subocupado_EdSC = sum(poblacion_subocupada_EdSC) / poblacion_activa_EdSC,
      otro_ocupado_no_demandante_EdSC = sum(otro_ocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC
    )
  
      # Terciario Incompleto ----
  tasa_EdTI <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdTI = sum(pondera_EdTI),
      poblacion_activa_EdTI = sum(poblacion_ocupada_EdTI) + sum(poblacion_desocupada_EdTI),
      poblacion_inactiva_EdTI = poblacion_total_EdTI - poblacion_activa_EdTI,
      poblacion_ocupada_EdTI = sum(poblacion_ocupada_EdTI),
      poblacion_subocupada_EdTI = sum(poblacion_subocupada_EdTI),
      poblacion_desocupada_EdTI = sum(poblacion_desocupada_EdTI),
      poblacion_asalariada_EdTI = sum(poblacion_asalariada_EdTI),
      poblacion_no_asalariada_EdTI = sum(poblacion_no_asalariada_EdTI),
      actividad_EdTI = poblacion_activa_EdTI / poblacion_total_EdTI,
      inactividad_EdTI = poblacion_inactiva_EdTI / poblacion_total_EdTI,
      desocupacion_EdTI = sum(poblacion_desocupada_EdTI) / poblacion_activa_EdTI,
      empleo_EdTI = sum(poblacion_ocupada_EdTI) / poblacion_total_EdTI,
      ocupado_demandante_EdTI = sum(ocupado_demandante_EdTI) / poblacion_activa_EdTI,
      subocupado_demandante_EdTI = sum(subocupado_demandante_EdTI) /
        poblacion_activa_EdTI,
      otro_ocupado_demandante_EdTI = sum(otro_ocupado_demandante_EdTI) /
        poblacion_activa_EdTI,
      ocupado_no_demandante_EdTI = sum(ocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI,
      subocupado_no_demandante_EdTI = sum(subocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI,
      subocupado_EdTI = sum(poblacion_subocupada_EdTI) / poblacion_activa_EdTI,
      otro_ocupado_no_demandante_EdTI = sum(otro_ocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI
    )
  
      # Terciario Completo ----
  tasa_EdTC <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdTC = sum(pondera_EdTC),
      poblacion_activa_EdTC = sum(poblacion_ocupada_EdTC) + sum(poblacion_desocupada_EdTC),
      poblacion_inactiva_EdTC = poblacion_total_EdTC - poblacion_activa_EdTC,
      poblacion_ocupada_EdTC = sum(poblacion_ocupada_EdTC),
      poblacion_subocupada_EdTC = sum(poblacion_subocupada_EdTC),
      poblacion_desocupada_EdTC = sum(poblacion_desocupada_EdTC),
      poblacion_asalariada_EdTC = sum(poblacion_asalariada_EdTC),
      poblacion_no_asalariada_EdTC = sum(poblacion_no_asalariada_EdTC),
      actividad_EdTC = poblacion_activa_EdTC / poblacion_total_EdTC,
      inactividad_EdTC = poblacion_inactiva_EdTC / poblacion_total_EdTC,
      desocupacion_EdTC = sum(poblacion_desocupada_EdTC) / poblacion_activa_EdTC,
      empleo_EdTC = sum(poblacion_ocupada_EdTC) / poblacion_total_EdTC,
      ocupado_demandante_EdTC = sum(ocupado_demandante_EdTC) / poblacion_activa_EdTC,
      subocupado_demandante_EdTC = sum(subocupado_demandante_EdTC) /
        poblacion_activa_EdTC,
      otro_ocupado_demandante_EdTC = sum(otro_ocupado_demandante_EdTC) /
        poblacion_activa_EdTC,
      ocupado_no_demandante_EdTC = sum(ocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC,
      subocupado_no_demandante_EdTC = sum(subocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC,
      subocupado_EdTC = sum(poblacion_subocupada_EdTC) / poblacion_activa_EdTC,
      otro_ocupado_no_demandante_EdTC = sum(otro_ocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC
    )
  
      # Universitaria Incompleta ----
  tasa_EdUI <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdUI = sum(pondera_EdUI),
      poblacion_activa_EdUI = sum(poblacion_ocupada_EdUI) + sum(poblacion_desocupada_EdUI),
      poblacion_inactiva_EdUI = poblacion_total_EdUI - poblacion_activa_EdUI,
      poblacion_ocupada_EdUI = sum(poblacion_ocupada_EdUI),
      poblacion_subocupada_EdUI = sum(poblacion_subocupada_EdUI),
      poblacion_desocupada_EdUI = sum(poblacion_desocupada_EdUI),
      poblacion_asalariada_EdUI = sum(poblacion_asalariada_EdUI),
      poblacion_no_asalariada_EdUI = sum(poblacion_no_asalariada_EdUI),
      actividad_EdUI = poblacion_activa_EdUI / poblacion_total_EdUI,
      inactividad_EdUI = poblacion_inactiva_EdUI / poblacion_total_EdUI,
      desocupacion_EdUI = sum(poblacion_desocupada_EdUI) / poblacion_activa_EdUI,
      empleo_EdUI = sum(poblacion_ocupada_EdUI) / poblacion_total_EdUI,
      ocupado_demandante_EdUI = sum(ocupado_demandante_EdUI) / poblacion_activa_EdUI,
      subocupado_demandante_EdUI = sum(subocupado_demandante_EdUI) /
        poblacion_activa_EdUI,
      otro_ocupado_demandante_EdUI = sum(otro_ocupado_demandante_EdUI) /
        poblacion_activa_EdUI,
      ocupado_no_demandante_EdUI = sum(ocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI,
      subocupado_no_demandante_EdUI = sum(subocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI,
      subocupado_EdUI = sum(poblacion_subocupada_EdUI) / poblacion_activa_EdUI,
      otro_ocupado_no_demandante_EdUI = sum(otro_ocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI
    )
  
      # Universitaria Completa ----
  tasa_EdUC <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EdUC = sum(pondera_EdUC),
      poblacion_activa_EdUC = sum(poblacion_ocupada_EdUC) + sum(poblacion_desocupada_EdUC),
      poblacion_inactiva_EdUC = poblacion_total_EdUC - poblacion_activa_EdUC,
      poblacion_ocupada_EdUC = sum(poblacion_ocupada_EdUC),
      poblacion_subocupada_EdUC = sum(poblacion_subocupada_EdUC),
      poblacion_desocupada_EdUC = sum(poblacion_desocupada_EdUC),
      poblacion_asalariada_EdUC = sum(poblacion_asalariada_EdUC),
      poblacion_no_asalariada_EdUC = sum(poblacion_no_asalariada_EdUC),
      actividad_EdUC = poblacion_activa_EdUC / poblacion_total_EdUC,
      inactividad_EdUC = poblacion_inactiva_EdUC / poblacion_total_EdUC,
      desocupacion_EdUC = sum(poblacion_desocupada_EdUC) / poblacion_activa_EdUC,
      empleo_EdUC = sum(poblacion_ocupada_EdUC) / poblacion_total_EdUC,
      ocupado_demandante_EdUC = sum(ocupado_demandante_EdUC) / poblacion_activa_EdUC,
      subocupado_demandante_EdUC = sum(subocupado_demandante_EdUC) /
        poblacion_activa_EdUC,
      otro_ocupado_demandante_EdUC = sum(otro_ocupado_demandante_EdUC) /
        poblacion_activa_EdUC,
      ocupado_no_demandante_EdUC = sum(ocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC,
      subocupado_no_demandante_EdUC = sum(subocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC,
      subocupado_EdUC = sum(poblacion_subocupada_EdUC) / poblacion_activa_EdUC,
      otro_ocupado_no_demandante_EdUC = sum(otro_ocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC
    )
  
  # Cruce con nivel educativo PAIS ----
      # Sin instrucción ----
  tasa_EdNoP <- datos %>%
    summarise(
      poblacion_total_EdNo = sum(pondera_EdNo),
      poblacion_activa_EdNo = sum(poblacion_ocupada_EdNo) + sum(poblacion_desocupada_EdNo),
      poblacion_inactiva_EdNo = poblacion_total_EdNo - poblacion_activa_EdNo,
      poblacion_ocupada_EdNo = sum(poblacion_ocupada_EdNo),
      poblacion_subocupada_EdNo = sum(poblacion_subocupada_EdNo),
      poblacion_desocupada_EdNo = sum(poblacion_desocupada_EdNo),
      poblacion_asalariada_EdNo = sum(poblacion_asalariada_EdNo),
      poblacion_no_asalariada_EdNo = sum(poblacion_no_asalariada_EdNo),
      actividad_EdNo = poblacion_activa_EdNo / poblacion_total_EdNo,
      inactividad_EdNo = poblacion_inactiva_EdNo / poblacion_total_EdNo,
      desocupacion_EdNo = sum(poblacion_desocupada_EdNo) / poblacion_activa_EdNo,
      empleo_EdNo = sum(poblacion_ocupada_EdNo) / poblacion_total_EdNo,
      ocupado_demandante_EdNo = sum(ocupado_demandante_EdNo) / poblacion_activa_EdNo,
      subocupado_demandante_EdNo = sum(subocupado_demandante_EdNo) /
        poblacion_activa_EdNo,
      otro_ocupado_demandante_EdNo = sum(otro_ocupado_demandante_EdNo) /
        poblacion_activa_EdNo,
      ocupado_no_demandante_EdNo = sum(ocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo,
      subocupado_no_demandante_EdNo = sum(subocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo,
      subocupado_EdNo = sum(poblacion_subocupada_EdNo) / poblacion_activa_EdNo,
      otro_ocupado_no_demandante_EdNo = sum(otro_ocupado_no_demandante_EdNo) /
        poblacion_activa_EdNo
    )
  
      # Primaria Incompleta ----
  tasa_EdPIP <- datos %>%
    summarise(
      poblacion_total_EdPI = sum(pondera_EdPI),
      poblacion_activa_EdPI = sum(poblacion_ocupada_EdPI) + sum(poblacion_desocupada_EdPI),
      poblacion_inactiva_EdPI = poblacion_total_EdPI - poblacion_activa_EdPI,
      poblacion_ocupada_EdPI = sum(poblacion_ocupada_EdPI),
      poblacion_subocupada_EdPI = sum(poblacion_subocupada_EdPI),
      poblacion_desocupada_EdPI = sum(poblacion_desocupada_EdPI),
      poblacion_asalariada_EdPI = sum(poblacion_asalariada_EdPI),
      poblacion_no_asalariada_EdPI = sum(poblacion_no_asalariada_EdPI),
      actividad_EdPI = poblacion_activa_EdPI / poblacion_total_EdPI,
      inactividad_EdPI = poblacion_inactiva_EdPI / poblacion_total_EdPI,
      desocupacion_EdPI = sum(poblacion_desocupada_EdPI) / poblacion_activa_EdPI,
      empleo_EdPI = sum(poblacion_ocupada_EdPI) / poblacion_total_EdPI,
      ocupado_demandante_EdPI = sum(ocupado_demandante_EdPI) / poblacion_activa_EdPI,
      subocupado_demandante_EdPI = sum(subocupado_demandante_EdPI) /
        poblacion_activa_EdPI,
      otro_ocupado_demandante_EdPI = sum(otro_ocupado_demandante_EdPI) /
        poblacion_activa_EdPI,
      ocupado_no_demandante_EdPI = sum(ocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI,
      subocupado_no_demandante_EdPI = sum(subocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI,
      subocupado_EdPI = sum(poblacion_subocupada_EdPI) / poblacion_activa_EdPI,
      otro_ocupado_no_demandante_EdPI = sum(otro_ocupado_no_demandante_EdPI) /
        poblacion_activa_EdPI
    )
  
      # Primaria Completa ----
  tasa_EdPCP <- datos %>%
    summarise(
      poblacion_total_EdPC = sum(pondera_EdPC),
      poblacion_activa_EdPC = sum(poblacion_ocupada_EdPC) + sum(poblacion_desocupada_EdPC),
      poblacion_inactiva_EdPC = poblacion_total_EdPC - poblacion_activa_EdPC,
      poblacion_ocupada_EdPC = sum(poblacion_ocupada_EdPC),
      poblacion_subocupada_EdPC = sum(poblacion_subocupada_EdPC),
      poblacion_desocupada_EdPC = sum(poblacion_desocupada_EdPC),
      poblacion_asalariada_EdPC = sum(poblacion_asalariada_EdPC),
      poblacion_no_asalariada_EdPC = sum(poblacion_no_asalariada_EdPC),
      actividad_EdPC = poblacion_activa_EdPC / poblacion_total_EdPC,
      inactividad_EdPC = poblacion_inactiva_EdPC / poblacion_total_EdPC,
      desocupacion_EdPC = sum(poblacion_desocupada_EdPC) / poblacion_activa_EdPC,
      empleo_EdPC = sum(poblacion_ocupada_EdPC) / poblacion_total_EdPC,
      ocupado_demandante_EdPC = sum(ocupado_demandante_EdPC) / poblacion_activa_EdPC,
      subocupado_demandante_EdPC = sum(subocupado_demandante_EdPC) /
        poblacion_activa_EdPC,
      otro_ocupado_demandante_EdPC = sum(otro_ocupado_demandante_EdPC) /
        poblacion_activa_EdPC,
      ocupado_no_demandante_EdPC = sum(ocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC,
      subocupado_no_demandante_EdPC = sum(subocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC,
      subocupado_EdPC = sum(poblacion_subocupada_EdPC) / poblacion_activa_EdPC,
      otro_ocupado_no_demandante_EdPC = sum(otro_ocupado_no_demandante_EdPC) /
        poblacion_activa_EdPC
    )
  
      # Secundaria Incompleta ----
  tasa_EdSIP <- datos %>%
    summarise(
      poblacion_total_EdSI = sum(pondera_EdSI),
      poblacion_activa_EdSI = sum(poblacion_ocupada_EdSI) + sum(poblacion_desocupada_EdSI),
      poblacion_inactiva_EdSI = poblacion_total_EdSI - poblacion_activa_EdSI,
      poblacion_ocupada_EdSI = sum(poblacion_ocupada_EdSI),
      poblacion_subocupada_EdSI = sum(poblacion_subocupada_EdSI),
      poblacion_desocupada_EdSI = sum(poblacion_desocupada_EdSI),
      poblacion_asalariada_EdSI = sum(poblacion_asalariada_EdSI),
      poblacion_no_asalariada_EdSI = sum(poblacion_no_asalariada_EdSI),
      actividad_EdSI = poblacion_activa_EdSI / poblacion_total_EdSI,
      inactividad_EdSI = poblacion_inactiva_EdSI / poblacion_total_EdSI,
      desocupacion_EdSI = sum(poblacion_desocupada_EdSI) / poblacion_activa_EdSI,
      empleo_EdSI = sum(poblacion_ocupada_EdSI) / poblacion_total_EdSI,
      ocupado_demandante_EdSI = sum(ocupado_demandante_EdSI) / poblacion_activa_EdSI,
      subocupado_demandante_EdSI = sum(subocupado_demandante_EdSI) /
        poblacion_activa_EdSI,
      otro_ocupado_demandante_EdSI = sum(otro_ocupado_demandante_EdSI) /
        poblacion_activa_EdSI,
      ocupado_no_demandante_EdSI = sum(ocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI,
      subocupado_no_demandante_EdSI = sum(subocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI,
      subocupado_EdSI = sum(poblacion_subocupada_EdSI) / poblacion_activa_EdSI,
      otro_ocupado_no_demandante_EdSI = sum(otro_ocupado_no_demandante_EdSI) /
        poblacion_activa_EdSI
    )
  
      # Secundaria Completa ----
  tasa_EdSCP <- datos %>%
    summarise(
      poblacion_total_EdSC = sum(pondera_EdSC),
      poblacion_activa_EdSC = sum(poblacion_ocupada_EdSC) + sum(poblacion_desocupada_EdSC),
      poblacion_inactiva_EdSC = poblacion_total_EdSC - poblacion_activa_EdSC,
      poblacion_ocupada_EdSC = sum(poblacion_ocupada_EdSC),
      poblacion_subocupada_EdSC = sum(poblacion_subocupada_EdSC),
      poblacion_desocupada_EdSC = sum(poblacion_desocupada_EdSC),
      poblacion_asalariada_EdSC = sum(poblacion_asalariada_EdSC),
      poblacion_no_asalariada_EdSC = sum(poblacion_no_asalariada_EdSC),
      actividad_EdSC = poblacion_activa_EdSC / poblacion_total_EdSC,
      inactividad_EdSC = poblacion_inactiva_EdSC / poblacion_total_EdSC,
      desocupacion_EdSC = sum(poblacion_desocupada_EdSC) / poblacion_activa_EdSC,
      empleo_EdSC = sum(poblacion_ocupada_EdSC) / poblacion_total_EdSC,
      ocupado_demandante_EdSC = sum(ocupado_demandante_EdSC) / poblacion_activa_EdSC,
      subocupado_demandante_EdSC = sum(subocupado_demandante_EdSC) /
        poblacion_activa_EdSC,
      otro_ocupado_demandante_EdSC = sum(otro_ocupado_demandante_EdSC) /
        poblacion_activa_EdSC,
      ocupado_no_demandante_EdSC = sum(ocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC,
      subocupado_no_demandante_EdSC = sum(subocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC,
      subocupado_EdSC = sum(poblacion_subocupada_EdSC) / poblacion_activa_EdSC,
      otro_ocupado_no_demandante_EdSC = sum(otro_ocupado_no_demandante_EdSC) /
        poblacion_activa_EdSC
    )
  
      # Terciario Incompleto ----
  tasa_EdTIP <- datos %>%
    summarise(
      poblacion_total_EdTI = sum(pondera_EdTI),
      poblacion_activa_EdTI = sum(poblacion_ocupada_EdTI) + sum(poblacion_desocupada_EdTI),
      poblacion_inactiva_EdTI = poblacion_total_EdTI - poblacion_activa_EdTI,
      poblacion_ocupada_EdTI = sum(poblacion_ocupada_EdTI),
      poblacion_subocupada_EdTI = sum(poblacion_subocupada_EdTI),
      poblacion_desocupada_EdTI = sum(poblacion_desocupada_EdTI),
      poblacion_asalariada_EdTI = sum(poblacion_asalariada_EdTI),
      poblacion_no_asalariada_EdTI = sum(poblacion_no_asalariada_EdTI),
      actividad_EdTI = poblacion_activa_EdTI / poblacion_total_EdTI,
      inactividad_EdTI = poblacion_inactiva_EdTI / poblacion_total_EdTI,
      desocupacion_EdTI = sum(poblacion_desocupada_EdTI) / poblacion_activa_EdTI,
      empleo_EdTI = sum(poblacion_ocupada_EdTI) / poblacion_total_EdTI,
      ocupado_demandante_EdTI = sum(ocupado_demandante_EdTI) / poblacion_activa_EdTI,
      subocupado_demandante_EdTI = sum(subocupado_demandante_EdTI) /
        poblacion_activa_EdTI,
      otro_ocupado_demandante_EdTI = sum(otro_ocupado_demandante_EdTI) /
        poblacion_activa_EdTI,
      ocupado_no_demandante_EdTI = sum(ocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI,
      subocupado_no_demandante_EdTI = sum(subocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI,
      subocupado_EdTI = sum(poblacion_subocupada_EdTI) / poblacion_activa_EdTI,
      otro_ocupado_no_demandante_EdTI = sum(otro_ocupado_no_demandante_EdTI) /
        poblacion_activa_EdTI
    )
  
      # Terciario Completo ----
  tasa_EdTCP <- datos %>%
    summarise(
      poblacion_total_EdTC = sum(pondera_EdTC),
      poblacion_activa_EdTC = sum(poblacion_ocupada_EdTC) + sum(poblacion_desocupada_EdTC),
      poblacion_inactiva_EdTC = poblacion_total_EdTC - poblacion_activa_EdTC,
      poblacion_ocupada_EdTC = sum(poblacion_ocupada_EdTC),
      poblacion_subocupada_EdTC = sum(poblacion_subocupada_EdTC),
      poblacion_desocupada_EdTC = sum(poblacion_desocupada_EdTC),
      poblacion_asalariada_EdTC = sum(poblacion_asalariada_EdTC),
      poblacion_no_asalariada_EdTC = sum(poblacion_no_asalariada_EdTC),
      actividad_EdTC = poblacion_activa_EdTC / poblacion_total_EdTC,
      inactividad_EdTC = poblacion_inactiva_EdTC / poblacion_total_EdTC,
      desocupacion_EdTC = sum(poblacion_desocupada_EdTC) / poblacion_activa_EdTC,
      empleo_EdTC = sum(poblacion_ocupada_EdTC) / poblacion_total_EdTC,
      ocupado_demandante_EdTC = sum(ocupado_demandante_EdTC) / poblacion_activa_EdTC,
      subocupado_demandante_EdTC = sum(subocupado_demandante_EdTC) /
        poblacion_activa_EdTC,
      otro_ocupado_demandante_EdTC = sum(otro_ocupado_demandante_EdTC) /
        poblacion_activa_EdTC,
      ocupado_no_demandante_EdTC = sum(ocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC,
      subocupado_no_demandante_EdTC = sum(subocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC,
      subocupado_EdTC = sum(poblacion_subocupada_EdTC) / poblacion_activa_EdTC,
      otro_ocupado_no_demandante_EdTC = sum(otro_ocupado_no_demandante_EdTC) /
        poblacion_activa_EdTC
    )
  
      # Universitaria Incompleta ----
  tasa_EdUIP <- datos %>%
    summarise(
      poblacion_total_EdUI = sum(pondera_EdUI),
      poblacion_activa_EdUI = sum(poblacion_ocupada_EdUI) + sum(poblacion_desocupada_EdUI),
      poblacion_inactiva_EdUI = poblacion_total_EdUI - poblacion_activa_EdUI,
      poblacion_ocupada_EdUI = sum(poblacion_ocupada_EdUI),
      poblacion_subocupada_EdUI = sum(poblacion_subocupada_EdUI),
      poblacion_desocupada_EdUI = sum(poblacion_desocupada_EdUI),
      poblacion_asalariada_EdUI = sum(poblacion_asalariada_EdUI),
      poblacion_no_asalariada_EdUI = sum(poblacion_no_asalariada_EdUI),
      actividad_EdUI = poblacion_activa_EdUI / poblacion_total_EdUI,
      inactividad_EdUI = poblacion_inactiva_EdUI / poblacion_total_EdUI,
      desocupacion_EdUI = sum(poblacion_desocupada_EdUI) / poblacion_activa_EdUI,
      empleo_EdUI = sum(poblacion_ocupada_EdUI) / poblacion_total_EdUI,
      ocupado_demandante_EdUI = sum(ocupado_demandante_EdUI) / poblacion_activa_EdUI,
      subocupado_demandante_EdUI = sum(subocupado_demandante_EdUI) /
        poblacion_activa_EdUI,
      otro_ocupado_demandante_EdUI = sum(otro_ocupado_demandante_EdUI) /
        poblacion_activa_EdUI,
      ocupado_no_demandante_EdUI = sum(ocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI,
      subocupado_no_demandante_EdUI = sum(subocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI,
      subocupado_EdUI = sum(poblacion_subocupada_EdUI) / poblacion_activa_EdUI,
      otro_ocupado_no_demandante_EdUI = sum(otro_ocupado_no_demandante_EdUI) /
        poblacion_activa_EdUI
    )
  
      # Universitaria Completa ----
  tasa_EdUCP <- datos %>%
    summarise(
      poblacion_total_EdUC = sum(pondera_EdUC),
      poblacion_activa_EdUC = sum(poblacion_ocupada_EdUC) + sum(poblacion_desocupada_EdUC),
      poblacion_inactiva_EdUC = poblacion_total_EdUC - poblacion_activa_EdUC,
      poblacion_ocupada_EdUC = sum(poblacion_ocupada_EdUC),
      poblacion_subocupada_EdUC = sum(poblacion_subocupada_EdUC),
      poblacion_desocupada_EdUC = sum(poblacion_desocupada_EdUC),
      poblacion_asalariada_EdUC = sum(poblacion_asalariada_EdUC),
      poblacion_no_asalariada_EdUC = sum(poblacion_no_asalariada_EdUC),
      actividad_EdUC = poblacion_activa_EdUC / poblacion_total_EdUC,
      inactividad_EdUC = poblacion_inactiva_EdUC / poblacion_total_EdUC,
      desocupacion_EdUC = sum(poblacion_desocupada_EdUC) / poblacion_activa_EdUC,
      empleo_EdUC = sum(poblacion_ocupada_EdUC) / poblacion_total_EdUC,
      ocupado_demandante_EdUC = sum(ocupado_demandante_EdUC) / poblacion_activa_EdUC,
      subocupado_demandante_EdUC = sum(subocupado_demandante_EdUC) /
        poblacion_activa_EdUC,
      otro_ocupado_demandante_EdUC = sum(otro_ocupado_demandante_EdUC) /
        poblacion_activa_EdUC,
      ocupado_no_demandante_EdUC = sum(ocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC,
      subocupado_no_demandante_EdUC = sum(subocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC,
      subocupado_EdUC = sum(poblacion_subocupada_EdUC) / poblacion_activa_EdUC,
      otro_ocupado_no_demandante_EdUC = sum(otro_ocupado_no_demandante_EdUC) /
        poblacion_activa_EdUC
    )
  
  # Cruce con Rango etario ----
      # Edad que no corresponde - Rango de 0-9 ----
  tasa_ReNo <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReNo = sum(pondera_ReNo),
      poblacion_activa_ReNo = sum(poblacion_ocupada_ReNo) + sum(poblacion_desocupada_ReNo),
      poblacion_inactiva_ReNo = poblacion_total_ReNo - poblacion_activa_ReNo,
      poblacion_ocupada_ReNo = sum(poblacion_ocupada_ReNo),
      poblacion_subocupada_ReNo = sum(poblacion_subocupada_ReNo),
      poblacion_desocupada_ReNo = sum(poblacion_desocupada_ReNo),
      poblacion_asalariada_ReNo = sum(poblacion_asalariada_ReNo),
      poblacion_no_asalariada_ReNo = sum(poblacion_no_asalariada_ReNo),
      actividad_ReNo = poblacion_activa_ReNo / poblacion_total_ReNo,
      inactividad_ReNo = poblacion_inactiva_ReNo / poblacion_total_ReNo,
      desocupacion_ReNo = sum(poblacion_desocupada_ReNo) / poblacion_activa_ReNo,
      empleo_ReNo = sum(poblacion_ocupada_ReNo) / poblacion_total_ReNo,
      ocupado_demandante_ReNo = sum(ocupado_demandante_ReNo) / poblacion_activa_ReNo,
      subocupado_demandante_ReNo = sum(subocupado_demandante_ReNo) /
        poblacion_activa_ReNo,
      otro_ocupado_demandante_ReNo = sum(otro_ocupado_demandante_ReNo) /
        poblacion_activa_ReNo,
      ocupado_no_demandante_ReNo = sum(ocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo,
      subocupado_no_demandante_ReNo = sum(subocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo,
      subocupado_ReNo = sum(poblacion_subocupada_ReNo) / poblacion_activa_ReNo,
      otro_ocupado_no_demandante_ReNo = sum(otro_ocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo
    )
  
      # Menor - Rango de 10-13 ----
  tasa_ReMe <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReMe = sum(pondera_ReMe),
      poblacion_activa_ReMe = sum(poblacion_ocupada_ReMe) + sum(poblacion_desocupada_ReMe),
      poblacion_inactiva_ReMe = poblacion_total_ReMe - poblacion_activa_ReMe,
      poblacion_ocupada_ReMe = sum(poblacion_ocupada_ReMe),
      poblacion_subocupada_ReMe = sum(poblacion_subocupada_ReMe),
      poblacion_desocupada_ReMe = sum(poblacion_desocupada_ReMe),
      poblacion_asalariada_ReMe = sum(poblacion_asalariada_ReMe),
      poblacion_no_asalariada_ReMe = sum(poblacion_no_asalariada_ReMe),
      actividad_ReMe = poblacion_activa_ReMe / poblacion_total_ReMe,
      inactividad_ReMe = poblacion_inactiva_ReMe / poblacion_total_ReMe,
      desocupacion_ReMe = sum(poblacion_desocupada_ReMe) / poblacion_activa_ReMe,
      empleo_ReMe = sum(poblacion_ocupada_ReMe) / poblacion_total_ReMe,
      ocupado_demandante_ReMe = sum(ocupado_demandante_ReMe) / poblacion_activa_ReMe,
      subocupado_demandante_ReMe = sum(subocupado_demandante_ReMe) /
        poblacion_activa_ReMe,
      otro_ocupado_demandante_ReMe = sum(otro_ocupado_demandante_ReMe) /
        poblacion_activa_ReMe,
      ocupado_no_demandante_ReMe = sum(ocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe,
      subocupado_no_demandante_ReMe = sum(subocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe,
      subocupado_ReMe = sum(poblacion_subocupada_ReMe) / poblacion_activa_ReMe,
      otro_ocupado_no_demandante_ReMe = sum(otro_ocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe
    )
  
      # Joven - Rango de 14-18 ----
  tasa_ReJov <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReJov = sum(pondera_ReJov),
      poblacion_activa_ReJov = sum(poblacion_ocupada_ReJov) + sum(poblacion_desocupada_ReJov),
      poblacion_inactiva_ReJov = poblacion_total_ReJov - poblacion_activa_ReJov,
      poblacion_ocupada_ReJov = sum(poblacion_ocupada_ReJov),
      poblacion_subocupada_ReJov = sum(poblacion_subocupada_ReJov),
      poblacion_desocupada_ReJov = sum(poblacion_desocupada_ReJov),
      poblacion_asalariada_ReJov = sum(poblacion_asalariada_ReJov),
      poblacion_no_asalariada_ReJov = sum(poblacion_no_asalariada_ReJov),
      actividad_ReJov = poblacion_activa_ReJov / poblacion_total_ReJov,
      inactividad_ReJov = poblacion_inactiva_ReJov / poblacion_total_ReJov,
      desocupacion_ReJov = sum(poblacion_desocupada_ReJov) / poblacion_activa_ReJov,
      empleo_ReJov = sum(poblacion_ocupada_ReJov) / poblacion_total_ReJov,
      ocupado_demandante_ReJov = sum(ocupado_demandante_ReJov) / poblacion_activa_ReJov,
      subocupado_demandante_ReJov = sum(subocupado_demandante_ReJov) /
        poblacion_activa_ReJov,
      otro_ocupado_demandante_ReJov = sum(otro_ocupado_demandante_ReJov) /
        poblacion_activa_ReJov,
      ocupado_no_demandante_ReJov = sum(ocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov,
      subocupado_no_demandante_ReJov = sum(subocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov,
      subocupado_ReJov = sum(poblacion_subocupada_ReJov) / poblacion_activa_ReJov,
      otro_ocupado_no_demandante_ReJov = sum(otro_ocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov
    )
  
      # Junior - Rango de 19-25 ----
  tasa_ReJu <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReJu = sum(pondera_ReJu),
      poblacion_activa_ReJu = sum(poblacion_ocupada_ReJu) + sum(poblacion_desocupada_ReJu),
      poblacion_inactiva_ReJu = poblacion_total_ReJu - poblacion_activa_ReJu,
      poblacion_ocupada_ReJu = sum(poblacion_ocupada_ReJu),
      poblacion_subocupada_ReJu = sum(poblacion_subocupada_ReJu),
      poblacion_desocupada_ReJu = sum(poblacion_desocupada_ReJu),
      poblacion_asalariada_ReJu = sum(poblacion_asalariada_ReJu),
      poblacion_no_asalariada_ReJu = sum(poblacion_no_asalariada_ReJu),
      actividad_ReJu = poblacion_activa_ReJu / poblacion_total_ReJu,
      inactividad_ReJu = poblacion_inactiva_ReJu / poblacion_total_ReJu,
      desocupacion_ReJu = sum(poblacion_desocupada_ReJu) / poblacion_activa_ReJu,
      empleo_ReJu = sum(poblacion_ocupada_ReJu) / poblacion_total_ReJu,
      ocupado_demandante_ReJu = sum(ocupado_demandante_ReJu) / poblacion_activa_ReJu,
      subocupado_demandante_ReJu = sum(subocupado_demandante_ReJu) /
        poblacion_activa_ReJu,
      otro_ocupado_demandante_ReJu = sum(otro_ocupado_demandante_ReJu) /
        poblacion_activa_ReJu,
      ocupado_no_demandante_ReJu = sum(ocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu,
      subocupado_no_demandante_ReJu = sum(subocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu,
      subocupado_ReJu = sum(poblacion_subocupada_ReJu) / poblacion_activa_ReJu,
      otro_ocupado_no_demandante_ReJu = sum(otro_ocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu
    )
  
      # Semi-senior - Rango de 26-35 ----
  tasa_ReSemi <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReSemi = sum(pondera_ReSemi),
      poblacion_activa_ReSemi = sum(poblacion_ocupada_ReSemi) + sum(poblacion_desocupada_ReSemi),
      poblacion_inactiva_ReSemi = poblacion_total_ReSemi - poblacion_activa_ReSemi,
      poblacion_ocupada_ReSemi = sum(poblacion_ocupada_ReSemi),
      poblacion_subocupada_ReSemi = sum(poblacion_subocupada_ReSemi),
      poblacion_desocupada_ReSemi = sum(poblacion_desocupada_ReSemi),
      poblacion_asalariada_ReSemi = sum(poblacion_asalariada_ReSemi),
      poblacion_no_asalariada_ReSemi = sum(poblacion_no_asalariada_ReSemi),
      actividad_ReSemi = poblacion_activa_ReSemi / poblacion_total_ReSemi,
      inactividad_ReSemi = poblacion_inactiva_ReSemi / poblacion_total_ReSemi,
      desocupacion_ReSemi = sum(poblacion_desocupada_ReSemi) / poblacion_activa_ReSemi,
      empleo_ReSemi = sum(poblacion_ocupada_ReSemi) / poblacion_total_ReSemi,
      ocupado_demandante_ReSemi = sum(ocupado_demandante_ReSemi) / poblacion_activa_ReSemi,
      subocupado_demandante_ReSemi = sum(subocupado_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      otro_ocupado_demandante_ReSemi = sum(otro_ocupado_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      ocupado_no_demandante_ReSemi = sum(ocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      subocupado_no_demandante_ReSemi = sum(subocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      subocupado_ReSemi = sum(poblacion_subocupada_ReSemi) / poblacion_activa_ReSemi,
      otro_ocupado_no_demandante_ReSemi = sum(otro_ocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi
    )
  
      # Senior - Rango de 36-45 ----
  tasa_ReSen <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReSen = sum(pondera_ReSen),
      poblacion_activa_ReSen = sum(poblacion_ocupada_ReSen) + sum(poblacion_desocupada_ReSen),
      poblacion_inactiva_ReSen = poblacion_total_ReSen - poblacion_activa_ReSen,
      poblacion_ocupada_ReSen = sum(poblacion_ocupada_ReSen),
      poblacion_subocupada_ReSen = sum(poblacion_subocupada_ReSen),
      poblacion_desocupada_ReSen = sum(poblacion_desocupada_ReSen),
      poblacion_asalariada_ReSen = sum(poblacion_asalariada_ReSen),
      poblacion_no_asalariada_ReSen = sum(poblacion_no_asalariada_ReSen),
      actividad_ReSen = poblacion_activa_ReSen / poblacion_total_ReSen,
      inactividad_ReSen = poblacion_inactiva_ReSen / poblacion_total_ReSen,
      desocupacion_ReSen = sum(poblacion_desocupada_ReSen) / poblacion_activa_ReSen,
      empleo_ReSen = sum(poblacion_ocupada_ReSen) / poblacion_total_ReSen,
      ocupado_demandante_ReSen = sum(ocupado_demandante_ReSen) / poblacion_activa_ReSen,
      subocupado_demandante_ReSen = sum(subocupado_demandante_ReSen) /
        poblacion_activa_ReSen,
      otro_ocupado_demandante_ReSen = sum(otro_ocupado_demandante_ReSen) /
        poblacion_activa_ReSen,
      ocupado_no_demandante_ReSen = sum(ocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen,
      subocupado_no_demandante_ReSen = sum(subocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen,
      subocupado_ReSen = sum(poblacion_subocupada_ReSen) / poblacion_activa_ReSen,
      otro_ocupado_no_demandante_ReSen = sum(otro_ocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen
    )
  
      # Especialista I - Rango de 46-55 ----
  tasa_ReEsp1 <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReEsp1 = sum(pondera_ReEsp1),
      poblacion_activa_ReEsp1 = sum(poblacion_ocupada_ReEsp1) + sum(poblacion_desocupada_ReEsp1),
      poblacion_inactiva_ReEsp1 = poblacion_total_ReEsp1 - poblacion_activa_ReEsp1,
      poblacion_ocupada_ReEsp1 = sum(poblacion_ocupada_ReEsp1),
      poblacion_subocupada_ReEsp1 = sum(poblacion_subocupada_ReEsp1),
      poblacion_desocupada_ReEsp1 = sum(poblacion_desocupada_ReEsp1),
      poblacion_asalariada_ReEsp1 = sum(poblacion_asalariada_ReEsp1),
      poblacion_no_asalariada_ReEsp1 = sum(poblacion_no_asalariada_ReEsp1),
      actividad_ReEsp1 = poblacion_activa_ReEsp1 / poblacion_total_ReEsp1,
      inactividad_ReEsp1 = poblacion_inactiva_ReEsp1 / poblacion_total_ReEsp1,
      desocupacion_ReEsp1 = sum(poblacion_desocupada_ReEsp1) / poblacion_activa_ReEsp1,
      empleo_ReEsp1 = sum(poblacion_ocupada_ReEsp1) / poblacion_total_ReEsp1,
      ocupado_demandante_ReEsp1 = sum(ocupado_demandante_ReEsp1) / poblacion_activa_ReEsp1,
      subocupado_demandante_ReEsp1 = sum(subocupado_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      otro_ocupado_demandante_ReEsp1 = sum(otro_ocupado_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      ocupado_no_demandante_ReEsp1 = sum(ocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      subocupado_no_demandante_ReEsp1 = sum(subocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      subocupado_ReEsp1 = sum(poblacion_subocupada_ReEsp1) / poblacion_activa_ReEsp1,
      otro_ocupado_no_demandante_ReEsp1 = sum(otro_ocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1
    )
  
      # Especialista II - Rango de 56-65 ----
  tasa_ReEsp2 <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReEsp2 = sum(pondera_ReEsp2),
      poblacion_activa_ReEsp2 = sum(poblacion_ocupada_ReEsp2) + sum(poblacion_desocupada_ReEsp2),
      poblacion_inactiva_ReEsp2 = poblacion_total_ReEsp2 - poblacion_activa_ReEsp2,
      poblacion_ocupada_ReEsp2 = sum(poblacion_ocupada_ReEsp2),
      poblacion_subocupada_ReEsp2 = sum(poblacion_subocupada_ReEsp2),
      poblacion_desocupada_ReEsp2 = sum(poblacion_desocupada_ReEsp2),
      poblacion_asalariada_ReEsp2 = sum(poblacion_asalariada_ReEsp2),
      poblacion_no_asalariada_ReEsp2 = sum(poblacion_no_asalariada_ReEsp2),
      actividad_ReEsp2 = poblacion_activa_ReEsp2 / poblacion_total_ReEsp2,
      inactividad_ReEsp2 = poblacion_inactiva_ReEsp2 / poblacion_total_ReEsp2,
      desocupacion_ReEsp2 = sum(poblacion_desocupada_ReEsp2) / poblacion_activa_ReEsp2,
      empleo_ReEsp2 = sum(poblacion_ocupada_ReEsp2) / poblacion_total_ReEsp2,
      ocupado_demandante_ReEsp2 = sum(ocupado_demandante_ReEsp2) / poblacion_activa_ReEsp2,
      subocupado_demandante_ReEsp2 = sum(subocupado_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      otro_ocupado_demandante_ReEsp2 = sum(otro_ocupado_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      ocupado_no_demandante_ReEsp2 = sum(ocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      subocupado_no_demandante_ReEsp2 = sum(subocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      subocupado_ReEsp2 = sum(poblacion_subocupada_ReEsp2) / poblacion_activa_ReEsp2,
      otro_ocupado_no_demandante_ReEsp2 = sum(otro_ocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2
    )
  
      # En edad jubilatoria - Rango 66+ ----
  tasa_ReJub <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_ReJub = sum(pondera_ReJub),
      poblacion_activa_ReJub = sum(poblacion_ocupada_ReJub) + sum(poblacion_desocupada_ReJub),
      poblacion_inactiva_ReJub = poblacion_total_ReJub - poblacion_activa_ReJub,
      poblacion_ocupada_ReJub = sum(poblacion_ocupada_ReJub),
      poblacion_subocupada_ReJub = sum(poblacion_subocupada_ReJub),
      poblacion_desocupada_ReJub = sum(poblacion_desocupada_ReJub),
      poblacion_asalariada_ReJub = sum(poblacion_asalariada_ReJub),
      poblacion_no_asalariada_ReJub = sum(poblacion_no_asalariada_ReJub),
      actividad_ReJub = poblacion_activa_ReJub / poblacion_total_ReJub,
      inactividad_ReJub = poblacion_inactiva_ReJub / poblacion_total_ReJub,
      desocupacion_ReJub = sum(poblacion_desocupada_ReJub) / poblacion_activa_ReJub,
      empleo_ReJub = sum(poblacion_ocupada_ReJub) / poblacion_total_ReJub,
      ocupado_demandante_ReJub = sum(ocupado_demandante_ReJub) / poblacion_activa_ReJub,
      subocupado_demandante_ReJub = sum(subocupado_demandante_ReJub) /
        poblacion_activa_ReJub,
      otro_ocupado_demandante_ReJub = sum(otro_ocupado_demandante_ReJub) /
        poblacion_activa_ReJub,
      ocupado_no_demandante_ReJub = sum(ocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub,
      subocupado_no_demandante_ReJub = sum(subocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub,
      subocupado_ReJub = sum(poblacion_subocupada_ReJub) / poblacion_activa_ReJub,
      otro_ocupado_no_demandante_ReJub = sum(otro_ocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub
    )
  # Cruce con Rango etario PAIS ----
      # Edad que no corresponde - Rango de 0-9 ----
  tasa_ReNoP <- datos %>%
    summarise(
      poblacion_total_ReNo = sum(pondera_ReNo),
      poblacion_activa_ReNo = sum(poblacion_ocupada_ReNo) + sum(poblacion_desocupada_ReNo),
      poblacion_inactiva_ReNo = poblacion_total_ReNo - poblacion_activa_ReNo,
      poblacion_ocupada_ReNo = sum(poblacion_ocupada_ReNo),
      poblacion_subocupada_ReNo = sum(poblacion_subocupada_ReNo),
      poblacion_desocupada_ReNo = sum(poblacion_desocupada_ReNo),
      poblacion_asalariada_ReNo = sum(poblacion_asalariada_ReNo),
      poblacion_no_asalariada_ReNo = sum(poblacion_no_asalariada_ReNo),
      actividad_ReNo = poblacion_activa_ReNo / poblacion_total_ReNo,
      inactividad_ReNo = poblacion_inactiva_ReNo / poblacion_total_ReNo,
      desocupacion_ReNo = sum(poblacion_desocupada_ReNo) / poblacion_activa_ReNo,
      empleo_ReNo = sum(poblacion_ocupada_ReNo) / poblacion_total_ReNo,
      ocupado_demandante_ReNo = sum(ocupado_demandante_ReNo) / poblacion_activa_ReNo,
      subocupado_demandante_ReNo = sum(subocupado_demandante_ReNo) /
        poblacion_activa_ReNo,
      otro_ocupado_demandante_ReNo = sum(otro_ocupado_demandante_ReNo) /
        poblacion_activa_ReNo,
      ocupado_no_demandante_ReNo = sum(ocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo,
      subocupado_no_demandante_ReNo = sum(subocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo,
      subocupado_ReNo = sum(poblacion_subocupada_ReNo) / poblacion_activa_ReNo,
      otro_ocupado_no_demandante_ReNo = sum(otro_ocupado_no_demandante_ReNo) /
        poblacion_activa_ReNo
    )
  
      # Menor - Rango de 10-13 ----
  tasa_ReMeP <- datos %>%
    summarise(
      poblacion_total_ReMe = sum(pondera_ReMe),
      poblacion_activa_ReMe = sum(poblacion_ocupada_ReMe) + sum(poblacion_desocupada_ReMe),
      poblacion_inactiva_ReMe = poblacion_total_ReMe - poblacion_activa_ReMe,
      poblacion_ocupada_ReMe = sum(poblacion_ocupada_ReMe),
      poblacion_subocupada_ReMe = sum(poblacion_subocupada_ReMe),
      poblacion_desocupada_ReMe = sum(poblacion_desocupada_ReMe),
      poblacion_asalariada_ReMe = sum(poblacion_asalariada_ReMe),
      poblacion_no_asalariada_ReMe = sum(poblacion_no_asalariada_ReMe),
      actividad_ReMe = poblacion_activa_ReMe / poblacion_total_ReMe,
      inactividad_ReMe = poblacion_inactiva_ReMe / poblacion_total_ReMe,
      desocupacion_ReMe = sum(poblacion_desocupada_ReMe) / poblacion_activa_ReMe,
      empleo_ReMe = sum(poblacion_ocupada_ReMe) / poblacion_total_ReMe,
      ocupado_demandante_ReMe = sum(ocupado_demandante_ReMe) / poblacion_activa_ReMe,
      subocupado_demandante_ReMe = sum(subocupado_demandante_ReMe) /
        poblacion_activa_ReMe,
      otro_ocupado_demandante_ReMe = sum(otro_ocupado_demandante_ReMe) /
        poblacion_activa_ReMe,
      ocupado_no_demandante_ReMe = sum(ocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe,
      subocupado_no_demandante_ReMe = sum(subocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe,
      subocupado_ReMe = sum(poblacion_subocupada_ReMe) / poblacion_activa_ReMe,
      otro_ocupado_no_demandante_ReMe = sum(otro_ocupado_no_demandante_ReMe) /
        poblacion_activa_ReMe
    )
  
      # Joven - Rango de 14-18 ----
  tasa_ReJovP <- datos %>%
    summarise(
      poblacion_total_ReJov = sum(pondera_ReJov),
      poblacion_activa_ReJov = sum(poblacion_ocupada_ReJov) + sum(poblacion_desocupada_ReJov),
      poblacion_inactiva_ReJov = poblacion_total_ReJov - poblacion_activa_ReJov,
      poblacion_ocupada_ReJov = sum(poblacion_ocupada_ReJov),
      poblacion_subocupada_ReJov = sum(poblacion_subocupada_ReJov),
      poblacion_desocupada_ReJov = sum(poblacion_desocupada_ReJov),
      poblacion_asalariada_ReJov = sum(poblacion_asalariada_ReJov),
      poblacion_no_asalariada_ReJov = sum(poblacion_no_asalariada_ReJov),
      actividad_ReJov = poblacion_activa_ReJov / poblacion_total_ReJov,
      inactividad_ReJov = poblacion_inactiva_ReJov / poblacion_total_ReJov,
      desocupacion_ReJov = sum(poblacion_desocupada_ReJov) / poblacion_activa_ReJov,
      empleo_ReJov = sum(poblacion_ocupada_ReJov) / poblacion_total_ReJov,
      ocupado_demandante_ReJov = sum(ocupado_demandante_ReJov) / poblacion_activa_ReJov,
      subocupado_demandante_ReJov = sum(subocupado_demandante_ReJov) /
        poblacion_activa_ReJov,
      otro_ocupado_demandante_ReJov = sum(otro_ocupado_demandante_ReJov) /
        poblacion_activa_ReJov,
      ocupado_no_demandante_ReJov = sum(ocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov,
      subocupado_no_demandante_ReJov = sum(subocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov,
      subocupado_ReJov = sum(poblacion_subocupada_ReJov) / poblacion_activa_ReJov,
      otro_ocupado_no_demandante_ReJov = sum(otro_ocupado_no_demandante_ReJov) /
        poblacion_activa_ReJov
    )
  
      # Junior - Rango de 19-25 ----
  tasa_ReJuP <- datos %>%
    summarise(
      poblacion_total_ReJu = sum(pondera_ReJu),
      poblacion_activa_ReJu = sum(poblacion_ocupada_ReJu) + sum(poblacion_desocupada_ReJu),
      poblacion_inactiva_ReJu = poblacion_total_ReJu - poblacion_activa_ReJu,
      poblacion_ocupada_ReJu = sum(poblacion_ocupada_ReJu),
      poblacion_subocupada_ReJu = sum(poblacion_subocupada_ReJu),
      poblacion_desocupada_ReJu = sum(poblacion_desocupada_ReJu),
      poblacion_asalariada_ReJu = sum(poblacion_asalariada_ReJu),
      poblacion_no_asalariada_ReJu = sum(poblacion_no_asalariada_ReJu),
      actividad_ReJu = poblacion_activa_ReJu / poblacion_total_ReJu,
      inactividad_ReJu = poblacion_inactiva_ReJu / poblacion_total_ReJu,
      desocupacion_ReJu = sum(poblacion_desocupada_ReJu) / poblacion_activa_ReJu,
      empleo_ReJu = sum(poblacion_ocupada_ReJu) / poblacion_total_ReJu,
      ocupado_demandante_ReJu = sum(ocupado_demandante_ReJu) / poblacion_activa_ReJu,
      subocupado_demandante_ReJu = sum(subocupado_demandante_ReJu) /
        poblacion_activa_ReJu,
      otro_ocupado_demandante_ReJu = sum(otro_ocupado_demandante_ReJu) /
        poblacion_activa_ReJu,
      ocupado_no_demandante_ReJu = sum(ocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu,
      subocupado_no_demandante_ReJu = sum(subocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu,
      subocupado_ReJu = sum(poblacion_subocupada_ReJu) / poblacion_activa_ReJu,
      otro_ocupado_no_demandante_ReJu = sum(otro_ocupado_no_demandante_ReJu) /
        poblacion_activa_ReJu
    )
  
      # Semi-senior - Rango de 26-35 ----
  tasa_ReSemiP <- datos %>%
    summarise(
      poblacion_total_ReSemi = sum(pondera_ReSemi),
      poblacion_activa_ReSemi = sum(poblacion_ocupada_ReSemi) + sum(poblacion_desocupada_ReSemi),
      poblacion_inactiva_ReSemi = poblacion_total_ReSemi - poblacion_activa_ReSemi,
      poblacion_ocupada_ReSemi = sum(poblacion_ocupada_ReSemi),
      poblacion_subocupada_ReSemi = sum(poblacion_subocupada_ReSemi),
      poblacion_desocupada_ReSemi = sum(poblacion_desocupada_ReSemi),
      poblacion_asalariada_ReSemi = sum(poblacion_asalariada_ReSemi),
      poblacion_no_asalariada_ReSemi = sum(poblacion_no_asalariada_ReSemi),
      actividad_ReSemi = poblacion_activa_ReSemi / poblacion_total_ReSemi,
      inactividad_ReSemi = poblacion_inactiva_ReSemi / poblacion_total_ReSemi,
      desocupacion_ReSemi = sum(poblacion_desocupada_ReSemi) / poblacion_activa_ReSemi,
      empleo_ReSemi = sum(poblacion_ocupada_ReSemi) / poblacion_total_ReSemi,
      ocupado_demandante_ReSemi = sum(ocupado_demandante_ReSemi) / poblacion_activa_ReSemi,
      subocupado_demandante_ReSemi = sum(subocupado_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      otro_ocupado_demandante_ReSemi = sum(otro_ocupado_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      ocupado_no_demandante_ReSemi = sum(ocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      subocupado_no_demandante_ReSemi = sum(subocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi,
      subocupado_ReSemi = sum(poblacion_subocupada_ReSemi) / poblacion_activa_ReSemi,
      otro_ocupado_no_demandante_ReSemi = sum(otro_ocupado_no_demandante_ReSemi) /
        poblacion_activa_ReSemi
    )
  
      # Senior - Rango de 36-45 ----
  tasa_ReSenP <- datos %>%
    summarise(
      poblacion_total_ReSen = sum(pondera_ReSen),
      poblacion_activa_ReSen = sum(poblacion_ocupada_ReSen) + sum(poblacion_desocupada_ReSen),
      poblacion_inactiva_ReSen = poblacion_total_ReSen - poblacion_activa_ReSen,
      poblacion_ocupada_ReSen = sum(poblacion_ocupada_ReSen),
      poblacion_subocupada_ReSen = sum(poblacion_subocupada_ReSen),
      poblacion_desocupada_ReSen = sum(poblacion_desocupada_ReSen),
      poblacion_asalariada_ReSen = sum(poblacion_asalariada_ReSen),
      poblacion_no_asalariada_ReSen = sum(poblacion_no_asalariada_ReSen),
      actividad_ReSen = poblacion_activa_ReSen / poblacion_total_ReSen,
      inactividad_ReSen = poblacion_inactiva_ReSen / poblacion_total_ReSen,
      desocupacion_ReSen = sum(poblacion_desocupada_ReSen) / poblacion_activa_ReSen,
      empleo_ReSen = sum(poblacion_ocupada_ReSen) / poblacion_total_ReSen,
      ocupado_demandante_ReSen = sum(ocupado_demandante_ReSen) / poblacion_activa_ReSen,
      subocupado_demandante_ReSen = sum(subocupado_demandante_ReSen) /
        poblacion_activa_ReSen,
      otro_ocupado_demandante_ReSen = sum(otro_ocupado_demandante_ReSen) /
        poblacion_activa_ReSen,
      ocupado_no_demandante_ReSen = sum(ocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen,
      subocupado_no_demandante_ReSen = sum(subocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen,
      subocupado_ReSen = sum(poblacion_subocupada_ReSen) / poblacion_activa_ReSen,
      otro_ocupado_no_demandante_ReSen = sum(otro_ocupado_no_demandante_ReSen) /
        poblacion_activa_ReSen
    )
  
      # Especialista I - Rango de 46-55 ----
  tasa_ReEsp1P <- datos %>%
    summarise(
      poblacion_total_ReEsp1 = sum(pondera_ReEsp1),
      poblacion_activa_ReEsp1 = sum(poblacion_ocupada_ReEsp1) + sum(poblacion_desocupada_ReEsp1),
      poblacion_inactiva_ReEsp1 = poblacion_total_ReEsp1 - poblacion_activa_ReEsp1,
      poblacion_ocupada_ReEsp1 = sum(poblacion_ocupada_ReEsp1),
      poblacion_subocupada_ReEsp1 = sum(poblacion_subocupada_ReEsp1),
      poblacion_desocupada_ReEsp1 = sum(poblacion_desocupada_ReEsp1),
      poblacion_asalariada_ReEsp1 = sum(poblacion_asalariada_ReEsp1),
      poblacion_no_asalariada_ReEsp1 = sum(poblacion_no_asalariada_ReEsp1),
      actividad_ReEsp1 = poblacion_activa_ReEsp1 / poblacion_total_ReEsp1,
      inactividad_ReEsp1 = poblacion_inactiva_ReEsp1 / poblacion_total_ReEsp1,
      desocupacion_ReEsp1 = sum(poblacion_desocupada_ReEsp1) / poblacion_activa_ReEsp1,
      empleo_ReEsp1 = sum(poblacion_ocupada_ReEsp1) / poblacion_total_ReEsp1,
      ocupado_demandante_ReEsp1 = sum(ocupado_demandante_ReEsp1) / poblacion_activa_ReEsp1,
      subocupado_demandante_ReEsp1 = sum(subocupado_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      otro_ocupado_demandante_ReEsp1 = sum(otro_ocupado_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      ocupado_no_demandante_ReEsp1 = sum(ocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      subocupado_no_demandante_ReEsp1 = sum(subocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1,
      subocupado_ReEsp1 = sum(poblacion_subocupada_ReEsp1) / poblacion_activa_ReEsp1,
      otro_ocupado_no_demandante_ReEsp1 = sum(otro_ocupado_no_demandante_ReEsp1) /
        poblacion_activa_ReEsp1
    )
  
      # Especialista II - Rango de 56-65 ----
  tasa_ReEsp2P <- datos %>%
    summarise(
      poblacion_total_ReEsp2 = sum(pondera_ReEsp2),
      poblacion_activa_ReEsp2 = sum(poblacion_ocupada_ReEsp2) + sum(poblacion_desocupada_ReEsp2),
      poblacion_inactiva_ReEsp2 = poblacion_total_ReEsp2 - poblacion_activa_ReEsp2,
      poblacion_ocupada_ReEsp2 = sum(poblacion_ocupada_ReEsp2),
      poblacion_subocupada_ReEsp2 = sum(poblacion_subocupada_ReEsp2),
      poblacion_desocupada_ReEsp2 = sum(poblacion_desocupada_ReEsp2),
      poblacion_asalariada_ReEsp2 = sum(poblacion_asalariada_ReEsp2),
      poblacion_no_asalariada_ReEsp2 = sum(poblacion_no_asalariada_ReEsp2),
      actividad_ReEsp2 = poblacion_activa_ReEsp2 / poblacion_total_ReEsp2,
      inactividad_ReEsp2 = poblacion_inactiva_ReEsp2 / poblacion_total_ReEsp2,
      desocupacion_ReEsp2 = sum(poblacion_desocupada_ReEsp2) / poblacion_activa_ReEsp2,
      empleo_ReEsp2 = sum(poblacion_ocupada_ReEsp2) / poblacion_total_ReEsp2,
      ocupado_demandante_ReEsp2 = sum(ocupado_demandante_ReEsp2) / poblacion_activa_ReEsp2,
      subocupado_demandante_ReEsp2 = sum(subocupado_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      otro_ocupado_demandante_ReEsp2 = sum(otro_ocupado_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      ocupado_no_demandante_ReEsp2 = sum(ocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      subocupado_no_demandante_ReEsp2 = sum(subocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2,
      subocupado_ReEsp2 = sum(poblacion_subocupada_ReEsp2) / poblacion_activa_ReEsp2,
      otro_ocupado_no_demandante_ReEsp2 = sum(otro_ocupado_no_demandante_ReEsp2) /
        poblacion_activa_ReEsp2
    )
  
      # En edad jubilatoria - Rango 66+ ----
  tasa_ReJubP <- datos %>%
    summarise(
      poblacion_total_ReJub = sum(pondera_ReJub),
      poblacion_activa_ReJub = sum(poblacion_ocupada_ReJub) + sum(poblacion_desocupada_ReJub),
      poblacion_inactiva_ReJub = poblacion_total_ReJub - poblacion_activa_ReJub,
      poblacion_ocupada_ReJub = sum(poblacion_ocupada_ReJub),
      poblacion_subocupada_ReJub = sum(poblacion_subocupada_ReJub),
      poblacion_desocupada_ReJub = sum(poblacion_desocupada_ReJub),
      poblacion_asalariada_ReJub = sum(poblacion_asalariada_ReJub),
      poblacion_no_asalariada_ReJub = sum(poblacion_no_asalariada_ReJub),
      actividad_ReJub = poblacion_activa_ReJub / poblacion_total_ReJub,
      inactividad_ReJub = poblacion_inactiva_ReJub / poblacion_total_ReJub,
      desocupacion_ReJub = sum(poblacion_desocupada_ReJub) / poblacion_activa_ReJub,
      empleo_ReJub = sum(poblacion_ocupada_ReJub) / poblacion_total_ReJub,
      ocupado_demandante_ReJub = sum(ocupado_demandante_ReJub) / poblacion_activa_ReJub,
      subocupado_demandante_ReJub = sum(subocupado_demandante_ReJub) /
        poblacion_activa_ReJub,
      otro_ocupado_demandante_ReJub = sum(otro_ocupado_demandante_ReJub) /
        poblacion_activa_ReJub,
      ocupado_no_demandante_ReJub = sum(ocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub,
      subocupado_no_demandante_ReJub = sum(subocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub,
      subocupado_ReJub = sum(poblacion_subocupada_ReJub) / poblacion_activa_ReJub,
      otro_ocupado_no_demandante_ReJub = sum(otro_ocupado_no_demandante_ReJub) /
        poblacion_activa_ReJub
    )
  
  # Cruce con condición de formalidad ----
      # Formales ----
  tasa_CF <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_CF = sum(pondera_CF),
      poblacion_activa_CF = sum(poblacion_ocupada_CF) + sum(poblacion_desocupada_CF),
      poblacion_inactiva_CF = poblacion_total_CF - poblacion_activa_CF,
      poblacion_ocupada_CF = sum(poblacion_ocupada_CF),
      poblacion_subocupada_CF = sum(poblacion_subocupada_CF),
      poblacion_desocupada_CF = sum(poblacion_desocupada_CF),
      poblacion_asalariada_CF = sum(poblacion_asalariada_CF),
      poblacion_no_asalariada_CF = sum(poblacion_no_asalariada_CF),
      actividad_CF = poblacion_activa_CF / poblacion_total_CF,
      inactividad_CF = poblacion_inactiva_CF / poblacion_total_CF,
      desocupacion_CF = sum(poblacion_desocupada_CF) / poblacion_activa_CF,
      empleo_CF = sum(poblacion_ocupada_CF) / poblacion_total_CF,
      ocupado_demandante_CF = sum(ocupado_demandante_CF) / poblacion_activa_CF,
      subocupado_demandante_CF = sum(subocupado_demandante_CF) / poblacion_activa_CF,
      otro_ocupado_demandante_CF = sum(otro_ocupado_demandante_CF) /
        poblacion_activa_CF,
      ocupado_no_demandante_CF = sum(ocupado_no_demandante_CF) / poblacion_activa_CF,
      subocupado_no_demandante_CF = sum(subocupado_no_demandante_CF) /
        poblacion_activa_CF,
      subocupado_CF = sum(poblacion_subocupada_CF) / poblacion_activa_CF,
      otro_ocupado_no_demandante_CF = sum(otro_ocupado_no_demandante_CF) /
        poblacion_activa_CF
    )
  
      # No formales ----
  tasa_CI <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_CI = sum(pondera_CI),
      poblacion_activa_CI = sum(poblacion_ocupada_CI) + sum(poblacion_desocupada_CI),
      poblacion_inactiva_CI = poblacion_total_CI - poblacion_activa_CI,
      poblacion_ocupada_CI = sum(poblacion_ocupada_CI),
      poblacion_subocupada_CI = sum(poblacion_subocupada_CI),
      poblacion_desocupada_CI = sum(poblacion_desocupada_CI),
      poblacion_asalariada_CI = sum(poblacion_asalariada_CI),
      poblacion_no_asalariada_CI = sum(poblacion_no_asalariada_CI),
      actividad_CI = poblacion_activa_CI / poblacion_total_CI,
      inactividad_CI = poblacion_inactiva_CI / poblacion_total_CI,
      desocupacion_CI = sum(poblacion_desocupada_CI) / poblacion_activa_CI,
      empleo_CI = sum(poblacion_ocupada_CI) / poblacion_total_CI,
      ocupado_demandante_CI = sum(ocupado_demandante_CI) / poblacion_activa_CI,
      subocupado_demandante_CI = sum(subocupado_demandante_CI) / poblacion_activa_CI,
      otro_ocupado_demandante_CI = sum(otro_ocupado_demandante_CI) /
        poblacion_activa_CI,
      ocupado_no_demandante_CI = sum(ocupado_no_demandante_CI) / poblacion_activa_CI,
      subocupado_no_demandante_CI = sum(subocupado_no_demandante_CI) /
        poblacion_activa_CI,
      subocupado_CI = sum(poblacion_subocupada_CI) / poblacion_activa_CI,
      otro_ocupado_no_demandante_CI = sum(otro_ocupado_no_demandante_CI) /
        poblacion_activa_CI
    )
  
  # Cruce con condición de formalidad PAIS ----
      # Formales ----
  tasa_CFP <- datos %>%
    summarise(
      poblacion_total_CF = sum(pondera_CF),
      poblacion_activa_CF = sum(poblacion_ocupada_CF) + sum(poblacion_desocupada_CF),
      poblacion_inactiva_CF = poblacion_total_CF - poblacion_activa_CF,
      poblacion_ocupada_CF = sum(poblacion_ocupada_CF),
      poblacion_subocupada_CF = sum(poblacion_subocupada_CF),
      poblacion_desocupada_CF = sum(poblacion_desocupada_CF),
      poblacion_asalariada_CF = sum(poblacion_asalariada_CF),
      poblacion_no_asalariada_CF = sum(poblacion_no_asalariada_CF),
      actividad_CF = poblacion_activa_CF / poblacion_total_CF,
      inactividad_CF = poblacion_inactiva_CF / poblacion_total_CF,
      desocupacion_CF = sum(poblacion_desocupada_CF) / poblacion_activa_CF,
      empleo_CF = sum(poblacion_ocupada_CF) / poblacion_total_CF,
      ocupado_demandante_CF = sum(ocupado_demandante_CF) / poblacion_activa_CF,
      subocupado_demandante_CF = sum(subocupado_demandante_CF) / poblacion_activa_CF,
      otro_ocupado_demandante_CF = sum(otro_ocupado_demandante_CF) /
        poblacion_activa_CF,
      ocupado_no_demandante_CF = sum(ocupado_no_demandante_CF) / poblacion_activa_CF,
      subocupado_no_demandante_CF = sum(subocupado_no_demandante_CF) /
        poblacion_activa_CF,
      subocupado_CF = sum(poblacion_subocupada_CF) / poblacion_activa_CF,
      otro_ocupado_no_demandante_CF = sum(otro_ocupado_no_demandante_CF) /
        poblacion_activa_CF
    )
  
      # No formales ----
  tasa_CIP <- datos %>%
    summarise(
      poblacion_total_CI = sum(pondera_CI),
      poblacion_activa_CI = sum(poblacion_ocupada_CI) + sum(poblacion_desocupada_CI),
      poblacion_inactiva_CI = poblacion_total_CI - poblacion_activa_CI,
      poblacion_ocupada_CI = sum(poblacion_ocupada_CI),
      poblacion_subocupada_CI = sum(poblacion_subocupada_CI),
      poblacion_desocupada_CI = sum(poblacion_desocupada_CI),
      poblacion_asalariada_CI = sum(poblacion_asalariada_CI),
      poblacion_no_asalariada_CI = sum(poblacion_no_asalariada_CI),
      actividad_CI = poblacion_activa_CI / poblacion_total_CI,
      inactividad_CI = poblacion_inactiva_CI / poblacion_total_CI,
      desocupacion_CI = sum(poblacion_desocupada_CI) / poblacion_activa_CI,
      empleo_CI = sum(poblacion_ocupada_CI) / poblacion_total_CI,
      ocupado_demandante_CI = sum(ocupado_demandante_CI) / poblacion_activa_CI,
      subocupado_demandante_CI = sum(subocupado_demandante_CI) / poblacion_activa_CI,
      otro_ocupado_demandante_CI = sum(otro_ocupado_demandante_CI) /
        poblacion_activa_CI,
      ocupado_no_demandante_CI = sum(ocupado_no_demandante_CI) / poblacion_activa_CI,
      subocupado_no_demandante_CI = sum(subocupado_no_demandante_CI) /
        poblacion_activa_CI,
      subocupado_CI = sum(poblacion_subocupada_CI) / poblacion_activa_CI,
      otro_ocupado_no_demandante_CI = sum(otro_ocupado_no_demandante_CI) /
        poblacion_activa_CI
    )
  
  # Cruce con Tipo de empleo ----
      # Empleo privado ----
  tasa_EmPriv <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EmPriv = sum(pondera_EmPriv),
      poblacion_activa_EmPriv = sum(poblacion_ocupada_EmPriv) + sum(poblacion_desocupada_EmPriv),
      poblacion_inactiva_EmPriv = poblacion_total_EmPriv - poblacion_activa_EmPriv,
      poblacion_ocupada_EmPriv = sum(poblacion_ocupada_EmPriv),
      poblacion_subocupada_EmPriv = sum(poblacion_subocupada_EmPriv),
      poblacion_desocupada_EmPriv = sum(poblacion_desocupada_EmPriv),
      poblacion_asalariada_EmPriv = sum(poblacion_asalariada_EmPriv),
      poblacion_no_asalariada_EmPriv = sum(poblacion_no_asalariada_EmPriv),
      actividad_EmPriv = poblacion_activa_EmPriv / poblacion_total_EmPriv,
      inactividad_EmPriv = poblacion_inactiva_EmPriv / poblacion_total_EmPriv,
      desocupacion_EmPriv = sum(poblacion_desocupada_EmPriv) / poblacion_activa_EmPriv,
      empleo_EmPriv = sum(poblacion_ocupada_EmPriv) / poblacion_total_EmPriv,
      ocupado_demandante_EmPriv = sum(ocupado_demandante_EmPriv) / poblacion_activa_EmPriv,
      subocupado_demandante_EmPriv = sum(subocupado_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      otro_ocupado_demandante_EmPriv = sum(otro_ocupado_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      ocupado_no_demandante_EmPriv = sum(ocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      subocupado_no_demandante_EmPriv = sum(subocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      subocupado_EmPriv = sum(poblacion_subocupada_EmPriv) / poblacion_activa_EmPriv,
      otro_ocupado_no_demandante_EmPriv = sum(otro_ocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv
    )
  
      # Empleo Público ----
  tasa_EmPubl <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EmPubl = sum(pondera_EmPubl),
      poblacion_activa_EmPubl = sum(poblacion_ocupada_EmPubl) + sum(poblacion_desocupada_EmPubl),
      poblacion_inactiva_EmPubl = poblacion_total_EmPubl - poblacion_activa_EmPubl,
      poblacion_ocupada_EmPubl = sum(poblacion_ocupada_EmPubl),
      poblacion_subocupada_EmPubl = sum(poblacion_subocupada_EmPubl),
      poblacion_desocupada_EmPubl = sum(poblacion_desocupada_EmPubl),
      poblacion_asalariada_EmPubl = sum(poblacion_asalariada_EmPubl),
      poblacion_no_asalariada_EmPubl = sum(poblacion_no_asalariada_EmPubl),
      actividad_EmPubl = poblacion_activa_EmPubl / poblacion_total_EmPubl,
      inactividad_EmPubl = poblacion_inactiva_EmPubl / poblacion_total_EmPubl,
      desocupacion_EmPubl = sum(poblacion_desocupada_EmPubl) / poblacion_activa_EmPubl,
      empleo_EmPubl = sum(poblacion_ocupada_EmPubl) / poblacion_total_EmPubl,
      ocupado_demandante_EmPubl = sum(ocupado_demandante_EmPubl) / poblacion_activa_EmPubl,
      subocupado_demandante_EmPubl = sum(subocupado_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      otro_ocupado_demandante_EmPubl = sum(otro_ocupado_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      ocupado_no_demandante_EmPubl = sum(ocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      subocupado_no_demandante_EmPubl = sum(subocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      subocupado_EmPubl = sum(poblacion_subocupada_EmPubl) / poblacion_activa_EmPubl,
      otro_ocupado_no_demandante_EmPubl = sum(otro_ocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl
    )
  
      # Otro tipo de empleo ----
  tasa_EmOtro <- datos %>%
    group_by(aglomerado) %>%
    summarise(
      poblacion_total_EmOtro = sum(pondera_EmOtro),
      poblacion_activa_EmOtro = sum(poblacion_ocupada_EmOtro) + sum(poblacion_desocupada_EmOtro),
      poblacion_inactiva_EmOtro = poblacion_total_EmOtro - poblacion_activa_EmOtro,
      poblacion_ocupada_EmOtro = sum(poblacion_ocupada_EmOtro),
      poblacion_subocupada_EmOtro = sum(poblacion_subocupada_EmOtro),
      poblacion_desocupada_EmOtro = sum(poblacion_desocupada_EmOtro),
      poblacion_asalariada_EmOtro = sum(poblacion_asalariada_EmOtro),
      poblacion_no_asalariada_EmOtro = sum(poblacion_no_asalariada_EmOtro),
      actividad_EmOtro = poblacion_activa_EmOtro / poblacion_total_EmOtro,
      inactividad_EmOtro = poblacion_inactiva_EmOtro / poblacion_total_EmOtro,
      desocupacion_EmOtro = sum(poblacion_desocupada_EmOtro) / poblacion_activa_EmOtro,
      empleo_EmOtro = sum(poblacion_ocupada_EmOtro) / poblacion_total_EmOtro,
      ocupado_demandante_EmOtro = sum(ocupado_demandante_EmOtro) / poblacion_activa_EmOtro,
      subocupado_demandante_EmOtro = sum(subocupado_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      otro_ocupado_demandante_EmOtro = sum(otro_ocupado_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      ocupado_no_demandante_EmOtro = sum(ocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      subocupado_no_demandante_EmOtro = sum(subocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      subocupado_EmOtro = sum(poblacion_subocupada_EmOtro) / poblacion_activa_EmOtro,
      otro_ocupado_no_demandante_EmOtro = sum(otro_ocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro
    )
  # Cruce con Tipo de empleo PAIS ----
      # Empleo privado ----
  tasa_EmPrivP <- datos %>%
    summarise(
      poblacion_total_EmPriv = sum(pondera_EmPriv),
      poblacion_activa_EmPriv = sum(poblacion_ocupada_EmPriv) + sum(poblacion_desocupada_EmPriv),
      poblacion_inactiva_EmPriv = poblacion_total_EmPriv - poblacion_activa_EmPriv,
      poblacion_ocupada_EmPriv = sum(poblacion_ocupada_EmPriv),
      poblacion_subocupada_EmPriv = sum(poblacion_subocupada_EmPriv),
      poblacion_desocupada_EmPriv = sum(poblacion_desocupada_EmPriv),
      poblacion_asalariada_EmPriv = sum(poblacion_asalariada_EmPriv),
      poblacion_no_asalariada_EmPriv = sum(poblacion_no_asalariada_EmPriv),
      actividad_EmPriv = poblacion_activa_EmPriv / poblacion_total_EmPriv,
      inactividad_EmPriv = poblacion_inactiva_EmPriv / poblacion_total_EmPriv,
      desocupacion_EmPriv = sum(poblacion_desocupada_EmPriv) / poblacion_activa_EmPriv,
      empleo_EmPriv = sum(poblacion_ocupada_EmPriv) / poblacion_total_EmPriv,
      ocupado_demandante_EmPriv = sum(ocupado_demandante_EmPriv) / poblacion_activa_EmPriv,
      subocupado_demandante_EmPriv = sum(subocupado_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      otro_ocupado_demandante_EmPriv = sum(otro_ocupado_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      ocupado_no_demandante_EmPriv = sum(ocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      subocupado_no_demandante_EmPriv = sum(subocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv,
      subocupado_EmPriv = sum(poblacion_subocupada_EmPriv) / poblacion_activa_EmPriv,
      otro_ocupado_no_demandante_EmPriv = sum(otro_ocupado_no_demandante_EmPriv) /
        poblacion_activa_EmPriv
    )
  
      # Empleo Público ----
  tasa_EmPublP <- datos %>%
    summarise(
      poblacion_total_EmPubl = sum(pondera_EmPubl),
      poblacion_activa_EmPubl = sum(poblacion_ocupada_EmPubl) + sum(poblacion_desocupada_EmPubl),
      poblacion_inactiva_EmPubl = poblacion_total_EmPubl - poblacion_activa_EmPubl,
      poblacion_ocupada_EmPubl = sum(poblacion_ocupada_EmPubl),
      poblacion_subocupada_EmPubl = sum(poblacion_subocupada_EmPubl),
      poblacion_desocupada_EmPubl = sum(poblacion_desocupada_EmPubl),
      poblacion_asalariada_EmPubl = sum(poblacion_asalariada_EmPubl),
      poblacion_no_asalariada_EmPubl = sum(poblacion_no_asalariada_EmPubl),
      actividad_EmPubl = poblacion_activa_EmPubl / poblacion_total_EmPubl,
      inactividad_EmPubl = poblacion_inactiva_EmPubl / poblacion_total_EmPubl,
      desocupacion_EmPubl = sum(poblacion_desocupada_EmPubl) / poblacion_activa_EmPubl,
      empleo_EmPubl = sum(poblacion_ocupada_EmPubl) / poblacion_total_EmPubl,
      ocupado_demandante_EmPubl = sum(ocupado_demandante_EmPubl) / poblacion_activa_EmPubl,
      subocupado_demandante_EmPubl = sum(subocupado_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      otro_ocupado_demandante_EmPubl = sum(otro_ocupado_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      ocupado_no_demandante_EmPubl = sum(ocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      subocupado_no_demandante_EmPubl = sum(subocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl,
      subocupado_EmPubl = sum(poblacion_subocupada_EmPubl) / poblacion_activa_EmPubl,
      otro_ocupado_no_demandante_EmPubl = sum(otro_ocupado_no_demandante_EmPubl) /
        poblacion_activa_EmPubl
    )
  
      # Otro tipo de empleo ----
  tasa_EmOtroP <- datos %>%
    summarise(
      poblacion_total_EmOtro = sum(pondera_EmOtro),
      poblacion_activa_EmOtro = sum(poblacion_ocupada_EmOtro) + sum(poblacion_desocupada_EmOtro),
      poblacion_inactiva_EmOtro = poblacion_total_EmOtro - poblacion_activa_EmOtro,
      poblacion_ocupada_EmOtro = sum(poblacion_ocupada_EmOtro),
      poblacion_subocupada_EmOtro = sum(poblacion_subocupada_EmOtro),
      poblacion_desocupada_EmOtro = sum(poblacion_desocupada_EmOtro),
      poblacion_asalariada_EmOtro = sum(poblacion_asalariada_EmOtro),
      poblacion_no_asalariada_EmOtro = sum(poblacion_no_asalariada_EmOtro),
      actividad_EmOtro = poblacion_activa_EmOtro / poblacion_total_EmOtro,
      inactividad_EmOtro = poblacion_inactiva_EmOtro / poblacion_total_EmOtro,
      desocupacion_EmOtro = sum(poblacion_desocupada_EmOtro) / poblacion_activa_EmOtro,
      empleo_EmOtro = sum(poblacion_ocupada_EmOtro) / poblacion_total_EmOtro,
      ocupado_demandante_EmOtro = sum(ocupado_demandante_EmOtro) / poblacion_activa_EmOtro,
      subocupado_demandante_EmOtro = sum(subocupado_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      otro_ocupado_demandante_EmOtro = sum(otro_ocupado_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      ocupado_no_demandante_EmOtro = sum(ocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      subocupado_no_demandante_EmOtro = sum(subocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro,
      subocupado_EmOtro = sum(poblacion_subocupada_EmOtro) / poblacion_activa_EmOtro,
      otro_ocupado_no_demandante_EmOtro = sum(otro_ocupado_no_demandante_EmOtro) /
        poblacion_activa_EmOtro
    )
  
  # Gini ----
  G <-
    mutate_at(
      datos,
      c(
        "ingreso_real_ocupacion_principal",
        "ingreso_real_otras_ocupaciones",
        "ingreso_real_jubilacion",
        "ingreso_real_aguinaldo",
        "ingreso_real_seguro_desempleo",
        "ingreso_real_subsidio",
        "ingreso_real_alquiler",
        "ingreso_real_negocio",
        "ingreso_real_renta"
      ),
      ~ replace(., is.na(.), 0)
    )
  G <- G %>%
    mutate_if(is.double, as.numeric) %>%
    filter(ingreso_real_capita_familiar > 0) %>%
    group_by(id_hogar) %>%
    mutate(
      #guarda con el paquete plyr
      miembros = max(componente),
      ocupacion_principal = sum(ingreso_real_ocupacion_principal) /
        miembros,
      ocupacion_secundaria = sum(ingreso_real_otras_ocupaciones) / miembros,
      jubilacion = sum(ingreso_real_jubilacion + ingreso_real_aguinaldo) /
        miembros,
      transferencias = sum(ingreso_real_seguro_desempleo + ingreso_real_subsidio) /
        miembros,
      capital = sum(
        ingreso_real_alquiler + ingreso_real_negocio + ingreso_real_renta
      ) / miembros
    ) %>%
    ungroup()
  # Gini aglomerado ----
  CGini1 <- G %>%
    group_by(aglomerado) %>%
    summarise(ircf = Gini(ingreso_real_capita_familiar, pondih))
  
  CGini2 <- G %>%
    filter(ocupacion_principal > 0) %>%
    group_by(aglomerado) %>%
    summarise(ocprin = Gini(ocupacion_principal, pondiio))
  
  CGini3 <- G %>%
    filter(ocupacion_secundaria > 0) %>%
    group_by(aglomerado) %>%
    summarise(ocsec = Gini(ocupacion_secundaria, pondera))
  
  CGini4 <- G %>%
    filter(jubilacion > 0) %>%
    group_by(aglomerado) %>%
    summarise(jubi = Gini(jubilacion, pondera))
  
  CGini5 <- G %>%
    filter(transferencias > 0) %>%
    group_by(aglomerado) %>%
    summarise(transf = Gini(transferencias, pondera))
  
  CGini6 <- G %>%
    filter(capital > 0) %>%
    group_by(aglomerado) %>%
    summarise(capi = Gini(capital, pondera))
  
  LYGini <- G %>%
    group_by(aglomerado) %>%
    summarise(
      lyircf = Gini(ingreso_real_capita_familiar, pondera),
      lyocprin = Gini(ocupacion_principal, pondiio),
      lyocsec = Gini(ocupacion_secundaria, pondera),
      lyjubi = Gini(jubilacion, pondera),
      lytransf = Gini(transferencias, pondera),
      lycapi = Gini(capital, pondera)
    )
  # Gini PAIS ----
  CGini1P <- G %>%
    summarise(ircf = Gini(ingreso_real_capita_familiar, pondih))
  
  CGini2P <- G %>%
    filter(ocupacion_principal > 0) %>%
    summarise(ocprin = Gini(ocupacion_principal, pondiio))
  
  CGini3P <- G %>%
    filter(ocupacion_secundaria > 0) %>%
    summarise(ocsec = Gini(ocupacion_secundaria, pondera))
  
  CGini4P <- G %>%
    filter(jubilacion > 0) %>%
    summarise(jubi = Gini(jubilacion, pondera))
  
  CGini5P <- G %>%
    filter(transferencias > 0) %>%
    summarise(transf = Gini(transferencias, pondera))
  
  CGini6P <- G %>%
    filter(capital > 0) %>%
    summarise(capi = Gini(capital, pondera))
  
  LYGiniP <- G %>%
    summarise(
      lyircf = Gini(ingreso_real_capita_familiar, pondera),
      lyocprin = Gini(ocupacion_principal, pondiio),
      lyocsec = Gini(ocupacion_secundaria, pondera),
      lyjubi = Gini(jubilacion, pondera),
      lytransf = Gini(transferencias, pondera),
      lycapi = Gini(capital, pondera)
    )
  
  # Unión ----
  tasas <- tasasG %>%
    full_join(tasasS) %>%
    full_join(tasa_EdNo) %>%
    full_join(tasa_EdPI) %>%
    full_join(tasa_EdPC) %>%
    full_join(tasa_EdSI) %>%
    full_join(tasa_EdSC) %>%
    full_join(tasa_EdTI) %>%
    full_join(tasa_EdTC) %>%
    full_join(tasa_EdUI) %>%
    full_join(tasa_EdUC) %>%
    full_join(tasa_ReNo) %>%
    full_join(tasa_ReMe) %>%
    full_join(tasa_ReJov) %>%
    full_join(tasa_ReJu) %>%
    full_join(tasa_ReSemi) %>%
    full_join(tasa_ReSen) %>%
    full_join(tasa_ReEsp1) %>%
    full_join(tasa_ReEsp2) %>%
    full_join(tasa_ReJub) %>%
    full_join(tasa_CF) %>%
    full_join(tasa_CI) %>%
    full_join(tasa_EmPubl) %>%
    full_join(tasa_EmPriv) %>%
    full_join(tasa_EmOtro) %>%
    full_join(CGini1) %>%
    full_join(CGini2) %>%
    full_join(CGini3) %>%
    full_join(CGini4) %>%
    full_join(CGini5) %>%
    full_join(CGini6) %>%
    full_join(LYGini) %>%
    mutate(
      periodo = paste0("20", periodo$anio[i], "_T", periodo$trimestre[i]),
      aglomerado = as.character(aglomerado)
    )
  
  rm(
    tasasG,
    tasasS,
    tasa_EdNo,
    tasa_EdPI,
    tasa_EdPC,
    tasa_EdSI,
    tasa_EdSC,
    tasa_EdTI,
    tasa_EdTC,
    tasa_EdUI,
    tasa_EdUC,
    tasa_CF,
    tasa_CI,
    tasa_ReNo,
    tasa_ReMe,
    tasa_ReJov,
    tasa_ReJu,
    tasa_ReSemi,
    tasa_ReSen,
    tasa_ReEsp1,
    tasa_ReEsp2,
    tasa_ReJub,
    tasa_EmPriv,
    tasa_EmPubl,
    tasa_EmOtro,
    CGini1,
    CGini2,
    CGini3,
    CGini4,
    CGini5,
    CGini6,
    LYGini
  )
  # Unifico PAIS ----
  tasasP <- tasasP %>%
    full_join(tasasSP) %>%
    full_join(tasa_EdNoP) %>%
    full_join(tasa_EdPIP) %>%
    full_join(tasa_EdPCP) %>%
    full_join(tasa_EdSIP) %>%
    full_join(tasa_EdSCP) %>%
    full_join(tasa_EdTIP) %>%
    full_join(tasa_EdTCP) %>%
    full_join(tasa_EdUIP) %>%
    full_join(tasa_EdUCP) %>%
    full_join(tasa_ReNoP) %>%
    full_join(tasa_ReMeP) %>%
    full_join(tasa_ReJovP) %>%
    full_join(tasa_ReJuP) %>%
    full_join(tasa_ReSemiP) %>%
    full_join(tasa_ReSenP) %>%
    full_join(tasa_ReEsp1P) %>%
    full_join(tasa_ReEsp2P) %>%
    full_join(tasa_ReJubP) %>%
    full_join(tasa_CFP) %>%
    full_join(tasa_CIP) %>%
    full_join(tasa_EmPublP) %>%
    full_join(tasa_EmPrivP) %>%
    full_join(tasa_EmOtroP) %>%
    full_join(CGini1P) %>%
    full_join(CGini2P) %>%
    full_join(CGini3P) %>%
    full_join(CGini4P) %>%
    full_join(CGini5P) %>%
    full_join(CGini6P) %>%
    full_join(LYGiniP) %>%
    mutate(
      periodo = paste0("20", periodo$anio[i], "_T", periodo$trimestre[i]),
      aglomerado = c("Todos los aglomerados")
    )
  
  rm(
    tasasGP,
    tasasSP,
    tasa_EdNoP,
    tasa_EdPIP,
    tasa_EdPCP,
    tasa_EdSIP,
    tasa_EdSCP,
    tasa_EdTIP,
    tasa_EdTCP,
    tasa_EdUIP,
    tasa_EdUCP,
    tasa_CFP,
    tasa_CIP,
    tasa_ReNoP,
    tasa_ReMeP,
    tasa_ReJovP,
    tasa_ReJuP,
    tasa_ReSemiP,
    tasa_ReSenP,
    tasa_ReEsp1P,
    tasa_ReEsp2P,
    tasa_ReJubP,
    tasa_EmPriv,
    tasa_EmPublP,
    tasa_EmOtroP,
    CGini1P,
    CGini2P,
    CGini3P,
    CGini4P,
    CGini5P,
    CGini6P,
    LYGiniP
  )
  rm(
    variables,
    variablesS,
    G,
    cat_ocupacional,
    educacion,
    etario,
    fabs,
    formalidad,
    Gini,
    sexo,
    tipo_empleo
  )
  
  # uno aglomerados y total pais ----
  tasas <- rbind(tasas, tasasP)
  # Unifico períodos en un archivo ----
  if (i == 1) {
    salida <- tasas
  } else {
    salida <- rbind(salida, tasas)
  }
  setwd(ruta_results)
  save(datos, file = file_name)
  save(salida, file = exit_name)
}

# Luego, guardo el archivo unificado ----
save(salida, file = "tasas_unificado.csv")
