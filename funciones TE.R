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