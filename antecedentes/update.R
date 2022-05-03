# ACTUALIZAR LA TABLA DE PONDERADORES #####

# https://www.datasciencemadesimple.com/match-function-in-r/


# FUNCION: Actualiza la base [ponderados]
#          Cuando se crean nuevas variables, identifica a aquellas que deben ponderarse por un peso distinto al de [pondera] 
update_pond <- function(data, pond) {
  # Selecciona las variables que han sido creadas y pueden ser afectadas por ponderadores distintos a pondera
  aux <- data %>%
    select(starts_with("ingreso"), starts_with("cobro")) 
  # Crea un dataframe auxilar, con el mismo nro de filas que columnas tiene aux
  aux2 <- data.frame(matrix(NA, nrow = ncol(aux), ncol = 2)) %>%
    rename(var = X1, var2 = X2)
  # Se queda solo con los nombres de las variables potencialmente elegibles
  aux <- colnames(aux)
  # Completa el dataframe aux2 con los nombres de las variables potencialmente elegibles
  aux2[ , 1] <- aux
  # Crea una referencia común [var2] para la base aux2
  for (i in aux) {
    aux3 <- str_match(aux2$var, i)
    aux2 <- cbind(aux2, aux3)
    aux2 <- aux2 %>%
      mutate(var2 = ifelse(!is.na(var2), var2, aux3)) %>%
      select(-aux3)
    }
  # Completa los ponderadores para cada variable de aux2 a través de la referencia común trazada con [pond]
  aux2 <- inner_join(aux2, pond, by = c("var2" = "var")) %>%
    select(-var2)
  # Une aux2 y pond, eliminando repetidos
  union(pond, aux2)
}


# Actualiza el dataframe de variables con ponderados distinto a pondera
ponderados <- update_pond(datos, ponderados)






# FUNCION: Identifica que ponderador le corresponde a una variable [x] en relación a una tabla relación entre variables y ponderadores [ponder]
#         - EJEMPLO: for (i in ingr1) {assign_pond(i, ponderados)}
assign_pond <- function(x) {
  # Se establece si existe alguna coincidencia entre la variable elegida [x] y las que tienen asignado un ponderador distinto en [ponderados]
  aux <- str_which(ponderados$var, x)
  # Para aquellas que no están en la lista de [ponderados], se le asigna el ponderador = pondera
  if (length(aux) == 0) {
    print("pondera")
  }
  # Para las que figuran en [ponder], se les asigna el ponderador que corresponde
  else {
    aux2 <- data.frame(matrix(NA, nrow = 1, ncol = 2)) %>%
      rename(var = X1) %>%
      select(-X2)
    aux2[1,1] <- x
    aux3 <- inner_join(aux2, ponderados, by = "var")
    print(aux3$ponder)
  }
}




x <- c("ingreso_real_familiar")
assign_pond(x)




#--------------------

# 1. Carga del diccionario ----------------------------------------------------------

diccio <- read_excel("/oes/eph_base/data/diccionario.xlsx", sheet = "diccio")


# 2. Extracción de los nombres de las variables a condicionar del diccionario -------

x <- unique(diccio$Variable)

# unique = devuelve un vector, marco de datos o matriz como x pero con elementos
#          / filas duplicados eliminados.


# 3. Loop para asignar etiquetas del diccionario a las variables seleccionadas en x -----

for (j in x) {
  manual <- diccio %>% 
    filter(Variable == j) %>% 
    select(id, Nivel)
  indice <- match(pull(datos, j), manual$id)
  datos[, j] <- manual$Nivel[indice]
}

datos[, x] <- lapply(datos[, x], factor)




  manual <- diccio %>% 
    filter(Variable == "caracter") %>% 
    select(id, Nivel)
  indice <- match(pull(datos, "caracter"), manual$id)
  datos2[, j] <- manual$Nivel[indice]


datos[, x] <- lapply(datos[, x], factor)
