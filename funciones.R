# ARCHIVO: funciones ####

## OBJETIVO: Reunir las funciones que van a ser utilizadas para la expansión de las variables en capa2

## ANTECEDENTES: Se toman de enfoque2 completo, excluyendo el código de original que figura como texto.

## Modificaciones: - Se generó como un apartado para la carga de funciones
##                 - Se incorporan las funciones de gestión de ponderadores que no estaban presentes en los antecedentes.
##                 - Para esto último, es requisito tener cargado la base [ponderados]
##                 - La función [cruza_var] originalmente era [f1 modificada]. Con la carga de [ponderados] puede ponderar automaticamente.



# GESTIÓN DE PONDERADORES ####

# FUNCION [update_pond]: 
# - Actualiza la base [ponderados]
# - Cuando se crean nuevas variables, identifica a aquellas que deben ponderarse por un peso distinto al de [pondera] y las incorpora
# - La ruta de la base [ponderados] se encuentra en ["C:/oes/eph_capa3/diccionario/tabla_ponder.xlsx"]
# - La carga de [ponderados] ocurre en [capa2_plus]

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


# FUNCION [assign_pond]: 
# - Necesita que esté cargada la base [ponderados]
# - La ruta de la base [ponderados] se encuentra en ["C:/oes/eph_capa3/diccionario/tabla_ponder.xlsx"]
# - Identifica que ponderador le corresponde a una variable [x] en relación a una tabla relación entre variables y ponderadores [ponder]
# - EJEMPLO: for (i in ingr1) {assign_pond(i, ponderados)}

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



# NOMBRADO DE VARIABLES ----


## FUNCION [prefijo]:
# - Se utiliza como insumo de las funciones de cruce de variables
# - Nuclea el nombrado de las matrices de dummies que se generan en los cruces, sobre las variables seleccionadas
# - Las variables seleccionadas son las que figuran en cada subtitulo/nota de la función
# - En caso de necesitar incorporar nuevas variables de cruce, los nombres se incorporan en esta función

prefijo <- function(vector) {
  case_when(
    # Sexo (var --> "sexo")
    vector == ".data_Mujer" ~ "mujer",
    vector == ".data_Hombre" ~ "hombre",
    vector == ".data_Mujeres" ~ "mujer",
    vector == ".data_Varones" ~ "hombre",
    # Nivel educativo (var --> "nivel_educ_obtenido2")
    vector == ".data_Primaria Completa" ~ "primario_completo",
    vector == ".data_Primaria Incompleta" ~ "primario_incompleto",
    vector == ".data_Secundaria Completa" ~ "secundario_completo",
    vector == ".data_Secundaria Incompleta" ~ "secundario_incompleto",
    vector == ".data_Sin instrucción" ~ "sin_instruccion",
    vector == ".data_Terciario Completo" ~ "terciario_completo",
    vector == ".data_Terciario Incompleto" ~ "terciario_incompleto",
    vector == ".data_Universitaria Completa" ~ "universitario_completo",
    vector == ".data_Universitaria Incompleta" ~ "universitario_incompleto",
    # Tipo de empleo (var --> "tipo_empleo")
    vector == ".data_Otro tipo" ~ "otro_tipo_empleo",
    vector == ".data_Privado" ~ "privado",
    vector == ".data_Público" ~ "publico",
    # Condición de formalidad (var --> "condicion_formalidad")
    vector == ".data_Formal" ~ "formal",
    vector == ".data_No corresponde" ~ "no_corresp_formalidad",
    vector == ".data_No formal" ~ "informal",
    # Categoría ocupacional (var --> categoria_ocupacional2")
    vector == ".data_Cuenta Propia" ~ "cuenta_propia",
    vector == ".data_Empleado" ~ "empleado",
    vector == ".data_Familiar" ~ "familiar",
    vector == ".data_Patrón"~ "patron",
    # Rango etareo (var --> "edad_categoria_intervalo")
    vector == ".data_0-9" ~ "edad_no_laborable", # Figura como No, porque no corresponde que se trabaje
    vector == ".data_10-13" ~ "menor",
    vector == ".data_14-18" ~ "joven",
    vector == ".data_19-25" ~ "junior",
    vector == ".data_26-35" ~ "semi_senior",
    vector == ".data_36-45" ~ "senior",
    vector == ".data_46-55" ~ "especialista1",
    vector == ".data_56-65" ~ "especialista2",
    vector == ".data_66+" ~ "jubilado")
}


## FUNCION [sufijos]
# - Se utiliza como insumo de las funciones de cruce de variables
# - Nuclea el nombrado de las matrices de dummies que se generan en los cruces, sobre las variables seleccionadas
# - Las variables seleccionadas son las que figuran en cada subtitulo/nota de la función
# - En caso de necesitar incorporar nuevas variables de cruce, los nombres se incorporan en esta función
# - A diferencia del anterior, incorpora todas las variables de ponderación, es caso de que solo se esté ponderando una variable sin cruzarse con otra

sufijo <- function(vector) {
  case_when(
    # Sexo (var --> "sexo")
    vector == ".data_Mujeres" ~ "_M",
    vector == ".data_Varones" ~ "_H",
    # Nivel educativo (var --> "nivel_educ_obtenido2")
    vector == ".data_Primaria Completa" ~ "_EdPC",
    vector == ".data_Primaria Incompleta" ~ "_EdPI",
    vector == ".data_Secundaria Completa" ~ "_EdSC",
    vector == ".data_Secundaria Incompleta" ~ "_EdSI",
    vector == ".data_Sin instrucción" ~ "_EdNo",
    vector == ".data_Terciario Completo" ~ "_EdTC",
    vector == ".data_Terciario Incompleto" ~ "_EdTI",
    vector == ".data_Universitaria Completa" ~ "_EdUC",
    vector == ".data_Universitaria Incompleta" ~ "_EdUI",
    # Tipo de empleo (var --> "tipo_empleo")
    vector == ".data_Otro tipo" ~ "_EmOtro",
    vector == ".data_Privado" ~ "_EmPriv",
    vector == ".data_Público" ~ "_EmPubl",
    # Condición de formalidad (var --> "condicion_formalidad")
    vector == ".data_Formal" ~ "_CF",
    vector == ".data_No corresponde" ~ "_CNo",
    vector == ".data_No formal" ~ "_CI",
    # Categoría ocupacional (var --> categoria_ocupacional2")
    vector == ".data_Cuenta Propia" ~ "_CtaProp",
    vector == ".data_Empleado" ~ "_Empl",
    vector == ".data_Familiar" ~ "_Fam",
    vector == ".data_Patrón"~ "_Patr",
    # Rango etáreo (var --> "edad_categoria_intervalo")
    vector == ".data_0-9" ~ "_ReNo", # Figura como No, porque no corresponde que se trabaje
    vector == ".data_10-13" ~ "_ReMe",
    vector == ".data_14-18" ~ "_ReJov",
    vector == ".data_19-25" ~ "_ReJu",
    vector == ".data_26-35" ~ "_ReSemi",
    vector == ".data_36-45" ~ "_ReSen",
    vector == ".data_46-55" ~ "_ReEsp1",
    vector == ".data_56-65" ~ "_ReEsp2",
    vector == ".data_66+" ~ "_ReJub",
    # Pondera (var --> todos los distintos tipos de ponderadores)
    vector == "pondera" ~ "",
    vector == "pondih" ~ "",
    vector == "pondioo" ~ "",
    vector == "pondii" ~ "")
}




# CRUCE DE VARIABLES PONDERADAS ----

## FUNCION [cruza_var]
# - Se utiliza para descomponer una variable por sus valores y ponderarla [x]
# - O bien para descomponer dos variables [x] e [y] por sus valores y ponderarla
# NOTA 1 | - x: variable de referencia. La que va a ser cruzada por otra variable o directamente ponderada
#          - y: variable de cruce. 
# NOTA 2 | Utiliza como insumo --> prefijo
#                              --> sufijo
#                              --> assing_pond
#                              --> base [ponderados] 

cruza_var <- function(data, x, y) {
  
  if (missing(y)) {
    # En esta parte se crea un dataframe de dummies para la variable [x] que se toma de referencia. En este caso, la que se va a ponderar
    dummies <- data %>% 
      pull({{x}}) %>% 
      as.character() %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
      replace(. == 0, NA)
    # Toma la variable [x], identifica cúal es su ponderador y lo multiplica por la matriz de dummies
    y <- assign_pond(x)
    y1 <- data %>% pull({{y}})
    ponderas <- dummies * y1 
    # Nombra el 1° termino del nombre de la variable con la variable [x]. Invoca la función prefijo
    nombre_x <- colnames(dummies)
    nombre_x <- prefijo(nombre_x)
    # Nombra el 2° termino del nombre de la variable con la variable [y]. Invoca la función sufijo
    nombre_y <- sufijo(y)
    # Crea el nombre definitivo de la variable
    nombre <- paste0(nombre_x, nombre_y)
    colnames(ponderas) <- nombre           
  }
  
  else {
    # Crea un dataframe de dummies para la variable [x] tomada de referencia
    dummies_x <- data %>% 
      pull({{x}}) %>% 
      as.character() %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
      replace(. == 0, NA)
    # Asigna los nombres de las variables de referencia
    nombre_x <- prefijo(colnames(dummies_x))
    # Crea un dataframe de dummies para la variable [y] de cruce
    dummies_y <- data %>% 
      pull({{y}}) %>% 
      as.character() %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
      replace(. == 0, NA)
    # Asigna los nombres de las variables de cruce
    nombre_y <- sufijo(colnames(dummies_y))
    # Crea un dataframe vacio sobre el que acumula el loop
    dummy <- data.frame(matrix(NA, nrow = nrow(dummies_x), ncol = 0))
    # Arma un loop para cruzar las variables de referencia y nombres 
    for(i in 1:ncol(dummies_y)) {
      aux <- dummies_y %>% pull(i)
      dummies_cross <- dummies_x * aux
      nombre <- paste0(nombre_x, nombre_y[i])
      colnames(dummies_cross) <- nombre
      dummy <- cbind(dummy, dummies_cross)
    }
    # Multiplica por la variable usada de ponderador
    z <- assign_pond(x)
    z1 <- data %>% pull({{z}})
    ponderas <- dummy * z1 
  }
  # Une los resultado renombrados a la matriz original
  cbind(data, ponderas)
}  




## FUNCION [cruza_ingr]
# - Se utiliza para descomponer una variable de ingresos [x] y cruzarla por otra variable [y]
# NOTA 1 | - x: variable de ingreso. La que va a ser cruzada por otra variable o directamente ponderada
#          - y: variable de cruce. 
# NOTA 2 | Utiliza como insumo --> prefijo
#                              --> sufijo


cruza_ingr <- function(data, x, y) {
  # En esta parte se crea una matriz de dummies para la variable de cruce (y), no la de ingreso (x)
  dummies <- data %>% 
    pull({{y}}) %>% 
    as.character() %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE)  %>% 
    replace(. == 0, NA)
  # Toma la variable de ingreso (x) y la multiplica en columnas [por eso el 2 en apply] por la matriz de dummies
  x1 <- datos %>% 
    pull({x}) %>% as.numeric() #ver
  ingr <- dummies * x1
  # Guarda los nombres de las dummies
  nombre_y <- sufijo(colnames(dummies))
  # Crea el nombre de la variable definitiva
  nombre <- paste0(x, nombre_y)
  colnames(ingr) <- nombre
  # Finalmente une los resultado de pondera renombrados a la matriz original
  cbind(data, ingr)
}

