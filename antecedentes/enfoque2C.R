# 1. Función de cruce de variables #####

## Original (no incorpora el pondera) ------

f1 <- function(data, x, y, z = "no") {
  
  if (z == "no") {
    # En esta parte se crea un dataframe de dummies para la variable [x] que se toma de referencia. En este caso, la que se va a ponderar
    dummies <- data %>% 
      pull({{x}}) %>% 
      as.character() %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
      replace(. == 0, NA)
    # Toma la variable [y] que acá es el ponderador y la multiplica por la matriz de dummies
    y1 <- data %>% pull({{y}})
    ponderas <- dummies * y1 
    # Nombra el 1° termino del nombre de la variable con la variable [x]. Invoca la función prefijo
    nombre_x <- colnames(dummies)
    nombre_x <- prefijo(nombre_x)
    # Nombra el 2° termino del nombre de la variable con la variable [y]. Invoca la función sufijo
    cruce <- data %>% select({{y}})
    nombre_y <- sufijo(colnames(cruce))
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
    z1 <- data %>% pull({{z}})
    ponderas <- dummy * z1 
  }
  # Une los resultado renombrados a la matriz original
  cbind(data, ponderas)
}  




## Modificada -----
 
cruza_var <- function(data, x, y) {
  
  if (missing(y)) {
    # En esta parte se crea un dataframe de dummies para la variable [x] que se toma de referencia. En este caso, la que se va a ponderar
    dummies <- data %>% 
      pull({{x}}) %>% 
      as.character() %>%
      fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
      replace(. == 0, NA)
    # Toma la variable [y] que acá es el ponderador y la multiplica por la matriz de dummies
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


## Prueba de cruza_var---------
x <- c("sexo")
y <- c("tipo_empleo")

datos2 <- cruza_var(datos, x) 
datos3 <- cruza_var(datos, x, y) 


## Laburo en seco  de cruza_var ----

x <- c("sexo")


# En esta parte se crea un dataframe de dummies para la variable [x] que se toma de referencia. En este caso, la que se va a ponderar
dummies <- datos %>% 
  pull({{x}}) %>% 
  as.character() %>%
  fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
  replace(. == 0, NA)
# Toma la variable [y] que acá es el ponderador y la multiplica por la matriz de dummies
y <- assign_pond(x)
y1 <- datos %>% pull({{y}})
ponderas <- dummies * y1 
# Nombra el 1° termino del nombre de la variable con la variable [x]. Invoca la función prefijo
nombre_x <- colnames(dummies)
nombre_x <- prefijo(nombre_x)
# Nombra el 2° termino del nombre de la variable con la variable [y]. Invoca la función sufijo
nombre_y <- sufijo(y)
# Crea el nombre definitivo de la variable
nombre <- paste0(nombre_x, nombre_y)
colnames(ponderas) <- nombre  






