# FUNCIONES ####

## Para nombrado ----

### Nombres de prefijos
prefijo <- function(vector) {
  case_when(
    # Sexo
    vector == ".data_Mujeres" ~ "mujer",
    vector == ".data_Varones" ~ "hombre",
    # Nivel educativo
    vector == ".data_Primaria Completa" ~ "primario_completo",
    vector == ".data_Primaria Incompleta" ~ "primario_incompleto",
    vector == ".data_Secundaria Completa" ~ "secundario_completo",
    vector == ".data_Secundaria Incompleta" ~ "secundario_incompleto",
    vector == ".data_Sin instrucción" ~ "sin_instruccion",
    vector == ".data_Terciario Completo" ~ "terciario_completo",
    vector == ".data_Terciario Incompleto" ~ "terciario_incompleto",
    vector == ".data_Universitaria Completa" ~ "universitario_completo",
    vector == ".data_Universitaria Incompleta" ~ "universitario_incompleto",
    # Tipo de empleo
    vector == ".data_Otro tipo" ~ "otro_tipo_empleo",
    vector == ".data_Privado" ~ "privado",
    vector == ".data_Público" ~ "publico",
    # Condición de formalidad
    vector == ".data_Formal" ~ "formal",
    vector == ".data_No corresponde" ~ "no_corresp_formalidad",
    vector == ".data_No formal" ~ "informal",
    # Categoría ocupacional
    vector == ".data_Cuenta Propia" ~ "cuenta_propia",
    vector == ".data_Empleado" ~ "empleado",
    vector == ".data_Familiar" ~ "familiar",
    vector == ".data_Patrón"~ "patron",
    # Rango etareo 
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


### Nombres de sufijos
sufijo <- function(vector) {
  case_when(
    # Sexo
    vector == ".data_Mujeres" ~ "_M",
    vector == ".data_Varones" ~ "_H",
    # Nivel educativo
    vector == ".data_Primaria Completa" ~ "_EdPC",
    vector == ".data_Primaria Incompleta" ~ "_EdPI",
    vector == ".data_Secundaria Completa" ~ "_EdSC",
    vector == ".data_Secundaria Incompleta" ~ "_EdSI",
    vector == ".data_Sin instrucción" ~ "_EdNo",
    vector == ".data_Terciario Completo" ~ "_EdTC",
    vector == ".data_Terciario Incompleto" ~ "_EdTI",
    vector == ".data_Universitaria Completa" ~ "_EdUC",
    vector == ".data_Universitaria Incompleta" ~ "_EdUI",
    # Tipo de empleo
    vector == ".data_Otro tipo" ~ "_EmOtro",
    vector == ".data_Privado" ~ "_EmPriv",
    vector == ".data_Público" ~ "_EmPubl",
    # Condición de formalidad
    vector == ".data_Formal" ~ "_CF",
    vector == ".data_No corresponde" ~ "_CNo",
    vector == ".data_No formal" ~ "_CI",
    # Categoría ocupacional
    vector == ".data_Cuenta Propia" ~ "_CtaProp",
    vector == ".data_Empleado" ~ "_Empl",
    vector == ".data_Familiar" ~ "_Fam",
    vector == ".data_Patrón"~ "_Patr",
    # Rango etáreo
    vector == ".data_0-9" ~ "_ReNo", # Figura como No, porque no corresponde que se trabaje
    vector == ".data_10-13" ~ "_ReMe",
    vector == ".data_14-18" ~ "_ReJov",
    vector == ".data_19-25" ~ "_ReJu",
    vector == ".data_26-35" ~ "_ReSemi",
    vector == ".data_36-45" ~ "_ReSen",
    vector == ".data_46-55" ~ "_ReEsp1",
    vector == ".data_56-65" ~ "_ReEsp2",
    vector == ".data_66+" ~ "_ReJub",
    # Pondera
    vector == "pondera" ~ "")
}


### Para dummies ponderadas
#### NOTA: _x: variable de referencia. La que va a ser cruzada por otra variable o directamente ponderada
####       _y: variable de cruce. Puede ser otra variable o un ponderador
####       _z: ponderador para cuando se cruzan dos variables [x] e [y]. Si se toma solo una variable [x], se deja vacio, o se pone z ="no"

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



### Para ingresos 
f2 <- function(data, x, y) {
  # En esta parte se crea una matriz de dummies para la variable de cruce (y), no la de ingreso (x)
  dummies <- data %>% 
    pull({{y}}) %>% 
    as.character() %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE)  %>% 
    replace(. == 0, NA)
  # Toma la variable de ingreso (x) y la multiplica en columnas [por eso el 2 en apply] por la matriz de dummies
  x1 <- datos %>% 
    pull({x})
  ingr <- dummies * x1
  # Guarda los nombres de las dummies
  nombre_y <- sufijo(colnames(dummies))
  # Crea el nombre de la variable definitiva
  nombre <- paste0(x, nombre_y)
  colnames(ingr) <- nombre
  # Finalmente une los resultado de pondera renombrados a la matriz original
  cbind(data, ingr)
}


# CONJUNTOS DE VARIABLES ####

## De Dummies ponderadas ----
dum <- c("sexo", "nivel_educ_obtenido2", "edad_categoria_intervalo", "tipo_empleo", "condicion_formalidad", "categoria_ocupacional2")
dum1 <- dum[2:length(dum)]
dum2 <- dum1[2:length(dum1)]
dum3 <- dum2[2:length(dum2)]
dum4 <- dum3[2:length(dum3)]
dum5 <- dum4[2:length(dum4)]

## De Ingresos ----
###  Variables a ser cruzadas
ingr1 <- c("ingreso_ocupacion_principal", "ingreso_real_ocupacion_principal", 
           "ingreso_otras_ocupaciones", "ingreso_real_otras_ocupaciones",
           "ingreso_total_individual", "ingreso_real_total_individual", 
           "ingreso_no_laborable", "ingreso_real_no_laborable")
ingr2 <- ingr1[1:6]

###  Variables de cruce
ingr1_c <- dum1[1:2]
ingr2_c <- dum1[3:5]



# PROCESADO ####

## Loop para dummies puras ponderadas ---- 
for (x in dum) { datos <- f1(datos, x, pondera)} 


## Loops para dummies cruzadas ----
for (y in dum1) {datos <- f1(datos, sexo, y, z = "pondera")}
for (y in dum2) {datos <- f1(datos, nivel_educ_obtenido2, y, z = "pondera")}
for (y in dum3) {datos <- f1(datos, edad_categoria_intervalo, y, z = "pondera")}
for (y in dum4) {datos <- f1(datos, tipo_empleo, y, z = "pondera")}
for (y in dum5) {datos <- f1(datos, condicion_formalidad, y, z = "pondera")} 
 


## Loop para todos los ingresos seleccionados ---- 
for (x in ingr1) {
  for (y in ingr1_c) {
    datos <- f2(datos, x, y)
  }
} 


## Loop para ingresos individuales ----
for (x in ingr2) {
  for (y in ingr2_c) {
    datos <- f2(datos, x, y)
  }
}  


# LIMPIEZA ####
rm(dum, dum1, dum2, dum3, dum4, dum5, ingr1, ingr1_c, ingr2, ingr2_c, x, y, f1, f2, prefijo, sufijo)
