# FUNCIONES ####

## Para nombrado ----

### Dummies ponderadas ----

fa <- function(vector) {
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


### De ingresos ----
fb <- function(vector) {
  case_when(
    # Sexo
    vector == ".data_Mujeres" ~ "_M",
    vector == ".data_Varones" ~ "_H",
    vector == "hombre" ~ "_H",
    vector == "mujer" ~ "_M",
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
    vector == "primario_completo" ~ "_EdPC",
    vector == "primario_incompleto" ~ "_EdPI",
    vector == "secundario_completo" ~ "_EdSC",
    vector == "secundario_incompleto" ~ "_EdSI",
    vector == "sin_instruccion" ~ "_EdNo",
    vector == "terciario_completo" ~ "_EdTC",
    vector == "terciario_incompleto" ~ "_EdTI",
    vector == "universitario_completo" ~ "_EdUC",
    vector == "universitario_incompleto" ~ "_EdUI",
    # Tipo de empleo
    vector == ".data_Otro tipo" ~ "_EmOtro",
    vector == ".data_Privado" ~ "_EmPriv",
    vector == ".data_Público" ~ "_EmPubl",
    vector == "otro_tipo_empleo" ~ "_EmOtro",
    vector == "privado" ~ "_EmPriv",
    vector == "publico" ~ "_EmPubl",
    # Condición de formalidad
    vector == ".data_Formal" ~ "_CF",
    vector == ".data_No corresponde" ~ "_CNo",
    vector == ".data_No formal" ~ "_CI",
    vector == "formal" ~ "_CF",
    vector == "no_corresp_formalidad" ~ "_CNo",
    vector == "informal" ~ "_CI",
    # Categoría ocupacional
    vector == ".data_Cuenta Propia" ~ "_CtaProp",
    vector == ".data_Empleado" ~ "_Empl",
    vector == ".data_Familiar" ~ "_Fam",
    vector == ".data_Patrón"~ "_Patr",
    vector == "cuenta_propia" ~ "_CtaProp",
    vector == "empleado" ~ "_Empl",
    vector == "familiar" ~ "_Fam",
    vector == "patron" ~ "_Patr",
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
    vector == "edad_no_laborable" ~ "_ReNo", # Figura como No, porque no corresponde que se trabaje
    vector == "menor" ~ "_ReMe",
    vector == "joven" ~ "_ReJov",
    vector == "junior" ~ "_ReJu",
    vector == "semi_senior" ~ "_ReSemi",
    vector == "senior" ~ "_ReSen",
    vector == "especialista1" ~ "_ReEsp1",
    vector == "especialista2" ~ "_ReEsp2",
    vector == "jubilado" ~ "_ReJub",
    # Pondera
    vector == "pondera" ~ "")
}


## Para procesado ----

### Para dummies ponderadas ---- 
f1 <- function(data, x, y) {
  # En esta parte se crea una matriz de dummies para la variable x seleccionada
  dummies <- data %>% 
    pull({{x}}) %>% 
    as.character() %>%
    fastDummies::dummy_cols(remove_selected_columns = TRUE, ignore_na = TRUE) %>% 
    replace(. == 0, NA)
  # Toma la variable que juega de ponderador y la multiplica por la matriz de dummies
  y1 <- data %>% 
    pull({{y}})
  ponderas <- dummies * y1 
  # Codifica el primer termino del nombre de la variable con la variable [x]. Invoca la función fa
  nombre_x <- colnames(dummies)
  nombre_x <- fa(nombre_x)
  # Codifica el segundo termino del nombre de la variable con la variable [y]. Invoca la función fb
  cruce <- data %>%
    select({{y}})
  nombre_y <- fb(colnames(cruce))
  # Crea el nombre definitivo de la variable
  nombre <- paste0(nombre_x, nombre_y)
  colnames(ponderas) <- nombre
  # Une los resultado renombrados a la matriz original
  bind_cols(data, as.data.frame(ponderas))
}


### Para ingresos ----
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
  nombre_y <- fb(colnames(dummies))
  # Crea el nombre de la variable definitiva
  nombre <- paste0(x, nombre_y)
  colnames(ingr) <- nombre
  # Finalmente une los resultado de pondera renombrados a la matriz original
  bind_cols(data, as.data.frame(ingr))
}




# CONJUNTOS DE VARIABLES ####

## De Dummies ponderadas ----

dum0 <- c("sexo", "nivel_educ_obtenido2", "edad_categoria_intervalo", "tipo_empleo", "condicion_formalidad", "categoria_ocupacional2")
dum1 <- c("primario_completo", "primario_incompleto", "secundario_completo", "secundario_incompleto",
          "sin_instruccion", "terciario_completo", "terciario_incompleto", "universitario_completo", "universitario_incompleto",
          "edad_no_laborable", "menor", "joven", "junior", "semi_senior", 
          "senior", "especialista1", "especialista2", "jubilado",
          "otro_tipo_empleo", "privado", "publico",
          "formal", "no_corresp_formalidad", "informal",
          "cuenta_propia", "empleado", "familiar", "patron")
dum2 <- dum1[10:28]
dum3 <- dum2[10:19]
dum4 <- dum3[4:10]
dum5 <- dum4[4:7]


## De Ingresos ----

ingr1 <- c("ingreso_ocupacion_principal", "ingreso_real_ocupacion_principal", "ingreso_otras_ocupaciones", "ingreso_real_otras_ocupaciones",
           "ingreso_total_individual", "ingreso_real_total_individual", "ingreso_no_laborable", "ingreso_real_no_laborable")
ingr2 <- ingr1[1:6]
ingr1_c <- c("sexo", "nivel_educ_obtenido2")
ingr2_c <- c("condicion_formalidad", "tipo_empleo", "edad_categoria_intervalo")


# PROCESADO ####

## Loop para dummies puras ponderadas ---- 
for (x in dum0) {
  datos <- f1(datos, x, pondera)
} 


## Loops para dummies cruzadas ----
for (x in dum1) { datos <- f1(datos, sexo, x) } 
for (x in dum2) { datos <- f1(datos, nivel_educ_obtenido2, x) } 
for (x in dum3) { datos <- f1(datos, edad_categoria_intervalo, x) } 
for (x in dum4) { datos <- f1(datos, tipo_empleo, x) }
for (x in dum5) { datos <- f1(datos, condicion_formalidad, x) }


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
 







