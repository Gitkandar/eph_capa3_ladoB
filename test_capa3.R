
# Armado de variables de ingresos sobre las que se van a calcular los promedios ####


## Variables dummies cruzadas y puras ----
## Se usan los puntos de corte en la generación de variables de [capa2_plus]
a <- corte$ini_dummies_puras
b <- corte$fin_dummies_cruzadas
## Se extren los nombres
dum <- datos %>% 
  select(c(a:b)) %>% 
  names()
rm(a, b)


## Variables de ingresos ----
## Son los ingresos sobre los que se decidió NO realizar seguimiento 
ingr_no <- c("ingreso_indemnizacion", "ingreso_real_indemnizacion",
             "ingreso_seguro_desempleo", "ingreso_real_seguro_desempleo", 
             "ingreso_subsidio", "ingreso_real_subsidio",
             "ingreso_alquiler", "ingreso_real_alquiler",
             "ingreso_beca", "ingreso_real_beca",
             "ingreso_cuota_alimentaria", "ingreso_real_cuota_alimentaria", 
             "ingreso_aguinaldo", "ingreso_real_aguinaldo",
             "ingreso_familiar", "ingreso_real_familiar") 
## Ingresos sobre los que se van a calcular los promedios ponderados 
ingr <- datos %>% 
        select(starts_with("ingreso_"), -all_of(ingr_no)) %>%
        names()
rm(ingr_no)


## Vector de variables ----
## Se crea el vector con el nombre de las variables sobre el que va a recorrer el bucle
loop <- c(dum, ingr)
rm(dum, ingr)



# Dataframe [capa3] ####
## Se crea el dataframe [capa3] con los datos correspondientes al aglomerado y al período en consideración
capa3 <- datos %>%
  group_by(aglomerado) %>% 
  select(aglomerado, anio, trimestre, region, mas_500) %>% 
  mutate(aglomerado = as.character(aglomerado))
## Se toma cualquier observación (total repite) para crear la observación "Argentina" 
capa3[1, 1] <- "Argentina"
capa3[1, 4:5] <- NA
## Se termina de unir todo
capa3 <- capa3 %>% 
  distinct() %>% 
  mutate(aglomerado = as.factor(aglomerado)) %>% 
  arrange(region)
  




# Function ####

aglom_simple <- function (data, x) {
  
  # Toma la variable [x] e identifica cual es su ponderador
  y <- assign_pond(x)

  # Cálculo de promedios ponderados para las variables de ingreso
  if (isTRUE(startsWith(x, "ingreso"))) {
    
    test <- c("ingreso_familiar", "ingreso_real_familiar")
    
    # Para ingresos familiares
    if(x %in% test) {
      
      # Prepara las variables para calcular el promedio ponderado y quita los duplicados por hogar
      aux <- data %>% 
        select(aglomerado, id_hogar, x1 = all_of(x), y1 = all_of(y)) %>% 
        mutate(aux1 = x1 * y1,
               aux1 = ifelse(is.na(aux1), 0, aux1),
               aux2 = ifelse(is.na(y1), 0, y1),
               aux2 = ifelse(aux1 == 0, 0, aux2)) %>% 
        filter(!duplicated(id_hogar))
      # Calcula el promedio para la totalidad de Argentina
      aux1 <- aux %>% 
        summarise(aglomerado  = "Argentina",
                  z = sum(aux1) / sum(aux2)) 
      colnames(aux1)[2] <- x
      # Calcula los promedios por aglomerado
      aux2 <- aux %>% 
        group_by(aglomerado) %>% 
        summarise(z = sum(aux1) / sum(aux2)) 
      colnames(aux2)[2] <- x
      # Une Argentina con Aglomerados
      aux <- union(aux1, aux2)
    
    }
    
    # Para el resto de los ingresos
    else {
    
    # Prepara las variables para calcular el promedio ponderado
    aux <- data %>% 
      select(aglomerado, x1 = all_of(x), y1 = all_of(y)) %>% 
      mutate(aux1 = x1 * y1,
             aux1 = ifelse(is.na(aux1), 0, aux1),
             aux2 = ifelse(is.na(y1), 0, y1),
             aux2 = ifelse(aux1 == 0, 0, aux2))
    # Calcula el promedio para la totalidad de Argentina
    aux1 <- aux %>% 
      summarise(aglomerado  = "Argentina",
                z = sum(aux1) / sum(aux2)) 
    colnames(aux1)[2] <- x
    # Calcula los promedios por aglomerado
    aux2 <- aux %>% 
      group_by(aglomerado) %>% 
      summarise(z = sum(aux1) / sum(aux2)) 
    colnames(aux2)[2] <- x
    # Une Argentina con Aglomerados
    aux <- union(aux1, aux2)
  
      }
  }
  
  # Cálculo de sumas simples en variables dummies ponderadas y ponderadas cruzadas
  
  else {
    # Calcula la sumatoria por aglomerado
    aux1 <- data %>%
      group_by(aglomerado) %>% 
      summarise(
        across(
          .cols = all_of(x),
          .fns = ~ sum(.x, na.rm = TRUE),
          .names = "{col}"))
    # Calcula la sumatoria para Argentina
    aux2 <- data %>%
      summarise(
        across(
          .cols = all_of(x),
          .fns = ~ sum(.x, na.rm = TRUE),
          .names = "{col}")) %>% 
      mutate(aglomerado = "Argentina")
    # Une Argentina con Aglomerados
    aux <- union(aux1, aux2)
    
  }
  # Finalmente une los resultado de pondera renombrados a la matriz original
  inner_join(capa3, aux, by = "aglomerado")

}



# Loop para variables ####
for (i in loop) {capa3 <- aglom_simple(datos, i)} 






# Variables excluibles ####
# Se toma como punto de referencia que los valores en los que el distrito Argentina == 0



## Dataframe vacio ----
capa3_excl <- capa3[1:5]



## Función de exclusión ----
for (i in loop) {
  
  if(capa3[33, i] == 0) {
    aux <- capa3[i]
    capa3_excl <- cbind(capa3_excl, aux)
  }
  else {next}
}


nombres <- names(capa3_excl)



## NOTA: ver si están bien deflacionados todos los ingresos reales



