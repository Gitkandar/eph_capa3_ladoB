# RESUMEN ----





### NOTA    

## INDIVIDUOS ----

# Ingresos ----
ingresos <- datos %>%
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
      .names = "{col}_{fn}"),
    
    #Ingresos pondih
    across(
      .cols = c(ingreso_capita_familiar, ingreso_real_capita_familiar),
      .fns = list(
        m = ~weighted.mean(., pondih, na.rm = TRUE),
        Md = ~median(rep(., times = pondih), na.rm = TRUE)
      ),
      .names = "{col}_{fn}")) %>%
  select(aglomerado, ends_with("_m"), ends_with("_Md"))

# Cruza de ingresos ----
#usamos summarise_at porque pisamos variables que ya existen
cruces_pondera <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(
      starts_with("ingreso_no_laborable_"),
      starts_with("ingreso_real_no_laborable_")
    ),
    .funs = ~weighted.mean(., w = pondera, na.rm = TRUE)
  )

cruces_pondii <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(
      starts_with("ingreso_total_individual_"),
      starts_with("ingreso_real_total_individual_")
    ),
    .funs = ~weighted.mean(., w = pondii, na.rm = TRUE)
  )

cruces_pondiio <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(starts_with("ingreso_ocupacion_principal_"),
                 starts_with("ingreso_otras_ocupaciones_"),
                 starts_with("ingreso_real_ocupacion_principal_"),
                 starts_with("ingreso_real_otras_ocupaciones_")),
    .funs = ~weighted.mean(., w = pondiio, na.rm = TRUE)
  )

# Sexo ----
sexo1 <- datos %>%
  group_by(aglomerado) %>% 
  summarise(
    mujer = sum(mujer, na.rm = T),
    mujer_Part = sum(mujer, na.rm = T)/sum(pondera),
    hombre = sum(hombre, na.rm = T),
    hombre_Part = sum(hombre, na.rm = T)/sum(pondera)
  )

sexo2 <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(starts_with("hombre_"), starts_with("mujer_")),
    .funs = ~sum(., na.rm = TRUE)
  )

# Formalidad ----
form1 <- datos %>%
  group_by(aglomerado) %>% 
  summarise(
    formal = sum(Formal, na.rm = T),
    informal = sum(NoFormal, na.rm = T),
    formal_Part = sum(Formal, na.rm = T)/sum(pondera),
    informal_Part = sum(NoFormal, na.rm = T)/sum(pondera),
  )

form2 <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(starts_with("formal_"), starts_with("informal_")),
    .funs = ~sum(., na.rm = TRUE)
  )

# Empleo ----
empleo1 <- datos %>%
  group_by(aglomerado) %>% 
  summarise(
    publico = sum(Publico, na.rm = T),
    privado = sum(Privado, na.rm = T),
    otro_tipo_empleo = sum(OtroTipo, na.rm = T), 
    publico_Part = sum(Publico, na.rm = T)/sum(pondera),
    privado_Part = sum(Privado, na.rm = T)/sum(pondera),
    otro_tipo_empleo_Part = sum(OtroTipo, na.rm = T)/sum(pondera), 
  )

empleo2 <- datos %>%
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
educ1 <- datos %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(ends_with("Completo"), SinInstruccion),
    .funs = ~sum(., na.rm = TRUE)
  )

educ2 <- datos %>%
  group_by(aglomerado) %>%
  summarise(
    across(
      .cols = c(ends_with("Completo"), SinInstruccion), 
      .fns = list(Part = ~sum(., na.rm = TRUE)/sum(pondera)),
      .names = "{col}_{fn}"
    )
  )

educ3 <- datos %>%
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
catoc1 <- datos %>%
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

catoc2 <- datos %>%
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
      .cols = c(ingreso_familiar, ingreso_real_familiar), 
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
  salida <- capa3} else {
    salida <- rbind(salida, capa3)
    }
setwd(ruta_results)
save(datos, file = file_name)
save(capa3, file = exit_name)