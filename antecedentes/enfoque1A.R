# NOTA #########
 
# Este archivo corresponde solo a la parte de cálculo de medias y promedios de ingresos ponderados que figuran en enfoque1,
# a partir de <<RESUMEN>>





# INGRESOS ####

## NOTA: estos son los ingresos sobre los que se cruzó información en capa2_plus
ingr1 <- c("ingreso_ocupacion_principal", "ingreso_real_ocupacion_principal", 
           "ingreso_otras_ocupaciones", "ingreso_real_otras_ocupaciones",
           "ingreso_total_individual", "ingreso_real_total_individual", 
           "ingreso_no_laborable", "ingreso_real_no_laborable") 


#usamos across porque se generan nuevas variables
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


#### CRUZA DE INGRESOS ####

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

#### SEXO ####

sexo1 <- capa3i %>%
  group_by(aglomerado) %>% 
  summarise(
    mujer = sum(Mujeres, na.rm = T),
    mujer_Part = sum(Mujeres, na.rm = T)/sum(pondera),
    hombre = sum(Varones, na.rm = T),
    hombre_Part = sum(Varones, na.rm = T)/sum(pondera)
  )

sexo2 <- capa3i %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(starts_with("hombre_"), starts_with("mujer_")),
    .funs = ~sum(., na.rm = TRUE)
  )

#### FORMALIDAD ####

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

#### EMPLEO ####

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


#### NIVEL EDUC ####

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

#### CATEG OCUP ####

catoc1 <- capa3i %>%
  group_by(aglomerado) %>% 
  summarise(
    Patron = sum(Patron, na.rm = T),
    CuentaPropia = sum(CuentaPropia, na.rm = T),
    Empleado = sum(Empleado, na.rm = T),
    Familiar = sum(Familiar, na.rm = T),
    Patron_Part = sum(Patron, na.rm = T)/sum(pondera),
    CuentaPropia_Part = sum(CuentaPropia, na.rm = T)/sum(pondera),
    Empleado_Part = sum(Empleado, na.rm = T)/sum(pondera),
    Familiar_Part = sum(Familiar, na.rm = T)/sum(pondera)
  )

catoc2 <- capa3i %>%
  group_by(aglomerado) %>% 
  summarise_at(
    .vars = vars(
      starts_with("Patron_"),
      starts_with("CuentaPropia_"),
      starts_with("Empleado_"),
      starts_with("Familiar_")
    ),
    .funs = ~sum(., na.rm = TRUE)
  )

#### HOGARES ####
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

#### UNION ####

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
    periodo = paste0("20", i, "_T", j),
    aglomerado = as.character(aglomerado)
  )
