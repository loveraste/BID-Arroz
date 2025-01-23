#-------------------------------------------------------#
# Arroz ----
# Procesamiento de datos Censo Nacional Agropecuario
#-------------------------------------------------------#

#--------------------------#
# packages ----
#--------------------------#

rm(list=ls())
pacman::p_load(tidyverse, glue, readr, readxl, janitor, writexl)

#--------------------------#
# paths ----
#--------------------------#

datos_ori <- "01_Datos_originales/CNA/Total_nacional(csv)"
datos <- "02_Datos/CNA"
graficas <- "03_Graficas"
options(scipen = 999)

#-------------------------------------------------------#
# 1. Procesar datos originales ----
#-------------------------------------------------------#

#--------------------------#
# A. Limpiar modulos ----
#--------------------------#

# Elegimos variables de interes por modulo y guardamos
# General (UPAs)
cna_gen <- read_csv(glue("{datos_ori}/S01_15(Unidad_productora).csv")) %>%
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, P_S5PAUTOS, PRED_ETNICA,
    # Capital
    P_S7P84F, P_S10P121,
    # Trabajo permanente y jornal
    P_S11P138, P_S11P138A, P_S11P138B, P_S11P140, 
    # Trabajo familiar
    P_S11P139, P_S11P139A, P_S11P139B,
    # Trabajo colectivo
    P_S11P141,
    # Tierra
    S05_TENENCIA, P_S12P142, P_S12P148, P_S12P150A,
    # Tierra pecuaria (pastos y forrajes)
    P_S6P66, P_S6P68,
    # Produccion y destino produccion
    P_S7P85B, starts_with("P_S6P61_"), 
    # Asistencia tecnica
    P_S11P135, starts_with("P_S11P135A_SP"),
    # Asociatividad (cooperativas, gremios)
    starts_with("P_S11P134"),
    # No agropecuarias
    TIPO_UC, P_S12P148, P_S12P149,
    # Maquinaria
    P_S9P117,
    # Agricolas
    P_S6P43, P_S6P44, P_S3P9, P_S3P10, P_S6P62, P_S6P65, P_S6P67, P_S6P72, 
    # Ganado bovino
    P_S7P78, P_S7P82, P_S7P83A, P_S7P84A,
    # Porcinos en confinamiento
    P_S7P86, P_S7P89A,P_S7P89B, P_S7P89D,
    # Avicultura
    P_S7P90, P_S7P92A, P_S7P92B, P_S7P93A, P_S7P93B,
    # Acuicultura
    P_S7P94,
    # Bufalos, equinos, ovinos y caprinos
    P_S7P101,P_S7P102A, P_S7P102B, P_S7P102C, P_S7P102D,
    P_S7P102E, P_S7P102F,P_S7P102G, P_S7P102H, P_S7P102I,
    P_S7P102J, P_S7P102K, P_S7P102L,
    # Otros animales
    P_S7P105,
    #Financiamiento
    P_S11P136, P_S11P136A,
    #acceso y uso del agua
    starts_with("P_S11P126_SP"),
    #sistemas de riego
    P_S6P52_SP99, P_S6P52_SPA02, P_S6P52_SPA04, P_S6P52_SPC20,
    P_S6P52_SPC22, P_S6P52_SPC21,
    #Fertilizante
    starts_with("P_S6P76_SP"),
    #MIPE
    starts_with("P_S6P77_SP"),
    # Toma de decisiones
    P_S4P16, SNH, SNM, SN9, P_S4P21
    ) %>%
  clean_names()

saveRDS(cna_gen, glue("{datos}/base_upas.rds"))

# Cultivos
cna_cul <- read_csv(glue("{datos_ori}/S06A(Cultivos).csv")) %>%
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
    # Cultivos
    P_S6P46, AREA_SEMBRADA, AREA_COSECHADA, P_S6P57A, P_S6P59_UNIF, P_S6P50
    ) %>% clean_names()

saveRDS(cna_cul, glue("{datos}/base_cultivos.rds"))

# Construcciones agropecuarias
# cna_cons <- read_csv(glue("{datos_ori}/S10(Construcciones_uso_agropecuario).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Construcciones
#     P_S10P122, P_S10P123
#   ) %>% clean_names()
# 
# saveRDS(cna_cons, glue("{datos}/CNA/base_construcciones_agro.rds"))

# Maquinaria agropecuaria
cna_maq <- read_csv(glue("{datos_ori}/S09(Maquinaria_uso_agropecuario).csv")) %>%
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, UC_UO,
    # Capital
    P_S9P118, P_S9P119, P_S9P120,
    ) %>% clean_names()

saveRDS(cna_maq, glue("{datos}/base_maquinaria_agro.rds"))

# Pesca
cna_pesca <- read_csv(glue("{datos_ori}/S08(Pesca_artesanal).csv")) %>%
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
    # Pesca de agua dulce y salada
    everything()
  ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
saveRDS(cna_pesca, glue("{datos}/base_pesca_artesanal.rds"))

# Acuicultura
cna_acui <- read_csv(glue("{datos_ori}/S07D(Acuicultura).csv")) %>%
  dplyr::select(
    # Identificadoras
    TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
    # Acuicultura
    everything()
  ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
saveRDS(cna_acui, glue("{datos}/base_acuicultura.rds"))

# Dispersos y viveros (esta base esta a nivel de cultivo y menciona cada uno de los cultivos dispersos en la finca)
# cna_dis <- read_csv(glue("{datos_ori}/S06BD(Frutales_y_forestales_dispersos,_y_viveros).csv")) %>%
# dplyr::select(
#   # Identificadoras
#   TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#   # Cultivos dispersos y viveros
#   everything()
# ) %>% clean_names() %>% dplyr::select(-c(pais, uc_uo))
# saveRDS(cna_dis, glue("{datos}/CNA/base_frutales_dispersos.rds"))

# No agropecuaria
# cna_noagro <- read_csv(glue("{datos_ori}/S14(Actividad_no_agropecuaria).csv")) %>%
#   dplyr::select(
#     # Identificadoras
#     TIPO_REG, P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA,
#     # Act no agro
#     P_S14P157_SP1
#     ) %>% clean_names()
# 
# saveRDS(cna_noagro, glue("{datos}/CNA/base_no_agropecuarias.rds"))

# Personas (productores)
cna_per <- read_csv(glue("{datos_ori}/S15P(Personas).csv")) %>%
  dplyr::select(P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, ID_PROD,
                # Identificadores del hogar
                P_S15P165A, P_S15P165B, P_S15P165C,
                # Parentesco con jefe hogar
                P_S15P167,
                # Sexo, edad
                P_S15P168, P_S15P169,
                # Nivel educativo y anios educacion
                P_S15P175A, P_S15P175C,
                # Autorreconocimiento
                P_S15P170, P_S15P173
                ) %>%
  clean_names()

saveRDS(cna_per, glue("{datos}/base_personas.rds"))

# Hogares
cna_hog <- read_csv(glue("{datos_ori}/S15H(Hogares).csv")) %>%
  dplyr::select(P_DEPTO, P_MUNIC, ENCUESTA, COD_VEREDA, ITER_HG, P_S15P165A, P_S15P165B,
                # Pobreza, conflicto
                P_S15P177, P_S15P178, starts_with("P_S15P179_S"), P_S15P180
  ) %>%
  clean_names()

saveRDS(cna_hog, glue("{datos}/base_hogares.rds"))

#-------------------------------------------------------#
# 2. Crear base CNA ----
#-------------------------------------------------------#

#--------------------------#
# A. Unir modulos ----
#--------------------------#

### Modulos del productor (a nivel de integrantes del hogar)
cna_per <- readRDS(glue("{datos}/base_personas.rds")) %>%
  # dplyr::select(p_depto, p_munic, encuesta, cod_vereda, id_prod, p_s15p168, p_s15p167) %>%
  distinct() %>%
  rename(sexo = p_s15p168, parentesco_jefe = p_s15p167, edad = p_s15p169,
         nivel_educativo = p_s15p175a, nivel_educativo_anio_aprob = p_s15p175c,
         autorreconocimiento_etnico = p_s15p170, sabe_leer_esc = p_s15p173,
         orden_vivienda = p_s15p165a, orden_persona = p_s15p165c, orden_hogar = p_s15p165b)

# Sexo del productor principal (se asume que es jefe de hogar)
jefe <- cna_per %>%
  dplyr::select(p_depto, p_munic, encuesta, cod_vereda, sexo, parentesco_jefe) %>%
  distinct() %>%
  dplyr::filter(parentesco_jefe == 1) %>%
  rename(cod_dpto = p_depto, cod_mpio = p_munic) %>%
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), 
         cod_vereda = as.numeric(cod_vereda))

saveRDS(jefe, glue("{datos}/sexo_jefe_hogar_upa.rds"))

### Modulo de hogar
cna_hog <- readRDS(glue("{datos}/base_hogares.rds")) %>%
  distinct() %>%
  rename(id_hogar = iter_hg, orden_vivienda = p_s15p165a, orden_hogar = p_s15p165b,
         considera_pobre = p_s15p177, nivel_vida = p_s15p178, 
         es_desplazado = p_s15p179_sp1, tuvo_despojo = p_s15p179_sp2,
         tuvo_abandono = p_s15p179_sp3, no_conflicto = p_s15p179_sp4,
         retorno_predio = p_s15p180)

# Unimos hogares y personas: match perfecto
cna_hog_per <- left_join(cna_per, cna_hog, by = c('p_depto', 'p_munic', 'encuesta', 'cod_vereda', 'orden_vivienda', 'orden_hogar')) %>%
  dplyr::select(p_depto, p_munic, encuesta, cod_vereda, starts_with("orden"), id_hogar, id_prod, 
                considera_pobre, nivel_vida, es_desplazado, tuvo_despojo, tuvo_abandono, no_conflicto,
                retorno_predio, everything())

saveRDS(cna_hog_per, glue("{datos}/base_hogares_personas.rds"))

# Solo guardamos ID_PROD para identificar residencia en base personas
cna_per <- cna_per %>% dplyr::select(p_depto, p_munic, encuesta, cod_vereda, id_prod) %>% distinct()

# Abrimos modulo general (UPAs) y pegamos los demas
cna_gen <- readRDS(glue("{datos}/base_upas.rds")) 
cna_cul <- readRDS(glue("{datos}/base_cultivos.rds")) 
cna_maq <- readRDS(glue("{datos}/base_maquinaria_agro.rds")) 
cna_pesca <- readRDS(glue("{datos}/base_pesca_artesanal.rds"))
cna_acui <- readRDS(glue("{datos}/base_acuicultura.rds"))

# cna_cons <- readRDS(glue("{datos}/base_construcciones_agro.rds")) 
# cna_dis <- readRDS(glue("{datos}/base_frutales_dispersos.rds"))

# Organizamos nombres variables
cna <- cna_gen %>%
  left_join(cna_cul, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_maq, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_pesca, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_acui, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>%
  left_join(cna_per, by = c("p_depto", "p_munic", "encuesta", "cod_vereda")) %>% 
  dplyr::select(-c(starts_with("tipo_reg"), p_s12p150a))

# Guardamos los modulos puros antes de limpiar nombres de variables y organizar en long
saveRDS(cna, glue("{datos}/base_cna_modulos.rds"))

#--------------------------#
# B. Organizar variables ----
#--------------------------#

# Abrir censo completo con nombres originales y renombramos variables para estandarizar
cna_clean <- readRDS(glue("{datos}/base_cna_modulos.rds")) %>%
  rename(cod_dpto = p_depto, cod_mpio = p_munic,
         # Agricola 
         tipo_cul_lote = p_s6p46, cant_cosecha = p_s6p57a, 
         rend_cosecha = p_s6p59_unif, tiene_cultivos = p_s6p43, tiene_plantaciones = p_s6p44, 
         siembra_consumo = p_s3p9, siembra_venta = p_s3p10, tiene_cul_dispersos = p_s6p62, 
         tiene_pastos_nat = p_s6p65, tiene_pastos_sem = p_s6p67, tiene_vivero = p_s6p72,
         # Tenencia tierra
         tenencia_tierra = s05_tenencia,
         # Construccion agropecuaria
         # tipo_cons_agropecuario = p_s10p122,
         # Ganaderia
         tiene_bov_12 = p_s7p78, tiene_bov_hoy = p_s7p82, num_bov_macho = p_s7p83a, 
         num_bov_hembra = p_s7p84a, hembras_bov_ord = p_s7p84f, hay_cons_pec = p_s10p121, 
         cant_leche = p_s7p85b,
         # Porcinos en confinamiento
         tiene_cerdos = p_s7p86, num_cer_macho_rep = p_s7p89a, num_cer_hembra = p_s7p89b,
         num_cer_des = p_s7p89d,
         # Avicultura
         criado_gall_poll = p_s7p90, num_av_e_piso = p_s7p92a, num_av_s_piso = p_s7p92b,
         num_av_e_jaula = p_s7p93a, num_av_s_jaula = p_s7p93b,
         # Acuicultura
         tiene_cria_pez = p_s7p94, nom_especie_pez = p_s7p96, num_cos_pez = p_s7p97, cant_cosecha_pez = p_s7p98, 
         peso_prom_pez = p_s7p99, prod_total_pez = p_s7p100,
         tipo_pez_dulce = p_s8p1141, especie_pez_dul = p_s8_numpezdul, cant_pez_dul_1 = p_s8p1151, cant_pez_dul_2 = p_s8p1161,
         tipo_pez_sal = p_s8p1142, especie_pez_sal = p_s8_numpezsal, cant_pez_sal_1 = p_s8p1152, cant_pez_sal_2 = p_s8p1162,
         u_med_pez_dulce_1 = p_s8p115a1, u_med_pez_dulce_2 = p_s8p116a1, u_med_pez_sal_1 = p_s8p115a2, u_med_pez_sal_2 = p_s8p116a2,
         # Bufalos, equinos, ovinos y caprinos
         tiene_b_e_o_c = p_s7p101, num_buf_macho = p_s7p102a, num_buf_hembra = p_s7p102b, 
         num_caballos = p_s7p102c, num_yeguas = p_s7p102d, 
         num_mula_macho = p_s7p102e, num_mula_hembra = p_s7p102f, num_burro_macho = p_s7p102g, 
         num_burro_hembra = p_s7p102h, num_cabra_macho = p_s7p102i, num_cabra_hembra = p_s7p102j,
         num_oveja_hembra = p_s7p102k, num_oveja_macho = p_s7p102l,
         # Otros animales
         tiene_otras_esps = p_s7p105,
         # Areas
         area_inf_noagro = p_s12p148, area_otros_usos = p_s12p149, area_agropecuario = p_s12p142, 
         area_pasto_nat = p_s6p66, area_pasto_sem = p_s6p68, area_upa = p_s5pautos,
         # Trabajo
         total_trab = p_s11p138, total_trab_h = p_s11p138a, total_trab_m = p_s11p138b, total_jorn = p_s11p140,
         total_trab_hogar = p_s11p139, total_trab_hogar_h = p_s11p139a, total_trab_hogar_m = p_s11p139b,
         tiene_trabajo_colectivo = p_s11p141,
         # Maquinaria
         existe_maq = p_s9p117, tipo_maq = p_s9p118, maq_menor5 = p_s9p119, maq_mayor5 = p_s9p120,
         # Asistencia tecnica
         tiene_asis_tec = p_s11p135, asis_tec_buen_prac_agri = p_s11p135a_sp1, 
         asis_tec_buen_prac_pec = p_s11p135a_sp2,
         asis_tec_prac_manej_ambie = p_s11p135a_sp3, asis_tec_manej_suel = p_s11p135a_sp4, 
         asis_tec_manej_posco = p_s11p135a_sp5,
         asis_tec_comerc = p_s11p135a_sp6, asis_tec_asocia = p_s11p135a_sp7, 
         asis_tec_cred_finan = p_s11p135a_sp8, asis_tec_gest_empre = p_s11p135a_sp9,
         asis_tec_cono_tradi = p_s11p135a_sp10,
         # Financiamiento
         soli_cred_finan = p_s11p136, soli_cred_finan_aprob = p_s11p136a,
         # Acceso y uso de agua
         dific_agua_por_contaminacion = p_s11p126_sp1, dific_agua_por_lodo_tierr = p_s11p126_sp2, 
         dific_agua_por_daño_infra = p_s11p126_sp3, dific_agua_por_sequia = p_s11p126_sp4,
         dific_agua_por_corte_serv = p_s11p126_sp5, dific_agua_por_restricc = p_s11p126_sp6, 
         dific_agua_por_fen_natu = p_s11p126_sp7, dific_agua_por_no_infrae = p_s11p126_sp8, 
         dific_agua_por_no_dific = p_s11p126_sp9,
         # Sistemas de riego
         sist_riego_no_usa = p_s6p52_sp99, sist_riego_goteo = p_s6p52_spa02, 
         sist_riego_aspersion = p_s6p52_spa04, sist_riego_gravedad = p_s6p52_spc20, 
         sist_riego_manual = p_s6p52_spc21, sist_riego_bombeo = p_s6p52_spc22,
         # Tipo de semillas
         tipo_semill_util = p_s6p50, 
         # Fertilizante
         fertili_organico = p_s6p76_sp1, fertili_quimico = p_s6p76_sp2, fertili_correc_acid = p_s6p76_sp3, 
         fertili_quemas = p_s6p76_sp4, fertili_rezos = p_s6p76_sp5, fertili_ritos = p_s6p76_sp6,
         fertili_pagament = p_s6p76_sp7, fertili_no_aplico = p_s6p76_sp8,
         # MIPE
         mipe_manual = p_s6p77_sp1, mipe_organico = p_s6p77_sp2, mipe_quimico = p_s6p77_sp3, 
         mipe_biologico = p_s6p77_sp4, mipe_mecanizado = p_s6p77_sp5, mipe_plant_repel = p_s6p77_sp6, 
         mipe_plant_gene_mod = p_s6p77_sp7, mipe_rezos = p_s6p77_sp8, mipe_ritos = p_s6p77_sp9, 
         mipe_pagamentos = p_s6p77_sp10, mipe_ninguno = p_s6p77_sp11,
         # temas_asis_tec = p_s11p135a_sp11, 
         # Toma de decisiones
         num_per_decisiones = p_s4p16,
         num_per_decisiones_h = snh,
         num_per_decisiones_m = snm,
         num_per_decisiones_no = sn9,
         num_per_decisiones_jur = p_s4p21
  ) %>%
  # Variables identificadoras numericas
  mutate(cod_dpto = as.numeric(cod_dpto), cod_mpio = as.numeric(cod_mpio), cod_vereda = as.numeric(cod_vereda)) %>%
  # Ordenar variables identificadoras
  dplyr::select(cod_dpto, cod_mpio, cod_vereda, encuesta, tipo_uc, pred_etnica, id_prod, tipo_cul_lote, area_upa,
                starts_with("area"), contains("bov"), cant_leche, everything()) %>%
  # Eliminamos variables que no son de interes
  dplyr::select(-c(p_s7pnumpez))

# Destino de la produccion
names(cna_clean) <- gsub("p_s6p61_sp", "dest_prod_", names(cna_clean))

# Productor pertenece a asociaciones
names(cna_clean) <- gsub("p_s11p134_sp", "esta_asoc_", names(cna_clean))

# Homogeneizar areas (pasamos de metros a hectareas)
# Area sembrada y area cosechada ya estan en hectareas
cna_clean <- cna_clean %>%
  mutate(area_upa = area_upa/10000, area_agropecuario = area_agropecuario/10000, area_inf_noagro = area_inf_noagro/10000, 
         area_pasto_nat = area_pasto_nat/10000, area_pasto_sem = area_pasto_sem/10000, area_otros_usos = area_otros_usos/10000)

# Chequear que areas esten en Ha: areas de cultivos y otros no pueden superar el area total de la UPA
cna_clean %>% dplyr::select(starts_with("area")) %>% summary()

# Pegar sexo del productor principal (jefe del hogar)
# cna_sample <- readRDS(glue("{datos}/base_cna_clean_sample.rds"))
# cna_clean <- readRDS(glue("{datos}/base_cna_clean.rds"))
# 
# jefe <- readRDS(glue("{datos}/sexo_jefe_hogar_upa.rds"))
# 
# cna_clean <- cna_clean %>% 
#   left_join(jefe, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda"))
# 
# cna_sample <- cna_sample %>% 
#   left_join(jefe, by = c("cod_dpto", "cod_mpio", "encuesta", "cod_vereda"))

#-------------------------------------------------------#
# 3. Cálculos adicionales ----
#-------------------------------------------------------#

# Area agricola
# cna_clean <- readRDS(glue("{datos}/base_cna_clean.rds"))

cna_clean <- cna_clean %>%
  mutate(area_agricola = area_upa - (area_pasto_nat + area_pasto_sem + area_inf_noagro + area_otros_usos),
         area_agricola = ifelse(area_agricola < 0, 0, area_agricola))

#--------------------------#
# A. Jornales e ingresos ----
#--------------------------#

# Calculo de jornales viene de Hamman (2018)
# Pasamos produccion de toneladas a kilogramos
cna_clean <- cna_clean %>%
  mutate(tpj = total_trab*(6*4*12), ja = total_jorn*12, jornales = tpj+ja,
         cant_cosecha_kg = cant_cosecha*1000)

summary(cna_clean$cant_cosecha)

# Precios por cultivo
precios <- readxl::read_excel(glue("{datos}/precios/lista_precios_cultivos_area_cna.xlsx")) %>%
  dplyr::select(-c("tipo_fuente", "fuente")) %>% dplyr::rename(tipo_cul_lote = cod_cultivo)

precios_info <- precios %>% 
  drop_na(precio) %>%
  mutate(part_all = sum(part))

cna_clean <- cna_clean %>% left_join(precios, by = "tipo_cul_lote")

# Calcular ingreso por cultivo de la UPA
cna_clean <- cna_clean %>% 
  mutate(ing_cosecha = precio*cant_cosecha_kg) %>%
  # Factores de conversion a cultivos especificos (Hamman, 2018)
  mutate(ing_cosecha = ifelse(tipo_cul_lote == "00180201001", ing_cosecha*(1/0.115), 
                              ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(1/0.187),
                                     ifelse(tipo_cul_lote == "00149102001", ing_cosecha*(0.664), ing_cosecha)))) 

# Sumamos ingresos por UPA (ingreso en pesos por kg cosechado)
# hay informacion de ingresos para 905,334 UPA y UPNA
ingresos_upa <- cna_clean %>%
  drop_na(cant_cosecha, precio) %>%
  distinct(cod_vereda, encuesta, tipo_cul_lote, cant_cosecha, .keep_all = T) %>%
  group_by(cod_vereda, encuesta) %>%
  summarise(ing_cul_upa = sum(ing_cosecha, na.rm = T)) %>%
  ungroup()

# % del Area de los cultivos con informacion de precios (75.7%)
info_precios <- cna_clean %>% 
  mutate(tiene_precio = ifelse(!is.na(precio), 1, 0)) %>%
  group_by(tiene_precio) %>%
  summarise(area_sembrada = sum(area_sembrada, na.rm = T)) %>%
  ungroup() %>%
  mutate(total = sum(area_sembrada), part = 100*(area_sembrada/total))

rm(ingresos_upa, info_precios)

#--------------------------#
# B. % UPA dedicado a cultivo ----
#--------------------------#

# Calcular area de la upa dedicada a cada cultivo
cna_clean <- cna_clean %>% 
  mutate(prc_sem = 100*(area_sembrada/area_agricola), prc_cos = 100*(area_cosechada/area_agricola))

# Tenemos 9610 UPA que reportan areas sembradas/cosechadas mayores al area agricola de la UPA
test <- cna_clean %>% 
  filter(prc_cos > 101 | prc_sem > 101) %>%
  dplyr::select(starts_with(c("cod", "encuesta", "area", "prc"))) %>% 
  distinct(encuesta)

# Si area mayor esta entre 100 y 101%, se reemplaza por 100, otros casos son reemplazados con NA
cna_clean <- cna_clean %>%
  mutate(prc_sem = ifelse(prc_sem > 100 & prc_sem <= 101, 100, prc_sem),
         prc_sem = ifelse(prc_sem > 101, NA, prc_sem),
         prc_cos = ifelse(prc_cos > 100 & prc_cos <= 101, 100, prc_cos),
         prc_cos = ifelse(prc_cos > 101, NA, prc_cos))

#-------------------------------------------------------#
# 4. Exportar ----
#-------------------------------------------------------#

# Guardar sample del 20% para pruebas
rows <- round(nrow(cna_clean)/5)
cna_sample <- cna_clean[1:rows,]
saveRDS(cna_sample, glue("{datos}/base_cna_clean_sample.rds"))

# Base completa final
saveRDS(cna_clean, glue("{datos}/base_cna_clean.rds"))
