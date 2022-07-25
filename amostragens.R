library(tidymodels)
library(themis)
library(dplyr)
library(readxl)
# 
# dados <- readRDS("dados_var.rds")
dados1 <- readRDS("dados_amostragem.rds")

dataset <- dados1  %>%
  mutate(classi_gesta_puerp = as.factor(case_when(
    CS_GESTANT == 1 ~ "1tri",
    CS_GESTANT == 2 ~ "2tri",
    CS_GESTANT == 3 ~ "3tri",
    CS_GESTANT == 4 ~ "IG_ig",
    CS_GESTANT == 5 & PUERPERA == 1 ~ "puerp",
    CS_GESTANT == 9 & PUERPERA == 1 ~ "puerp",
    CS_GESTANT == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(raca = as.factor(case_when( # race
    CS_RACA == 1 ~ "branca",
    CS_RACA == 2 | CS_RACA == 3 | CS_RACA == 4 |
      CS_RACA == 5 ~ "não branca",
    CS_RACA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(escol = as.factor(case_when( # instruction
    # ~ "sem escolaridade",
    CS_ESCOL_N == 0 | CS_ESCOL_N == 1 | CS_ESCOL_N == 2 ~ "até fundamental",
    CS_ESCOL_N == 3 ~ "médio",
    CS_ESCOL_N == 4 ~ "superior",
    CS_ESCOL_N == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(vacina_cov = as.factor(case_when( #vacine
    VACINA_COV == 1  ~ "sim",
    VACINA_COV == 2 ~ "não",
    variante == "original" ~ "não",
    VACINA_COV == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(vacina = as.factor(case_when( #vacine
    VACINA == 1  ~ "sim",
    VACINA == 2 ~ "não",
    VACINA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(febre = as.factor(case_when( # fever
    FEBRE == 1 ~ "sim",
    FEBRE == 2 ~ "não",
    FEBRE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(tosse = as.factor(case_when( # cough
    TOSSE == 1 ~ "sim",
    TOSSE == 2 ~ "não",
    TOSSE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(garganta = as.factor(case_when( # throat
    GARGANTA == 1 ~ "sim",
    GARGANTA  == 2 ~ "não",
    GARGANTA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(dispneia = as.factor(case_when( # dyspnea
    DISPNEIA == 1 ~ "sim",
    DISPNEIA == 2 ~ "não",
    DISPNEIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(desc_resp = as.factor(case_when( # respiratory distress
    DESC_RESP == 1 ~ "sim",
    DESC_RESP == 2 ~ "não",
    DESC_RESP == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(saturacao = as.factor(case_when( # saturation
    SATURACAO == 1 ~ "sim",
    SATURACAO == 2 ~ "não",
    SATURACAO == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(diarreia = as.factor(case_when( # diarrhea
    DIARREIA == 1 ~ "sim",
    DIARREIA == 2 ~ "não",
    DIARREIA == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(cardiopati = as.factor(case_when( # heart disease
    CARDIOPATI == 1 ~ "sim",
    CARDIOPATI == 2 ~ "não",
    CARDIOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(pneumopati = as.factor(case_when( # lung disease
    PNEUMOPATI == 1 ~ "sim",
    PNEUMOPATI == 2 ~ "não",
    PNEUMOPATI == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(renal = as.factor(case_when( # kidney disease
    RENAL == 1 ~ "sim",
    RENAL == 2 ~ "não",
    RENAL == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>%
  mutate(obesidade = as.factor(case_when( # obesity
    OBESIDADE == 1 ~ "sim",
    OBESIDADE == 2 ~ "não",
    OBESIDADE == 9 ~ "ignorado",
    TRUE ~ "em branco"))
  ) %>% 
  mutate(mudou_muni = case_when((CO_MUN_RES == CO_MU_INTE) &
                                  !is.na(CO_MU_INTE) &
                                  !is.na(CO_MUN_RES) ~ "não",
                                (CO_MUN_RES != CO_MU_INTE) &
                                  !is.na(CO_MU_INTE) &
                                  !is.na(CO_MUN_RES) ~ "sim",
                                TRUE ~ "em branco")
  ) %>% 
  mutate(sg_para_srag = case_when(SURTO_SG == 1 ~ "sim",
                                  SURTO_SG == 2 ~ "não",
                                  SURTO_SG == 9 ~ "ignorado",
                                  TRUE ~ "em branco")
  ) %>% 
  mutate(inf_inter = case_when(NOSOCOMIAL == 1 ~ "sim",
                               NOSOCOMIAL == 2 ~ "não",
                               NOSOCOMIAL == 9 ~ "ignorado",
                               TRUE ~ "em branco")
  ) %>% 
  mutate(cont_ave_suino = case_when(AVE_SUINO == 1 ~ "sim",
                                    AVE_SUINO == 2 ~ "não",
                                    AVE_SUINO == 9 ~ "ignorado",
                                    TRUE ~ "em branco")
  ) %>% 
  mutate(
    antiviral = case_when(
      ANTIVIRAL == 1 ~ "Oseltamivir",
      ANTIVIRAL == 2 ~ "Zanamivir",
      ANTIVIRAL == 9 ~ "ignorado",
      TRUE ~ "em branco")
  ) %>% 
  mutate(vomito = case_when(VOMITO == 1 ~ "sim",
                            VOMITO == 2 ~ "não",
                            VOMITO == 9 ~ "ignorado",
                            TRUE ~ "em branco")) %>%
  mutate(dor_abd = case_when(DOR_ABD == 1 ~ "sim",
                             DOR_ABD == 2 ~ "não",
                             DOR_ABD == 9 ~ "ignorado",
                             TRUE ~ "em branco")) %>% 
  mutate(fadiga = case_when(FADIGA == 1 ~ "sim",
                            FADIGA == 2 ~ "não",
                            FADIGA == 9 ~ "ignorado",
                            TRUE ~ "em branco")) %>% 
  mutate(perd_olft = case_when(PERD_OLFT == 1 ~ "sim",
                               PERD_OLFT == 2 ~ "não",
                               PERD_OLFT == 9 ~ "ignorado",
                               TRUE ~ "em branco")) %>% 
  mutate(perd_pala = case_when(PERD_PALA == 1 ~ "sim",
                               PERD_PALA == 2 ~ "não",
                               PERD_PALA == 9 ~ "ignorado",
                               TRUE ~ "em branco")) %>% 
  mutate(hematologi = case_when(HEMATOLOGI == 1 ~ "sim",
                                HEMATOLOGI == 2 ~ "não",
                                HEMATOLOGI == 9 ~ "ignorado",
                                TRUE ~ "em branco")) %>% 
  mutate(hepatica = case_when(HEPATICA == 1 ~ "sim",
                              HEPATICA == 2 ~ "não",
                              HEPATICA == 9 ~ "ignorado",
                              TRUE ~ "em branco")) %>% 
  mutate(asma = case_when(ASMA == 1 ~ "sim",
                          ASMA == 2 ~ "não",
                          ASMA== 9 ~ "ignorado",
                          TRUE ~ "em branco")) %>% 
  mutate(neuro = case_when(NEUROLOGIC == 1 ~ "sim",
                           NEUROLOGIC == 2 ~ "não",
                           NEUROLOGIC == 9 ~ "ignorado",
                           TRUE ~ "em branco")) %>% 
  mutate(imunodepre = case_when(IMUNODEPRE == 1 ~ "sim",
                                IMUNODEPRE == 2 ~ "não",
                                IMUNODEPRE == 9 ~ "ignorado",
                                TRUE ~ "em branco")) %>% 
  mutate(uti = case_when(UTI == 1 ~ "sim",
                         UTI == 2 ~ "não",
                         UTI == 9 ~ "ignorado",
                         TRUE ~ "em branco")) %>% 
  mutate(
    suport_ven = case_when(
      SUPORT_VEN == 1 ~ "invasivo",
      SUPORT_VEN == 2 ~ "não invasivo",
      SUPORT_VEN == 3 ~ "não",
      SUPORT_VEN == 9 ~ "ignorado",
      TRUE ~ "em branco"
    )
  ) %>% 
  mutate(
    evolucao = case_when(
      EVOLUCAO == 1 ~ "Cura",
      EVOLUCAO == 2 ~ "Obito",
      EVOLUCAO == 3 ~ "Obito",
      EVOLUCAO == 9 ~ "ignorado",
      TRUE ~ "em branco"
    )
  ) %>% 
  mutate(diabetes = case_when(DIABETES == 1 ~ "sim",
                              DIABETES == 2 ~ "não",
                              DIABETES == 9 ~ "ignorado",
                              TRUE ~ "em branco"))
  

dados2 <-  dataset %>% select(
    ano,
    variante,
    classi_covid,
    classi_gesta_puerp,
    SG_UF,
    vacina_cov,
    vacina,
    raca,
    escol,
    idade_anos,
    mudou_muni,
    sg_para_srag,
    inf_inter,
    cont_ave_suino,
    antiviral,
    febre,
    tosse,
    garganta,
    dispneia,
    desc_resp,
    saturacao,
    diarreia,
    vomito,
    dor_abd,
    fadiga,
    perd_olft,
    perd_pala,
    cardiopati,
    hematologi,
    hepatica,
    asma,
    diabetes,
    neuro,
    pneumopati,
    imunodepre,
    renal,
    obesidade,
    uti,
    suport_ven,
    evolucao,
  )

dados2 <- dados2 %>% drop_na()

dados <- dados2 %>% 
  mutate(vacinacov_variante2 = as.factor(case_when(
    variante == "original" ~ "original",
    variante == "gama" & vacina_cov == "sim" ~ "gama_vacinasim",
    variante == "gama" & vacina_cov == "não" ~ "gama_vacinanao",
    variante == "delta" & vacina_cov == "sim" ~ "delta_vacinasim",
    variante == "delta" & vacina_cov == "não" ~ "delta_vacinanao",
    variante == "omicron" & vacina_cov == "sim" ~ "omicron_vacinasim",
    variante == "omicron" & vacina_cov == "não" ~ "omicron_vacinanao",
    TRUE ~ "Em branco")))



#Up-Sample


ds_rec <- recipe(vacinacov_variante2 ~., data = dados) %>%
  step_smotenc(vacinacov_variante2) %>%
  prep()

new_data <- juice(ds_rec)


new_data <- new_data %>% 
  mutate(
    grupos = case_when(
      classi_gesta_puerp == "1tri" |  classi_gesta_puerp == "2tri"  | classi_gesta_puerp == "3tri"| classi_gesta_puerp == "IG_ig"  ~ "gestante",
      classi_gesta_puerp == "puerp" ~ "puerpera"
    )
  ) %>% 
  mutate(
    faixa_et = case_when(
      idade_anos <= 19 ~ "<20",
      idade_anos >= 20
      & idade_anos <= 34 ~ "20-34",
      idade_anos >= 35 ~ ">=35",
      TRUE ~ NA_character_
    )) %>% 
  mutate(intubacao_SN = case_when(suport_ven == 1 ~ "sim",
                                  suport_ven == 2 ~ "não",
                                  suport_ven == 3 ~ "não",
                                  suport_ven == 9 ~ "ignorado",
                                  TRUE ~ "em branco"))


regions <- function(state) {
  southeast <- c("SP", "RJ", "ES", "MG")
  south <- c("PR", "SC", "RS")
  central <- c("GO", "MT", "MS", "DF")
  northeast <-
    c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
  north <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
  out <-
    ifelse(any(state == southeast),
           "southeast",
           ifelse(any(state == south),
                  "south",
                  ifelse(
                    any(state == central),
                    "central",
                    ifelse(any(state == northeast),
                           "northeast", "north")
                  )))
  return(out)
}

new_data$region <- sapply(new_data$SG_UF, regions)
new_data$region <-
  ifelse(is.na(new_data$region) == TRUE, 0, new_data$region)  


saveRDS(new_data,"dados_smote.rds")

# Downsample

ds_rec <- recipe(vacinacov_variante2 ~., data = dados) %>%
  step_downsample(vacinacov_variante2) %>%
  prep()

new_data <- juice(ds_rec)

new_data <- new_data %>% 
  mutate(
    grupos = case_when(
      classi_gesta_puerp == "1tri" |  classi_gesta_puerp == "2tri"  | classi_gesta_puerp == "3tri"| classi_gesta_puerp == "IG_ig"  ~ "gestante",
      classi_gesta_puerp == "puerp" ~ "puerpera"
    )
  ) %>% 
  mutate(
    faixa_et = case_when(
      idade_anos <= 19 ~ "<20",
      idade_anos >= 20
      & idade_anos <= 34 ~ "20-34",
      idade_anos >= 35 ~ ">=35",
      TRUE ~ NA_character_
    )) %>% 
  mutate(intubacao_SN = case_when(suport_ven == 1 ~ "sim",
                                  suport_ven == 2 ~ "não",
                                  suport_ven == 3 ~ "não",
                                  suport_ven == 9 ~ "ignorado",
                                  TRUE ~ "em branco"))


regions <- function(state) {
  southeast <- c("SP", "RJ", "ES", "MG")
  south <- c("PR", "SC", "RS")
  central <- c("GO", "MT", "MS", "DF")
  northeast <-
    c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE")
  north <- c("AC", "AP", "AM", "PA", "RO", "RR", "TO")
  out <-
    ifelse(any(state == southeast),
           "southeast",
           ifelse(any(state == south),
                  "south",
                  ifelse(
                    any(state == central),
                    "central",
                    ifelse(any(state == northeast),
                           "northeast", "north")
                  )))
  return(out)
}

new_data$region <- sapply(new_data$SG_UF, regions)
new_data$region <-
  ifelse(is.na(new_data$region) == TRUE, 0, new_data$region)  
    
  

saveRDS(new_data,"dados_downsample.rds")

#fazer depois > grupos,region,faixa_etaria



