
teste_breslowday <- function(dados1, dados2, dados3, dados4, var) {
  tab1 <- array(0, dim = c(2,2,2))
  tab2 <- array(0, dim = c(2,2,2))
  tab3 <- array(0, dim = c(2,2,2))
  tab1[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab1[,,2] <- table(dados2$grupos, as.character(dados2[[var]]))
  tab2[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab2[,,2] <- table(dados3$grupos, as.character(dados3[[var]]))
  tab3[,,1] <- table(dados1$grupos, as.character(dados1[[var]]))
  tab3[,,2] <- table(dados4$grupos, as.character(dados4[[var]]))
  a <- BreslowDayTest(tab1, correct = TRUE)
  b <- BreslowDayTest(tab2, correct = TRUE)
  d <- BreslowDayTest(tab3, correct = TRUE)
  out <- data.frame(comp=c("orig-gama","orig-delta","orig-omicr"),
                    stat = c(a$statistic, b$statistic, d$statistic),
                    p_valor = c(a$p.value, b$p.value, d$p.value)
  )
  return(out)
}

dados123 <- function(dados,var){
  dados1 <- dados %>% select(variante,vacinacov_variante2,grupos,var)
  
  return(dados1)
}


df <- dados123(dados5,"evolucao")



dados_smote <- function(dados,var){
  dados1 <- dados %>% select(variante,vacinacov_variante2,grupos,var) %>% drop_na()
  
  set.seed(69)
  
  ds_rec <- recipe(vacinacov_variante2 ~., data = dados1) %>%
    step_smotenc(vacinacov_variante2) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}


df <- teste_breslowday(
  new_data %>% filter(variante == "original"),
  new_data %>% filter(variante == "gama"),
  new_data %>% filter(variante == "delta"),
  new_data %>% filter(variante == "omicron"),
  var
)

return(df)


var <- "evolucao"
dados <- dados5

dados1 <- dados %>% select(variante,vacinacov_variante2,grupos,var) %>% drop_na()

set.seed(69)

ds_rec <- recipe(vacinacov_variante2 ~., data = dados1) %>%
  step_smotenc(vacinacov_variante2) %>%
  prep()

new_data <- juice(ds_rec)


df <- teste_breslowday(
  new_data %>% filter(variante == "original"),
  new_data %>% filter(variante == "gama"),
  new_data %>% filter(variante == "delta"),
  new_data %>% filter(variante == "omicron"),
  var
)


dados_smote <- function(dados,var){
  dados1 <- dados %>% select(variante,vacinacov_variante2,grupos,var) %>% drop_na()
  
  set.seed(69)
  
  ds_rec <- recipe(vacinacov_variante2 ~., data = dados1) %>%
    step_smotenc(vacinacov_variante2) %>%
    prep()
  
  new_data <- juice(ds_rec)
  
  return(new_data)
  
}

new_data <- dados_smote(dados5,"evolucao")

df <- teste_breslowday1_smote(dados5,"evolucao")
