

teste_breslowday <- function(dados1, dados2, dados3, dados4, var) {
  tab1 <- array(0, dim = c(2,2,2))
  tab2 <- array(0, dim = c(2,2,2))
  tab3 <- array(0, dim = c(2,2,2))
  tab1[,,1] <- table(dados1$grupos, dados1[[var]])
  tab1[,,2] <- table(dados2$grupos, dados2[[var]])
  tab2[,,1] <- table(dados1$grupos, dados1[[var]])
  tab2[,,2] <- table(dados3$grupos, dados3[[var]])
  tab3[,,1] <- table(dados1$grupos, dados1[[var]])
  tab3[,,2] <- table(dados4$grupos, dados4[[var]])
  a <- BreslowDayTest(tab1, correct = TRUE)
  b <- BreslowDayTest(tab2, correct = TRUE)
  d <- BreslowDayTest(tab3, correct = TRUE)
  out <- data.frame(comp=c("orig-gama","orig-delta","orig-omicr"),
                    stat = c(a$statistic, b$statistic, d$statistic),
                    p_valor = c(a$p.value, b$p.value, d$p.value)
  )
  return(out)
}

dados_smote1 <- dados_smote

dados_smote <- dados_smote %>% 
  filter(evolucao !="ignorado" & evolucao !="em branco")



teste_breslowday(
  dados_smote %>% filter(variante == "original"),
  dados_smote %>% filter(variante == "gama"),
  dados_smote %>% filter(variante == "delta"),
  dados_smote %>% filter(variante == "omicron"),
  "evolucao"
)

dados1 <- dados_smote %>% filter(variante == "original")
dados2 <- dados_smote %>% filter(variante == "gama")
dados3 <- dados_smote %>% filter(variante == "delta")
dados4 <- dados_smote %>% filter(variante == "omicron")
var <- "evolucao"


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
out <- data.frame(comp = c("orig-gama", "orig-delta", "orig-omicr"),
                  stat = c(a$statistic,b$statistic,d$statistic),
                  p_valor = c(a$p.value,b$p.value,c$p.value)
)






## Teste de Breslow Day ------------------------

dados5 <- readRDS("dados_var.rds")

df <- dados5 %>% select (evolucao,variante,grupos) %>% filter(!is.na(evolucao))

# Original  e Gama

df1 <- df %>% filter(variante == "original" | variante == "gama") 

tabela1 <- table(df1$grupos,df1$evolucao,df1$variante)

tab <- array(0, dim = c(2,2,2))
tab[,,1]<-tabela1[,,1]
tab[,,2]<-tabela1[,,2]

a <- BreslowDayTest(tab, correct = TRUE)

# Original  e Delta

df2 <- df %>% filter(variante == "original" | variante == "delta") 

tabela2 <- table(df2$grupos,df2$evolucao,df2$variante)

tab <- array(0, dim = c(2,2,2))
tab[,,1]<-tabela2[,,1]
tab[,,2]<-tabela2[,,2]

b <- BreslowDayTest(tab, correct = TRUE)

# Original  e omicron

df3 <- df %>% filter(variante == "original" | variante == "omicron") 

tabela3 <- table(df3$grupos,df3$evolucao,df3$variante)

BreslowDayTest(tabela3)



## Teste de Breslow Day SMOTE ------------------------

dados_smote <- readRDS("dados_smote.rds")

df <- dados_smote  %>% select (evolucao,variante,grupos) %>% filter(!is.na(evolucao))

# Original  e Gama

df1 <- df %>% filter((variante == "original" | variante == "gama") & (evolucao == "Obito" | evolucao== "Cura")) %>%  data.frame() 

tabela1 <- table(as.character(df1$variante),as.character(df1$grupos),as.character(df1$evolucao))

BreslowDayTest(tabela1)

# Original  e Delta

df2 <- df %>% filter(variante == "original" | variante == "delta") 

tabela2 <- table(as.character(df2$variante),as.character(df2$grupos),as.character(df2$evolucao))

BreslowDayTest(tabela2)

# Original  e omicron

df3 <- df %>% filter(variante == "original" | variante == "omicron") 

tabela3 <- table(as.character(df3$variante),as.character(df3$grupos),as.character(df3$evolucao))

BreslowDayTest(tabela3)







## Teste de Breslow Day Down-sample ------------------------

dados_downsample <- readRDS("dados_downsample.rds")

df <- dados_downsample  %>% select (evolucao,variante,grupos) %>% filter(!is.na(evolucao))

# Original  e Gama

df1 <- df %>% filter(variante == "original" | variante == "gama") 

tabela1 <- table(as.character(df1$variante),as.character(df1$grupos),as.character(df1$evolucao))

BreslowDayTest(tabela1)

# Original  e Delta

df2 <- df %>% filter(variante == "original" | variante == "delta") 

tabela2 <- table(as.character(df2$variante),as.character(df2$grupos),as.character(df2$evolucao))

BreslowDayTest(tabela2)

# Original  e omicron

df3 <- df %>% filter(variante == "original" | variante == "omicron") 

tabela3 <- table(df3$variante,df3$grupos,df3$evolucao)

tabela3 <- table(as.character(df3$variante),as.character(df3$grupos),as.character(df3$evolucao))

BreslowDayTest(tabela3)
