#Script AGENDA POLÍTICA

#install.packages("survey")   
#install.packages("readstata13")
 

library(writexl)
library(tidyverse)
library(survey)
library(readstata13)
library(haven)
library(memisc)


#DESCRITIVAS E RECODIFICAÇÕES####
#2023####
BaseQual = bd_clivagens_classe

#Merge da base que está toda numérica
BaseNum <- 
merge(Banco_de_dados_Pesquisa_UFSC_Questionário_1_, 
      Banco_de_dados_Pesquisa_UFSC_Questionário_2_, all = T)

#salvando numérica
writexl::write_xlsx(BaseNum, path = "BaseNum.xlsx")

#salvando qualitativa
writexl::write_xlsx(BaseQual, path = "BaseQual.xlsx")


table(BASEQUAL_QUANTI$P50)

# 1    2    3    4    5 
# 142 1069  153   86   50 
table(BASEQUAL_QUANTI$P50Q)
# A democracia é preferível a qualquer outra forma de governo 
# 1069 
# Em algumas circunstâncias, um governo autoritário pode ser preferível a um democrático 
# 153 
# Não respondeu 
# 50 
# Não sabe 
# 86 
# Para pessoas como eu, tanto faz um regime democrático ou um não democrático 
#142


#Casamento homossexual
mean(BASEQUAL_QUANTI$P49)
#[1] 8.57

table(BASEQUAL_QUANTI$P49Q)

# table(BASEQUAL_QUANTI$P49)
#   0    1    2    3    4    5    6    7    8    9   10   11   12 #11NS e 12 NR
# 109    3    8   12   16   71   18   57   95   47 1029   19   16 
# > table(BASEQUAL_QUANTI$P49Q)
# 
# Cinco           Dez          Dois Não Respondeu      Não sabe          Nove 
# 71          1029             8            16            19            47 
# Oito        Quatro          Seis          Sete          Três            Um 
# 95            16            18            57            12             3 
# Zero 
# 109 

#Tranformar o 11 e 12 em NA
BASEQUAL_QUANTI$P49 <- ifelse(BASEQUAL_QUANTI$P49 %in% c(11, 12), 
                               NA, BASEQUAL_QUANTI$P49)

# Calculando a média da variável quantitativa por categoria
media_por_categoria <- aggregate(P49 ~ P50, 
                                 data = BASEQUAL_QUANTI, FUN = mean)

# Exibindo o resultado
print(media_por_categoria)

#P50      P49
# 1   1 8.514286
# 2   2 8.455924
# 3   3 8.706667
# 4   4 8.506494
# 5   5 8.837209  

#MÉDIAS ALTAS PARA TODAS AS CATEGORIAS, ou seja, em média democratas 
#e não democratas são mais tolerantes

#Recodificando a variável democracia para descritiva####
#Democrata = 2 #Não democrata = 1,3,4,5

BASEQUAL_QUANTI$Dem <- memisc::recode(BASEQUAL_QUANTI$P50,
                              "Democrata" <- 2, 
                              "Não democrata" <- c(1,5),
                              NA <- c(3,4))

table(BASEQUAL_QUANTI$Dem)
# Democrata Não democrata 
#  1069           192 

# Calculando a média da variável quantitativa por categoria
media_por_categoria <- aggregate(P49 ~ Dem, 
                                 data = BASEQUAL_QUANTI, FUN = mean)


#Relação2023####
media_por_categoria
#          Dem      P49
# 1     Democrata 8.455924
# 2 Não democrata 8.590164

#LAPOP 
#homossexuais candidatarem a cargos públicos
#d5

#homossexuais casarem
#d6

#democracia
# DEM2. Agora mudando de assunto, com qual das seguintes três frases o(a) sr./sra. está 
# mais de acordo: 
#   (1) Para pessoas como eu, tanto faz um regime democrático ou um não democrático, ou 
# (2) A democracia é preferível a qualquer outra forma de governo, ou 
# (3) Em algumas circunstâncias, um governo autoritário pode ser preferível a um 
# democrático. 
# (88) NS (98) NR



#2014####

Bra2014$dem2
# label
# Le da lo mismo un régimen democrático que uno no democrático
# La democracia es preferible a cualquier otra forma de gobierno
# En algunas circunstancias un gobierno autoritario puede ser preferible
# No Sabe
# No Responde

#Eu já havia drop na
table(Bra2014$dem2)
# 1   2   3 
# 207 903 280 

#Análise da média de democratas e não democratas 
#aceitarem o casamento de pessoas homossexuais
media_por_categoria <- aggregate(d6 ~ dem2, 
                                 data = Bra2014, FUN = mean)
media_por_categoria
#    dem2       d6
# 1    1 4.873786
# 2    2 5.105323
# 3    3 5.353791

media_por_categoria <- aggregate(d5 ~ dem2, 
                                 data = Bra2014, FUN = mean)

media_por_categoria 
#  dem2       d5
# 1    1 6.466019
# 2    2 6.431920
# 3    3 6.163043

#Recodificando a variável democracia para descritiva####
#Democrata = 2 #Não democrata = 1,3
Bra2014$Dem <- as.factor(Bra2014$dem2)

Bra2014$Dem <- recode(Bra2014$Dem,
                              "Democrata" <- 2, 
                              "Não democrata" <- c(1,3))

table(Bra2014$Dem)
# Democrata Não democrata 
# 903           487

# Calculando a média da variável quantitativa por categoria
#Cargo político
media_por_categoria <- aggregate(d5 ~ Dem, 
                                 data = Bra2014, FUN = mean)
#Relação2014 CP####
media_por_categoria
#        Dem       d5
# 1     Democrata 6.431920
# 2 Não democrata 6.292531

# Calculando a média da variável quantitativa por categoria
#Casamento
media_por_categoria <- aggregate(d6 ~ Dem, 
                                 data = Bra2014, FUN = mean)
#Relação2014 Cas####
media_por_categoria
#        Dem       d6
# 1     Democrata 5.105323
# 2 Não democrata 5.149068

#2017####
#democracia 
Bra2017$ing4

# Labels:
#   value             label
# 1 Strongly Disagree
# 7    Strongly Agree
# NA(a)        Don't Know
#  NA(b)       No Response

#Análise da média de democratas e não democratas 
#aceitarem o casamento de pessoas homossexuais
media_por_categoria <- aggregate(d6 ~ ing4, 
                                 data = Bra2017, FUN = mean)
media_por_categoria
# ing4       d6
# 1    1 5.691176
# 2    2 4.798077
# 3    3 5.528205
# 4    4 5.648551
# 5    5 5.572917
# 6    6 5.777778
# 7    7 6.206780

media_por_categoria <- aggregate(d5 ~ ing4, 
                                 data = Bra2017, FUN = mean)

media_por_categoria 
#    ing4       d5
# 1    1 6.274074
# 2    2 5.761905
# 3    3 6.649485
# 4    4 6.552727
# 5    5 6.608247
# 6    6 6.842640
# 7    7 7.772881

#Recodificando a variável democracia para descritiva####
#Democrata = 5,6,7 #Não democrata = 1,2,3,4
Bra2017$Dem <- as.factor(Bra2017$ing4)

Bra2017$Dem <- recode(Bra2017$Dem,
                      "Democrata" <- c(4,5,6,7), 
                      "Não democrata" <- c(1,2,3))

table(Bra2017$Dem)
#  Democrata Não democrata 
#  1066           439

# Calculando a média da variável quantitativa por categoria
#Cargo político
media_por_categoria <- aggregate(d5 ~ Dem, 
                                 data = Bra2017, FUN = mean)
#Relação2017 CP####
media_por_categoria
#        Dem       d5
# 1     Democrata  6.962193
# 2 Não democrata  6.317972

# Calculando a média da variável quantitativa por categoria
#Casamento
media_por_categoria <- aggregate(d6 ~ Dem, 
                                 data = Bra2017, FUN = mean)
#Relação2017 Cas####
media_por_categoria
#        Dem       d6
# 1     Democrata 5.807947
# 2 Não democrata 5.404598


#2019####
#democracia 
Bra2019$ing4

#  Labels:
#   value             label
# 1 Muy en desacuerdo
# 7    Muy de acuerdo
# NA(a)           No sabe
# NA(b)       No responde
# NA(c)         No Aplica

#Análise da média de democratas e não democratas 
#aceitarem o casamento de pessoas homossexuais
media_por_categoria <- aggregate(d6 ~ ing4, 
                                 data = Bra2019, FUN = mean)
media_por_categoria
# ing4       d6
# 1    1 5.092593
# 2    2 4.634615
# 3    3 5.814815
# 4    4 5.754386
# 5    5 5.801980
# 6    6 5.481481
# 7    7 6.070822
media_por_categoria <- aggregate(d5 ~ ing4, 
                                 data = Bra2019, FUN = mean)

media_por_categoria 
# ing4       d5
# 1    1 6.462963
# 2    2 5.653846
# 3    3 6.637037
# 4    4 6.871080
# 5    5 6.741722
# 6    6 6.764977
# 7    7 7.585714

#Retirar os NAs
library(tidyr)
Bra2019 <- drop_na(Bra2019, ing4)
mean(Bra2019$ing4)



Bra2019 <- drop_na(Bra2019, d5)
mean(Bra2019$d5)


media_relacao <- mean(Bra2019$ing4/ Bra2019$d5)

media_relacao
#[1] 1.256349 não faz sentido essa relação

#Recodificando a variável democracia para descritiva####
#Democrata = 5,6,7 #Não democrata = 1,2,3,4
Bra2019$Dem <- as.factor(Bra2019$ing4)

Bra2019$Dem <- recode(Bra2019$Dem,
                      "Democrata" <- c(4,5,6,7), 
                      "Não democrata" <- c(1,2,3))

table(Bra2019$Dem)
#  Democrata Não democrata 
#  1156           295 

# Calculando a média da variável quantitativa por categoria
#Cargo político
media_por_categoria <- aggregate(d5 ~ Dem, 
                                 data = Bra2019, FUN = mean)
#Relação2019 CP####
media_por_categoria
#        Dem       d5
# 1     Democrata  7.033737
# 2 Não democrata  6.400000

# Calculando a média da variável quantitativa por categoria
#Casamento
media_por_categoria <- aggregate(d6 ~ Dem, 
                                 data = Bra2019, FUN = mean)
#Relação2019 Cas####
media_por_categoria
#        Dem       d6
# 1     Democrata 5.814075
# 2 Não democrata 5.329932

##############
#FATORIAL
#Fazer análise fatorial para juntar as duas variáveis
#sobre democracia 
#juntar tbém a tolerância ao casamento e candidatura

#ANÁLISE FATORIAL
# índice de apoio institucional 
ab21 %>%
  # selecionando variáveis de interesse apenas
  dplyr::select(b2:b47a) %>%
  # FA exploratória
  psych::fa(nfactors = 1, rotate = 'varimax', fm = 'ols') -> fa1

#################

#Visualização dos dados####

library(officer)
library(magrittr)
library(tibble)

# Criando os vetores com os dados
ano <- c(2014, 2017, 2019, 2023)
democracia <- c("Democrata", "Não democrata", "Democrata", "Não democrata", "Democrata", "Não democrata", "Democrata", "Não democrata")
media_aprov_casamento <- c(5.105323, 5.149068, 5.807947, 5.404598, 5.814075, 5.329932, 8.496726, 8.751740)

# Criando um dataframe com os dados
df <- tibble(Ano = rep(ano, each = 2),
             Democracia = democracia,
             Media_Aprov_Casamento = media_aprov_casamento)

# Reduzindo as casas decimais para 2
df$Media_Aprov_Casamento <- round(df$Media_Aprov_Casamento, 2)

# Criando um documento Word em branco
doc <- read_docx()

# Criando a tabela
tab_content <- df %>%
  tidyr::pivot_wider(names_from = Democracia, values_from = Media_Aprov_Casamento)

# Adicionando a tabela ao documento Word
doc <- doc %>%
  body_add_table(tab_content, style = "Normal Table")

# Salvando o documento Word
print(doc, target = "tabela2.docx")

########
# Dados da tabela 1
ano <- c(2014, 2017, 2019, 2023)
dem_tolerancia_casamento <- c(5.11, 5.81, 5.81, 8.45)
nao_dem_tolerancia_casamento <- c(5.15, 5.4, 5.33, 8.59)
media_geral_casamento <- c(5.13, 5.6, 5.57, 8.52)

(8.455924+8.590164)/2

# Dados da tabela 2
dem_tolerancia_candidatura <- c( 6.43, 6.96, 7.03,NA)
nao_dem_tolerancia_candidatura <- c( 6.29, 6.96, 6.4,NA)
media_geral_candidatura <- c( 6.36, 6.96, 6.72,NA)

# Criar um dataframe combinando as tabelas
dados <- data.frame(ano, dem_tolerancia_casamento, nao_dem_tolerancia_casamento, media_geral_casamento,
                    dem_tolerancia_candidatura, nao_dem_tolerancia_candidatura, media_geral_candidatura)

# Gráfico de linhas
library(ggplot2)


GraMedias <- ggplot(dados, aes(x = ano)) +
  geom_line(aes(y = dem_tolerancia_casamento, color = "Democrata", linetype ="Tol. Social" ), size = 0.6) +
  geom_line(aes(y = nao_dem_tolerancia_casamento, color = "Não Democrata", linetype ="Tol. Social" ), size = 0.6) +
  geom_line(aes(y = media_geral_casamento, color = "Média Geral", linetype ="Tol. Social" ), size = 0.6) +
  geom_line(aes(y = dem_tolerancia_candidatura, color = "Democrata", linetype ="Tol. Política"), size = 0.8) +
  geom_line(aes(y = nao_dem_tolerancia_candidatura, color = "Não Democrata", linetype ="Tol. Política" ), size = 0.8) +
  geom_line(aes(y = media_geral_candidatura, color = "Média Geral", linetype ="Tol. Política"), size = 0.8) +
  labs(x = "Anos", y = "Média de Tolerância", color = "Grupo", linetype = "Tipo de Linha") +
  scale_color_manual(values = c("Democrata" = "blue", "Não Democrata" = "red", "Média Geral" = "black")) +
  scale_linetype_manual(values = c("Tol. Social" = "solid", "Tol. Política" = "dashed")) +
  theme_minimal()



GraMedias + theme(text = element_text(family = "serif", size = 12),
                  title = element_text(color = "black"),
                  axis.line = element_line(color = "black"), 
                  axis.text = element_text(colour = "black", size = rel(0.7)))


#########################
#ANÁLISE DAS VARIÁVEIS SOBRE TOLERÂNCIA

#Relação entre as tolerância

#2014####
# Execute o teste de correlação de Pearson
resultado <- cor.test(Bra2014$d5, Bra2014$d6)

# Exiba o resultado do teste
print(resultado)

# Pearson's product-moment correlation
# 
# data:  Bra2014$d5 and Bra2014$d6
# t = 30.085, df = 1458, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5861933 0.6495625
# sample estimates:
#       cor 
# 0.6188839 


#2017####
# Execute o teste de correlação de Pearson
resultado <- cor.test(Bra2017$d5, Bra2017$d6)

# Exiba o resultado do teste
print(resultado)

# Pearson's product-moment correlation
# 
# data:  Bra2017$d5 and Bra2017$d6
# t = 29.807, df = 1505, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#  0.5764978 0.6400619
# sample estimates:
#       cor 
# 0.6092576 

###

#2019####
# Execute o teste de correlação de Pearson
resultado <- cor.test(Bra2019$d5, Bra2019$d6)

# Exiba o resultado do teste
print(resultado)

# Pearson's product-moment correlation
# 
# data:  Bra2019$d5 and Bra2019$d6
# t = 26.92, df = 1460, p-value < 2.2e-16
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
#   0.5406415 0.6092237
# sample estimates:
#   cor 
# 0.5759451 


library(psych)

# Criar um data frame com as duas variáveis de interesse
dados <- data.frame(Bra2014$d5, Bra2014$d6)

# Realizar a análise fatorial com duas variáveis
fatores <- principal(dados, nfactors = 1)

# Exibir as cargas fatoriais resultantes
print(fatores$loadings)

# Loadings:
#   PC1
# Bra2014.d5 0.9
# Bra2014.d6 0.9
# 
# PC1
# SS loadings    1.619
# Proportion Var 0.809

#Eplicação####
# O resultado apresenta os loadings (cargas fatoriais) e a variância explicada para um componente principal (PC1) em uma análise fatorial. Aqui está a explicação dos diferentes valores:
#   
#   Loadings: Os loadings representam a correlação entre cada variável e o componente principal extraído. No caso apresentado, as variáveis "Bra2014.d5" e "Bra2014.d6" têm um loading de 0.9 no PC1. Isso indica que essas duas variáveis têm uma correlação forte com o componente principal extraído. Quanto maior o valor do loading (mais próximo de 1), mais influência a variável tem no componente principal.
# 
# SS loadings: SS loadings (soma dos quadrados dos loadings) é uma medida da quantidade total de variância explicada pelo componente principal. No exemplo, a soma dos quadrados dos loadings no PC1 é igual a 1.619. Esse valor indica que o componente principal explica 1.619 unidades de variância.
# 
# Proportion Var: A proporção de variância (Proportion Var) indica a fração da variância total explicada pelo componente principal. No caso apresentado, o PC1 explica 0.809 da variância total. Essa proporção é calculada dividindo a SS loadings pela soma total da variância.
# 
# Em resumo, os resultados indicam que o PC1 tem loadings fortes para as variáveis "Bra2014.d5" e "Bra2014.d6", indicando uma correlação positiva e significativa. Além disso, o PC1 explica uma proporção significativa (80.9%) da variância total.




fa.parallel(dados)

fa.r <- principal(dados, nfactors = 2,n.obs = 1500)

print(fa.r$loadings, cutoff = 0.45)

# Loadings:
#   RC1    RC2   
# Bra2014.d5  0.900       
# Bra2014.d6  0.900       
# 
# RC1   RC2
# SS loadings    1.619 0.381
# Proportion Var 0.809 0.191
# Cumulative Var 0.809 1.000
dados$Bra2014.d5 
alpha(dados[c("Bra2014.d5", "Bra2014.d6")])

#2017####
# Criar um data frame com as duas variáveis de interesse
dados <- data.frame(Bra2017$d5, Bra2017$d6)

# Realizar a análise fatorial com duas variáveis
fatores <- principal(dados, nfactors = 1)

# Exibir as cargas fatoriais resultantes
print(fatores$loadings)
# 
# Loadings:
#   PC1  
# Bra2017.d5 0.897
# Bra2017.d6 0.897
# 
# PC1
# SS loadings    1.609
# Proportion Var 0.805


#2019####
# Criar um data frame com as duas variáveis de interesse
dados <- data.frame(Bra2019$d5, Bra2019$d6)

# Realizar a análise fatorial com duas variáveis
fatores <- principal(dados, nfactors = 1)

# Exibir as cargas fatoriais resultantes
print(fatores$loadings)
# Loadings:
#   PC1  
# Bra2019.d5 0.888
# Bra2019.d6 0.888
# 
# PC1
# SS loadings    1.576
# Proportion Var 0.788

#ÍNDICE 2014####
# Se as variáveis não estiverem na mesma escala, você pode normalizá-las
Bra2014.d5_norm <- scale(Bra2014$d5)
Bra2014.d6_norm <- scale(Bra2014$d6)


# Atribuindo pesos iguais (0.5) aos loadings das variáveis
pesos <- c(0.5, 0.5)

# Criação do índice ponderado
Bra2014$IndBra2014 <- (Bra2014$d5 * pesos[1]) + 
  (Bra2014$d6 * pesos[2])

summary(Bra2014$IndBra2014)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   3.000   5.500   5.738   9.000  10.000      40 


# Calcula o coeficiente alfa de Cronbach
coef_alpha <- alpha(data.frame(Bra2014$d5, Bra2014$d6))
print(coef_alpha)

#Explicação####

# Os resultados indicam que a escala construída com as variáveis "d5" e "d6" apresenta uma confiabilidade razoavelmente boa. O coeficiente Alpha bruto e o coeficiente Alpha padronizado estão em torno de 0.76 e 0.77, respectivamente, o que sugere uma consistência interna moderada a boa entre os itens da escala.
# 
# Além disso, a média das correlações item-total (média_r) também é alta, com um valor de 0.62, o que indica uma forte relação entre os itens e a medida geral da escala.



# Carregar o pacote haven
library(haven)

# Salvar a base de dados em formato DTA
write_dta(Bra2014, "Bra2014.dta")


#ÍNDICE 2017####

# Atribuindo pesos iguais (0.5) aos loadings das variáveis
pesos <- c(0.5, 0.5)

# Criação do índice ponderado
Bra2017$IndBra2017 <- (Bra2017$d5 * pesos[1]) + 
  (Bra2017$d6 * pesos[2])

summary(Bra2017$IndBra2017)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   3.500   6.500   6.234  10.000  10.000      25 


# Calcula o coeficiente alfa de Cronbach
coef_alpha <- alpha(data.frame(Bra2017$d5, Bra2017$d6))
print(coef_alpha)

# Salvar a base de dados em formato DTA
write_dta(Bra2017, "Bra2017.dta")

#ÍNDICE 2019####

# Atribuindo pesos iguais (0.5) aos loadings das variáveis
pesos <- c(0.5, 0.5)

# Criação do índice ponderado
Bra2019$IndBra2019 <- (Bra2019$d5 * pesos[1]) + 
  (Bra2019$d6 * pesos[2])

summary(Bra2019$IndBra2019)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#   1.000   4.000   6.500   6.308   9.500  10.000      36 


# Calcula o coeficiente alfa de Cronbach
coef_alpha <- alpha(data.frame(Bra2019$d5, Bra2019$d6))
print(coef_alpha)

# Salvar a base de dados em formato DTA
write_dta(Bra2019, "Bra2019.dta")

table(Bra2014$IndBra2014)

#Regr2014####


table(Bra2014$Dem)
#1democrata   2não democrata 
#903          487 

#Recode
Bra2014$Dem <- as.factor(Bra2014$Dem)
Bra2014$Dem <- recode(Bra2014$Dem, 0 <- 2, 1 <- 1)

#Criação dos modelos de regressão
Mod2014.1 <-  glm(Dem ~ d5 + d6, data = Bra2014,
                family = binomial)


####

Bra2014 <- drop_na(Bra2014, IndBra2014)
mean(Bra2019$IndBra2014)

Mod2014.2 <-  glm(Dem ~  IndBra2014, 
                  data = Bra2014,
                  family = binomial(link = logit))

#Tabmodel####
summary(Mod2014.1)

library(sjPlot)

Mod1 <- tab_model(Mod2014.1, wrap.labels = 45, 
                  show.ci = F, show.se = F,  p.style = "stars")

Mod2 <- tab_model(Mod2014.2, wrap.labels = 45, 
                  show.ci = F, show.se = F,  p.style = "stars")
summary(Mod2014.2)


##Controle####
###religião###
table(Bra2014$q3c)

library(memisc)
Bra2014$q3c <- as.factor(Bra2014$q3c)
Bra2014$Relig <- recode(Bra2014$q3c, 
                        "Nenhuma" <-  c(4, 11), 
                        "Outras" <-  c(3, 6, 7, 12),
                        "Católico" <-  c(1),
                        "Protestante" <-  c(2, 5))
table(Bra2014$Relig)
# Nenhuma      Outras    Católico Protestante 
# 132          34         846         453 


#Sexo
#SEXI. Anote seu sexo: (1) Homem (2) Mulher 

table(Bra2014$sexi)
# 1   2 
# 661 839 

Bra2014$sexi <- as.factor(Bra2014$sexi)
Bra2014$Sexo <- recode(Bra2014$sexi, 
                        "Homem" <-  1, 
                        "Mulher" <-  2)

table(Bra2014$Sexo)


###Escolaridade###
#ED. Qual foi o último ano de escola que o(a) sr./sra. terminou 
table(Bra2014$ed)

Bra2014$Escol <- as.numeric(Bra2014$ed)

###Idade###
summary(Bra2014$q2)
Bra2014$Idade <- Bra2014$q2


#Modelo com as variáveis de controle

Mod2014.3 <-  glm(Dem ~ d5 + d6 + Escol + Idade + Sexo +
                    Relig, data = Bra2014,
                  family = binomial)


summary(Mod2014.3)
#
#Tabmodel####

Mod1.5 <- tab_model(Mod2014.3, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")
#
Mod2014.4 <-  glm(Dem ~ IndBra2014 + Escol + Idade + Sexo +
                    Relig, data = Bra2014,
                  family = binomial)


summary(Mod2014.4)

#Tabmodel####

Mod1.6 <- tab_model(Mod2014.4, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")
#Tabmodel####

Mod1.4 <- tab_model(Mod2023.2, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")

#Regr2017####
#Recode
Bra2017$Dem <- as.factor(Bra2017$Dem)
Bra2017$Dem <- recode(Bra2017$Dem, 0 <- 2, 1 <- 1)
table(Bra2017$Dem)
# 0    1 
# 439 1066 
#Criação dos modelos de regressão
Mod2017.1 <-  glm(Dem ~ d5 + d6, data = Bra2017,
                  family = binomial)

summary(Mod2017.1)


Mod2017.2 <-  glm(Dem ~  IndBra2017, 
                  data = Bra2017,
                  family = binomial(link = logit))

summary(Mod2017.2)

#Tabmodel####

Mod1.1 <- tab_model(Mod2017.1, wrap.labels = 45, 
                  show.ci = F, show.se = F,  p.style = "stars")

Mod2.1 <- tab_model(Mod2017.2, wrap.labels = 45, 
                  show.ci = F, show.se = F,  p.style = "stars")


##Controle####
###religião###
table(Bra2017$q3c)

library(memisc)
Bra2017$q3c <- as.factor(Bra2017$q3c)
Bra2017$Relig <- recode(Bra2017$q3c, 
                        "Nenhuma" <-  c(4, 11), 
                        "Outras" <-  c(3, 6, 7, 12),
                        "Católico" <-  c(1),
                        "Protestante" <-  c(2, 5))
table(Bra2017$Relig)
# Nenhuma      Outras    Católico Protestante 
#   134          42         776         487 


#Sexo
#SEXI. Anote seu sexo: (1) Homem (2) Mulher 

table(Bra2017$q1)
# 1   2 
# 661 839 

Bra2017$q1 <- as.factor(Bra2017$q1) #Ver 
Bra2017$Sexo <- recode(Bra2017$q1, 
                       "Homem" <-  1, 
                       "Mulher" <-  2)

table(Bra2017$Sexo)


###Escolaridade###
#ED. Qual foi o último ano de escola que o(a) sr./sra. terminou 
table(Bra2017$ed)

Bra2017$Escol <- as.numeric(Bra2017$ed)

###Idade###
summary(Bra2017$q2)
Bra2017$Idade <- Bra2017$q2


#Modelo com as variáveis de controle

Mod2017.3 <-  glm(Dem ~ d5 + d6 + Escol + Idade + Sexo +
                    Relig, data = Bra2017,
                  family = binomial)


summary(Mod2017.3)
#
#Tabmodel####

Mod1.5 <- tab_model(Mod2017.3, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")
#
Mod2017.4 <-  glm(Dem ~ IndBra2017 + Escol + Idade + Sexo +
                    Relig, data = Bra2017,
                  family = binomial)


summary(Mod2017.4)

#Tabmodel####

Mod1.6 <- tab_model(Mod2017.4, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")


#Regr2019####

#Recode
Bra2019$Dem <- as.factor(Bra2019$Dem)
Bra2019$Dem <- recode(Bra2019$Dem, 0 <- 2, 1 <- 1)
table(Bra2019$Dem)
# 0    1 
#  295 1156  

#Criação dos modelos de regressão
Mod2019.1 <-  glm(Dem ~ d5 + d6, data = Bra2019,
                  family = binomial)

summary(Mod2019.1)


Mod2019.2 <-  glm(Dem ~  IndBra2019, 
                  data = Bra2019,
                  family = binomial(link = logit))

summary(Mod2019.2)

#Tabmodel####

Mod1.2 <- tab_model(Mod2019.1, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")

Mod2.2 <- tab_model(Mod2019.2, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")
##Controle####
###religião###
table(Bra2019$q3cn)

library(memisc)
Bra2019$q3cn <- as.factor(Bra2019$q3cn)
Bra2019$Relig <- recode(Bra2019$q3cn, 
                        "Nenhuma" <-  c(4, 11), 
                        "Outras" <-  c(77,3,7,1501),
                        "Católico" <-  c(1),
                        "Protestante" <-  c(2, 5))
table(Bra2019$Relig)
# Nenhuma      Outras    Católico Protestante 
#  174          92         723         447 


#Sexo
#SEXI. Anote seu sexo: (1) Homem (2) Mulher 

table(Bra2019$q1)
# 1   2 
# 661 839 

Bra2019$q1 <- as.factor(Bra2019$q1)
Bra2019$Sexo <- recode(Bra2019$q1, 
                       "Homem" <-  1, 
                       "Mulher" <-  2)

table(Bra2019$Sexo)
# Homem Mulher 
# 736    715

###Escolaridade###
#ED. Qual foi o último ano de escola que o(a) sr./sra. terminou 
table(Bra2019$ed)

Bra2019$Escol <- as.numeric(Bra2019$ed)

###Idade###
summary(Bra2019$q2)
Bra2019$Idade <- Bra2019$q2


#Modelo com as variáveis de controle

Mod2019.3 <-  glm(Dem ~ d5 + d6 + Escol + Idade + Sexo +
                    Relig, data = Bra2019,
                  family = binomial)


summary(Mod2019.3)
#
#Tabmodel####

Mod1.5 <- tab_model(Mod2019.3, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")
#
Mod2019.4 <-  glm(Dem ~ IndBra2019 + Escol + Idade + Sexo +
                    Relig, data = Bra2019,
                  family = binomial)


summary(Mod2019.4)

#Tabmodel####

Mod1.6 <- tab_model(Mod2019.4, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")


#Regr2023####
#Criação dos modelos de regressão

save(BASEQUAL_QUANTI, file = "BASEQUAL_QUANTI.RData")
Bra2023 <- BASEQUAL_QUANTI

levels(Bra2023$Dem)

table(Bra2023$Dem)
# Democrata Não democrata 
# 1069           192 


Bra2023$Dem <- as.factor(Bra2023$Dem)
Bra2023$Dem <- memisc::recode(Bra2023$Dem, 0 <- "Não democrata", 
                              1 <- "Democrata")

Mod2023.1 <-  glm(Dem ~ P49, data = Bra2023,
                  family = binomial)


summary(Mod2023.1)
write_dta(Bra2023, "Bra2023.dta")

#Tabmodel####

Mod1.3 <- tab_model(Mod2023.1, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")

##Controle####
###religião###
table(Bra2023$P13Q)
library(tidyverse)

Bra2023$Relig <- case_match(Bra2023$P13Q, 
                       "Católico" ~ "Católico",
                       "Espírita Kardecista" ~ "Outras",
                       "Evangélico Pentecostal ou Carismático (por exemplo: Igreja de Deus, Assembleia de Deus, Igreja Universal do Reino de De" ~ "Protestantes",
                       "Igreja de Jesus Cristo dos Santos dos Últimos Dias ou SUD (Mórmon)" ~ "Outras",
                       "Não respondeu" ~ NA,
                       "Nenhuma" ~ "Nenhuma",
                       "Outra religião" ~ "Outras",
                       "Protestante (por exemplo: Luterana, Presbiteriana, Calvinista, Metodista e Batista)" ~ "Protestantes",
                       "Religiões de matriz afro (Umbanda, Candomblé, Quimbanda)" ~ "Outras",
                       "Testemunha de Jeová" ~ "Outras")

table(Bra2023$Relig)
Bra2023$Relig <- factor(Bra2023$Relig, 
                   levels = c("Nenhuma", "Outras", 
                              "Católico", "Protestantes"))
#Sexo
table(Bra2023$P6Q)

Bra2023$Sexo <- Bra2023$P6Q


###Escolaridade###
Bra2023$Escol <- case_match(Bra2023$P12Q, "Até a 4a série do Ensino Fundamental (Antigo Primário)" ~ 1,
                     "Da 5a à 9a série do Ensino Fundamental (Antigo Ginásio)" ~ 2,
                     "Ensino Médio (Antigo Colegial) incompleto" ~ 3,
                     "Ensino Médio (Antigo Colegial) completo" ~ 3,
                     "Superior incompleto" ~ 4,
                     "Superior completo ou mais" ~ 4)

###Idade###
Bra2023$Idade <- Bra2023$P8


#Modelo com as variáveis de controle

Mod2023.2 <-  glm(Dem ~ P49 + Escol + Idade + Sexo +
                    Relig, data = Bra2023,
                  family = binomial)


summary(Mod2023.2)


#Tabmodel####

Mod1.4 <- tab_model(Mod2023.2, wrap.labels = 45, 
                    show.ci = F, show.se = F,  p.style = "stars")


#############
#depois tenho que arrumar as variáveis de controle
#INserir os controles no modelo e gerar os tab models
#Ver se tem um jeito de já gerar as tabelas com valores
#exponeciados

tab_model(Mod2023.1, Mod2023.2,
          show.ci = F, show.se = F,  p.style = "stars")
