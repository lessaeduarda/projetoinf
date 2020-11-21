
require(plyr)
require(dplyr)
require(tidyverse)
require(tidyr)
require(ggplot2)
require(gridExtra)
require(ggpubr)
require(readxl)
require(haven)
require(magrittr)
require(labelled)
require(ggthemes)
require(extrafont)
require(scales)
require(gridExtra)
require(MASS)
require(lme4)
require(lmerTest)
require(MuMIn)
require(sjPlot)
require(ordinal)
---
  
# LATINOBAROMETRO:
# Carregar base do latinobarometro:
setwd("C:/Users/Duda/Desktop/Trab Final - OP")
latinob <- readRDS("Latinobarometro_2018_Esp_R_v20190303.rds")
latinooriginal <- latinob 

# Filtrar para países democráticos (economist's democracy index, v-dem):
latinob <- filter(latinob, IDENPA != 68)
latinob <- filter(latinob, IDENPA != 558)
latinob <- filter(latinob, IDENPA != 862)
latinob <- filter(latinob, IDENPA != 340)
latinob <- filter(latinob, IDENPA != 320)
latinob <- filter(latinob, IDENPA != 724)
latinob <- filter(latinob, IDENPA != 218)
count(latinob, IDENPA)

# Criar índice aditivo para VD:
# P19ST.A = Familia, P19ST.B = Amigos, P19ST.C = Colegas de trabalho, 
# P19ST.D = Colegas de estudo, P19ST.E = Radio, P19ST.F = Jornais/revistas,
# P19ST.G = Meios eletronicos/internet, P19ST.H = TV, P19NC.I = Facebook, 
# P19NC.J = Twitter, P19F.K = Youtube, P19ST.L = Outro 

latinob <- mutate(latinob, polinf = P19ST.A + P19ST.B + P19ST.C + P19ST.D +
                    P19ST.E + P19ST.F + P19ST.G + P19ST.H + P19NC.I + P19NC.J +
                    P19F.K + P19ST.L)


# Selecionar e renomear variáveis de interesse:
latinob <- data.frame("polinf" = latinob$polinf, 
                              "pais" = latinob$IDENPA,
                              "apoiodemocracia" = latinob$P12STGBS,
                              "confgoverno" = latinob$P15STGBSC.E,
                              "confpartidos" = latinob$P15STGBSC.H,
                              "oficiaiscorrup" = latinob$P73TI.C,
                              "nivelcorrup" = latinob$P80STTI,
                              "situacaoempreg" = latinob$S14A,
                              "percepsituecon" = latinob$P13STGBS.B,
                              "ideologia" = latinob$P22ST,
                              "idade" = latinob$EDAD,
                              "educacao" = latinob$S10,
                              "renda" = latinob$S1,
                              "confmidia" = latinob$P16NC.D,
                              "satsdemocracia" = latinob$P13STGBS.A,
                              "aprovpresidente" = latinob$P20STGBSC,
                              "sexo" = latinob$SEXO)

# Adicionar variável com escore do V-Dem:
## Código dos países:
## 32.- Argentina, 76.- Brasil,152.- Chile, 170.- Colombia,
## 188.- Costa Rica, 214.- Rep. Dominicana, 222.- El Salvador, 
## 484.- México, 591.- Panamá, 600.- Paraguay, 604.- Perú, 858.- Uruguay
latinob <- mutate(latinob, niveldem = ifelse(pais == 32, 81,
                                             ifelse(pais == 76, 67,
                                             ifelse(pais == 152, 77,
                                             ifelse(pais == 170, 67,
                                             ifelse(pais == 188, 89, 
                                             ifelse(pais == 214, 60,
                                             ifelse(pais == 222, 63,
                                             ifelse(pais == 484, 71,
                                             ifelse(pais == 591, 78,
                                             ifelse(pais == 600, 60,
                                             ifelse(pais == 604, 78,
                                             ifelse(pais == 858, 86, "NA"
                                                    )))))))))))))  

latinob <- mutate(latinob, eleicao = ifelse(pais == 32, 0,
                                      ifelse(pais == 76, 1,
                                      ifelse(pais == 152, 0,
                                      ifelse(pais == 170, 1,
                                      ifelse(pais == 188, 1, 
                                      ifelse(pais == 214, 0,
                                      ifelse(pais == 222, 0,
                                      ifelse(pais == 484, 1,
                                      ifelse(pais == 591, 0,
                                      ifelse(pais == 600, 1,
                                      ifelse(pais == 604, 0,
                                      ifelse(pais == 858, 0, "NA"
                                             )))))))))))))  


# Recodificar variáveis para que em todas o menor número signifique o menor nível:

latinob$confgoverno <- recode(latinob$confgoverno, 
                              "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                              "-1" = "NA", "-2" = "NA", "-4" = "NA")

latinob$apoiodemocracia <- recode(latinob$apoiodemocracia, 
                              "1" = "3", "2" = "1", "3" = "2","-1" = "NA", 
                              "-2" = "NA", "-3" = "NA", "-4" = "NA")

latinob$confpartidos <- recode(latinob$confpartidos, 
                              "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                              "-1" = "NA", "-2" = "NA", "-3" = "NA",  "-4" = "NA")

latinob$oficiaiscorrup <- recode(latinob$oficiaiscorrup, 
                               "1" = "1", "2" = "2", "3" = "3", "4" = "4", 
                               "-1" = "NA", "-2" = "NA", "-3" = "NA",  "-4" = "NA")

latinob$nivelcorrup <- recode(latinob$nivelcorrup, 
                              "1" = "5", "2" = "4", "3" = "3", "4" = "2", 
                              "5" = "1", "-1" = "NA")

latinob$situacaoempreg <- recode(latinob$situacaoempreg, 
                                 "1" = "1", "2" = "1", "3" = "1", "4" = "0", 
                                 "5" = "1", "6" = "0", "7" = "1",
                                 "-1" = "NA", "-2" = "NA")

latinob$percepsituecon <- recode(latinob$percepsituecon,
                                 "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                                 "-1" = "NA", "-2" = "NA", "-4" = "NA")


latinob$ideologia <- recode(latinob$ideologia,
                                 "0" = "1", "1" = "2", "2" = "3", "3" = "4", 
                                 "4" = "5", "5" = "6", "6" = "7", "7" = "8",
                                 "8" = "9", "9" = "10", "10" = "11", "-1" = "NA",
                                 "-2" = "NA", "-3" = "NA",  "-4" = "NA", "-5" = "NA",
                                 "-8" = "NA")

latinob$idade[latinob$idade == -2] <- NA

educ <- c(-1, -2)
latinob$educacao[latinob$educacao %in% educ] <- NA

renda <- c(-1,-2,-4)
latinob$renda[latinob$renda %in% renda] <- NA
latinob$renda <- recode(latinob$renda,
                            "1" = "5", "2" = "4", "3" = "3", "4" = "2", 
                            "5" = "1", "NA" = "NA")

latinob$confmidia <- recode(latinob$confmidia, 
                               "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                               "-1" = "NA", "-2" = "NA", "-3" = "NA",  "-4" = "NA")

latinob$satsdemocracia <- recode(latinob$satsdemocracia, 
                            "1" = "4", "2" = "3", "3" = "2", "4" = "1", 
                            "-1" = "NA", "-2" = "NA", "-3" = "NA",  "-4" = "NA")

latinob$aprovpresidente <- recode(latinob$aprovpresidente, 
                                 "1" = "2", "2" = "1", "-1" = "NA", "-2" = "NA",
                                 "-4" = "NA")

latinob$eleicao <- as.numeric(as.character(latinob$eleicao))
latinob$niveldem <- as.numeric(as.character(latinob$niveldem))
latinob$apoiodemocracia <- as.numeric(as.character(latinob$apoiodemocracia))
latinob$confgoverno <- as.numeric(as.character(latinob$confgoverno))
latinob$confpartidos <- as.numeric(as.character(latinob$confpartidos))
latinob$oficiaiscorrup <- as.numeric(as.character(latinob$oficiaiscorrup))
latinob$nivelcorrup <- as.numeric(as.character(latinob$nivelcorrup))
latinob$situacaoempreg <- as.numeric(as.character(latinob$situacaoempreg))
latinob$percepsituecon <- as.numeric(as.character(latinob$percepsituecon))
latinob$ideologia <- as.numeric(as.character(latinob$ideologia))
latinob$idade <- as.numeric(as.character(latinob$idade))
latinob$educacao <- as.numeric(as.character(latinob$educacao))
latinob$renda <- as.numeric(as.character(latinob$renda))
latinob$confmidia <- as.numeric(as.character(latinob$confmidia))
latinob$satsdemocracia <- as.numeric(as.character(latinob$satsdemocracia))
latinob$aprovpresidente <- as.numeric(as.character(latinob$aprovpresidente))
latinob$polinf <- as.numeric(as.character(latinob$polinf))
latinob$pais <- as.factor(as.numeric(latinob$pais))


# Salvar base:

saveRDS(latinob, file = "latinotratada")

# Estatística descritiva das variáveis:
summary(latinob$polinf)

# Regressão hierárquica: 
reg <- lmer(polinf ~ confgoverno + confpartidos + ideologia + niveldem + sexo +
              eleicao + idade + educacao + situacaoempreg + renda + confmidia + 
              nivelcorrup + aprovpresidente + (1 | pais), data = latinob)
                    
summary(reg)
r.squaredGLMM(reg)

p <- plot_model(reg, colors = c("#000000"), sort.est = TRUE, vline.color = "#000000",title = "Busca por Informação")+
  theme_economist()+
  theme(text=element_text(family="Times"))
p + theme(
  panel.background = element_rect(fill = "#f5f5ef"), # bg of the panel
  plot.background = element_rect(fill = "#f5f5ef", color = NA), # bg of the plot
  legend.background = element_rect(fill = "#f5f5ef"), # get rid of legend bg
  legend.box.background = element_rect(fill = "#f5f5ef") # get rid of legend panel bg
)


--------------------------------------------------------------------------------

# Teste com a VD categórica: abaixo da média, na média e  acima da média.
  
# Média:
mean(latinob$polinf)  
count(latinob, polinf)  

# Criar variável com esta medida:
latinob2 <- mutate(latinob, vdcategorica = ifelse(polinf < 3, "Abaixo da Média",
                                     ifelse(polinf == 3, "na Média",
                                     ifelse(polinf == 4, "na Média",
                                     ifelse(polinf > 4,"Acima da Média", "NA"
                                            )))))  

## 32.- Argentina, 76.- Brasil,152.- Chile, 170.- Colombia,
## 188.- Costa Rica, 214.- Rep. Dominicana, 222.- El Salvador, 
## 484.- México, 591.- Panamá, 600.- Paraguay, 604.- Perú, 858.- Uruguay


latinob2$vdcategorica <- as.factor(as.character(latinob2$vdcategorica))

# Logit ordinal hierárquico:
reg2 <- clmm2(location = vdcategorica ~ confgoverno + confpartidos + 
                sexo + niveldem + nivelcorrup + ideologia + idade + educacao + 
                situacaoempreg + renda + confmidia + aprovpresidente, random = pais, 
              data = latinob2, Hess = T)

summary(reg2)

--------------------------------------------------------------------------------
  
# Adicionar variável com o nome dos países:
  latinob2 <- mutate(latinob, nomepais = ifelse(pais == 32, "Argentina",
                                         ifelse(pais == 76, "Brasil",
                                         ifelse(pais == 152, "Chile",
                                         ifelse(pais == 170, "Colômbia",
                                         ifelse(pais == 188, "Costa Rica", 
                                         ifelse(pais == 214, "República Dominicana",
                                         ifelse(pais == 222, "El Salvador",
                                         ifelse(pais == 484, "México",
                                         ifelse(pais == 591, "Panamá",
                                         ifelse(pais == 600, "Paraguai",
                                         ifelse(pais == 604, "Peru",
                                         ifelse(pais == 858, "Uruguai", "NA"
                                         )))))))))))))  

# Transformar variável adicionada em fator:
latinob2$nomepais <- as.factor(as.character(latinob2$nomepais))  

# Frequência do Número de Canais utilizados por país: 
windowsFonts(Times=windowsFont("Times New Roman"))
plot <- ggplot(latinob2, aes(x= polinf,  group=nomepais)) + 
  geom_bar(aes(y = ..prop..), fill = "#4A91B0AA") +
  labs(x ="Número de Canais", y="%", title="Frequência da Quantidade de Canais Utilizados por País")+
  scale_y_continuous(labels=c("0.0"="0%","0.1"="10%", "0.2"="20%", "0.3"="30%"))+
  theme_economist()+
  theme(text=element_text(family="Times"))+
  facet_wrap(~nomepais)

plot + theme(
  plot.background = element_rect(fill = "#FFFFFF", color = NA),
  panel.grid.major = element_line(colour = "#000000AA", size=0.1))

# Transformar variável "polinf" em fator para plotar a média de canais por país:
latinob2$polinf <- as.factor(as.numeric(latinob2$polinf))  

# Média de canais utilizados por país:
windowsFonts(Times=windowsFont("Times New Roman"))
plot2 <- ggplot(latinob2) + 
  geom_bar(aes(reorder(nomepais, -polinf), polinf), stat = "summary", fun.y = "mean", fill="#4A91B0AA") +
  xlab("") +
  ylab("")+
  labs(x="", y="", title="Média de Canais Utilizados por País")+
  theme_economist()+
  coord_flip()+
  theme(text=element_text(family="Times"))

plot2 + theme(
  plot.background = element_rect(fill = "#FFFFFF", color = NA),
  panel.grid.major = element_line(colour = "#000000AA", size=0.1)
)

latinob2 <- read_rds("latinotratada")



--------------------------------------------------------------------------------
  
# Análise LAPOP:
  # Barômetro das Américas (LAPOP):
  
# Agregado:
lapopagregado <- read_dta("2018 LAPOP AmericasBarometer Merge_v1.0_W.dta")
paiseslapop <- c(1,6,11,8,12,15,14,3,13,17,7,21)
# 1 = México
# 6 = Costa Rica
# 8 = Colômbia
# 12 = Paraguai
# 15 = Brasil
# OBS: não tem Venezuela
lapopagregado <- filter(lapopagregado, pais %in% paiseslapop)
lapopagregado$pol1exp[is.na(lapopagregado$pol1exp)] <- 0
lapopagregado$pol1exp[is.na(lapopagregado$pol1)] <- 0  
lapopagregado$exc7new[is.na(lapopagregado$exc7new)] <- 0
lapopagregado$exc7new[is.na(lapopagregado$exc7)] <- 0
lapopagregado <- mutate(lapopagregado, exc7fim = exc7 + exc7new)
lapopagregado <- mutate(lapopagregado, pol1fim = pol1 + pol1exp)
lapopagregado2 <- data.frame("freqnot" = lapopagregado$gi0n, 
                             "apoiosistpol" = lapopagregado$b6,
                             "confmidia" = lapopagregado$b37,
                             "confeleicoes" = lapopagregado$b47a,
                             "confpartidos" = lapopagregado$b21,
                             "confpresidente" = lapopagregado$b21a,
                             "efext1" = lapopagregado$eff1,
                             "efint" = lapopagregado$eff2,
                             "efext2" = lapopagregado$eff10, # mais de 7 mil NAs
                             "interesse" = lapopagregado$pol1fim,
                             "idade" = lapopagregado$q2,
                             "educacao" = lapopagregado$ed,
                             "renda" = lapopagregado$q10new,
                             "pais" = lapopagregado$pais)

# Recodificar a VD (frequência de atenção às notícias) para que o menor valor
# se refira à menor frequência e o maior valor à maior frequência:
lapopagregado2$freqnot <- recode(lapopagregado2$freqnot, 
                                 `1` = 5, `2` = 4, `3` = 3, `4` = 2, 
                                 `5` = 1)
count(lapopagregado2, freqnot)

# Recodificar VI Interesse:
lapopagregado2$interesse <- recode(lapopagregado2$interesse, 
                                   `1` = 4, `2` = 2, `3` = 3, `4` = 1)


lapopagregado2$pais <- as.factor(as.numeric(lapopagregado2$pais))

reg3 <- lmer(freqnot ~ apoiosistpol + confmidia + confpartidos + confpresidente +
               efext1 + efint + interesse + idade + educacao + renda +
               (1 | pais), data = lapopagregado2)

summary(reg3)
r.squaredGLMM(reg3)
summary(lapopagregado2)


--------------------------------------------------------------------------------
  
# Análise GCB:

# VD:
# Q25B – Você já solicitou infos?
  
# VIs:
# Q21DCAR – Oficiais do governo usam os seus poderes para beneficiar as pessoas
# Q21ECAR – Você pode dizer se a pessoa é boa ou ruim pela sua posição política
# Q1A – Confiança no governo
# Q1B – Nas cortes
# Q1C – Na polícia
# Q2 – Nível de problema corrupção
# Q19_A – Frequência fake news espalhada para influenciar a eleição
# Q21ACAR – Políticos devem sempre ouvir os problemas das pessoas
# DEMGENDERFIN – Gênero
# DEMAGEFIN – Idade
# DEMEDU2FIN - Educação
# COUNTRY

basegcb <- read_rds("baselatam.rds")
summary(basegcb)

count(basegcb, baselatam.Q25B)
basegcb$baselatam.Q25B  <- recode(basegcb$baselatam.Q25B, 
                                  "Never" = 1, "Once or twice" = 2, 
                                  "A few times" = 3, "Often" = 4, "Don't know" = 0)

basegcb$baselatam.Q2 <- recode(basegcb$baselatam.Q2, 
                               "No problem at all" = 1, "Fairly small" = 2, 
                               "Quite big" = 3, "A very big problem" = 4, "Don't know" = 0)
count(basegcb, baselatam.Q2)


basegcb$baselatam.Q1A <- recode(basegcb$baselatam.Q1A, 
                               "No trust at all" = 1, "Not a lot of trust" = 2, 
                               "A fair amount of trust" = 3, "A great deal of trust" = 4,
                               "Don't know" = 0)
count(basegcb, baselatam.Q1A)

basegcb$baselatam.Q1B <- recode(basegcb$baselatam.Q1B, 
                                "No trust at all" = 1, "Not a lot of trust" = 2, 
                                "A fair amount of trust" = 3, "A great deal of trust" = 4,
                                "Don't know" = 0)
count(basegcb, baselatam.Q1B)

basegcb$baselatam.Q1C <- recode(basegcb$baselatam.Q1C, 
                                "No trust at all" = 1, "Not a lot of trust" = 2, 
                                "A fair amount of trust" = 3, "A great deal of trust" = 4,
                                "Don't know" = 0)
count(basegcb, baselatam.Q1C)

basegcb$baselatam.Q19_A <- recode(basegcb$baselatam.Q19_A, 
                                "Never" = 1, "Rarely" = 2, 
                                "Occasionally" = 3, "Frequently" = 4,"Very frequently" = 5,
                                "Don't know" = 0)
count(basegcb, baselatam.Q19_A)

basegcb$baselatam.Q21ACAR <- recode(basegcb$baselatam.Q21ACAR, 
                                  "Strongly disagree" = 1, "Tend to disagree" = 2, 
                                  "Neither agree nor disagree" = 3, "Tend to agree" = 4,"Strongly agree" = 5,
                                  "Don't know" = 0)
count(basegcb, baselatam.Q21ACAR)


basegcb$baselatam.DEMEDU2FIN <- recode(basegcb$baselatam.DEMEDU2FIN, 
                                         "No formal education" = 1, "Primary" = 2,
                                       "Secondary" = 3, "Post-secondary" = 4)

count(basegcb, baselatam.DEMEDU2FIN)


basegcb2 <- na_if(basegcb, 0)

basegcb2$baselatam.DEMGENDERFIN <- recode(basegcb2$baselatam.DEMGENDERFIN, 
                                         "Male" = 1, "Female" = 0)

count(basegcb2, baselatam.DEMGENDERFIN)

### !!!! AINDA NÃO FUNCIONA !!! ###
basegcb3 <- data.frame("solicinfo" = basegcb$baselatam.Q25B, 
                             "corrupprob" = basegcb$baselatam.Q2,
                             "confgov" = basegcb$baselatam.Q1A,
                             "confcortes" = basegcb$baselatam.Q1B,
                             "confpolicia" = basegcb$baselatam.Q1C,
                             "fakenews" = basegcb$baselatam.Q19_A,
                             "efext" = basegcb$baselatam.Q21ACAR,
                             "educacao" = basegcb$baselatam.DEMEDU2FIN, 
                             "homem" = basegcb$baselatam.DEMGENDERFIN,
                             "idade" = basegcb$DEMAGEFIN,
                             "pais" = basegcb$baselatam.COUNTRY)

### !!!! !!!! ###


reg4 <- lmer(baselatam.Q25B ~ baselatam.Q2 + baselatam.Q1A + baselatam.Q1B + baselatam.Q1C +
               baselatam.Q19_A + baselatam.Q21ACAR + baselatam.DEMGENDERFIN + 
               baselatam.DEMAGEFIN + baselatam.DEMEDU2FIN +
               (1 | baselatam.COUNTRY), data = basegcb2)
summary(reg4)
r.squaredGLMM(reg4)
