
require(dplyr)
require(tidyverse)
require(tidyr)
require(ggplot2)
require(gridExtra)
require(ggpubr)
require(readxl)
require(haven)
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
setwd()
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


polinfsolo <- c(latinob4$polinf)
latinob4 <-  na.omit(latinob) 

# Testando pressupostos:
modellinearity <- plot(resid(reg))
latinob4$reg.res<- residuals(reg) #extracts the residuals and places them in a new column in our original data table
latinob4$abs.reg.res <-abs(latinob4$reg.res) #creates a new column with the absolute value of the residuals
latinob4$reg.res2 <- latinob4$abs.reg.res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.reg <- lm(reg.res2 ~ pais, data=latinob4) #ANOVA of the squared residuals
anova(Levene.reg) #displays the results
                               

--------------------------------------------------------------------------------

# Teste com a VD categórica: abaixo da média, na média e  acima da média.
  
# Média:
mean(latinob$polinf)  
count(latinob, polinf)  

# Criar variável com esta medida:
latinob2 <- mutate(latinob2, vdcategorica = ifelse(polinf < 3, "Abaixo da Média",
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
  
# Criar nova base adicionando variável com o nome dos países:
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

