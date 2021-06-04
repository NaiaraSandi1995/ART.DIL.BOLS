# Artigo-Dilma-e-Bolsonaro
DA ONDA RODA A ASCENÇÃO DO CONSERVADORISMO: Análise longitudinal do governo Dilma ao Bolsonaro 
library(haven)
Latinobarometro_2011_eng <- 
  read_sav("C:/Users/Isabel Lima/Downloads/Latinobarometro_2011_eng.sav")
Latinobarometro2016Eng_v20170205 <- 
  read_sav("C:/Users/Isabel Lima/Downloads/Latinobarometro2016Eng_v20170205.sav")

library(tidyverse)

LB_Brasil2016 <- Latinobarometro2016Eng_v20170205 %>%
  filter(IDENPA == 76) 


LB_Brasil2016<-subset(Latinobarometro2016Eng_v20170205,IDENPA==76)
LB_Brasil2011<-subset(Latinobarometro_2011_eng,IDENPA==76)

LB_Brasil2018 <-subset(Latinobarometro_2018_Esp_R_v20190303,IDENPA==76)

save(LB_Brasil2016, file = "LB_Brasil2016.RData")
save(LB_Brasil2011, file = "LB_Brasil2011.RData")
save(LB_Brasil2018, file = "LB_Brasil2018.RData")



LB_BR2016 <-data.frame(LB_Brasil2016$NUMINVES,LB_Brasil2016$P16STGBS, LB_Brasil2016$S8, 
                      LB_Brasil2016$SEXO, LB_Brasil2016$EDAD, LB_Brasil2016$REEDUC_1)
LB_BR2011 <-data.frame(LB_Brasil2011$NUMINVES,LB_Brasil2011$P36ST,
                      LB_Brasil2011$S18,LB_Brasil2011$S16,
                      LB_Brasil2011$S17,LB_Brasil2011$REEDUC1)

names(LB_BR2016)<-c("Ano","Aprovação","Religião","Sexo","Idade","Escolaridade")
names(LB_BR2011)<-c("Ano","Aprovação","Religião","Sexo","Idade","Escolaridade")

save(LB_BR2016, file = "LB_BR2016.RData")
save(LB_BR2011, file = "LB_BR2011.RData")

LB_Brasil <- rbind(LB_BR2011, LB_BR2016)
LB_Brasil <- merge(LB_BR2011, LB_BR2016, all = T)

##Linha para merge IMportante####
LB_Brasil <- plyr::rbind.fill(LB_BR2011, LB_BR2016)

LB_Brasil$Ano <- as.factor(LB_Brasil$Ano)

levels(LB_Brasil$Ano)<-c('2011','2016')

LB_BRA<-LB_Brasil[!is.na(LB_Brasil$Aprovação),]

LB_BR<-LB_BRA[!is.na(LB_BRA$Religião),]
LB_BR$Aprovação<-as.factor(LB_BR$Aprovação)
levels(LB_BR$Aprovação)<-c('Aprova','Não aprova')
LB_BR$Sexo<-as.factor(LB_BR$Sexo)
levels(LB_BR$Sexo)<-c('Homem','Mulher')
LB_BR$Escolaridade<-as.factor(LB_BR$Escolaridade)
levels(LB_BR$Escolaridade)<-c('Analfabeto','Básico incompleto',
                              'Básico completo','Ensino Médio,
                              Técnico incompleto','Ensino Médio, 
                              Técnico completo','Superior incompleto',
                              'Superior completo')
LB_BR$Religião<-as.factor(LB_BR$Religião)
levels(LB_BR$Religião)<-c('Católica','Evangélica sem especificaçãoo',
                          'Evangélica batista','Evangélica metodista',
                          'Evangélica pentecostal','Adventista',
                          'Testemunha de Jeová','Mórmon','Judáica',
                          'Protestante','Cultos afroamericanos,
                          Umbanda, etc.','Crente, não pertence a igreja',
                          'Agnóstico','Ateu','Outra','Nenhuma')
table(LB_BR$Religião)
LB_BR$Religião<-as.factor(LB_BR$Religião)
levels(LB_BR$Religião)<-c('Católica','Evangélica sem especificaçãoo',
                          'Evangélica batista','Evangélica metodista',
                          'Evangélica pentecostal','Adventista',
                          'Testemunha de Jeová','Mórmon','Judáica',
                          'Protestante','Cultos afroamericanos,
                          Umbanda, etc.','Crente, não pertence a igreja',
                          'Agnóstico','Ateu','Outra','Nenhuma')
LB_BR$Idade<-as.factor(LB_BR$Idade)
table(LB_BR$Religião)
LB_BR_11<-subset(LB_BR,Ano=="2011")
LB_BR_16<-subset(LB_BR,Ano=="2016")

library(ggplot2)
library(scales)

ggplot(LB_BR_11, aes(x = factor(Aprovação), y = (..count..)/sum(..count..))) +  
  geom_bar(width=0.5,color=c("#400819","#719CB0"), fill=c("#400819","#719CB0"))+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Governo Dilma: Aprovação em 2011", y = "Porcentagem", x = "Aprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
    )

# Modificando e fazendo gráfico 1#### 

LB_BR2011 <-LB_BR2011[!is.na(LB_BR2011$Aprovação),]
LB_BR2011$Aprovação<-as.factor(LB_BR2011$Aprovação)
levels(LB_BR2011$Aprovação)<-c('Aprova','Não aprova')
table(LB_BR2011$Aprovação)

gráfico1 <- ggplot(LB_BR2011, aes(x = factor(Aprovação), y = (..count..)/sum(..count..))) +  
  geom_bar(width=0.5)+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Governo Dilma: Aprovação em 2011", y = "Porcentagem", x = "Aprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
    
  )

gráfico1+ theme_classic()


# Modificando e fazendo gráfico 2#### 
LB_BR2016 <-LB_BR2016[!is.na(LB_BR2016$Aprovação),]
LB_BR2016$Aprovação<-as.factor(LB_BR2016$Aprovação)
levels(LB_BR2016$Aprovação)<-c('Aprova','Não aprova')

gráfico2 <- ggplot(LB_BR2016, aes(x = factor(Aprovação), y = (..count..)/sum(..count..))) +  
  geom_bar(width=0.5)+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Governo Dilma: Aprovação em 2016", y = "Porcentagem", x = "Aprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
    
  )

gráfico2 + theme_classic()


ggplot(LB_BR_16, aes(x = factor(Aprovação), y = (..count..)/sum(..count..))) +  
  geom_bar(width=0.5,color=c("#400819","#719CB0"), fill=c("#400819","#719CB0"))+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), stat = "count", vjust = -0.25) +
  scale_y_continuous(labels = percent) +
  labs(title = "Governo Dilma: Aprovação em 2016", y = "Porcentagem", x = "Aprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
    
  )

tabelarelano<-table(LB_BR$Ano, LB_BR$Religião)
tabelarelanopor<-prop.table(tabelarelano,1)*100
tabelarelanopor

# Católica Evangélica sem especificaçãoo Evangélica batista Evangélica metodista
# 2011 64.9059982                     0.8057296          0.3581021            0.4476276
# 2016 58.3150985                     0.4376368          0.8752735            0.5470460
# 
# Evangélica pentecostal Adventista Testemunha de Jeová     Mórmon    Judáica Protestante
# 2011              0.1790510  9.8478066           1.7905103  0.0000000  7.6096688   0.3581021
# 2016              0.5470460 14.3326039           0.7658643  0.1094092 10.3938731   0.8752735
# 
# Cultos afroamericanos,\n                          Umbanda, etc.
# 2011                                                       0.3581021
# 2016                                                       0.6564551
# 
# Crente, não pertence a igreja  Agnóstico       Ateu      Outra    Nenhuma
# 2011                     0.1790510  2.5962399 10.5640107  0.0000000  0.0000000
# 2016                     0.2188184  2.4070022  9.5185996  0.0000000  0.0000000




ggplot(LB_BR, aes(x = Ano, y = Religião)) +  
  geom_bar(width=0.5,color="red2", fill="red2")+
  scale_y_continuous(labels = percent) +
  labs(title = "Governo Dilma: Aprovação em 2011", y = "Porcentagem", x = "Aprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
    
  )

barplot(tabelarelanopor, beside = TRUE,
        col = c("#AD0805","#400819"),
        legend=rownames(tabelarelanopor),
        main ="Religião x Ano")
ggplot(LB_BR, aes(x = Religião, y = (..count..)/sum(..count..), fill=Ano))+
  geom_bar(color="black", position=position_dodge())+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust=1.6, color="black",
            position = position_dodge(0.9), size=3.5) +
  scale_y_continuous(labels = percent) +
  labs(title = "Religião: 2011 x 2016", y = "Porcentagem", x = "Religião")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5),
  )

aprovsex11<-table(LB_BR_11$Sexo, LB_BR_11$Aprovação)
aprovsex11pr<-prop.table(aprovsex11,1)*100
aprovsex11pr
barplot(aprovsex11pr, beside = TRUE,
        legend=rownames(aprovsex11),
        main ="2011:Aprovação x Sexo")

aprovsex16<-table(LB_BR_16$Sexo, LB_BR_16$Aprovação)
aprovsex16pr<-prop.table(aprovsex16,1)*100
aprovsex16pr
barplot(aprovsex16pr, beside = TRUE,
        legend=rownames(aprovsex16),
        main ="2016:Aprovação x Sexo")

aprovsex<-table(LB_BR$Sexo,LB_BR$Aprovação)
aprovsexpor<-prop.table(aprovsex,1)*100
aprovsexpor
OBJ2 <- ggplot(data = LB_BR, aes(x=Ano)) +
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(Aprovação~Sexo)

OBJ2 + theme_classic()

aprovrel11<-table(LB_BR_11$Aprovação, LB_BR_11$Religião)
barplot(aprovrel11por, beside = TRUE,
        col = c("red","blue"),
        legend=rownames(aprovrel11),
        main ="2011:Aprovação x Religião")
aprovrel11por<-prop.table(aprovrel11,1)*100
aprovrel11por
aprovrel16<-table(LB_BR_16$Aprovação, LB_BR_16$Religião)
aprovrel16por<-prop.table(aprovrel16,2)*100
aprovrel16por
barplot(aprovrel16por, beside = TRUE,
        col = c("red","blue"),
        legend=rownames(aprovrel16),
        main ="2016:Aprovação x Religião")
aproved11<-table(LB_BR_11$Aprovação, LB_BR_11$Escolaridade)
aproved11por<-prop.table(aproved11,1)*100
aproved11por
barplot(aproved11por, beside = TRUE,
        col = c("red","blue"),
        legend=rownames(aproved11por),
        main ="2011:Aprovação x Escolaridade")
barplot(aproved16por, beside = TRUE,
        col = c("red","blue"),
        legend=rownames(aproved16por),
        main ="2016:Aprovação x Escolaridade")
aproved16<-table(LB_BR_16$Aprovação, LB_BR_16$Escolaridade)
aproved16por<-prop.table(aproved16,1)*100
aproved16por

# Analfabeto Básico incompleto Básico completo
# Aprova       4.545455         40.151515        6.818182
# Não aprova   6.153846         31.846154        7.384615
# 
# Ensino Médio,\n                              Técnico incompleto
# Aprova                                                           12.121212
# Não aprova                                                       10.923077
# 
# Ensino Médio, \n                              Técnico completo
# Aprova                                                          22.727273
# Não aprova                                                      24.307692
# 
# Superior incompleto Superior completo
# Aprova                5.681818          7.954545
# Não aprova            9.384615         10.000000
# > 

boxplot(LB_BR_11$Idade~LB_BR_11$Aprovação, col=c ("red","blue"))
boxplot(LB_BR_16$Idade~LB_BR_16$Aprovação, col=c ("red","blue"))

ggplot(data = LB_BR, aes(x=Ano)) +
  geom_bar(stat="count",position = position_dodge())+
  facet_grid(Aprovação~Religião)
LB_BR


#Regressão####
LB_Brasil2016$esco

Modelo11<-glm(Aprovação~ Religiões +Sexo+ Idade + Escolaridade,
              data=LB_Brasil2016,
              family="binomial")
summary(Modelo11)
tab_model (Modelo11, show.ci = F, auto.label = T,
           show.se= T, collapse.se = T, wrap.labels= 60, p.style = "numeric")

Modelo16<-glm(Aprovação~Religião+Sexo+as.numeric(Idade)+
                Escolaridade,data=LB_BR2016, 
              family="binomial")
summary(Modelo16)

Modelo1<-glm(Aprovação~Religião+Sexo+Escolaridade+as.numeric(Idade)+Ano,
             data=LB_BR,family="binomial")
summary(Modelo1)

Modelo2<-glm(Aprovação~Religião+Sexo+Escolaridade+as.numeric(Idade)+
               Ano+Ano*Religião,
             data=LB_BR,family="binomial")

Modelo3<-glm(Aprovação~Religião+Sexo+Escolaridade+as.numeric(Idade)+Ano+
               Ano*as.numeric(Idade),
             data=LB_BR,family="binomial")
summary(Modelo3)

library(dplyr)
LB_Brasil2011 <- LB_Brasil2011 %>% 
  mutate_at("Aprovação", funs(recode(., "Aprova"=1,
                                     "Não aprova"=0, .default = NaN)))
Modelo_A<-glm(Aprovação~Religião+Sexo+Escolaridade+as.numeric(Idade)+Ano+
                Ano*Religião,data=LB_BR,family="binomial")
summary(Modelo_A)

library(stargazer)
stargazer(Modelo11, Modelo16, Modelo1, Modelo_A, type = "text", 
          out = "TABELAAAA.xlsx")

summary(Modelo)
confint(Modelo_A)
exp(coef(Modelo_A))
exp(cbind(OR = coef(Modelo_A), confint(Modelo_A)))

novosdados1<-data.frame(Sexo="Mulher", Religião = "Evangélica batista", 
                        Escolaridade="Básico completo", Idade="40", Ano="2016")
novosdados1
novosdados1$logit <-predict(Modelo_A, newdata = novosdados1, type = "response")
novosdados1$logit

novosdados1<-data.frame(Sexo="Mulher", Religião = "Evangélica pentecostal", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados1
novosdados1$logit <-predict(Modelo_A, newdata = novosdados1, type = "response")
novosdados1$logit

novosdados2<-data.frame(Sexo="Homem", Religião = "Evangélica pentecostal", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados2
novosdados2$logit <-predict(Modelo_A, newdata = novosdados2, type = "response")
novosdados2$logit

novosdados1<-data.frame(Sexo="Mulher", Religião = "Evengélica sem especificação", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados1
novosdados1$logit <-predict(Modelo_A, newdata = novosdados1, type = "response")
novosdados1$logit

novosdados2<-data.frame(Sexo="Homem", Religião = "Evangélica sem especificaçãoo", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados2
novosdados2$logit <-predict(Modelo_A, newdata = novosdados2, type = "response")
novosdados2$logit

novosdados1<-data.frame(Sexo="Mulher", Religião = "Agnóstico/Ateu", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados1
novosdados1$logit <-predict(Modelo_A, newdata = novosdados1, type = "response")
novosdados1$logit

novosdados2<-data.frame(Sexo="Homem", Religião = "Agnóstico/Ateu", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados2
novosdados2$logit <-predict(Modelo_A, newdata = novosdados2, type = "response")
novosdados2$logit

novosdados1<-data.frame(Sexo="Mulher", Religião = "Católica", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados1
novosdados1$logit <-predict(Modelo_A, newdata = novosdados1, type = "response")
novosdados1$logit

novosdados2<-data.frame(Sexo="Homem", Religião = "Católica", 
                        Escolaridade="Ensino Médio, Técnico incompleto", 
                        Idade="32", Ano="2016")
novosdados2
novosdados2$logit <-predict(Modelo_A, newdata = novosdados2, type = "response")
novosdados2$logit

library(glm2)
library(dplyr)
levels(LB_BR$Aprovao)<-c('Não aprova','Aprova')
aprovtab<-table(LB_BR$Aprovação)
aprovtab
library(caTools)
set.seed(123)
split<-sample.split(LB_BR$Aprovação,SplitRatio=0.53)
split
train<-subset(LB_BR, split=="TRUE")
test<-subset(LB_BR, split=="FALSE")
nrow(train)
nrow(test)

Modelo_Train<-glm(Aprovação~Religião+Sexo+Escolaridade+as.numeric(Idade)+
                    Ano+Ano*Religião,
                  family=binomial(link = "logit"),data = train)
summary(Modelo_Train)

predictTrain=predict(Modelo_Train, type="response")
summary(predictTrain)
tapply(predictTrain, train$Aprovação, mean)
table(train$Aprovação, predictTrain>0.5)

##Gráficos de religião x aprovação####





#2016####

tab2 <- table(LB_Brasil2016$Aprovação, LB_Brasil2016$Religiões)
tab2
# Católica Evangélica Outras_Religiões Ateu/Agnóstico
# Aprova          144         83                9             28
# Não aprova      389        163               29             69

tab2 <- prop.table(tab2, margin = 1)
tab2 * 100
#             Católica Evangélica Outras_Religiões Ateu/Agnóstico
# Aprova     54.545455  31.439394         3.409091      10.606061
# Não aprova 59.846154  25.076923         4.461538      10.615385

tab3 <- table(LB_Brasil2016$Religiões, LB_Brasil2016$Aprovação)
tab3
tab3 <- prop.table(tab3, margin = 1)
tab3 * 100

#                  Aprova Não aprova
# Católica         27.01689   72.98311
# Evangélica       33.73984   66.26016
# Outras_Religiões 23.68421   76.31579
# Ateu/Agnóstico   28.86598   71.13402

###
# Outro modelo de regressão, 
# Agora com mais variáveis 

#2016
#Recodificações ####
#Ano
table(LB_Brasil2016$NUMINVES)

#Aprovação
table(LB_Brasil2016$P16STGBS)
summary(LB_Brasil2016$P16STGBS)

#Não usei?
LB_Brasil2016 <-LB_Brasil2016[!is.na(LB_Brasil2016$P16STGBS),]

LB_Brasil2016$Aprovação <-as.factor(LB_Brasil2016$P16STGBS)
levels(LB_Brasil2016$Aprovação)<-c('Aprova','Não aprova')
table(LB_Brasil2016$Aprovação)

#Religião
table(LB_Brasil2016$S8)
LB_Brasil2016$Religiões <- as.factor(LB_Brasil2016$S8)
LB_Brasil2016$Religiões <- recode(LB_Brasil2016$Religiões, 
                     "Católica" <- 1, "Evangélica" <- c(2,3,4,5,6,10),
                "Outras_Religiões" <- c(7, 8, 9,11, 96), "Ateu/Agnóstico" <- c(13,14,97))
table(LB_Brasil2016$Religiões)


#Frequência religiosa
# 1.- Muy practicante
# 2.- Practicante
# 3.- No muy practicante
# 4.- No practicante
table(LB_Brasil2016$S8A)
#1   2   3   4 
#166 288 285  78 
LB_Brasil2016$FreqRelig <- as.factor(LB_Brasil2016$S8A)
LB_Brasil2016$FreqRelig <- recode(LB_Brasil2016$FreqRelig, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1 )
table(LB_Brasil2016$FreqRelig)
# 0   1   2   3 
# 78 285 288 166 
LB_Brasil2016$FreqRelig <- as.numeric(LB_Brasil2016$FreqRelig)


#Sexo
table(LB_Brasil2016$SEXO)
LB_Brasil2016$SEXO <- as.factor(LB_Brasil2016$SEXO)
LB_Brasil2016$Sexo <- recode (LB_Brasil2016$SEXO, "Homem" <- 1, "Mulher" <-2 )
table(LB_Brasil2016$Sexo)

#IDade
table(LB_Brasil2016$Idade)
LB_Brasil2016$Idade <- as.numeric(LB_Brasil2016$EDAD)

#Escolaridade - quantitativa discreta
table(LB_Brasil2016$REEDUC_1)
#1   2   3   4   5   6   7 
#52 313  67 104 218  77  86 
LB_Brasil2016$Escolaridade <- as.numeric(LB_Brasil2016$REEDUC_1)

#ideologia _ Escala de 0-Esqueda a 10 - Direita
table(LB_Brasil2016$P17ST)
# 0   1   2   3   4   5   6   7   8   9  10 
# 91  31  48  61  68 214  69  58  51  25  97 
LB_Brasil2016$Ideologia <- as.numeric(LB_Brasil2016$P17ST)


save(LB_Brasil2016, file = "LB_Brasil2016.RData")

Model1<-glm(Aprovação~ Religiões  + FreqRelig+ Sexo+ Idade 
              +Escolaridade ,
              data=LB_Brasil2016,
              family="binomial")
summary(Model1)

tab_model (Model1, show.ci = F, auto.label = T,
           show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

#Interpretação:
#*Temos que exponenciar os resultados
#*A maneira mais fácil de fazer isso é a seguinte:
#*  Razão de chance - 1 = X *100

#Ideologia
0.66-1
#[1] -0.16
-0.16 *100

coefplot(Model1, intercept = F, outerCI = F)

#2011

#Ano
table(LB_Brasil2011$NUMINVES)

#Aprovação
table(LB_Brasil2011$P36ST)
summary(LB_Brasil2011$P36ST)
#Não usei?
LB_Brasil2011 <-LB_Brasil2011[!is.na(LB_Brasil2011$P36ST),]

LB_Brasil2011$Aprovação <-as.factor(LB_Brasil2011$P36ST)
levels(LB_Brasil2011$Aprovação)<-c('Aprova','Não aprova')
table(LB_Brasil2011$Aprovação)

#Religião
table(LB_Brasil2011$S18)
summary(LB_Brasil2011$S18)

LB_Brasil2011 <-LB_Brasil2011[!is.na(LB_Brasil2011$S18),]
LB_Brasil2011$S18 <- as.factor(LB_Brasil2011$S18)
LB_Brasil2011$S18 <- as.character(LB_Brasil2011$S18)

LB_Brasil2011$Religiões<- recode(LB_Brasil2011$S18, 
                                  "Católica" <- 1, "Evangélica" <- c(2,3,5,6,10),
                                  "Outras_Religiões" <- c(7, 8,11, 96), 
                                  "Ateu/Agnóstico" <- c(13,14,97))
table(LB_Brasil2011$Religiões)


#Frequência religiosa
# 1.- Muy practicante
# 2.- Practicante
# 3.- No muy practicante
# 4.- No practicante
table(LB_Brasil2011$S18.A)
#1   2   3   4 
#166 288 285  78 
LB_Brasil2011$FreqRelig <- as.factor(LB_Brasil2011$S18.A)
LB_Brasil2011$FreqRelig <- recode(LB_Brasil2011$FreqRelig, 0 <- 4, 1 <- 3, 2 <- 2, 3 <- 1 )
table(LB_Brasil2011$FreqRelig)
# 0   1   2   3 
# 78 285 288 166 
LB_Brasil2011$FreqRelig <- as.numeric(LB_Brasil2011$FreqRelig)


#Sexo
table(LB_Brasil2011$SEXO)
LB_Brasil2011$SEXO <- as.factor(LB_Brasil2011$SEXO)
LB_Brasil2011$Sexo <- recode (LB_Brasil2011$SEXO, "Homem" <- 1, "Mulher" <-2 )
table(LB_Brasil2011$Sexo)

#IDade
table(LB_Brasil2011$EDAD)
LB_Brasil2011$Idade <- as.numeric(LB_Brasil2011$EDAD)

#Escolaridade - quantitativa discreta
table(LB_Brasil2011$REEDUC_1)
#1   2   3   4   5   6   7 
#52 313  67 104 218  77  86 
LB_Brasil2011$Escolaridade <- as.numeric(LB_Brasil2011$REEDUC1)

#ideologia _ Escala de 0-Esqueda a 10 - Direita
table(LB_Brasil2011$P76ST)
#Escala Izquierda-Derecha

# 0   1   2   3   4   5   6   7   8   9  10 
# 91  31  48  61  68 214  69  58  51  25  97 
LB_Brasil2011$Ideologia <- as.numeric(LB_Brasil2011$P76ST)


save(LB_Brasil2011, file = "LB_Brasil2011.RData")

Model2<-glm(Aprovação~ Religiões  + FreqRelig+ Sexo+ Idade 
            +Escolaridade,
            data=LB_Brasil2011,
            family="binomial")
summary(Model1)

tab_model (Model2, show.ci = F, auto.label = T,
           show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

#2019 Bolsonaro####

# Variável dependente: 
#   M1. Falando em geral do atual governo, como o(a) sr./sra. avalia o trabalho que o Presidente Jair Bolsonaro está realizando [Ler alternativas]
# (1) Muito bom (2) Bom (3) Nem bom, nem mau (regular)
# (4) Mau (5) Muito mau (péssimo)
# (888888) Não sabe [NÃO LER] (988888) Não responde [NÃO LER]

#Manterei a variável dependente como está, porque queremos testar a reprovação, 
#Para identificar quem é que mais reprova esse presidente. 

table(X2019$m1)
# 1   2   3   4   5 
# 194 576 514  83  83

X2019$AprovaçãoB <- as.numeric(X2019$m1)

#Sexo
#1- homem e 2- mulher
X2019$Sexo <- as.factor(X2019$q1)

#Idade (Quantitativa contínua)
X2019$Idade <- as.numeric(X2019$q2)

#Escolaridade (quantitativa discreta)
X2019$Escolaridade <- as.numeric(X2019$ed)

#Religião

# Q3CN. Qual a sua religião, se tiver? [Não leia as alternativas]
# 
# (01) Católico [Siga]
# (02) Protestante Tradicional ou Evangélica não pentecostal
#(Batista, Calvinista, Luterano,Metodista, Presbiteriano, Discípulo de Cristo, 
#Anglicano, Episcopal, Igreja Cristã Reformada,
#   Igreja Morava, Menonita, Irmãos em Cristo; Igreja do Nazareno) [Siga]
# (03) Outra religião oriental não cristã (Muçulmano, Budista, Induísta, Taoísta, Confuciano,
#  Baha’i) [Siga]
# (05) Evangélica pentecostal (Pentecostal, Igreja de Deus, Assembleias de Deus, Igreja
#    Universal do Reino de Deus, Igreja Quadrangular, Igreja de Cristo, Congregação Cristã,
#   Adventista, Adventista de Sétimo Dia, Sara Nossa Terra, Carismático não Católico, Bola de
#  Neve, etc) [Siga]
# (07) Religiões Tradicionais ou nativas (Santeria, Candomblé, Umbanda, Vodu, Rastafari,
#    religiões mayas, Santo Daime, Esotérica) [Siga]
# (1501) Espírita kardecista [Siga]
# (04) Nenhuma (Acredita em uma entidade suprema mas não pertence à religião nenhuma)
# [Siga]
# (11) Agnóstico ou ateu/não acredita em Deus [VÁ PARA Q5B]
# (77) Outra [Siga]
# (888888) Não sabe [NÃO LER] [Siga]
# (988888) Não responde [NÃO LER] [Siga]

#Recodificação
# 1- Católica (1), 2- EvangélicoNãoPet.(2), 3- EvangélicoPet.(5), 4- Outras religioões 
# (3,7,1501,77), 5- Agnóstico/Ateu (4,11)

table(X2019$q3cn)
#   1    2    3    4    5    7   11   77 1501 
# 746  140    3  153  323   22   25   34   36 

X2019$Religião <- as.factor(X2019$q3cn)
X2019$Religião <- recode (X2019$Religião, 'Católica' <- 1, 'Protestante' <- c(2,5), 
               'Outras.Religiões' <- c(3,7,1501,77),
                          'Agnóstico/Ateu'<- c(4,11))
table(X2019$Religião)
# Católica      Evang.N.Pet        Evang.Pet Outras.Religiões   Agnóstico/Ateu 
# 746              140              323               95              178 

#Regressão Bolsaonaro####
modelBol <- lm(AprovaçãoB ~ Religião + Sexo + Idade + Escolaridade, data = X2019)

tab_model(modelBol, show.ci = F, auto.label = T,
          show.se= F, collapse.se = F, wrap.labels= 60, p.style = "stars")

obj1 <- coefplot(modelBol, title = "Reprovação ao governo Bolsonaro,2019",
                 xlab = "Valor",
                 ylab = "Coefficient",
                 color = "black",
                 outerCI = T,
                 innerCI = 1,
                 intercept = F)

obj1 + theme_classic() + geom_point(size=3, pch=21, fill="red",
                                    alpha=105) 


library(olsrr)
ols_vif_tol(modelBol)

par(mfrow=c(2,2))
plot(modelBol)

X2019$AprovaçãoB

obj <- ggplot(X2019, aes(x = factor(AprovaçãoB), 
                         y = (..count..)/sum(..count..), 
                         )) +  
  geom_bar(width=0.8)+
  geom_text(aes(y = ((..count..)/sum(..count..)), 
                label = scales::percent((..count..)/sum(..count..))), 
            stat = "count", vjust = -0.25) 

obj  + 
  labs(title = "Escala de reprovação Bolsonorabo, 2019", 
       y = "Porcentagem", x = "Escala de reprovação
    1- Aprovação a 5 - Reprovação")+
  theme(
    axis.text.y=element_blank(), axis.ticks=element_blank(),
    axis.title.y=element_text(), plot.title = element_text(hjust = 0.5)) +
  theme_classic()
