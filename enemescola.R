#H0 a classe socioeconomica não influencia nas médias
#H1 a classe socioeconomica influencia nas médias
setwd("C:/Users/rianb/OneDrive/Área de Trabalho/Catalhuyuk/Estudos/R/Estudo Enem")
db = read.csv("enemescola.csv",sep=";",stringsAsFactors = TRUE)


library(sqldf)
require(caret)
require(dplyr)
require(reshape2)

#selecionando apenas os anos de 2013 a 2015
dbano = subset(db,db$NU_ANO>=2013)

#Deixando apenas colunas que vou utilizar na análise exploratória(factors)
dbex = dbano[,-c(1,2,4,5,6,7,11,12,13,19,20,23,24,27)]

#Criando uma nota total e eliminando as especificas

dbex$NotaTotal = rowMeans(dbex[,5:9])
dimnames(dbex)
dbex = dbex[,-5:-9]

#Agora renomeando as colunas
colnames(dbex) = c("UF","TipoEscola","Localizacao","Porte","INSE","FormacaoDocente","TxReprovacao","TxAbandono","Nota")
#Entendendo os dados
str(dbex)


###Removendo NA's e valores ausentes
dbnaus = dbex
for(i in 1:ncol(dbex)){
  if(anyNA(dbex[,i])){
    dbnaus = na.omit(dbex)
  }
}
for(i in 1:nrow(dbnaus)){
  if(dbnaus[i,5]==""){
    dbnaus[i,5]=5
  }
}
dbnaus$INSE = as.character(dbnaus$INSE)
dbnaus[is.na(dbnaus)] <- "Indefinido"
dbnaus$INSE = as.factor(dbnaus$INSE)

###Transofrmando as colunas numericas para factor

#Fatorando o natureza juridica

dbfactorTipo=dbnaus

for(i in 1:nrow(dbfactorTipo)){
  if(dbfactorTipo[i,2]==1){
    dbfactorTipo[i,2]="Federal"
  }
  if(dbfactorTipo[i,2]==2){
    dbfactorTipo[i,2]="Estadual"
  }
  if(dbfactorTipo[i,2]==3){
    dbfactorTipo[i,2]="Municipal"
  }
  if(dbfactorTipo[i,2]==4){
    dbfactorTipo[i,2]="Privada"
  }
}
dbfactorTipo$TipoEscola = as.factor(dbfactorTipo$TipoEscola)
str(dbfactorTipo)
summary(dbfactorTipo)

#Fatorando o localizacao

dbfactorLoc = dbfactorTipo


for(i in 1:nrow(dbfactorLoc)){
  if(dbfactorLoc[i,3]==1){
    dbfactorLoc[i,3]="Urbana"
  }
  if(dbfactorLoc[i,3]==2){
    dbfactorLoc[i,3]="Rural"
  }
}
dbfactorLoc$Localizacao = as.factor(dbfactorLoc$Localizacao)
str(dbfactorLoc)
summary(dbfactorLoc)

#Fatorando tx de abandono

dbfactorAb = dbfactorLoc

ab1 = summary(dbfactorAb$TxAbandono)[[3]]
ab3 = summary(dbfactorAb$TxAbandono)[[5]]
for(i in 1:nrow(dbfactorAb)){
  if(dbfactorAb[i,8]<ab1){
    dbfactorAb[i,8]="Baixa"
  }
  else{
      if(dbfactorAb[i,8]>=ab1&dbfactorAb[i,8]<ab3){
        dbfactorAb[i,8]="Media"

      }else{
        if(dbfactorAb[i,8]>=ab3){
          dbfactorAb[i,8]="Alta"
          
        }
    }}}
  
dbfactorAb$TxAbandono = as.factor(dbfactorAb$TxAbandono)
summary(dbfactorAb)

#Fatorando o tx de reprovacao

dbfactorRep = dbfactorAb
re1 = summary(dbfactorRep$TxReprovacao)[[2]]
re3 = summary(dbfactorRep$TxReprovacao)[[5]]
sre1 = subset(dbfactorRep,TxReprovacao<re1)
sre2 = subset(dbfactorRep,TxReprovacao>=re1&TxReprovacao<re3)
sre3 = subset(dbfactorRep,TxReprovacao>=re3)

for(i in 1:nrow(sre1))
  sre1[i,7]="Baixa"
for(i in 1:nrow(sre2))
  sre2[i,7]="Media"
for(i in 1:nrow(sre3))
  sre3[i,7]="Alta"
dbfactorRep = rbind(sre1,sre2,sre3)

dbfactorRep$TxReprovacao = as.factor(dbfactorRep$TxReprovacao )
summary(dbfactorRep)

#Fatorando o porte

dbfactorPo = dbfactorRep

po1 = summary(dbfactorPo$Porte)[[2]]
po3 = summary(dbfactorPo$Porte)[[5]]
spo1 = subset(dbfactorPo,Porte<po1)
spo2 = subset(dbfactorPo,Porte>=po1&Porte<po3)
spo3 = subset(dbfactorPo,Porte>=po3)

for(i in 1:nrow(spo1))
  spo1[i,4]="Pequeno"
for(i in 1:nrow(spo2))
  spo2[i,4]="Medio"
for(i in 1:nrow(spo3))
  spo3[i,4]="Grande"
dbfactorPo = rbind(spo1,spo2,spo3)

dbfactorPo$Porte = as.factor(dbfactorPo$Porte )
summary(dbfactorPo)

# Fatorando o formaçao docente

dbfactor = dbfactorPo
firstqr = summary(dbfactor$FormacaoDocente)[[2]]
thirdqr = summary(dbfactor$FormacaoDocente)[[5]]
for(i in 1:nrow(dbfactor)){
  if(dbfactor[i,6]<firstqr){
    dbfactor[i,6]="Baixa"
  }
  else{
    if(dbfactor[i,6]>=firstqr&dbfactor[i,6]<thirdqr){
      dbfactor[i,6]="Media"
    }else{
      if(dbfactor[i,6]>=thirdqr){
        dbfactor[i,6]="Alta"
      }}}}
dbfactor$FormacaoDocente = as.factor(dbfactor$FormacaoDocente)

str(dbfactor)
summary(dbfactor)


#selecionando apenas 10 estados com maior ocorrencia

result_set <- sqldf('select UF, count(UF) from dbfactor group by UF order by count(UF) desc limit 10')

dbfac <- dbfactor[dbfactor$UF %in% result_set$UF, ]
dbfac$UF = as.character(dbfac$UF)
dbfac$UF = as.factor(dbfac$UF)

#randomizando o dataset

dbfac <- dbfac[order(runif(nrow(dbfac))),]

###TREINANDO O ROBO

# Creating training and testing samples
indice = createDataPartition(y=dbfac$Nota,p = 0.75, list = FALSE)
df_treino <- dbfac[indice,]
df_teste <- dbfac[-indice,]

#Training
model = aov(Nota~.,data = df_treino)

#Making predictions
prev = fitted(model)

#Adding predictions to the dataset
df_treino$Previsto <- predict(model, newdata = df_treino)
View(df_treino)

showTrain = df_treino[,c(9,10)]
showTrain = melt(showTrain)


ggplot(data = showTrain,aes(x=value,fill = variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Training results")+ labs(fill = "variable")

# Testing
df_teste$Previsto <- predict(model, newdata = df_teste)

showTest = df_teste[,c(9,10)]
showTest = melt(showTest)


ggplot(data = showTest,aes(x=value,fill=variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Testing results")


resultado = sqrt(mean((df_teste$Nota - df_teste$Previsto)^2))
resultado




####OTIMIZANDO O BANCO DE DADOS

summary(dbfac)
summary(balance)

balance = subset(dbfac,(TipoEscola=="Estadual"|TipoEscola=="Privada")&(Localizacao=="Urbana")&(INSE=="Indefinido"))
dbfac = anti_join(dbfac,balance)
dbbal = balance[-order(runif(10000)),]

dbbalance1 = rbind(dbfac,dbbal)

summary(dbbalance1)
summary(dbfactor)



###Treinando e testando o robo tentativa 2

indice1 = createDataPartition(y=dbbalance1$Nota,p = 0.75, list = FALSE)
df_treino1 <- dbfac[indice1,]
df_teste1 <- dbfac[-indice1,]

#Training
model1 = aov(Nota~.,data = df_treino1)

#Making predictions
prev1 = fitted(model1)

#Adding predictions to the dataset
df_treino1$Previsto <- predict(model1, newdata = df_treino1)
View(df_treino1)

showTrain1 = df_treino1[,c(9,10)]
showTrain1 = melt(showTrain1)


ggplot(data = showTrain1,aes(x=value,fill = variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Training results")+ labs(fill = "variable")

# Testing
df_teste1$Previsto <- predict(model1, newdata = df_teste1)

showTest1 = df_teste1[,c(9,10)]
showTest1 = melt(showTest1)


ggplot(data = showTest1,aes(x=value,fill=variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Testing results")


resultado1 = sqrt(mean((df_teste1$Nota - df_teste1$Previsto)^2))
resultado1
resultado


###Otimizando a melhor tentativa 3
melhor = resultado1
optim = 10000
y = c()
x = c()
j=0
df_test0=c()
set.seed(33)
for (otimo in seq(2000,to=21000,by=1000)) {


dbbal2 = balance[-order(runif(otimo)),]
dbbalance2 = rbind(dbfac,dbbal2)
indice2 = createDataPartition(y=dbbalance2$Nota,p = 0.75, list = FALSE)
df_teste2 <- dbfac[-indice2,]
df_treino2 <- dbfac[indice2,]
model2 = aov(Nota~.,data = df_treino2)
prev2 = fitted(model2)
df_treino2$Previsto <- predict(model2, newdata = df_treino2)
df_teste2$Previsto <- predict(model2, newdata = df_teste2)
resultado2 = sqrt(mean((df_teste2$Nota - df_teste2$Previsto)^2))
y[j]=resultado2
x[j]=otimo
j=j+1

if(resultado2<melhor){
  optim=otimo
  melhor=resultado2
  df_test0=df_teste2
}
}



###Resultado otimizado
showTrain2 = df_treino2[,c(9,10)]
showTrain2 = melt(showTrain2)

ggplot(data = showTrain2,aes(x=value,fill = variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Training results")+ labs(fill = "variable")

showTest2 = df_test0[,c(9,10)]
showTest2 = melt(showTest2)


ggplot(data = showTest2,aes(x=value,fill=variable))+geom_density(alpha=0.5)+theme_dark()+ggtitle("Testing results")

resultado2
resultado2 = sqrt(mean((df_test0$Nota - df_test0$Previsto)^2))
"Resultado 3"
resultado2/mean(dbfactor$Nota)
"Resultado 2"
resultado1/mean(dbfactor$Nota)
"Resultado 1"
resultado/mean(dbfactor$Nota)


