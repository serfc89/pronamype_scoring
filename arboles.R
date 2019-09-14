library(tidyverse)
library(rpart)
library(rpart.plot)
library(schoRsch)
library(gmodels)
library(caret)


#Misma muestr para todos
muestra <- sample(1:dim(arbol2)[1], round(0.3*dim(arbol2)[1]))
ttesting <- arbol2[muestra,]
taprendizaje <- arbol2[-muestra,]

####Modelo 1 de arbol

modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso  + `NUMCUOTAS` + `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)


####Modelo 2 de arbol
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso  +  `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)

####Modelo 3 de arbol
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso  +  `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton + garantia_quartiles , data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)


####Modelo 4 de arbol
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso_quartiles  +  `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton + garantia_quartiles , data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)

##Modelo 5 de arbol
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+  percapita +  `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton + garantia_quartiles , data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)

##Modelo 6 de arbol
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero +  percapita +  `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton + `Monto Garantia` , data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)
