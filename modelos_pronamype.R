#modelos pronamype

library(tidyverse)
library(rpart)
library(rpart.plot)
library(schoRsch)
library(gmodels)
library(caret)
#source("limpieza_base_final.R", encoding = "UTF-8")
source("limpieza_base_final.R", encoding = "UTF-8")

indices.general <- function(MC) {
  precision.global <- sum(diag(MC))/sum(MC)
  error.global <- 1 - precision.global
  precision.categoria <- diag(MC)/rowSums(MC)
  res <- list(matriz.confusion = MC, precision.global = precision.global, error.global = error.global, 
              precision.categoria = precision.categoria)
  names(res) <- c("Matriz de Confusión", "Precisión Global", "Error Global", 
                  "Precisión por categoría")
  return(res)
}
#termina canton
#ingreso
arbol <-  arbol %>% filter(!(ingreso %in% c("ND", "-", "<NA>") | is.na(ingreso)))
arbol$ingreso
#modelos
#arbol


#partir monto de la garantia por quantiles

arbol$garantia_quartiles <-  ntiles(arbol, "Monto Garantia", bins = 5, res.labels = quantile(arbol$`Monto Garantia`, probs = seq(0, 1, 0.2))[-1])





##########quitar la edad
arbol <-arbol %>% filter(!((is.na(edad) | str_detect(edad, "[:digit]")))) %>% mutate(edad = as.numeric(edad))


muestra <- sample(1:dim(arbol)[1], round(0.2*dim(arbol)[1]))
ttesting <- arbol[muestra,]
taprendizaje <- arbol[-muestra,]

 arbol[, str_detect(colnames(arbol), "prop.")] <- as.data.frame(lapply(arbol[, str_detect(colnames(arbol), "prop.")], as.factor))

modelo <- rpart(as.factor(prop1) ~ `Monto Garantia` + provincia + edad + genero + `NUMCUOTAS` + `Plazo en meses` + actividad_económica + intermediaria + tipo_garantia_mister + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
prp(modelo,extra=104,branch.type=2, box.col=c("pink", "palegreen3","cyan")[modelo$frame$yval])
MC<-table(ttesting$prop1,prediccion) # Índices de Calidad de la predicción indices.general(MC)
MC
indices.genemodelo <- rpart(as.factor(prop3) ~ `Monto Garantia` + provincia + edad + genero + `NUMCUOTAS` + `Plazo en meses` + actividad_económica + intermediaria + tipo_garantia_mister + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
prp(modelo,extra=104,branch.type=2, box.col=c("pink", "palegreen3","cyan")[modelo$frame$yval])
MC<-table(ttesting$prop3,prediccion) # Índices de Calidad de la predicción indices.general(MC)
MC
indices.general(MC)



####  que el ingreso no sea numeros y la cantidad de miembros tampoco

arbol2 <- arbol %>% filter( !str_detect(ingreso, "[:alpha:]") & str_detect(miembros, "[:digit:]")) %>%  mutate(ingreso = as.numeric(ingreso), miembros = as.numeric(miembros) )%>% filter(ingreso < 1000000) %>% filter(!(miembros==0 | miembros>9))
arbol2 <- arbol2 %>%  mutate(ingreso = ingreso * Nivel, `Monto Garantia` =  `Monto Garantia` * Nivel) %>% mutate(percapita= ingreso/miembros  )


arbol2$ingreso_quartiles <-  ntiles(arbol2, "ingreso", bins = 5, res.labels = quantile(arbol2$ingreso, probs = seq(0, 1, 0.2))[-1])

arbol2$percapita_quartiles <-  ntiles(arbol2, "percapita", bins = 5, res.labels = quantile(arbol2$percapita, probs = seq(0, 1, 0.2))[-1])



####Modelo 1 de arbol
muestra <- sample(1:dim(arbol2)[1], round(0.3*dim(arbol2)[1]))
ttesting <- arbol2[muestra,]
taprendizaje <- arbol2[-muestra,]
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso  + `NUMCUOTAS` + `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type = 'class')
prp(modelo,extra=104,branch.type=2, box.col=c("pink", "palegreen3","cyan")[modelo$frame$yval])
MC<-table(ttesting$prop1,prediccion)
indices.general(MC)





n <- dim(arbol2)[1]
deteccion.error.arbol<-rep(0,20)
for(i in 1:20) { 
  grupos <- createFolds(1:n,10)
  error.arbol<-0
for(k in 1:10) { 
muestra <- grupos[[k]] # Por ser una lista requiere de doble paréntesis
ttesting <- arbol2[muestra,] 
taprendizaje <- arbol2[-muestra,]
modelo <- rpart(as.factor(prop1) ~ provincia + edad + genero+ miembros +  ingreso  + `NUMCUOTAS` + `Plazo en meses` + actividad_económica +  tipo_garantia_mister  + canton, data=taprendizaje,minsplit=2)
prediccion <- predict(modelo, ttesting, type='class') 
Actual<-ttesting[,20] 
MC<-table(Actual,prediccion) # Detección del ERROR
error.arbol<-error.arbol+(1-(sum(diag(MC)))/sum(MC))*100
} 
deteccion.error.arbol[i]<-error.arbol/10  }


plot(deteccion.error.arbol, col = "magenta", type = "b", ylim = c(min(deteccion.error.arbol), max(deteccion.error.arbol)+3), main = "Detección del ERROR", xlab = "Número de iteración", ylab = "ERROR Cometido") 
points(deteccion.error.arbol, col = "red", type = "b") 
legend("topright", legend = c("Árbol"), col = c("red"), lty = 1, lwd = 2)





define.colores <- function(n) { hues <- seq(15, 375, length = n + 1) 
hcl(h = hues, l = 65, c = 100)[1:n] }  
ggplot(arbol2, aes_string('miembros_quartiles', fill = 'prop1')) + geom_density( alpha = .85) + theme_minimal() + theme(text = element_text(size=15)) + scale_fill_manual(values = define.colores(length(levels(arbol2[,'prop1'])))) + labs(title = 'Densidad de la variable s.largo según tipo', y = '', x = '') + theme(legend.position = 'top', legend.title = element_blank(), text = element_text(size = 15))


ist.x.predecir <- function(data, variable, variable.predecir) { 
  data. <- data %>% group_by_(variable, variable.predecir) %>% summarise(count = n()) %>% mutate(prop = round(count/sum(count),4)) 
  return(data.) }

arbol2$garantia_quartiles <- as.factor(arbol2$garantia_quartiles)
colores <- define.colores(length(unique(arbol2[,'prop1']))) 
label.size <- 9.5 - length(unique(arbol2[,'garantia_quartiles'])) 
label.size <- ifelse(label.size < 3, 3, label.size) 
data. <- dist.x.predecir(arbol2, 'garantia_quartiles', 'prop1') 
ggplot(data., aes(fct_reorder(data.[['garantia_quartiles']], count, .desc = T), prop, fill = data.[['prop1']])) + geom_bar(stat = 'identity') + geom_text(aes(label = paste0(count, ' (', scales::percent(prop), ')'), y = prop), color = 'gray90', position = position_stack(vjust = .5), size = label.size) + theme_minimal() + theme(text = element_text(size=15)) + scale_fill_manual(values = colores) + scale_y_continuous(labels = scales::percent)+ coord_flip() + labs(title = 'Distribución relativa de la variable garantia_quartiles según la prop1', x = '', y = '') + guides(fill = guide_legend(reverse=T)) + theme(legend.position = 'top', legend.title = element_blank())


