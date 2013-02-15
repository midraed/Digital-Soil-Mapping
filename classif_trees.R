library(rpart)

#Vamos a cargar los datos
load("Practica/6. Modelado parametrico/calib.final.Rdata")

# Nuevamente, para trabajar mas ordenado, vamos a crear un nuevo objeto "calib" con la 
# las variables de posición (X e Y), la variable a predecir "Corg"
# Y las 19 covariables ambientales, calculadas el día anterior

calib <- cbind(calib.final[,1:2], calib.final$Orden, calib.final[,13:31])
names(calib)[3] <- "Orden"
names(calib)

# Veamos la cantidad de casos por cada orden de suelo
table(calib$Orden)

### Árboles de decisión con rpart ##########################

# Vamos a crear el árbol de decisión utilizando la función rpart:
# Los parametros son similares a la función lm o a nnet, ya vistas en el curso.
class.rpart <- rpart(Orden ~ ., calib)
# Veamos el resultado:
class.rpart

# Una salida un poco compleja, vamos a analizarla:
# node hace referencia a la hoja del árbol, split es el criterio de decisión
# n es la cantidad de casos en esa hoja, 
# yval es el valor más probable para esa hoja
# yprob son las probabilidades para cada clase, para entenderla mejor vamos los niveles 
# que puede asumir la variable Orden
levels(calib$Orden)

plot(class.rpart, compress=TRUE,uniform=TRUE)

text(class.rpart,use.n=F,all=F,cex=.7,pretty=0,xpd=TRUE, col="blue")

# Vamos a hacer uno con más información:
plot(class.rpart, compress=TRUE,uniform=TRUE)
text(class.rpart,use.n=T,all=T,cex=.7,pretty=0,xpd=TRUE, col="red")

# Debería poder interpretar bien este árbol. Comparelo con la versión en texto que obtuvo antes
# Además, interprete de que lado de cada división es positiva la regla de decisión
levels(calib$Orden)

# Vamos a volver a visualizar el árbol, y esta vez vamos a cargar una libreria llamada rattle 
# que permite graficar mejor los árboles
library(rattle)
drawTreeNodes(class.rpart,cex=.6,pch=11,size=4*.8, col=NULL,nodeinfo=TRUE,
              units="",cases="obs",digits=getOption("digits"),decimals=2,print.levels=TRUE,
              new=TRUE)

# Tomesé unos minutos para interpretar este último árbol.
# En particular, el porcentaje que aparece abajo. 
# Puede volver a ver el esquema de texto del árbol que obtuvimos antes

# Vamos a ver otro tipo de salida, con la probabilidad de cada orden para cada uno
# de nuestros puntos de entrenamiento
predict(class.rpart)

# Trate de comprender bien esta tabla.
# Ahora veamos una salida más simple
predict(class.rpart, type="class")

# Para terminar vamos a calcular una matriz de confusion
# en filas tenemos las clases observadas y en columnas las clases estimadas por el árbol
table(calib$Orden, predict(class.rpart, type="class"))

# Por ejemplo, mirando la primera fila:
# De los 18 casos de ARGISSOLO, 12 fueron correctamente clasificados, y 
# 6 fueron incorrectamente clasificados como LATOSSOLO
# y también que hubieron 10 (2+0+0+5+2+1) suelos clasificados como Argissolos y no lo eran

# Ahora vamos a calcular varios parámetros de evaluación del árbol de decisión:
library(caret)
library(e1071)
confusionMatrix(data=predict(class.rpart, type="class"), reference=calib$Orden)

# Podemos concluir que si bien es simple de obtener no hemos tenido mucha precision
# Vamos a utilizar otro método que posee algunas ventajas.


#### Árboles de decisión con C50 ###################
# Vamos a cargar una libreria llamada C50 que permite generar árboles de decisión 
# utilizando el algoritmo C5.0 de J.R. Quinlan (1986)
library("C50")

# Creamos el árbol de decisión con el método de C50
# Analice la formula
class.c5 <- C5.0(y=calib$Orden, x=calib[,4:22], data=calib, rules=F)

# Veamos el árbol generado
summary(class.c5)

# C50 calcula la matriz de confusión así como los errores totales.
# Pero no calcula la exactitud global. Calculela Ud:
confusionMatrix(data=predict(class.c5, calib), reference=calib$Orden)


### Árboles de decisión potenciados con C50 ##################################
# Ahora vamos a probar un árbol de decisión potenciado con C5.0.
# El comando a utilizar es el mismo que en el caso anterior pero vamos a agregarle
# el parámetro trials que hace referencia al número de iteraciones para potenciar el árbol
class.c5.boosted <- C5.0(y=calib$Orden, x=calib[,4:22], data=calib, rules=F, trials=100)

# Veamos el resultado: 
summary(class.c5.boosted)

# Y por último, calculemos la matriz de confusión:
confusionMatrix(data=predict(class.c5.boosted, calib), reference=calib$Orden)


# Vamos a guardar el árbol final como un objeto, para el ejercicio de validación:
save(class.c5.boosted, file="C:/Curso de Cartografia Digital de Suelos/Practica/7. Modelado no parametrico/arbol.final.RData")

# Para terminar vamos a guardar el espacio de trabajo:
save.image("C:/Curso de Cartografia Digital de Suelos/Practica/7. Modelado no parametrico/ArbolesDecision.RData")
