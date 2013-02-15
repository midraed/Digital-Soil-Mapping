require(gstat)
require(sp)
library(gstat)
library(raster)

## Cargamos las covariables ambientales en R
## Deben estar en una carpeta "covar" dentro de la carpeta de trabajo
startdir <- getwd()
setwd(paste(getwd(), "/covar", sep=""))
files <- list.files(pattern="sdat")
stack1 <- list()
for(i in 1:length(files)) {
  stack1[[i]] <- raster(files[i])}
covariables <- do.call(stack, stack1) ### JO!
setwd(startdir)
plot(covariables)
covariables

# Cargamos los datos 
datos <- read.csv("datosRK.csv")  ## datosRK tiene ID/X/Y/var
datos.sp <- datos
coordinates(datos.sp) = ~X+Y
plot(datos.sp)
datos <- cbind(datos, extract(covariables, datos.sp))

## Ajustamos un modelo de regresion lineal multiple
# Entre CE y las covariables espaciales (todas, excepto X, Y e ID)
CE.MLR <- lm(CE~.-ID-X-Y, datos) 
summary(CE.MLR)
anova(CE.MLR)

## Hacemos seleccion de variables por stepwise
CE.MLR.step <- step(CE.MLR, direction="both")
summary(CE.MLR.step)
anova(CE.MLR.step)

# Es mejor el modelo mas complejo?
anova(CE.MLR.step, CE.MLR)

## Separamos los residuos y los reservamos para para krigearlos
residuos <- CE.MLR.step$residuals
residuos <- cbind(datos[,2:4],residuos)

### Predecimos el valor de la variable segun el modelo de MLR
CE.MLR <- predict(covariables, CE.MLR.step, progress="window")

### KRIGING DE LOS RESIDUOS
coordinates(residuos) = ~X+Y
residuos
## Vemos un resumen del objeto
summary(residuos)

### Probemos con un metodo para visualizar ademas los valores de la variable
# bubble() grafica los valores de la var en relacion al tamaño de los circulos
# y ademas diferencia valores negativos (en azul en este caso) de positivos (en rojo)
bubble(residuos, "residuos",col=c("blue", "red")) # Este graf. se ve mejor a pantalla completa

### Creamos la grilla de interpolacion
halfres <- res(covariables)[1]/2
grilla <- expand.grid(x=seq(from=xmin(covariables)+halfres, to=xmax(covariables)-halfres, by=res(covariables)[1]), y=seq(from=ymin(covariables)+halfres, to=ymax(covariables)-halfres, by=res(covariables)[2]))
coordinates(grilla) <- ~ x+y
gridded(grilla) <- TRUE
extent(grilla)

## Calculamos el variograma muestral
variog = variogram(residuos~1, residuos)

## Vemos los valores del variograma y los graficamos con respecto a la distancia
## Estimamos y tomamos nota del alcance visualmente
variog
plot(variog)

## Ajustamos un modelo al variograma muestral
## Elegimos el modelo esférico y completamos con el alcance estimado
## El ajuste se hace por minimos cuadrados

## RECUERDE REEMPLAZAR "ALCANCE" por el valor de alcance que estimo!!
## Ademas "Sph" se refiere al modelos esférico, esto puede alterarse por otros modelos
variog.ajust <- fit.variogram(variog, model=vgm(1, "Exp", 5000, 1))

## Visualizamos los parametros del modelo ajustado
variog.ajust
plot(variog, variog.ajust)

## Ahora interpolamos por krige los datos
Residuos.krig <- krige(residuos~1, residuos, newdata=grilla, model=variog.ajust)
Residuos.MLR <- raster(Residuos.krig["var1.pred"])

Resultado <- CE.MLR + Residuos.MLR

plot(Resultado)
