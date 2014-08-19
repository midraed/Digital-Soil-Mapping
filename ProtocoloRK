
setwd("working_dir")
library(gstat)
library(raster)
library(sp)
library(soiltexture)


datos <- read.csv("Horiz_A.csv")

datos.sp <- datos
coordinates(datos.sp) = ~X+Y
plot(datos.sp)
summary(datos.sp)
## Deben estar en una carpeta "covar" dentro de la carpeta de trabajo
## Cargamos las covariables ambientales en R
startdir <- getwd()
setwd(paste(getwd(), "/covar_ss", sep=""))
files <- list.files(pattern="sdat")
stack1 <- list()
for(i in 1:length(files)) {
  stack1[[i]] <- raster(files[i])}
covariables <- do.call(stack, stack1) ### JO!
setwd(startdir)
covariables
plot(covariables)

covariables.sp <- as(covariables, "SpatialGridDataFrame") ## 45Mb!!!

## Creamos la grilla de interpolacion
halfres <- res(covariables)[1]/2
grilla <- expand.grid(x=seq(from=xmin(covariables)+halfres, to=xmax(covariables)-halfres, by=res(covariables)[1]), y=seq(from=ymin(covariables)+halfres, to=ymax(covariables)-halfres, by=res(covariables)[2]))
coordinates(grilla) <- ~ x+y
gridded(grilla) <- TRUE

# Extraemos los valores de las covariables
datos <- cbind(datos, extract(covariables, datos.sp))  


### Análisis Descriptivo ####
summary(datos)
plot(datos.sp,pch=1 ,cex = datos$Arcilla/max(datos$Arcilla)*4)
boxplot(datos$Arcilla)
summary(datos$Arcilla)

#Algunos gráficos
boxplot(datos$Arcilla)
hist(datos$Arcilla)
#Diagrama de tallo-hoja
stem(datos$Arcilla)
#La Varianza
var(datos$Arcilla)
# Desvio standar
mean(datos$Arcilla)
sd(datos$Arcilla)

plot(datos.sp)
points(datos.sp[datos$Arcilla<30,])
plot(datos$Serie, datos$Arcilla, las=3, main="Contenido de Arcillas, horiz superficial", cex.axis=0.7)


#### Modelo del regresion MLR ####

Model.Arcilla.Super <- lm(Arcilla ~ Altitude_above_Channel_Network+Aspect+Channel_Network_Base_Level
                        +LS.Factor+Profile_Curvature+Slope_Height+Slope+SRTM30_estaca+Standardized_Height+Valley_Depth+Wetness_Index, data=datos)

summary(Model.Arcilla.Super)

Model.Arcilla.Super.step <- step(Model.Arcilla.Super, direction="both")
summary(Model.Arcilla.Super.step)

###

Model.Arcilla.Super2 <- lm(Arcilla ~ Altitude_above_Channel_Network+Channel_Network_Base_Level
                      +LS.Factor+Profile_Curvature+Slope+SRTM30_estaca+Wetness_Index, data=datos)
summary(Model.Arcilla.Super2)
Model.Arcilla.Super.step2 <- step(Model.Arcilla.Super2, direction="both")
summary(Model.Arcilla.Super.step2)

Model.Arcilla.Super.step2$residuals

Arcilla.Super.MLR <- predict(covariables, Model.Arcilla.Super.step2, progress="text")
plot(Arcilla.Super.MLR)
points(datos.sp)

#### Modelo de kriging ####

residuos.Arcilla.Sup <- Model.Arcilla.Super.step2$residuals
datos$Arcilla.res <- residuos.Arcilla.Sup

datos.sp <- datos
coordinates(datos.sp) = ~X+Y
plot(datos.sp,pch=1 ,cex = datos$Arcilla.res/max(datos$Arcilla.res)*4)
bubble(datos.sp, "Arcilla.res",col=c("blue", "red")) # Este graf. se ve mejor a pantalla completa


### Regression-kriging ####

dependend_var.v <- variogram(as.formula(Model.Arcilla.Super.step2$call$formula), datos.sp)
dependend_var.ovgm <- fit.variogram(dependend_var.v, vgm(nugget=0, "Exp", range=sqrt(diff(datos.sp@bbox[1,])^2 + diff(datos.sp@bbox[2,])^2)/4, psill=var(Model.Arcilla.Super.step2$residuals)))   
plot(dependend_var.v, dependend_var.ovgm, plot.nu=T, main="Residuos Arcilla Sup") ## 350*260
dependend_var.ovgm
Arcilla.Super.auto.rk <- krige(as.formula(Model.Arcilla.Super.step2$call$formula), datos.sp, covariables.sp, dependend_var.ovgm)

#### Leave One Out Cross Validation 

Arcilla.Super.cross <- krige.cv(as.formula(Model.Arcilla.Super.step2$call$formula), datos.sp, dependend_var.ovgm , verbose=F)
Arcilla.Super.explained_variation <- 1-var(Arcilla.Super.cross$residual, na.rm=T)/var(datos.sp$Arcilla)

summary(Arcilla.Super.cross)
# mean error, ideally 0:
mean(Arcilla.Super.cross$residual)
# MSPE, ideally small
mean(Arcilla.Super.cross$residual^2)
# Mean square normalized error, ideally close to 1
mean(Arcilla.Super.cross$zscore^2)
# correlation observed and predicted, ideally 1
cor(Arcilla.Super.cross$observed, Arcilla.Super.cross$observed - Arcilla.Super.cross$residual)
# correlation predicted and residual, ideally 0
cor(Arcilla.Super.cross$observed - Arcilla.Super.cross$residual, Arcilla.Super.cross$residual)
# explained variation
Arcilla.Super.explained_variation

Arcilla.Super.rk.pred <- raster(Arcilla.Super.auto.rk["var1.pred"])
Arcilla.Super.rk.var <- raster(Arcilla.Super.auto.rk["var1.var"])
Arcilla.Super.rk.error <- qnorm(0.95)*sqrt(Arcilla.Super.rk.var)/sqrt(nrow(datos.sp))

plot(Arcilla.Super.rk.pred, main="Arcilla por RK")
points(datos.sp)

plot(Arcilla.Super.rk.pred-Arcilla.Super.rk.error, main="IC95 minimo") ## Esto sería: el valor mínimo del modelo dentro de un intervalo de confianza del 95%
plot(Arcilla.Super.rk.pred+Arcilla.Super.rk.error, main="IC95 maximo")

writeRaster(Arcilla.Super.rk.pred, "Resultados/Arcilla.Super.RK.tiff", overwrite=TRUE)




