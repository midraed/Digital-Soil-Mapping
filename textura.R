library(soiltexture)
library(raster)

## Cargamos las capas necesarias
SAND <- raster("RK/resultados/ARE_5.15_RK.tif")
SILT <- raster("RK/resultados/LIM_5.15_RK.tif")
CLAY <- raster("RK/resultados/ARC_5.15_RK.tif")

textura.df <- as.data.frame(cbind(na.omit(values(SAND)),na.omit(values(SILT)),na.omit(values(CLAY))))
names(textura.df) <- c("SAND","SILT","CLAY")
textura.df <- TT.normalise.sum(tri.data=textura.df,tri.pos.tst=FALSE)
textura <-   TT.points.in.classes(tri.data=textura.df, class.sys="USDA.TT", PiC.type="t", tri.pos.tst=F)
textura <- as.factor(textura)

table(textura)
