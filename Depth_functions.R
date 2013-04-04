# Depende de spline_functions.RData


FileName = "datos.csv"	 #El separador tiene que ser ";", en caso contrario hay que cambiarlo en la línea  
cID=2					 #Columna  con el ID número (tienen que ser enteros, correlativos a partir de 1)	
cLS=4					 #Columna con el límite superior de la capa (desde)
cLI=5					 #Columna con el límite inferior de la capa (hasta)
cAttrib=25				 #Columna con atributo a mapear
cX = 4  ## Columna con la coordenada X
cY =5  ## Columna con la coordenada Y

VarName = "CO"			 #Nombre de la variable (se usa para las columnas, y el archivo de salida)
##########################################
# El archivo de salida tiene el mismo nombre que el de entrada, con el nombre de la variable al final
#
##### Parametros de los splines ##################################################################

dat2 <- dat2[order(dat2[cID],dat2[cLS]), ]  ##Tambien ordenar por LS
		
lam <- 0.5 					 #Lambda value  aumentar el valor para hacerlo mas duro
mxd <- 100 					 #max depth for spline 
d<- t(c(0,5,15,30,100)) 			 #GlobalSoilMap.Net specifications or user defined depths

###########################################################################################################
# Inputs
dat2[,cLS]<-as.numeric(as.character(dat2[,cLS]))
dat2[,cLI]<-as.numeric(as.character(dat2[,cLI]))
dat2[,cAttrib]<-as.numeric(as.character(dat2[,cAttrib]))

###  Prueba de integridad de los datos ####
dat2$rechazado <- 0
dat2[which(dat2[,3]-dat2[,4]==0),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cID])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cLS])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cLI])),ncol(dat2)]<-1
dat2[which(is.na(dat2[,cAttrib])),ncol(dat2)]<-1
table(dat2$rechazado==0) ## FALSE corresponde a datos rechazados

     
write.table(subset(dat2, dat2$rechazado==1), "rechazados.csv", row.names=F)
dat2 <- subset(dat2, dat2$rechazado==0)

###
ndata=length(unique.default(dat2[,cID]))
IDnames <- as.character(unique.default(dat2[,cID]))
dat2[,cID]<-as.factor(dat2[,cID])
levels(dat2[,cID]) <- c(1:length(levels(dat2[,cID])))
dat_x<-cbind(dat2[,cID], as.numeric(dat2[,cLS]), as.numeric(dat2[,cLI]), dat2[,cAttrib])  
dat_m<- as.matrix(dat_x)

############################################################################## 
## Generamos los horizontes sintéticos mediantes ea plines
# Requiere paquete Splines
library(splines)
load("spline_functions.RData") #load all functions
# Inputs
dat_x<-cbind(dat2[,cID], dat2[,cLS], dat2[,cLI], dat2[,cAttrib])  
names (dat_x)
###Function for changing alphabet codes to numeric codes | Muy bueno esta
dat_x[,1]<-as.factor(dat_x[,1])
dat_m<- as.matrix(dat_x)
dat_m<-alp_num(dat_m)

############################################################################## 
## Aplicamos la funcion ea spline
p<- NA
int_s<-ea_spline(dat_x,ndata,lam,d)
nyfit<-int_s[[1]]
spfit<-int_s[[2]]

## Ahora hacemos lo mismo pero variando el valor de lambda
int_s_menor<-ea_spline(dat_m,ndata,lam-0.2,d)
nyfit_menor<-int_s_menor[[1]]
spfit_menor<-int_s_menor[[2]]
int_s_mayor<-ea_spline(dat_m,ndata,lam+0.2,d)
nyfit_mayor<-int_s_mayor[[1]]
spfit_mayor<-int_s_mayor[[2]]

## Extraemos los resultados y limpiamos los valores negativos.
nyfit[nyfit<0] <- NA
spfit[spfit<0] <- NA
nyfit_menor[nyfit_menor<0] <- NA
spfit_menor[spfit_menor<0] <- NA
nyfit_mayor[nyfit_mayor<0] <- NA
spfit_mayor[spfit_mayor<0] <- NA

#### Graficos  #####
# Esta seccion selecciona un perfil al azar y lo grafica
# con diferentes curvas
st=sample(1:ndata, 1) ##Elegimos un perfil al azar
subs <- subset(dat_m, dat_m[, 1] == st)
plot(rep(subs[,4],each=2), sort(c(subs[,2], subs[,3])), type="s", xlab=VarName, ylab="Profundidad", 
     ylim = rev(range(c(0,subs[,3]))), main=paste("Perfil: ", st,  " / ", IDnames[st]), xlim=c(min(subs[,4])*0.8,max(subs[,4])*1.2))
lines(spfit[,st], 0:mxd,col="red", lty=1, ylim = rev(range(c(0,subs[,3]))))
lines(spfit_menor[,st], 0:mxd,col="red2", lty=3, ylim = rev(range(c(0,subs[,3]))))
lines(spfit_mayor[,st], 0:mxd,col="red2", lty=3, ylim = rev(range(c(0,subs[,3]))))
abline(h=as.vector(d), lty=2, col="grey60")
newhor <- vector()
for(nwh in 1:length(d)-1){newhor[nwh] <- (d[nwh]+d[nwh+1])/2}
points(nyfit_menor[st,1:length(d)-1],newhor, col="lightblue", pch=20,ylim = rev(range(c(0,subs[,3]))))
points(nyfit_mayor[st,1:length(d)-1],newhor, col="lightblue", pch=20,ylim = rev(range(c(0,subs[,3]))))
points(nyfit[st,1:length(d)-1],newhor, col="blue", pch=19,ylim = rev(range(c(0,subs[,3]))))
legend(inset=.02, x="bottomright", legend= c("medido", paste("spline (lambda ", lam, "+-0.2)"), "horiz. sintéticos", "muestreo"), 
       col=c("black", "red", "grey60", "blue"),text.col = "black", lty = c(1, 1, 2, 0), pch = c(NA, NA, NA, 20), cex=0.8)

#### Exportar ####
cNames <- c("ID","X", "Y", paste(VarName, paste(d[,1:length(d)-1], "-", d[,2:length(d)], sep=""), sep="_"), "MaxDepth")
OutFileName <- paste(substr(FileName, 1, nchar(FileName)-4), "_", VarName, ".csv", sep="")
levels(dat2[,cID])<-IDnames
XY <- unique(merge(IDnames, dat2[,c(cID,cX,cY)], by.x=1, by.y=1))
write.table(cbind(IDnames,XY[,2:3],nyfit), OutFileName, col.names= cNames, row.names=FALSE, sep=";")
