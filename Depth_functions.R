# Depende de spline_functions.RData


FileName = "embrapa.csv"	 #El separador tiene que ser ";", en caso contrario hay que cambiarlo en la línea  
cID=2					 #Columna  con el ID número (tienen que ser enteros, correlativos a partir de 1)	
cLS=4					 #Columna con el límite superior de la capa (desde)
cLI=5					 #Columna con el límite inferior de la capa (hasta)
cAttrib=25				 #Columna con atributo a mapear
VarName = "CO"			 #Nombre de la variable (se usa para las columnas, y el archivo de salida)
##########################################
# El archivo de salida tiene el mismo nombre que el de entrada, con el nombre de la variable al final
#
##### Parametros de los splines ##################################################################

dat2<-read.table (FileName, sep=",", header=TRUE)
dat2 <- dat2[order(dat2[cID]), ]
IDnames <- as.character(unique.default(dat2[,cID]))

ndata=length(unique.default(dat2[,cID]))		
lam <- 0.5 					 #Lambda value  aumentar el valor para hacerlo mas duro
mxd <- 100 					 #max depth for spline 
d<- t(c(0,5,15,30,100)) 			 #GlobalSoilMap.Net specifications or user defined depths


###########################################################################################################
############# ACA EMPIEZA EL CODIGO PURO, no deberia haber necesidad de cambiar nada ######################
###########################################################################################################
# Requiere paquete Splines
library(splines)
load("spline_functions.RData") #load all functions
# Inputs
dat2
dat_x<-cbind(dat2[,cID], dat2[,cLS], dat2[,cLI], dat2[,cAttrib])  
names (dat_x)
###Function for changing alphabet codes to numeric codes | Muy bueno esta
levels(dat_x[,1]) <- c(1:length(levels(dat_x[,1])))
dat_m<- as.matrix(dat_x)
dat_m<-alp_num(dat_m)
dat_m
############################################################################## 
# Fit splines to raw profile data
#dat_m<- as.matrix(dat)
# Run fit equal area spline function
int_s<-ea_spline(dat_m,ndata,lam,d)
nyfit<-int_s[[1]]
spfit<-int_s[[2]]
nyfit
cNames <- c("ID", paste(VarName, paste(d[,1:length(d)-1], "-", d[,2:length(d)], sep=""), sep="_"), "MaxDepth")
cNames
OutFileName <- paste(substr(FileName, 1, nchar(FileName)-4), "_", VarName, ".csv", sep="")
write.table(cbind(IDnames,nyfit), OutFileName, col.names= cNames, row.names=FALSE, sep=";")
