SOC <- c(0.5, 1.0, 1.5, 2.0, 3.0)

estimateBD <- function(SOC, method="Saini_1996"){
  OM <- SOC * 1.724
  if(method=="Saini_1996"){BD <- 1.62 - 0.06 * OM}
  if(method=="Drew_1973"){BD <- 1 / (0.6268 + 0.0361 * OM)}
  if(method=="Jeffrey_1979"){BD <- 1.482 - 0.6786 * (log(OM))}
  if(method=="Grigal_1989"){BD <- 0.669 + 0.941 * exp(1)^(-0.06 * OM)}
  if(method=="Adams_1973"){BD <- 100 / (OM /0.244 + (100 - OM)/2.65)}
  if(method=="Honeyset_Ratkowsky_1989"){BD <- 1/(0.564 + 0.0556 * OM)}
  return(BD)
}


estimateBD(SOC, method="Saini_1996")
estimateBD(SOC, method="Drew_1973")
estimateBD(SOC, method="Jeffrey_1979")
estimateBD(SOC, method="Grigal_1989")
estimateBD(SOC, method="Adams_1973")
estimateBD(SOC, method="Honeyset_Ratkowsky_1989")

save(estimateBD, file = "estimateBD.RData")


