#Proyecto_Final
        #Equipo:
#Avila Argüello Carlos
#Bonilla Cruz José Armando
#Gutierrez Luna Yanley
#Rivera Mata Dante Tristán

#librerias----

library(readr)
library(dplyr)
library(readxl)
library(corrplot)
library(moments)
library(tidyverse)
library(car)
library(timeDate)
library(MASS)
library(audio)
library(beepr)
library(zoo)
library(psych) 
library(nortest)
library(lmtest)
library(stats)

#Cargamos la base
airb <- read.csv("~/Facultad de Ciencias/Estad?stica/Modelos no Param?tricos y de  Regresi?n/Proyectos/AB_NYC_2019.csv")

#--------------------------------------------------------Filtrar la base-----
      #Filtramos la base con las variables regresoras continuas para el precio.
#La columnas corresponden a las variables continuas/numéricas
airb <- filter(airb, number_of_reviews>0)
airb1 <- airb[,c(5, 7, 8, 9 ,10, 11, 13, 14, 16)]
        #Con precios positivos y días disponibles mayores a cero. Con últimas reseñas
airb1 <- filter(airb1, price>0, availability_365>0)

          #Ponemos los ceros originales que en RStudio se vuelven NA
for(i in 1:length(airb1$reviews_per_month)){
  if(is.na(airb1$reviews_per_month[i])){
    airb1$reviews_per_month[i] <- 0
  }
}
          #Restringirlos al número de noches menores a un año
airb1<-filter(airb1, minimum_nights<365)

#Conertimos la fecha de última crítica a meses.
ultima_critica<-airb1$last_review
uc <- as.timeDate(ultima_critica)
mes_ultima_critica <- months(uc)
str(mes_ultima_critica)

distritos <- as.character(airb1$neighbourhood_group)
distrito <- factor(distritos)

habitaciones <- as.character(airb1$room_type)
habitacion <- factor(habitaciones)





#--------------------------------CON DATOS ATÍPICOS--------------------------

#--------------------------------Análisis Descriptivo----

# Convertimos las columnas en vectores
precio <- airb1$price
min_noches <- airb1$minimum_nights
critica_mensual <- airb1$reviews_per_month
disponibilidad <- airb1$availability_365
latitud<-airb1$latitude
longitud<-airb1$longitude
habitacion
distrito
meses<-factor(mes_ultima_critica, levels = 1:12,
  labels =c("ene", "feb", "marz", "abr", "may", "jun", "jul", "ago", "sept", "oct","nov", "dic"))

#Resumen
summary(airb1)
length(airb1$latitude)

#---Precio----
hist(precio, col = "lightblue4",freq = FALSE,main="Precio")
lines(density(precio),col="darkblue",lwd=2)
abline(v=mean(precio),lwd=2,col="purple")
legend("topright", legend=c("Asimetría: 18.56","Curtosis: 638.97"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(precio);  kurtosis(precio)
boxplot(precio, main = "Precio")

#Reescalamiento logarítmico
log_precio <- log(precio)

hist(log_precio, col = "lightblue4",freq = FALSE,main="Logaritmo de precio")
lines(density(log_precio),col="darkblue",lwd=2)
abline(v=mean(log_precio),lwd=2,col="purple")
legend("topright", legend=c("Asimetría: 0.53","Curtosis: 0.85"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(log_precio);  kurtosis(log_precio)
summary(log_precio)

#---Minimo de noches----
hist(min_noches, col = "orange",freq = FALSE,main="Mínimo de noches")
lines(density(min_noches),col="red",lwd=2)
abline(v=mean(min_noches),lwd=2,col="yellow")
legend("topright", legend=c("Asimetría: 7.78","Curtosis: 113.33"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(min_noches);  kurtosis(min_noches)
boxplot(min_noches, main="Mínimo de noches")

#Rescalamiento logarítmico
log_min_noches <- log(min_noches)
hist(log_min_noches, col = "orange",freq = FALSE,main="Logarítmico de mínimo de noches")
lines(density(log_min_noches),col="red",lwd=2)
abline(v=mean(log_min_noches),lwd=2,col="yellow")
legend("topright", legend=c("Asimetría: 0.99","Curtosis: 0.11"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(log_min_noches);  kurtosis(log_min_noches)
summary(log_min_noches)

#---Crítica mensual----
hist(critica_mensual, col = "green3",freq = FALSE,main="Crítica Mensual")
lines(density(critica_mensual),col="darkgreen",lwd=2)
abline(v=mean(critica_mensual),lwd=2,col="black")
legend("topright", legend=c("Asimetría: 2.99","Curtosis: 43.89"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(critica_mensual);  kurtosis(critica_mensual)
boxplot(critica_mensual, main= "Critica mensual")

#---Disponibilidad----
hist(disponibilidad, col = "pink",freq = FALSE,main="Disponibilidad")
lines(density(disponibilidad),col="violet",lwd=2)
abline(v=mean(disponibilidad),lwd=2,col="purple")
legend("topright", legend=c("Asimetría: 0.17","Curtosis: -1.44"), col= "white", cex=.7,lwd=1)
#Asimetría                 #Curtósis
skewness(disponibilidad);  kurtosis(disponibilidad)
boxplot(disponibilidad, main="Disponibilidad")
#---Latitud----
hist(latitud, col = "yellow",freq = FALSE,main="Latitud")
lines(density(latitud),col="orange3",lwd=2)
abline(v=mean(latitud),lwd=2,col="red")
legend("topleft", legend=c("Asimetría: 0.22","Curtosis: 0.06"), col= "white", cex=.7,lwd=2)
#Asimetría    #Curtósis
skewness(latitud);  kurtosis(latitud)
boxplot(latitud, main="Latitud")

#Estandarización
latitud_est <- (latitud-mean(latitud))/sd(latitud)
hist(latitud_est, col = "yellow",freq = FALSE,main="Latitud Estandar")
lines(density(latitud_est),col="orange3",lwd=2)
abline(v=mean(latitud_est),lwd=2,col="red")
legend("topleft", legend=c("Asimetría: 0.22","Curtosis: 3.05"), col= "white", cex=.7,lwd=2)
#Asimetría    #Curtósis
skewness(latitud_est);  kurtosis(latitud_est)


#---Longitud----
hist(longitud, col = "#D4E6F1",freq = FALSE,main="Longitud")
lines(density(longitud),col="#48C9B0",lwd=2)
abline(v=mean(longitud),lwd=2,col="white")
legend("topleft", legend=c("Asimetría: 1.11","Curtosis: 3.72"), col= "white", cex=,lwd=2)
#Asimetría    #Curtósis
skewness(longitud);  kurtosis(longitud)
boxplot(longitud, main ="Longitud")

#Estandarización
longitud_est <- (longitud-mean(longitud))/sd(longitud)
hist(longitud_est, col = "#D4E6F1",freq = FALSE,main="Longitud Estandarizada")
lines(density(longitud_est),col="#48C9B0",lwd=2)
abline(v=mean(longitud_est),lwd=2,col="white")
legend("topleft", legend=c("Asimetría: 1.11","Curtosis: 6.72"), col= "white", cex=,lwd=2)
#Asimetría    #Curtósis
skewness(longitud_est);  kurtosis(longitud_est)

#---Tipo de habitación----
barplot(table(habitaciones), col = "#66B3FF", main = "Tipo de habitación")
summary(habitacion)

boxplot(airbnb$log_precio~airbnb$habitacion,pch = 19, cex = .3,col = "#F6DDCC", xlab = "Tipo de habitación", ylab ="Logaritmo del precio" , main = "Boxplot")

#---Distrito----
barplot(table(distritos), col = "#FFD9EC", main = "Distrito")
summary(distrito)

boxplot(airbnb$log_precio~airbnb$distrito,pch = 19, cex = .5,col = "#FFFFCC", xlab = "Distrito", ylab ="Logaritmo del precio" , main = "Boxplot")

#---Meses----
barplot(table(meses), col = "orange3", main = "Meses de crítica")
summary(meses)

boxplot(airbnb$log_precio~airbnb$meses,pch = 19, cex = .5,col = "lightblue2", xlab = "Meses de crítica", ylab ="Logaritmo del precio" , main = "Boxplot")
grid(20,20, col="lightblue3")

#---Correlación de variables-----
airbnb_cont<-data.frame(latitud_est, longitud_est, log_precio, log_min_noches, critica_mensual)
corrplot(cor(airbnb_cont))

airbnb_num<-data.frame(latitud, longitud, log_precio, log_min_noches, critica_mensual, disponibilidad)
pairs(airbnb_num, cex = .2, pch =19, col= "orange3", upper.panel = NULL)

#Factores de inflación de la varianza
vif(modelo)

#-----------------------------Construcción del modelo----
airbnb <- data.frame(log_precio, log_min_noches, latitud_est, longitud_est, critica_mensual,disponibilidad, distrito, habitacion, meses)

airbnb <- airbnb %>% mutate(distrito = relevel(distrito, ref = "Manhattan"))
airbnb <- airbnb %>% mutate(habitacion = relevel(habitacion, ref = "Entire home/apt"))
airbnb <- airbnb %>% mutate(meses = relevel(meses, ref = "jun"))

modelo<-lm(log_precio~log_min_noches+latitud_est+longitud_est+critica_mensual+disponibilidad+distrito+habitacion+meses, data =  airbnb)
summary(modelo)

#Pruebas de de hipótesis de significancia
anova(modelo)

#Intervalos de confianza de los parámetros
confint(modelo)

#----------------------------- Analisis de Residuales-----
#----Homocedasticidad----
error_estud <- rstudent(modelo)
error_est <- rstandard(modelo)

log_precio_ajust <- fitted(modelo)
#   Graficas
plot(log_precio_ajust, error_estud, main="Homocedasticidad",col="pink3",cex=.3,pch=19)
abline(0,0, col="magenta", lw=2)
abline(2,0, col="black", lw=2, lty=4)
abline(-2,0, col="black", lw=2, lty=4)
#   Pruebas Estadisticas 
bptest(modelo)
ncvTest(modelo)

#----No Correlación----
#   Grafica
plot(error_estud,main="No correlación",cex=.3,xlab="Subindice",ylab="R.estudentizados",pch=19, col="yellow3")
acf(error_estud, main= "Serie de los residuos", col="red4",lag=10)
#   Pruebas Estadisticas
dwtest(modelo)
bgtest(modelo)

#----Normalidad-----
#   Graficas
qqnorm(error_est, pch=11, col="green4",lwd=3,cex=.1,main = "Normalidad")
qqline(error_est, col="orange", lwd=2)

hist(error_est, col = "yellow",freq = FALSE,main="Error estudentizados")
lines(density(error_est),col="orange",lwd=2)
abline(v=mean(error_est),lwd=2,col="red")
legend("topright", legend=c("Asimetría: 1.078","Curtosis: 7.597"), col= "white", cex=.7,lwd=2)
skewness(error_est);  kurtosis(error_est)

#   Pruebas Estadisticas
ad.test(error_est)

#----Outliers----
n <- length(airbnb$log_precio)
p <- length(modelo$coefficients)

indices <- c()
for (i in 1:n) {
  if(abs(error_estud[i])>=qt(.975,n-p)){
    indices <- c(indices,i)
  }
}
#Número de datos atípicos
length(indices)

airbnb_sda <- airbnb[-indices,]
length(airbnb_sda$log_precio)


#----------------------------------Transformaciones--------
#BoxCox
bc<-boxCox(modelo, lambda=seq(-1,1,.1))
max(bc$y)
lambday<- bc$x[bc$y == max(bc$y)]
lambday

log_precio_bc<-log_precio^lambday

#Creamos una función que nos diga lo que nos interesa al decidir
#un modelo (Sin Datos Atípicos) luego de una transformación.
transformacion <- function(yt, minimo_noches,latitus,longitus,critic_mens,disponibilidas){
  opcion <- lm(yt~minimo_noches + latitus + longitus + critic_mens + disponibilidas + habitacion + distrito + meses)
  s <- summary(opcion)
  print(paste0("R2: ", s$r.squared))
  print(paste0("R2 ajustada : ", s$adj.r.squared))
  
  print("Sobre los residuos:")
  
  #No correlación
  bg <- bgtest(opcion)
  if(bg$p.value > 0.05){
    print(paste0("No hay correlación (Brush-Godfry). P-value: ", bg$p.value ))
  }
  else{
    print(paste0("Sí hay correlación (Brush-Godfry). P-value: ", bg$p.value ))
  }
  
  dW <- dwtest(opcion)
  if(dW$p.value > 0.05){
    print(paste0("No hay correlación (Durbin-Watson). P-value: ", dW$p.value ))
  }
  else{
    print(paste0("Sí hay correlación (Durbin-Watson). P-value: ", dW$p.value ))
  }
  #Homocedasticidad
  bp <- (bptest(opcion))
  if(bp$p.value > 0.05){
    print(paste0("Sí hay varianza constante (Brush-Pagan). P-value: ", bp$p.value ))
  }
  else{
    print(paste0("No hay varianza constante (Brush-Pagan). P-value: ", bp$p.value ))
  }
  
  n_c_v <- ncvTest(opcion)
  if(n_c_v$p > 0.05){
    print(paste0("Sí hay varianza constante (Non-Constant-Variance). P-value: ", n_c_v$p))
  }
  else{
  print(paste0("No hay varianza constante (Non-Constant-Variance). P-value: ", n_c_v$p))
  }
  
  #Normalidad
  ad <- ad.test(opcion$residuals)
  if(ad$p.value > 0.05){
    print(paste0("Sí hay normalidad (Anderson-Darlin). P-value: ", ad$p.value ))
  }
  else{
    print(paste0("No hay normalidad (Anderson-Darlin). P-value: ", ad$p.value ))
  }
  
  ## Outliers
  n <- length(yt)
  p <- length(opcion$coefficients)
  error_estud <- rstudent(opcion)
  indices <- c()
  for (i in 1:n) {
    if(abs(error_estud[i])>=qt(.975,n-p)){
      indices <- c(indices,i)
    }
  }
  #Número de datos atípicos
  print(paste0("Porcentaje de datos atípicos:", (length(indices)/length(yt))*100, "%"))
  
}

#Opción 1
transformacion(log_precio, log_min_noches, latitud_est, longitud_est, 
               critica_mensual, disponibilidad)

#Opción 2
transformacion(log_precio_bc, log_min_noches, latitud_est, longitud_est, 
               critica_mensual, sqrt(disponibilidad))

#Opción 3
transformacion((log_precio)^(1/2), log_min_noches, latitud_est, longitud_est, 
               critica_mensual, disponibilidad)

#Opción 4
transformacion(log_precio^(-lambday), log_min_noches, latitud_est, longitud_est, 
               critica_mensual, disponibilidad)

#Original
transformacion(exp(log_precio), exp(log_min_noches), latitud_est, longitud_est, 
               critica_mensual, disponibilidad)




#--------------------------------SIN DATOS ATÍPICOS---------------------------

#-------------------------------Análisis Descriptivo----

# Convertimos las columnas en vectores
log_precio_sda <- airbnb_sda$log_precio
log_min_noches_sda <- airbnb_sda$log_min_noches
critica_mensual_sda <- airbnb_sda$critica_mensual
disponibilidad_sda <- airbnb_sda$disponibilidad
latitud_est_sda<-airbnb_sda$latitud_est
longitud_est_sda<-airbnb_sda$longitud_est
habitacion_sda<-airbnb_sda$habitacion
distrito_sda<-airbnb_sda$distrito
meses_sda<- airbnb_sda$meses
  
#Resumen
modelo_sda<-lm(log_precio_sda~log_min_noches_sda+latitud_est_sda+longitud_est_sda+
                 critica_mensual_sda+disponibilidad_sda+distrito_sda+habitacion_sda+meses_sda)
summary(modelo_sda)
summary(airbnb_sda)

#---Precio----
hist(log_precio_sda, col = "lightblue4",freq = FALSE,main="Logaritmo de precio SDA")
lines(density(log_precio_sda),col="darkblue",lwd=2)
abline(v=mean(log_precio_sda),lwd=2,col="purple")
legend("topright", legend=c("Asimetría: 0.12","Curtosis: 2.49"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(log_precio_sda);  kurtosis(log_precio_sda)
summary(log_precio_sda)

#---Minimo de noches----
hist(log_min_noches_sda, col = "orange",freq = FALSE,main="Logarítmico de mínimo de noches SDA")
lines(density(log_min_noches_sda),col="red",lwd=2)
abline(v=mean(log_min_noches_sda),lwd=2,col="yellow")
legend("topright", legend=c("Asimetría: 1.16","Curtosis: 3.79"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(log_min_noches_sda);  kurtosis(log_min_noches_sda)
summary(log_min_noches_sda)

#---Crítica mensual----
hist(critica_mensual_sda, col = "green3",freq = FALSE,main="Crítica Mensual SDA")
lines(density(critica_mensual_sda),col="darkgreen",lwd=2)
abline(v=mean(critica_mensual_sda),lwd=2,col="black")
legend("topright", legend=c("Asimetría: 1.78","Curtosis: 8.43"), col= "white", cex=1,lwd=2)
#Asimetría    #Curtósis
skewness(critica_mensual_sda);  kurtosis(critica_mensual_sda)
boxplot(critica_mensual_sda, main= "Critica mensual SDA")

#---Disponibilidad----
hist(disponibilidad_sda, col = "pink",freq = FALSE,main="Disponibilidad SDA")
lines(density(disponibilidad_sda),col="violet",lwd=2)
abline(v=mean(disponibilidad_sda),lwd=2,col="purple")
legend("topright", legend=c("Asimetría: 0.17","Curtosis: 1.56"), col= "white", cex=.7,lwd=1)
#Asimetría                 #Curtósis
skewness(disponibilidad_sda);  kurtosis(disponibilidad_sda)
boxplot(disponibilidad, main="Disponibilidad SDA")

#---Latitud----
hist(latitud_est_sda, col = "yellow",freq = FALSE,main="Latitud Estandar SDA")
lines(density(latitud_est_sda),col="orange3",lwd=2)
abline(v=mean(latitud_est),lwd=2,col="red")
legend("topleft", legend=c("Asimetría: 0.23","Curtosis: 3.03"), col= "white", cex=.7,lwd=2)
#Asimetría    #Curtósis
skewness(latitud_est_sda);  kurtosis(latitud_est_sda)


#---Longitud----
hist(longitud_est_sda, col = "#D4E6F1",freq = FALSE,main="Longitud Estandarizada SDA")
lines(density(longitud_est_sda),col="#48C9B0",lwd=2)
abline(v=mean(longitud_est_sda),lwd=2,col="white")
legend("topleft", legend=c("Asimetría: 1.08","Curtosis: 6.69"), col= "white", cex=,lwd=2)
#Asimetría    #Curtósis
skewness(longitud_est_sda);  kurtosis(longitud_est_sda)

#---Tipo de habitación----
barplot(table(habitacion_sda), col = "#66B3FF", main = "Tipo de habitación")
summary(habitacion_sda)

boxplot(airbnb_sda$log_precio~airbnb_sda$habitacion,pch = 19, cex = .3,col = "#F6DDCC", xlab = "Tipo de habitación SDA", ylab ="Logaritmo del precio SDA" , main = "Boxplot")

#---Distrito----
barplot(table(distrito_sda), col = "#FFD9EC", main = "Distrito SDA")
summary(distrito_sda)

boxplot(airbnb_sda$log_precio~airbnb_sda$distrito,pch = 19, cex = .5,col = "#FFFFCC", xlab = "Distrito", ylab ="Logaritmo del precio SDA" , main = "Boxplot SDA")

#---Meses----
barplot(table(meses_sda), col = "orange3", main = "Meses de crítica SDA")
summary(meses_sda)

boxplot(airbnb_sda$log_precio~airbnb_sda$meses,pch = 19, cex = .5,col = "lightblue2", xlab = "Meses de crítica", ylab ="Logaritmo del precio SDA" , main = "Boxplot SDA")
grid(20,20, col="lightblue3")


#---Correlación de variables-----
airbnb_cont_sda<-data.frame(latitud_est_sda, longitud_est_sda, log_precio_sda, log_min_noches_sda, critica_mensual_sda)
corrplot(cor(airbnb_cont_sda))

airbnb_num_sda<-data.frame(latitud_est_sda, longitud_est_sda, log_precio_sda, log_min_noches_sda, critica_mensual_sda, disponibilidad_sda)
pairs(airbnb_num_sda, cex = .1, pch =19, col= "orange3", upper.panel = NULL)

#Factores de inflación de la varianza
vif(modelo_sda)

#------------------------------Análisis estadístico----

#Resumen del modelo
summary(modelo_sda)

#Pruebas de de hipótesis de significancia
anova(modelo_sda)

#Intervalos de confianza de los parámetros
confint(modelo_sda)


#-----------------------------Analisis de Residuales-----
#----Homocedasticidad----
error_estud_sda <- rstudent(modelo_sda)
error_est_sda <- rstandard(modelo_sda)

log_precio_ajust_sda <- fitted(modelo_sda)
#   Graficas
plot(log_precio_ajust_sda, error_estud_sda, main="Homocedasticidad SDA",col="pink3",cex=.3,pch=19,ylab="Errorres estudentizados", xlab="Y gorro")
abline(0,0, col="magenta", lw=2)
abline(2,0, col="black", lw=2, lty=4)
abline(-2,0, col="black", lw=2, lty=4)
#   Pruebas Estadisticas
bptest(modelo_sda)
ncvTest(modelo_sda)

#----No Correlación----
#   Grafica
plot(error_estud_sda,main="No correlación SDA",cex=.3,xlab="Subindice",ylab="R.estudentizados",pch=19, col="yellow3")
abline(0,0, col="orange3", lw=2)
abline(2,0, col="red2", lw=2, lty=4)
abline(-2,0, col="red2", lw=2, lty=4)

#Gráfica de autocorrelacion parcial
pacf(error_estud_sda, main= "Autocorrelación parcial SDA", col="red4",lag=1000)
#Gráfica de autocorrelacion
acf(error_estud_sda, main= "Serie de los residuos SDA", col="red4",lag=1000)

#   Pruebas Estadisticas
dwtest(modelo_sda)
bgtest(modelo_sda)

summary(modelo_sda)
#----Normalidad-----
#   Graficas
qqnorm(error_est_sda, pch=11, col="green4",lwd=3,cex=.1,main = "Normalidad SDA")
qqline(error_est_sda, col="orange", lwd=2)

hist(error_est_sda, col = "yellow3",freq = FALSE,main="Error estandarizados SDA")
lines(density(rnorm(10000)),col="orange3",lwd=2)
abline(v=mean(error_est_sda),lwd=2,col="red")

legend("topright", legend=c("Asimetría: 0.24","Curtosis: 2.71"), col= "white", cex=1,lwd=2)
skewness(error_est_sda);  kurtosis(error_est_sda)

#   Pruebas Estadisticas
ad.test(error_est_sda)




#-------------------------------Transformaciones--------
#BoxCox
bc_sda<-boxCox(modelo_sda, lambda=seq(-1,1,.1))
max(bc_sda$y)
lambday_sda<- bc_sda$x[bc_sda$y == max(bc_sda$y)]
lambday_sda

log_precio_sda_bc<-log_precio_sda^lambday_sda

#Creamos una función que nos diga lo que nos interesa al decidir
#un modelo (Sin Datos Atípicos) luego de una transformación.
transformacion_sda <- function(yt, minimo_noches,latitus,longitus,critic_mens,disponibilidas){
  opcion <- lm(yt~minimo_noches + latitus + longitus + critic_mens + disponibilidas + habitacion_sda + distrito_sda + meses_sda)
  s <- summary(opcion)
  print(paste0("R2: ", s$r.squared))
  print(paste0("R2 ajustada : ", s$adj.r.squared))
  
  print("Sobre los residuos:")
  
  #No correlación
  bg <- bgtest(opcion)
  if(bg$p.value > 0.05){
    print(paste0("No hay correlación (Brush-Godfry). P-value: ", bg$p.value ))
  }
  else{
    print(paste0("Sí hay correlación (Brush-Godfry). P-value: ", bg$p.value ))
  }
  
  dW <- dwtest(opcion)
  if(dW$p.value > 0.05){
    print(paste0("No hay correlación (Durbin-Watson). P-value: ", dW$p.value ))
  }
  else{
    print(paste0("Sí hay correlación (Durbin-Watson). P-value: ", dW$p.value ))
  }
  #Homocedasticidad
  bp <- (bptest(opcion))
  if(bp$p.value > 0.05){
    print(paste0("Sí hay varianza constante (Brush-Pagan). P-value: ", bp$p.value ))
  }
  else{
    print(paste0("No hay varianza constante (Brush-Pagan). P-value: ", bp$p.value ))
  }
  
  n_c_v <- ncvTest(opcion)
  if(n_c_v$p > 0.05){
    print(paste0("Sí hay varianza constante (Non-Constant-Variance). P-value: ", n_c_v$p))
  }
  else{
    print(paste0("No hay varianza constante (Non-Constant-Variance). P-value: ", n_c_v$p))
  }
  
  #Normalidad
  ad <- ad.test(opcion$residuals)
  if(ad$p.value > 0.05){
    print(paste0("Sí hay normalidad (Anderson-Darlin). P-value: ", ad$p.value ))
  }
  else{
    print(paste0("No hay normalidad (Anderson-Darlin). P-value: ", ad$p.value ))
  }
  
  ## Outliers
  n <- length(yt)
  p <- length(opcion$coefficients)
  error_estud <- rstudent(opcion)
  indices <- c()
  for (i in 1:n) {
    if(abs(error_estud[i])>=qt(.975,n-p)){
      indices <- c(indices,i)
    }
  }
  #Número de datos atípicos
  print(paste0("Porcentaje de datos atípicos:", (length(indices)/length(yt))*100, "%"))
  
}

#Opción 1
transformacion_sda(log_precio_sda_bc, log_min_noches_sda, latitud_est_sda,
                   longitud_est_sda, critica_mensual_sda, disponibilidad_sda)

#Opción 2
transformacion_sda((log_precio_sda)^(1/3), log_min_noches_sda, latitud_est_sda,
                   longitud_est_sda, critica_mensual_sda, disponibilidad_sda)

#Opción 3
transformacion_sda(log(log_precio_sda), log_min_noches_sda, latitud_est_sda,
                   longitud_est_sda, critica_mensual_sda, disponibilidad_sda)

#Opción 4
transformacion_sda(sqrt(log_precio_sda), log_min_noches_sda, latitud_est_sda,
                   longitud_est_sda, critica_mensual_sda, disponibilidad_sda)
#Original
transformacion_sda(exp(log_precio_sda), exp(log_min_noches_sda), latitud_est_sda,
                   longitud_est_sda, critica_mensual_sda, disponibilidad_sda)
















#----------------------------------------------------APLICACIONES---------
#----------Ejemplo 1
#Mínimo de noches: 7 
#Latitud: 40.777594
#Longitud: -73.962734
#Crítica mensual: 2
#Disponibilidad: 365
#Tipo de habitación: Departamento (Entire home/apt) 
#Distrito: Manhattan
#Mes: Junio (jun)

#     Solución-
#Tranformar varables
latitud_est_ej1 <- (40.777594-mean(latitud))/sd(latitud)
longitud_est_ej1 <- (-73.962734-mean(longitud))/sd(longitud)
log_min_noches_ej1<-log(7)

ej1 <- data.frame(log_min_noches_sda=log_min_noches_ej1, latitud_est_sda=latitud_est_ej1, longitud_est_sda =
          longitud_est_ej1, critica_mensual_sda=2, disponibilidad_sda= 365, distrito_sda = factor(c("Manhattan")), 
          habitacion_sda=factor(c("Entire home/apt")), meses_sda = factor(c("jun")))

#Intervalos de Prediccion del precio
intervalo_prediccion_ej1 <- predict(modelo_sda, ej1, interval = "prediction")
exp(intervalo_prediccion_ej1)
#Intervalos de Confianza del precio
intervalo_confianza_ej1<-predict(modelo_sda, ej1, interval = "confidence")
exp(intervalo_confianza_ej1)


#---------Ejemplo 2
#Mínimo de noches: 3
#Latitud: 40.746922
#Longitud: -73.834151
#Crítica mensual: 0.1
#Disponibilidad: 150
#Tipo de habitación: Recamara compartida (Shared room)
#Distrito: Queens
#Mes: Agosto (ago)

#     Solución-
#Tranformar varables
latitud_est_ej2 <- (40.746922-mean(latitud))/sd(latitud)
longitud_est_ej2 <- (-73.834151-mean(longitud))/sd(longitud)
log_min_noches_ej2 <-log(3)

ej2<- data.frame(log_min_noches_sda=log_min_noches_ej2, latitud_est_sda=latitud_est_ej2, longitud_est_sda =
                   longitud_est_ej2, critica_mensual_sda=0.1, disponibilidad_sda= 150, distrito_sda = factor(c("Queens")), 
                 habitacion_sda=factor(c("Shared room")), meses_sda = factor(c("ago")))

#Intervalos de Prediccion del precio
intervalo_prediccion_ej2<-predict(modelo_sda, ej2, interval = "prediction")
exp(intervalo_prediccion_ej2)
#Intervalos de Confianza del precio
intervalo_confianza_ej2<-predict(modelo_sda, ej2, interval = "confidence")
exp(intervalo_confianza_ej2)



as.Date("2020-09-01")-as.Date("2020-11-01")





#----------




