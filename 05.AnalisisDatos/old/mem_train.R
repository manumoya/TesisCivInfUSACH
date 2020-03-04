### Instalar Paquetes, Importar Librerias Y Configurar Script ### 
library(ggplot2)

### Abrir archivo train y hacer copia ###
train_file <- read.csv("/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/train_r.csv", sep=",")
train_file_conv <- train_file

# Crear campos   #
train_file_conv$anno  <- as.numeric( substr( train_file[,c(1)], 1,4) )
train_file_conv$mes  <-  as.numeric( substr( train_file[,c(1)], 5,6) )
train_file_conv$dia  <-  as.numeric( substr( train_file[,c(1)], 7,8) )
train_file_conv$hora  <- as.numeric( substr( train_file[,c(1)], 9,10) )

### HISTOGRAMAS ###

par(mar = rep(2, 4))
hist(train_file_conv$wp1)
hist(train_file_conv$anno)
hist(train_file_conv$anno ,
     prob = TRUE,
     col="#FAFAFA",#rainbow(20),
     xlab="Edad",
     ylab="Frecuencia Relativa",
     breaks=15,
     main="Histograma de Personas que Contactadas por Edad")
lines(density(train_file_conv$anno ), col="green",lwd=3)
abline(v=mean(train_file_conv$anno ), col="red", lwd=3)    #Media
abline(v=median(train_file_conv$anno ), col="blue", lwd=3) #Mediana


write.csv(train_file_conv,"/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/train_conv_r.csv", row.names = FALSE)

