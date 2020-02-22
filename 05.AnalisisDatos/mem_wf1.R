
library(scales)

library(dplyr)
library(tibble)
library(modeest)
library(raster)
library(moments)
library(ggplot2)
library(caret)
library(lubridate)
library(lattice)
library(beanplot)
library(corrplot)
library(tidyr)

width.default <- getOption("width")
options(width=120)
options(scipen=3)

library(dummies)

##############
#  libraries #
##############
library(lattice)
library(caret)

#############
# functions #
#############

# Muestra resumen del archivo #
nelems=function(d) paste(nrow(d),"x",ncol(d))

# Saca espacios de string#
trimf = function(sentenceString){
  searchString <- ' '
  replacementString <- ''
  return( gsub(searchString,replacementString,sentenceString) )
}

# Convertir nro con formato '02' #
convNum2D = function(num){
  numConv = trimf(paste('0',num))
  numConv = substr( numConv, nchar(numConv)-1, nchar(numConv) )
  return (numConv)
}

#cleanCommill = function(sentenceString){
#  searchString <- '"'
#  replacementString <- ''
#  return( gsub(searchString,replacementString,sentenceString) )
#}

library(ggplot2)
par(mar = rep(2, 4))

### Abrir archivo train y hacer copia ###
f_wf1 <- read.csv("/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/wf1_r.csv", sep=",")
f_wf1_conv <- f_wf1

indices <- createDataPartition(f_wf1_conv$date, p=0.1, list=F)
f_wf1_mues <- f_wf1_conv[indices,]
f_wf1_conv <- f_wf1_mues

f_train_conv <- read.csv("/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/train_conv_r.csv", sep=",")

# eliminar NA y nulos#
cat("f_wf1_conv",nelems(f_wf1_conv),"\n")

na.omit(f_wf1_conv)
f_wf1_conv[sapply(f_wf1_conv, is.null)] <- NULL

cat("f_wf1_conv",nelems(f_wf1_conv),"\n")

# Separaci??n fecha campos   #
f_wf1_conv$anno  <- as.numeric( substr( f_wf1_conv[,c(1)], 1,4) )
f_wf1_conv$mes  <-  as.numeric( substr( f_wf1_conv[,c(1)], 5,6) )
f_wf1_conv$dia  <-  as.numeric( substr( f_wf1_conv[,c(1)], 7,8) )
f_wf1_conv$horsObs  <- as.numeric( substr( f_wf1_conv[,c(1)], 9,10) )
f_wf1_conv$daysAdd  <- "-"
f_wf1_conv$horsFore  <- "-"
f_wf1_conv$dateFore  <- "-"
f_wf1_conv$wp1  <- "-"

# Prepara fecha #
for (i in 1:nrow(f_wf1_conv)){
  sum <- 0
  mes <- substr( f_wf1_conv[i,"date"], 5,6)
  dia <- substr( f_wf1_conv[i,"date"], 7,8)
  f_wf1_conv[i, "horsFore"] <- convNum2D(f_wf1_conv[i, "hors"])
  if (f_wf1_conv[i, "horsObs"]==0){
    if(f_wf1_conv[i, "hors"] > 24){ 
      sum <- 1
      f_wf1_conv[i, "horsFore"] <- convNum2D(f_wf1_conv[i, "hors"]-24)
    }
  }else{
    if(f_wf1_conv[i, "hors"] > 12 & f_wf1_conv[i, "hors"] <= 36){ 
      sum <- 1
      f_wf1_conv[i, "horsFore"] <- convNum2D(f_wf1_conv[i, "hors"]-12)
    }else if (f_wf1_conv[i, "hors"] > 36) {
      sum <- 2
      f_wf1_conv[i, "horsFore"] <- convNum2D(f_wf1_conv[i, "hors"]-36)
    } 
  }
  f_wf1_conv[i, "daysAdd"] <- sum
  fechaForeStr <- trimf(paste (f_wf1_conv[i, "anno"],"-", mes,"-", dia))
  fechaFore <- as.Date( fechaForeStr ) + sum
  fechaStr <- paste ( format(fechaFore,'%Y'), format(fechaFore,'%m'), format(fechaFore,'%d'))
  horaFore <- f_wf1_conv[i, "horsFore"]
  if (horaFore==24){
    horaFore <- convNum2D(0)
  }
  f_wf1_conv[i, "dateFore"] <- trimf(paste(fechaStr,horaFore))

}

# Elimina campos de apoyo#

f_wf1_conv$daysAdd <- NULL
f_wf1_conv$horsFore <- NULL

#f_wf1_conv$annoFore  <- as.numeric( substr( f_wf1_conv[,c(13)], 1,4) )
#f_wf1_conv$mesFore  <-  as.numeric( substr( f_wf1_conv[,c(13)], 5,6) )
#f_wf1_conv$diaFore  <-  as.numeric( substr( f_wf1_conv[,c(13)], 7,8) )

# Agregar data wp1 #

#temp1 <- f_wf1_conv[which(nchar(f_wf1_conv$wp1) == 0 ), ]
#temp1 <- f_train_conv[which(f_train_conv$date == "2009070210" ), ]

  for (i in 1:nrow(f_wf1_conv)){
    temp_train <- f_train_conv[which(f_train_conv$date == f_wf1_conv[i, "dateFore"] ), ]
    f_wf1_conv[i, "wp1"] <- as.numeric(temp_train[1, "wp1"])
  
  }
# ==========

f_wf1_temp <- f_wf1_conv[which(f_wf1_conv$wp1 == "-" |  is.na(f_wf1_conv$wp1)), ]
cat("f_wf1_temp",nelems(f_wf1_temp),"\n")
cat("f_wf1_conv",nelems(f_wf1_conv),"\n")
f_wf1_fin <-  f_wf1_conv[which(f_wf1_conv$wp1 != "-" | !is.na(f_wf1_conv$wp1) ), ]
cat("f_wf1_fin",nelems(f_wf1_conv),"\n")

f_wf1_fin$dateFore <- NULL
f_wf1_fin$u <- NULL
f_wf1_fin$v <- NULL
f_wf1_fin$date <- NULL

write.csv(f_wf1_fin,"/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/reg_wf_r.csv", row.names = FALSE)

###
library(MASS)

f_fin <- read.csv("/Users/manuelmoya/Documents/USACH/2019-1er/Tesis/1.Desarrollo/05.AnalisisDatos/Datos/reg_wf_r.csv", sep=",")


cor(x=f_fin$anno,
    y=f_fin$wp1)

cor(f_fin)

hist(f_wf1_conv$horsAdd)


# medias #

### HISTOGRAMAS ###

par(mar = rep(2, 4))
hist(train_file_conv$anno ,
     prob = TRUE,
     col="#FAFAFA",#rainbow(20),
     xlab="Edad",
     ylab="Frecuencia Relativa",
     ##breaks=5,
     main="Histograma de Personas que Contactadas por Edad")
lines(density(train_file_conv$anno ), col="green",lwd=3)
abline(v=mean(train_file_conv$anno ), col="red", lwd=3)    #Media
abline(v=median(train_file_conv$anno ), col="blue", lwd=3) #Mediana




colnames(train_file) <- c("date","wp1","wp2","wp3","wp4","wp5","wp6", "wp7", "anno" )


bank <- read.csv("H:/Mi unidad/Estudios/Ing Civil Inform?tica/4to Semestre/Analisis de Datos y BI/Taller/Lab1/bank-full.csv", sep=";")
bank["accept"] <- ifelse(bank$y=="yes", 1, 0)
bank.dummy <- dummy.data.frame(bank, names=c("marital","job","default","education","housing","contact","loan","poutcome","y"), sep=".")
bank.dummy$month = NULL
bank.dummy.yes = bank.dummy[bank.dummy$y.yes==1,]
bank.dummy.no = bank.dummy[bank.dummy$y.no==1,]


### Agrupar Columnas con valores num?ricos ###
bp.age <- c(-Inf, 20, 25,30,35,40,45,50,55,60,Inf)
grp.age <- c("Hasta 20 A?os",
             "Entre 21 y 25 A?os", 
             "Entre 26 y 30 A?os", 
             "Entre 31 y 35 A?os", 
             "Entre 36 y 40 A?os", 
             "Entre 41 y 45 A?os", 
             "Entre 46 y 50 A?os", 
             "Entre 51 y 55 A?os", 
             "Entre 56 y 60 A?os", 
             "Mayor a 61 A?os")
bank$age.cat <- cut(bank$age, breaks = bp.age, labels = grp.age)

### Crear fecha de contacto ###
bank["month_num"] <- ifelse(bank$month=="jan", 1,
                     ifelse(bank$month=="feb", 2,
                     ifelse(bank$month=="mar",3,
                     ifelse(bank$month=="apr",4,
                     ifelse(bank$month=="may",5,
                     ifelse(bank$month=="jun",6,
                     ifelse(bank$month=="jul",7,
                     ifelse(bank$month=="aug",8,
                     ifelse(bank$month=="sep",9,
                     ifelse(bank$month=="oct",10,
                     ifelse(bank$month=="nov",11,
                     ifelse(bank$month=="dec",12,0))))))))))))
bank["date"] <- as.Date(paste("2012",bank$month_num,bank$day,sep="-"))

### Escalado de Variables continuas ###
#rescale.many <- function(dataframe, cols){
#  names <- names(dataframe)
#  for(col in cols){
#    name <- paste(names[col], "rescaled", sep=".")
#    dataframe[name] <- rescale(dataframe[,col])
#  }
#  cat(paste("Se ha(n) rescalado ", length(cols), "columna(s)..."))
#  dataframe
#}
#bank <- rescale.many(bank, c(1,6,10,12,13,14,15))

### Distribuci?n Normal (Camp Gauss)  de Variables  ###
scale.many <- function(dataframe, cols){
  names <- names(dataframe)
  for(col in cols){
    name <- paste(names[col], "z", sep=".")
    dataframe[name] <- scale(dataframe[,col])
  }
  cat(paste("Se ha(n) estandarizado ", length(cols), "columna(s)..."))
  dataframe
}
bank <- scale.many(bank, c(1,6,10,12:15))
bank[order(bank$date),]

### DATASETS FILTRADOS  ###
bank_yes = bank[bank$y=="yes",]
bank_no = bank[bank$y=="no",]

############################
############################
### ANALISIS DESCRIPTIVO ###
############################
############################


### MEDIAS ###

# TODOS
mean(bank$age)
mean(bank$balance)
mean(bank$duration)
# ACEPTARON
mean(bank_yes$age)
mean(bank_yes$balance)
mean(bank_yes$duration)
# NO ACEPTARON
mean(bank_no$age)
mean(bank_no$balance)
mean(bank_no$duration)


### MEDIANAS ###

# TODOS
median(bank$age)
median(bank$balance)
median(bank$duration)
# ACEPTARON
median(bank_yes$age)
median(bank_yes$balance)
median(bank_yes$duration)
# NO ACEPTARON
median(bank_no$age)
median(bank_no$balance)
median(bank_no$duration)


### MODAS ###

# TODOS
mfv(bank$age)
mfv(bank$balance)
mfv(bank$duration)
# ACEPTARON
mfv(bank_yes$age)
mfv(bank_yes$balance)
mfv(bank_yes$duration)
# NO ACEPTARON
mfv(bank_no$age)
mfv(bank_no$balance)
mfv(bank_no$duration)


### QUANTILES ###

# TODOS
quantile(bank$age)
quantile(bank$balance)
quantile(bank$duration)
# ACEPTARON
quantile(bank_yes$age)
quantile(bank_yes$balance)
quantile(bank_yes$duration)
# NO ACEPTARON
quantile(bank_no$age)
quantile(bank_no$balance)
quantile(bank_no$duration)


### MEDIDAS DE DISPERSI?N ###

### VARIANZA ###

# TODOS
var(bank$age)
var(bank$balance)
var(bank$duration)
# ACEPTARON
var(bank_yes$age)
var(bank_yes$balance)
var(bank_yes$duration)
# NO ACEPTARON
var(bank_no$age)
var(bank_no$balance)
var(bank_no$duration)


### DESVIACI?N ESTANDAR ###

# TODOS
sd(bank$age)
sd(bank$balance)
sd(bank$duration)
# ACEPTARON
sd(bank_yes$age)
sd(bank_yes$balance)
sd(bank_yes$duration)
# NO ACEPTARON
sd(bank_no$age)
sd(bank_no$balance)
sd(bank_no$duration)


### COEFICIENTE DE VARIACI?N ###

# TODOS
cv(bank$age)
cv(bank$balance)
cv(bank$duration)
# ACEPTARON
cv(bank_yes$age)
cv(bank_yes$balance)
cv(bank_yes$duration)
# NO ACEPTARON
cv(bank_no$age)
cv(bank_no$balance)
cv(bank_no$duration)


### MEDIDAS DE ASIMETR?A ###

### ASIMETRIA DE FISHER ###

# TODOS
moments::skewness(bank$age)
moments::skewness(bank$balance)
moments::skewness(bank$duration)
# ACEPTARON
moments::skewness(bank_yes$age)
moments::skewness(bank_yes$balance)
moments::skewness(bank_yes$duration)
# NO ACEPTARON
moments::skewness(bank_no$age)
moments::skewness(bank_no$balance)
moments::skewness(bank_no$duration)


### CURTOSIS ###

# TODOS
kurtosis(bank$age)
kurtosis(bank$balance)
kurtosis(bank$duration)
# ACEPTARON
kurtosis(bank_yes$age)
kurtosis(bank_yes$balance)
kurtosis(bank_yes$duration)
# NO ACEPTARON
kurtosis(bank_no$age)
kurtosis(bank_no$balance)
kurtosis(bank_no$duration)


### GRAFICOS DE INTRODUCCI?N ###

par(mfrow=c(1,1))


### HISTOGRAMA POR EDAD ###

hist(bank$age,
     prob = TRUE,
     col="#FAFAFA",#rainbow(20),
     xlab="Edad",
     ylab="Frecuencia Relativa",
     breaks =15,
     main="Histograma de Personas que Contactadas por Edad")
lines(density(bank$age), col="green",lwd=3)
abline(v=mean(bank$age), col="red", lwd=3)    #Media
abline(v=median(bank$age), col="blue", lwd=3) #Mediana

hist(bank_yes$age,
     prob = TRUE,
     col="#FAFAFA",#rainbow(20),
     xlab="Edad",
     ylab="Frecuencia Relativa",
     breaks = 25,
     main="Histograma de Personas que Contactadas por Edad - Aceptaron")
lines(density(bank_yes$age), col="green",lwd=3)
abline(v=mean(bank_yes$age), col="red", lwd=3)    #Media
abline(v=median(bank_yes$age), col="blue", lwd=3) #Mediana

hist(bank_no$age,
     prob = TRUE,
     col="#FAFAFA",#rainbow(20),
     xlab="Edad",
     ylab="Frecuencia Relativa",
     breaks = 15,
     main="Histograma de Personas que Contactadas por Edad - Aceptaron")
lines(density(bank_no$age), col="green",lwd=3)
abline(v=mean(bank_no$age), col="red", lwd=3)    #Media
abline(v=median(bank_no$age), col="blue", lwd=3) #Mediana


#########################
# GRAFICOS DE ATRIBUTOS #
#########################

library(dplyr)

# Atributo Job
bank.summary <- bank_yes %>% group_by(job, y) %>% count(job)
bank.summary
g = ggplot(bank.summary, aes(x=job, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Job por Clases") +
  ylab("Cantidad")
g
#ggsave('job.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Marital
bank.summary <- bank_yes %>% group_by(marital) %>% count(marital)
bank.summary
g = ggplot(bank.summary, aes(x=marital, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Marital por Clases") +
  ylab("Cantidad")
g  
#ggsave('marital.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Education 
bank.summary <- bank_yes %>% group_by(education) %>% count(education)
bank.summary
g = ggplot(bank.summary, aes(x=education, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Education por Clases") +
  ylab("Cantidad")
g  
#ggsave('education.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo default 
bank.summary <- bank_yes %>% group_by(default) %>% count(default)
bank.summary
g = ggplot(bank.summary, aes(x=default, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Default por Clases") +
  ylab("Cantidad")
g  
#ggsave('default.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Housing 
bank.summary <- bank_yes %>% group_by(housing) %>% count(housing)
bank.summary
g = ggplot(bank.summary, aes(x=housing, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Housing por Clases") +
  ylab("Cantidad")
g  
#ggsave('housing.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Loan 
bank.summary <- bank_yes %>% group_by(loan) %>% count(loan)
bank.summary
g = ggplot(bank.summary, aes(x=loan, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Loan por Clases") +
  ylab("Cantidad")
g  
#ggsave('loan.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Contact 
bank.summary <- bank_yes %>% group_by(contact) %>% count(contact)
bank.summary
g = ggplot(bank.summary, aes(x=contact, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Contact por Clases") +
  ylab("Cantidad")
g  
#ggsave('contact.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Day 
bank.summary <- bank_yes %>% group_by(day) %>% count(day)
bank.summary
g = ggplot(bank.summary, aes(x=day, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Day por Clases") +
  ylab("Cantidad")
g  
#ggsave('day.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Month 
bank.summary <- bank_yes %>% group_by(month, month_num) %>% count(month, month_num)
bank.summary
g = ggplot(bank.summary, aes(x=month_num, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Month por Clases") +
  ylab("Cantidad") 
g  
#ggsave('month.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Previous 
bank.summary <- bank_yes %>% group_by(previous) %>% count(previous) 
bank.summary
g = ggplot(bank.summary, aes(x=previous, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Previous por Clases") +
  ylab("Cantidad") 
g  
#ggsave('previous.png', plot = last_plot(), scale = 1, device="jpg" )

# Atributo Poutcome 
bank.summary <- bank_yes %>% group_by(poutcome) %>% count(poutcome) 
bank.summary
g = ggplot(bank.summary, aes(x=poutcome, y=n ) ) +
  geom_bar(show.legend = T, stat = "identity") +
  labs(title="An?lisis atributo Poutcome por Clases") +
  ylab("Cantidad") 
g  
#ggsave('poutcome.png', plot = last_plot(), scale = 1, device="jpg" )



#### Relaci?n entre Edad y Cantidad de Llamadas Previos en Casos Aceptados  ####

xyplot(bank_yes$age ~ bank_yes$pdays | bank_yes$marital, data = bank_yes,
       main="Relaci?n entre Edad y Cantidad de Llamadas Previos en Casos Aceptados",
       xlab="Cantidad de Contactos Previos",
       ylab="Edad")

#### Histograma Duraci?n de Contactos Aceptados seg?n la Duraci?n   ####

hist(bank_yes$duration,
     prob = F,
     col="#FAFAFA",#rainbow(20),
     xlab="Duraci?n",
     ylab="Frecuencia",
     breaks = 20,
     main="Histograma de Contactos Aceptados seg?n la Duraci?n")
abline(v=mean(bank_yes$duration), col="red", lwd=3)    #Media

bank_yes.contact.telephone <- bank_yes[bank_yes$contact=="telephone",]
hist(bank_yes.contact.telephone$duration,
     prob = F,
     col="#FAFAFA",#rainbow(20),
     xlab="Duraci?n",
     ylab="Frecuencia",
     breaks = 20,
     main="Histograma de Contactos Aceptados por Tel?fono seg?n la Duraci?n")
abline(v=mean(bank_yes.contact.telephone$duration), col="red", lwd=3) #Media

bank_yes.contact.cellular <- bank_yes[bank_yes$contact=="cellular",]
hist(bank_yes.contact.cellular$duration,
     prob = F,
     col="#FAFAFA",#rainbow(20),
     xlab="Duraci?n",
     ylab="Frecuencia",
     breaks = 20,
     main="Histograma de Contactos Aceptados por Celular seg?n la Duraci?n")
abline(v=mean(bank_yes.contact.cellular$duration), col="red", lwd=3) #Media

xyplot(bank_yes$age ~ bank_yes$duration | bank_yes$contact, data = bank_yes,
       main="Relaci?n entre Edad y Duraci?n de los Contactos en Casos Aceptados",
       xlab="Duraci?n",
       ylab="Edad")




## CHI CUADRADO ## 

library(reshape2)

# 1)	Casos de Aceptaci?n de la Propuesta seg?n Estado Civil
bank.res <- bank %>% group_by(y, marital)  %>% count(y)
bank.res.matrix <- dcast(bank.res, y~paste0('',marital), value.var='n')
bank.res.matrix
bank.res.matrix <- subset(bank.res.matrix, select=c("divorced", "married","single"))
chisq.test(bank.res.matrix)

# 2)	Casos de Aceptaci?n de la Propuesta seg?n Tipo de Contacto
bank.res <- bank %>% group_by(y, contact)  %>% count(y)
bank.res.matrix <- dcast(bank.res, y~paste0('',contact), value.var='n')
bank.res.matrix
bank.res.matrix <- subset(bank.res.matrix, select=c("cellular", "telephone","unknown"))
chisq.test(bank.res.matrix)

# 2)	Casos de Aceptaci?n de la Propuesta seg?n Tipo de Contacto
bank.res <- bank %>% group_by(y, contact)  %>% count(y)
bank.res.matrix <- dcast(bank.res, y ~ paste0('',contact), value.var='n')
bank.res.matrix
bank.res.matrix <- subset(bank.res.matrix, select=c("cellular", "telephone","unknown"))
chisq.test(bank.res.matrix)


# 3)	Casos Aceptado: Educacion vs Hipotecario
bank_yes["housing01"] <- ifelse(bank_yes$housing=="yes", 1, 0) 
bank.res <- bank_yes %>% group_by(housing, education)  %>% count(housing)
bank.res.matrix <- dcast(bank.res, housing ~ paste0('',education), value.var='n')
bank.res.matrix
bank.res.matrix <- subset(bank.res.matrix, select=c("primary", "secondary","unknown","unknown"))
chisq.test(bank.res.matrix)

