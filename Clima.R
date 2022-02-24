#carichiamo data

setwd("C:\\Users\\emanu\\Desktop\\Ema\\Unisi\\Laurea Magistrale\\1° Anno 2° Semestre\\Inferenza Statistica\\Dati tesine")
library(readxl)
Data<-read_excel("City-climate.xlsx")

#togliamo na

Data<-na.omit(Data)
attach(Data)

#aggiustiamo coordinate longitudine

Data$Long<-(-Long)
table(State)

as.numeric(Long)
as.numeric(Lat)
as.numeric(JanTF)
as.numeric(JulyTF)

#matrice scatter plot 

plot(Data[, 2:7])
cor(Data[, 2:7])

boxplot(Rain ~ State)

######Analisi esplorativa univariata
setwd("C:\\Users\\emanu\\Desktop\\Emanuele\\Clima")

png("box_jan.png")
boxplot(JanTF, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Temperatura media Gennaio (F°)")
rug(JanTF, side = 2)
dev.off()
summary(JanTF)


#Dal boxplot vediamo la presenza di un valore anomalo (67), però tralasciando ciò possiamo già intravedere
#la forma della distribuzione, che sembra avere una certa simmetria, forse con una leggera asimmetria 
#positiva. Il valore mediano è 31 che non si discosta molto dalla media 33.8; inoltre il 50% delle 
#osservazioni è compreso tra 27 e 39.5.

png("box_jul.png")
boxplot(JulyTF, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Temperatura media Luglio (F°)")
rug(JulyTF, side = 2)
dev.off()
summary(JulyTF)


#Anche qui dal boxplot possiamo notare la presenza di alcuni valore anomali, per l'esattezza 3, e qui
#la distribuzione sembra essere simmetrica. Il valore mediano e la media qui sono quasi identici, 
#rispettivamente 74 e 74.41; è da notare inoltre che il 50% delle osservazioni è compreso qui in 
#un range molto più piccolo ossia tra 72 e 77.

png("box_umi.png")
boxplot(RelHum, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Umidità relativa (%)")
rug(RelHum, side = 2)
dev.off()
summary(RelHum)


#Qui, così come per JulyTF, abbiamo un range, per il 50% delle osservazioni, molto piccolo, ossia tra 55.5 e 60
#però qui abbiamo più valori anomali, 5, che si allontanano anche di molto dal valore mediano 57 e medio 57.75
#(anche qui molto simili). La simmetria sembra essere abbastanza ragionevole.

png("box_rain.png")
boxplot(Rain, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Quantità di pioggia (l/m2)")
rug(Rain, side = 2)
dev.off()
summary(Rain)

#Anche qui ci troviamo difronte ad alcuni valori anomali, 7. Media e mediana tendono allo stesso valore
#anche qui, rispettivamente 38.51 e 38; in questo caso sembra esserci una leggera asimmetria negativa.


#mettiamo a confronto i boxplot delle due temperature

boxplot(JanTF, JulyTF, boxwex = 0.2, main = "Box-and-whiskers plot", 
        ylab = "Temperature medi Gennaio-Luglio")


#Andando a vedere un boxplot congiunto delle due variabili riguardo la TF notiamo che, ovviamente,
#in July le temperature sono nettamente più alte, ma ciò che incuriosisce è che durante July le
#temperature sono molto più concentrate, ossia hanno un piccolo range, mentre in January non è così, anzi
#alcune temperature massime raggiungono le minime di July.


####creiamo 3 funzioni

variance <- function(x){
  m2 <- sum((x - mean(x))^2)/length(x)
  m2}
skewness <- function(x){
  s3 <- sum((x - mean(x))^3)/length(x)/sqrt(variance(x))^3
  s3}
kurtosis <- function(x){
  s4 <- sum((x - mean(x))^4)/length(x)/variance(x)^2
  s4}

#applichiamo le funzioni alle variabili
#JanuaryTF

variance(JanTF)
skewness(JanTF)
kurtosis(JanTF)

#Var=101.31, Asimm=0.99, Curtosi=4.04

#JulyTF

variance(JulyTF)
skewness(JulyTF)
kurtosis(JulyTF)

#Var=20.81, Asimm=0.06, Curtosi=2.94

#RelHum

variance(RelHum)
skewness(RelHum)
kurtosis(RelHum)

#Var=28.46, Asimm=0.20, Curtosi=6.88

#Rain

variance(Rain)
skewness(Rain)
kurtosis(Rain)

#Var=131.67, Asimm=-0.17, Curtosi=3.80


######Istogrammi
#1
png("hist_jan.png")
hist(JanTF)
dens<-density(JanTF)
hist(JanTF,probability = T, ylim = c(0, 0.06), main = "Istogramma", 
     xlab = "Temperatura media Gennaio (°F)", ylab = "Densità")
lines(dens)
dev.off()

#2
png("hist_jul.png")
hist(JulyTF)
dens1<-density(JulyTF)
hist(JulyTF,probability = T, ylim = c(0, 0.11), main = "Istogramma",
     xlab = "Temperatura media Luglio (°F)", ylab = "Densità")
lines(dens1)
dev.off()

#3
png("hist_umi.png")
hist(RelHum)
dens2<-density(RelHum)
hist(RelHum,probability = T, ylim = c(0, 0.11), main = "Istogramma", 
     xlab = "Umidità relativa (%)", ylab = "Densità")
lines(dens2)
dev.off()

#4
png("hist_rain.png")
hist(Rain)
dens3<-density(Rain)
hist(Rain,probability = T, ylim = c(0, 0.06), main = "Istogramma", 
     xlab = "Quantità di pioggia (l/m2)", ylab = "Densità")
lines(dens3)
dev.off()


#Istogrammi da sistemare le classi

library(lattice)
png("barchart_state.png")
barchart(table(State), xlab = "Frequenza", main = "Barplot")
dev.off()

table(State)

#Solo per vedere quante osservazioni per ogni regione, non ci dice niente di più.

#######Analisi bivariata

#Vediamo il tutto con una matrice dei diagrammi a dispersione
png("plot_tot.png")
plot(Data[, 2:7])
dev.off()

png("plot_long_rain.png")
plot(JulyTF, RelHum, xlab = "Longitudine (°)", ylab = "Quantità di pioggia (l/m2)", main = "Scatter plot")
dev.off()

#...

plot(Lat, JulyTF)

#...

plot(Lat, Rain)

#Per vedere casomai una dipendenza non lineare (regressione locale)

plot(Long, JanTF)

#...

plot(Long, JulyTF)

#...

plot(Long, Rain)

#...

plot(JanTF, JulyTF)

#...

plot(JanTF, Rain)

#...

plot(JulyTF, Rain)

#...

plot(RelHum, Rain)

#...

cor(Data[, 2:7])

#Per vedere anche numericamente queste dipendenze
#Da notare i valori alti tra Lat-JanTF, Lat-JulyTF, Long-Rain, etc...
#Ora passiamo ad una analisi delle variabili quantitative condizionate con la variabile qualitative State.

png("box_janstate.png")
boxplot(JanTF ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Temperatura media Gennaio (°F)",
        main = "Box-and-whiskers plot")
dev.off()

#Considerare valori anomali Minneapolis e Miami

png("box_julstate.png")
boxplot(JulyTF ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Temperatura media Luglio (°F)",
        main = "Box-and-whiskers plot")
dev.off()

#Valore anomalo Louisville

png("box_umistate.png")
boxplot(RelHum ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Umidità relativa (%)",
        main = "Box-and-whiskers plot")
dev.off()


#Particolare la situazione del West (Denver)

png("box_rainstate.png")
boxplot(Rain ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Quantità di pioggia (l/m2) ",
        main = "Box-and-whiskers plot")
dev.off()

#Valori anomali Worchester e York



########Passiamo ad una analisi a gruppo, facendo scatter plot condizionati.

library(lattice)
png("xy_lat_jan.png")
xyplot(Data$Lat ~ Data$JanTF | Data$State , xlab = "Temperatura media Gennaio (°F)",   
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Latitudine (°)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si

png("xy_lat_july.png")
xyplot(Data$Lat ~ Data$JulyTF | Data$State , xlab = "Temperatura media Luglio (°F)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Latitudine (°)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si


xyplot(Data$Lat ~ Data$Rain | Data$State , xlab = "Quantità di pioggia (l/m2)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Latitudine (°)",
       main = "Scatter plot condizionato a Stati")
#no

png("xy_lat_umi.png")
xyplot(Data$Lat ~ Data$RelHum | Data$State , xlab = "Umidità relativa (%)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Latitudine (°)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si

png("xy_long_jan.png")
xyplot(Data$Long ~ Data$JanTF | Data$State , xlab = "Temperatura media Gennaio (°F)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Longitudine (°)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si

png("xy_long_july.png")
xyplot(Data$Long ~ Data$JulyTF | Data$State , xlab = "Temperatura media Luglio (°F)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Longitudine (°)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si


xyplot(Data$Long ~ Data$RelHum | Data$State , xlab = "Umidità relativa (%)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Longitudine (°)",
       main = "Scatter plot condizionato a Stati")
#no


xyplot(Data$Long ~ Data$Rain | Data$State , xlab = "Quantità di pioggia (l/m2)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Longitudine (°)",
       main = "Scatter plot condizionato a Stati")
#no

png("xy_jan_july.png")
xyplot(Data$JanTF ~ Data$JulyTF | Data$State , xlab = "Temperatura media Luglio (°F)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Temperatura media Gennaio (°F)",
       main = "Scatter plot condizionato a Stati")
dev.off()
#si


xyplot(Data$JanTF ~ Data$Rain | Data$State , xlab = "Quantità di pioggia (l/m2)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Temperatura media Gennaio (°F)",
       main = "Scatter plot condizionato a Stati")
#no

xyplot(Data$JulyTF ~ Data$Rain | Data$State , xlab = "Quantità di pioggia (l/m2)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Temperatura media Luglio (°F)",
       main = "Scatter plot condizionato a Stati")
#no


xyplot(Data$RelHum ~ Data$Rain | Data$State , xlab = "Quantità di pioggia (l/m2)",
       strip = strip.custom(strip.names = F, strip.levels = T),
       ylab = "Umidità relativa (%)",
       main = "Scatter plot condizionato a Stati")

#...

plot(Data[, 2:7], pch = 21, bg = c("red", "blue", "black", "green"),
        main = "Scatter-plot matrix (Red=F, Blue=M)")

#...

#########Funzione di ripartizione empirica
#1
png("ecdf_jan.png")
plot(ecdf(JanTF), do.points = F, verticals = T,
     xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
rug(JanTF)
dev.off()

#2
png("ecdf_july.png")
plot(ecdf(JulyTF), do.points = F, verticals = T,
     xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
rug(JulyTF)
dev.off()

#3
png("ecdf_umi.png")
plot(ecdf(RelHum), do.points = F, verticals = T,
     xlab = "Umidità relativa (%)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
rug(RelHum)
dev.off()

#4
png("ecdf_rain.png")
plot(ecdf(Rain), do.points = F, verticals = T,
     xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
rug(Rain)
dev.off()

#...

#######Mappa USA

library(maps)
Data$New_Long<-(-Long)
attach(Data)

#map<-map("state", interior = FALSE, )
#map("state", boundary = FALSE, col="gray", add = TRUE, plot = T)
#map.axes(cex.axis=0.8)



library(maps)
library(ggplot2)
usa_map <- map_data("state")

#colore gennaio, low="#002FA7", high="84C3BE"
#colore luglio, low="84C3BE", high="#FF0000"

#alternative

#colore gennaio, low="84C3BE", high="#FFDB58"
#colore luglio, low="#FFDB58", high="#FF0000"

#p=gennaio
p <- ggplot() + coord_fixed() +
  xlab("Longitudine (°)") + ylab("Latitudine (°)") + scale_color_gradient(low="84C3BE", high="#FFDB58") 
  

#p1=luglio
p1 <- ggplot() + coord_fixed() +
  xlab("Longitudine (°)") + ylab("Latitudine (°)") + scale_color_gradient(low="#FFDB58", high="#FF0000")

  ?scale_color_gradient

#Add map to base plot
base_world_messy_gen <- p + geom_polygon(data=usa_map, aes(x=long, y=lat, group=group), 
                                     colour="black", fill= "white")
base_world_messy_lug <- p1 + geom_polygon(data=usa_map, aes(x=long, y=lat, group=group), 
                                         colour="black", fill= "white")

#cleanup <- 
#  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#        panel.background = element_rect(fill = 'white', colour = 'white'), 
#        axis.line = element_line(colour = "white"), legend.position="none",
#        axis.ticks=element_blank(), axis.text.x=element_blank(),
#        axis.text.y=element_blank())

#base_world <- base_world_messy + cleanup

summary(JanTF)
summary(JulyTF)

map_data_coloured_jan <-base_world_messy_gen +  geom_point(data=Data,
                      aes(x=as.numeric(Long), y=as.numeric(Lat), colour=(JanTF)), size=6, alpha=I(1.6))  
                      + theme_bw()
map_data_coloured_jan

map_data_coloured_jul <-base_world_messy_lug +  geom_point(data=Data, 
                      aes(x=as.numeric(Long), y=as.numeric(Lat), colour=(JulyTF)), size=6, alpha=I(1.6)) 
                      + theme_bw()
map_data_coloured_jul

library(grid)
library(gridExtra)
png("mappausa.png")
grid.arrange(map_data_coloured_jan,map_data_coloured_jul, nrow=2)
dev.off()




#######Massima verosimiglianza

mean(JanTF)
var(JanTF)

mean(JulyTF)
var(JulyTF)

mean(RelHum)
var(RelHum)

mean(Rain)
var(Rain)

qqnorm(JanTF)
qqline(JanTF)
shapiro.test(JanTF)

#...
png("qqplot_jul.png")
qqnorm(JulyTF)
qqline(JulyTF)
dev.off()
shapiro.test(JulyTF)

#Normalità qui

qqnorm(RelHum)
qqline(RelHum)
shapiro.test(RelHum)

#...
png("qqplot_rain.png")
qqnorm(Rain)
qqline(Rain)
dev.off()
shapiro.test(Rain)

#...

########Stimatori di nucleo

library(sm)
#JanTF

sm.density(JanTF, 1.0, yht = 0.12, xlim = c(15, 60),
             xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 1.00)")

sm.density(JanTF, 2.0, yht = 0.12, xlim = c(15, 60),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 2.00)")

sm.density(JanTF, 3.0, yht = 0.12, xlim = c(15, 60),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 3.00)")

#JulyTF

sm.density(JulyTF, 1.0, yht = 0.12, xlim = c(60, 90),
           xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 1.00)")

sm.density(JulyTF, 2.0, yht = 0.12, xlim = c(60, 90),
           xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 2.00)")

sm.density(JulyTF, 3.0, yht = 0.12, xlim = c(60, 90),
           xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 3.00)")

#RelHum

sm.density(RelHum, 1.0, yht = 0.12, xlim = c(30, 80),
           xlab = "Umidità relativa (%)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 1.00)")

sm.density(RelHum, 2.0, yht = 0.12, xlim = c(30, 80),
           xlab = "Umidità relativa (%)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 2.00)")

sm.density(RelHum, 3.0, yht = 0.12, xlim = c(30, 80),
           xlab = "Umidità relativa (%)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 3.00)")

#Rain

sm.density(Rain, 1.0, yht = 0.08, xlim = c(0, 80),
           xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 1.00)")

sm.density(Rain, 2.0, yht = 0.08, xlim = c(0, 80),
           xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 2.00)")

sm.density(Rain, 3.0, yht = 0.08, xlim = c(0, 80),
           xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità")
title(main = "Stima della densità Kernel (h = 3.00)")

##Parametro di smorzamento automatico
#JanTF_Plugin

par(mfrow=c(1,2))

png("dens_jan.png")
par(mfrow=c(1,2))
sm.density(JanTF, hsj(JanTF), yht= 0.12, xlim = c(15, 60),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità")
title(main = "Stima densità ('Plug-in h=2.85')")

#JanTF_CV

sm.density(JanTF, hcv(JanTF, hstart = 1, hend = 4.00),
             yht = 0.12, xlim = c(15, 60), xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità")
title(main = "Stima densità ('CV h=3.15')")
dev.off()

#JulyTF_Plugin
png("den_july.png")
par(mfrow=c(1,2))
sm.density(JulyTF, hsj(JulyTF), yht= 0.12, xlim = c(60, 90),
           xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità")
title(main = "Stima densità ('Plug-in h=1.49')")

#JulyTF_CV

sm.density(JulyTF, hcv(JulyTF, hstart = 1, hend = 4.00),
           yht = 0.12, xlim = c(60, 90), xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità")
title(main = "Stima densità ('CV h=1.78')")
dev.off()

#RelHum_Plugin
png("dens_umi.png")
par(mfrow=c(1,2))
sm.density(RelHum, hsj(RelHum), yht= 0.12, xlim = c(30, 80),
           xlab = "Umidità relativa (%)", ylab = "Probabilità")
title(main = "Stima densità ('Plug-in h=1.50')")

#RelHum_CV

sm.density(RelHum, hcv(RelHum, hstart = 1, hend = 4.00),
           yht = 0.12, xlim = c(30, 80), xlab = "Umidità relativa (%)", ylab = "Probabilità")
title(main = "Stima densità ('CV h=1.79')")
dev.off()

#Rain_Plugin
png("dens_rain.png")
par(mfrow=c(1,2))
sm.density(Rain, hsj(Rain), yht= 0.08, xlim = c(0, 80),
           xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità")
title(main = "Stima densità ('Plug-in h=3.35')")

#Rain_CV

sm.density(Rain, hcv(Rain, hstart = 1, hend = 4.00),
           yht = 0.08, xlim = c(0, 80), xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità")
title(main = "Stima densità ('CV h=1.75')")
dev.off()

########Stimatori di nucleo bivariati

#hcv
summary(JanTF)
hcv_long_jan<-c(hcv(Long, hstart = 1, hend = 5), hcv(JanTF, hstart = 1, hend = 4))

png("biv_long_jan.png")
par(mfrow=c(1, 2))
plot(-Long, JanTF, xlim = c(-125, -65), ylim = c(10, 70),
     xlab = "Longitudine(°)", ylab = "Temperatura media Gennaio (°F)")
sm.density(Data[, c(3, 4)], hcv_long_jan, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima densità ('CV h1=3.16 h2=3.15')")

sm.density(Data[, c(3, 4)], hcv_long_jan, display = "image", xlim = c(-125, -65), ylim = c(10, 70),
           xlab = "Longitudine (°)", ylab = "Temperatura media Gennaio (°F)")
title(main = "Stima densità ('CV h1=3.16 h2=3.15')")
dev.off()





#Lat, JanTF
par(mfrow=c(1,2))
hcv(Lat, hstart = 1, hend = 4)
hcv(JanTF, hstart = 1, hend = 4)
hcv_lat_jan<-c(hcv(Lat, hstart = 1, hend = 4), hcv(JanTF, hstart = 1, hend = 4))
sm.density(Data[, c(4, 5)], hcv_lat_jan, xlim=c(15, 60), ylim=c(60, 90), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 1.43, h2 = 3.15)")
hsj_lat_jan<-c(hsj(Lat), hsj(JanTF))
sm.density(Data[, c(4, 5)], hsj_lat_jan, xlim=c(15, 60), ylim=c(60, 90), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 0.06, h2 = 0.03)")


summary(Lat)
summary(JanTF)

#hcv
png("biv_lat_jan.png")
par(mfrow=c(1, 2))
plot(Lat, JanTF, xlim = c(20, 55), ylim = c(10, 70),
     xlab = "Latitudine(°)", ylab = "Temperatura media Gennaio (°F)")
sm.density(Data[, c(2, 4)], hcv_lat_jan, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima densità ('CV h1=1.44 h2=3.15')")

sm.density(Data[, c(2, 4)], hcv_lat_jan, display = "image", xlim = c(20, 55), ylim = c(10, 70),
           xlab = "Latitudine (°)", ylab = "Temperatura media Gennaio (°F)")
title(main = "Stima densità ('CV h1=1.44 h2=3.15')")
dev.off()
#h1=3.15, h2=1.78

#hsj
par(mfrow=c(1, 2))
plot(Lat, JanTF, xlim = c(15, 60), ylim = c(60, 90),
     xlab = "lat", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 5)], hsj_lat, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('Plug-in')")

sm.density(Data[, c(4, 5)], hsj_lat, display = "image", xlim = c(15, 60), ylim = c(60, 90),
           xlab = "lat", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('Plug-in')")
#h1=2.85, h2=1.49





#JanTF, JulyTF
par(mfrow=c(1,2))
hcv(JanTF, hstart = 1, hend = 4)
hcv(JulyTF, hstart = 1, hend = 4)
hcv<-c(hcv(JanTF, hstart = 1, hend = 4), hcv(JulyTF, hstart = 1, hend = 4))
sm.density(Data[, c(4, 5)], hcv, xlim=c(15, 60), ylim=c(60, 90), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 0.06, h2 = 0.03)")
hsj<-c(hsj(JanTF), hsj(JulyTF))
sm.density(Data[, c(4, 5)], hsj, xlim=c(15, 60), ylim=c(60, 90), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 0.06, h2 = 0.03)")

#hcv
png("biv_jan_lug.png")
par(mfrow=c(1, 2))
plot(JanTF, JulyTF, xlim = c(15, 60), ylim = c(60, 90),
       xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 5)], hcv, display = "slice",
             props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('CV h1=3.15 h2=1.78')")

sm.density(Data[, c(4, 5)], hcv, display = "image", xlim = c(15, 60), ylim = c(60, 90),
             xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('CV h1=3.15 h2=1.78')")
dev.off()
#h1=3.15, h2=1.78


#hsj
par(mfrow=c(1, 2))
plot(JanTF, JulyTF, xlim = c(15, 60), ylim = c(60, 90),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 5)], hsj, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('Plug-in')")

sm.density(Data[, c(4, 5)], hsj, display = "image", xlim = c(15, 60), ylim = c(60, 90),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('Plug-in')")
#h1=2.85, h2=1.49




#JanTF, RelHum
par(mfrow=c(1,2))
hcv(JanTF, hstart = 1, hend = 4)
hcv(RelHum, hstart = 1, hend = 4)
hcv1<-c(hcv(JanTF, hstart = 1, hend = 4), hcv(RelHum, hstart = 1, hend = 4))
sm.density(Data[, c(4, 6)], hcv1, xlim=c(15, 60), ylim=c(30, 80), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 3.15, h2 = 1.8)")
hsj1<-c(hsj(JanTF), hsj(RelHum))
sm.density(Data[, c(4, 6)], hsj1, xlim=c(15, 60), ylim=c(30, 80), zlim=c(0, 0.007))
title(main = "Kernel density estimation ('CV' h1 = 2.85, h2 = 1.5)")

#hcv
par(mfrow=c(1, 2))
plot(JanTF, RelHum, xlim = c(15, 60), ylim = c(30, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 6)], hcv1, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('CV')")

sm.density(Data[, c(4, 6)], hcv1, display = "image", xlim = c(15, 60), ylim = c(30, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('CV')")
#h1=3.15, h2=1.8

#hsj
par(mfrow=c(1, 2))
plot(JanTF, RelHum, xlim = c(15, 60), ylim = c(30, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 6)], hsj1, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('Plug-in')")

sm.density(Data[, c(4, 6)], hsj1, display = "image", xlim = c(15, 60), ylim = c(30, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('Plug-in')")
#h1=2.85, h2=1.5




#JanTF, Rain
par(mfrow=c(1,2))
hcv(JanTF, hstart = 1, hend = 4)
hcv(Rain, hstart = 1, hend = 4)
hcv2<-c(hcv(JanTF, hstart = 1, hend = 4), hcv(Rain, hstart = 1, hend = 4))
sm.density(Data[, c(4, 7)], hcv2, xlim=c(15, 60), ylim=c(0, 80), zlim=c(0, 0.004))
title(main = "Kernel density estimation ('CV' h1 = 3.15, h2 = 1.8)")
hsj2<-c(hsj(JanTF), hsj(Rain))
sm.density(Data[, c(4, 7)], hsj2, xlim=c(15, 60), ylim=c(0, 80), zlim=c(0, 0.004))
title(main = "Kernel density estimation ('CV' h1 = 2.85, h2 = 1.5)")

#hcv
par(mfrow=c(1, 2))
plot(JanTF, Rain, xlim = c(15, 60), ylim = c(0, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 7)], hcv2, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('CV')")

sm.density(Data[, c(4, 7)], hcv2, display = "image", xlim = c(15, 60), ylim = c(0, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('CV')")
#h1=3.15 h2=1.75

#hsj
par(mfrow=c(1, 2))
plot(JanTF, Rain, xlim = c(15, 60), ylim = c(0, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(4, 7)], hsj2, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('Plug-in')")

sm.density(Data[, c(4, 7)], hsj2, display = "image", xlim = c(15, 60), ylim = c(0, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('Plug-in')")
#h1=2.85 h2=3.35




#JulyTF, RelHum
par(mfrow=c(1,2))
hcv(JulyTF, hstart = 1, hend = 4)
hcv(RelHum, hstart = 1, hend = 4)
hcv3<-c(hcv(JulyTF, hstart = 1, hend = 4), hcv(RelHum, hstart = 1, hend = 4))
sm.density(Data[, c(5, 6)], hcv3, xlim=c(60, 90), ylim=c(30, 80), zlim=c(0, 0.01))
title(main = "Kernel density estimation ('CV' h1 = 1.78, h2 = 1.8)")
hsj3<-c(hsj(JulyTF), hsj(RelHum))
sm.density(Data[, c(5, 6)], hsj3, xlim=c(60, 90), ylim=c(30, 80), zlim=c(0, 0.01))
title(main = "Kernel density estimation ('CV' h1 = 1.49, h2 = 1.5)")

#hcv
par(mfrow=c(1, 2))
plot(JulyTF, RelHum, xlim = c(60, 90), ylim = c(30, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(5, 6)], hcv3, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('CV')")

sm.density(Data[, c(5, 6)], hcv3, display = "image", xlim = c(60, 90), ylim = c(30, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('CV')")
#h1=3.15 h2=1.75

#hsj
par(mfrow=c(1, 2))
plot(JulyTF, RelHum, xlim = c(60, 90), ylim = c(30, 80),
     xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
sm.density(Data[, c(5, 6)], hsj3, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima della densità Kernel ('Plug-in')")

sm.density(Data[, c(5, 6)], hsj3, display = "image", xlim = c(60, 90), ylim = c(30, 80),
           xlab = "Temperatura media Gennaio (°F)", ylab = "Temperatura media Luglio (°F)")
title(main = "Stima della densità Kernel ('Plug-in')")
#h1=1.49 h2=1.5




#JulyTF, Rain
par(mfrow=c(1,2))
hcv(JulyTF, hstart = 1, hend = 4)
hcv(Rain, hstart = 1, hend = 4)
hcv4<-c(hcv(JulyTF, hstart = 1, hend = 4), hcv(Rain, hstart = 1, hend = 4))
sm.density(Data[, c(5, 7)], hcv4, xlim=c(60, 90), ylim=c(0, 80), zlim=c(0, 0.006))
title(main = "Kernel density estimation ('CV' h1 = 1.78, h2 = 1.75)")
hsj4<-c(hsj(JulyTF), hsj(Rain))
sm.density(Data[, c(5, 7)], hsj4, xlim=c(60, 90), ylim=c(0, 80), zlim=c(0, 0.006))
title(main = "Kernel density estimation ('CV' h1 = 1.49, h2 = 3.35)")

#hcv
png("biv_jul_rain.png")
par(mfrow=c(1, 2))
plot(JulyTF, Rain, xlim = c(60, 90), ylim = c(0, 80),
     xlab = "Temperatura media Luglio (°F)", ylab = "Quantità di pioggia (l/m2)")
sm.density(Data[, c(5, 7)], hcv4, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Stima densità ('CV' h1=1.78, h2=1.75)")

sm.density(Data[, c(5, 7)], hcv4, display = "image", xlim = c(60, 90), ylim = c(0, 80),
           xlab = "Temperatura media Luglio (°F)", ylab = "Quantità di pioggia (l/m2)")
title(main = "Stima densità ('CV' h1=1.78, h2=1.75)")
dev.off()

#hsj
par(mfrow=c(1, 2))
plot(JulyTF, Rain, xlim = c(60, 90), ylim = c(0, 80),
     xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
sm.density(Data[, c(5, 7)], hsj2, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Kernel density estimation ('Plug-in' h1 = 2.85, h2 = 3.35)")

sm.density(Data[, c(5, 7)], hsj4, display = "image", xlim = c(60, 90), ylim = c(0, 80),
           xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
title(main = "Kernel density estimation ('Plug-in' h1 = 2.85, h2 = 3.35)")





#RelHum, Rain
par(mfrow=c(1,2))
hcv(RelHum, hstart = 1, hend = 4)
hcv(Rain, hstart = 1, hend = 4)
hcv5<-c(hcv(RelHum, hstart = 1, hend = 4), hcv(Rain, hstart = 1, hend = 4))
sm.density(Data[, c(6, 7)], hcv5, xlim=c(30, 80), ylim=c(0, 80), zlim=c(0, 0.006))
title(main = "Kernel density estimation ('CV' h1 = 1.8, h2 = 1.75)")
hsj5<-c(hsj(RelHum), hsj(Rain))
sm.density(Data[, c(6, 7)], hsj5, xlim=c(30, 80), ylim=c(0, 80), zlim=c(0, 0.006))
title(main = "Kernel density estimation ('CV' h1 = 1.5, h2 = 3.35)")

#hcv
par(mfrow=c(1, 2))
plot(RelHum, Rain, xlim = c(30, 80), ylim = c(0, 80),
     xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
sm.density(Data[, c(6, 7)], hcv5, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Kernel density estimation ('CV' h1 = 3.15, h2 = 1.75)")

sm.density(Data[, c(6, 7)], hcv5, display = "image", xlim = c(30, 80), ylim = c(0, 80),
           xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
title(main = "Kernel density estimation ('CV' h1 = 3.15, h2 = 1.75)")

#hsj
par(mfrow=c(1, 2))
plot(RelHum, Rain, xlim = c(30, 80), ylim = c(0, 80),
     xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
sm.density(Data[, c(6, 7)], hsj5, display = "slice",
           props = c(75, 50, 25, 2), add = T)
title(main = "Kernel density estimation ('Plug-in' h1 = 2.85, h2 = 3.35)")

sm.density(Data[, c(6, 7)], hsj5, display = "image", xlim = c(30, 80), ylim = c(0, 80),
           xlab = "Temperature January (°F)", ylab = "Temperature July (°F)")
title(main = "Kernel density estimation ('Plug-in' h1 = 2.85, h2 = 3.35)")


#####Regressione lineare locale
#JanTF, Lat
par(mfrow=c(1, 3))
plot(Lat, JanTF, xlab = "Latitudine (°)",
       ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JanTF, h = 1.00, add = T)
title(main = "Regressione lineare locale (h = 1.00)")
plot(Lat, JanTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JanTF, h = 2.50, add = T)
title(main = "Regressione lineare locale (h = 2.50)")
plot(Lat, JanTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JanTF, h = 4.00, add = T)
title(main = "Regressione lineare locale (h = 4.00)")


#JanTF, Long
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JanTF, h = 1.00, add = T)
title(main = "Regressione lineare locale (h = 1.00)")
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JanTF, h = 3.00, add = T)
title(main = "Regressione lineare locale (h = 3.00)")
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JanTF, h = 4.20, add = T)
title(main = "Regressione lineare locale (h = 4.20)")

#JulyTF, Lat
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Lat, JulyTF, h = 0.5, add = T)
title(main = "Regressione lineare locale (h = 0.50)")
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Lat, JulyTF, h = 1.50, add = T)
title(main = "Regressione lineare locale (h = 1.50)")
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Lat, JulyTF, h = 2.50, add = T)
title(main = "Regressione lineare locale (h = 1.50)")

#JulyTF, Long
plot(Long, JulyTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Long, JulyTF, h = 1.5, add = T)
title(main = "Regressione lineare locale (h = 1.50)")
plot(Long, JulyTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Long, JulyTF, h = 2.5, add = T)
title(main = "Regressione lineare locale (h = 2.50)")
plot(Long, JulyTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(Long, JulyTF, h = 3.5, add = T)
title(main = "Regressione lineare locale (h = 3.50)")

#JulyTF, JanTF
plot(JanTF, JulyTF, xlab = "Temperatura media Gennaio (°F)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(JanTF, JulyTF, h = 1.5, add = T)
title(main = "Regressione lineare locale (h = 1.50)")
plot(JanTF, JulyTF, xlab = "Temperatura media Gennaio (°F)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(JanTF, JulyTF, h = 2.5, add = T)
title(main = "Regressione lineare locale (h = 2.50)")
plot(JanTF, JulyTF, xlab = "Temperatura media Gennaio (°F)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(JanTF, JulyTF, h = 3.5, add = T)
title(main = "Regressione lineare locale (h = 3.50)")

#Con scelta automatica

#JanTF, Lat
par(mfrow=c(1, 2))
plot(Lat, JanTF, xlab = "Latitudine (°)",
       ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JanTF, method = "df", add = TRUE)
plot(Lat, JanTF, xlab = "Latitudine (°)",
       ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JanTF, method = "cv", add = TRUE)

#JanTF, Long
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JanTF, method = "df", add = TRUE)
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JanTF, method = "cv", add = TRUE)

#JulyTF, Lat
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JulyTF, method = "df", add = TRUE)
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Lat, JulyTF, method = "cv", add = TRUE)

#JulyTF, Long
plot(Long, JulyTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)")
sm.regression(Long, JulyTF, method = "df", add = TRUE)
plot(Long, JulyTF, xlab = "Longitudine (°)",
     ylab = "CTemperatura media Gennaio (°F)")
sm.regression(Long, JulyTF, method = "cv", add = TRUE)

#JulyTF, JanTF
plot(JanTF, JulyTF, xlab = "Temperatura media Gennaio (°F)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(JanTF, JulyTF, method = "df", add = TRUE)
plot(JanTF, JulyTF, xlab = "Temperatura media Gennaio (°F)",
     ylab = "Temperatura media Luglio (°F)")
sm.regression(JanTF, JulyTF, method = "cv", add = TRUE)

#######Inferenza con 1 variabile

#Test dei segni
binom.test(length(JanTF[JanTF > 30]), length(JanTF),
             p = 1/2, alternative = "two.sided")

#Test di Wilcox
wilcox.test(JulyTF, alternative = "two.sided", mu = 75, conf.int = TRUE)

#Test permutazione
library(exactRankTests)
perm.test(round(JulyTF), paired = FALSE,
            alternative = "two.sided", mu = 75)

#Accettiamo l'ipotesi di base più nettamente rispetto al wilcox

#Test di Kolmogorov
#Janu
png("ecdf_jan_norm.png")
par(mfrow=c(1, 1))
set.seed(123)
normale<-rnorm(10000, 33.8, 10.15)
plot(ecdf(JanTF), do.points = F, verticals = T,
     xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione")
rug(JanTF)
curve(pnorm(x, mean(JanTF), sd(JanTF)), xlab = "x", ylab = "Probabilità", type = "s", col = "red",add = T, 
      main = "Theorical distribution function")
legend(55, 0.3, c("Empirica", "Teorica"), lty = c(1, 1), col = c("black", "red"))
dev.off()

ks.test(JanTF, "pnorm", 33.8, 10.15)


#July
png("ecdf_jul_norm.png")
par(mfrow=c(1, 1))
set.seed(123)
normale<-rnorm(10000, 74.4, 4.6)
plot(ecdf(JulyTF), do.points = F, verticals = T,
       xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità",
       main = "Funzione di ripartizione")
rug(JulyTF)
curve(pnorm(x, mean(JulyTF), sd(JulyTF)), xlab = "x", ylab = "Probabilità", type = "s", col = "red",add = T, 
     main = "Theorical distribution function")
legend(80, 0.3, c("Empirica", "Teorica"), lty = c(1, 1), col = c("black", "red"))
dev.off()

ks.test(JulyTF, "pnorm", 74.4, 4.6)

#RelHum
png("ecdf_umi_norm.png")
par(mfrow=c(1, 1))
set.seed(123)
normale<-rnorm(10000, 57.75, 5.37)
plot(ecdf(RelHum), do.points = F, verticals = T,
     xlab = "Umidità relativa (%)", ylab = "Probabilità",
     main = "Funzione di ripartizione")
rug(RelHum)
curve(pnorm(x, mean(RelHum), sd(RelHum)), xlab = "x", ylab = "Probabilità", type = "s", col = "red",add = T, 
      main = "Theorical distribution function")
legend(68, 0.3, c("Empirica", "Teorica"), lty = c(1, 1), col = c("black", "red"))
dev.off()

ks.test(RelHum, "pnorm", 57.75, 5.37)

#Rain
png("ecdf_rain_norm.png")
par(mfrow=c(1, 1))
set.seed(123)
normale<-rnorm(10000, 38.5, 11.57)
plot(ecdf(Rain), do.points = F, verticals = T,
     xlab = "Quantità di pioggia (l/m2)", ylab = "Probabilità",
     main = "Funzione di ripartizione")
rug(Rain)
curve(pnorm(x, mean(Rain), sd(Rain)), xlab = "x", ylab = "Probabilità", type = "s", col = "red",add = T, 
      main = "Theorical distribution function")
legend(55, 0.3, c("Empirica", "Teorica"), lty = c(1, 1), col = c("black", "red"))
dev.off()

ks.test(Rain, "pnorm", 38.5, 11.57)


##########Inferenza con più variabili

library(car)
median(Lat)
Data$Dummy_Lat<-c(1,1,1,0,0,0,1,1,1,1,0,1,0,1,0,0,0,0,1,1,1,
                  0,1,0,0,0,1,0,0,0,0,1,1,0,1,0,1,0,1,1,1,1,0,1,0,0,0,0,1,1,1,1,1,0,0,0,1,0,1)
  
median(-Long)
Data$Dummy_Long<-recode(-Long, "lo:-83.00=0; else=1")
  
attach(Data)
boxplot(JanTF ~ Dummy_Lat)
boxplot(JanTF ~ Dummy_Long)
boxplot(JulyTF ~ Dummy_Lat)
boxplot(JulyTF ~ Dummy_Long)

JanTF_Lat_0 <- split(JanTF, Dummy_Lat)[["0"]]
JanTF_Lat_1 <- split(JanTF, Dummy_Lat)[["1"]]

JanTF_Long_0 <- split(JanTF, Dummy_Long)[["0"]]
JanTF_Long_1 <- split(JanTF, Dummy_Long)[["1"]]

JulyTF_Lat_0 <- split(JulyTF, Dummy_Lat)[["0"]]
JulyTF_Lat_1 <- split(JulyTF, Dummy_Lat)[["1"]]

JulyTF_Long_0 <- split(JulyTF, Dummy_Long)[["0"]]
JulyTF_Long_1 <- split(JulyTF, Dummy_Long)[["1"]]

#Test T
#JanTF, Lat
library(sm)

par(mfrow=c(1,2))
hcv(JanTF_Lat_0, hstart = 0.5, hend = 4)
sm.density(JanTF_Lat_0, hcv(JanTF_Lat_0, hstart = 0.5, hend = 4),
             yht = 0.1, xlim = c(15, 60),
             xlab = "Temperatura media Gennaio (°F) <40° latitudine")
title(main = "Kernel density estimation ('CV' h = 2.27)")
hcv(JanTF_Lat_1, hstart = 0.5, hend = 4)
sm.density(JanTF_Lat_1, hcv(JanTF_Lat_1, hstart = 0.5, hend = 4),
             yht = 0.1, xlim = c(15, 60),
             xlab = "Temperatura media Gennaio (°F) >40° latitudine")
title(main = "Kernel density estimation ('CV' h = 2.39)")

var.test(JanTF_Lat_0, JanTF_Lat_1, paired = F, alternative = "two.sided")
t.test(JanTF_Lat_0, JanTF_Lat_1, var.equal = T, alternative = "two.sided")

#JanTF, Long
png("dummy_dens_long_gen.png")
par(mfrow=c(1,2))
hsj(JanTF_Long_0)
sm.density(JanTF_Long_0, hsj(JanTF_Long_0),
           yht = 0.1, xlim = c(15, 60),
           xlab =  "Gennaio (°F) <-83° longitudine")
title(main = "Stima densità ('Plugin' h = 5.53)")
hsj(JanTF_Long_1)
sm.density(JanTF_Long_1, hsj(JanTF_Long_1),
           yht = 0.1, xlim = c(15, 60),
           xlab = "Gennaio (°F) >-83° longitudine")
title(main = "Stima densità ('Plugin' h = 2.69)")
dev.off()

var.test(JanTF_Long_0, JanTF_Long_1, paired = F, alternative = "two.sided")
t.test(JanTF_Long_0, JanTF_Long_1, var.equal = T, alternative = "two.sided")

#JulyTF, Lat

par(mfrow=c(1,2))
hcv(JulyTF_Lat_0, hstart = 0.5, hend = 4)
sm.density(JulyTF_Lat_0, hcv(JulyTF_Lat_0, hstart = 0.5, hend = 4),
           yht = 0.1, xlim = c(60, 90),
           xlab = "Line 1 ball diameter (micron)")
title(main = "Kernel density estimation ('CV' h = 3.01)")
hcv(JulyTF_Lat_1, hstart = 0.1, hend = 4)
sm.density(JulyTF_Lat_1, hcv(JulyTF_Lat_1, hstart = 0.1, hend = 4),
           yht = 0.1, xlim = c(60, 90),
           xlab = "Line 2 ball diameter (micron)")
title(main = "Kernel density estimation ('CV' h = 0.37)")
#non va

var.test(JulyTF_Lat_0, JulyTF_Lat_1, paired = F, alternative = "two.sided")
t.test(JulyTF_Lat_0, JulyTF_Lat_1, var.equal = F, alternative = "two.sided")

#JulyTF, Long

png("dummy_dens_long_jul.png")
par(mfrow=c(1,2))
hsj(JulyTF_Long_0)
sm.density(JulyTF_Long_0, hsj(JulyTF_Long_0),
           yht = 0.17, xlim = c(45, 90),
           xlab = "Luglio (°F) <-83° longitudine")
title(main = "Stima densità ('Plugin' h = 3.70)")
hsj(JulyTF_Long_1)
sm.density(JulyTF_Long_1, hsj(JulyTF_Long_1),
           yht = 0.17, xlim = c(45, 90),
           xlab = "Luglio (°F) >-83° longitudine")
title(main = "Stima densità ('Plugin' h = 0.85)")
dev.off()
#non va

var.test(JulyTF_Long_0, JulyTF_Long_1, paired = F, alternative = "two.sided")
t.test(JulyTF_Long_0, JulyTF_Long_1, var.equal = F, alternative = "two.sided")

#Test di Mann-Whitney
by(JanTF, Dummy_Long, mean)
wilcox.test(JanTF ~ Dummy_Lat, alternative = "two.sided")
wilcox.test(JanTF ~ Dummy_Long, alternative = "greater")
wilcox.test(JulyTF ~ Dummy_Lat, alternative = "two.sided")
wilcox.test(JulyTF ~ Dummy_Long, alternative = "two.sided")

#Test di Permutazione

perm.test(round(JanTF_Lat_0), round(JanTF_Lat_1), paired = F,
            alternative = "two.sided")
perm.test(round(JanTF_Long_0), round(JanTF_Long_1), paired = F,
          alternative = "two.sided")
perm.test(round(JulyTF_Lat_0), round(JulyTF_Lat_1), paired = F,
          alternative = "two.sided")
perm.test(round(JulyTF_Long_0), round(JulyTF_Long_1), paired = F,
          alternative = "two.sided")

#Test di Kolmogorov-Smirnov
#Jan, Lat

par(mfrow=c(1,1))
plot(ecdf(JanTF_Lat_0), do.points = F, verticals = T, xlim = c(15, 60),
       lty = 1, xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità",
       main = "Funzione di ripartizione empirica")
plot(ecdf(JanTF_Lat_1), do.points = F, verticals = T, lty = 3, add = T)
legend(45, 0.3, c("Latitudine <40°", "Latitudine >40°"), lty = c(1, 3))

ks.test(JanTF_Lat_0, JanTF_Lat_1)

#Jan, Long

par(mfrow=c(1,1))
plot(ecdf(JanTF_Long_0), do.points = F, verticals = T, xlim = c(15, 60),
     lty = 1, xlab = "Temperatura media Gennaio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
plot(ecdf(JanTF_Long_1), do.points = F, verticals = T, lty = 3, add = T)
legend(45, 0.3, c("Longitudine <-83°", "Longitudine >-83°"), lty = c(1, 3))

ks.test(JanTF_Long_0, JanTF_Long_1)

#July, Lat

par(mfrow=c(1,1))
plot(ecdf(JulyTF_Lat_0), do.points = F, verticals = T, xlim = c(60, 90),
     lty = 1, xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
plot(ecdf(JulyTF_Lat_1), do.points = F, verticals = T, lty = 3, add = T)
legend(83, 0.3, c("Latitudine <40°", "Latitudine >40°"), lty = c(1, 3))

ks.test(JulyTF_Lat_0, JulyTF_Lat_1)

#July, Long

par(mfrow=c(1,1))
plot(ecdf(JulyTF_Long_0), do.points = F, verticals = T, xlim = c(60, 90),
     lty = 1, xlab = "Temperatura media Luglio (°F)", ylab = "Probabilità",
     main = "Funzione di ripartizione empirica")
plot(ecdf(JulyTF_Long_1), do.points = F, verticals = T, lty = 3, add = T)
legend(83, 0.3, c("Longitudine <-83°", "Longitudine >-83°"), lty = c(1, 3))

ks.test(JulyTF_Long_0, JulyTF_Long_1)

#######Analisi della varianza

#Jan, State
Jan_State_1 <- split(JanTF, State)[[1]]
Jan_State_2 <- split(JanTF, State)[[2]]
Jan_State_3 <- split(JanTF, State)[[3]]
Jan_State_4 <- split(JanTF, State)[[4]]


boxplot(JanTF ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Temperatura media Gennaio (°F)",
        main = "Box-and-whiskers plot")

table(State)
library(sm)
par(mfrow = c(2, 2))
sm.density(Jan_State_1, hnorm(Jan_State_1), yht = 0.1, xlim = c(15, 65),
             xlab = "Medio-Occidente", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Jan_State_2, hnorm(Jan_State_2), yht = 0.1, xlim = c(15, 65),
             xlab = "Nord-Est", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Jan_State_3, hnorm(Jan_State_3), yht = 0.1, xlim = c(15, 65),
             xlab = "Sud", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Jan_State_4, hnorm(Jan_State_4), yht = 0.1, xlim = c(15, 65),
             xlab = "Ovest", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
par(mfrow=c(1, 1))

summary(aov(JanTF ~ State))
TukeyHSD(aov(JanTF ~ State), "State")


attach(Data)

#July, State
July_State_1 <- split(JulyTF, State)[[1]]
July_State_2 <- split(JulyTF, State)[[2]]
July_State_3 <- split(JulyTF, State)[[3]]
July_State_4 <- split(JulyTF, State)[[4]]

par(mfrow = c(1, 1))
boxplot(JulyTF ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Temperatura media Luglio (°F)",
        main = "Box-and-whiskers plot conzionato a Stati")



library(sm)
par(mfrow = c(2, 2))
sm.density(July_State_1, hnorm(July_State_1), yht = 0.18, xlim = c(55, 90),
           xlab = "Medio-Occidente", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(July_State_2, hnorm(July_State_2), yht = 0.18, xlim = c(55, 90),
           xlab = "Nord-Est", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(July_State_3, hnorm(July_State_3), yht = 0.18, xlim = c(55, 90),
           xlab = "Sud", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(July_State_4, hnorm(July_State_4), yht = 0.18, xlim = c(55, 90),
           xlab = "Ovest", ylab = "Probabilità")
title(main = "Stima della densità Kernel")

summary(aov(JulyTF ~ State))
TukeyHSD(aov(JulyTF ~ State), "State")

#Relhum, State
Rel_State_1 <- split(RelHum, State)[[1]]
Rel_State_2 <- split(RelHum, State)[[2]]
Rel_State_3 <- split(RelHum, State)[[3]]
Rel_State_4 <- split(RelHum, State)[[4]]

par(mfrow = c(1, 1))


boxplot(RelHum ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Umidità relativa (%)",
        main = "Box-and-whiskers plot conzionato a Stati")


library(sm)
par(mfrow = c(2, 2))
sm.density(Rel_State_1, hnorm(Rel_State_1), yht = 0.18, xlim = c(35, 80),
           xlab = "Medio-Occidente", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rel_State_2, hnorm(Rel_State_2), yht = 0.18, xlim = c(35, 80),
           xlab = "Nord-Est", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rel_State_3, hnorm(Rel_State_3), yht = 0.18, xlim = c(35, 80),
           xlab = "Sud", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rel_State_4, hnorm(Rel_State_4), yht = 0.18, xlim = c(35, 80),
           xlab = "Ovest", ylab = "Probabilità")
title(main = "Stima della densità Kernel")

summary(aov(RelHum ~ State))
TukeyHSD(aov(RelHum ~ State), "State")

#Rain, State
Rain_State_1 <- split(Rain, State)[[1]]
Rain_State_2 <- split(Rain, State)[[2]]
Rain_State_3 <- split(Rain, State)[[3]]
Rain_State_4 <- split(Rain, State)[[4]]

par(mfrow = c(1, 1))

png("box_rain_cond.png")
boxplot(Rain ~ State, boxwex = 0.2, xlab = "Stati", names = c("Medio-Occidente", "Nord-Est", "Sud", "Ovest"),
        ylab = "Quantità di pioggi (l/m2)",
        main = "Box-and-whiskers plot conzionato a Stati")
dev.off()



library(sm)
par(mfrow = c(2, 2))
sm.density(Rain_State_1, hnorm(Rain_State_1), yht = 0.18, xlim = c(10, 70),
           xlab = "Medio-Occidente", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rain_State_2, hnorm(Rain_State_2), yht = 0.18, xlim = c(10, 70),
           xlab = "Nord-Est", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rain_State_3, hnorm(Rain_State_3), yht = 0.18, xlim = c(10, 70),
           xlab = "Sud", ylab = "Probabilità")
title(main = "Stima della densità Kernel")
sm.density(Rain_State_4, hnorm(Rain_State_4), yht = 0.18, xlim = c(10, 70),
           xlab = "Ovest", ylab = "Probabilità")
title(main = "Stima della densità Kernel")

summary(aov(Rain ~ State))
TukeyHSD(aov(Rain ~ State), "State")

#Test di Kruskall-Wallis

library(PMCMR)
kruskal.test(RelHum ~ State)
posthoc.kruskal.nemenyi.test(Rain ~ State)

kruskal.test(Rain ~ State)

#Test per l'indipendenza
#spearman, kendall

cor.test(Lat, JanTF, method = "pearson")
cor.test(Lat, JulyTF, method = "pearson")
cor.test(JulyTF, JanTF, method = "pearson")
cor.test(Rain, JulyTF, method = "pearson")
cor.test(Long, JanTF, method = "pearson")
cor.test(Long, JulyTF, method = "pearson")
cor.test(RelHum, JulyTF, method = "pearson")

cor(Data[, 2:7])

########Regressione (OLS)

#JanTF, Lat

par(mfrow = c(1, 1))
Reg<-lm(JanTF ~ Lat)
png("reg_jan_lat.png")
plot(Lat, JanTF, xlab = "Latitudine (°)", ylab = "Temperatura media Gennaio (°F)", main = "Scatter plot")
abline(Reg)
summary(Reg)
dev.off()

png("asc_jan_lat.png")
par(mfrow = c(2, 2))
plot(lm(JanTF ~ Lat), which = c(1:4), add.smooth = F)
dev.off()


par(mfrow = c(1, 1))
png("reg_lat_jan_ano.png")
plot(Lat, JanTF, xlab = "Latitudine (°)", ylab = "Temperatura media Gennaio (°F)", main = "Scatter plot")
abline(lm(JanTF ~ Lat))
text(x = Lat[49] + 0.2, y = JanTF[49], labels = "49", adj=0)
dev.off()

library(sm)
png("local_jan_lat.png")
plot(Lat, JanTF, xlab = "Latitudine (°)", 
     ylab = "Temperatura media Gennaio (°F)", main = "Regressione lineare locale")
sm.regression(Lat, JanTF, method = "df", add = T)
dev.off()

#togliendo l'intercetta(però significativa)
summary(lm(JanTF ~ -1 + Lat))

#togliendo l'intercettala bontà sale da 0.73 a 0.86

#togliendo valori anomali

summary(lm(JanTF ~ Lat, subset = (1:length(JanTF) !=  49)))
#togliendo i valori anomali 49 il modello sale a 0.83


#JanTF, Long
Reg1<-lm(JanTF ~ Long)
png("reg_jan_long.png")
plot(Long, JanTF)
abline(Reg1)
dev.off()
summary(Reg1)
#R^2=0.22, basso

png("asco_2.png")
par(mfrow = c(2, 2))
plot(lm(JanTF ~ Long), which = c(1:4), add.smooth = F)
dev.off()

par(mfrow = c(1, 1))
png("reg_jan_long.png")
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)", main = "Scatter plot")
abline(lm(JanTF ~ Long))
dev.off()
text(x = Long[31] + 0.3, y = JanTF[31], labels = "31", adj=0)
dev.off()

png("local_reg2.png")
plot(Long, JanTF, xlab = "Longitudine (°)",
     ylab = "Temperatura media Gennaio (°F)", main = "Scatter plot")
sm.regression(Long, JanTF, method = "df", add = T)
dev.off()

#togliendo valori anomali

summary(lm(JanTF ~ Long, subset = (1:length(JanTF) != 31)))

#intercetta non significativa
summary(lm(JanTF ~ -1 + Long, subset = (1:length(JanTF) != 31)))


#togliendo l'intercetta si passa da 0.22 a 0.95



#JulyTF, Lat
Reg2<-lm(JulyTF ~ Lat)
plot(Lat, JulyTF)
abline(Reg2)
summary(Reg2)

par(mfrow = c(2, 2))
plot(lm(JulyTF ~ Lat), which = c(1:4), add.smooth = F)

par(mfrow = c(1, 1))
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Luglio (°F)", main = "Scatter plot")
abline(lm(JulyTF ~ Lat))
text(x = Lat[46] + 0.3, y = JulyTF[46], labels = "46", adj=0)
text(x = Lat[47] + 0.3, y = JulyTF[47], labels = "47", adj=0)


library(sm)
plot(Lat, JulyTF, xlab = "Latitudine (°)",
     ylab = "Temperatura media Luglio (°F)", main = "Regressione lineare locale")
sm.regression(Lat, JulyTF, method = "df", add = T)

#togliendo valori anomali

summary(lm(JulyTF ~ Lat, subset = (1:length(JulyTF) != 47)))
#togliendo i valori anomali 47 e 48 il modello sale a 0.38 da 0.46




#JulyTF, Long
Reg3<-lm(JulyTF ~ Long)
plot(Long, JulyTF)
abline(Reg3)
summary(Reg3)

#R^2=0.07, bassissimo

#JulyTF, JanTF
Reg4<-lm(JulyTF ~ JanTF)
plot(JanTF, JulyTF)
abline(Reg4)
summary(Reg4)

#R^2=0.08, bassissimo


##########Regressione multipla
#1
summary(lm(JulyTF ~ Lat + Long + JanTF + Rain + RelHum + State))

#modello con tutte la variabili da 0.84

summary(step(lm(JulyTF ~ Lat + Long + JanTF + Rain + RelHum + State)))
#Facendo AIC abbiamo che modello il migliore si ha togliendo Lat

summary(step(lm(JulyTF ~ Long + JanTF + Rain + RelHum + State)))



par(mfrow = c(2, 2))
plot(lm(JulyTF ~ Long + JanTF + Rain + RelHum + State), which = c(1:4),
       add.smooth = FALSE)
par(mfrow = c(1, 1))

#Andando a vedere se ci sta qualche valore anomalo sembra esserci un 28

summary(lm(JulyTF ~ Long + JanTF + Rain + RelHum + State, subset = (1:length(JulyTF) !=  28)))

#Provando a fare la regressione togliendo l'osservazione 28 effettivamente il modello sembra migliorare

summary(step(lm(JulyTF ~ Long + JanTF + Rain + RelHum + State, subset = (1:length(JulyTF) !=  28))))

#Anche l'AIC migliora togliendo l'osservazione 28

