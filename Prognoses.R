###
rm(list = ls())

library(data.table)
library(rms)

d <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv")

setwd("H:/CORONA")

col1 <- c("black", "white")
palette(col1)

lbls <- c("13\nmrt", "27\nmrt", "10\napr", "24\napr", "8\nmei", "22\nmei",
          "5\njun", "19\njun", "3\njul", "17\njul", "31\njul", "14\naug",
          "28\naug", "11\nsep", "25\nsep", "9\nokt")

###

IC   <- c(78, 96, 120, 150,
          178, 228, 288, 366, 430, 480, 575, 657, 755, 838, 927, 995, 1044,
          1135, 1192, 1211, 1256, 1283, 1287, 1310, 1320, 1329, 1299, 1297,
          1310, 1271, 1252, 1237, 1215, 1190, 1155, 1127, 1082, 1058, 1044,
          1008, 958, 917, 884, 851, 836, 804, 781, 750, 724, 691, 655, 648, 636,
          600, 576, 524, 494, 470, 454, 447, 407, 388, 364, 350, 336, 335, 330,
          298, 288, 271, 255, 243, 231, 215, 192, 177, 170, 161, 152, 153, 151,
          150, 136, 131, 128, 128, 128, 127, 117, 115, 111, 107, 106, 107, 109,
          97, 91, 81, 78, 71, 67, 68, 62, 60, 55, 51, 46, 42, 42, 41, 37, 38,
          35, 37, 35, 34, 33, 32, 33, 31, 29, 30, 29, 29, 28, 27, 29, 29, 31,
          30, 29, 30, 28, 27, 25, 26, 24, 23, 22, 25, 27, 27, 29, 33,35,34,36, 
          36,37,41,39,42,44,44,50,48,50,58,54,54, 47, 49, 53, 48, 53, 48, 46,
          45, 42, 43, 48, 48, 44, 42, 43, 48, 44, 46, 45, 47, 46, 49, 54, 59,
          65, 66, 65, 61, 61, 65, 76, 89, 98, 106, 114, 121, 136, 140, 152, 162,
          170, 72, 178, 177, 185, 198, 215, 239, 239)
IC_I <- IC - shift(IC, n=1, fill=0, type="lag")
dag  <- 1:length(IC)

png("ICopnames.png", width = 900, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(IC_I ~ dag, ylim = c(-50, 100), xlim = c(0, length(dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 - verschil dag ervoor")
axis(side = 1, at = seq(1, length(dago)+2, 14), labels = lbls, tick = FALSE)
tick_I <- c(-50, -25, 0, 25, 50, 75, 100, 125, 150)
axis(side = 2, at = tick_I)
abline(h = tick_I, v = seq(1, length(dago)+2, 7), lty = 3)

par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()


### COVID-19 prognosis RIVM data

dday <- aggregate(d$Total_reported, by = list(d$Date_of_report), FUN = sum)
dday
dday$inc <- dday$x - shift(dday$x, n=1, fill=0, type="lag")
dday$inc

CoV  <- dday$inc
dag  <- seq(1, length(CoV))

png("incidentie_RIVM.png", width = 900, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(CoV ~ dag, ylim = c(0, 6000), xlim = c(1, length(dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 - incidentie")
axis(side = 1, at = seq(1, length(dag)+2, 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 1000, 2000, 3000, 4000, 5000, 6000)
axis(side = 2, at = tick_o)
abline(h = seq(0, 6000, 500), v = seq(1, length(dag)+2, 7), lty = 3)
dev.off()

#Limburg
dday <- aggregate(formula= Total_reported~ Date_of_report,
                  FUN = sum, 
                  data=subset(d, Province=="Limburg"))
dday
dday$x <- dday$Total_reported
dday$inc <- dday$x - shift(dday$x, n=1, fill=0, type="lag")
dday$inc

CoV  <- dday$inc
dag  <- seq(1, length(CoV))

png("incidentie_RIVM_limb.png", width = 800, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(CoV ~ dag, ylim = c(0, 200), xlim = c(1, length(dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 in Limburg - incidentie")
axis(side = 1, at = seq(1, length(dag), 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 25, 50, 75, 100, 125, 150, 175, 200)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(dag), 7), lty = 3)

dev.off()


#Ziekenhuisopnames 

dday <- aggregate(d$Hospital_admission, by = list(d$Date_of_report), FUN = sum)
dday

dday$inc <- dday$x - shift(dday$x, n=1, fill=0, type="lag")
dday$inc

opge  <- dday$inc
dago <- seq(1, length(opge))

png("opnames_NL.png", width = 900, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(opge ~ dago, ylim = c(0, 140), xlim = c(1, length(dago)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 ziekenhuisopname - incidentie")
axis(side = 1, at = seq(1, length(dago)+2, 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 20, 40, 60, 80, 100, 120, 140)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(dago)+2, 7), lty = 3)

par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

#Ziekenhuisopnames Limburg
dday <- aggregate(formula= Hospital_admission~ Date_of_report,
                  FUN = sum, 
                  data=subset(d, Province=="Limburg"))
dday
dday$x <- dday$Hospital_admission
dday$inc <- dday$x - shift(dday$x, n=1, fill=0, type="lag")
dday$inc

opge  <- dday$inc
dago <- seq(1, length(opge))

########## Figuur ##########
png("opnames_limb.png", width = 800, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(opge ~ dago, ylim = c(0, 100), xlim = c(1, length(dago)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 ziekenhuisopnames Limburg - incidentie")
axis(side = 1, at = seq(1, length(dago), 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 25, 50, 70, 100, 125, 150, 175, 200)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(dago), 7), lty = 3)

par(mar=c(5.1, 4.1, 4.1, 2.1))
dev.off()

#Schatting overlijdens
dday <- aggregate(d$Deceased, by = list(d$Date_of_report), FUN = sum)
dday

dday <- aggregate(formula= Deceased~ Date_of_report,
                  FUN = sum, 
                  data=subset(d, Province=="Limburg"))
dday


