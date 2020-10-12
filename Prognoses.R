###
rm(list = ls())

library(data.table)
library(rms)

#setwd("H:/CORONA") #Dit kan denk ik weg nu het op GitHub staat?

col1 <- c("black", "white")
palette(col1)
lbls <- c("13\nmrt", "27\nmrt", "10\napr", "24\napr", "8\nmei", "22\nmei",
          "5\njun", "19\njun", "3\njul", "17\njul", "31\njul", "14\naug",
          "28\naug", "11\nsep", "25\nsep", "9\nokt")

###### DATA ######
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

dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv")
COV <- aggregate(formula= Total_reported~ Date_of_report, FUN = sum, 
                 data=dat_RIVM)
COV_limb <- aggregate(formula= Total_reported ~ Date_of_report, FUN = sum, 
                      data=subset(dat_RIVM, Province=="Limburg"))
Hosp <- aggregate(formula= Hospital_admission ~ Date_of_report, FUN = sum, 
                          data=dat_RIVM)
Hosp_limb <- aggregate(formula= Hospital_admission ~ Date_of_report, FUN = sum, 
                    data=subset(dat_RIVM, Province=="Limburg"))
Death <- aggregate(formula= Deceased ~ Date_of_report, FUN = sum, 
                        data=dat_RIVM)
Death_limb <- aggregate(formula= Deceased ~ Date_of_report, FUN = sum, 
                       data=subset(dat_RIVM, Province=="Limburg"))

###### DATAFRAMES ######
IC <- data.frame(A = IC,
                 I = IC - shift(IC, n=1, fill=0, type="lag"),
                 dag = 1:length(IC)
)

COV <- data.frame(P = COV$Total_reported,
                  I = COV$Total_reported - shift(COV$Total_reported, n=1, fill=0, type="lag"),
                  dag = 1:length(COV$Total_reported),
                  P_limb = COV_limb$Total_reported,
                  I_limb = COV_limb$Total_reported - shift(COV_limb$Total_reported, n=1, fill=0, type="lag")
)
rm(COV_limb)

Hosp <- data.frame(A = Hosp$Hospital_admission,
                   I = Hosp$Hospital_admission - shift(Hosp$Hospital_admission, n=1, fill=0, type="lag"),
                   dag = 1:length(Hosp$Hospital_admission),
                   A_limb = Hosp_limb$Hospital_admission,
                   I_limb = Hosp_limb$Hospital_admission - shift(Hosp_limb$Hospital_admission, n=1, fill=0, type="lag")
)
rm(Hosp_limb)

Death <- data.frame(P = Death$Deceased,
                  I = Death$Deceased - shift(Death$Deceased, n=1, fill=0, type="lag"),
                  dag = 1:length(Death$Deceased),
                  P_limb = Death_limb$Deceased,
                  I_limb = Death_limb$Deceased - shift(Death_limb$Deceased, n=1, fill=0, type="lag")
)
rm(Death_limb)

###### IC COVID-19 ######
# Figuur
png("ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(IC$I ~ IC$dag, ylim = c(floor(min(IC$I)/10)*10, ceiling(max(IC$I)/10)*10), xlim = c(0, length(IC$dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 - IC opnames - verschil dag ervoor")
axis(side = 1, at = seq(1, length(IC$dag)+2, 14), labels = lbls, tick = FALSE)
tick_o <- c(-100, -75, -50, -25, 0, 25, 50, 75, 100, 125, 150)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(IC$dag)+2, 7), lty = 3)

dev.off()

###### Positief COVID-19 ######
# Figuur
png("incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(COV$I ~ COV$dag, ylim = c(0, ceiling(max(COV$I)/1000)*1000), xlim = c(1, length(COV$dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 - incidentie")
axis(side = 1, at = seq(1, length(COV$dag)+2, 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 1000, 2000, 3000, 4000, 5000, 6000, 7000, 8000)
axis(side = 2, at = tick_o)
abline(h = seq(0, 8000, 500), v = seq(1, length(COV$dag)+2, 7), lty = 3)

dev.off()

# Figuur - Limburg
png("incidentie_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_limb ~ COV$dag, ylim = c(0, ceiling(max(COV$I_limb)/100)*100), xlim = c(1, length(COV$dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 in Limburg - incidentie")
axis(side = 1, at = seq(1, length(COV$dag), 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 25, 50, 75, 100, 125, 150, 175, 200, 225, 250, 275, 300)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(COV$dag), 7), lty = 3)

dev.off()

###### Ziekenhuisopnames COVID-19 ###### 
# Figuur
png("opnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I ~ Hosp$dag, ylim = c(0, 140), xlim = c(1, length(Hosp$dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 ziekenhuisopnames - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag)+2, 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 20, 40, 60, 80, 100, 120, 140)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(Hosp$dag)+2, 7), lty = 3)

dev.off()

# Figuur - Limburg
png("opnames_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar=c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I_limb ~ Hosp$dag, ylim = c(0, 100), xlim = c(1, length(Hosp$dag)),
     ylab = "", xlab = "Datum",
     xaxt = "n", yaxt = "n", pch = 16, cex = 0.6,
     main = "COVID-19 ziekenhuisopnames Limburg - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag), 14), labels = lbls, tick = FALSE)
tick_o <- c(0, 25, 50, 70, 100, 125, 150, 175, 200)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, length(Hosp$dag), 7), lty = 3)

dev.off()

###### Overlijdens ###### 


