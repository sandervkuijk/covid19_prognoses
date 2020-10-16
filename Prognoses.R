# code formatted using formatR::tidy_source(width.cutoff = 80)
Sys.setlocale("LC_TIME", "Dutch") #set to Dutch locale (to get Dutch month names) for this session
rm(list = ls())
library(data.table)
library(rms)
library(forecast)
library(zoo)

palette(c("black", "white"))
date_start <- as.Date("2020-3-14") #selected 1 day after RIVM data starts 
lbls <- format(seq(date_start, Sys.Date() + 2, by = "2 week"), "%e\n%b")

###### RETRIEVE AND MANIPULATE DATA ######
dat_NICE <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-ic/data-nice/NICE_IC_wide_latest.csv?raw=true") 
dat_CBS <- fread("https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE")
dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 
dat_RIVM_test <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-misc/data-test/RIVM_NL_test_latest.csv?raw=true")

# Data partly from:
# De Bruin, J. (2020). Novel Coronavirus (COVID-19) Cases in The Netherlands
# [Data set]. Zenodo. http://doi.org/10.5281/zenodo.4068121

# Data manipulation
IC <- dat_NICE$CumulatiefOpnamen # Total number of IC intakes since the start of the outbreak
IC_COV <- dat_NICE$ToenameOpnamen # Number of newly confirmed or suspected COVID-19 IC intakes 
COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat_RIVM)
COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                      data = subset(dat_RIVM, Province == "Limburg"))
Hosp <- aggregate(formula = Hospital_admission ~ Date_of_report, FUN = sum, data = dat_RIVM)
Hosp_limb <- aggregate(formula = Hospital_admission ~ Date_of_report, FUN = sum, 
                       data = subset(dat_RIVM, Province == "Limburg"))
Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat_RIVM)
Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                        data = subset(dat_RIVM, Province == "Limburg"))
dat_RIVM_test$Type <- as.factor(dat_RIVM_test$Type)

# Create dataframes 
# I = incidentie, C = cumulatieve incidentie
IC <- data.frame(C = IC,
                 I = IC - shift(IC, n=1, fill=0, type="lag"),
                 I_COV = IC_COV,
                 date = as.Date(dat_NICE$Datum)
)
IC <- subset(IC, IC$date >= date_start) # Select data from start date 
IC <- IC[1:(dim(IC)[1] - 1),] # Remove last row (as these are still being updated)
IC$dag <- 1:dim(IC)[1]

COV <- data.frame(C = COV$Total_reported,
                  I = pmax(COV$Total_reported - shift(COV$Total_reported, n=1, fill=0, type="lag"), 0),
                  C_limb = COV_limb$Total_reported,
                  I_limb = pmax(COV_limb$Total_reported - shift(COV_limb$Total_reported, n=1, fill=0, type="lag"), 0),
                  date = as.Date(COV$Date_of_report)
)
COV$Iweek <- rollsumr(COV$I, k = 7, fill = NA)
COV$I_rel <- COV$Iweek/tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1)*100000
COV <- subset(COV, COV$date >= date_start)
COV <- COV[1:(dim(COV)[1] - 1),] # Remove last row (as these are still being updated)
COV$dag <- 1:dim(COV)[1]

COV_test <- data.frame(I_pos = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal,
                       I_total = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       prop_pos = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal / dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       date = as.Date(dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$EindDatum) - 4, #minus 4 so it is mid week
                       week = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Week
) # Separate dataframe as unit is week (not day as in COV)
COV_test$I_pos_rel <- COV_test$I_pos/tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1)*100000
COV_test <- subset(COV_test, COV_test$date >= date_start)

Hosp <- data.frame(C = Hosp$Hospital_admission,
                   I = pmax(Hosp$Hospital_admission - shift(Hosp$Hospital_admission, n=1, fill=0, type="lag"), 0),
                   C_limb = Hosp_limb$Hospital_admission,
                   I_limb = pmax(Hosp_limb$Hospital_admission - shift(Hosp_limb$Hospital_admission, n=1, fill=0, type="lag"), 0),
                   date = as.Date(Hosp$Date_of_report)
)
Hosp <- subset(Hosp, Hosp$date >= date_start)
Hosp <- Hosp[1:(dim(Hosp)[1] - 1),] # Remove last row (as these are still being updated)
Hosp$dag <- 1:dim(Hosp)[1]

Death <- data.frame(C = Death$Deceased,
                    I = Death$Deceased - shift(Death$Deceased, n=1, fill=0, type="lag"),
                    C_limb = Death_limb$Deceased,
                    I_limb = Death_limb$Deceased - shift(Death_limb$Deceased, n=1, fill=0, type="lag"),
                    date = as.Date(Death$Date_of_report)
)
Death <- subset(Death, Death$date >= date_start)
Death <- Death[1:(dim(Death)[1] - 1),] # Remove last row (as these are still being updated)
Death$dag <- 1:dim(Death)[1]

rm(IC_COV, COV_limb, Hosp_limb, Death_limb) #clean workspace

###### TRENDLIJN ######
### IC ###
pred <- data.frame(time = seq(length(IC$dag) + 1, length.out = 7))

# loess
loess <- loess(I ~ dag, IC, control = loess.control(surface = "direct"), span = 0.25)
pred$IC_I_loess <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[1]]
pred$IC_I_loess_se <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[2]]

# arima
arima <- auto.arima(IC$I)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
pred$IC_I_arima <- summary(forecast(arima, h = length(pred$time)))[[1]]
pred$IC_I_arima_lo <- summary(forecast(arima, h = length(pred$time)))[[4]]
pred$IC_I_arima_up <- summary(forecast(arima, h = length(pred$time)))[[5]]

# fit
plot(I ~ dag, xlim = c(0, length(dag) + 7), data = IC)
lines(loess$x, loess$fitted, col = "red")
lines(pred$time, pred$IC_I_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(pred$time, pred$IC_I_arima, col = "blue", lty= 3)

### IC COVID-19 ###
pred$time = seq(length(IC$dag) + 1, length.out = 7)

# loess
loess <- loess(I_COV ~ dag, IC, control = loess.control(surface = "direct"), span = 0.25)
pred$IC_COV_I_loess <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[1]]
pred$IC_COV_I_loess_se <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[2]]

# arima
arima <- auto.arima(IC$I_COV)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
pred$IC_COV_I_arima <- summary(forecast(arima, h = length(pred$time)))[[1]]
pred$IC_COV_I_arima_lo <- summary(forecast(arima, h = length(pred$time)))[[4]]
pred$IC_COV_I_arima_up <- summary(forecast(arima, h = length(pred$time)))[[5]]

# fit
plot(I_COV ~ dag, xlim = c(0, length(dag) + 7), data = IC)
lines(loess$x, loess$fitted, col = "red")
lines(pred$time, pred$IC_COV_I_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(pred$time, pred$IC_COV_I_arima, col = "blue", lty= 3)

###  Positief ### 
pred$time = seq(length(COV$dag) + 1, length.out = 7)

# loess
loess <- loess(I ~ dag, COV, control = loess.control(surface = "direct"), span = 0.25)
pred$COV_I_loess <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[1]]
pred$COV_I_loess_se <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[2]]

# arima
arima <- auto.arima(COV$I)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
pred$COV_I_arima <- summary(forecast(arima, h = length(pred$time)))[[1]]
pred$COV_I_arima_lo <- summary(forecast(arima, h = length(pred$time)))[[4]]
pred$COV_I_arima_up <- summary(forecast(arima, h = length(pred$time)))[[5]]

# fit
plot(I ~ dag, xlim = c(0, length(dag) + 7), ylim = c(0, 9000), data = COV)
lines(loess$x, loess$fitted, col = "red")
lines(pred$time, pred$COV_I_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(pred$time, pred$COV_I_arima, col = "blue", lty= 3)

### Ziekenhuisopnames ### 
pred$time = seq(length(Hosp$dag) + 1, length.out = 7)

# loess
loess <- loess(I ~ dag, Hosp, control = loess.control(surface = "direct"), span = 0.25)
pred$Hosp_I_loess <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[1]]
pred$Hosp_I_loess_se <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[2]]

# arima
arima <- auto.arima(Hosp$I)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
pred$Hosp_I_arima <- summary(forecast(arima, h = length(pred$time)))[[1]]
pred$Hosp_I_arima_lo <- summary(forecast(arima, h = length(pred$time)))[[4]]
pred$Hosp_I_arima_up <- summary(forecast(arima, h = length(pred$time)))[[5]]

# fit
plot(I ~ dag, xlim = c(0, length(dag) + 7), data = Hosp)
lines(loess$x, loess$fitted, col = "red")
lines(pred$time, pred$Hosp_I_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(pred$time, pred$Hosp_I_arima, col = "blue", lty= 3)

rm(arima, loess) #clean workspace
pred <- pred[ , -1] #clean pred dataframe (time differs per outcome)

###### IC COVID-19 #####
# IC TOTAL
# Figuur - NL
png("Figures/ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I ~ IC$dag, ylim = c(0, ceiling(max(IC$I)/10) * 10), 
     xlim = c(0, length(IC$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", 
     pch = 16, cex = 0.6, main = "Totaal IC opnames - incidentie")
axis(side = 1, at = seq(1, length(IC$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(IC$I)/10) * 10, 25)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(IC$dag) + 9)/7), lty = 3)

dev.off()

# IC COVID-19
# Figuur - NL
png("Figures/ICopnames_COV_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I_COV ~ IC$dag, ylim = c(0, ceiling(max(IC$I_COV)/10) * 10), 
     xlim = c(0, length(IC$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", 
     pch = 16, cex = 0.6, main = "COVID-19 IC opnames - incidentie")
axis(side = 1, at = seq(1, length(IC$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(IC$I_COV)/10) * 10, 25)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(IC$dag) + 9)/7), lty = 3)

dev.off()

###### Positief COVID-19 #####
# Figuur - NL
png("Figures/Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I ~ COV$date, ylab = "", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(min(COV$date), max(COV$date)),
     main = "COVID-19 - incidentie")
factor <- 1 / 7 / (100000 / tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1)) # NEEDS TO BE CHECKED
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(50 * factor, 50 * factor, 150 * factor, 150 * factor), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(150 * factor, 150 * factor, 250 * factor, 250 * factor), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(250 * factor, 250 * factor, 100000 * factor, 100000 * factor), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
tick_o <- seq(0, ceiling(max(COV$I)/2000) * 2000, 2000)
abline(h = tick_o, lty = 3)
abline(v = as.Date(seq(as.Date("2020-1-1"), Sys.Date() + 7, by = "1 month")), lty = 3)
abline(v = as.Date(seq(as.Date("2020-1-15"), Sys.Date() + 7, by = "1 month")), lty = 3)

dev.off()

# Figuur - Incidentie NL per 100.000 per week
png("Figures/Incidentie_NL_relative.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_rel ~ COV$date, ylab = "", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(min(COV$date), max(COV$date)),
     main = "COVID-19 - incidentie per 100.000", type = "l")
lines(COV_test$I_pos_rel ~ COV_test$date, type = "l", lty = 2)
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(50, 50, 150, 150), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(150, 150, 250, 250), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(min(COV$date) - 7, max(COV$date) + 7, max(COV$date) + 7, 
          min(COV$date) - 7), c(250, 250, 100000, 100000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
abline(v = as.Date(seq(as.Date("2020-1-1"), Sys.Date() + 7, by = "1 month")), lty = 3)
abline(v = as.Date(seq(as.Date("2020-1-15"), Sys.Date() + 7, by = "1 month")), lty = 3)
legend("topleft", inset = 0.05, col=c(1, 1), lty=1:2, cex=0.6, box.lty=0, 
       legend=c("COVID-19 aantallen\n(per week per 100.000)\n ", "Aantal COVID-19 positieve testen\n(per week per 100.000)\n "))
  
dev.off()

# Figuur - Incidentie positieve testen NL per 100.000 per week
png("Figures/Incidentie_test_NL_relative.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos ~ COV_test$date, ylab = "", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(min(COV_test$date), max(COV_test$date)),
     main = "COVID-19 - proportie positieve testen")
polygon(c(min(COV_test$date) - 7, max(COV_test$date) + 7, max(COV_test$date) + 7, 
          min(COV_test$date) - 7), c(0.05, 0.05, 0.1, 0.1), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(min(COV_test$date) - 7, max(COV_test$date) + 7, max(COV_test$date) + 7, 
          min(COV_test$date) - 7), c(0.1, 0.1, 1, 1), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
abline(v = as.Date(seq(as.Date("2020-1-1"), Sys.Date() + 7, by = "1 month")), lty = 3)
abline(v = as.Date(seq(as.Date("2020-1-15"), Sys.Date() + 7, by = "1 month")), lty = 3)

dev.off()

# Figuur - Limburg
png("Figures/Incidentie_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_limb ~ COV$dag, ylim = c(0, ceiling(max(COV$I_limb)/100) * 100), 
     xlim = c(1, length(COV$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, 
     cex = 0.6, main = "COVID-19 in Limburg - incidentie")
axis(side = 1, at = seq(1, length(COV$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(COV$I_limb)/100) * 100, 50)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(COV$dag) + 9)/7), lty = 3)

dev.off()

###### Ziekenhuisopnames COVID-19 #####
# Figuur - NL
png("Figures/Opnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I ~ Hosp$dag, ylim = c(0, ceiling(max(Hosp$I)/100) * 100), xlim = c(1, length(Hosp$dag)), ylab = "", 
     xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 ziekenhuisopnames - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Hosp$I)/100) * 100, 100)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Hosp$dag) + 9)/7), lty = 3)

dev.off()

# Figuur - Limburg
png("Figures/Opnames_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I_limb ~ Hosp$dag, ylim = c(0, ceiling(max(Hosp$I_limb)/50) * 50), xlim = c(1, length(Hosp$dag)), ylab = "", 
     xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 ziekenhuisopnames Limburg - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Hosp$I_limb)/50) * 50, 50)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Hosp$dag) + 9)/7), lty = 3)

dev.off()

###### Sterfte ######
# Figuur - NL
png("Figures/Sterfte_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I ~ Death$dag, ylim = c(0, ceiling(max(Death$I)/50) * 50), xlim = c(1, length(Death$dag)), 
     ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 sterfte - incidentie")
axis(side = 1, at = seq(1, length(Death$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Death$I)/50) * 50, 50)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Death$dag) + 9)/7), lty = 3)

dev.off()

# Figuur - Limburg
png("Figures/Sterfte_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I_limb ~ Death$dag, ylim = c(0, ceiling(max(Death$I_limb)/10) * 10), 
     xlim = c(1, length(Death$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, 
     cex = 0.6, main = "COVID-19 sterfte Limburg - incidentie")
axis(side = 1, at = seq(1, length(Death$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Death$I_limb)/10) * 10, 10)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Death$dag) + 9)/7), lty = 3)

dev.off()

###### Save R session ######
save.image(file="COVID19.RData") 
