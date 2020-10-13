# code formatted using formatR::tidy_source(width.cutoff = 80)
Sys.setlocale("LC_TIME", "Dutch") #set to Dutch locale (to get Dutch month names) for this session
rm(list = ls())
library(data.table)
library(rms)
library(forecast)

palette(c("black", "white"))
lbls <- format(seq(as.Date("2020-3-13"), Sys.Date() + 2, by = "2 week"), "%e\n%b")

###### DATA ######
dat_NICE <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-ic/data-nice/NICE_IC_wide_latest.csv?raw=true") # https://stichting-nice.nl/ & https://github.com/J535D165/CoronaWatchNL
dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 

#data manipulation
dat_NICE <- subset(dat_NICE, as.Date(dat_NICE$Datum) >= as.Date("2020-3-13")) # select same start date as RIVM data
IC <- dat_NICE$TotaalOpnamen #Total number of IC intakes
IC_COV_new <- dat_NICE$ToenameOpnamen #Number of newly confirmed or suspected COVID-19 IC intakes 
COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat_RIVM)
COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                      data = subset(dat_RIVM, Province == "Limburg"))
Hosp <- aggregate(formula = Hospital_admission ~ Date_of_report, FUN = sum, data = dat_RIVM)
Hosp_limb <- aggregate(formula = Hospital_admission ~ Date_of_report, FUN = sum, 
                       data = subset(dat_RIVM, Province == "Limburg"))
Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat_RIVM)
Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                        data = subset(dat_RIVM, Province == "Limburg"))

###### DATAFRAMES ######
# A = huidig aantal, I = incidentie, C = cumulatieve incidentie
IC <- data.frame(A = IC,
                 I = IC - shift(IC, n=1, fill=0, type="lag"),
                 dag = 1:length(IC)
)

IC_COV_new <- data.frame(I = IC_COV_new,
                         dag = 1:length(IC_COV_new)
)

COV <- data.frame(C = COV$Total_reported,
                  I = COV$Total_reported - shift(COV$Total_reported, n=1, fill=0, type="lag"),
                  dag = 1:length(COV$Total_reported),
                  C_limb = COV_limb$Total_reported,
                  I_limb = COV_limb$Total_reported - shift(COV_limb$Total_reported, n=1, fill=0, type="lag")
)

Hosp <- data.frame(C = Hosp$Hospital_admission,
                   I = Hosp$Hospital_admission - shift(Hosp$Hospital_admission, n=1, fill=0, type="lag"),
                   dag = 1:length(Hosp$Hospital_admission),
                   C_limb = Hosp_limb$Hospital_admission,
                   I_limb = Hosp_limb$Hospital_admission - shift(Hosp_limb$Hospital_admission, n=1, fill=0, type="lag")
)

Death <- data.frame(C = Death$Deceased,
                    I = Death$Deceased - shift(Death$Deceased, n=1, fill=0, type="lag"),
                    dag = 1:length(Death$Deceased),
                    C_limb = Death_limb$Deceased,
                    I_limb = Death_limb$Deceased - shift(Death_limb$Deceased, n=1, fill=0, type="lag")
)
rm(COV_limb, Hosp_limb, Death_limb) #clean workspace

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

### IC COVID-19 NEW ###
pred <- data.frame(time = seq(length(IC_COV_new$dag) + 1, length.out = 7))

# loess
loess <- loess(I ~ dag, IC_COV_new, control = loess.control(surface = "direct"), span = 0.25)
pred$IC_COV_new_I_loess <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[1]]
pred$IC_COV_new_I_loess_se <- predict(loess, data.frame(dag = pred$time), se = TRUE)[[2]]

# arima
arima <- auto.arima(IC_COV_new$I)
# autoplot(forecast(arima)) 
# checkresiduals(arima)
pred$IC_COV_new_I_arima <- summary(forecast(arima, h = length(pred$time)))[[1]]
pred$IC_COV_new_I_arima_lo <- summary(forecast(arima, h = length(pred$time)))[[4]]
pred$IC_COV_new_I_arima_up <- summary(forecast(arima, h = length(pred$time)))[[5]]

# fit
plot(I ~ dag, xlim = c(0, length(dag) + 7), data = IC_COV_new)
lines(loess$x, loess$fitted, col = "red")
lines(pred$time, pred$IC_COV_new_I_loess, col = "red", lty= 3)
lines(loess$x, arima$fitted, col = "blue")
lines(pred$time, pred$IC_COV_new_I_arima, col = "blue", lty= 3)

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
plot(I ~ dag, xlim = c(0, length(dag) + 7), ylim = c(0, 8000), data = COV)
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
# Figuur - NL
png("Figures/ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I ~ IC$dag, ylim = c(floor(min(IC$I)/10) * 10, ceiling(max(IC$I)/10) * 10), 
     xlim = c(0, length(IC$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", 
     pch = 16, cex = 0.6, main = "Totaal - IC opnames - verschil dag ervoor")
axis(side = 1, at = seq(1, length(IC$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(floor(min(IC$I)/10) * 10, ceiling(max(IC$I)/10) * 10, 25)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(IC$dag) + 9)/7), lty = 3)

dev.off()

### IC COVID-19 NEW ###
# Figuur - NL
png("Figures/ICopnames_COVnew_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC_COV_new$I ~ IC_COV_new$dag, ylim = c(0, ceiling(max(IC_COV_new$I)/10) * 10), 
     xlim = c(0, length(IC_COV_new$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", 
     pch = 16, cex = 0.6, main = "COVID-19 - nieuwe IC opnames")
axis(side = 1, at = seq(1, length(IC_COV_new$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(IC_COV_new$I)/10) * 10, 25)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(IC_COV_new$dag) + 9)/7), lty = 3)

dev.off()

###### Positief COVID-19 #####
# Figuur - NL
png("Figures/Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I ~ COV$dag, ylim = c(0, ceiling(max(COV$I)/1000) * 1000), xlim = c(1, length(COV$dag)), 
     ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 - incidentie")
axis(side = 1, at = seq(1, length(COV$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(COV$I)/1000) * 1000, 1000)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(COV$dag) + 9)/7), lty = 3)

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

plot(Hosp$I ~ Hosp$dag, ylim = c(floor(min(Hosp$I)/50) * 50, ceiling(max(Hosp$I)/50) * 50), xlim = c(1, length(Hosp$dag)), ylab = "", 
     xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 ziekenhuisopnames - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(floor(min(Hosp$I)/50) * 50, ceiling(max(Hosp$I)/50) * 50, 50)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Hosp$dag) + 9)/7), lty = 3)

dev.off()

# Figuur - Limburg
png("Figures/Opnames_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I_limb ~ Hosp$dag, ylim = c(floor(min(Hosp$I_limb)/25) * 25, ceiling(max(Hosp$I_limb)/25) * 25), xlim = c(1, length(Hosp$dag)), ylab = "", 
     xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 ziekenhuisopnames Limburg - incidentie")
axis(side = 1, at = seq(1, length(Hosp$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(floor(min(Hosp$I_limb)/25) * 25, ceiling(max(Hosp$I_limb)/25) * 25, 25)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Hosp$dag) + 9)/7), lty = 3)

dev.off()

###### Overlijdens ######
# Figuur - NL
png("Figures/Overlijdens_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I ~ Death$dag, ylim = c(0, ceiling(max(Death$I)/50) * 50), xlim = c(1, length(Death$dag)), 
     ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, cex = 0.6, main = "COVID-19 - incidentie")
axis(side = 1, at = seq(1, length(Death$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Death$I)/50) * 50, 50)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Death$dag) + 9)/7), lty = 3)

dev.off()

# Figuur - Limburg
png("Figures/Overlijdens_limb.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I_limb ~ Death$dag, ylim = c(0, ceiling(max(Death$I_limb)/10) * 10), 
     xlim = c(1, length(Death$dag)), ylab = "", xlab = "Datum", xaxt = "n", yaxt = "n", pch = 16, 
     cex = 0.6, main = "COVID-19 in Limburg - incidentie")
axis(side = 1, at = seq(1, length(Death$dag) + 2, 14), labels = lbls, tick = FALSE)
tick_o <- seq(0, ceiling(max(Death$I_limb)/10) * 10, 10)
axis(side = 2, at = tick_o)
abline(h = tick_o, v = seq(1, by = 7, length.out = ceiling(length(Death$dag) + 9)/7), lty = 3)

dev.off()

###### Save R session ######
save.image(file="COVID19.RData") 
