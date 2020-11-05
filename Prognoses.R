# code formatted using formatR::tidy_source(width.cutoff = 80)
Sys.setlocale("LC_TIME", "Dutch") #set to Dutch locale (to get Dutch month names) for this session
options(scipen = 999)
rm(list = ls())

library(data.table)
library(rms)
library(forecast)
library(zoo)
library(tidyverse)
library(rjson)
library(pdftools)
source("f_trend.R")
source("f_pdf_rivm_test.R")

palette(c("black", "white"))
date_start <- as.Date("2020-6-1") #as.Date("2020-3-14") #minimal 1 day after RIVM data starts 
lbls <- format(seq(date_start, Sys.Date() + 30, by = "2 week"), "%e %b")

###### RETRIEVE AND MANIPULATE DATA ######
dat_NICE_IC_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE)
dat_NICE_IC_C <- dat_NICE_IC_C %>% map(as.data.table) %>% rbindlist(fill = TRUE)
dat_NICE_IC_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE)
dat_NICE_IC_I <- dat_NICE_IC_I %>% map(as.data.table) %>% rbindlist(fill=TRUE)
dat_NICE_IC_I <- as.data.frame(t(dat_NICE_IC_I))[, -3]
dat_NICE_IC_B <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/",simplify = TRUE)
dat_NICE_IC_B <- dat_NICE_IC_B %>% map(as.data.table) %>% rbindlist(fill = TRUE)
dat_NICE_Hosp_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE)
dat_NICE_Hosp_C <- dat_NICE_Hosp_C %>% map(as.data.table) %>% rbindlist(fill = TRUE)
dat_NICE_Hosp_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify = TRUE)
dat_NICE_Hosp_I <- dat_NICE_Hosp_I %>% map(as.data.table) %>% rbindlist(fill = TRUE)
dat_NICE_Hosp_I <- as.data.frame(t(dat_NICE_Hosp_I))[, -3]
dat_NICE_Hosp_B <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/",simplify = TRUE)
dat_NICE_Hosp_B <- dat_NICE_Hosp_B %>% map(as.data.table) %>% rbindlist(fill = TRUE)

dat_LCPS <- data.frame(fread("https://lcps.nu/wp-content/uploads/covid-19.csv"))
# Uitleg LCPS data: https://lcps.nu/datafeed/

dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 
# Wellicht onderstaande R data gebruiken direct van RIVM (ipv obv github)
# dat_RIVM_R <- fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json",simplify = TRUE)
# dat_RIVM_R <- dat_RIVM_R %>% map(as.data.table) %>% rbindlist(fill = TRUE)
dat_RIVM_test <- f_pdf_rivm_test("https://www.rivm.nl/sites/default/files/2020-11/COVID-19_WebSite_rapport_wekelijks_20201103_1216.pdf")
# https://www.rivm.nl/documenten/wekelijkse-update-epidemiologische-situatie-covid-19-in-nederland

dat_RIVM_test <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-misc/data-test/RIVM_NL_test_latest.csv?raw=true")
dat_RIVM_R <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-dashboard/data-reproduction/RIVM_NL_reproduction_index.csv?raw=true")
dat_RIVM_nursery <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-dashboard/data-nursery/data-nursery_homes/RIVM_NL_nursery_counts.csv?raw=true")

dat_OWiD <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true") #https://github.com/owid/covid-19-data/tree/master/public/data
# OWiD codebook: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv

dat_CBS <- fread("https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE")
dat_CBS_prov <- fread("https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC")

# Data partly from:
# De Bruin, J. (2020). Novel Coronavirus (COVID-19) Cases in The Netherlands
# [Data set]. Zenodo. http://doi.org/10.5281/zenodo.4068121
#
# AND
#
# Hasell, J., Mathieu, E., Beltekian, D. et al. A cross-country database of COVID-19 testing. 
# Sci Data 7, 345 (2020). https://doi.org/10.1038/s41597-020-00688-8

# Data manipulation
COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat_RIVM)
COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                      data = subset(dat_RIVM, Province == "Limburg"))
Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat_RIVM)
Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                        data = subset(dat_RIVM, Province == "Limburg"))

dat_LCPS$Datum <- as.Date(dat_LCPS$Datum, tryFormats = c("%d-%m-%Y"))
dat_LCPS <- dat_LCPS[order(dat_LCPS$Datum), ]

dat_RIVM_test$Type <- as.factor(dat_RIVM_test$Type)
dat_RIVM_R$Type <- as.factor(dat_RIVM_R$Type)

# Create dataframes 
# I = incidentie, C = cumulatieve incidentie, B = bezetting, A = huidig aantal, _rel = per 100,000

# Populatie
Population <- data.frame(NLD = tail(as.numeric(dat_CBS$`Bevolking aan het eind van de periode (aantal)`), n=1), 
                         Limb = tail(as.numeric(dat_CBS_prov[dat_CBS_prov$`Regio's` == "Limburg (PV)"]$'Bevolking aan het einde van de periode (aantal)'), n=1)
)
  
# Gemelde patienten
COV <- data.frame(C = COV$Total_reported,
                  I = pmax(COV$Total_reported - shift(COV$Total_reported, n=1, fill=0, type="lag"), 0),
                  C_limb = COV_limb$Total_reported,
                  I_limb = pmax(COV_limb$Total_reported - shift(COV_limb$Total_reported, n=1, fill=0, type="lag"), 0),
                  date = as.Date(COV$Date_of_report)
)
COV <- data.frame(COV,
                  I_rel = COV$I / Population$NLD * 100000,
                  I_rel_limb = COV$I_limb/Population$Limb * 100000,
                  I_3d = rollsumr(COV$I, k = 3, fill = NA),
                  I_3d_limb = rollsumr(COV$I_limb, k = 3, fill = NA),
                  I_3d_rel = rollsumr(COV$I, k = 3, fill = NA) / Population$NLD * 100000,
                  I_3d_rel_limb = rollsumr(COV$I_limb, k = 3, fill = NA) / Population$Limb * 100000,
                  I_7d = rollsumr(COV$I, k = 7, fill = NA),
                  I_7d_limb = rollsumr(COV$I_limb, k = 7, fill = NA),
                  I_7d_rel = rollsumr(COV$I, k = 7, fill = NA) / Population$NLD * 100000,
                  I_7d_rel_limb = rollsumr(COV$I_limb, k = 7, fill = NA) / Population$Limb * 100000
)
COV <- subset(COV, COV$date >= date_start) # Select data from start date 

COV_test <- data.frame(I_pos_7d = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal,
                       I_total_7d = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       prop_pos = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal / dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       date = as.Date(dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$EindDatum),
                       week = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Week,
                       I_pos_7d_rel = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal / Population$NLD * 100000
) 
COV_test <- subset(COV_test, COV_test$date >= date_start) # Select data from start date 

Rt <- data.frame(R = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[3], ]$Waarde,
                 Rmin = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[2], ]$Waarde,
                 Rmax = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[1], ]$Waarde,
                 date = as.Date(dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[3], ]$Datum)
) 
Rt <- subset(Rt, Rt$date >= date_start) # Select data from start date 

# Ziekenhuisopnames (excl IC)
# NICE
Hosp <- data.frame(C = unlist(dat_NICE_Hosp_C[, 2]),
                   I = unlist(dat_NICE_Hosp_I[, 2]) + unlist(dat_NICE_Hosp_I[, 3]), 
                   B = unlist(dat_NICE_Hosp_B[, 2]),
                   date = as.Date(unlist(dat_NICE_Hosp_I[, 1]))
)
Hosp <- data.frame(Hosp,
                   I_3d = rollsumr(Hosp$I, k = 3, fill = NA),
                   B_3d = rollsumr(Hosp$B, k = 3, fill = NA)
                   
)
Hosp <- subset(Hosp, Hosp$date >= date_start) # Select data from start date 
Hosp <- subset(Hosp, Hosp$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# LCPS (voor bezetting)
Hosp_LCPS <- data.frame(B = dat_LCPS$Kliniek_Bedden,
                        date = dat_LCPS$Datum,
                        B_3d = rollsumr(dat_LCPS$Kliniek_Bedden, k = 3, fill = NA)
)
Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date >= date_start) # Select data from start date 
Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# IC opnames 
# NICE
IC <- data.frame(C = unlist(dat_NICE_IC_C[, 2]),
                 I = unlist(dat_NICE_IC_I[, 2]) + unlist(dat_NICE_IC_I[, 3]), 
                 B = unlist(dat_NICE_IC_B[, 2]),
                 date = as.Date(unlist(dat_NICE_IC_I[, 1]))
)
IC <- data.frame(IC,
                 I_3d = rollsumr(IC$I, k = 3, fill = NA),
                 B_3d = rollsumr(IC$B, k = 3, fill = NA)
)
IC <- subset(IC, IC$date >= date_start) # Select data from start date 
IC <- subset(IC, IC$date <= (Sys.Date() - 1)) # Remove data that are still being updated

# LCPS (voor bezetting)
IC_LCPS <- data.frame(B = dat_LCPS$IC_Bedden_COVID,
                      B_non_covid = dat_LCPS$IC_Bedden_Non_COVID,
                      B_total = dat_LCPS$IC_Bedden_COVID + dat_LCPS$IC_Bedden_Non_COVID,
                      date = dat_LCPS$Datum,
                      B_3d = rollsumr(dat_LCPS$IC_Bedden_COVID, k = 3, fill = NA),
                      B_non_covid_3d = rollsumr(dat_LCPS$IC_Bedden_Non_COVID, k = 3, fill = NA),
                      B_total_3d = rollsumr(dat_LCPS$IC_Bedden_COVID + dat_LCPS$IC_Bedden_Non_COVID, k = 3, fill = NA)
)
IC_LCPS <- subset(IC_LCPS, IC_LCPS$date >= date_start) # Select data from start date 
IC_LCPS <- subset(IC_LCPS, IC_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# Verpleeghuislocaties
Nurs <- data.frame(A = dat_RIVM_nursery$Aantal,
                   I = dat_RIVM_nursery$NieuwAantal,
                   date = as.Date(dat_RIVM_nursery$Datum)
)
Nurs <- data.frame(Nurs, 
                   I_3d = rollsumr(Nurs$I, k = 3, fill = NA)
)
Nurs <- subset(Nurs, Nurs$date >= date_start) # Select data from start date 

# Sterfte
Death <- data.frame(C = Death$Deceased,
                    I = pmax(Death$Deceased - shift(Death$Deceased, n=1, fill=0, type="lag"), 0),
                    C_limb = Death_limb$Deceased,
                    I_limb = pmax(Death_limb$Deceased - shift(Death_limb$Deceased, n=1, fill=0, type="lag"), 0),
                    date = as.Date(Death$Date_of_report)
)
Death <- data.frame(Death, 
                    I_3d = rollsumr(Death$I, k = 3, fill = NA),
                    I_3d_limb = rollsumr(Death$I_limb, k = 3, fill = NA),
                    I_7d = rollsumr(Death$I, k = 7, fill = NA),
                    I_7d_limb = rollsumr(Death$I_limb, k = 7, fill = NA)
)
Death <- subset(Death, Death$date >= date_start) # Select data from start date 

# Internationaal
Int <- data.frame(continent = as.factor(dat_OWiD$continent),
                  iso = as.factor(dat_OWiD$iso_code),
                  country = as.factor(dat_OWiD$location),
                  population = dat_OWiD$population,
                  I_COV = pmax(dat_OWiD$population * dat_OWiD$new_cases_per_million/1000000, 0),
                  I_COV_smooth = pmax(dat_OWiD$population * dat_OWiD$new_cases_smoothed_per_million/1000000, 0),
                  I_COV_rel = pmax(dat_OWiD$new_cases_per_million/10, 0),
                  I_COV_rel_smooth = pmax(dat_OWiD$new_cases_smoothed_per_million/10, 0),
                  I_test_pos_rel = pmax(dat_OWiD$new_tests_per_thousand * dat_OWiD$positive_rate * 100, 0),
                  prop_test_pos = pmax(dat_OWiD$positive_rate, 0),
                  stringency_index = dat_OWiD$stringency_index,
                  GDP = dat_OWiD$gdp_per_capita,
                  LE = dat_OWiD$life_expectancy,
                  date = as.Date(dat_OWiD$date)
)
Int <- subset(Int, Int$continent == "Europe") # Select Europe
Int <- subset(Int, Int$LE >= 80) # Select countries with life expectancy above or equal to 80 
Int<- subset(Int, Int$population >= 1000000) # Select countries with population >= 1 mln 
Int <- droplevels(Int)
Int <- subset(Int, Int$date >= date_start - 14)
Int <- data.frame(Int,
                  I_3d = rollsumr(Int$I_COV, by = Int$country, k = 3, fill = NA),
                  I_3d_rel = rollsumr(Int$I_COV, by = Int$country, k = 3, fill = NA) / Int$population * 100000,
                  I_7d = rollsumr(Int$I_COV, by = Int$country, k = 7, fill = NA),
                  I_7d_rel = rollsumr(Int$I_COV, by = Int$country, k = 7, fill = NA) / Int$population * 100000
)
Int <- subset(Int, Int$date >= date_start) # Select data from start date 

row.names(Hosp) <- row.names(IC) <- row.names(dat_LCPS) <- NULL
rm(COV_limb, Death_limb) #clean workspace

###### TRENDLIJN ######
pred_COV <- f_trend(x = COV$I, time = 7, span = 0.25)$pred
pred_COV_rel <- pred_COV
pred_COV_rel[, -1] <- pred_COV_rel[, -1] / Population$NLD * 100000
pred_COV_limb <- f_trend(x = COV$I_limb, time = 7, span = 0.25)$pred
pred_COV_rel_limb <- pred_COV_limb
pred_COV_rel_limb[, -1] <- pred_COV_rel_limb[, -1] / Population$NLD * 100000
pred_Hosp <- f_trend(x = Hosp$I, time = 7, span = 0.25)$pred
pred_Hosp_B <- f_trend(x = Hosp$B, time = 7, span = 0.25)$pred
pred_IC <- f_trend(x = IC$I, time = 7, span = 0.25)$pred
pred_IC_B <- f_trend(x = IC$B, time = 7, span = 0.25)$pred
pred_IC_LCPS_B <- f_trend(x = IC_LCPS$B, time = 7, span = 0.25)$pred
pred_IC_LCPS_B_non_covid <- f_trend(x = IC_LCPS$B_non_covid, time = 7, span = 0.25)$pred
pred_IC_LCPS_B_total <- pred_IC_LCPS_B
pred_IC_LCPS_B_total[, -1] <- pred_IC_LCPS_B_total[, -1] + pred_IC_LCPS_B_non_covid[, -1]

###### FIGUREN ###### 
# Singaalwaardes obv https://www.rijksoverheid.nl/documenten/publicaties/2020/10/13/risiconiveaus-en-maatregelen-covid-19

png("Figures/Date.png", width = 1000, height = 600, pointsize = 18)

plot.new()
text(0.1, 1, paste0("Plots created on: ", Sys.time()), adj = c(0,0))

dev.off()

###### Gemelde patiënten ###### 
# Incidentie
png("Figures/Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_3d / 3 ~ COV$date, ylab = "Incidentie/dag", xlab = "Datum", lwd = 2, xlim = c(date_start, Sys.Date() + 10),
     main = "COVID-19 aantal nieuwe gemelde patienten  (RIVM-GGD)", type = "l", xaxt = "n", ylim = c(0, max(COV$I, na.rm = TRUE)))
points(COV$I ~ COV$date, cex = 0.6, pch = 16)
factor <- 1 / 7 / (100000 / Population$NLD) 
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50 * factor, 50 * factor, 150 * factor, 150 * factor), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150 * factor, 150 * factor, 250 * factor, 250 * factor), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
           date_start - 30), c(250 * factor, 250 * factor, 100000 * factor, 100000 * factor), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV$I, na.rm = TRUE)/2000) * 2000, 2000), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points(c(((pred_COV$loess[7]+pred_COV$arima[7])/2), pred_COV$lo[7], pred_COV$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 3), pch = "-", cex = 2, col = "black")
lines(c(pred_COV$lo[7], pred_COV$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = "black", lty = 2)
text(((pred_COV$loess[7]+pred_COV$arima[7])/2) ~ as.Date(max(COV$date) + 12), labels = ceiling(((pred_COV$loess[7]+pred_COV$arima[7])/2) / 50) * 50, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid"), cex=0.6, pch=c(16, NA), box.lty=1, 
       legend=c("Gemeld aantal",
                "Gemeld aantal (3-dagen gemiddelde)"))

dev.off()

# Incidentie per 100.000 
png("Figures/Incidentie_NL_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_3d_rel / 3 ~ COV$date, ylab = "Incidentie/dag per 100.000", xlab = "Datum", 
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(COV$I_rel, na.rm = TRUE)),
     main = "COVID-19 aantal nieuwe patienten (RIVM-GGD)", type = "l", lty = 1, lwd=2, xaxt = "n")
#lines(COV_test$I_pos_7d_rel  / 7 ~ COV_test$date, type = "l", lty = 2, lwd=2)
lines(COV$I_3d_rel_limb  / 3 ~ COV$date, type = "l", lty = "9414", lwd=2)
points(COV$I_rel ~ COV$date, cex = 0.6, pch = 16)
points(COV$I_rel_limb ~ COV$date, cex = 0.6, pch = 1)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50 / 7, 50 / 7, 150 / 7, 150 / 7), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150 / 7, 150 / 7, 250 / 7, 250 / 7), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(250 / 7, 250 / 7, 100000 / 7, 100000 / 7), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV$I_rel, na.rm = TRUE)/5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
# points(c(pred_COV_rel$loess[7], pred_COV_rel$lo[7], pred_COV_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 3), pch = "-", cex = 2, col = "black")
# lines(c(pred_COV_rel$lo[7], pred_COV_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = "black", lty = 2)
# text(pred_COV_rel$loess[7] ~ as.Date(max(COV$date) + 12), labels = ceiling(pred_COV_rel$loess[7] / 50) * 50, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid", NA, "9414"), cex=0.6, pch=c(16, NA, 1, NA), 
       box.lty=1, legend=c("Gemeld aantal nationaal",
                           "Gemeld aantal nationaal (3-dagen gemiddelde)",
                           "Gemeld aantal Limburg",
                           "Gemeld aantal Limburg (3-dagen gemiddelde)"))

dev.off()

# Percentage positieve testen per week
png("Figures/Perc_test_pos_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10),
     main = "COVID-19 percentage positieve testen (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0.05 * 100, 0.05 * 100, 0.1 * 100, 0.1 * 100), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0.1 * 100, 0.1 * 100, 1 * 100, 1 * 100), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV_test$prop_pos * 100, na.rm = TRUE)/2) * 2, 2), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

# Reproductie index
png("Figures/Rt_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Rt$R ~ Rt$date, ylab = "R", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10),
     ylim = c(0, 2), main = "COVID-19 reproductie index (Dashboard COVID-19)",  type = "l", lwd = 2, xaxt = "n")
polygon(c(Rt$date, rev(Rt$date)), c(Rt$Rmin, rev(Rt$Rmax)), 
        col = adjustcolor("black", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, # circa 1.0 aangenomen als 0.98 - 1.02
          date_start - 30), c(0.98, 0.98, 1.02, 1.02), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(1.02, 1.02, 100, 100), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Rt$Rmax, na.rm = TRUE)/0.5) * 0.5, 0.5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

###### Ziekenhuisopnames ###### 
png("Figures/Opnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I_3d / 3 ~ Hosp$date, ylab = "Incidentie/dag", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10),
     ylim = c(0, max(Hosp$I, na.rm = TRUE)), main = "COVID-19 ziekenhuisopnames exclusief IC (NICE)", type = "l", lwd = 2, xaxt = "n")
points(Hosp$I ~ Hosp$date, cex = 0.6, pch = 16)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0, 0, 40, 40), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30,
          date_start - 30), c(40, 40, 80, 80), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(80, 80, 1000, 1000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Hosp$I, na.rm = TRUE)/50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points(c(((pred_Hosp$loess[7]+pred_Hosp$arima[7])/2), pred_Hosp$lo[7], pred_Hosp$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 3), pch = "-", cex = 2, col = "black")
lines(c(pred_Hosp$lo[7], pred_Hosp$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = "black", lty = 2)
text(((pred_Hosp$loess[7]+pred_Hosp$arima[7])/2) ~ as.Date(max(COV$date) + 12), labels = ceiling(((pred_Hosp$loess[7]+pred_Hosp$arima[7])/2) / 10) * 10, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid"), cex=0.6, pch=c(16, NA), box.lty=1, 
       legend=c("Aantal",
                "Aantal (3-dagen gemiddelde)"))

dev.off()

###### IC opnames ###### 
png("Figures/ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I_3d / 3 ~ IC$date, ylab = "Incidentie/dag", xlab = "Datum", 
     lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(IC$I, na.rm = TRUE)),
     main = "COVID-19 IC opnames (NICE)", type = "l", lty = 1, xaxt = "n")
points(IC$I ~ IC$date, cex = 0.6, pch = 16)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0, 0, 10, 10), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(10, 10, 20, 20), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(20, 20, 10000, 10000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(IC$I, na.rm = TRUE)/10) * 10, 10), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points(c(((pred_IC$loess[7]+pred_IC$arima[7])/2), pred_IC$lo[7], pred_IC$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 3), pch = "-", cex = 2, col = "black")
lines(c(pred_IC$lo[7], pred_IC$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = "black", lty = 2)
text(((pred_IC$loess[7]+pred_IC$arima[7])/2) ~ as.Date(max(COV$date) + 12), labels = ceiling(((pred_IC$loess[7]+pred_IC$arima[7])/2) / 5) * 5, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid"), cex=0.6, pch=c(16, NA), box.lty=1, 
       legend=c("Aantal",
                "Aantal (3-dagen gemiddelde)"))

dev.off()

###### Verpleeghuislocaties ######
png("Figures/Verpleeghuislocaties_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Nurs$I_3d / 3 ~ Nurs$date, ylab = "Aantal nieuwe locaties met minimaal 1 besmette bewoner", xlab = "Datum", 
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(Nurs$I, na.rm = TRUE)), 
     main = "COVID-19 verpleeghuislocaties (Dashboard COVID-19)", type = "l", lwd = 2, xaxt = "n")
points(Nurs$I ~ Nurs$date, cex = 0.6, pch = 16)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Nurs$I, na.rm = TRUE)/5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid"), cex=0.6, pch=c(16, NA), box.lty=1, 
       legend=c("Aantal",
                "Aantal (3-dagen gemiddelde)"))

dev.off()

###### Sterfte ######
png("Figures/Sterfte_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I_3d / 3 ~ Death$date, ylab = "Incidentie/dag", xlab = "Datum",
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(Death$I, na.rm = TRUE)),  
     main = "COVID-19 sterfte (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
lines(Death$I_3d_limb / 3 ~ Death$date, type = "l", lty = "9414")
points(Death$I ~ Death$date, cex = 0.6, pch = 16)
points(Death$I_limb ~ Death$date, cex = 0.6, pch = 1)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Death$I, na.rm = TRUE) / 10) * 10, 10), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col=c(1, 1), lty=c("solid", "9414"), cex=0.6, box.lty=1,
       legend=c("Nationaal", "Limburg"))
legend("topleft", inset = 0.05, col=c(1, 1), lty=c(NA, "solid", NA, "9414"), cex=0.6, pch=c(16, NA, 1, NA), 
       box.lty=1, legend=c("Gemeld aantal nationaal",
                           "Gemeld aantal nationaal (3-dagen gemiddelde)",
                           "Gemeld aantal Limburg",
                           "Gemeld aantal Limburg (3-dagen gemiddelde)"))
dev.off()

###### Internationaal ######
# Incidentie
png("Figures/Incidentie_INT_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_7d_rel ~ COV$date, ylab = "Incidentie/week per 100.000", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10),
     ylim = c(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE)/100) * 100), main = "COVID-19 aantal nieuwe gemelde patienten", type = "l", col = "black", lwd = 4, xaxt = "n")
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50, 50, 150, 150), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150, 150, 250, 250), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(250, 250, 100000, 100000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
lines(Int[Int$iso == levels(Int$iso)[1], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[2], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[3], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[4], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[5], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[6], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[7], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[8], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[9], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[10], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[11], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[12], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[14], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[15], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[16], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[17], ]$I_7d_rel ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE)/100) * 100, 100), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty=c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex=0.6, box.lty=1,
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()

# Percentage positieve testen per 100.000 per week
png("Figures/Perc_test_pos_INT.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10),
     ylim = c(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE)/5) * 5), main = "COVID-19 percentage positieve testen", type = "l", col = "black", lwd = 4, xaxt = "n")
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30,
          date_start - 30), c(5, 5, 10, 10),
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30,
          date_start - 30), c(10, 10, 100, 100),
         col = adjustcolor("orange", alpha.f = 0.3), border = NA)
lines(Int[Int$iso == levels(Int$iso)[1], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[2], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[3], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[4], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[5], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[6], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[7], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[8], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[9], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[10], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[11], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[12], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[14], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[15], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[16], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[17], ]$prop_test_pos * 100 ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE)/5) * 5, 5), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty=c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex=0.6, box.lty=1,
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()

# Government Stringency Index (see https://ourworldindata.org/policy-responses-covid#government-stringency-index)
png("Figures/Stringency_index_INT.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Int[Int$iso == levels(Int$iso)[13], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[13], ]$date, ylab = "Index", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10),
     ylim = c(0, 100), main = "Government Stringency Index - composite of nine response metrics", type = "l", col = "black", lwd = 4, xaxt = "n")
lines(Int[Int$iso == levels(Int$iso)[1], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[2], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[3], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[4], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[5], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[6], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[7], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[8], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[9], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[10], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[11], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[12], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[14], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[15], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[16], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[17], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, 100, 5), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
legend("bottomleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty=c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex=0.6, box.lty=1,
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()


###### Save R session ######
save.image(file="Figures/COVID19.RData") 
