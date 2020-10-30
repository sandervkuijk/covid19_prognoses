# code formatted using formatR::tidy_source(width.cutoff = 80)
Sys.setlocale("LC_TIME", "Dutch") #set to Dutch locale (to get Dutch month names) for this session
options(scipen = 999)
rm(list = ls())
library(data.table)
library(rms)
library(forecast)
library(zoo)
source("f_trend.R")
library(rjson)
library(tidyverse)

palette(c("black", "white"))
date_start <- as.Date("2020-6-1") #as.Date("2020-3-14") #selected 1 day after RIVM data starts 
lbls <- format(seq(date_start, Sys.Date() + 30, by = "2 week"), "%e %b")

###### RETRIEVE AND MANIPULATE DATA ######
dat_NICE <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-ic/data-nice/NICE_IC_wide_latest.csv?raw=true") 
dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 
dat_RIVM_test <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-misc/data-test/RIVM_NL_test_latest.csv?raw=true")
dat_RIVM_R <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-dashboard/data-reproduction/RIVM_NL_reproduction_index.csv?raw=true")
dat_RIVM_nursery <- fread("https://github.com/J535D165/CoronaWatchNL/blob/master/data-dashboard/data-nursery/data-nursery_homes/RIVM_NL_nursery_counts.csv?raw=true")
dat_CBS <- fread("https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE")
dat_CBS_prov <- fread("https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC")
dat_OWiD <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true") #https://github.com/owid/covid-19-data/tree/master/public/data
# OWiD codebook: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv

# Data partly from:
# De Bruin, J. (2020). Novel Coronavirus (COVID-19) Cases in The Netherlands
# [Data set]. Zenodo. http://doi.org/10.5281/zenodo.4068121
#
# AND
#
# Hasell, J., Mathieu, E., Beltekian, D. et al. A cross-country database of COVID-19 testing. 
# Sci Data 7, 345 (2020). https://doi.org/10.1038/s41597-020-00688-8


# New patients at IC 
ic_intake <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)
ic_intake <- as.data.frame(t(ic_intake[c(1, 2, 4),]))
ic_intake$date <- unlist(ic_intake$V1)
ic_intake$ic_intake_proven <- unlist(ic_intake$V2)
ic_intake$ic_intake_suspected <- unlist(ic_intake$V3)
ic_intake <- ic_intake[,c(4:6)]
# IC patients cumulative
ic.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)
#New patients hospital
json_zkh_df <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/",simplify=TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill=TRUE)
zkh_new <- as.data.frame(t(json_zkh_df[c(1,2,4),]))
zkh_new$date <- unlist(zkh_new$V1)
zkh_new$new_hosp_proven <- unlist(zkh_new$V2)
zkh_new$new_hosp_suspected <- unlist(zkh_new$V3)
zkh_new <- zkh_new[,c(4:6)]
#Hospital patients cumulative
zkh.cumulative <- rjson::fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/",simplify = TRUE) %>%
  map(as.data.table) %>%
  rbindlist(fill = TRUE)

# Data manipulation
ic.cumulative$value<- as.numeric(ic.cumulative$value)
ic_intake$ic_intake_proven<- as.numeric(ic_intake$ic_intake_proven)
ic_intake$ic_intake_suspected<- as.numeric(ic_intake$ic_intake_suspected)

IC <- aggregate(formula = value ~ date, FUN = sum, data = ic.cumulative)
IC_COV<- aggregate(formula = ic_intake_proven + ic_intake_suspected ~ date, FUN = sum, data = ic_intake)
IC_COV$value<- IC_COV$'ic_intake_proven + ic_intake_suspected'
COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat_RIVM)
COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                      data = subset(dat_RIVM, Province == "Limburg"))
Hosp <- aggregate(formula = value ~ date, FUN = sum, data = zkh.cumulative)
Hosp_limb <- aggregate(formula = Hospital_admission ~ Date_of_report, FUN = sum, 
                       data = subset(dat_RIVM, Province == "Limburg"))
Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat_RIVM)
Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                        data = subset(dat_RIVM, Province == "Limburg"))
dat_RIVM_test$Type <- as.factor(dat_RIVM_test$Type)
dat_RIVM_R$Type <- as.factor(dat_RIVM_R$Type)
dat_CBS_prov$`Bevolking aan het einde van de periode (aantal)` <- as.numeric(dat_CBS_prov$`Bevolking aan het einde van de periode (aantal)`) 


# Create dataframes 
# I = incidentie, C = cumulatieve incidentie, A = huidig aantal, _rel = per 100,000
IC <- data.frame(C = IC$value,
                 I = pmax(IC$value - shift(IC$value, n=1, fill=0, type="lag"), 0, IC_COV$value),
                 I_COV = IC_COV$value,
                 date = as.Date(ic_intake$date)
)
IC <- subset(IC, IC$date >= date_start) # Select data from start date 
IC <- subset(IC, IC$date <= (Sys.Date() - 2)) # Remove data that are still being updated

COV <- data.frame(C = COV$Total_reported,
                  I = pmax(COV$Total_reported - shift(COV$Total_reported, n=1, fill=0, type="lag"), 0),
                  C_limb = COV_limb$Total_reported,
                  I_limb = pmax(COV_limb$Total_reported - shift(COV_limb$Total_reported, n=1, fill=0, type="lag"), 0),
                  date = as.Date(COV$Date_of_report)
)
COV$Iweek <- rollsumr(COV$I, k = 7, fill = NA)
COV$Iweek_limb <- rollsumr(COV$I_limb, k = 7, fill = NA)
COV$I_rel <- COV$Iweek/tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1) * 100000
COV$I_rel_limb <- COV$Iweek_limb/tail(dat_CBS_prov[dat_CBS_prov$`Regio's` == "Limburg (PV)"]$'Bevolking aan het einde van de periode (aantal)', n=1) * 100000
COV <- subset(COV, COV$date >= date_start) # Select data from start date 
COV <- subset(COV, COV$date <= Sys.Date()) # Remove todays data (as these are still being updated)

COV_test <- data.frame(I_pos = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal,
                       I_total = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       prop_pos = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Aantal / dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[2], ]$Aantal,
                       date = as.Date(rowMeans(cbind(as.Date(dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$EindDatum), as.Date(dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$BeginDatum)))),
                       week = dat_RIVM_test[dat_RIVM_test$Type==levels(dat_RIVM_test$Type)[1], ]$Week
) # Separate dataframe as unit is week (not day as in COV)
COV_test$I_pos_rel <- COV_test$I_pos/tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1) * 100000
COV_test <- subset(COV_test, COV_test$date >= date_start)

R0 <- data.frame(R = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[3], ]$Waarde,
                 Rmin = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[2], ]$Waarde,
                 Rmax = dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[1], ]$Waarde,
                 date = as.Date(dat_RIVM_R[dat_RIVM_R$Type==levels(dat_RIVM_R$Type)[3], ]$Datum)
) # Separate dataframe due to difference in dates 
R0 <- subset(R0, R0$date >= date_start)

Hosp <- data.frame(C = Hosp$value,
                   I = pmax(Hosp$value - shift(Hosp$value, n=1, fill=0, type="lag"), 0),
                   C_limb = Hosp_limb$Hospital_admission,
                  I_limb = pmax(Hosp_limb$Hospital_admission - shift(Hosp_limb$Hospital_admission, n=1, fill=0, type="lag"), 0),
                   date = as.Date(Hosp$date)
)
Hosp <- subset(Hosp, Hosp$date >= date_start) # Select data from start date 
Hosp <- subset(Hosp, Hosp$date <= Sys.Date()-1) # Remove todays data (as these are still being updated)

Nurs <- data.frame(A = dat_RIVM_nursery$Aantal,
                   I = dat_RIVM_nursery$NieuwAantal,
                   date = as.Date(dat_RIVM_nursery$Datum)
)
Nurs <- subset(Nurs, Nurs$date >= date_start) # Select data from start date 
Nurs <- subset(Nurs, Nurs$date <= Sys.Date()) # Remove todays data (as these are still being updated)

Death <- data.frame(C = Death$Deceased,
                    I = pmax(Death$Deceased - shift(Death$Deceased, n=1, fill=0, type="lag"), 0),
                    C_limb = Death_limb$Deceased,
                    I_limb = pmax(Death_limb$Deceased - shift(Death_limb$Deceased, n=1, fill=0, type="lag"), 0),
                    date = as.Date(Death$Date_of_report)
)
Death$Iweek <- rollsumr(Death$I, k = 7, fill = NA)
Death$Iweek_limb <- rollsumr(Death$I_limb, k = 7, fill = NA)
Death <- subset(Death, Death$date >= date_start) # Select data from start date 
Death <- subset(Death, Death$date <= Sys.Date()) # Remove todays data (as these are still being updated)

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
Int$Iweek <- rollsumr(Int$I_COV, by = Int$country, k = 7, fill = NA)
Int$Iweek_rel <- Int$Iweek/Int$population * 100000
Int <- subset(Int, Int$date >= date_start) # Select data from start date 
Int <- subset(Int, Int$date <= Sys.Date()) # Remove todays data (as these are still being updated)

rm(IC_COV, COV_limb, Hosp_limb, Death_limb) #clean workspace

###### TRENDLIJN ######
pred_IC <- f_trend(x = IC$I)$pred
pred_IC_COV <- f_trend(x = IC$I_COV)$pred
pred_COV <- f_trend(x = COV$I)$pred
#pred_COV_test_pos <- f_trend(x = COV_test$I_pos)$pred
#pred_COV_test_prop_pos <- f_trend(x = COV_test$prop_pos)$pred
pred_Hosp <- f_trend(x = COV$I)$pred
pred_Nurs <- f_trend(x = Nurs$I)$pred
pred_Death <- f_trend(x = Death$Iweek)$pred

###### FIGUREN ###### 
# singaalwaardes obv https://www.rijksoverheid.nl/documenten/publicaties/2020/10/13/risiconiveaus-en-maatregelen-covid-19

png("Figures/Date.png", width = 1000, height = 600, pointsize = 18)

plot.new()
text(0.1, 1, paste0("Plots created on: ", Sys.time()), adj = c(0,0))

dev.off()

###### Positief COVID-19 #####
# Incidentie
png("Figures/Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I ~ COV$date, ylab = "Incidentie/dag", xlab = "Datum", pch = 16, cex = 0.6, lwd = 2, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 aantal nieuwe patienten", type = "l", xaxt = "n")
factor <- 1 / 7 / (100000 / tail(dat_CBS$`Bevolking aan het eind van de periode (aantal)`, n=1)) 
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

dev.off()

# Incidentie per 100.000 per week
png("Figures/Incidentie_NL_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_rel ~ COV$date, ylab = "Incidentie/week per 100.000", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 aantal nieuwe patienten", type = "l", lty = 1, lwd=2, xaxt = "n")
lines(COV_test$I_pos_rel ~ COV_test$date, type = "l", lty = 2, lwd=2)
lines(COV$I_rel_limb ~ COV$date, type = "l", lty = "9414", lwd=2)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50, 50, 150, 150), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150, 150, 250, 250), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(250, 250, 100000, 100000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV$I_rel, na.rm = TRUE)/50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col=c(1, 1, 1), lty=c("solid", "dashed", "9414"), cex=0.6, box.lty=1, 
       legend=c("COVID-19 aantal patienten",
                "COVID-19 aantal positieve testen", 
                "COVID-19 aantal patienten Limburg"))
  
dev.off()

# Percentage positieve testen per week
png("Figures/Perc_test_pos_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 percentage positieve testen", type = "l", lwd = 2, xaxt = "n")
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
png("Figures/R0_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(R0$R ~ R0$date, ylab = "R0", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     ylim = c(0, 2), main = "COVID-19 reproductie index",  type = "l", lwd = 2, xaxt = "n")
polygon(c(R0$date, rev(R0$date)), c(R0$Rmin, rev(R0$Rmax)), 
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
abline(h = seq(0, ceiling(max(R0$Rmax, na.rm = TRUE)/0.5) * 0.5, 0.5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

###### Ziekenhuisopnames COVID-19 #####
png("Figures/Opnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I ~ Hosp$date, ylab = "Incidentie/dag", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     ylim = c(0, 350), main = "COVID-19 ziekenhuisopnames in Nederland", type = "l", lwd = 2, xaxt = "n")
#lines(Hosp$I_limb ~ Hosp$date, type = "l", lty = "9414")
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
#legend("topleft", inset = 0.05, col=c(1, 1), lty=c("solid", "9414"), cex=0.6, box.lty=1,
     # legend=c("Nationaal", "Limburg"))

dev.off()

###### IC COVID-19 #####
# IC TOTAL AND COVID-19
png("Figures/ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I ~ IC$date, ylab = "Incidentie/dag", xlab = "Datum", pch = 16, cex = 0.6, lwd = 2, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 IC opnames in Nederland", type = "l", lty = 1, xaxt = "n")
#lines(IC$I ~ IC$date, type = "l", lwd = 2, lty = 2)
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
#legend("topleft", inset = 0.05, col=c(1, 1), lty=c("solid", "dashed"), cex=0.6, lwd = 2, box.lty=1, 
 #      legend=c("COVID-19 IC opnames", "Totaal IC opnames"))

dev.off()

###### Verpleeghuislocaties ######
png("Figures/Verpleeghuislocaties_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Nurs$I ~ Nurs$date, ylab = "Aantal locaties met minimaal 1 besmette bewoner", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 verpleeghuislocaties", type = "l", lwd = 2, xaxt = "n")
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Nurs$I, na.rm = TRUE)/5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

###### Sterfte ######
png("Figures/Sterfte_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$Iweek ~ Death$date, ylab = "Incidentie/week", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     main = "COVID-19 sterfte", type = "l", lwd = 2, xaxt = "n")
lines(Death$Iweek_limb ~ Death$date, type = "l", lty = "9414")
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Death$Iweek, na.rm = TRUE)/50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col=c(1, 1), lty=c("solid", "9414"), cex=0.6, box.lty=1,
       legend=c("Nationaal", "Limburg"))

dev.off()

###### Internationaal ######
# Incidentie
png("Figures/Incidentie_INT_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_rel ~ COV$date, ylab = "Incidentie/week per 100.000", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     ylim = c(0, 900), main = "COVID-19 aantal nieuwe patienten", type = "l", col = "black", lwd = 4, xaxt = "n")
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50, 50, 150, 150), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150, 150, 250, 250), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(250, 250, 100000, 100000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
lines(Int[Int$iso == levels(Int$iso)[1], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[1], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[2], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[2], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[3], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[3], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[4], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[4], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[5], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[5], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[6], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[6], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[7], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[7], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[8], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[8], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2)
lines(Int[Int$iso == levels(Int$iso)[9], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[9], ]$date, type = "l", col = palette.colors(palette = "Set 1")[1], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[10], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[10], ]$date, type = "l", col = palette.colors(palette = "Set 1")[2], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[11], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[11], ]$date, type = "l", col = palette.colors(palette = "Set 1")[3], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[12], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[12], ]$date, type = "l", col = palette.colors(palette = "Set 1")[4], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[14], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[14], ]$date, type = "l", col = palette.colors(palette = "Set 1")[5], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[15], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[15], ]$date, type = "l", col = palette.colors(palette = "Set 1")[7], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[16], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[16], ]$date, type = "l", col = palette.colors(palette = "Set 1")[8], lwd = 2, lty = 2)
lines(Int[Int$iso == levels(Int$iso)[17], ]$Iweek_rel ~ Int[Int$iso == levels(Int$iso)[17], ]$date, type = "l", col = palette.colors(palette = "Set 1")[9], lwd = 2, lty = 2)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "2 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3,
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Int$Iweek_rel, na.rm = TRUE)/100) * 100, 100), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty=c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex=0.6, box.lty=1,
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()

# Percentage positieve testen per 100.000 per week
png("Figures/Perc_test_pos_INT.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
     ylim = c(0, 25), main = "COVID-19 percentage positieve testen", type = "l", col = "black", lwd = 4, xaxt = "n")
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

plot(Int[Int$iso == levels(Int$iso)[13], ]$stringency_index ~ Int[Int$iso == levels(Int$iso)[13], ]$date, ylab = "Index", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 7),
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
