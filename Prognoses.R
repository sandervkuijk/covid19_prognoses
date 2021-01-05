Sys.setlocale("LC_TIME", "Dutch") # set to Dutch locale (to get Dutch month names) for this session
options(scipen = 999)
packages <- c("data.table", "rms", "forecast", "zoo", "tidyverse", "rjson", "pdftools")
suppressPackageStartupMessages(lapply(packages, require, character.only = TRUE)) # load packages
rm(list = ls())

source("f_trend.R") # function to estimate and extrapolate trends over time 
source("f_pdf_rivm_test.R") # function to extract test data from RIVM report

palette(c("black", "white"))
date_start <- as.Date("2020-6-1") #as.Date("2020-3-15") #minimal 2 days after RIVM data starts 
lbls <- format(seq(date_start, Sys.Date() + 30, by = "4 week"), "%e %b")

###### RETRIEVE AND COMBINE INPUT DATA ######
# RIVM
dat_RIVM <- fread("https://data.rivm.nl/covid-19/COVID-19_aantallen_gemeente_cumulatief.csv") 
dat_RIVM_R <- fromJSON(file = "https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json", simplify = TRUE)
dat_RIVM_test <- f_pdf_rivm_test("https://www.rivm.nl/sites/default/files/2021-01/COVID-19_WebSite_rapport_wekelijks_20210105_1254.pdf")
# https://www.rivm.nl/coronavirus-covid-19/actueel/wekelijkse-update-epidemiologische-situatie-covid-19-in-nederland
# vergelijk dat_RIVM_test met RIVM rapport
dat_RIVM_nursery <- fread("https://data.rivm.nl/covid-19/COVID-19_verpleeghuizen.csv") 

# NICE
dat_NICE_IC_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-cumulative", simplify = TRUE)
dat_NICE_IC_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/new-intake/", simplify = TRUE)
dat_NICE_IC_B <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/intake-count/", simplify = TRUE)
dat_NICE_Hosp_C <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-cumulative/", simplify = TRUE)
dat_NICE_Hosp_I <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/new-intake/", simplify = TRUE)
dat_NICE_Hosp_B <- fromJSON(file = "https://www.stichting-nice.nl/covid-19/public/zkh/intake-count/", simplify = TRUE)

# LCPS
dat_LCPS <- data.frame(fread("https://lcps.nu/wp-content/uploads/covid-19.csv"))
# Uitleg LCPS data: https://lcps.nu/datafeed/

# OWiD
dat_OWiD <- fread("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv?raw=true") #https://github.com/owid/covid-19-data/tree/master/public/data
# OWiD codebook: https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-codebook.csv
# https://doi.org/10.1038/s41597-020-00688-8

# CBS
dat_CBS <- fread("https://opendata.cbs.nl/CsvDownload/csv/83474NED/UntypedDataSet?dl=41CFE")
dat_CBS_prov <- fread("https://opendata.cbs.nl/CsvDownload/csv/37230ned/UntypedDataSet?dl=433DC")

# Create input data list
dat <- list(RIVM = dat_RIVM, RIVM_R = dat_RIVM_R, RIVM_R = dat_RIVM_R, RIVM_test = dat_RIVM_test, 
            RIVM_nursery = dat_RIVM_nursery, NICE_IC_C = dat_NICE_IC_C, NICE_IC_I = dat_NICE_IC_I, 
            NICE_IC_B = dat_NICE_IC_B, NICE_Hosp_C = dat_NICE_Hosp_C, NICE_Hosp_I = dat_NICE_Hosp_I, 
            NICE_Hosp_B = dat_NICE_Hosp_B, LCPS = dat_LCPS,OWiD = dat_OWiD, CBS = dat_CBS, 
            CBS_prov = dat_CBS_prov)
rm(dat_RIVM, dat_RIVM_R, dat_RIVM_test, dat_RIVM_nursery, dat_NICE_IC_C, dat_NICE_IC_I, dat_NICE_IC_B, 
   dat_NICE_Hosp_C, dat_NICE_Hosp_I, dat_NICE_Hosp_B, dat_LCPS, dat_OWiD, dat_CBS, dat_CBS_prov) # Clean workspace

###### MANIPULATE INPUT DATA ######
# RIVM
dat$RIVM_R <- data.frame(rbindlist(dat$RIVM_R, fill = TRUE)) 
dat$RIVM_R$population <- as.factor(dat$RIVM_R$population)

# NICE
dat$NICE_IC_C <- data.frame(rbindlist(dat$NICE_IC_C, fill = TRUE)) 
dat$NICE_IC_I <- data.frame(rbindlist(dat$NICE_IC_I[[1]], fill = TRUE), rbindlist(dat$NICE_IC_I[[2]], fill = TRUE) [, 2])
dat$NICE_IC_B <- data.frame(rbindlist(dat$NICE_IC_B, fill = TRUE)) 
dat$NICE_Hosp_C <- data.frame(rbindlist(dat$NICE_Hosp_C, fill = TRUE)) 
dat$NICE_Hosp_I <- data.frame(rbindlist(dat$NICE_Hosp_I[[1]], fill = TRUE), rbindlist(dat$NICE_Hosp_I[[2]], fill = TRUE) [, 2])
dat$NICE_Hosp_B <- data.frame(rbindlist(dat$NICE_Hosp_B, fill = TRUE)) 

# LCPS
dat$LCPS$Datum <- as.Date(dat$LCPS$Datum, tryFormats = c("%d-%m-%Y"))
dat$LCPS <- dat$LCPS[order(dat$LCPS$Datum), ]
row.names(dat$LCPS) <- NULL

###### DATA CALCULATIONS ######
# RIVM
COV <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, data = dat$RIVM)
COV_limb <- aggregate(formula = Total_reported ~ Date_of_report, FUN = sum, 
                      data = subset(dat$RIVM, Province == "Limburg"))
Death <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, data = dat$RIVM)
Death_limb <- aggregate(formula = Deceased ~ Date_of_report, FUN = sum, 
                        data = subset(dat$RIVM, Province == "Limburg"))
Nurs <- aggregate(formula = Total_infected_locations_reported ~ Date_of_statistic_reported, FUN = sum, data = dat$RIVM_nursery)
Nurs_I <- aggregate(formula = Total_new_infected_locations_reported ~ Date_of_statistic_reported, FUN = sum, data = dat$RIVM_nursery)
COV_Rt <- subset(dat$RIVM_R, dat$RIVM_R$population == levels(dat$RIVM_R$population)[2])

###### CREATE DATAFRAMES ######
# I = incidentie, C = cumulatieve incidentie, B = bezetting, A = huidig aantal, _rel = per 100, 000
# Populatie
Population <- data.frame(NLD = tail(as.numeric(dat$CBS$`Bevolking aan het eind van de periode (aantal)`), n = 1), 
                         Limb = tail(as.numeric(dat$CBS_prov[dat$CBS_prov$`Regio's` == "Limburg (PV)"]$'Bevolking aan het einde van de periode (aantal)'), n = 1)
)

# Infectie cijfers; gemelde patienten / aantal positieve testen / Rt
COV <- data.frame(C = as.numeric(COV$Total_reported), 
                  I = pmax(COV$Total_reported - shift(COV$Total_reported, n = 1, fill = 0, type = "lag"), 0), 
                  C_limb = as.numeric(COV_limb$Total_reported), 
                  I_limb = pmax(COV_limb$Total_reported - shift(COV_limb$Total_reported, n = 1, fill = 0, type = "lag"), 0), 
                  date = as.Date(COV$Date_of_report)
)
COV <- data.frame(COV, 
                  I_rel = COV$I / Population$NLD * 100000, 
                  I_rel_limb = COV$I_limb / Population$Limb * 100000, 
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

COV_test <- data.frame(I_pos_7d = dat$RIVM_test$positief, 
                       I_total_7d = dat$RIVM_test$totaal, 
                       prop_pos = dat$RIVM_test$prop_pos, 
                       date = dat$RIVM_test$datum_tot, 
                       I_pos_7d_rel = dat$RIVM_test$positief / Population$NLD * 100000
) 
COV_test <- subset(COV_test, COV_test$date >= date_start) # Select data from start date 

COV_Rt <- data.frame(R = COV_Rt$Rt_avg, 
                     R_lo = COV_Rt$Rt_low, 
                     R_up = COV_Rt$Rt_up, 
                     date = as.Date(COV_Rt$Date)
) 
COV_Rt <- subset(COV_Rt, COV_Rt$date >= date_start) # Select data from start date 
COV_Rt <- drop_na(COV_Rt, c("R_lo", "R_up")) # drop NAs for min and max (to prevent problems with polygon())

# Ziekenhuisopnames (excl IC)
# NICE
Hosp <- data.frame(C = dat$NICE_Hosp_C[, 2], 
                   I = dat$NICE_Hosp_I[, 2] + dat$NICE_Hosp_I[, 3], 
                   B = dat$NICE_Hosp_B[, 2], 
                   date = as.Date(dat$NICE_Hosp_I[, 1])
)
Hosp <- data.frame(Hosp, 
                   I_3d = rollsumr(Hosp$I, k = 3, fill = NA), 
                   B_3d = rollsumr(Hosp$B, k = 3, fill = NA)
)
Hosp <- subset(Hosp, Hosp$date >= date_start) # Select data from start date 
Hosp <- subset(Hosp, Hosp$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# LCPS (bezetting)
Hosp_LCPS <- data.frame(B = as.numeric(dat$LCPS$Kliniek_Bedden), 
                        date = dat$LCPS$Datum, 
                        B_3d = rollsumr(as.numeric(dat$LCPS$Kliniek_Bedden), k = 3, fill = NA),
                        B_7d = rollsumr(as.numeric(dat$LCPS$Kliniek_Bedden), k = 7, fill = NA)
)
Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date >= date_start) # Select data from start date 
#Hosp_LCPS <- subset(Hosp_LCPS, Hosp_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# IC opnames 
# NICE
IC <- data.frame(C = dat$NICE_IC_C[, 2], 
                 I = dat$NICE_IC_I[, 2] + dat$NICE_IC_I[, 3], 
                 B = dat$NICE_IC_B[, 2], 
                 date = as.Date(dat$NICE_IC_I[, 1])
)
IC <- data.frame(IC, 
                 I_3d = rollsumr(IC$I, k = 3, fill = NA), 
                 B_3d = rollsumr(IC$B, k = 3, fill = NA),
                 I_7d = rollsumr(IC$I, k = 7, fill = NA), 
                 B_7d = rollsumr(IC$B, k = 7, fill = NA)
)
IC <- subset(IC, IC$date >= date_start) # Select data from start date 
IC <- subset(IC, IC$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# LCPS (bezetting)
IC_LCPS <- data.frame(B = as.numeric(dat$LCPS$IC_Bedden_COVID), 
                      B_non_covid = as.numeric(dat$LCPS$IC_Bedden_Non_COVID), 
                      B_total = as.numeric(dat$LCPS$IC_Bedden_COVID) + as.numeric(dat$LCPS$IC_Bedden_Non_COVID), 
                      date = dat$LCPS$Datum)
IC_LCPS <- data.frame(IC_LCPS, 
                      B_3d = rollsumr(IC_LCPS$B, k = 3, fill = NA), 
                      B_non_covid_3d = rollsumr(IC_LCPS$B_non_covid, k = 3, fill = NA), 
                      B_total_3d = rollsumr(IC_LCPS$B_total, k = 3, fill = NA),
                      B_7d = rollsumr(IC_LCPS$B, k = 7, fill = NA), 
                      B_non_covid_7d = rollsumr(IC_LCPS$B_non_covid, k = 7, fill = NA), 
                      B_total_7d = rollsumr(IC_LCPS$B_total, k = 7, fill = NA)
)
IC_LCPS <- subset(IC_LCPS, IC_LCPS$date >= date_start) # Select data from start date 
#IC_LCPS <- subset(IC_LCPS, IC_LCPS$date <= Sys.Date() - 1) # Remove todays data (as these are still being updated)

# Verpleeghuislocaties
Nurs <- data.frame(A = as.numeric(Nurs$Total_infected_locations_reported), 
                   I = as.numeric(Nurs_I$Total_new_infected_locations_reported), 
                   date = as.Date(Nurs$Date_of_statistic_reported)
)
Nurs <- data.frame(Nurs, 
                   A_3d = rollsumr(Nurs$A, k = 3, fill = NA),
                   A_prop = Nurs$A / 2451, #uitgaande van 2451 Verpleeghuislocaties in NL (ligt ergens tussen 2451 - 2459)
                   I_3d = rollsumr(Nurs$I, k = 3, fill = NA),
                   I_7d = rollsumr(Nurs$I, k = 7, fill = NA)
)
Nurs <- subset(Nurs, Nurs$date >= date_start) # Select data from start date 

# Sterfte
Death <- data.frame(C = as.numeric(Death$Deceased), 
                    I = pmax(Death$Deceased - shift(Death$Deceased, n = 1, fill = 0, type = "lag"), 0), 
                    C_limb = as.numeric(Death_limb$Deceased), 
                    I_limb = pmax(Death_limb$Deceased - shift(Death_limb$Deceased, n = 1, fill = 0, type = "lag"), 0), 
                    date = as.Date(Death$Date_of_report)
)
Death <- data.frame(Death, 
                    I_rel = Death$I / Population$NLD * 100000, 
                    I_rel_limb = Death$I_limb / Population$Limb * 100000, 
                    I_3d = rollsumr(Death$I, k = 3, fill = NA), 
                    I_3d_limb = rollsumr(Death$I_limb, k = 3, fill = NA), 
                    I_3d_rel = rollsumr(Death$I, k = 3, fill = NA) / Population$NLD * 100000, 
                    I_3d_rel_limb = rollsumr(Death$I_limb, k = 3, fill = NA) / Population$Limb * 100000, 
                    I_7d = rollsumr(Death$I, k = 7, fill = NA), 
                    I_7d_limb = rollsumr(Death$I_limb, k = 7, fill = NA),
                    I_7d_rel = rollsumr(Death$I, k = 7, fill = NA) / Population$NLD * 100000, 
                    I_7d_rel_limb = rollsumr(Death$I_limb, k = 7, fill = NA) / Population$Limb * 100000
)
Death <- subset(Death, Death$date >= date_start) # Select data from start date 

# Internationaal
Int <- data.frame(continent = as.factor(dat$OWiD$continent), 
                  iso = as.factor(dat$OWiD$iso_code), 
                  country = as.factor(dat$OWiD$location), 
                  population = dat$OWiD$population, 
                  I_COV = pmax(dat$OWiD$population * dat$OWiD$new_cases_per_million / 1000000, 0), 
                  I_COV_smooth = pmax(dat$OWiD$population * dat$OWiD$new_cases_smoothed_per_million / 1000000, 0), 
                  I_COV_rel = pmax(dat$OWiD$new_cases_per_million / 10, 0), 
                  I_COV_rel_smooth = pmax(dat$OWiD$new_cases_smoothed_per_million / 10, 0), 
                  I_test_pos_rel = pmax(dat$OWiD$new_tests_per_thousand * dat$OWiD$positive_rate * 100, 0), 
                  prop_test_pos = pmax(dat$OWiD$positive_rate, 0), 
                  stringency_index = dat$OWiD$stringency_index, 
                  GDP = dat$OWiD$gdp_per_capita, 
                  LE = dat$OWiD$life_expectancy, 
                  date = as.Date(dat$OWiD$date)
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

row.names(Hosp) <- row.names(IC) <- NULL
rm(COV_limb, Death_limb, Nurs_I) # Clean workspace

###### TRENDLIJN ######
pred_COV_I <- f_trend(x = COV$I, time = 7, span = 0.25)$pred
pred_COV_I_rel <- pred_COV_I
pred_COV_I_rel[, -1] <- pred_COV_I_rel[, -1] / Population$NLD * 100000
pred_COV_I_limb <- f_trend(x = COV$I_limb, time = 7, span = 0.25)$pred
pred_COV_I_rel_limb <- pred_COV_I_limb
pred_COV_I_rel_limb[, -1] <- pred_COV_I_rel_limb[, -1] / Population$Limb * 100000

pred_Hosp_I <- f_trend(x = Hosp$I, time = 7, span = 0.25)$pred
pred_Hosp_B <- f_trend(x = Hosp$B, time = 7, span = 0.25)$pred
pred_IC_I <- f_trend(x = IC$I, time = 7, span = 0.25)$pred
pred_IC_B <- f_trend(x = IC$B, time = 7, span = 0.25)$pred

pred_Hosp_LCPS_B <- f_trend(x = Hosp_LCPS$B, time = 7, span = 0.25)$pred
pred_IC_LCPS_B <- f_trend(x = IC_LCPS$B, time = 7, span = 0.25)$pred
pred_IC_LCPS_B_non_covid <- f_trend(x = IC_LCPS$B_non_covid, time = 7, span = 0.25)$pred
pred_IC_LCPS_B_total <- pred_IC_LCPS_B
pred_IC_LCPS_B_total[, -1] <- pred_IC_LCPS_B[, -1] + pred_IC_LCPS_B_non_covid[, -1]
pred_Hosp_IC_LCPS_B_total_cov <- pred_IC_LCPS_B
pred_Hosp_IC_LCPS_B_total_cov[, -1] <- pred_IC_LCPS_B[, -1] + pred_Hosp_LCPS_B[, -1]

# Create prediction data list
pred <- list(COV_I = pred_COV_I, COV_I_rel = pred_COV_I_rel, COV_I_limb = pred_COV_I_limb,
             COV_I_rel_limb = pred_COV_I_rel_limb, Hosp_I = pred_Hosp_I, Hosp_B = pred_Hosp_B,
             IC_I = pred_IC_I, IC_B = pred_IC_B, Hosp_LCPS_B = pred_Hosp_LCPS_B, 
             IC_LCPS_B = pred_IC_LCPS_B, IC_LCPS_B_non_covid = pred_IC_LCPS_B_non_covid, 
             IC_LCPS_B_total = pred_IC_LCPS_B_total, Hosp_IC_LCPS_B_total_cov = pred_Hosp_IC_LCPS_B_total_cov)
rm(pred_COV_I, pred_COV_I_rel, pred_COV_I_limb, pred_COV_I_rel_limb, pred_Hosp_I, pred_Hosp_B, pred_IC_I, pred_IC_B,
   pred_Hosp_LCPS_B, pred_IC_LCPS_B, pred_IC_LCPS_B_non_covid, pred_IC_LCPS_B_total, pred_Hosp_IC_LCPS_B_total_cov) # Clean workspace

###### FIGUREN ###### 
# Singaalwaardes obv https://www.rijksoverheid.nl/documenten/publicaties/2020/10/13/risiconiveaus-en-maatregelen-covid-19

png("Figures/0_Date.png", width = 1000, height = 600, pointsize = 18)

plot.new()
text(0.1, 1, paste0("Plots created on: ", Sys.time()), adj = c(0, 0))

dev.off()

###### Infectie cijfers ###### 
# Incidentie
png("Figures/1_Incidentie_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_3d / 3 ~ COV$date, ylab = "Incidentie / dag", xlab = "Datum", lwd = 2, xlim = c(date_start, Sys.Date() + 10), 
     main = "COVID-19 aantal nieuwe gemelde patienten  (RIVM-GGD)", type = "l", xaxt = "n", yaxt = "n", ylim = c(0, max(c(COV$I, pred$COV_I$up), na.rm = TRUE)))
points(COV$I ~ COV$date, cex = 0.6, pch = 16)
lines(COV_test$I_pos_7d / 7 ~ COV_test$date, type = "l", lty = 3, lwd = 2)
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
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
axis(side = 2, at = seq(0, ceiling(max(c(COV$I, pred$COV_I$up), na.rm = TRUE) / 2500) * 2500, 2500))
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(COV$I, pred$COV_I$up), na.rm = TRUE) / 2500) * 2500, 2500), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$COV_I$lo[7], pred$COV_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$COV_I$lo[7], pred$COV_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$COV_I$loess[7] + pred$COV_I$arima[7]) / 2) / 50) * 50, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", "dotted"), cex = 0.6, pch = c(16, NA, NA), box.lty = 1, 
       legend = c("Gemeld aantal", 
                  "Gemeld aantal (3-dagen gemiddelde)", 
                  "Aantal positieve testen (7-dagen gemiddelde)"))

dev.off()

# Incidentie per 100.000 inwoners 
png("Figures/2_Incidentie_NL_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_3d_rel / 3 ~ COV$date, ylab = "Incidentie / dag per 100.000 inwoners", xlab = "Datum", 
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(COV$I_rel, pred$COV_I_rel$up), na.rm = TRUE)), 
     main = "COVID-19 aantal nieuwe gemelde patienten (RIVM-GGD)", type = "l", lty = 1, lwd = 2, xaxt = "n")
lines(COV$I_3d_rel_limb / 3 ~ COV$date, type = "l", lty = "9414", lwd = 2)
points(COV$I_rel ~ COV$date, cex = 0.6, pch = 16)
points(COV$I_rel_limb ~ COV$date, cex = 0.6, pch = 1)
lines(COV_test$I_pos_7d_rel / 7 ~ COV_test$date, type = "l", lty = 3, lwd = 2)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(50 / 7, 50 / 7, 150 / 7, 150 / 7), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(150 / 7, 150 / 7, 250 / 7, 250 / 7), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(250 / 7, 250 / 7, 100000 / 7, 100000 / 7), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(COV$I_rel, pred$COV_I_rel$up), na.rm = TRUE) / 5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$COV_I_rel$lo[7], pred$COV_I_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$COV_I_rel$lo[7], pred$COV_I_rel$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$COV_I_rel$loess[7] + pred$COV_I_rel$arima[7]) / 2) / 5) * 5, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "9414", "dotted"), cex = 0.6, pch = c(16, NA, 1, NA, NA), 
       box.lty = 1, legend = c("Gemeld aantal nationaal", 
                               "Gemeld aantal nationaal (3-dagen gemiddelde)", 
                               "Gemeld aantal Limburg", 
                               "Gemeld aantal Limburg (3-dagen gemiddelde)", 
                               "Aantal positieve testen (7-dagen gemiddelde)"))

dev.off()

# Percentage positieve testen
png("Figures/3_Perc_test_pos_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10), 
     main = "COVID-19 percentage positieve testen (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0.05 * 100, 0.05 * 100, 0.1 * 100, 0.1 * 100), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(0.1 * 100, 0.1 * 100, 1 * 100, 1 * 100), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV_test$prop_pos * 100, na.rm = TRUE) / 2) * 2, 2), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

# Reproductie index
png("Figures/4_Rt_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_Rt$R ~ COV_Rt$date, ylab = "R", xlab = "Datum", xlim = c(date_start, Sys.Date() + 10), 
     ylim = c(0, 2), main = "COVID-19 reproductie index (RIVM)", type = "l", lwd = 2, xaxt = "n")
polygon(c(COV_Rt$date, rev(COV_Rt$date)), c(COV_Rt$R_lo, rev(COV_Rt$R_up)), 
        col = adjustcolor("black", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, # circa 1.0 aangenomen als 0.98 - 1.02
          date_start - 30), c(0.98, 0.98, 1.02, 1.02), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(1.02, 1.02, 100, 100), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(COV_Rt$R_up, na.rm = TRUE) / 0.5) * 0.5, 0.5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))

dev.off()

###### Ziekenhuisopnames ###### 
png("Figures/5_Opnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Hosp$I_3d / 3 ~ Hosp$date, ylab = "Incidentie / dag (met verdachte of bewezen COVID-19)", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
     ylim = c(0, max(c(Hosp$I, pred$Hosp_I$up), na.rm = TRUE)), main = "COVID-19 ziekenhuisopnames exclusief IC (NICE)", type = "l", lwd = 2, xaxt = "n")
points(Hosp$I ~ Hosp$date, cex = 0.6, pch = 16)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(16, 16, 40, 40), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(40, 40, 80, 80), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(80, 80, 1000, 1000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(Hosp$I, pred$Hosp_I$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$Hosp_I$lo[7], pred$Hosp_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$Hosp_I$lo[7], pred$Hosp_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$Hosp_I$loess[7] + pred$Hosp_I$arima[7]) / 2) / 10) * 10, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid"), cex = 0.6, pch = c(16, NA), box.lty = 1, 
       legend = c("Aantal", 
                  "Aantal (3-dagen gemiddelde)"))

dev.off()

###### IC opnames ###### 
png("Figures/6_ICopnames_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC$I_3d / 3 ~ IC$date, ylab = "Incidentie / dag (met verdachte of bewezen COVID-19)", xlab = "Datum", 
     lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC$I, pred$IC_I$up), na.rm = TRUE)), 
     main = "COVID-19 IC opnames (NICE)", type = "l", lty = 1, xaxt = "n")
points(IC$I ~ IC$date, cex = 0.6, pch = 16)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(4, 4, 10, 10), 
        col = adjustcolor("yellow2", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(10, 10, 20, 20), 
        col = adjustcolor("orange", alpha.f = 0.3), border = NA)
polygon(c(date_start - 30, Sys.Date() + 30, Sys.Date() + 30, 
          date_start - 30), c(20, 20, 10000, 10000), 
        col = adjustcolor("red", alpha.f = 0.3), border = NA)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(IC$I, pred$IC_I$up), na.rm = TRUE) / 10) * 10, 10), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$IC_I$lo[7], pred$IC_I$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$IC_I$lo[7], pred$IC_I$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$IC_I$loess[7] + pred$IC_I$arima[7]) / 2) / 5) * 5, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid"), cex = 0.6, pch = c(16, NA), box.lty = 1, 
       legend = c("Aantal", 
                  "Aantal (3-dagen gemiddelde)"))

dev.off()

###### Bezetting ziekenhuisbedden ###### 
# IC bedden bezetting COVID-19
png("Figures/7_ICbezetting_cov_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC_LCPS$B_7d / 7 ~ IC_LCPS$date, ylab = "Bezetting (COVID-19)", xlab = "Datum", 
     lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B, IC$B, pred$IC_LCPS_B$up), na.rm = TRUE)), 
     main = "COVID-19 IC bedden bezetting (LCPS & NICE)", type = "l", lty = 1, xaxt = "n")
points(IC_LCPS$B ~ IC_LCPS$date, cex = 0.6, pch = 16)
lines(IC$B_7d / 7  ~ IC$date, type = "l", lty = 3, lwd = 2)
points(IC$B ~ IC$date, cex = 0.6, pch = 1)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(IC_LCPS$B, IC$B, pred$IC_LCPS_B$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$IC_LCPS_B$lo[7], pred$IC_LCPS_B$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$IC_LCPS_B$lo[7], pred$IC_LCPS_B$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$IC_LCPS_B$loess[7] + pred$IC_LCPS_B$arima[7]) / 2) / 10) * 10, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
       legend = c("Aantal LCPS", 
                  "Aantal LCPS (7-dagen gemiddelde)", 
                  "Aantal NICE", 
                  "Aantal NICE (7-dagen gemiddelde)"))

dev.off()

# IC bedden bezetting non-COVID-19 en totaal
png("Figures/8_ICbezetting_noncov_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(IC_LCPS$B_total_7d / 7 ~ IC_LCPS$date, ylab = "Bezetting", xlab = "Datum", 
     lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B_total, pred$IC_LCPS_B_total$up), na.rm = TRUE)), 
     main = "IC bedden bezetting (LCPS)", type = "l", lty = 1, xaxt = "n")
points(IC_LCPS$B_total ~ IC_LCPS$date, cex = 0.6, pch = 16)
lines(IC_LCPS$B_non_covid_7d / 7  ~ IC_LCPS$date, type = "l", lty = 3, lwd = 2)
points(IC_LCPS$B_non_covid ~ IC_LCPS$date, cex = 0.6, pch = 1)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(IC_LCPS$B_total, pred$IC_LCPS_B_total$up), na.rm = TRUE) / 50) * 50, 50), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$IC_LCPS_B_total$lo[7], pred$IC_LCPS_B_total$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$IC_LCPS_B_total$lo[7], pred$IC_LCPS_B_total$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$IC_LCPS_B_total$loess[7] + pred$IC_LCPS_B_total$arima[7]) / 2) / 25) * 25, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
       legend = c("Totaal aantal", 
                  "Totaal aantal (7-dagen gemiddelde)", 
                  "Aantal zonder COVID-19", 
                  "Aantal zonder COVID-19 (7-dagen gemiddelde)"))

dev.off()

# Ziekenhuisbedden bezetting COVID-19
png("Figures/9_Ziekenhuisbezetting_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot((IC_LCPS$B_7d + Hosp_LCPS$B_7d) / 7 ~ IC_LCPS$date, ylab = "Bezetting (COVID-19)", xlab = "Datum", 
     lwd = 2, xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(IC_LCPS$B + Hosp_LCPS$B, pred$Hosp_IC_LCPS_B_total_cov$up), na.rm = TRUE)), 
     main = "COVID-19 ziekenhuisbedden bezetting (LCPS)", type = "l", lty = 1, xaxt = "n")
points((IC_LCPS$B + Hosp_LCPS$B)  ~ IC_LCPS$date, cex = 0.6, pch = 16)
lines(Hosp_LCPS$B_7d / 7  ~ Hosp_LCPS$date, type = "l", lty = 3, lwd = 2)
points(Hosp_LCPS$B ~ Hosp_LCPS$date, cex = 0.6, pch = 1)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(IC_LCPS$B + Hosp_LCPS$B, pred$Hosp_IC_LCPS_B_total_cov$up), na.rm = TRUE) / 100) * 100, 100), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
points((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2 ~ rep(as.Date(max(COV$date) + 7), 1), pch = 16, cex = 0.6, col = "black")
points(c(pred$Hosp_IC_LCPS_B_total_cov$lo[7], pred$Hosp_IC_LCPS_B_total_cov$up[7]) ~ rep(as.Date(max(COV$date) + 7.2), 2), pch = "-", cex = 2, col = "black")
lines(c(pred$Hosp_IC_LCPS_B_total_cov$lo[7], pred$Hosp_IC_LCPS_B_total_cov$up[7]) ~ rep(as.Date(max(COV$date) + 7), 2), lwd = 1, col = adjustcolor("black", alpha.f = 0.5), lty = 1)
text(((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2) ~ as.Date(max(COV$date) + 12), 
     labels = ceiling(((pred$Hosp_IC_LCPS_B_total_cov$loess[7] + pred$Hosp_IC_LCPS_B_total_cov$arima[7]) / 2) / 25) * 25, col = "black", font = 1, cex = 0.6)
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "dotted"), cex = 0.6, pch = c(16, NA, 1, NA), box.lty = 1, 
       legend = c("Aantal inclusief IC", 
                  "Aantal inclusief IC (7-dagen gemiddelde)", 
                  "Aantal exclusief IC", 
                  "Aantal exclusief IC (7-dagen gemiddelde)"))

dev.off()

###### Verpleeghuislocaties ######
png("Figures/10_Verpleeghuislocaties_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Nurs$I_7d / 7 ~ Nurs$date, ylab = "Verpleeghuislocaties", xlab = "Datum", 
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE)), 
     main = "COVID-19 verpleeghuislocaties (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
points(Nurs$I ~ Nurs$date, cex = 0.6, pch = 16)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE) / 5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
par(new = TRUE)
plot(Nurs$A_prop * 100 ~ Nurs$date, type = "l", lty = 3, lwd = 2, xaxt = "n", yaxt = "n", ylab = "", xlab = "",
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(c(Nurs$I, Nurs$A_prop * 100), na.rm = TRUE)))
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", "dashed"), cex = 0.6, pch = c(16, NA, NA), box.lty = 1, 
       legend = c("Aantal nieuwe locaties met minimaal 1 besmette bewoner", 
                  "Aantal nieuwe locaties met minimaal 1 besmette bewoner (7-dagen gemiddelde)",
                  "Totaal aantal besmette locaties (%)"))

dev.off()

###### Sterfte ######
png("Figures/11_Sterfte_NL.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(Death$I_7d_rel / 7 ~ Death$date, ylab = "Incidentie / dag per 100.000 inwoners", xlab = "Datum", 
     xlim = c(date_start, Sys.Date() + 10), ylim = c(0, max(Death$I_rel, na.rm = TRUE)), 
     main = "COVID-19 sterfte (RIVM-GGD)", type = "l", lwd = 2, xaxt = "n")
lines(Death$I_7d_rel_limb / 7 ~ Death$date, type = "l", lty = "9414")
#points(Death$I_rel ~ Death$date, cex = 0.6, pch = 16)
#points(Death$I_rel_limb ~ Death$date, cex = 0.6, pch = 1)
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Death$I_rel, na.rm = TRUE) / 0.1) * 0.1, 0.1), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = 1, lty = c(NA, "solid", NA, "9414"), cex = 0.6, pch = c(16, NA, 1, NA), 
       box.lty = 1, legend = c("Gemeld aantal nationaal", 
                               "Gemeld aantal nationaal (7-dagen gemiddelde)", 
                               "Gemeld aantal Limburg", 
                               "Gemeld aantal Limburg (7-dagen gemiddelde)"))
dev.off()

###### Internationaal ######
# Incidentie
png("Figures/12_Incidentie_INT_per100000.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV$I_7d_rel ~ COV$date, ylab = "Incidentie / week per 100.000 inwoners", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
     ylim = c(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE) / 100) * 100), main = "COVID-19 aantal nieuwe gemelde patienten", type = "l", col = "black", lwd = 4, xaxt = "n")
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
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Int$I_7d_rel, na.rm = TRUE) / 100) * 100, 100), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty = c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex = 0.6, box.lty = 1, 
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()

# Percentage positieve testen per 100.000 inwoners per week
png("Figures/13_Perc_test_pos_INT.png", width = 1000, height = 600, pointsize = 18)
par(mar = c(5.1, 4.1, 4.1, 1.1))

plot(COV_test$prop_pos * 100 ~ COV_test$date, ylab = "%", xlab = "Datum", pch = 16, cex = 0.6, xlim = c(date_start, Sys.Date() + 10), 
     ylim = c(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE) / 5) * 5), main = "COVID-19 percentage positieve testen", type = "l", col = "black", lwd = 4, xaxt = "n")
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
axis(side = 1, at = as.Date(seq(date_start, Sys.Date() + 30, by = "4 week")), labels = lbls)
abline(v = as.Date(seq(date_start, Sys.Date() + 30, by = "1 week")), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
abline(h = seq(0, ceiling(max(Int$prop_test_pos * 100, na.rm = TRUE) / 5) * 5, 5), lty = 3, 
       col = adjustcolor("grey", alpha.f = 0.7))
legend("topleft", inset = 0.05, col = c("black", rep(palette.colors(palette = "Set 1")[c(1:5, 7:9)], 3)), lwd = 2, lty = c(rep("solid", 9), rep("dashed", 8), rep("9414", 8), rep("1234", 3)), cex = 0.6, box.lty = 1, 
       legend = levels(Int$iso)[c(13, 1:12, 14:17)])

dev.off()

###### SAVE R ENVIRONMENT ######
save.image(file = "Figures/COVID19.RData") 

