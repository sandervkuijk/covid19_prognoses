f_pdf_rivm_test <- function(x) {

PDF_RIVM_test <- pdf_text(x)
lines <- unlist(strsplit(PDF_RIVM_test, '\r\n')) 
start <- which(grepl('Aantal testen uitgevoerd door de GGD', lines))[1] + 2
stop <- which(grepl('Totaal', lines)) - 1
stop <- stop[stop > start][1]

write(lines, file = "PDF_RIVM_test.txt", sep = "\t")
dat_RIVM_test <- read.table("PDF_RIVM_test.txt", skip = start, nrows = stop - start, fill = TRUE, 
                            colClasses = c(rep("character", 3), rep("numeric", 3)))[, -2]
dat_RIVM_test[, 1] <- as.Date(dat_RIVM_test[, 1], tryFormats = c("%d-%m-%Y"))
dat_RIVM_test[, 2] <- as.Date(dat_RIVM_test[, 2], tryFormats = c("%d-%m-%Y"))
colnames(dat_RIVM_test) <- c("datum_van", "datum_tot", "totaal", "positief", "prop_pos")
dat_RIVM_test$prop_pos <- dat_RIVM_test$positief / dat_RIVM_test$totaal
# dat_RIVM_test$date <- seq(as.Date(paste(2020, dat_RIVM_test$week[1], 1, sep="-"), "%Y-%U-%u"), 
#                          length.out = length(dat_RIVM_test$week), by = "1 week") + 1
file.remove("PDF_RIVM_test.txt")

return(dat_RIVM_test)

}
