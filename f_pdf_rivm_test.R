f_pdf_rivm_test <- function(x) {

PDF_RIVM_test <- pdf_text(x)
lines <- strsplit(PDF_RIVM_test, '\r\n') %>% unlist
start <- which(grepl('Aantal testen uitgevoerd door de GGD', lines)) + 2
stop <- which(grepl('Totaal', lines)) - 1
stop <- stop[stop > start][1]
write(lines, file = "PDF_RIVM_test.txt", sep = "\t")

dat_RIVM_test <- read.table("PDF_RIVM_test.txt", skip = start, nrows = stop - start, fill = TRUE, colClasses = "numeric")
colnames(dat_RIVM_test) <- c("week", "totaal", "positief", "prop_pos")
dat_RIVM_test$week <- as.numeric(23:(23 + dim(dat_RIVM_test)[1] - 1))
dat_RIVM_test$prop_pos <- dat_RIVM_test$positief / dat_RIVM_test$totaal
dat_RIVM_test$date <- seq(as.Date(paste(2020, dat_RIVM_test$week[1], 1, sep="-"), "%Y-%U-%u"), 
                          length.out = length(dat_RIVM_test$week), by = "1 week") + 1
file.remove("PDF_RIVM_test.txt")

return(dat_RIVM_test)

}
