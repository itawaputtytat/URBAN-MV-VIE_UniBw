
# Preparatory settings ----------------------------------------------------

sett_file <- c()
sett_file$file_name <- "template_prec_vhcl.xlsx"



# Export as Excel file ----------------------------------------------------

library(openxlsx)
write.xlsx(dat, sett_file$file_name, rowNames = F)
