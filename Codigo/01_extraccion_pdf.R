# Extracción PDF

library(dplyr)
library(pdftools)

file_vector <- list.files(path = "PDF/")
file_vector %>% head()

pdf_text("PDF/69512_Zarcero.pdf") %>% 
    strsplit(split = "\n")


tablas <- vector("list", NROW(file_vector))
names(tablas) <- file_vector

for (i in seq_along(file_vector)) {
    primera_linea <- read_lines(file_vector[i], n_max = 1)
    
}
