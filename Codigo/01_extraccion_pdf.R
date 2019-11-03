# Extracción PDF

library(dplyr)
library(pdftools)
library(readr)    
library(data.table)
library(stringr)


file_vector <- list.files(path = "PDF/")
file_vector %>% head()

a<- pdf_text(paste0("PDF/",file_vector[[2]])) %>% 
    strsplit(split = "\n")


tablas <- vector("list", NROW(file_vector))
names(tablas) <- file_vector

for (i in seq_along(file_vector)) {
    leer <- pdf_text(paste0("PDF/",file_vector[[3]])) %>% 
        strsplit(split = "\n")
    validos <-grepl("[12]\\d{3}\\s{5}",leer[[1]])
    
    a<-paste(leer[[1]][validos])
    b<- str_trim(a)
    c<- strsplit(b,"[[:blank:]]")
    c2<-lapply(c, function(x) x[!x %in% ""])
    c3 = rbindlist(lapply(c, function(x) data.table((t(x)))),fill = TRUE)
}
