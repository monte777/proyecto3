# Extracción PDF

library(dplyr)
library(pdftools)
library(readr)    
library(data.table)
library(stringr)
library(RDS)


file_vector <- list.files(path = "PDF/")
file_vector %>% head()

pdf_text(paste0("PDF/",file_vector[[2]])) %>% 
    strsplit(split = "\n")


tablas <- vector("list", NROW(file_vector))

for (i in seq_along(file_vector)) {
    leer <- pdf_text(paste0("PDF/",file_vector[[i]])) %>% 
        strsplit(split = "\n")
    validos <-grepl("[12]\\d{3}\\s{5}",leer[[1]])
    
    a<-paste(leer[[1]][validos])
    b<- str_trim(a)
    c<- strsplit(b,"[[:blank:]]")
    c2<-lapply(c, function(x) x[!x %in% ""])
    c3 <- rbindlist(lapply(c2, function(x) data.table((t(x)))),fill = TRUE) %>% 
        dplyr::select(V1:V15) %>% mutate(archivo= !!file_vector[[i]])
    tablas[[i]] <- c3
}

datos <- bind_rows(tablas)  

save(datos, file = "Datos/datos_tabla1.Rds")
