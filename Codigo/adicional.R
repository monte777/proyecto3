#Cargamos la librerías que se utilizarán

library(gstat)
library(sf)
library(sp)
library(MASS)
library(dismo)
library(tmap)
library(rgdal)
library(tidyverse)
library(lattice)
library(RColorBrewer)
library(broom)
library(ggplot2)
library(kableExtra)
library(ggfortify)
        

#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")

#nombres de columnas en minúscula
colnames(datos) <- tolower(colnames(datos))

#Pasar los datos a numericos
#Media de temperatura
datos1 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    dplyr::select(cod_estacion,lat,lon,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluviaprom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))) %>% 
    dplyr::select(cod,lat,lon,alt,lluviaprom) %>% 
    ungroup()

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 

coordinates(datos1) <- c("lon", "lat")
datos_sp_2 <- as(datos1, "SpatialPixelsDataFrame")

provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos,datos1)

Los datos IMN contienen información sobre la ubicación y mediciones de lluvia promedio en las **48** estaciones de medición del IMN distribuidas en diferentes puntos de Costa Rica.

# Scrip clase

print(xyplot(lluviaprom~alt, as.data.frame(datos_sp), asp = .8), split = 
          c(1, 1,2,1), more = TRUE)

lm <- lm(lluviaprom~1, datos_sp)
lm_1 <- lm(lluviaprom~alt, datos_sp)

tidy(summary(lm))%>%
    knitr::kable("html",format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    kableExtra::kable_styling(c("striped"), full_width = F, position = "center")  %>%     kableExtra::add_header_above(c("Modelo LLuvia promedio media constante " = 5))

datos_sp$fitted.s <- predict(lm, datos_sp) - mean(predict(lm, datos_sp))
datos_sp$residuals <- residuals(lm)

autoplot(lm,title = "Fig. 5 Gráficos de diagnóstico de Residuos varianza constante")
autoplot(lm_1,title = "Fig. 5 Gráficos de diagnóstico de Residuos lluvia promedio ~ ALtura")

# print(spplot(datos_sp, c("fitted.s", "residuals"), col.regions = pal(), cuts = 8, colorkey=TRUE), split = c(2,1,2,1))


# Interpolación con datos nuevos, uso el mismo set

idw.out <- gstat::idw(lluviaprom~1,datos_sp_2, datos_sp_2, idp = 2.5)
as.data.frame(idw.out)[1:5,]

lm_2 <- lm(lluviaprom~1,datos_sp_2)
datos_sp_2$pred <- predict(lm_2, datos_sp_2)
datos_sp_2$se.fit <- predict(lm_2, datos_sp_2, se.fit=TRUE)$se.fit

tidy(summary(lm_2))%>%
    knitr::kable("html",format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    kableExtra::kable_styling(c("striped"), full_width = F, position = "center")  %>%     kableExtra::add_header_above(c("Modelo segundo grado LLuvia promedio media constante " = 5))


# Krige

sp.lm <- krige(lluviaprom~1,datos_sp_2, datos_sp_2)
summary(sp.lm)

# Análisis de tendencia.

sp.tr2 <- krige(lluviaprom~1,datos_sp_2, datos_sp_2, degree = 2)
lm(lluviaprom~I(lon^2)+I(lat^2)+I(lon*lat) + lon + lat, datos_sp_2)
lm(lluviaprom~ poly(lon, lat, degree = 2), datos_sp_2)

hscat(lluviaprom~1,data=datos_sp,breaks=(0:3)*10, pch=1, cex=.3, col = 'gray')


# Estimación del semivariograma y variograma

cld <- variogram(lluviaprom ~ 1, datos_sp,cloud = TRUE)
svgm <- variogram(lluviaprom ~ 1, datos_sp)

# ~1 quiere decir media constante

# Calculando el d

d <- data.frame(gamma = c(cld$gamma, svgm$gamma),
                dist = c(cld$dist, svgm$dist),
                id = c(rep("cloud", nrow(cld)), rep("sample variogram", nrow(svgm))))

xyplot(gamma ~ dist | id, d,
       scales = list(y = list(relation = "free")),
       layout = c(1, 2), as.table = TRUE,
       panel = function(x,y, ...) {
           if (panel.number() == 2)
               ltext(x+10, y, svgm$np, adj = c(0,0.05))
           panel.xyplot(x,y,...)
       }, cex = .5, pch = 3
)


v <- variogram(lluviaprom ~ 1, datos_sp)

print(xyplot(gamma ~ dist, v, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
                 for (i in 1:30) {
                     datos_sp$random = sample(datos_sp$lluviaprom)
                     v = variogram(random ~ 1, datos_sp)
                     llines(v$dist, v$gamma, col = 'grey')
                 }
                 panel.xyplot(x, y, ...)
             },
             xlab = 'distance', ylab = 'semivariance'
))

plot(variogram(lluviaprom ~ 1, datos_sp, alpha = c(0, 45, 90, 135)))


# Obtener distancia entre puntos adicional 

library(purrr)
library(geosphere)
library(rlist)

get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
    loadNamespace("purrr")
    loadNamespace("geosphere")
    longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
    longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
    distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
    distance_m = distance_list[[1]]
    if (units == "km") {
        distance = distance_m / 1000.0;
    }
    else if (units == "miles") {
        distance = distance_m / 1609.344
    }
    else {
        distance = distance_m
        # This will return in meter as same way as distHaversine function. 
        
    }
    distance
}



