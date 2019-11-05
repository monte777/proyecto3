#Cargamos la librerías que se utilizarán

library(gstat)
library(sf)
library(sp)
library(MASS)
library(dismo)
library(tmap)
library(rgdal)
library(tidyverse)

library(sp)
library(lattice)
library(RColorBrewer)

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
    dplyr::select(cod,lat,lon,alt,lluviaprom)

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 

coordinates(datos1) <- c("lon", "lat")
datos_sp_2 <- as(datos1, "SpatialPixelsDataFrame")

provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos,datos1)



#Primer ploteo
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluviaprom",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#Análisis no Geospacial
#Hacer regresiones IDW

#Análisis Geoespacial
plot(sort(datos_sp$lluviaprom))
plot(datos_sp$alt,datos_sp$lluviaprom)
plot(variogram(lluviaprom ~ 1, datos_sp))



#dd

print(xyplot(lluviaprom~alt, as.data.frame(datos_sp), asp = .8), split = 
          c(1, 1,2,1), more = TRUE)
lm <- lm(lluviaprom~alt, datos_sp)

datos_sp$fitted.s <- predict(lm, datos_sp) - mean(predict(lm, datos_sp))

datos_sp$residuals <- residuals(lm)

print(sp::spplot(datos_sp, c("fitted.s", "residuals"), col.regions = pal(), cuts = 8, colorkey=TRUE), split = c(2,1,2,1))


#parte 2
idw.out <- gstat::idw(lluviaprom~1,datos_sp_2, datos_sp_2, idp = 2.5)
as.data.frame(idw.out)[1:5,]

lm <- lm(lluviaprom~1,datos_sp_2)
datos_sp_2$pred <- predict(lm, datos_sp_2)
datos_sp_2$se.fit <- predict(lm, datos_sp_2, se.fit=TRUE)$se.fit


sp.lm <- krige(lluviaprom~1,datos_sp_2, datos_sp_2)
summary(sp.lm)

# Trend surface analysis:
sp.tr2 <- krige(lluviaprom~1,datos_sp_2, datos_sp_2, degree = 2)

lm(lluviaprom~I(lon^2)+I(lat^2)+I(lon*lat) + lon + lat, datos_sp_2)

lm(lluviaprom~ poly(lon, lat, degree = 2), datos_sp_2)


hscat(lluviaprom~1,data=datos_sp,breaks=(0:3)*10, pch=1, cex=.3, col = 'gray')

# Estimación del semivariograma
plot(variogram(lluviaprom ~ 1, datos_sp))


cld <- variogram(lluviaprom ~ 1, datos_sp,cloud = TRUE)
svgm <- variogram(lluviaprom ~ 1, datos_sp)

## ~1 quiere decir media constante
d <- data.frame(gamma = c(cld$gamma, svgm$gamma),
                dist = c(cld$dist, svgm$dist),
                id = c(rep("cloud", nrow(cld)), rep("sample variogram", nrow(svgm)))
)

xyplot(gamma ~ dist | id, d,
       scales = list(y = list(relation = "free", 
                              #ylim = list(NULL, c(-.005,0.7)))),
                              limits = list(NULL, c(-.005,0.7)))),
       layout = c(1, 2), as.table = TRUE,
       panel = function(x,y, ...) {
           if (panel.number() == 2)
               ltext(x+10, y, svgm$np, adj = c(0,0.05)) #$
           panel.xyplot(x,y,...)
       },
       xlim = c(0, 1.5),
       cex = .5, pch = 3
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

# Distancia de puntos

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

