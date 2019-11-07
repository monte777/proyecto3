# Cargamos la librerías que se utilizarán

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
library(gridExtra)


#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")
provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")
dic_prov <- readRDS("Diccionario/dic_prov.Rds")

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
    mutate(
        lluvia_prom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
        lluvia_med = median(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
        lluvia_min = min(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
        lluvia_max = max(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic))
        ) %>% 
    dplyr::select(cod,lat,lon,alt,lluvia_prom,lluvia_med,lluvia_min,lluvia_max) %>% 
    ungroup() #%>% 
    #left_join(dic_prov)

# Conversión formato sf y sp

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 
#datos_sp1 <- st_as_sf(datos1,coords = c("lon","lat")) 

coordinates(datos1) <- c("lon", "lat")
datos_sp_2 <- as(datos1, "SpatialPixelsDataFrame")

#str(datos_sp_2)

#Plots 
plot_mean <- tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_prom",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

plot_med <- tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_med",alpha=0.9,col="steelblue",
               title.size = "Lluvia mediana(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

plot_min <- tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_min",alpha=0.9,col="steelblue",
               title.size = "Lluvia mínima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

plot_max <- tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_max",alpha=0.9,col="steelblue",
               title.size = "Lluvia máxima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#grid.arrange(plot_max,plot_min,plot_mean,plot_med,
             ncol = 2, nrow = 2)

#Arreglar escala
#Transformaciones
data_sp <- datos_sp %>% 
    st_set_crs(32617) %>% 
    st_transform(crs=5367)

prov_sp <- provincias_sp %>% 
    st_set_crs(32617) %>% 
    st_transform(crs=st_crs(5367))

# datos_u <- provincias_sp %>% 
#     st_set_crs(32617) %>% 
#     st_transform(crs=st_crs(5367)) %>% 
#     left_join(datos1)
# 
# data_sp_2 <- st_as_sf(datos_u,coords = c("lon","lat")) 

rm(datos_sp,provincias_sp,datos1)

Los datos IMN contienen información sobre la ubicación y mediciones de lluvia promedio en las **48** estaciones de medición del IMN distribuidas en diferentes puntos de Costa Rica.

# Scrip clase

data_sp %>% ggplot(aes(y=lluvia_min, x=alt)) + geom_point(shape = 1) 
data_sp %>% ggplot(aes(y=lluvia_max, x=alt)) + geom_point(shape = 1) 
data_sp %>% ggplot(aes(y=lluvia_med, x=alt)) + geom_point(shape = 1) 
data_sp %>% ggplot(aes(y=lluvia_prom, x=alt)) + geom_point(shape = 1) 

lm <- lm(lluvia_min~1, data_sp)
lm_1 <- lm(lluvia_min~alt, data_sp)

tidy(summary(lm))%>%
    knitr::kable("html",format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    kableExtra::kable_styling(c("striped"), full_width = F, position = "center")  %>% 
    kableExtra::add_header_above(c("Modelo LLuvia promedio media constante " = 5))

tidy(summary(lm_1))%>%
    knitr::kable("html",format.args = list(decimal.mark = ',', big.mark = ".")) %>%
    kableExtra::kable_styling(c("striped"), full_width = F, position = "center")  %>% 
    kableExtra::add_header_above(c("Modelo LLuvia promedio ~ altura" = 5))

data_sp$fitted.s <- predict(lm, data_sp) - mean(predict(lm, data_sp))
data_sp$residuals <- residuals(lm)
data_sp$fitted.s1 <- predict(lm_1, data_sp) - mean(predict(lm, data_sp))
data_sp$residuals1 <- residuals(lm_1)

autoplot(lm,title = "Fig. 5 Gráficos de diagnóstico de Residuos varianza constante")
autoplot(lm_1,title = "Fig. 5 Gráficos de diagnóstico de Residuos lluvia promedio ~ ALtura")

pal = function(n = 9) brewer.pal(n, "Reds")
print(spplot( as(data_sp, 'Spatial'), c("fitted.s", "residuals"), col.regions = pal(),
              cuts = 8, colorkey=TRUE))

print(spplot( as(data_sp, 'Spatial'), c("fitted.s1", "residuals1"), col.regions = pal(),
              cuts = 8, colorkey=TRUE))

# Interpolación con datos nuevos, puede ser una muestra o puede ser datos similados definir el idp

idw.out_min <- gstat::idw(lluvia_min~1,datos_sp_2, datos_sp_2, idp = 2.5)
idw.out_max <- gstat::idw(lluvia_max~1,datos_sp_2, datos_sp_2, idp = 2.5)
idw.out_prom <- gstat::idw(lluvia_prom~1,datos_sp_2, datos_sp_2, idp = 2.5)
idw.out_med <- gstat::idw(lluvia_med~1,datos_sp_2, datos_sp_2, idp = 2.5)

# Krige

sp.lm <- krige(lluvia_min~1,datos_sp_2, datos_sp_2)
summary(sp.lm)

# Análisis de tendencia.

sp.tr2 <- krige(lluvia_min~1,datos_sp_2, datos_sp_2, degree = 2)
lm(lluvia_min~I(lon^2)+I(lat^2)+I(lon*lat) + lon + lat, datos_sp_2)
lm(lluvia_min~ poly(lon, lat, degree = 2), datos_sp_2)

hscat(lluvia_min~1,data=data_sp,breaks=(0:9)*0.3, pch=1, cex=.3, col = 'gray')
hscat(lluvia_max~1,data=data_sp,breaks=(0:9)*0.3, pch=1, cex=.3, col = 'gray')
hscat(lluvia_prom~1,data=data_sp,breaks=(0:9)*0.3, pch=1, cex=.3, col = 'gray')
hscat(lluvia_med~1,data=data_sp,breaks=(0:9)*0.3, pch=1, cex=.3, col = 'gray')

Los gráficos de dispersión de los rezagados muestran autocorrelaciones
más débiles y con cambio de signo a distancias más largas.

# Estimación del semivariograma y variograma

cld_min <- variogram(lluvia_min ~ 1, data_sp,cloud = TRUE)
svgm_min <- variogram(lluvia_min ~ 1, data_sp)

cld_max <- variogram(lluvia_min ~ 1, data_sp,cloud = TRUE)
svgm_max <- variogram(lluvia_min ~ 1, data_sp)

cld_prom <- variogram(lluvia_min ~ 1, data_sp,cloud = TRUE)
svgm_prom <- variogram(lluvia_min ~ 1, data_sp)

cld_med <- variogram(lluvia_min ~ 1, data_sp,cloud = TRUE)
svgm_med <- variogram(lluvia_min ~ 1, data_sp)


ggplot(cld_min,aes(x=dist,y=gamma)) + 
    geom_point() +
    labs(y=expression("Semivariance ("*gamma*")"),
         x="Distance (m)", main = "Lluvia mínima medición es Estaciones IMN (mm)")

ggplot(svgm_min,aes(x=dist,y=gamma)) + 
    geom_point() +
    labs(y=expression("Semivariance ("*gamma*")"),
         x="Distance (m)", main = "Lluvia mínima medición es Estaciones IMN (mm)")

svgm_min_g <- variogram(lluvia_min ~ 1, data_sp,alpha=c(0,45,90,135),cloud = FALSE)

ggplot(svgm_min_g,aes(x=dist,y=gamma,col=factor(dir.hor))) + geom_point()

# Forma 2
d <- data.frame(gamma = c(cld_min$gamma, svgm_min$gamma),
                dist = c(cld_min$dist, svgm_min$dist),
                id = c(rep("cloud", nrow(cld_min)), rep("sample variogram", nrow(svgm_min))))

xyplot(gamma ~ dist | id, d,
       scales = list(y = list(relation = "free")),
       layout = c(1, 2), as.table = TRUE,
       panel = function(x,y, ...) {
           if (panel.number() == 2)
               ltext(x+10, y, svgm_min$np, adj = c(0,0.05))
           panel.xyplot(x,y,...)
       }, cex = .5, pch = 3)

v <- variogram(lluvia_min ~ 1, data_sp)

print(xyplot(gamma ~ dist, v, pch = 3, type = 'b', lwd = 2, col = 'darkblue',
             panel = function(x, y, ...) {
                 for (i in 1:30) {
                     data_sp$random = sample(data_sp$lluvia_min)
                     v = variogram(random ~ 1, data_sp)
                     llines(v$dist, v$gamma, col = 'grey')
                 }
                 panel.xyplot(x, y, ...)
             },
             xlab = 'distance', ylab = 'semivariance'
))

plot(variogram(lluvia_min ~ 1, data_sp, alpha = c(0, 45, 90, 135)))

Problemas, se espera que el semivariograma este fuera de los variogramas teoricos.

