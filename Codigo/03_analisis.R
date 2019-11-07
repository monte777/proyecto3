#Cargamos la librerías que se utilizarán

library(gstat)
library(sf)
library(sp)
library(MASS)
library(dismo)
library(tmap)
library(rgdal)
library(tidyverse)
library(ggfortify)
library(automap)
library(spatstat)
library(maptools)
library(raster)
library(RColorBrewer)
library(tmaptools)

#cargamos los datos
datos <- readRDS("Datos/datos_finales.Rds")

#nombres de columnas en minúscula
colnames(datos) <- tolower(colnames(datos))

#Pasar los datos a numericos
datos1 <- datos %>% 
    filter(elementos == "LLUVIA") %>%
    dplyr::select(cod_estacion,alt,ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic,lon,lat) %>% 
    mutate_if(is.character,as.numeric) %>% 
    mutate(cod = factor(cod_estacion)) %>% 
    group_by(cod) %>% 
    mutate(lluvia_prom = mean(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_med = median(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_min = min(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)),
           lluvia_max = max(c(ene,feb,mar,abr,may,jun,jul,ago,set,oct,nov,dic)))%>% 
    dplyr::select(cod,alt,lluvia_prom,lluvia_med,lluvia_min,lluvia_max,lon,lat) %>% 
    ungroup()

datos_sp <- st_as_sf(datos1,coords = c("lon","lat")) 
provincias_sp <- read_sf(dsn="Datos/provincias",layer = "provincias")

rm(datos)
#Plots 
tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_prom",alpha=0.9,col="steelblue",
               title.size = "Lluvia promedio(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_med",alpha=0.9,col="steelblue",
               title.size = "Lluvia mediana(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_min",alpha=0.9,col="steelblue",
               title.size = "Lluvia mínima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

tm_shape(provincias_sp) + 
    tm_polygons(col="white")+
    tm_shape(datos_sp) + 
    tm_bubbles(size = "lluvia_max",alpha=0.9,col="steelblue",
               title.size = "Lluvia máxima(mm)")+
    tm_compass(type="rose",size =4, position = c(0.75,0.75))+
    tm_scale_bar(position = c(0.40,0.04))

#Leer datos como sp
coordinates(datos1) <- c("lon", "lat") 
provincias <- as(provincias_sp,"Spatial")
proj4string(provincias) <- CRS("+proj=tmerc +lat_0=0 +lon_0=-84 +k=0.9999 +x_0=500000 +y_0=0 +ellps=WGS84 +towgs84=0.0,0.0,0.0,0.0,0.0,0.0,0.0 +units=m +no_defs")
coord <- CRS("+init=epsg:4326") 
provincias <- spTransform(provincias, coord)
projection(datos1) <- projection(provincias)
datos1@bbox <- provincias@bbox

plot(provincias)
plot(datos1,add=T)
rm(coord,datos_sp,provincias_sp)
#Análisis no Geospacial
#Hacer regresiones IDW

#Diagramas de voronoi
th  <-  as(dirichlet(as.ppp(datos1)), "SpatialPolygons")
proj4string(th) <- proj4string(datos1)
th.z     <- over(th, datos1, fn=mean)
th.spdf  <-  SpatialPolygonsDataFrame(th, th.z)
th.clp   <- raster::intersect(provincias,th.spdf)

tm_shape(th.clp) + 
    tm_polygons(col="lluvia_prom", palette="-viridis", auto.palette.mapping=FALSE,
                title="Lluvia promedio(mm)") +
    tm_legend(legend.outside=TRUE)

tm_shape(th.clp) + 
    tm_polygons(col="lluvia_med", palette="-viridis", auto.palette.mapping=FALSE,
                title="Lluvia mediana(mm)") +
    tm_legend(legend.outside=TRUE)

tm_shape(th.clp) + 
    tm_polygons(col="lluvia_min", palette="-viridis", auto.palette.mapping=FALSE,
                title="Lluvia mínima(mm)") +
    tm_legend(legend.outside=TRUE)

tm_shape(th.clp) + 
    tm_polygons(col="lluvia_max", palette="-viridis", auto.palette.mapping=FALSE,
                title="Lluvia máxima(mm)") +
    tm_legend(legend.outside=TRUE)
rm(th,th.z,th.spdf,th.clp)

#IDW
grd <- as.data.frame(spsample(datos1, "regular", n=50000))
names(grd) <- c("lon", "lat")
coordinates(grd) <- c("lon", "lat")
gridded(grd) <- TRUE 
fullgrid(grd) <- TRUE 
proj4string(grd) <- proj4string(datos1)
datos.idw <- gstat::idw(lluvia_prom ~ 1, datos1, newdata=grd, idp=3.0)
r <- raster(datos.idw)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia promedio(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

datos.idw <- gstat::idw(lluvia_med ~ 1, datos1, newdata=grd, idp=3.0)
r <- raster(datos.idw)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia mediana(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

datos.idw <- gstat::idw(lluvia_min ~ 1, datos1, newdata=grd, idp=3.0)
r <- raster(datos.idw)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia mínima(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

datos.idw <- gstat::idw(lluvia_max ~ 1, datos1, newdata=grd, idp=3.0)
r <- raster(datos.idw)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia máxima(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

rm(datos.idw,r,r.m)

#Variogramas
v1 <- variogram(lluvia_prom~1,datos1)
v2 <- variogram(lluvia_med~1,datos1)
v3 <- variogram(lluvia_min~1,datos1)
v4 <- variogram(lluvia_max~1,datos1)

f1 <- fit.variogram(v1, fit.ranges = FALSE, fit.sills = FALSE,
                    vgm(psill=6651, model="Mat", range=11, nugget=161,kappa = 2))
plot(v1,f1)
f2 <- fit.variogram(v2, fit.ranges = FALSE, fit.sills = FALSE,
                    vgm(psill=10202, model="Ste", range=34, nugget=77,kappa = 2))
plot(v2,f2)
f3 <- fit.variogram(v3, fit.ranges = FALSE, fit.sills = FALSE,
                    vgm(psill=3040, model="Ste", range=34, nugget=0,kappa = 2))
plot(v3,f3)
f4 <- fit.variogram(v4, fit.ranges = FALSE, fit.sills = FALSE,
                                vgm(psill=15517, model="Sph", range=68, nugget=861))
plot(v4,f4)

#Kriging

krg1 <- krige(lluvia_prom~1,datos1, grd, f1)
krg2 <- krige(lluvia_med~1,datos1, grd, f2)
krg3 <- krige(lluvia_min~1,datos1, grd, f3)
krg4 <- krige(lluvia_max~1,datos1, grd, f4)

r <- raster(krg1)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia promedio (mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

r <- raster(krg2)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia mediana(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

r <- raster(krg3)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia mínima(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)

r <- raster(krg4)
r.m <- mask(r, provincias)

tm_shape(r.m) + 
    tm_raster(n=10,palette = "-viridis", auto.palette.mapping = FALSE,
              title="LLuvia máxima(mm)") + 
    tm_shape(datos1) + tm_dots(size=0.3) +
    tm_legend(legend.outside=TRUE)



