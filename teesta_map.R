
##-----------------------------------START HERE------------------------------
# reading files
library(sf);library(tidyverse)
teesta <- st_read("Teesta_shape_files/Teesta_Main_polyline.shp")
sk <- st_read("sikkim_sedac_files/india-india-village-level-geospatial-socio-econ-1991-2001-sk-2001-shp/india-village-census-2001-SK.shp")
streams <- st_read("Teesta_shape_files/streams_1000_polyline.shp")
admin_sk <- st_read("sikkim_administrative/sikkim_administrative.shp")

# transform the crs for all
teesta1 <- st_transform(teesta, crs = st_crs(sk))
streams1 <- st_transform(streams, crs = st_crs(sk))
admin <- st_transform(admin_sk, crs = st_crs(sk))

# adding the point geoms for dams
DT <- data.frame(
  place=c("Teesta Stage III","Teesta Stage IV", "Teesta Stage V",  "Teesta Stage VI" ),
  longitude=c(88.645850, 88.542713, 88.503727,88.477932 ),
  latitude=c(27.604330, 27.527630, 27.387107,27.241452 ))

# st_as_sf()
# sf version 0.3-4, 0.4-0
DT_sf <- st_as_sf(DT, coords = c("longitude", "latitude"), 
                 crs = 4326, agr = "constant")
dams <- st_transform(DT_sf, crs = st_crs(sk))

# subset of sk with only the required places/villages
sub <- which(sk$NAME == "CHUNGTHANG" | sk$NAME == "SINGHIK" | sk$NAME == "RAKDONG" 
             | sk$NAME == "NAMPHING" | sk$NAME == "CHISOPANI" & sk$DISTRICT == 4)


# plotting
plot(admin[,2], reset = FALSE, lwd = 4, 
     main = "Pre Testing Locations", legend = FALSE)
plot(st_geometry(teesta1), add = TRUE, col = rgb(0,0,1), lwd = 4)
text(x = st_centroid(teesta1)$geom[[1]][1]- 2000 ,
     y = st_centroid(teesta1)$geom[[1]][2]+25000,"Teesta", cex = 0.7, font = 4,
     srt = 100, col = "black")
text(x = st_centroid(teesta1)$geom[[1]][1]- 10000 ,
     y = st_centroid(teesta1)$geom[[1]][2]-53000,"Teesta", cex = 0.7, font = 4,
     srt = 50, col = "black")


plot(sk[sub,5], add = TRUE)

plot(st_geometry(streams1), add = TRUE, col = rgb(0,0,1, alpha = 0.5), lwd = 1)
plot(sk[sk$NAME == "SINGHIK",5], col = "green", add = TRUE)
plot(sk[sk$NAME == "RAKDONG", 5], col = 69, add = TRUE)
plot(sk[sk$NAME == "CHUNGTHANG", 5], col = rgb(0.5,0.2,0.6, alpha = 0.5), add = TRUE)
plot(sk[sk$NAME == "NAMPHING", 5], col = 90, add = TRUE)
plot(sk[sk$NAME == "CHISOPANI" & sk$DISTRICT == 4, 5], col = 90, add = TRUE)
plot(dams[1:4,], col = "red", pch = c(1,2,3,4), lwd = 2,cex =1.5, add = TRUE)


legend("topright",
       legend = c("Chungthang", "Singhik", "Rakdong", "Namphing & Chisopani"),
       fill = c(rgb(0.5,0.2,0.6, alpha = 0.5), "green",69,90))
legend("topleft",
       legend = c("Teesta Stage III", "Teesta Stage IV", "Teesta Stage V", "Teesta Stage VI" ),
       pch = c(1:4),
       col = c("red"))



Sample =as.numeric(c(nrow(na.omitbook_combined1[book_combined1$add == 'Bardang',])),
                   nrow(na.omit(book_combined1[book_combined1$add == 'Dikchu','add'])),
                   nrow(na.omit(book_combined1[book_combined1$add == 'Majitar',])),
                   nrow(na.omit(book_combined1[book_combined1$add == 'Makha',])),
                   nrow(na.omit(book_combined1[book_combined1$add == 'Mamring',])),
                   nrow(book_combined1[book_combined1$add == 'Manglay',]),
                   nrow(book_combined1[book_combined1$add == 'Rangpo',]),
                   nrow(book_combined1[book_combined1$add == 'Singtam',]),
                   nrow(book_combined1[book_combined1$add == 'Sirwani',])))
