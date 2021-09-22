library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)
library(GEOmap)
library(extrafont)

rm(list = ls())

world <- ne_countries(scale = "large", returnclass = "sf")

b = marmap::getNOAA.bathy(lon1 = min(-158.6),
                          lon2 = max(-157),
                          lat1 = min(21.01),
                          lat2 = max(21.99),
                          resolution = 1)

b = marmap::fortify.bathy(b)
b$z = ifelse(b$z < 0, b$z, NA)
b$z = b$z * 0.5468066492 # m to fathoms
summary(b$z)

islands = c("Kauai", #1
            "Lehua", #2
            "Niihau", #3
            "Kaula", #4
            "Oahu", #5
            "Molokai", #6
            "Maui", #7
            "Lanai", #8
            "Molokini", #9
            "Kahoolawe", #10
            "Hawaii")

load("MHI_islands_shp.RData")
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
world = ISL_bounds[which(ISL_bounds$ISLAND %in% toupper(islands)),]
world = st_transform(st_as_sf(world))

scale_x_longitude <- function(xmin=-180, xmax = 180, step=0.002, ...) {
  xbreaks <- seq(xmin,xmax,step)
  xlabels <- unlist(
    lapply(xbreaks, function(x){
      ifelse(x < 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
                                      paste0(abs(dms(x)$m), expression("*{minute}*")),
                                      paste0(abs(dms(x)$s)), expression("*{second}*W"))),
             ifelse(x > 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
                                             paste0(abs(dms(x)$m), expression("*{minute}*")),
                                             paste0(abs(dms(x)$s)), expression("*{second}*E"))),
                    abs(dms(x))))}))
  return(scale_x_continuous("Longitude", breaks = xbreaks, labels = xlabels, expand = c(0, 0), ...))
}

scale_y_latitude <- function(ymin=-90, ymax=90, step=0.002, ...) {
  ybreaks <- seq(ymin,ymax,step)
  ylabels <- unlist(
    lapply(ybreaks, function(x){
      ifelse(x < 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
                                      paste0(abs(dms(x)$m), expression("*{minute}*")),
                                      paste0(abs(dms(x)$s)), expression("*{second}*S"))),
             ifelse(x > 0, parse(text=paste0(paste0(abs(dms(x)$d), expression("*{degree}*")),
                                             paste0(abs(dms(x)$m), expression("*{minute}*")),
                                             paste0(abs(dms(x)$s)), expression("*{second}*N"))),
                    abs(dms(x))))}))
  return(scale_y_continuous("Latitude", breaks = ybreaks, labels = ylabels, expand = c(0, 0), ...))
}

hawaii <- ggplot() +
  geom_sf(data = world, fill = "grey40", colour="grey40") +
  coord_sf(crs = st_crs(4135),   # old hawaii projection code
           xlim = c(-158.6, -157),
           ylim = c(21.01, 21.99), expand = F) +
  scale_x_continuous(breaks = seq(-158.6, -157, by = 0.1)) +
  scale_y_continuous(breaks = seq(21.01, 21.99, by = 0.1)) +
  # scale_x_longitude(-160, -155, 0.1) +
  # scale_y_latitude(19, 22, 0.1) +
  # ylim(21.01, 21.99) +
  # xlim(-158.6, -157) +
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               # breaks = seq(-8000, 0, by = 200),
               breaks = c(-50, -100, -1000),
               size = c(0.05),
               # alpha = 0.8,
               colour = grey.colors(723)) +
  theme_minimal() +
  theme(axis.title = element_blank())

pdf('/Users/Kisei.Tanaka/Desktop/mahi_map.pdf', height = 8, width = 12)
print(hawaii)
dev.off()
