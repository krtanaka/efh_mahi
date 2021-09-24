library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)

rm(list = ls())

df = read.csv('Oahu_FADS_Buoy_Locations.csv'); df
chd = "-"
chm = "'"
# chs = "\""
cd = char2dms(df$lon, chd = chd, chm = chm)
df$lon = as.numeric(cd)
cd = char2dms(df$lat, chd= chd, chm = chm)
df$lat = as.numeric(cd)

world <- ne_countries(scale = "large", returnclass = "sf")

b_MHI = marmap::getNOAA.bathy(lon1 = min(-161),
                              lon2 = max(-154),
                              lat1 = min(18),
                              lat2 = max(23),
                              resolution = 1)

b_Oahu = marmap::getNOAA.bathy(lon1 = min(-159),
                               lon2 = max(-157),
                               lat1 = min(20.5),
                               lat2 = max(22),
                               resolution = 1)

b_MHI = marmap::fortify.bathy(b_MHI)
b_Oahu = marmap::fortify.bathy(b_Oahu)

b_MHI$z = ifelse(b_MHI$z < 0, b_MHI$z, NA)
b_MHI$z = b_MHI$z * 0.5468066492 # m to fathoms

b_Oahu$z = ifelse(b_Oahu$z < 0, b_Oahu$z, NA)
b_Oahu$z = b_Oahu$z * 0.5468066492 # m to fathoms

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
world = rmapshaper::ms_simplify(world, keep = 0.01); plot(world)

scale_x_longitude <- function(xmin = -180, xmax = 180, step = 0.002, ...) {
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
scale_y_latitude <- function(ymin = -90, ymax = 90, step = 0.002, ...) {
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

(oahu = ggplot() +
    geom_sf(data = world, fill = "grey40", colour="grey40") +
    coord_sf(crs = st_crs(4135),   # old hawaii projection code
             xlim = c(-159, -157),
             ylim = c(20.5, 22),
             expand = F) +
    geom_text(data = df, aes(lon, lat, label = id)) +
    geom_contour(data = b_Oahu,
                 aes(x = x, y = y, z = z),
                 breaks = c(-50, -100, -1000),
                 size = c(0.1),
                 colour = "grey20") +
    scale_x_continuous(breaks = seq(-159, -157, by = 0.1)) +
    scale_y_continuous(breaks = seq(20, 22, by = 0.1)) +
    theme_bw() +
    theme(axis.title = element_blank(),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)))

(mhi = ggplot() +
    geom_sf(data = world, fill = "grey40", colour="grey40") +
    coord_sf(crs = st_crs(4135),   # old hawaii projection code
             xlim = c(-161.5, -154.4),
             ylim = c(18, 22.3), expand = F) +
    geom_contour(data = b_MHI,
                 aes(x = x, y = y, z = z),
                 breaks = c(-50, -100, -1000),
                 size = c(0.05),
                 # alpha = 0.8,
                 colour = "grey20") +
    # scale_x_continuous(breaks = seq(-160.5, -154, by = 0.5)) +
    # scale_y_continuous(breaks = seq(18.5, 22.5, by = 0.5)) +
    theme_bw() +
    theme(axis.title = element_blank()))

pdf('/Users/kisei/Desktop/oahu.pdf', height = 5, width = 6)
print(oahu)
dev.off()

pdf('/Users/kisei/Desktop/mhi.pdf', height = 6, width = 9)
print(mhi)
dev.off()

mahi_map = ggdraw() +
    draw_plot(mhi) +
    draw_plot(oahu, x = 0, y = 0.08, width = 0.55, height = 0.55)

pdf('/Users/kisei/Desktop/mhi_oahu.pdf', height = 6, width = 9)
print(mahi_map)
dev.off()
