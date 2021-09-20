library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)
library(raster)

world <- ne_countries(scale = "large", returnclass = "sf")

b = marmap::getNOAA.bathy(lon1 = min(-158.6),
                          lon2 = max(-157),
                          lat1 = min(21.01),
                          lat2 = max(21.99),
                          resolution = 1)

b = marmap::fortify.bathy(b)

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

hawaii  <- ggplot(data = world) +
  geom_sf(fill = "gray20", colour="grey20") +
  coord_sf(crs = st_crs(4135), # old hawaii projection code
           xlim = c(-158.6, -157),
           ylim = c(21.01, 21.99), expand = F) +
  scale_x_continuous(breaks = seq(-158.6, -157, by = 0.25)) +
  scale_y_continuous(breaks = seq(21.01, 21.99, by = 0.25)) +
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-8000, 0, by = 200),
               size = c(0.05),
               alpha = 0.8,
               colour = grey.colors(5129, rev = T)) +
  theme_minimal() +
  theme(
    # axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank())

pdf('/Users/Kisei.Tanaka/Desktop/MHI_200m_Bathy_Countour.pdf', height = 10, width = 13)
print(hawaii)
dev.off()
