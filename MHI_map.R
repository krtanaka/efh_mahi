library(sf)
library(ggplot2)
library(rnaturalearth)
library(marmap)

world <- ne_countries(scale = "large", returnclass = "sf")

b = marmap::getNOAA.bathy(lon1 = min(-161),
                          lon2 = max(-154),
                          lat1 = min(18),
                          lat2 = max(23),
                          resolution = 2)

b = marmap::fortify.bathy(b)

hawaii  <- ggplot(data = world) +
  geom_sf(fill = "gray20", colour="grey20") +
  coord_sf(crs = st_crs(4135), # old hawaii projection code
           xlim = c(-161, -154),
           ylim = c(18, 23), expand = FALSE) +
  scale_x_continuous(breaks = seq(-161, -154, by = 0.5)) +
  scale_y_continuous(breaks = seq(18, 23, by = 0.5)) +
  geom_contour(data = b,
               aes(x = x, y = y, z = z),
               breaks = seq(-8000, 0, by = 500),
               size = c(0.05),
               alpha = 0.5,
               colour = topo.colors(11999, rev = T)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_blank())

pdf('/Users/Kisei.Tanaka/Desktop/MHI_500m_Bathy_Countour.pdf', height = 10, width = 13)
print(hawaii)
dev.off()
