#############################################################################
### Topography, NOAA Coastal Relief Model, 3 arc second, Vol. 10 (Hawaii) ###
### https://coastwatch.pfeg.noaa.gov/erddap/griddap/usgsCeCrm10.html      ###
### NOAA NGDC   (Dataset ID: usgsCeCrm10)                                 ###
#############################################################################


library(sp)

df = read.csv('EFH Gut Content_Sampling Effort_Oahu.csv')
df = df %>% subset(Species == "Mahi")
df$size = as.numeric(df$Size..units..FL.in.cm.)
df$Sex. = ifelse(df$Sex. == "female", "Female", df$Sex.)
df$Sex. = ifelse(df$Sex. == "", "Unknown", df$Sex.)
df$Sex. = ifelse(df$Sex. == "not recorded", "Not recorded", df$Sex.)
df$Sex. = ifelse(df$Sex. %in% c("Unknown", "Not recorded"), "Not recorded / Unknown", df$Sex.)
table(df$Sex.)

size = df %>% 
  ggplot(aes(x = size, fill = Sex., color = Sex.)) + 
  geom_density(alpha = .6, position = "identity") +
  labs(x = "Size (cm)", y = "Frequency") + 
  scale_fill_discrete("") + 
  scale_color_discrete("") + 
  ggdark::dark_theme_minimal() +
  # theme_minimal() + 
  theme(legend.position = c(0.1, 0.9))

df = read.csv('EFH Gut Content_Sampling Effort_Oahu.csv')
df = df %>% subset(Species == "Mahi")
df$Sex. = ifelse(df$Sex. == "female", "Female", df$Sex.)
df$Sex. = ifelse(df$Sex. == "", "Unknown", df$Sex.)
df$Sex. = ifelse(df$Sex. == "not recorded", "Not recorded", df$Sex.)
df$Sex. = ifelse(df$Sex. %in% c("Unknown", "Not recorded"), "Not recorded / Unknown", df$Sex.)
df$depth = as.numeric(df$Depth..m.)
df$depth = round(df$depth, 0)

depth = df %>% 
  group_by(depth, Sex.) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(depth, n, color = factor(Sex.), group = Sex.)) + 
  geom_point(size = 5, alpha = 0.5, show.legend = F) + 
  geom_smooth(se = F, show.legend = F, size = 0.5) +
  facet_wrap(.~Sex., nrow = 1) + 
  scale_x_log10("Depth caught (m)") + 
  ggdark::dark_theme_minimal() +
  # theme_minimal() + 
  theme(legend.position = c(0.2, 0.8))

size + depth

df = read.csv('EFH Gut Content_Sampling Effort_Oahu.csv')
df = df %>% subset(Species == "Mahi")
df$Sex. = ifelse(df$Sex. == "female", "Female", df$Sex.)
df$Sex. = ifelse(df$Sex. == "", "Unknown", df$Sex.)
df$Sex. = ifelse(df$Sex. == "not recorded", "Not recorded", df$Sex.)
df$Sex. = ifelse(df$Sex. %in% c("Unknown", "Not recorded"), "Not recorded / Unknown", df$Sex.)
df = df %>% subset(Lon != "")
chd = "°"
chm = "'"
chs = "\""
cd = char2dms(df$Lon, chd= chd, chm = chm, chs = chs)
df$Lon = as.numeric(cd)
cd = char2dms(df$Lat, chd= chd, chm = chm, chs = chs)
df$Lat = as.numeric(cd)

df$Year = substr(df$Date, 1, 4)
df$Month = substr(df$Date, 5, 6)
df$Day = substr(df$Date, 7, 8)

load('Topography_NOAA_CRM_vol10_min0_max2500.RData')

sum = df %>% 
  group_by(Year, Sex., Lon, Lat) %>% 
  summarise(n = n())
  
bathymetry = topo %>%
  mutate(x = round(x, 2),
         y = round(y, 2)) %>% 
  group_by(x, y) %>%
  summarise(x = mean(x),
            y = mean(y),
            Topography = mean(Topography, na.rm = T)) 

bathymetry <- bathymetry %>% subset(x > range(pretty(df$Lon))[1] 
                        & x < range(pretty(df$Lon))[2] 
                        & y > range(pretty(df$Lat))[1] 
                        & y < range(pretty(df$Lat))[2])

map = ggplot() +
  geom_raster(data = bathymetry, aes(x, y, 
                                     # width = 0.005, height = 0.005) 
                                     fill = Topography), 
            show.legend = F, alpha = 0.5) +
  geom_point(data = sum, aes(Lon, Lat, color = Sex., size = n)) + 
  # scale_fill_gradientn(colours = matlab.like(100)) +
  coord_fixed() +
  ggdark::dark_theme_minimal() +
  # theme_minimal() +
  theme(axis.title = element_blank())

pdf("Mahi_oahu.pdf", height = 8, width = 20)
(size / depth) | map
dev.off()
