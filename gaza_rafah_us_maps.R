# Create map of Gaza overlaid with U.S. cities
# Last updated: May 2024
# Author: @southatscale on Medium/Github

# Sources
# Gaza and Rafah boundaries downloaded from the Humantarian Data Exchange, from a data set contributed by the United Nations Office for the Coordination of Humanitarian Affairs: https://data.humdata.org/dataset/cod-ab-pse?
# Roads downloaded from US Census: https://www2.census.gov/geo/tiger/TIGER2022/PRISECROADS/
# County boundaries downloaded from the US Census TIGER/LINE shapefiles: https://www.census.gov/cgi-bin/geo/shapefiles/index.php


# Import packages
library(tidyverse)
library(sf)
library(sp)
library(ggspatial)


###
### Set variables for city: run the code only under the city you want to map, then skip to the "Import locality data" section
###

# Austin
utmnumber = "14"
statefips = "48"
city_centroidx = 30.290750
city_centroidy = -97.744826
locality_name = "Austin, TX"
state = "tx"
locality = "austin"
epsg_code = 32614

# Chapel Hill and Durham
utmnumber = "17"
statefips = "37"
city_centroidx = 35.995279
city_centroidy = -78.958126
locality_name = "Chapel Hill and Durham, NC"
state = "nc"
locality = "chapel_hill_durham"
epsg_code = 32617

# Charlotte
utmnumber = "17"
statefips = "37"
statefips2 = "45"
city_centroidx = 35.2120734
city_centroidy = -80.8675658
locality_name = "Charlotte, NC"
state = "nc"
locality = "charlotte"
epsg_code = 32617

# Chicago
utmnumber = "16"
statefips = "17"
city_centroidx = 41.83732
city_centroidy = -87.68635
locality_name = "Chicago, IL"
state = "il"
locality = "chicago"
epsg_code = 32616

# Dallas
utmnumber = "14"
statefips = "48"
city_centroidx = 32.7935
city_centroidy = -96.76676
locality_name = "Dallas, TX"
state = "tx"
locality = "dallas"
epsg_code = 32614

# Houston
utmnumber = "15"
statefips = "48"
city_centroidx = 29.78622
city_centroidy = -95.39056
locality_name = "Houston, TX"
state = "tx"
locality = "houston"
epsg_code = 32615

# Los Angeles
utmnumber = "11"
statefips = "06"
city_centroidx = 34.048251751962155
city_centroidy = -118.2537128901433
locality_name = "Los Angeles, CA"
state = "ca"
locality = "los_angeles"
epsg_code = 32611

# New York
utmnumber = "18"
statefips = "36"
statefips2 = "34"
city_centroidx = 40.73431849784649
city_centroidy = -73.93286610045641
locality_name = "New York, NY"
state = "ny"
locality = "new_york"
epsg_code = 32618

# Raleigh
utmnumber = "17"
statefips = "37"
city_centroidx = 35.794325
city_centroidy = -78.661625
locality_name = "Raleigh, NC"
state = "nc"
locality = "raleigh"
epsg_code = 32617

# San Francisco
utmnumber = "10"
statefips = "06"
city_centroidx = 37.621937
city_centroidy = -122.236484
locality_name = "the San Francisco Bay area, CA"
state = "ca"
locality = "san_francisco"
epsg_code = 32610

# Washington, DC
utmnumber = "18"
statefips = "11"
statefips2 = "24"
statefips3 = "51"
city_centroidx = 38.882950
city_centroidy = -77.039998
locality_name = "Washington, DC"
state = "dc"
locality = "washington"
epsg_code = 32618


###
### Import Gaza and Rafah data
###

# Import Gaza shapefile
gaza = st_read("data/pse_adm_pamop_20231019_shp/pse_admbnda_adm2_pamop_20231019.shp") %>%
  filter(ADM1_EN == "Gaza Strip")

# Summarize into one polygon
gaza_boundary <- gaza %>%
  group_by(ADM1_EN) %>%
  summarize() 

# Import shapefile and select out Rafah
rafah = st_read("data/pse_adm_pamop_20231019_shp/pse_admbnda_adm2_pamop_20231019.shp") %>%
  filter(ADM2_EN == "Rafah")

# Make sure Rafah polygon maps out right
rafah = st_make_valid(rafah)

# Transform coordinate system
gaza_boundary_4326 = st_transform(gaza_boundary, crs = 4326)
rafah_4326 = st_transform(rafah, crs = 4326)


###
### Import locality data
###

# Import county polygons
county = st_read("data/nhgis/US_county_2020.shp")

# Save UTM projection as variable
utm = paste0("+proj=utm +zone=", utmnumber, " +ellps=GRS80 +datum=NAD83")

# Reproject county polygons
county_projected = st_transform(county, utm)

# Select out state(s) you need for counties to make the mapping function run faster
county_projected = county_projected %>% 
  filter(STATEFP %in% c(statefips))


###
### Shift Gaza boundary to over locality
###

# Calculating how much you have to shift coordinates
# Calculate Rafah centroid
st_centroid(rafah_4326)

# Save Gaza centroid coordinates as variables
g_centroidx = 31.433104
g_centroidy = 34.377680

# Calculate new centroid for Gaza
new_centroidx = city_centroidx - g_centroidx
new_centroidy = city_centroidy - g_centroidy

# Make Gaza/Rafah polygons into SpatialPolygonsDataFrames to be able to manipulate the coordinates
gaza_boundary_4326_sp =  as(gaza_boundary_4326, 'Spatial')
rafah_4326_sp =  as(rafah_4326, 'Spatial')

# Next code and some comments are pulled from this source: https://gis.stackexchange.com/questions/169599/extract-all-the-polygon-coordinates-from-a-spatialpolygonsdataframe

# Coerce to points, then to data frame
# Convert to lines
lin <- as(gaza_boundary_4326_sp, "SpatialLinesDataFrame") 
lin_raf <- as(rafah_4326_sp, "SpatialLinesDataFrame") 
# Convert to points
pts <- as.data.frame(as(lin, "SpatialPointsDataFrame"))
pts_raf <- as.data.frame(as(lin_raf, "SpatialPointsDataFrame"))

# Edit latitude
pts[,6] = pts[,6] + new_centroidx
pts_raf[,17] = pts_raf[,17] + new_centroidx

# Edit longitude
pts[,5] = pts[,5] + new_centroidy
pts_raf[,16] = pts_raf[,16] + new_centroidy

# Rename columns
pts = pts %>%
  rename(lon = coords.x1, lat = coords.x2)
pts_raf = pts_raf %>%
  rename(lon = coords.x1, lat = coords.x2)

# Create sf object
gaza_boundary_4326_transformed <- pts %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")
rafah_4326_transformed <- pts_raf %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON")

# Fix projection
gaza_boundary_utm_transformed = st_transform(gaza_boundary_4326_transformed, utm)
rafah_utm_transformed = st_transform(rafah_4326_transformed, utm)


# Import street data
streets = st_read(paste0("data/roads/tl_2022_",statefips,"_prisecroads/tl_2022_",statefips,"_prisecroads.shp"))

# Reproject street data
streets_projected = st_transform(streets, utm)


# Compute bounding box - convert centroid coordinates from degrees to meters
# https://stackoverflow.com/questions/75089406/r-convert-sf-polygon-defined-using-lat-long-to-utm
lats <- c(city_centroidx)
lons <- c(city_centroidy)
df <- data.frame(lon = lons, lat = lats)
df_unprojected = st_as_sf(df, coords = c('lon', 'lat'), crs = 4326)
df_projected = st_transform(df_unprojected, epsg_code)
df_projected_df =  as.data.frame(st_coordinates(df_projected))


# Save centroid in meters as variable
centroidx = df_projected_df[1, 1]
centroidy = df_projected_df[1, 2]

# Bounding box
x1 = centroidx + 35000 
x2 = centroidx - 29000
y1 = centroidy + 32000
y2 = centroidy - 32000

# Compute coordinates for title and other objects
titlex = centroidx + 3000
titley = centroidy + 25000

rafahlabx = centroidx - 18000
rafahlaby = centroidy - 20000

inmilesx = centroidx + 19200
inmilesy = centroidy - 34600

ccx = centroidx + 37795
ccy = centroidy - 28000

citex = centroidx + 37795
citey = centroidy - 30000

signaturex = centroidx + 29750
signaturey = centroidy - 32000



# Save title as variable
map_title = paste0("Gaza’s boundary superimposed on \n ", locality_name)


###
### Map for localities that only need to visualize data for one state: Austin, Chapel Hill, Chicago, Dallas, Houston, Los Angeles, Raleigh, San Francisco
###

map = ggplot() + 
  geom_sf(data = county_projected, linewidth = .5, color=alpha("gray60",.9), fill=alpha("white",1)) +
  geom_sf(data = streets_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) +
  geom_sf(data = gaza_boundary_utm_transformed, linewidth = .7, color="#009736", fill=alpha("white",.5)) +
  geom_sf(data = rafah_utm_transformed, linewidth = 0, color="#009736", fill=alpha("#009736",.6)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0),
        panel.background=element_rect(fill = '#def4fa'),
        panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank()) + 
  labs(title = "" ,
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-180, 180)) +
  scale_y_continuous(breaks=c(-89.999, 89.999)) +
  coord_sf(xlim = c(x1, x2), ylim = c(y1, y2)) +
  geom_label(aes(x = titlex, y = titley, label = map_title), 
             size = 9, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7)) +
  
  geom_label(aes(x = rafahlabx, y = rafahlaby, label = "Rafah"), 
             size = 7, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "#009736") +
  geom_label(aes(x = inmilesx, y = inmilesy, label = "10 mi"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = signaturex, y = signaturey, label = "medium.com/@southatscale"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = citex, y = citey, label = "Sources: U.N. OCHA, U.S. Census Bureau, IPUMS NHGIS"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = ccx, y = ccy, label = "CC BY-NC-SA 4.0"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_rect(aes(xmin = centroidx + 21206.56, xmax = centroidx + 25229.92, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 25229.92, xmax = centroidx + 29253.28, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  geom_rect(aes(xmin = centroidx + 29253.28, xmax = centroidx + 33276.64, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 33276.64, xmax = centroidx + 37300, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.0, "in"), style = north_arrow_minimal)

# Save plot
ggsave(file=paste0(locality, "_map_utm_withgaza_rafah.png"), plot=map, width=8, height=8)


###
### Map for localities that need to visualize data for two states: Charlotte, New York
###
# Reproject county polygons
county_projected = st_transform(county, utm)

# Select out counties (run code only under the name of the locality you're mapping)
# New York:
county_projected = county_projected %>% 
  filter(STATEFP %in% c("34","36"))
# Charlotte:
county_projected = county_projected %>% 
  filter(STATEFP %in% c("37", "45"))

# Import state polygons
state = st_read("data/nhgis/US_state_2020.shp")

# Reproject state polygons
state_projected = st_transform(state, utm)

# Import extra state's road data
streets_2 = st_read(paste0("data/roads/tl_2022_",statefips2,"_prisecroads/tl_2022_",statefips2,"_prisecroads.shp"))
streets_2_projected = st_transform(streets_2, utm)

map = ggplot() + 
  geom_sf(data = county_projected, linewidth = .5, color=alpha("gray60",.9), fill=alpha("white",1)) +
  geom_sf(data = state_projected, linewidth = .5, color=alpha("gray50",.9), fill=alpha("gray",0)) +
  geom_sf(data = streets_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) + 
  geom_sf(data = streets_2_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) +
  geom_sf(data = gaza_boundary_utm_transformed, linewidth = .7, color="#009736", fill=alpha("white",.5)) +
  geom_sf(data = rafah_utm_transformed, linewidth = 0, color="#009736", fill=alpha("#009736",.6)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0),
        panel.background=element_rect(fill = '#def4fa'),
        panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank()) + 
  labs(title = "" ,
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-180, 180)) +
  scale_y_continuous(breaks=c(-89.999, 89.999)) +
  coord_sf(xlim = c(x1, x2), ylim = c(y1, y2)) +
  geom_label(aes(x = titlex, y = titley, label = map_title), 
             size = 9, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7)) +
  
  geom_label(aes(x = rafahlabx, y = rafahlaby, label = "Rafah"), 
             size = 7, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "#009736") +
  geom_label(aes(x = inmilesx, y = inmilesy, label = "10 mi"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = signaturex, y = signaturey, label = "medium.com/@southatscale"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = citex, y = citey, label = "Sources: U.N. OCHA, U.S. Census Bureau"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = ccx, y = ccy, label = "CC BY-NC-SA 4.0"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_rect(aes(xmin = centroidx + 21206.56, xmax = centroidx + 25229.92, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 25229.92, xmax = centroidx + 29253.28, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  geom_rect(aes(xmin = centroidx + 29253.28, xmax = centroidx + 33276.64, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 33276.64, xmax = centroidx + 37300, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.0, "in"), style = north_arrow_minimal)

# Save plot
ggsave(file=paste0(locality, "_map_utm_withgaza_rafah.png"), plot=map, width=8, height=8)


###
### Map for localities that need to visualize data for three states: DC
###
# Reproject county polygons
county_projected = st_transform(county, utm)

# Select out counties (run code only under the name of the locality you're mapping)
# Washington, DC:
county_projected = county_projected %>% 
  filter(STATEFP %in% c("11", "24", "51"))

# Import state polygons
state = st_read("data/state/tl_2023_us_state.shp")

# Reproject state polygons
state_projected = st_transform(state, utm)

# Import and reproject extra states' road data
streets_2 = st_read(paste0("data/roads/tl_2022_",statefips2,"_prisecroads/tl_2022_",statefips2,"_prisecroads.shp"))
streets_2_projected = st_transform(streets_2, utm)

streets_3 = st_read(paste0("data/roads/tl_2022_",statefips3,"_prisecroads/tl_2022_",statefips3,"_prisecroads.shp"))
streets_3_projected = st_transform(streets_3, utm)

map = ggplot() + 
  geom_sf(data = county_projected, linewidth = .5, color=alpha("gray60",.9), fill=alpha("white",1)) +
  geom_sf(data = state_projected, linewidth = .5, color=alpha("gray50",.9), fill=alpha("gray",0)) +
  geom_sf(data = streets_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) + 
  geom_sf(data = streets_2_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) +
  geom_sf(data = streets_3_projected, linewidth = 0.05, color="gray50", fill=alpha("gray",.8)) +
  geom_sf(data = gaza_boundary_utm_transformed, linewidth = .7, color="#009736", fill=alpha("white",.5)) +
  geom_sf(data = rafah_utm_transformed, linewidth = 0, color="#009736", fill=alpha("#009736",.6)) + 
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0),
        panel.background=element_rect(fill = '#def4fa'),
        panel.grid = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y = element_blank()) + 
  labs(title = "" ,
       x = "",
       y = "") +
  scale_x_continuous(breaks = c(-180, 180)) +
  scale_y_continuous(breaks=c(-89.999, 89.999)) +
  coord_sf(xlim = c(x1, x2), ylim = c(y1, y2)) +
  geom_label(aes(x = titlex, y = titley, label = map_title), 
             size = 9, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7)) +
  
  geom_label(aes(x = rafahlabx, y = rafahlaby, label = "Rafah"), 
             size = 7, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "#009736") +
  geom_label(aes(x = inmilesx, y = inmilesy, label = "10 mi"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = signaturex, y = signaturey, label = "medium.com/@southatscale"), size = 3, vjust = 0, hjust = 0.5, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = citex, y = citey, label = "Sources: U.N. OCHA, U.S. Census Bureau"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_label(aes(x = ccx, y = ccy, label = "CC BY-NC-SA 4.0"), size = 3, vjust = 0, hjust = 1, fontface = "bold", label.size = 0, fill = alpha("white",0.7), color = "black") +
  geom_rect(aes(xmin = centroidx + 21206.56, xmax = centroidx + 25229.92, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 25229.92, xmax = centroidx + 29253.28, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  geom_rect(aes(xmin = centroidx + 29253.28, xmax = centroidx + 33276.64, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "white") +
  geom_rect(aes(xmin = centroidx + 33276.64, xmax = centroidx + 37300, ymin = centroidy - 34400, ymax = centroidy - 33500), color = "black", fill = "black") +
  annotation_north_arrow(location = "bl", which_north = "true", pad_x = unit(0.0, "in"), pad_y = unit(0.0, "in"), style = north_arrow_minimal)

# Save plot
ggsave(file=paste0(locality, "_map_utm_withgaza_rafah.png"), plot=map, width=8, height=8)
