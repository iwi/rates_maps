#install the packages
if (!require(rgdal)) install.packages("rgdal")
if (!require(mapproj)) install.packages("mapproj")
if (!require(raster)) install.packages("raster")
if (!require(rgeos)) install.packages("rgeos")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(plyr)) install.packages("plyr")
if (!require(gpclib)) install.packages("gpclib", type="source")

gpclibPermit()

setwd("C:/Users/Annie/Documents/Work/Maps/cpa_maps/inputs")

#read in laa to cpa mapping
laa_cpa_mapping <- read.csv("laa_cpa.csv", header = TRUE)

#read in LAA polygons
laa_polygons <- readOGR("ctyua_ew_generalised_WGS84.json", "OGRGeoJSON")

#merge by laa to add cpa column to polygon data
laa_polygons_with_cpa_data <- merge(laa_polygons, laa_cpa_mapping,
                                    by.x = "CTYUA13NM", by.y = "LAA",
                                    all.x = TRUE, all.y = TRUE)

# aggregate laa polygons by the 21 cpas (aggregate by cpa_code)
cpa_polygons <- raster::aggregate(laa_polygons_with_cpa_data, "cpa_code")

#fortify and merge to create the data frame ggplot will show on the map
cpa_polygons@data$id <- rownames(cpa_polygons@data)
cpa_polygons.points <- fortify(cpa_polygons, region = "id")
cpa_polygons.df <- merge(cpa_polygons.points, cpa_polygons@data, sort=FALSE)

# merge to add rates
rates <- read.csv("cpa_rates.csv", header = TRUE)
cpa_polygons.df <- merge(cpa_polygons.df, rates,
                         by.x = "cpa_code", by.y = "cpa_code",
                         all.x = TRUE, all.y = TRUE)

### ggplot2 maps ###

# add quintiles field for discrete  scale mapping
cpa_polygons.df$quintiles <- cut(cpa_polygons.df$rate,6)

# discrete scale map
ggplot(data = cpa_polygons.df, aes(x= long, y = lat, group = group, fill=quintiles)) +
  geom_polygon() +
  geom_path() +
  ggtitle("Rates by CPA - in quintiles") +
  scale_fill_brewer(palette="Blues")

# continuous scale map
ggplot(data = cpa_polygons.df, aes(x= long, y = lat, group = group, fill=rate)) +
  geom_polygon() +
  geom_path(size = 0.3) +
  ggtitle("Rates by CPA") +
  scale_fill_gradient(low = 'lightblue', high = 'darkblue')

### ggvis interactive maps ###
library(dplyr)
library(ggvis)

# this is for the tooltip. it does a lookup into the data frame
# and then uses those values for the popup
all_values <- function(x) {
  if(is.null(x)) return(NULL)
  row <- cpa_polygons.df[cpa_polygons.df$id==x$id,c("CPA","rate1","rate2")]
  unique_row <- unique(row)
  paste0(names(unique_row), ": ", format(unique_row), collapse = "<br />")
}

cpa_polygons.df %>%
  group_by(group, id) %>%
  ggvis(~long, ~lat) %>%
  layer_paths(strokeWidth := 0.5, stroke := "white", fill = ~rate1) %>%
  scale_numeric("fill", range = c("lightblue","darkblue")) %>%
  hide_axis("x") %>% hide_axis("y") %>%
  #add_legend(title = "Rate 1 (%)") %>% # for some reason this gives an error!
  add_tooltip(all_values, "hover")