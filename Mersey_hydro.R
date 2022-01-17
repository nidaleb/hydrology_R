

# Function to check and install packages
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# Checks and installs packages
packages <- c("ggplot2","sf","sp","here","units" ,"tidyverse","tidyr", "raster", "whitebox", "ggspatial", "patchwork")
check.packages(packages)

# Sets file path for DEM
dem <- here("data", "practical_2","mersey_dem_fill.tif")
dem_<- raster(here("data", "practical_2","mersey_dem_fill.tif"))


  ggplot() +
  layer_spatial(dem_) + scale_fill_continuous(na.value = NA)+
  theme_minimal() +coord_sf()+
  theme(panel.background = element_rect(fill = "light blue")) +
  annotation_scale(location = "br", line_width = .5) +
  annotation_north_arrow(location = "bl", height = unit(0.7, "cm"), width = unit(0.7, "cm"))

# Calculates D8 pointer
wbt_d8_pointer(here("data", "practical_2","mersey_dem_fill.tif"), 
               here( "output", "practical_2","mersey_dem_D8_pointer.tif"))
# Calculates D8 accumulation file
wbt_d8_flow_accumulation(
  here( "output", "practical_2","mersey_dem_D8_pointer.tif"),
  here( "output", "practical_2","mersey_dem_flow_accumulation.tif"),
  out_type = "specific contributing area",
  log = TRUE,
  pntr = TRUE,
)

# Loads acccumulation files using raster and here packages
mersey_d8 <- raster(here( "output", "practical_2","mersey_dem_flow_accumulation.tif"))

# Plots D8 using ggplot2
p_d8 <- ggplot() +
  # Adds raster layer using layer_spatial
  layer_spatial(mersey_d8, aes(fill = stat(band1))) +
  # Sets ggplot theme
  theme_classic() + coord_sf()+
  # Axis and legend labeling
  labs(fill = "Flow accumulation value", x = "Easting", y = "Northing", 
       title="D8") +
  # Sets fill symbology
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  # Removes legend, sets title size
  theme(legend.position = "none", 
        plot.title = element_text(hjust = 0.5))
p_d8


# Loads flow accumulation raster using the raster and here packages
mersey_accumulation <- raster(here("output", "practical_2", "mersey_dem_flow_accumulation.tif"))

# Loads EA monitoring sites using the st_read function
seed_points <- st_read(here("data", "practical_2", "mersey_EA_sites.shp"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_accumulation, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = seed_points, shape = 21, fill = "#FFFFFF", colour = "black", size = 3) + # Adds vector layer
  theme_classic() + 
  labs(fill = "Log-transformed flow accumulation value", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 


wbt_d8_flow_accumulation(
  here("data", "practical_2","mersey_dem_fill.tif"),
  here("output", "practical_2","mersey_dem_D8_flow_accumulation_NUE.tif"),
  out_type = "cells",
  pntr = FALSE,
)
##stream network 
wbt_extract_streams(
  here( "output", "practical_2","mersey_dem_D8_flow_accumulation_NUE.tif"),
  here( "output", "practical_2","mersey_dem_streams_act200.tif"),
  threshold= 200,
  zero_background = TRUE,
)


wbt_jenson_snap_pour_points(
  here("data", "practical_2", "mersey_EA_sites.shp"),
  here( "output", "practical_2","mersey_dem_streams_act200.tif"),
  here("output", "practical_2", "mersey_EA_sites_snapped.shp"),
 snap_dist = 500,
)

# Loads streams raster using the raster and here packages
mersey_streams <- raster(here("output", "practical_2", "mersey_dem_streams_act200.tif"))

# Crops the streams raster based on a defined extent (row and col indexes) 
mersey_streams_crop <- crop(mersey_streams, extent(mersey_streams,  632, 645, 540, 578))

# Loads original and snapped EA monitoring sites using the st_read function
seed_points <- st_read(here("data", "practical_2", "mersey_EA_sites.shp"))
snapped_seed_points <- st_read(here("output", "practical_2", "mersey_EA_sites_snapped.shp"))
# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_streams_crop, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = seed_points, shape = 21, fill = "#FFFFFF", colour = "black", size = 3) + # Adds seeds layer
  annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Stream network ID", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 
#Watershed creation

wbt_watershed(
  here( "output", "practical_2","mersey_dem_D8_pointer.tif"),
  here("output", "practical_2", "mersey_EA_sites_snapped.shp"),
  here("output", "practical_2", "mersey_watersheds.tif"),
 
)
# Loads streams raster using the raster and here packages
mersey_watersheds <- raster(here("output", "practical_2", "mersey_watersheds.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_watersheds, aes(fill = stat(band1))) + # Adds raster layer
  annotation_spatial(data = snapped_seed_points, shape = 21, fill = "#FB5858", colour = "black", size = 3) + # Adds snapped seeds layer
  theme_classic() + 
  labs(fill = "Stream network", x = "Easting", y = "Northing") +
  scale_fill_continuous(type = "viridis",  na.value = NA) +
  theme(legend.position = "top")
p 
# Converts watershed to vector format (polygon)
wbt_raster_to_vector_polygons(here("output", "practical_2", "mersey_watersheds.tif"),
                              here("output", "practical_2", "mersey_watersheds.shp"))

## Reclassification
# Loads land cover raster

land_cover <- raster(here("data", "practical_2", "mersey_LC.tif"))
# Converts the land cover raster to a factor, overwriting the original variable
land_cover <- as.factor(land_cover)
unique(land_cover)

# Categories of interest
categories <- as.data.frame(c(41, 42, 43, 91, 101, 102, 51, 52, 61, 71, 81, 171, 172, 111, 121))
colnames(categories) <- "ID"
head(categories)

# Collapse categories into groups based on ID
categories$name <- fct_collapse(as.factor(categories$ID),
                                "Arable" = c("41", "42", "43"),
                                "Heath" = c("91", "101", "102"),
                                "Grassland" = c("51", "52", "61", "71", "81"),
                                "Urban" = c("171", "172"), 
                                "Wetland" = c("111", "121"))

# Prints categories data frame
categories

# Substitutes raster values with new categories
land_cover_classified <- subs(land_cover, categories)

# Write to new raster
writeRaster(land_cover_classified, here("output", "practical_2", "mersey_LC_reclass.tif"))

#Loads land cover raster using the raster and here packages
mersey_land_cover <- raster(here("output", "practical_2", "mersey_LC_reclass.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_land_cover, aes(fill = stat(band1))) + # Adds raster layer
  theme_classic() + 
  labs(fill = "Land cover class", x = "Easting", y = "Northing") +
  scale_fill_distiller(palette = "RdYlBu", na.value = NA) + # Updated fill aesthetic
  theme(legend.position = "top")
p 
#host

soils <- raster(here("data", "practical_2", "mersey_HOST.tif"))
#Converts the land cover raster to a factor, overwriting the original variable
soils <- as.factor(soils)
unique(soils)

# Categories of interest
categories2 <- as.data.frame(c( 1, 3, 4, 5, 6, 7, 15, 16,12, 17, 18, 19, 21, 22, 8, 9, 13, 24, 10, 11, 14, 25, 26, 27, 29))
colnames(categories2) <- "ID"
head(categories2)

# Collapse categories into groups based on ID
categories2$name <- fct_collapse(as.factor(categories2$ID),
                                 "Permeable" =c( "1","3", "4", "5", "6", "7", "15", "16"),
                                 "Impermeable" = c("12", "17", "18", "19", "21", "22"),
                                 "Gleyed" = c("8", "9", "13", "24"),
                                 "Peats" = c("10", "11", "14", "25", "26", "27", "29"))

# Prints categories data frame
categories2

# Substitutes raster values with new categories
soils_classified <- subs(soils, categories2)

# Write to new raster
writeRaster(soils_classified, here("output", "practical_2", "mersey_HOST_reclass.tif"))

#Loads land cover raster using the raster and here packages
mersey_soils <- raster(here("output", "practical_2", "mersey_HOST_reclass.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_soils, aes(fill = stat(band1))) + # Adds raster layer
  theme_classic() + 
  labs(fill = "Land cover class", x = "Easting", y = "Northing") +
  scale_fill_distiller(palette = "RdYlBu", na.value = NA) + # Updated fill aesthetic
  theme(legend.position = "top")
p 
#bedrock
bedrock <- raster(here("data", "practical_2", "mersey_bedrock.tif"))
#Converts the land cover raster to a factor, overwriting the original variable
bedrock <- as.factor(bedrock)
unique(bedrock)

# Categories of interest
categories3 <- as.data.frame(c(  5, 16, 18, 24, 28, 34, 10, 11, 17, 19, 9, 15, 22.))
colnames(categories3) <- "ID"
head(categories3)

# Collapse categories into groups based on ID
categories3$name <- fct_collapse(as.factor(categories3$ID),
                                 "Sands_and_Muds" =c( "5", "16", "18", "24", "28", "34"),
                                 "Limestone "= c("10", "11", "17", "19"),
                                 "Coal" = c("9", "15", "22"))


# Prints categories data frame
categories3

# Substitutes raster values with new categories
bedrock_classified <- subs(bedrock, categories3)

# Write to new raster
writeRaster(bedrock_classified, here("output", "practical_2", "mersey_bedrock_reclass.tif"))

#Loads land cover raster using the raster and here packages
mersey_bedrock <- raster(here("output", "practical_2", "mersey_bedrock_reclass.tif"))

# Plots using ggplot
p <- ggplot() +
  layer_spatial(mersey_bedrock, aes(fill = stat(band1))) + # Adds raster layer
  theme_classic() + 
  labs(fill = "Land cover class", x = "Easting", y = "Northing") +
  scale_fill_distiller(palette = "RdYlBu", na.value = NA) + # Updated fill aesthetic
  theme(legend.position = "top")
p

mersey_rainfall <- raster(here("data", "practical_2", "mersey_rainfall.tif"))
wbt_slope(
  here("data", "practical_2","mersey_dem_fill.tif"),
  here("output", "practical_2", "mersey_dem_slope.tif"),
  units = "degrees",
)
mersey_slope<- raster(here("output", "practical_2", "mersey_dem_slope.tif"))
ggplot() +
  layer_spatial(mersey_slope)
wbt_aspect(
  here("data", "practical_2","mersey_dem_fill.tif"),
  here("output", "practical_2", "mersey_dem_aspect.tif"),

)
mersey_aspect<- raster(here("output", "practical_2", "mersey_dem_aspect.tif"))
ggplot() +
  layer_spatial(mersey_aspect,aes(fill = stat(band1))) +
  # Sets ggplot theme
  theme_classic() + coord_sf()+
  # Axis and legend labeling
  labs(fill = "aspect")

watersheds <- st_read(here("output", "practical_2", "mersey_watersheds.shp"))
colnames(watersheds)

# Replaces column name 'VALUE' with 'SEED_Point_ID'
names(watersheds)[names(watersheds) == 'VALUE'] <- 'Seed_Point_ID'
# Loads csv using read.csv
ea_data <- read.csv(here("data", "practical_2", "mersey_EA_chemistry.csv"))
# Merge based upon matching Seed_Point_IDs
watersheds_ea <- merge(watersheds, ea_data, by = "Seed_Point_ID")
head(watersheds_ea)
# Calculates area geometry using st_area()
watersheds_ea$area <- st_area(watersheds_ea)
# Calculates area geometry using st_area(), converting to km^2 using the units package
watersheds_ea$area <- set_units(st_area(watersheds_ea), km^2)
# Load elevation raster
mersey_dem <- raster(here("data", "practical_2", "mersey_dem_fill.tif"))
# Calculates the number of raster cells per watershed 
watersheds_ea$count <- extract(mersey_dem, watersheds_ea, fun=function(x, ...) length(x))
# Extracts raster values for each watershed, calculates mean (fun=mean), and stores in attribute table ($average_elevation)
watersheds_ea$average_elevation <- extract(mersey_dem, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_rainfall <- extract(mersey_rainfall, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_slope <- extract(mersey_slope, watersheds_ea, fun=mean, na.rm=TRUE)
watersheds_ea$average_aspect<- extract(mersey_aspect, watersheds_ea, fun=mean, na.rm=TRUE)
# Extract land cover counts (5 classes so levels = 1:5)
land_cover_classes <- extract(mersey_land_cover, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:5)))
colnames(land_cover_classes) <- c("Arable", "Heath", "Grassland", "Urban", "Wetland")
head(land_cover_classes)
# Extract soils counts (4 classes so levels = 1:4)
soils_classes <- extract(mersey_soils, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:4)))
colnames(soils_classes) <- c("Permeable","Impermeable","Gleyed","Peats")
head(soils_classes)
# Extract bedrock counts (3 classes so levels = 1:3)
bedrock_classes <- extract(mersey_bedrock, watersheds_ea, fun=function(i,...) table(factor(i, levels = 1:3)))
colnames(bedrock_classes) <- c("Sands_and_Muds","Limestone","Coal")
head(bedrock_classes)
# Combines watersheds data frame with categorical counts
watersheds_ea <- cbind(watersheds_ea, land_cover_classes, soils_classes, bedrock_classes)

# Creates vector of categorical variables
categorical_names <- c("Arable", "Heath", "Grassland", "Urban", "Wetland", "Permeable", "Impermeable", "Gleyed", "Peats", "Sands_and_Muds", "Limestone", "Coal")

# Prints vector
categorical_names
# Loops through each element of categorical_names and stores it in variable "i"
for (i in categorical_names){
  # Prints element stored in i
  print(i)
}
# Loops through each element of categorical_names and stores it in variable "i"
for (i in categorical_names){
  # Creates a new column name using the variable "i" and the string "percent", separated by an underscore.
  col <- paste(i, "percent", sep="_")
  # Prints new column name
  print(col)
}

# Loops through each element of categorical_names and stores it in variable "i"
for (i in categorical_names){
  # Creates a new column name using the variable "i" and the string "percent", separated by an underscore.
  col <- paste(i, "percent", sep="_")
  # Updates watersheds_ea with the percentage cover of each category
  watersheds_ea[col] <- as.character(as.numeric(watersheds_ea[[i]]/watersheds_ea$count*100))
}
# Drops geometry attribute from watersheds_ea
watersheds_ea <- st_drop_geometry(watersheds_ea)

# Writes data frame to comma-separated file
write.csv(x = watersheds_ea, here("output", "practical_2", "mersey_watersheds_ea.csv"), row.names=FALSE)

