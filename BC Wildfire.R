# Load necessary libraries
library(rvest)        # For web scraping
library(sf)           # For handling spatial data
library(raster)       # For raster data manipulation
library(ggplot2)      # For plotting
library(dplyr)        # For data manipulation
library(ggspatial)    # For map overlays
library(terra)        # Alternative for raster manipulation if needed
library(geodata)      # Pull the temperature and elevation data 
library(osmdata)      # To get BC boundary 
library(ggplot2)      # For plotting maps 
library(dplyr)        # For data manipulation 
library(viridis)      # For importing color for raster and maps
library(leaflet)      # For plotting interactive maps 
library(htmltools)    # For customizing UI of interactive maps
library(readxl)       # For importing excel file 
library(magick)       # To view plots saved as images due to memory issue 
library(patchwork)
library(knitr)

# Step 1: Scrape Wildfire Data (2012-2017)
wildfire_html <- read_html("https://www2.gov.bc.ca/gov/content/safety/wildfire-status/about-bcws/wildfire-statistics")
wildfire_html


# Step 2 : Scrape 2012-2017 wildfires data 
tables <- wildfire_html |>
  html_nodes("table") |>
  html_nodes(xpath = "//table[caption/p/strong[contains(text(), 'Locations of all Wildfires in B.C., 2012-2017')]]") |>
  html_table(fill = TRUE)

# Print out the tables to identify the correct one
print(tables)

# Select the desired table by index after inspecting
# Replace 1 with the correct index if the first table is not the one you need
wildfire_data <- as_tibble(tables[[1]])

# Display the first few rows
head(wildfire_data)


# Convert longitudes and latitudes in proper format 
convert_to_decimal <- function(coord) {
  parts <- strsplit(coord, " ")[[1]]
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  decimal_degrees <- degrees + (minutes / 60)
  return(decimal_degrees)
}

# Apply the function to latitude and longitude columns
wildfire_data$Latitude <- sapply(wildfire_data$Latitude, convert_to_decimal)
wildfire_data$Longitude <- sapply(wildfire_data$Longitude, convert_to_decimal)


#Rename the columns 
wildfire_data <- wildfire_data |>
  rename(
    Size_ha =`Size (ha)`,
    Fire_Centre = `Fire Centre`
  )
View(wildfire_data)

# Convert Longitudes into negative format since BC has negative longitutde 
wildfire_data <- wildfire_data |>
  mutate(Longitude = -abs(Longitude))  
head(wildfire_data)


# Pull temperature and elevation data 
can <- worldclim_country("Canada", var="tavg", res=10, path=tempdir())

can_excl_winter <- can[[c(3:11)]]  # March to November

# Calculate the average temperature across these months
#can_avg_non_winter <- calc(can_excl_winter, mean)


# Extract province boundaries
province_boundary <- gadm("Canada", level = 1, path = tempdir())

#Extract only BC boundary
bc_boundary <- province_boundary[province_boundary$NAME_1 == "British Columbia", ]


bc_temp <- crop(can_excl_winter, bc_boundary)  # Crop to the extent of BC
bc_temp <- mask(bc_temp, bc_boundary)# Mask using the exact BC boundary


# Calculate the average BC temperature
avg_temp_bc <- mean(bc_temp)
plot(avg_temp_bc, main = "Average BC Temperature")

#avg_temp_bc <- mean(bc_temp)
#plot(avg_temp_bc, main = "Average BC Temperature")



# Question 4 focal and zonal operation on temperature data 
wildfire_points <- st_as_sf(wildfire_data, coords = c("Longitude", "Latitude"), crs = st_crs(avg_temp_bc))

# Focal operation 
focal_temp <- focal(avg_temp_bc, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)

# Plot the original and smoothed (focal) temperature rasters for comparison
par(mfrow = c(1, 2))
plot(avg_temp_bc, main = "Original Temperature Raster")
plot(focal_temp, main = "Smoothed Temperature (Focal) Raster")
par(mfrow = c(1, 1))


# Zonal operation
# Define a buffer around each fire location 
wildfire_buffer <- st_buffer(wildfire_points, dist = 8000)

# Extract average temperature within each fire buffer zone
buffered_temp <- extract(focal_temp, wildfire_buffer, fun = mean, na.rm = TRUE)

# Add the extracted temperature values to the wildfire data
wildfire_points$Avg_Temperature <- buffered_temp

wildfire_points <- wildfire_points %>%
  mutate(
    Avg_Temperature = Avg_Temperature$focal_mean  # Extract 'mean' from the Temperature data frame
  )
library(ggplot2)

# Create the histogram for temperature with labels and improved aesthetics
ggplot(wildfire_points, aes(x = Avg_Temperature)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black", alpha = 0.7) +  # Adjust transparency
  geom_text(
    aes(label = ..count..), 
    stat = "bin", 
    binwidth = 1, 
    vjust = -0.5,  # Position labels above bars
    size = 5,  # Font size for labels
    color = "black"  # Color of the labels
  ) +
  labs(title = "Impact of Temperature on Wildfire Occurrence",
       x = "Average Temperature (°C)",
       y = "Number of Fires") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center and make title bold and large
    axis.title = element_text(size = 14),  # Larger axis titles
    axis.text = element_text(size = 12),  # Larger axis text
    panel.grid = element_blank(),  # Remove gridlines
    plot.background = element_rect(fill = "white", color = "white")  # Clean white background
  )




# Question 4 focal and zonal operations on elevation raster 

# Pull the elevation data for Canada
can_elev <- elevation_30s(country="CAN", path=tempdir(),mask=TRUE)


# Extract province boundaries
province_boundary2 <- gadm("Canada", level = 1, path = tempdir())

#Extract only BC boundary
bc_boundary2 <- province_boundary2[province_boundary2$NAME_1 == "British Columbia", ]

bc_elev <- crop(can_elev, bc_boundary2)  # Crop to the extent of BC
bc_elev <- mask(bc_elev, bc_boundary2)# Mask using the exact BC boundary

#Plot the elevation raster
plot(bc_elev, main = " BC Elevation Raster")


# Focal operation on elevation raster
focal_elev <- focal(bc_elev, w = matrix(1, 3, 3), fun = mean, na.rm = TRUE)

# Plot the original and smoothed (focal) elevation rasters for comparison
par(mfrow = c(1, 2))
plot(bc_elev, main = "Original Elevation Raster")
plot(focal_elev, main = "Smoothed Elevation (Focal) Raster")
par(mfrow = c(1, 1))

# Define a buffer around each fire location 
wildfire_buffer_elev <- st_buffer(wildfire_points, dist = 5000)

# Extract average elevation within each fire buffer zone
buffered_elev <- extract(focal_elev, wildfire_buffer_elev, fun = mean, na.rm = TRUE)

# Add the extracted elevation values to the wildfire data
wildfire_points$Avg_Elevation <- buffered_elev

wildfire_points <- wildfire_points %>%
  mutate(
    Avg_Elevation = Avg_Elevation$focal_mean  # Extract 'mean' from the Elevation data frame
  )



# Create the histogram with labels and improved aesthetics
ggplot(wildfire_points, aes(x = Avg_Elevation)) +
  geom_histogram(binwidth = 20, fill = "green", color = "black", alpha = 0.7) +  # Adjust transparency for a nicer look
  geom_text(
    aes(label = ..count..), 
    stat = "bin", 
    binwidth = 20, 
    vjust = -0.5,  # Position the labels slightly above the bars
    size = 3,  # Font size for labels
    color = "black"  # Color of the labels
  ) +
  labs(title = "Impact of Elevation on wildfire occurence",
       x = "Average Elevation (m)",
       y = "Number of Fires") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),  # Center title, make it bold and larger
    axis.title = element_text(size = 14),  # Larger axis titles
    axis.text = element_text(size = 12),  # Larger axis text
    panel.grid = element_blank(),  # Remove grid lines for a cleaner look
    plot.background = element_rect(fill = "white", color = "white")  # Clean white background
  )










## Plotting wildfires 2012,2013 & 2014 overtop temperature raster


# Convert to sf and set CRS
wildfires_sf <- wildfire_data |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Define color palette for the temperature raster
temp_palette <- viridis(10)

# Define a function to add wildfire data for each year with different colors and a fixed size, without a border
add_wildfire_layer <- function(map, year, fill_color) {
  wildfire_year_sf <- wildfires_sf |>
    filter(Year == year)
  
  # Create a label with multiple fields for hover-over information
  wildfire_year_sf <- wildfire_year_sf %>%
    mutate(label_info = paste0(
      "Fire Number: ", `Fire Number`, "<br>",
      "Fire Centre: ", Fire_Centre, "<br>",
      "Location: ", Geographic, "<br>",
      "Date Discovered: ", `Discovery Date`, "<br>",
      "Fire Size: ", Size_ha
    ))
  
  # Add circle markers with different colors based on the year and a fixed radius without any border
  map <- map |>
    addCircleMarkers(data = wildfire_year_sf,
                     fillColor = fill_color,
                     fillOpacity = 1, radius = 3.5,  # Fixed radius for uniform marker size
                     stroke = FALSE,  # Removes the border
                     group = as.character(year),
                     label = ~lapply(label_info, HTML))  # Apply HTML formatting for the label
  
  map
}

# Initialize the leaflet map with the temperature raster as a separate layer
map <- leaflet() |>
  addTiles() |>
  addRasterImage(avg_temp_bc, colors = temp_palette, opacity = 1, group = "Temperature Raster") |>
  addScaleBar(position = "bottomleft")

# Specify the years and colors for 2012, 2013, and 2014 only
years <- c(2012, 2013, 2014)
colors <- c("darkred", "orange", "black")

# Add wildfire data for each specified year
for (i in seq_along(years)) {
  map <- add_wildfire_layer(map, years[i], colors[i])
}

# Add layer control to toggle between temperature raster and wildfire data for each year
map <- map |>
  addLayersControl(
    overlayGroups = c("Temperature Raster", as.character(years)),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  )
# Add a custom legend for temperature raster
map <- map |>
  addLegend("bottomright", pal = colorNumeric(palette = temp_palette,
                                              domain = c(min(avg_temp_bc[], na.rm = TRUE), max(avg_temp_bc[], na.rm = TRUE))),
            values = seq(min(avg_temp_bc[], na.rm = TRUE), max(avg_temp_bc[], na.rm = TRUE), length.out = 50),
            title = "Temperature (°C)")


# Display the map
map



## Plotting wildfires 2015,2016 & 2017 overtop temperature raster


# Convert to sf and set CRS
wildfires_sf <- wildfire_data |>
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)


# Define color palette for the temperature raster
temp_palette <- viridis(10)

# Define a function to add wildfire data for each year with different colors and a fixed size, without a border
add_wildfire_layer <- function(map, year, fill_color) {
  wildfire_year_sf <- wildfires_sf |>
    filter(Year == year)
  
  # Create a label with multiple fields for hover-over information
  wildfire_year_sf <- wildfire_year_sf %>%
    mutate(label_info = paste0(
      "Fire Number: ", `Fire Number`, "<br>",
      "Fire Centre: ", Fire_Centre, "<br>",
      "Location: ", Geographic, "<br>",
      "Date Discovered: ", `Discovery Date`, "<br>",
      "Fire Size: ", Size_ha
    ))
  
  # Add circle markers with different colors based on the year and a fixed radius without any border
  map <- map |>
    addCircleMarkers(data = wildfire_year_sf,
                     fillColor = fill_color,
                     fillOpacity = 1, radius = 3.5,  # Fixed radius for uniform marker size
                     stroke = FALSE,  # Removes the border
                     group = as.character(year),
                     label = ~lapply(label_info, HTML))  # Apply HTML formatting for the label
  
  map
}
# Initialize the leaflet map with the temperature raster as a separate layer
map <- leaflet() |>
  addTiles() |>
  addRasterImage(avg_temp_bc, colors = temp_palette, opacity = 1, group = "Temperature Raster") |>
  addScaleBar(position = "bottomleft")

# Specify the years and colors for 2012, 2013, and 2014 only
years <- c(2015, 2016, 2017)
colors <- c("darkred", "orange", "black")

# Add wildfire data for each specified year
for (i in seq_along(years)) {
  map <- add_wildfire_layer(map, years[i], colors[i])
}

# Add layer control to toggle between temperature raster and wildfire data for each year
map <- map |>
  addLayersControl(
    overlayGroups = c("Temperature Raster", as.character(years)),
    position = "bottomright",
    options = layersControlOptions(collapsed = FALSE)
  )
# Add a custom legend for temperature raster
map <- map |>
  addLegend("bottomright", pal = colorNumeric(palette = temp_palette,domain = c(min(avg_temp_bc[], na.rm = TRUE), max(avg_temp_bc[], na.rm = TRUE))),
            values = seq(min(avg_temp_bc[], na.rm = TRUE), max(avg_temp_bc[], na.rm = TRUE), length.out = 50),
            title = "Temperature (°C)")


# Display the map
map














# Current Fire and Historical Fire distribution 
# Step 1: Scrape Wildfire Data (2012-2017)
wildfire_new_htm <- read_html("https://www2.gov.bc.ca/gov/content/safety/wildfire-status/about-bcws/wildfire-statistics")
wildfire_new_htm


# Step 2 : Scrape 2012-2017 wildfires data 
tables <- wildfire_new_htm |>
  html_nodes("table") |>
  html_nodes(xpath = "//table[caption/p/strong[contains(text(), 'Locations of all Wildfires in B.C., 2012-2017')]]") |>
  html_table(fill = TRUE)

# Print out the tables to identify the correct one
print(tables)

# Select the desired table by index after inspecting
# Replace 1 with the correct index if the first table is not the one you need
wildfire_new_data <- as_tibble(tables[[1]])

# Display the first few rows
head(wildfire_new_data)


# Convert longitudes and latitudes in proper format 
convert_to_decimal <- function(coord) {
  parts <- strsplit(coord, " ")[[1]]
  degrees <- as.numeric(parts[1])
  minutes <- as.numeric(parts[2])
  decimal_degrees <- degrees + (minutes / 60)
  return(decimal_degrees)
}

# Apply the function to latitude and longitude columns
wildfire_new_data$Latitude <- sapply(wildfire_new_data$Latitude, convert_to_decimal)
wildfire_new_data$Longitude <- sapply(wildfire_new_data$Longitude, convert_to_decimal)


#Rename the columns 
wildfire_new_data <- wildfire_new_data |>
  rename(
    Size_ha =`Size (ha)`,
    Fire_Number = `Fire Number`,
    Fire_Centre = `Fire Centre`
  )
View(wildfire_new_data)

# Convert Longitudes into negative format since BC has negative longitutde 
wildfire_new_data <- wildfire_new_data |>
  mutate(Longitude = -abs(Longitude))  
head(wildfire_new_data)




# Read the current data )
wildfire_current <- read_excel("C:/Users/avich/Downloads/R/Current Wildfire.xlsx")
View(wildfire_current)

# Drop columns that are unnecessary 
clean_widlfire <- wildfire_current|>
  select(-RSPNS_TYPC,-IGN_DATE,-FR_T_DTE,-FIRESTATUS,-FIRE_CAUSE,-ZONE,-FIRE_ID,-FIRE_TYPE,-INCDNT_NM,
         -FIRE_URL,-FR_F_NT_ND,-WS_FR_F_ND,-FCODE	,-SHAPE,-OBJECTID,-X_COORDINATE,-Y_COORDINATE)

View(clean_widlfire)

#Renaming the columns in wildfire_new 
clean_widlfire <- clean_widlfire |>
  rename(
    Fire_Number = `FIRE_NO`,
    Year = `FIRE_YEAR`,
    Fire_Centre = `FIRE_CENTR`,
    Geographic = `GEO_DESC`,
    Latitude = `LATITUDE`,
    Longitude = `LONGITUDE`,
    Size_ha = `SIZE_HA`)

head(clean_widlfire)

clean_widlfire$Fire_Centre <- as.character(clean_widlfire$Fire_Centre)


# Combine both the datasets 
combined_data <- bind_rows(wildfire_new_data, clean_widlfire)
View(combined_data)


ggplot(combined_data, aes(x = as.factor(Year))) +
  geom_bar(alpha = 0.6) +
  labs(title = "Number of Wildfires per Year (2012-2017, 2024)",
       x = "Year", y = "Frequency of Wildfires") +
  theme_minimal()



# Create a new column to categorize historical data and 2024
combined_data$Period <- ifelse(combined_data$Year %in% 2012:2017, "Historical (2012-2017)", "2024")

# Plot with distinct categories
ggplot(combined_data, aes(x = as.factor(Year), fill = Period)) +
  geom_bar(alpha = 0.6) +
  labs(title = "Number of Wildfires per Year (Historical vs. 2024)",
       x = "Year", y = "Frequency of Wildfires") +
  scale_fill_manual(values = c("Historical (2012-2017)" = "skyblue", "2024" = "orange")) +
  theme_minimal()



# Extract province boundaries
province_boundary <- gadm("Canada", level = 1, path = tempdir())

# Extract only BC boundary
bc_boundary <- province_boundary[province_boundary$NAME_1 == "British Columbia", ]

# Ensure bc_boundary is an sf object
bc_boundary_sf <- st_as_sf(bc_boundary)

# Prepare the data: Separate 2012-2017 and 2024
combined_data$Period <- ifelse(combined_data$Year %in% 2012:2017, "Historical (2012-2017)", "2024")

# Convert the wildfire data to an sf object
combined_data_sf <- combined_data %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)  # WGS84 CRS

# Filter data for historical and 2024 wildfires
historical_data <- combined_data_sf %>% filter(Period == "Historical (2012-2017)")
current_data <- combined_data_sf %>% filter(Period == "2024")

# Create historical plot
historical_plot <- ggplot() +
  geom_sf(data = bc_boundary_sf, fill = "gray90", color = "black") +
  geom_sf(data = historical_data, aes(color = Period), size = 1, alpha = 0.7) +
  scale_color_manual(values = c("Historical (2012-2017)" = "blue")) +
  labs(title = "Historical Wildfire Locations (2012-2017)",
       color = "Fire Period") +
  theme_minimal() +
  theme(legend.position = "none")

# Create current plot
current_plot <- ggplot() +
  geom_sf(data = bc_boundary_sf, fill = "gray90", color = "black") +
  geom_sf(data = current_data, aes(color = Period), size = 1, alpha = 0.7) +
  scale_color_manual(values = c("2024" = "red")) +
  labs(title = "2024 Wildfire Locations",
       color = "Fire Period") +
  theme_minimal() +
  theme(legend.position = "none")

# Combine the two plots side by side
ggsave("combined_wildfire_plot.png", plot = combined_plot, width = 12, height = 6)
img <- image_read("combined_wildfire_plot.png")
print(img) 







































































































