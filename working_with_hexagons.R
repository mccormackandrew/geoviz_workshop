library(mapcan)
library(sp)

# Get coordindates for ridings


# This is an excel file where I've filled in the cells to make a map of Canada
# One cell for each riding
ridings <- readxl::read_xls("canada_squares.xls", sheet = "representation_2013_provinces")


# Need to turn this "map" into x and y coordinations
binsfun <- function(data, year) {
  rows <- data
  for(i in 1:nrow(rows)) {
    rows[i, ] <- seq(1, ncol(rows), 1)
  }
  cols <- data
  for(i in 1:ncol(cols)) {
    cols[ , i] <- seq(1, nrow(cols), 1)
  }
  bins_df <- data.frame(x = gather(rows)[,2],
                        y = gather(cols)[,2],
                        vals = gather(data)[,2])
  bins_df$year <- rep(year, nrow(bins_df))
  return(na.omit(bins_df))
}

# I have a year function in here because I also do the 
# same for operation for representation orders from different years 
riding_coords <- binsfun(ridings, 2015)

names(riding_coords) <- c("x", "y", "province", "year")

# Should fix this futher up, i know
riding_coords$x <- as.numeric(riding_coords$x)
riding_coords$y <- as.numeric(riding_coords$y)

ggplot(riding_coords, aes(x, y)) +
  geom_point()
# K with a little manipulation it will be the right shape

# NOW WE ARE READY TO MAKE A HEXAGONAL BIN SHAPEFILE
# This function takes a grid of coordinate point and draws hexagons around each coordinate point

coords_to_spdf <- function(coordinate_data, hex_size, hex, xval, yval, value, bin_num) {
  # Create empty list to fill with polygon coordinate data frames
  polylist <- list()
  
  # Create polygon coordinate data frames
  for(i in 1:nrow(coordinate_data)) {
    x <- coordinate_data[ , xval][i]
    y <- coordinate_data[ , yval][i]
    hx <- hex
    hex_coords <- hx/hex_size
    xym <- cbind(c(x - hex_coords[1], x + hex_coords[1], x + hex_coords[2],
                   x + hex_coords[1], x - hex_coords[1], x - hex_coords[2]),
                 c(y + hex_coords[3], y + hex_coords[3], y,
                   y - hex_coords[3], y - hex_coords[3], y))
    xym <- data.frame(xym)
    xym$x_orig <- x
    xym$y_orig <- y
    names(xym) <- c("x", "y", "x_orig", "y_orig")
    polylist[[i]] <- xym
  }
  
  # Offset every second column so the hexagons snuggle up together
  polylist_offset <- list()
  
  for(i in 1:length(polylist)) {
    if(polylist[[i]]$x_orig[1] %% 2 == 0) {
      polylist_offset[[i]] <- polylist[[i]] %>%
        mutate(y = y + 0.5)
    } else {
      polylist_offset[[i]] <- polylist[[i]]
    }
  }
  
  # Keep only x and y
  polylist_offset <- lapply(polylist_offset, function(x) {
    x %>% dplyr::select(x, y)})
  
  # Turn dataframes into matrices (for the Polygons function)
  polylist_offset <- lapply(polylist_offset, as.matrix)
  
  # Turn each polygon data frame into a polygon object
  polylist_offset <- lapply(polylist_offset, Polygon)
  
  polygon_objects <- list()
  
  for(i in 1:length(polylist_offset)) {
    polygon_objects[[i]] <- Polygons(list(polylist_offset[[i]]), coordinate_data[ , value][i])
    
  }
  
  # Convert into an SpatialPolygons
  polygon_sp <- SpatialPolygons(polygon_objects, 1:bin_num)
  
  # Create data for
  spdf_data <- data.frame(coordinate_data, row.names = coordinate_data[, value])
  
  #Convert into an SpatialPolygonsDataFrame
  polygon_spdf <- SpatialPolygonsDataFrame(polygon_sp, spdf_data)
  
  return(polygon_spdf)
}

spdf_to_df <- function(spdf, data, value) {
  spdf %>%
    ggplot2::fortify(region = value) %>%
    mutate(id = as.numeric(id)) %>%
    left_join(data, by = c("id" = value)) %>%
    left_join(data) %>%
    mutate(value = id) %>%
    dplyr::select(-id)
}

##### IMPLEMENTING THE FUNCTION ####


riding_spdf <- coords_to_spdf(riding_coords,
               hex_size = 1.7,
               hex = c(0.5, 1, sqrt(3)/2.),
               #hex_size = 1.6,
               #hex = c(0.5, 1, sqrt(3)/2.3),
               xval = "y", yval = "x",
               bin_num = 125,
               value = "province")
plot(qc_spdf)
quebec_riding_hexagons <- spdf_to_df(qc_spdf, qc, "riding_code")
