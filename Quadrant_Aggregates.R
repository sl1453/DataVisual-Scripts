#library(readxl)
#library(tidyverse)
#library(ggplot2)
##Creating spatstat objects
#library(spatstat)

#install.packages("sp")
#options("sp_evolution_status"=2)
#library(sp)
#sp::get_evolution_status()
#######
library(readxl)
library(tidyverse)
library(ggplot2)
library(sf)
library(polyCub)
library(spatstat)
setwd("~/Documents/R workshop 2017/project pattern/analysis")
#######################################################################
#### Basic Individual Plots: ##########################################
time_points <- c("12h", "24h", "36h", "48h", "60h")
replicates <- c("BioRep1_1_25_21", "BioRep2_2_27_21", "BioRep3_4_6_21")

# Replicate type specifications for each replicate
replicate_types <- list(
  BioRep1_1_25_21 = list(Bio = 3:4),
  BioRep2_2_27_21 = list(Bio = 3:4, Tech = 5:6),
  BioRep3_4_6_21 = list(Bio = 3:4, Tech = c(5, 6, 8, 9, 10, 11))
)

# Sheet numbers related to ceiling and floor
sheet_type <- c("3" = "Ceiling", "5" = "Ceiling_1", "8" = "Ceiling_2", "10" = "Ceiling_3",
                "4" = "Floor", "6" = "Floor_1", "9" = "Floor_2", "11" = "Floor_3")

# Loop through replicates
for (replicate in replicates) {
  
  # Loop through time points
  for (time_point in time_points) {
    
    file_name <- paste0("data 7/", time_point, "_", replicate, ".xlsx")
    
    # Loop through replicate types and respective sheets
    for (rep_type in names(replicate_types[[replicate]])) {
      for (sheet_num in replicate_types[[replicate]][[rep_type]]) {
        
        # Identify the type of sheet (Ceiling/Floor)
        sheet_name <- sheet_type[as.character(sheet_num)]
        
        giardia_polygon <- read_excel(file_name, sheet = 7)
        giardia_polygon_df <- data.frame(
          lon = c(giardia_polygon$x_coordinate),
          lat = c(giardia_polygon$y_coordinate)
        )
        
        polygonarea <- giardia_polygon_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = NA) %>% 
                                  #use crs=4326 (WGS 84) for Geo-data
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        
        #Coordinate units
        coordinate.units <- c("um", "ums")
        #Window
        spg <- as(polygonarea, 'Spatial')
        win <- as.owin.SpatialPolygons(spg)
        
        giardia_data <- read_excel(file_name, sheet = sheet_num)
        giardia_data_df <- data.frame(
          lon = c(giardia_data$x_coordinate),
          lat = c(giardia_data$y_coordinate)
        )
        
        giardia_points <- giardia_data_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326)
        
        ppp.giardia_points <- ppp(x = st_coordinates(giardia_points)[,1],
                                      y = st_coordinates(giardia_points)[,2],
                                      window = win)
        unitname(ppp.giardia_points) <- coordinate.units
        
        plot(ppp.giardia_points, main = paste(replicate, time_point, rep_type, sheet_name, sep = "_"))
        
      }
    }
  }
}
############# Group Plots: Print Name Method ################################################
time_points <- c("12h", "24h", "36h", "48h", "60h")
replicates <- c("BioRep1_1_25_21", "BioRep2_2_27_21", "BioRep3_4_6_21")

# Replicate type specifications for each replicate
replicate_types <- list(
  BioRep1_1_25_21 = list(Bio = 3:4),
  BioRep2_2_27_21 = list(Bio = 3:4, Tech = 5:6),
  BioRep3_4_6_21 = list(Bio = 3:4, Tech = c(5, 6, 8, 9, 10, 11))
)

# Sheet numbers related to ceiling and floor
sheet_location <- c("3" = "Ceiling", "5" = "Ceiling_1", "8" = "Ceiling_2", "10" = "Ceiling_3",
                    "4" = "Floor", "6" = "Floor_1", "9" = "Floor_2", "11" = "Floor_3")

# Create an empty list to store ppp.giardia_points objects
ppp_list <- list()

# Loop through replicates
for (replicate in replicates) {
  
  # Loop through replicate types for the current replicate
  for (rep_type in names(replicate_types[[replicate]])) {
    
    # Loop through each sheet_location that belongs to the current replicate type
    for (sheet_num in replicate_types[[replicate]][[rep_type]]) {
      location <- sheet_location[as.character(sheet_num)]
      
      # Loop through time points for the current sheet_location
      for (time_point in time_points) {
        
        file_name <- paste0("data 7/", time_point, "_", replicate, ".xlsx")
        
        giardia_polygon <- read_excel(file_name, sheet = 7)
        giardia_polygon_df <- data.frame(
          lon = c(giardia_polygon$x_coordinate),
          lat = c(giardia_polygon$y_coordinate)
        )
        
        polygonarea <- giardia_polygon_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        
        # Window
        spg <- as(polygonarea, 'Spatial')
        win <- as.owin.SpatialPolygons(spg)
        
        giardia_data <- read_excel(file_name, sheet = sheet_num)
        giardia_data_df <- data.frame(
          lon = c(giardia_data$x_coordinate),
          lat = c(giardia_data$y_coordinate)
        )
        
        giardia_points <- giardia_data_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = 4326)
        
        ppp.giardia_points <- ppp(x = st_coordinates(giardia_points)[,1],
                                  y = st_coordinates(giardia_points)[,2],
                                  window = win)
        
        # Assign ppp.giardia_points to a name
        ppp_name <- paste(replicate, rep_type, location, time_point, sep = "_")
        ppp_list[[ppp_name]] <- ppp.giardia_points
      }
    }
  }
}
# Print out the names of the list
names(ppp_list)
# Save all the plots into a PDF file 
pdf(file = "All_Plots.pdf", 
    height = 4, width = 20)

chunks <- ceiling(length(ppp_list) / 10)  # How many chunks of 10 we have

for (chunk in 1:chunks) {
  par(mfrow=c(2,5), mar=c(1,1,1,1))
  
  start <- (chunk - 1) * 10 + 1
  end <- min(chunk * 10, length(ppp_list))
  
  for (i in start:end) {
    plot(ppp_list[[i]], main = names(ppp_list)[i])
  }
}

dev.off()
############### Group Plots: W/quadrant count #########################    
time_points <- c("12h", "24h", "36h", "48h", "60h")
replicates <- c("BioRep1_1_25_21", "BioRep2_2_27_21", "BioRep3_4_6_21")

# Replicate type specifications for each replicate
replicate_types <- list(
  BioRep1_1_25_21 = list(Bio = 3:4),
  BioRep2_2_27_21 = list(Bio = 3:4, Tech = 5:6),
  BioRep3_4_6_21 = list(Bio = 3:4, Tech = c(5, 6, 8, 9, 10, 11))
)

# Sheet numbers related to ceiling and floor
sheet_location <- c("3" = "Ceiling", "5" = "Ceiling_1", "8" = "Ceiling_2", "10" = "Ceiling_3",
                    "4" = "Floor", "6" = "Floor_1", "9" = "Floor_2", "11" = "Floor_3")

# Create an empty list to store ppp.giardia_points objects
ppp_list <- list()
qc_list <- list()
# Loop through replicates
for (replicate in replicates) {
  
  # Loop through replicate types for the current replicate
  for (rep_type in names(replicate_types[[replicate]])) {
    
    # Loop through each sheet_location that belongs to the current replicate type
    for (sheet_num in replicate_types[[replicate]][[rep_type]]) {
      location <- sheet_location[as.character(sheet_num)]
      
      # Loop through time points for the current sheet_location
      for (time_point in time_points) {
        
        file_name <- paste0("data 7/", time_point, "_", replicate, ".xlsx")
        
        giardia_polygon <- read_excel(file_name, sheet = 7)
        giardia_polygon_df <- data.frame(
          lon = c(giardia_polygon$x_coordinate),
          lat = c(giardia_polygon$y_coordinate)
        )
        
        polygonarea <- giardia_polygon_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = NA) %>%
          summarise(geometry = st_combine(geometry)) %>%
          st_cast("POLYGON")
        
        # Window
        spg <- as(polygonarea, 'Spatial')
        win <- as.owin.SpatialPolygons(spg)
        
        giardia_data <- read_excel(file_name, sheet = sheet_num)
        giardia_data_df <- data.frame(
          lon = c(giardia_data$x_coordinate),
          lat = c(giardia_data$y_coordinate)
        )
        
        giardia_points <- giardia_data_df %>%
          st_as_sf(coords = c("lon", "lat"), crs = NA)
        
        ppp.giardia_points <- ppp(x = st_coordinates(giardia_points)[,1],
                                  y = st_coordinates(giardia_points)[,2],
                                  window = win)
        
        qc.giardia_points <- quadratcount(ppp.giardia_points, nx=8, ny=4)
        
        # Assign ppp.giardia_points to a name
        ppp_name <- paste(replicate, rep_type, location, time_point, sep = "_")
        ppp_list[[ppp_name]] <- ppp.giardia_points
        
        qc_list[[ppp_name]] <- qc.giardia_points
      }
    }
  }
}

# Save all the plots into a PDF file 
pdf(file = "All_Plots.pdf", 
    height = 4, width = 20)

chunks <- ceiling(length(ppp_list) / 10)  # How many chunks of 10 we have

for (chunk in 1:chunks) {
  par(mfrow=c(2,5), mar=c(1,1,1,1))
  
  start <- (chunk - 1) * 10 + 1
  end <- min(chunk * 10, length(ppp_list))
  
  for (i in start:end) {
    plot(ppp_list[[i]], main = names(ppp_list)[i])
    plot(qc_list[[i]], add=T, textargs=list(col="red"))
  }
}

dev.off()
################################################################
