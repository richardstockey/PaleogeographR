
library(RNetCDF)
library(dplyr)
library(sf)
library(sp)
library(ggspatial)
library(reshape2)
library(ggplot2)


list <- list.files("~/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2")

age_text <- sub(".*_", "", list)
age <- sub("Ma.nc", "", age_text)
age_num <- as.numeric(age)

# list <- list[1:109]
# age_num <- age_num[1:109]

df_sum <- list()
ages <- c()

for(file.no in 1:length(list)){
  print(paste0("On file number ", file.no))
  map <- list[file.no]
  nc <- open.nc(paste0("~/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc/Scotese_Wright_2018_Maps_1-88_1degX1deg_PaleoDEMS_nc_v2/", map))

  age_ma <- age_num[file.no]

  # Extract general variables
  lat <- var.get.nc(nc, "lat") # units: degrees
  lat.edges <- c(lat - mean(diff(lat)/2), lat[length(lat)] + mean(diff(lat)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  lon <- var.get.nc(nc, "lon") # units: degrees
  lon.edges <- c(lon - mean(diff(lon)/2), lon[length(lon)] + mean(diff(lon)/2)) # should work for any evenly spaced grid (although note we have values outside reality! removed later...)
  z <- var.get.nc(nc, "z") # units: metres

  df_sum[[file.no]] <- z
  ages <- c(ages, age_ma)

}

save(file = "~/PaleogeographR/Scotese_1deg_list.rda", df_sum, ages)



