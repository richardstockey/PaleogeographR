
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

df_sum <- c()

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


  # generate dataframe
  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
    rep(lat, times = 1, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
    as.data.frame(melt(z)$value),
    age_ma))

  names(df) <- c("lon.mid",
                 "lon.min",
                 "lon.max",
                 "lat.mid",
                 "lat.min",
                 "lat.max",
                 "z",
                 "age"
  )

  df <- df %>%
    filter(lon.max < 180,
           lon.min > -180,
           lat.max < 90,
           lat.min > -90
    )

  df_sum <- rbind(df_sum, df)
}

save(file = "~/PaleogeographR/Scotese_1deg.Rdata", df_sum)



