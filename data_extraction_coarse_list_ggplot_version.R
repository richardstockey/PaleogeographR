
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

df_sum_gg <- list()
ages_gg <- c()

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


  df <- as.data.frame(cbind(
    rep(lon, times = length(lat), each = 1),
    rep(lon.edges[1:(length(lon.edges)-1)], times = length(lat), each = 1),
    rep(lon.edges[2:(length(lon.edges))], times = length(lat), each = 1),
    rep(lat, times = 1, each = length(lon)),
    rep(lat.edges[1:(length(lat.edges)-1)], times = 1, each = length(lon)),
    rep(lat.edges[2:(length(lat.edges))], times = 1, each = length(lon)),
    #as.data.frame(melt(var.arr[,, depth.level, time.step]))$value))
    as.data.frame(melt(z))$value))

  names(df) <- c("lon.mid",
                 "lon.min",
                 "lon.max",
                 "lat.mid",
                 "lat.min",
                 "lat.max",
                 "depth"
  )

  df_sum_gg[[file.no]] <- df
  ages_gg <- c(ages_gg, age_ma)

}

save(file = "~/PaleogeographR/Scotese_1deg_list_ggplot2.rda", df_sum_gg)

save(file = "~/PaleogeographR/Scotese_1deg_ages_ggplot2.rda", ages_gg)

usethis::use_data(df_sum_gg)
usethis::use_data(ages_gg)
