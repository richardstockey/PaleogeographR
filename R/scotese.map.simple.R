###################################################
# scotese.map.R
# Rich Stockey 20231030
# designed to make maps from imported .nc files (from https://zenodo.org/records/5460860)
###################################################
# full comments to follow...

scotese.map.simple <- function(age # Age in millions of years ago
                       # min.value = -6000,
                       # max.value = 6000,
                       # intervals = 2000,
                       # continents.outlined,
                       # scale.label
                       ){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default

  #load("~/PaleogeographR/Scotese_1deg_list.Rdata")

  closest.age.step <- which.min(abs(age - ages))

  closest.age <- ages[closest.age.step]

  df <- df_sum[[closest.age.step]]

  # image(x = -180:180, y = -90:90, z = df, col = rev(paletteer_c("grDevices::Earth", 30)), xlab = "Longitude (°)", ylab = "Latitude (°)")
  # box()

  filled.contour(x = -180:180, y = -90:90, z = df, color.palette = function(n) rev(paletteer_c("grDevices::Earth", 11)), xlab = "Longitude (°)", ylab = "Latitude (°)", nlevels = 14)

}


