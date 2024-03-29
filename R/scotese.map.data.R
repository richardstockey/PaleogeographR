###################################################
# scotese.map.data.R
# Rich Stockey 20240313
###################################################
# full comments to follow...

scotese.map.data <- function(age # Age in millions of years ago
                       ){

  # other projection options include:
  # - 6933 - Lambert Cylindrical Equal Area (need only numbers no text and no quotes) [this is equal area rectangle]
  # still need to come up with a good option for a sphere...
  # dims is dimensions of netcdf being read in - this is set to 3d by default

  #load("~/PaleogeographR/Scotese_1deg_list.Rdata")

  closest.age.step <- which.min(abs(age - ages))

  closest.age <- ages[closest.age.step]

  df <- df_sum[[closest.age.step]]

  return(df)
}


