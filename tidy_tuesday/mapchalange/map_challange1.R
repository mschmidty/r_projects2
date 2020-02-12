library(tidyverse)
library(raster)
library(sf)


if (!all(file.exists(c(here::here("data/Zn_tif.zip"),
                       here::here("data/Fe_tif.zip"),
                       here::here("data/U_tif.zip"),
                       here::here("data/Cu_tif.zip"))))) {
  download.file(
    url = c(
      "https://pubs.usgs.gov/sir/2017/5118/elements/Zinc/Zn_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Iron/Fe_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Uranium/U_tif.zip",
      "https://pubs.usgs.gov/sir/2017/5118/elements/Copper/Cu_tif.zip"
    ),
    destfile = c(
      here::here("data/Zn_tif.zip"),
      here::here("data/Fe_tif.zip"),
      here::here("data/U_tif.zip"),
      here::here("data/Cu_tif.zip")
    ),
    method = "libcurl"
  )
  
  unzip(zipfile = here::here("data/Zn_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/Fe_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/U_tif.zip"), exdir = here::here("data/usgs"))
  unzip(zipfile = here::here("data/Cu_tif.zip"), exdir = here::here("data/usgs"))
  
}

r<-brick("data/usgs/A_Cu.tif")

plot(r)

install.packages("rgdal")


