### Main script for visualizations ###

# this script contains code for 2 leaflet visualizations #

# date: June 2019

# load libraries ----
library(leaflet)
library(raster)
library(rgdal)

# load scripts ----
source('scripts/leaflet_total.R')
source('scripts/leaflet_categories.R')
source("scripts/getpoints.R")

# visualize total amounts of litter as result of kriging ----
visualization_total <- visualize_total()
visualization_total # html object that can be hosted on server

 # visualize amounts of litter per category ----
visualization_categories <- visualize_categories()
visualization_categories # html object that can be hosted on server
