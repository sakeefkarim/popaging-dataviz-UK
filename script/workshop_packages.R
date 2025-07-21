# ------------------------------------------------------------------------------

# Pre-Workshop Session I
# PopAging DataViz 
# Sakeef M. Karim
# Script File
# CC-BY-SA 4.0

# LIST OF PACKAGES (AS OF LATE-JULY, 2025) --------------------------

# Suite of Packages for Day 1

day1 <- c("ggdist", "ggtext", "ggridges", "ggrepel", 
          "lattice", "skimr", "summarytools", "tinyplot",
          "ggraph", "lemon", "colorspace",
          "gglgbtq", "paletteer", "see", "hrbrthemes",
          "ggthemes", "LexisPlotR", "demography", "forecast",
          "gapminder",  "palmerpenguins", "tidyverse", "systemfonts")

# Suite of Packages for Day 2

day2 <- c("scales", "patchwork", "gganimate", "gifski", "nomisr",
          "plotly", "htmltools",  "leafpop", "leaflet", 
          "mapview", "usmapdata", "usmap", "mapcan", "cancensus",
          "tidycensus",  "geojsonsf", "tidygeocoder",  
          "rnaturalearthdata", "rnaturalearth", "ggspatial", 
          "terra", "sp", "sf", "gt", "colorspace")

# Suite of Packages for Day 3

day3 <- c("estimatr", "ggeffects", "marginaleffects", 
          "modelsummary", "effects", "gtsummary", "gt", 
          "ggthemes", "panelr", "remotes", "GGally")


# LOAD PACKAGES -----------------------------------------------------------

do.call(pacman::p_load, as.list(c(day1, day2, day3)))
