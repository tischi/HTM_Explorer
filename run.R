# path = "\\\\almf\\almf\\software\\scripts\\R\\HTM_Explorer";setwd(path);source("run.R");


if (!require("gWidgets")) {
  install.packages("gWidgets")
  library(gWidgets)
}
if (!require("gWidgetstcltk")) {
  install.packages("gWidgetstcltk")
  library(gWidgetstcltk)
}
if (!require("markdown")) {
  install.packages("markdown")
  library(markdown)
}
if (!require("plyr")) {
  install.packages("plyr")
  library(plyr)
}
if (!require("RColorBrewer")) {
  install.packages("RColorBrewer")
  library(RColorBrewer)
}
if (!require("raster")) {
  install.packages("raster")
  library(raster)
}
#if (!require("plotly")) {
#  install.packages("plotly")
#  library(plotly)
#}
#if (!require("shiny")) {
#  install.packages("shiny")
#  library(plotly)
#}


#options(warn = 2)


## source code
#setwd(path);source("install.R"); 

source("classes.R")

print("Checking htm object...")
if(exists("htm")) {
  print("  Found htm object; left it as it was; only restarting the GUI.")
} else {
  print("  No htm object found; initialising...")
  htm <<- htmMake()
}

htmPath = getwd()

## create the package:
# package.skeleton(name="htmTool",path=path, code_files=c("classes.R","gui.R","plots.R","functions.R","install.R"))

guiToolkit("tcltk")

source("plots.R")
source("functions.R")
source("gui.R")

## make package
## - you can simply delete all files in the man directory to get through "check"
## - check has options to ignore certain issues

# R CMD build
# R CMD check  
# R CMD INSTALL


## only "export" functions that you want the user to see; 
## also only for those you need to write documenation


## Roxygen
