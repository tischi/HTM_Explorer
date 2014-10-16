
## specify libraries
libs = list("gWidgets","gWidgetstcltk","markdown")

## install libraries
# sapply(libs, biocLite) 
# install.packages("gWidgets")
# install.packages("gWidgetstcltk")
# install.packages("markdown")


## load libraries
print("Loading libraries...")
sapply(libs, function(x) do.call("library", list(x))) 

options(warn = 2)

#path = "C:/Users/Christian Tischer/Dropbox/R/HTM/";setwd(path);source("install.R"); 
#path = "/Users/tischi/Dropbox/R/HTM";setwd(path);source("install.R"); 

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
