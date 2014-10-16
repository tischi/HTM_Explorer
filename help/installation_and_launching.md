## HTM tool: installation and starting ##


### Installation of R ###

you need an R version > 3.0

do not use R Studio!

[R on the web](http://www.r-project.org/)

make sure the Tcl/Tk libraries are part of the installation package; normally this is the case by default.


### Installation of R packages ###

start R
within the R terminal window type:

`install.packages("gWidgets")`

`install.packages("gWidgetstcltk")`

`install.packages("markdown")`


note: if you are using Windows 7 it may ask you whether it should install it as "personal libraries"...just say "yes"


### Installation of the HTM tool ###

the HTM Tool itself needs no installation, you just need the HTM folder and its contents somewhere on your computer; these files you get from Tischi (tischer@embl.de)


### Starting the HTM tool ###

Start R and type below commands replacing `/path/to/` by the location of the HTM folder:

`setwd("/path/to/HTM"); source("install.R");`

Now you see the graphical user interface popping up and yo are ready to go. 

#### Special procedure when using Mac OS > 10.9 ####

- start XQuartz (you may need to install [XQuartz](https://xquartz.macosforge.org/landing/) first).

- type `R` in a XQuartz terminal window (this will start R)

- now the XQuartz terminal window will have become the R console window and you can proceed as explained above in __Starting the HTM tool__ 