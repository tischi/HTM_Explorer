## HTM Explorer: help, installation and starting up ##

### More help on functionality and usage ###

Go to the [HELP FOLDER](https://github.com/tischi/HTM_Explorer/tree/master/help) and just select any of the *.md files.


### Installation of R ###

First, you need to install the free software R (you'll need a version > 3.0). It will not work with R Studio!

[R on the web](http://www.r-project.org/)

Make sure the Tcl/Tk libraries are part of the installation package; normally this is the case by default.


### Installation of R packages ###

In order to run the HTM Explorer you'll need to install a few R packages.

Start R; within the R console window type:

`install.packages("gWidgets")`

`install.packages("gWidgetstcltk")`

`install.packages("markdown")`

Note: if you are using Windows 7 it may ask you whether it should install it as "personal libraries"...just say "yes"


### Installation of the HTM Explorer ###

Download [HTM Explorer](
https://github.com/tischi/HTM_Explorer/archive/master.zip) and simply copy all the files somewhere onto your computer.


### Starting the HTM Explorer ###

Start R and type below commands replacing `/path/to/` by the location of the HTM Explorer folder:

`setwd("/path/to/HTM_Explorer-master"); source("run.R");`

Now you should see the graphical user interface popping up and you are ready to go. 

### More help on usage ###

For more help on the usage of HTM Explorer please check all the "Help" buttons and "Help" menu items within the GUI.


### Special procedure when using Mac OS > 10.9 ###

- start XQuartz (you may need to install [XQuartz](https://xquartz.macosforge.org/landing/) first).

- type `R` in a XQuartz terminal window (this will start R)

- now the XQuartz terminal window will have become the R console window and you can proceed as explained above in __Starting the HTM tool__ 