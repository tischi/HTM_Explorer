# HTM Explorer
### HTM Explorer: an R-based software for inspection and analysis of image-derived numerical data

Increasing automation of both microscopy and image analysis leads to large amounts of numerical data that need to be curated and (statistically) analyzed.

Typical steps in the inspection and analysis of image-derived numerical data are:
- Identify confounding variables, e.g. cell density or cell cycle state
- Identify data sets where image analysis failed (=> improve image analysis)
- Perform (automated) quality control on image sets, e.g. rejecting out-of-focus images 
- Compute the statistical significance of different treatment effects compared to controls

HTM Explorer is an R-based software that facilitates this workflow by providing:
- Graphical user interface: Ease of use to perform standard tasks
- Interactive plotting: Click&View functionality opens images associated with each data point in ImageJ
- Statistical analysis tools adapted to biological data, e.g. dealing with batch effects
- Freedom to perform non-standard analysis via the R-console


## Help, installation and getting started ##

### More help on functionality and usage ###

Go to the [HELP FOLDER](https://github.com/tischi/HTM_Explorer/tree/master/help) and just select any of the *.md files.


### Installation of R ###

First, you need to install the free software R (you'll need a version > 3.0). It will not work with R Studio!

[R on the web](http://www.r-project.org/)

Make sure the Tcl/Tk libraries are part of the installation package; normally this is the case by default.


### Installation of HTM Explorer ###

Download [HTM Explorer](
https://github.com/tischi/HTM_Explorer/archive/master.zip) and simply copy all the files somewhere onto your computer.


### Starting HTM Explorer ###

Start R and type below commands replacing `/path/to/` by the location of the HTM Explorer folder:

`setwd("/path/to/HTM_Explorer-master"); source("run.R");`

The first time you run HTM Explorer a few R packageswill be installed automatically. 
Now you should see the graphical user interface popping up and you are ready to go. 

### More help on usage ###

For more help on the usage of HTM Explorer please check all the "Help" buttons and "Help" menu items within the GUI.


### Special procedure when using Mac OS > 10.9 ###

- start XQuartz (you may need to install [XQuartz](https://xquartz.macosforge.org/landing/) first).

- type `R` in a XQuartz terminal window (this will start R)

- now the XQuartz terminal window will have become the R console window and you can proceed as explained above in __Starting the HTM tool__ 
