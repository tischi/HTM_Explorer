## Typical workflow using the HTM tool ##

### Loading your data and configuration ###

The input data to the HTM Explorer must be a single comma separated value (csv) table, where each row corresponds to one image; load this table:

`Main..Load Image Table`

You can view its content:

`Tables..Image Table..View`

If you already have a configuration file, load it with *(it is important that you first loaded your image table, because otherwise your settings may not be loaded correctly)*

`Main..Load configuration`

If this is a new assay, configure which information is stored in which column of your table (check the help button in this menu item for more information):

`Main..Configure..Assay columns`

Now you should also configure some settings that are important for the plotting functionality (check the help button in this menu item for more information):

`Main..Configure..Visualisation settings`

Now save the configurations:

`Main..Configure..Save configuration`

*Please note that there are more settings in the __Analysis__ menu item that are also saved in the configuration file. It therefore is a good idea to save the configuration file again after you modified the analysis settings!*
 

### Check whether the image table contains all data ###

Execute `Analysis..Assay overview` and check the information in the R terminal window!
If something is inconsistent with your expectations something might have gone wrong during the analysis which generated the image table.


### Visually check your experimental batches ###

Use `Plot..Heatmap` to inspect your data. As Measurements you typically inspect:

- Cell count in order to check your cell seeding etc.
- Your assay readout

Use the __Click and View__ button to inspect suspicious (outlier) images

    
### Automated per-image quality control ###

Find threshold values for image-based quality using 

	Plot..ScatterPlot 
		x-axis: not so important, maybe cell count 
		y-axis: an image quality measurement (see below)

Typical measurements for automated image-based quality control are
 
- **Focus quality** (e.g., *ImageQuality_PowerLogLogSlope_* in CellProfiler) 
- **Cell count** as too high/low cell density can affect your readout (see below)

Use **Click and View** to check which numbers correspond to "bad" images and use them in `Analysis..Image QC`.


### Check whether your readout correlates with cell density ###

Quite often, due to technical and biological reasons, the readout of an assay correlates with cell density. Check this using

	Plots..Scatter plot
    	x-axis: cell count
    	y-axis: your readout

If you find a correlation you have to think about what to do...

### Check whether your controls worked ###

	Plots..Jitter plot images
    	Label axis: treatment (e.g., Metadata_siRNA_gene)
    	Value axis: your assay readout
    	Treatment selection: your positive and negative controls

Do this batch by batch using the `Experiment selection` or make a new column in your image table containing both batch and treatment information and use this as the label axis. You can generate such a column (named **batch_treat**) in the R console window using the following command (this only works like this if you set the assay columns already as described above):

	htm@data$batch_treat = paste(htm@data[[htm@settings@columns$experiment]], htm@data[[htm@settings@columns$treatment]],sep="–-")

For doing the same with the per-position data using `Jitter plot positions` you need to type

	htm@wellSummary$plate_treat = paste(htm@wellSummary$experiment, htm@wellSummary$treatment ,sep="-–")



### Compute a per-position and per-treatment score from the images ###

Execute `Analysis..Statistical Analysis` carefully reading the `Help` of this menu item.
 
### Check the results of the per-position computation

either using 

  	Tables..Well Table..View
  	Plots..Heatmap
		Data set: positions

or saving the table with `Tables..Position Table..Save` and inspecting the results in something like Excel.

Tips for Excel:

    View...Freeze Panes..Freeze Top Row (keeps the header column in view)
	Home..Sort and Filter..Filter
 
 
### Check the results of the per-treatment computation


Saving the table with `Tables..Treatment table..Save` and inspect the results in something like Excel.
  

### More useful R commands (to be used in the console window) ###

Converting **siRNA--gene** to **gene--siRNA**, the latter being usually better for alphabetic sorting.

`htm@data$treatment = unlist(lapply(htm@data$Metadata_siRNA_gene, function(x) paste(strsplit(x,"--")[[1]][2],strsplit(x,"--")[[1]][1],sep="--")))`


 
