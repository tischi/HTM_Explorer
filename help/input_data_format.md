## Input data ##

The input to HTM Explorer is a simple comma separated value (.csv) table containing per-image measurements. 


Each row of the table should contain measurements or meta-data of one image, e.g. "cell count", "mean cell intensity", "image filename". See also the table provided in the "example_input" folder.

The table must have a header row specifying the names of each column.

The table also must contain two columns with the following information:

- The biological treatment that the image was subjected to
- The measurement batch that the image should be assigned to

Please see the help on "assay column configuration" for more information on the "treatment" and "batch" columns!

In addition, to fully use all features of HTM Explorer you need columns containing the following information:

- Position coordinates 
- Sub-position coordinates
- Paths to the raw (and processed) images
- Filenames of the raw (and processed) images

Please see the help on "visualization settings" for more information.
