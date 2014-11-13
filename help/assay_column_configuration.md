## Configuration of assay columns ##


### Treatment (compulsory)###

The column that you chose here should contain text that signifies the treatment that the image in the respective row was subjected to. 
- Based on this column you will be able to chose a treatment subset in most of the plotting tools.
- The statistics analysis  will use this column to sort your data according to the treatment.


### Batch (compulsory) ###

The column that you chose here should contain text that determines which images are from the same measurement "batch".

- All images from the same batch will be shown together in the "Batch heatmap" plotting tool.
- In the statistical analysis there are batch-correction steps that will normalise the measurement values of one batch against all the control measurements from the same batch (see help on Statistical Analysis for more details).

Typically, one measurement batch is defined by data that are "comparable"; this could be due to

- cells from same passage number are used
- antibody stainings are from same "mix"
- data was acquired in same microscopy session

In high-throughput screening such a measurement batch typically is considered to be one multi-well plate.



### Position (Well) coordinate (needed for Heatmap plotting and Statistics) ###

The column containing the position coordinate must contain plain numbers that specify where (spatially) in each batch the measurements were taken (in the Heatmap plotting tool you specify a grid according to which the positions will be displayed.)
All images from the same position must have been subjected to the same treatment (it can be that there are multiple positions subjected to the same treatment). In the statistics tool there are "position-based" statistics computed, where in a first step all images from one position are averaged and, in a second step, the significance testing is then done by comparing different positions (from multiple batches) (see also help on statistical analysis).

__Example "Plate"__: In multi-well plate experiment one position would be one well; it is often the case the a number of wells contain the same (control) treatment.

__Example "Coverslips"__: In an experiment where you compare the look of cells on different coverslips one position would be one coverslip.


### Sub-position (image) coordinate (needed for Batch heatmap) ###

The column containing the sub-position coordinate must contain a number that specifies where (spatially) each image is located within its position. In the Batch Heatmap plotting tool you specify a grid according to which the positions and subpositions will be displayed.

__Example "Plate"__: The sub-position would be the positions of the images in a well.

__Example "Coverslips"__: You measured 10 coverslips subjected to different treatments and acquired 5x5 images on each coverslip.
The position coordinate would go from 1 to 10, specifying on which coverslip the image was taken.
The sub-position coordinate would go from 1 to 25, specifying on which position on the coverslip the image was taken.
In the Heatmap plotting tool you could specify for instance a 2x5 grid for the positions and should specify a 5x5 grid for the subpositions.
