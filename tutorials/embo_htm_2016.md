# EMBO course on High-throughput microscopy 2016

## HTM data analysis literature

- Bray and Carpenter. Advanced Assay Development Guidelines for Image-Based High Content Screening and Analysis. 2012. 
- Birmingham A, Selfors LM, Forster T, Wrobel D, Kennedy CJ, Shanks E, Santoyo-Lopez J, Dunican DJ, Long A, Kelleher D, Smith Q, Beijersbergen RL, Ghazal P, Shamu CE. Statistical methods for analysis of high-throughput RNA interference screens. Nat Methods. 2009
- Malo et al., statistical practica in high-throughput screening data analysis Nature Biotech 2006
- Boutros, Bras, Huber, Genome Biology 2006

**Many methods, no standards => you have to think yourself and be prepared to defend your choices!**

## Starting HTM Explorer

- Follow instructions [here](https://github.com/tischi/HTM_Explorer/blob/master/README.md#help-installation-and-getting-started) to install R and to download the HTM Explorer software
- Start R or RStudio
- In the R console window execute below line of commands:
- `path = "\\\\embo2016\\embo2016\\htm2016\\HTM-Explorer\\R-code"; setwd(path); source("run.R");`

## Load data table

- [Tables > Load single table]
  - \\embo2016\\embo2016\htm2016\HTM-Explorer\Data\images--EMBO_HTM_2012.csv

- [Tables > View as spreadsheet]


## Configuration 

Here you tell the software which information is stored in which column of your data table.

### Load pre-configured configuration file

- [Configuration > Load configuration]
  - \\embo2016\\embo2016\\htm2016\\HTM-Explorer\\Data\\images--EMBO_HTM_2012--Config.R

### Configure assay columns

- [Configuration > Configure assay columns]
  - [Help] 
  - Treatment: Metadata_gene_siRNA
  - Batch: Metadata_plate

### Configure spatial layout

- [Configuration > Configure spatial layout]
  - Position: Metadata_wellNum
  - Sub-position: Metadata_posNum
  - positions x: 12
  - positions y: 8
  - sub-positions x: 1
  - sub-positions y: 1

### Configure visualisation settings

- [Configuration > Configure visualisation settings]
  - Check “Overlay” to be viewed
  - image_root_foldername_in_table: /g/
  - image_root_foldername_on_this_computer: //embo2016/embo2016/htm2016/HTM-Explorer/Data
  - Path to FIJI: C:\Users\teach\Desktop\Fiji.app\ImageJ-win64.exe

## Check for cell density gradients

- [Plotting > Heatmap]
  - Batch: EMBO_2012_Group1  (you may also look at the other ones..)
  - Measurement: **Count_Nuclei**
  - LUT settings: [3% quantiles all batches]
  - Highlight treatments: Scramble, PLK1, KIF11
  - [Show Heatmap] 
  - [Show LUT]
  - [Click & View]
  - [Save heatmaps of all plates]
     - Uncheck autoscale option 

## Check for staining gradients

- [Plotting > Heatmap]
  - Batch: EMBO_2012_Group1  (you may also look at the other ones..)
  - Measurement: **Mean_Nuclei_Intensity_MeanIntensity_Dna**
  - LUT settings: [3% quantiles all batches]
  - Highlight treatments: Scramble, PLK1, KIF11
  - [Show Heatmap] 
  - [Show LUT]
  - [Click & View]
  - [Save heatmaps of all plates]
     - Uncheck autoscale option 

## Check positive and negative controls for all plates

- [Plotting > Jitter plot]
- Label axis: Metadata_batch_treatment
- Value axis: **Count_Nuclei**
- Experiment selection: None selected
- Treatment selection: Scramble, Plk1, KIF11
- Sorting: alphabetic
- [Options] Check: Color by treatment
- [Plot]

Repeat above with different measurements such as **MaxIntensity_Dna**, **IntegratedIntensity_Dna**, **MeanIntensity_Dna**.
Think about the biological meaning about the different intensity measurements.

## Check for cell density dependence of your measurement

This does not really apply in this assay...but it usually is an issue.
See for instance these publications:

- [Population context determines cell-to-cell variability in endocytosis and virus infection. Nature 2009](http://www.nature.com/nature/journal/v461/n7263/full/nature08282.html)

Or check the example shown in [this presentation](https://github.com/tischi/presentation-biostatistics).

## Identify out-of-focus images

- [Plot > Scatter plot]
  - X axis: Count_Nuclei
  - Y axis: Image_Quality_PowerLogLogSlope_Dna
  - Experiment selection: None selected
  - Treatment selection: None selected
  - [Show plot]

Using [Click & View] find a threshold for "Image_Quality_PowerLogLogSlope_Dna” above which the images are OK; note this number, as you will need it below for the automated quality control.

## Quality control

- [Analysis > Quality Control]
  - Measurement: Image_Quality_PowerLogLogSlope_Dna
  - Minimum: -2
  - Maximum: 100000
  - [Add QC]
  - [Apply QCs Now] 
 
Check the output in the R console window!
  
After applying the QC, data that did not pass QC will be marked with a cross in all other plots.
You can check this for instance like this:

- [Plotting > Heatmap]
  - Batch: EMBO_2012_Group2
  - Measurement: Count_Nuclei
  - [Show Heatmap]


## Data transformation and normalisation 

Before you go on have a look at [this presentation](https://github.com/tischi/presentation-biostatistics).

- [Analysis > Normalisation]
  - [Help] for more information!
  - Measurement: Count_Nuclei
  - Data transformation: log2
  - Batch-wise normalisation: subtract_mean_control
  - Negative Control: Scramble
  - [Normalise]

Check the output in the R console window! 
Basically you now have new columns in your table.

## Visual inspection of normalised data

Use any of the plotting tools to check how the derived readout **HTM_norm__log2__subtract_mean_ctrl__Count_Nuclei** behaves.

## Statistical analysis

- [Analysis > Treatment summary]
  - Measurement: HTM_norm__log2__subtract_mean_ctrl__Count_Nuclei
  - Negative Control: Scramble
  - Positive Control (optional): KIF11
  - Number of objects: Count_Nuclei
  - [Analyze]

Check the output in the R console window.
Examine saved table in Excel.
