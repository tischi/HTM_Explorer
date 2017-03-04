## Configure vizualisation settings ##

### Path mapping ###

In order to open images you need to teach the software about the location of the images on your computer. Often the folder is different on your computer right now as it was while the analysis was done.

Please inspect the columns in your table where the paths to your images are stored and find out what is different on your computer.

Examples:

- If you ran the analysis on the __EMBL computing cluster__ and are now on __Windows__ you will have to enter something like
 
    `image_root_foldername_in_table = /g/almfscreen/`
    `image_root_foldername_on_this_computer = //almfscreen/almfscreen/`

- If you ran the analysis on the __EMBL computing cluster__ and are now on a __Mac__ you will have to enter something like
 
    `image_root_foldername_in_table = /g/almfscreen/`
    `image_root_foldername_on_this_computer = /Volumes/almfscreen/`

### Path to FIJI ###

Check that the "Path to FIJI" is pointing to an executable of ImageJ/Fiji.
For instance: "C:\Users\Bill\Desktop\Fiji.app\ImageJ-win64.exe