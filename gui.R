# TODO: if one window opens, close the other!
# make windows non-modal
###
### GUI helper functions
###

gui_AddRemoveVectorSetting <- function(setting,name,choices,container){
  
  .setting = setting
  
  #w <- gbasicdialog(title="...")
  
  gg <- ggroup(horizontal = FALSE, container=container)
  
  gf <- gframe(name, horizontal=F, container=gg, expand=TRUE)
  
  #glabel("   ", container=gf)
  #gftext <- gframe("Current Selection", horizontal=F, container=gg, expand=TRUE)
  #gf2 <- gframe("  Current selection:  ",container=gf)
  .selectiontext <- glabel("", bg="#ffffff", container=gf)
  htm <- get("htm", envir = globalenv())
  svalue(.selectiontext) <- paste(htmGetVectorSettings(.setting),collapse="\n")
  #font(.selectiontext) <- c("color"="black", "weight"="bold")
  #glabel("   ", container=gf)
  
  tmp <- ggroup(horizontal = TRUE, container=gf)
  obj <- gbutton("Add", container = tmp, handler = function(h,...) {
    htmAddVectorSetting(.setting,svalue(.choices))
    # update
    .selection[] <<- htmGetVectorSettings(.setting)  
    svalue(.selection) <<- .selection[length(.selection)]
    svalue(.selectiontext) <<- paste(htmGetVectorSettings(.setting),collapse="\n")
    #print(htmGetVectorSettings(.setting))
  })
  glabel("   ", container=tmp)
  .choices <- gcombobox(choices, container=tmp) 
  
  
  tmp <- ggroup(horizontal = TRUE, container=gf)
  obj <- gbutton("Remove", container = tmp, handler = function(h,...) {
    htm <- get("htm", envir = globalenv())
    htm <- htmRemoveVectorSetting(.setting,which(htmGetVectorSettings(.setting)==svalue(.selection)))
    # update
    .selection[] <<- htmGetVectorSettings(.setting)  
    svalue(.selection) <<- .selection[length(.selection)] 
    svalue(.selectiontext) <<- paste(htmGetVectorSettings(.setting),collapse="\n")   
    assign("htm", htm, envir = globalenv())
  })
  glabel("   ", container=tmp)
  htm <- get("htm", envir = globalenv())
  .selection <- gcombobox(htmGetVectorSettings(.setting), expand=TRUE, container=tmp)
  
  return(.choices)
  
}


gui_ListSettingDropdown <- function(text, setting, key, choices=c("None selected"), default = "None selected", container) {
  
  gg <- ggroup(horizontal = TRUE, container=container, expand=T)
  glabel(text, container=gg, expand=T)
  
  selected <- htmGetListSetting(htm, setting, key, gui=T)
  
  if(selected == "None selected") {
    htmSetListSetting(htm, setting, key, default,gui=T)
    selected = default
    print(paste(setting,key,"did not exist yet; set to",default))
  }
  
  print(paste("selected: ",selected))
  
  gc <- gcombobox(choices,
                  selected = selected,
                  container = gg, handler = function(h,...) {
                     htmSetListSetting(htm,setting,key,svalue(h$obj),gui=T)
                  }
  )
  
  return(gc)

  
}

gui_ListSettingTextfield <- function(text, setting, key, type="numeric", default = "None selected", container) {
  
  gg <- ggroup(horizontal = TRUE, container=container, expand=T)
  glabel(text, container=gg, expand=T)
  
  selected <- htmGetListSetting(htm, setting, key, gui=T)
  
  if(selected=="None selected") {
    htmSetListSetting(htm, setting, key, default, gui = T)
    selected = default
    print(paste(setting,key,"did not exist yet; set to",default))
  }
  
  
  if(type=="numeric") {
    
    ge <- gedit(text = selected,
                container = gg,
                handler = function(h,...) {
                  htmSetListSetting(htm, setting,key,as.numeric(svalue(h$obj)), gui = T)
                }
    )
    
    addHandlerKeystroke(ge, handler = function(h,...) {
      htmSetListSetting(htm, setting,key,as.numeric(svalue(h$obj)), gui = T);
    }           
    )
    
  } else if(type=="string") {
    
    ge <- gedit(text = selected,
                container = gg,
                handler = function(h,...) {
                  htmSetListSetting(htm, setting,key,svalue(h$obj), gui = T)
                }
    )
    
    addHandlerKeystroke(ge, handler = function(h,...) {
      htmSetListSetting(htm, setting,key,svalue(h$obj), gui = T);
    }           
    )
  }
  
  
}

gui_makeListSettingGuiTable <- function(setting,keys,container,type="numeric") {
  
  # todo: check the global assignments!!!
  
  tbl <- glayout(container = container, spacing = 1, expand = T)
  
  for(i in 1:length(keys)) {
    
    key = keys[i]
    tbl[i,1] <- glabel(key, container=tbl)
    
    if(type=="numeric") {
      tbl[i,2] <- eval(parse(text=(paste('gedit(htmGetListSetting(htm, setting,"',key,'", gui=T), container=tbl, handler = function(h,...) {
                                         htmSetListSetting(htm, setting,"',key,'",as.numeric(svalue(h$obj)), gui=T)
    })',sep=""))))
      eval(parse(text=(paste('addHandlerKeystroke(tbl[i,2], handler = function(h,...) {
                             htmSetListSetting(htm, setting,"',key,'",as.numeric(svalue(h$obj)), gui=T);
                             #print(as.numeric(svalue(h$obj)))
      })',sep=""))))
    } else if (type=="string") {
      tbl[i,2] <- eval(parse(text=(paste('gedit(htmGetListSetting(htm, setting,"',key,'", gui=T), container=tbl, handler = function(h,...) {
                                         htmSetListSetting(htm, setting,"',key,'",svalue(h$obj), gui=T)
  })',sep=""))))
      eval(parse(text=(paste('addHandlerKeystroke(tbl[i,2], handler = function(h,...) {
                             htmSetListSetting(htm, setting,"',key,'",svalue(h$obj), gui=T);
                             #print(as.numeric(svalue(h$obj)))
      })',sep=""))))      
    } 
    
    
  }
  
  return(tbl)
  
}


###
### GUI Handlers
###


# todo: add Mean of All Plates and Median of All Plates as viewing options
guiHandler_Heatmap <- function(h, ...) {

  if(is.null(htm@settings@columns$experiment)) {
    gmessage("You need to specify the experiment and treatment columns [Main..Configure..Assay columns]")
    return(NULL)
  }
  
  
  #htm <- htmMakeLayoutReplicateColumn(htm); 
  plates <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  
  w <- gwindow("Batch heatmap", visible=F)
  
  gf <- gframe(text=" Layout Settings ", horizontal = FALSE, container=w, expand=T)
  tbl <- gui_makeListSettingGuiTable(setting="visualisation",
                                     keys=c("number_positions_x","number_positions_y","number_subpositions_x","number_subpositions_y"),
                                     container=gf)
  glabel("  ",cont=w)
  
  gp <- ggroup(horizontal = FALSE, container=w)
  
  tmp <- ggroup(horizontal = TRUE, container=gp)
  glabel("Data set:   ",cont=tmp)
  choices <- c("images","positions")
  guiSelectedData <<- gcombobox(choices, container=tmp, handler = function(h,...){
    if(svalue(h$obj)=="images") guiSelectedMeasurement[] <<- colnames(htm@data)
    if(svalue(h$obj)=="positions") guiSelectedMeasurement[] <<- colnames(htm@wellSummary)
  })
  
  
  #tmp <- ggroup(horizontal = TRUE, container=gp)
  #glabel("Layout:   ",cont=tmp)
  #choices <- unique(htm@data[[htm@settings@columns$visualisation]])
  #guiSelectedLayout <- gcombobox(choices, container=tmp, handler=function(h,...){
  #  guiSelectedReplicate[] <<- unique(htm@data[[htm@settings@columns$platereplicate]][which(htm@data[[htm@settings@columns$visualisation]]==svalue(h$obj))])
  #})
  #svalue(guiSelectedLayout) <- choices[1] # intialise
  
  #tmp <- ggroup(horizontal = TRUE, container=gp)
  #glabel("Replicate:   ",cont=tmp)
  #choices <- unique(htm@data[[htm@settings@columns$platereplicate]][which(htm@data[[htm@settings@columns$visualisation]]==svalue(guiSelectedLayout))])
  #guiSelectedReplicate <- gcombobox(choices, container=tmp)
  #svalue(guiSelectedReplicate) <- choices[1] # intialise
  #glabel("  (only used for image view)  ",cont=tmp)
  
  tmp <- ggroup(horizontal = TRUE, container=gp)
  glabel("Batch:  ", container = tmp)
  guiPlateSubset <- gcombobox(plates, 
                              selected = htmGetListSetting(htm,"visualisation","platePlotPlate", gui=T), 
                              container = tmp, 
                              handler = function(h,...){
                                htmSetListSetting(htm,"visualisation","platePlotPlate",svalue(h$obj),gui=T)
                              })
      
  tmp <- ggroup(horizontal = TRUE, container=gp)
  glabel("Measurement:   ",cont=tmp)
  measurements <- colnames(htm@data)
  guiSelectedMeasurement <- gcombobox(measurements, 
                              selected = htmGetListSetting(htm,"visualisation","platePlotMeasurement",gui=T), 
                              container = tmp, 
                              handler = function(h,...){
                                htmSetListSetting(htm,"visualisation","platePlotMeasurement",svalue(h$obj),gui=T)
                              })  
  glabel("   ", container=tmp)
 
   
  tmp <- ggroup(horizontal = TRUE, container=gp)
  glabel("LUT min (blue):   ", container=tmp, expand=F)
  guiLUTmin <- gedit(0, width = 5, container=tmp)
  glabel("   LUT max (red):   ", container=tmp, expand=F)
  guiLUTmax <- gedit(0, width = 5, container=tmp)
  svalue(guiLUTmin) <- 0.0 # intialise
  svalue(guiLUTmax) <- 100.0 # intialise
  addHandlerKeystroke(guiLUTmin, handler = function(h,...) { svalue(guiLUTmin) <<- svalue(h$obj) })
  addHandlerKeystroke(guiLUTmax, handler = function(h,...) { svalue(guiLUTmax) <<- svalue(h$obj) })
  glabel("  ", container= tmp)
  
  
  gbutton("3% quantiles selected batch", container = tmp, handler = function(h,...) {
    
    if(svalue(guiSelectedData)=="images") {
      #print(paste("Setting LUT to Min&Max of",svalue(guiSelectedMeasurement),"in per_image data"))
      dat <- subset(htm@data,htm@data[[htm@settings@columns$experiment]]==svalue(guiPlateSubset),svalue(guiSelectedMeasurement))
    }
    
    if(svalue(guiSelectedData)=="positions") {
      #print(paste("Setting LUT to Min&Max of",svalue(guiSelectedMeasurement),"in per_well data"))
      dat <- subset(htm@wellSummary,htm@wellSummary$experiment==svalue(guiPlateSubset),svalue(guiSelectedMeasurement))
    }
 
    #print(paste("Lower = ", min(dat, na.rm=T) ))
    #print(paste("Maximum = ", max(dat, na.rm=T) ))
    svalue(guiLUTmin) <<- quantile(dat, 0.03, na.rm=T) #min(dat, na.rm=T) 
    svalue(guiLUTmax) <<- quantile(dat, 0.97, na.rm=T) #max(dat, na.rm=T) 
    
    }
  )
  glabel("   ", container=tmp)
  
    
  gbutton("3% quantiles all batches", container = tmp, handler = function(h,...) {
    
    # todo: make more concise
    if(svalue(guiSelectedData)=="images") {
      dat <- htm@data[[svalue(guiSelectedMeasurement)]]
    }
    if(svalue(guiSelectedData)=="positions") {
      dat <- htm@wellSummary[[svalue(guiSelectedMeasurement)]]
    }
    svalue(guiLUTmin) <<- quantile(dat, 0.03, na.rm=T) #min(dat, na.rm=T) 
    svalue(guiLUTmax) <<- quantile(dat, 0.97, na.rm=T) #max(dat, na.rm=T)      
  }
  )
  glabel("   ", container=tmp)
  
  
  
  tmp <- ggroup(horizontal = TRUE, container = w)
  
  obj <- gbutton("Show Heatmap", container = tmp, handler = function(h,...) {
       
    htmShowHeatmap(htm=htm, 
                   #selectedLayout=svalue(guiSelectedLayout), 
                   #selectedReplicate=svalue(guiSelectedReplicate),
                   selectedExp = svalue(guiPlateSubset),
                   selectedMeasurement = svalue(guiSelectedMeasurement),
                   markQC = T,
                   colorLUT.autoscale = F,
                   colorLUT.min = svalue(guiLUTmin),
                   colorLUT.max = svalue(guiLUTmax),
                   datatype = svalue(guiSelectedData))
    
    
    # todo: make this generic 
    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp = svalue(guiPlateSubset), 
                                     selectedTreatment = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_1",gui=T),
                                     color = "black")
    
    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp = svalue(guiPlateSubset), 
                                     selectedTreatment = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_2",gui=T),
                                     color = "white")
    
    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp = svalue(guiPlateSubset), 
                                     selectedTreatment = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_3",gui=T),
                                     color = "green")
    
    
  } )
  
  
  glabel("   ", container=tmp)

  obj <- gbutton("Show LUT", container = tmp, handler = function(h,...) {
    colorbar(colorRampPalette(c("blue","white","red"))(255), min = svalue(guiLUTmin), max = svalue(guiLUTmax))
  }  )
  
  glabel("   ", container=tmp)
  
  ### click and view image
  obj <- gbutton(" Click & View ", container = tmp, handler = function(h,...) {
    
    if(dev.cur()>1) {
      print("please select an image for viewing!")
      selectedExp = svalue(guiPlateSubset)
      datatype = svalue(guiSelectedData)
      selectedMeasurement = svalue(guiSelectedMeasurement)
      
      if(datatype == "images") {
        xy = htm_convert_wellNum_posNum_to_xy(htm@data[[htm@settings@columns$wellnum]],htm@data[[htm@settings@columns$posnum]])
        ids <- which(htm@data[[htm@settings@columns$experiment]]==selectedExp )
        x = xy$x[ids]
        y = xy$y[ids]
        i <- identify(x, y, n = 1, plot = FALSE)
        #print(irow)
        #print(ids[irow])
        print("       ")
        print(paste("experiment =", htm@data[[htm@settings@columns$experiment]][ids[i]]))
        print(paste("treatment =", htm@data[[htm@settings@columns$treatment]][ids[i]]))
        print(paste(selectedMeasurement,"=", htm@data[[selectedMeasurement]][ids[i]]))
        print("       ")
        htmShowImagesFromRow(htm,htm@data,ids[i])
      } else if (datatype =="positions") {
        ids <- which(htm@wellSummary$experiment==selectedExp )
        xy = htm_convert_wellNum_to_xy(htm@wellSummary$wellNum)
        x = xy$x[ids]
        y = xy$y[ids]
        i <- identify(x, y, n = 1, plot = FALSE)
        print("       ")
        print(paste("experiment =", htm@wellSummary$experiment[ids[i]]))
        print(paste("treatment =", htm@wellSummary$treatment[ids[i]]))
        print(paste(selectedMeasurement,"=", htm@wellSummary[[selectedMeasurement]][ids[i]]))
        print("       ")
      }
    } else {
        print("No plot open.")
    }
  })
  
  
  glabel(" ", container = gp)
  
  # todo: make this generic (loop over colors, using mapply)
  # - labelColors <- c("black","darkgreen","yellow")
  gf <- gframe(" Highlight Treatments ", horizontal=F, container=gp, expand=TRUE)
  ###  TREATMENT HIGHLIGHT
  .treatments = c("None selected",sort(unique(htm@data[[htm@settings@columns$treatment]])))
  
    
  tmp <- ggroup(horizontal = TRUE, container=gf)
  glabel("black:   ",container=tmp)
  gcombobox(.treatments, container=tmp,
            selected = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_1",gui=T), 
            handler = function(h,...){
              htmSetListSetting(htm, "visualisation","heatmap_mark_treat_1",svalue(h$obj),gui=T)
            })
  
  tmp <- ggroup(horizontal = TRUE, container=gf)
  glabel("white:   ",container=tmp)
  gcombobox(.treatments, container=tmp,
            selected = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_2",gui=T), 
            handler = function(h,...){
              htmSetListSetting(htm, "visualisation","heatmap_mark_treat_2",svalue(h$obj),gui=T)
            })
  
  tmp <- ggroup(horizontal = TRUE, container=gf)
  glabel("green:   ",container=tmp)
  gcombobox(.treatments, container=tmp,
            selected = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_3",gui=T), 
            handler = function(h,...){
              htmSetListSetting(htm, "visualisation","heatmap_mark_treat_3",svalue(h$obj),gui=T)
            })
   
  glabel(" ", container = gp)
  
  tmp <- ggroup(horizontal = TRUE, container = w)
  
  obj <- gbutton("Save heatmaps of all plates", container = tmp, handler = function(h,...) {
    .path = gfile("Select output file (.pdf)", type="save", filter = list("All files" = list(patterns = c("*.pdf"))))
    # todo: automatically add .pdf if it is not part of the filename
    htmMakeAllHeatmaps(htm=htm, 
                       datatype = svalue(guiSelectedData),
                       measurement=svalue(guiSelectedMeasurement),
                       path=.path,
                       markQC = T,
                       colorLUT.autoscale = htmGetListSetting(htm,"visualisation","heatmap_autoscale_TF",gui=T),
                       colorLUT.min=svalue(guiLUTmin),
                       colorLUT.max=svalue(guiLUTmax),
                       selectTreat1 = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_1",gui=T),
                       selectTreat2 = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_2",gui=T),
                       selectTreat3 = htmGetListSetting(htm,"visualisation","heatmap_mark_treat_3",gui=T)
    )
  })
  
  glabel("  ",cont = tmp)
  gcheckbox("Autoscale LUT to 3% quantiles of each batch", 
            checked = htmGetListSetting(htm,"visualisation","heatmap_autoscale_TF",gui=T), 
            container = tmp, 
            handler = function(h,...){
              htmSetListSetting(htm,"visualisation","heatmap_autoscale_TF",svalue(h$obj),gui=T)
            })
  
  
  visible(w) <- T
  
  return(htm)
  
}

guiHandler_VisualisationSettings  <- function(h, ...) {
  # todo: default initalisation somewhere else!
  
  w <- gwindow("Visualisation Settings", visible=F, use.scrollwindow =T, expand=T)
   
  # *******************
  glabel(" ", container = w)
  gf <- gframe(text=" Image Viewing Settings ", horizontal = FALSE, container=w, expand=T)
  gp <- ggroup(horizontal = FALSE, container=gf, expand=T)
  tbl <- gui_makeListSettingGuiTable(setting="visualisation",
                                 keys=c("image_filename_prefix","image_foldername_prefix"),
                                 container=gp,
                                 type="string")
  
  
  
  
  getImageList <- function(){
    
    # convenience function
    getimagenames <- function(colnames,prefix) {
      imagenames <- vector()
      for ( colname in colnames ) {
        if( grepl(prefix,colname) ) {
          imagenames[length(imagenames)+1] <- strsplit(colname,prefix)[[1]][2]
        }
      }
      return(imagenames)
    }
    
    print(htm@settings@visualisation$image_filename_prefix)
    images <- getimagenames(colnames(htm@data),htm@settings@visualisation$image_filename_prefix)
    imageschecked <- htmGetListSetting(htm,"visualisation","viewImages",gui=T)
    imagescheckedTF <- rep(F,length(images))
    if(length(images)) {
      for(i in 1:length(images)) {
        imagescheckedTF[i] <- ifelse(images[i] %in% imageschecked, T, F)
      }
      #checkboxgroup_viewImages <<- imagescheckedTF
    } else {
      print("no images found")
      images <- c("No images found in table, using image_filename_prefix")
      imagescheckedTF <- c(F)
    }
    l = list()
    l$images <- images
    l$selected <- imagescheckedTF
    return(l)
  }
  
  gf2 <- gframe(" Select images to be viewed upon clicking within a plot: ", container=gf)
  
  # init checkboxgroup_viewImages
  l <- getImageList()
  checkboxgroup_viewImages <- gcheckboxgroup(l$images,
                                             l$selected,
                                              container = gf2, expand = T, handler = function(h,...){
                                                print(svalue(h$obj))
                                                htmSetListSetting(htm, "visualisation","viewImages",svalue(h$obj), gui=T)
                                              })
  

  # refresh checkboxgroup_viewImages
  #refreshImageList()
  
  obj <- gbutton("Refresh image list", container = gf, handler = function(h,...) {
    print("refresh")
    l <- getImageList()
    print(l$images)
    checkboxgroup_viewImages[] <<- l$images
    svalue(checkboxgroup_viewImages) <<- l$selected 
    })
  
  gp <- ggroup(horizontal = FALSE, container=gf, expand=T)
  glabel("Path mapping:", cont = gp)
  tbl <- gui_makeListSettingGuiTable(setting="visualisation",
                                 keys=c("image_root_foldername_in_table","image_root_foldername_on_this_computer"),
                                 container=gp,
                                 type="string")
  
  
  # *******************
  
  glabel(" ", container = w)
  gf <- gframe(text=" Plotting Settings ", horizontal = FALSE, container=w, expand=T)
  tbl <- gui_makeListSettingGuiTable(setting="visualisation",
                                    keys=c("heatmap_width","heatmap_image_size_cex","jitter"),    #,"heatmap_well_size_cex"
                                    container=gf)
  
  gcheckbox("Show data points that did not pass QC", 
            checked = htmGetListSetting(htm,"visualisation","plotting_showQCfailData_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm, "visualisation","plotting_showQCfailData_TF",svalue(h$obj),gui=T)
              print(paste("color points by treatment",svalue(h$obj)))
            })
  
  
  gui_ListSettingTextfield(text = "Path to FIJI (only necessary for Windows):",
                           setting = "visualisation",
                           key = "windows_path_to_fiji",
                           type = "string",
                           default = "C:\\Program Files\\Fiji.app\\ImageJ-win64.exe",
                           container = w)
  
  
  
  gp <- ggroup(horizontal = T, container=w)
  
  obj <- gbutton("Close", container = gp, handler = function(h,...) {
    dispose(w)
  })
  
  obj <- gbutton("Help", editable=FALSE, container = gp, handler = function(h,...) { guiShowHelpFile("visualisation_settings.md") } )
  
  
  visible(w) <- T
  
}



guiHandler_JitterPlot_Help <- function(h,...) {
  
  w <- gwindow("", visible=F)
  gf <- gframe("  Help for Jitter Plot  ", cont=w, horizontal=F)
  glabel(" ", container=gf)
  glabel("Statistical output:", container=gf)
  glabel("", container=gf)
  glabel("Statistics are computed against the selected Reference.")
  glabel("  Z-score = abs(mean(x)-mean(ref)) / sd(ref) ", container=gf)
  glabel("  Robust Z-score = abs(median(x)-median(ref)) / mad(ref) ", container=gf)
  glabel("  Z-factor = 1 - 3*(sd(ref)+sd(x))/abs(mean(ref)-mean(x))", container=gf)
  glabel("  T-test = Student's t-test", container=gf)
  glabel("  ", container=gf)
  glabel("The Z-score is the number of standard deviations by which the mean of condition x differs from the reference mean.", container=gf)
  glabel("  ", container=gf)  
  glabel("The robust Z-score is the number of median average deviations by which the median of condition x differs from the reference median.", container=gf)
  glabel("  ", container=gf)  
  glabel("The Z-factor is typically computed between positive and negative controls and is a measure of assay quality.", container=gf)
  glabel("  http://en.wikipedia.org/wiki/Z-factor", container=gf)
  glabel("  Z-factor>0.5 => excellent separation (>12 sd separation)", container=gf)
  glabel("  Z-factor>0 => marginal separation (>6 sd separation) ", container=gf)
  glabel("  Z-factor<0 => too much overlap", container=gf)
  glabel("  ", container=gf)  
  glabel("The T-test is used to determine if two sets of data are significantly different from each other.", container=gf)
  glabel("  http://en.wikipedia.org/wiki/Student's_t-test", container=gf)  
  glabel("  The output value gives the probabiliy that the means of both sets are the same.", container=gf)
  glabel("  Typically values <0.05 are considered to indicate a statistically signficant difference of the sample means.", container=gf)
  glabel("  ", container=gf)  
  visible(w) <- T
  
} 

guiHandler_JitterPlotImages <- function(h,...){
  gui_JitterPlot(datatype="images") 
}

guiHandler_JitterPlotWells <- function(h,...){
  gui_JitterPlot(datatype="positions") 
}

gui_JitterPlot <- function(datatype="images"){

  if(is.null(htm@settings@columns$experiment)) {
    gmessage("You need to specify the experiment and treatment columns [Main..Configure..Assay columns]")
    return(NULL)
  }
  
  
  htm <- get("htm", envir = globalenv())
  
  w <- gwindow(paste("Jitter Plot:",datatype), visible = F)
  
  gp <- ggroup(horizontal = FALSE, container=w)
  
  if(datatype=="images") {
    columns <- sort(colnames(htm@data))
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    treatments <- sort(unique(htm@data[[htm@settings@columns$treatment]]))
  } else if(datatype=="positions") {
    columns <- sort(colnames(htm@wellSummary))
    experiments <- sort(unique(htm@wellSummary$experiment))
    treatments <- sort(unique(htm@wellSummary$treatment))
  }
  
  glabel("Label axis:  ", container=gp)
  cx <- gcombobox(c("None selected", columns), 
                  selected = htmGetListSetting(htm,"visualisation","jitterPlotX",gui=T), 
                  container = gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","jitterPlotX",svalue(h$obj),gui=T)
                  })

  
  glabel("Value axis:  ", container=gp)
  cy <- gcombobox(c("None selected", columns), 
                  selected = htmGetListSetting(htm,"visualisation","jitterPlotY",gui=T), 
                  container = gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","jitterPlotY",svalue(h$obj),gui=T)
                  })
  
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Experiment selection:  ", container=gp)
  
  guiExpSubset <- gcombobox(c("None selected", experiments), 
                  selected = htmGetListSetting(htm,"visualisation","jitterPlotExpSubset",gui=T), 
                  container = gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm,"visualisation","jitterPlotExpSubset",svalue(h$obj),gui=T)
                  })
   
  
  glabel(" ", container=w)
  
  gui_AddRemoveVectorSetting(setting="visualisation$treatmentSelectionForPlotting",
                             name=" Treatment selection: ",
                             choices = c("None selected", treatments ),
                             container = w)  
  
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Sorting:  ", container=gp)
  guiSorting <- gcombobox(c("none","alphabetic","median value"), container=gp)
  
  gp <- ggroup(horizontal = T, container=w)
  obj <- gbutton("Plot it!", editable=FALSE, container = gp, handler = function(h,...){
    htm <- get("htm", envir = globalenv())
    print(paste("treatmentSelectionForPlotting",htmGetVectorSettings("visualisation$treatmentSelectionForPlotting")))
    htmJitterplot(htm = htm,
                  cx = svalue(cx),
                  cy = svalue(cy),
                  .ylab = svalue(cy),
                  datatype = datatype,
                  experimentSubset = svalue(guiExpSubset),
                  treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                  sorting = svalue(guiSorting),
                  colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                  showMedian = htmGetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",gui=T),
                  showMean = htmGetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",gui=T)
                  )
  })
 
  
  obj <- gbutton("Zoom in", editable=FALSE, container = gp, handler = function(h, ...){
      loc = locator(n=2)
      htm <- get("htm", envir = globalenv())
      htmJitterplot(htm, svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y),.ylab=svalue(cy),
                    experimentSubset = svalue(guiExpSubset), 
                    datatype = datatype,
                    colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                    sorting = svalue(guiSorting),
                    newdev = F,
                    treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                    showMedian = htmGetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",gui=T),
                    showMean = htmGetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",gui=T)
                    )
  })
  
  if(datatype=="images") {
    
    gbutton("Click and view", container = gp, handler = function(h, ...){
      if(dev.cur()==1) {
        print("No plot open.")
      } else {
        htm <- get("htm", envir = globalenv())
        htmJitterplot(htm,
                      svalue(cx),svalue(cy),.xlim=sort(loc$x),.ylim=sort(loc$y),
                      experimentSubset = svalue(guiExpSubset), 
                      colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                      sorting = svalue(guiSorting),
                      datatype = datatype,
                      newdev = F, action="click",
                      treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting")
        )
      }
    })
  }
  
  obj <- gbutton("Help", editable=FALSE, container = gp, handler = guiHandler_JitterPlot_Help)
  
  obj <- glabel("   ", container = gp) 
  gbutton(" Options ", container = gp, handler = guiHandler_JitterPlot_Options)

  visible(w) <- T
                   
  #obj <- gbutton("Click and view image", editable=TRUE, container = gp, handler = handler_showImageJitterPlot )
  
}



guiHandler_JitterPlot_Options <- function(h,...) {
  
  w <- gwindow("Jitter Plot Options", visible = F)
  
  glabel(" ", container=w)
  gcheckbox("Color points by treatment", 
            checked = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",svalue(h$obj),gui=T)
              print(paste("color points by treatment",svalue(h$obj)))
            })
  
  gcheckbox("Indicate Mean and SD as green lines in the plot", 
            checked = htmGetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",svalue(h$obj),gui=T)
            })

  gcheckbox("Indicate Median and MAD as blue lines in the plot", 
            checked = htmGetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",svalue(h$obj),gui=T)
            })
  

  gcheckbox("Scale y-axis from zero", 
            checked = htmGetListSetting(htm,"visualisation","jitterPlot_scaleFromZero_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm,"visualisation","jitterPlot_scaleFromZero_TF",svalue(h$obj),gui=T)
            })
  
  visible(w) <- T
}


guiHandler_ScatterPlot <- function(h,...){

  
  if(is.null(htm@settings@columns$experiment)) {
    gmessage("You need to specify the experiment and treatment columns [Main..Configure..Assay columns]")
    return(NULL)
    }
  
  
  w <- gwindow("Scatter Plot", visible = F)
  print("Scatter Plot") 
  tmp <- ggroup(horizontal = TRUE, container=w)
  
  glabel("Data set:   ",cont=tmp)
  choices <- c("images","objects","positions")
  guiSelectedData <- gcombobox(choices, container=tmp, handler = function(h,...){
    if(svalue(h$obj)=="images") {
      cx[] <<- colnames(htm@data)
      cy[] <<- colnames(htm@data)
    }
    if(svalue(h$obj)=="objects") {
      cx[] <<- colnames(htm@objectdata)
      cy[] <<- colnames(htm@objectdata)
    }
    if(svalue(h$obj)=="positions") {
      cx[] <<- colnames(htm@wellSummary)
      cy[] <<- colnames(htm@wellSummary)
    }
  })
 

  gp <- ggroup(horizontal = T, container=w)
  glabel("x axis:", container=gp)
  cx <- gcombobox(c("None selected",colnames(htm@data)), 
                  selected = htmGetListSetting(htm,"visualisation","scatterPlotX",gui=T), container=gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","scatterPlotX",svalue(h$obj),gui=T)
                  })
   
  gp <- ggroup(horizontal = T, container=w)
  glabel("y axis:", container=gp)
  cy <- gcombobox(c("None selected",colnames(htm@data)), 
                  selected = htmGetListSetting(htm,"visualisation","scatterPlotY",gui=T), container=gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm,"visualisation","scatterPlotY",svalue(h$obj),gui=T)
                  })
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Experiment selection:  ", container=gp)
  guiExpSubset <- gcombobox(c("None selected", unique(htm@data[[htm@settings@columns$experiment]])), container=gp)
  
  glabel(" ", container=w)
  gui_AddRemoveVectorSetting(setting="visualisation$treatmentSelectionForPlotting",
                             name=" Treatment selection: ",
                             choices = c("None selected",sort(unique(htm@data[[htm@settings@columns$treatment]]))),
                             container = w)  
  
  glabel(" ", container=w)
  gp <- ggroup(horizontal = T, container=w)
  glabel("color points by:  ", container=gp)
  guiColorize <- gcombobox(c("None selected","treatment","experiment"), container = gp)
  
  glabel(" ", container=w)
  gp <- ggroup(horizontal = T, container=w)
  gbutton("Show plot", container = gp, handler = function(h,...) {
    htmScatterPlot(get("htm", envir = globalenv()), 
                   svalue(cx), svalue(cy), 
                   experimentSubset = svalue(guiExpSubset),
                   treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                   datatype = svalue(guiSelectedData),
                   colorize = svalue(guiColorize),
                   newdev = T) 
  })
  
  gbutton("Zoom", container = gp, handler = function(h, ...){
    loc = locator(n=2)
    htmScatterPlot(get("htm", envir = globalenv()), 
                   svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y), 
                   experimentSubset = svalue(guiExpSubset),
                   treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                   datatype = svalue(guiSelectedData),
                   colorize = svalue(guiColorize),
                   newdev = F)
  })
  
  gbutton("Click and view", container = gp, handler = function(h, ...){
    if(dev.cur()==1) {
      print("No plot open.")
    } else {
      htmScatterPlot(get("htm", envir = globalenv()), 
                   svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y), 
                   experimentSubset = svalue(guiExpSubset),
                   treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                   datatype = svalue(guiSelectedData),
                   colorize = svalue(guiColorize),
                   newdev=F , action="click")
    }
  })
  
  gbutton(" Options ", container = gp, handler = guiHandler_ScatterPlot_Options)
  
  #obj <- gbutton("Zoom (not working yet)", editable=FALSE, container = gp, handler = handler_zoomJitterPlot )
  #obj <- gbutton("Click and view image", editable=TRUE, container = gp, handler = handler_showImageJitterPlot )
  
  visible(w) <- T
}



guiHandler_ScatterPlot_Options <- function(h,...) {
  
  w <- gwindow("Scatter Plot Options", visible = F)
  
  glabel(" ", container=w)
  #gcheckbox("Color points by treatment", 
  #          checked = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
  #          container = w, 
  #          handler = function(h,...){
  #            htmSetListSetting("visualisation","jitterPlot_colorizeTreatments_TF",svalue(h$obj))
  #           print(paste("color points by treatment",svalue(h$obj)))
  #         })
  
  
  
  gcheckbox("Scale x and y axis from zero", 
            checked = htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm, "visualisation","scatterPlot_scaleFromZero_TF",svalue(h$obj),gui=T)
            })
  
  visible(w) <- T
}


guiHandler_LoadImageTable <- function(h, ...) {
  .path = gfile("Select image table (.csv)", type="open",filter = list("All files" = list(patterns = c("*.csv"))))
  if(is.na(.path)) {
   print("no file selected. loading aborted.")
   return()
  }
  # todo: handle the cancel
  print(paste("selected table file = ",.path))
  
  # todo: have some modal waiting box
  w <- gwindow("Please wait", visible=F, expand=T)
  gp <- ggroup(horizontal = FALSE, container=w, expand=T)
  obj <- glabel("Loading. Please wait.", container=gp, expand=T)
  visible(w) <- T
  print("Please Wait. Loading Data.")
  flush.console()
  Sys.sleep(0.5)
  
  if(!exists("htm")) {
    htm <<- htmLoadDataFromFile(htm=NULL, "data", path=.path)
  } else {
    print(paste("htm <<- htmLoadDataFromFile(htm=NULL, 'data', path =",.path ))
    htm <<- htmLoadDataFromFile(htm=htm, "data", path=.path)
  }
  print("done.")
  dispose(w)
}


guiHandler_LoadObjectTable <- function(h, ...) {
  .path = gfile("Select object table (.csv)", type="open")
  if(is.na(.path)) {
    print("no file selected. loading aborted.")
    return()
  }
  # todo: have some modal waiting box
  w <- gwindow("Please wait", visible=F, expand=T)
  gp <- ggroup(horizontal = FALSE, container=w, expand=T)
  obj <- glabel("Loading. Please wait.", container=gp, expand=T)
  visible(w) <- T
  print("Please Wait. Loading Data.")
  flush.console()
  Sys.sleep(0.5)
  
  if(!exists("htm")) {
    htm <<- htmLoadDataFromFile(htm=NULL, "objectdata", path=.path)
  } else {
    htm <<- htmLoadDataFromFile(htm=htm, "objectdata", path=.path)
  }
  print("done.")
  dispose(w)
}

guiHandler_htmOverview <- function(h, ...) {
  htmOverview(htm=htm)
}

guiHandler_NewHTM <- function(h,...) {
  if(gconfirm("Really reinitialise all settings and data?")) {
    htm <<- htmMake()
    print("cleaned everything. HTM object is empty.")    
  } else {
    print("done nothing. HTM object is as was.")   
  }
}


guiHandler_SetColumns <- function(h, ...) {
  # todo: put into a for loop 
  
  w <- gwindow("Select column names containing ...", visible=F, expand=T)
  gp <- ggroup(horizontal = FALSE, container=w, expand=T)
  tbl <- glayout(container = gp, expand=T)
  
  columns = colnames(htm@data)
  
  i = 1
  tbl[i,1] <- glabel("Treatment:", container=tbl, expand=T)
  tbl[i,2] <- gcombobox(columns, 
                        selected=htmGetColumnNumber(htm, htmGetListSetting(htm,"columns","treatment",gui=T)), 
                        container=tbl,
                        handler=function(h,...) {
                          htmSetListSetting(htm,"columns","treatment",svalue(h$obj),gui=T)
                        })
  
  i = i + 1
  tbl[i,1] <- glabel("Batch (=Experiment=Replicate):", container=tbl)
  tbl[i,2] <- gcombobox(columns, 
                        selected=htmGetColumnNumber(htm, htmGetListSetting(htm,"columns","experiment",gui=T)), 
                        container=tbl,
                        handler=function(h,...) {
                          htmSetListSetting(htm,"columns","experiment",svalue(h$obj),gui=T)
                        })

  i = i + 1
  tbl[i,1] <- glabel("Position (=Well) coordinate:", container=tbl)
  tbl[i,2] <- gcombobox(columns, 
                        selected=htmGetColumnNumber(htm, htmGetListSetting(htm,"columns","wellnum",gui=T)), 
                        container=tbl,
                        handler=function(h,...) {
                          htmSetListSetting(htm,"columns","wellnum",svalue(h$obj),gui=T)
                        })
  
  i = i + 1
  tbl[i,1] <- glabel("Sub-position (=Image) coordinate:", container=tbl)
  tbl[i,2] <- gcombobox(columns, 
                        selected=htmGetColumnNumber(htm, htmGetListSetting(htm,"columns","posnum",gui=T)), 
                        container=tbl,
                        handler=function(h,...) {
                          htmSetListSetting(htm,"columns","posnum",svalue(h$obj),gui=T)
                        })
  
  gg <- ggroup(horizontal = TRUE, container=w, expand=T)
  
  gbutton(" Close ", container = gg, handler = function(h,...) {
    htm <- get("htm", envir = globalenv()) 
    cTreat = htmGetListSetting(htm,"columns","treatment",gui=T)
    cBatch = htmGetListSetting(htm,"columns","experiment",gui=T)
    htm@data$Metadata_batch_treatment = paste(htm@data[[cBatch]],htm@data[[cTreat]],sep="__")
    print(paste("Generated new colum Metadata_batch_treatment, combining",cBatch,"and",cTreat))
    assign("htm", htm, envir = globalenv())   
    dispose(w)
  })
  
  glabel("   ", container=gg)
  
  gbutton(" Help ", container = gg, handler = function(h,...) {
    guiShowHelpFile("assay_column_configuration.md")
    })
          
  
  visible(w) <- TRUE
  
  }


guiShowHelpFile <- function(help_file) {
  
  help_path = paste0(htmPath,"/help/") 
  renderMarkdown(paste0(help_path,help_file),output=paste0(help_path,"tmp.html"))
  browseURL(paste0(help_path,"tmp.html"))
  
}


guiHandler_ImageQCs <- function(h, ...) {
  w <- gwindow("Image QC Add/Remove", visible=F)
  gp <- ggroup(horizontal = FALSE, container=w)

  # todo: one can add the same one twice!
  
  tmp <- gframe("Add Image QC", horizontal=F, container=gp, expand=TRUE)
  # todo: make this a table layout
  glabel("Measurement:", container=tmp)
  imQCmeasurement <- gcombobox(colnames(htm@data), container=tmp)
  glabel("Minimum:", container=tmp)
  imQCmin <- gedit("1", container=tmp); addHandlerKeystroke(imQCmin, handler = function(h,...) {imQCmin <<- as.numeric(svalue(h$obj))})
  glabel("Maximum:", container=tmp)
  imQCmax <- gedit("100", container=tmp); addHandlerKeystroke(imQCmax, handler = function(h,...) {imQCmax <<- as.numeric(svalue(h$obj))})
  #glabel("",container=tmp)
  obj <- gbutton("Add QC", container = tmp, handler = function(h,...) {
    htm <- get("htm", envir = globalenv())   
    htm <- htmAddImageQC(htm,svalue(imQCmeasurement),as.numeric(svalue(imQCmin)),as.numeric(svalue(imQCmax)))
    imQCs[] <<- htmGetImageQCs(htm) # update 
    svalue(imQCs) <<- imQCs[length(imQCs)]
    assign("htm", htm, envir = globalenv()) 
    })
  
  glabel("", container=gp)
  tmp <- gframe("Remove Image QC", horizontal=F, container=gp, expand=TRUE)
  imQCs <- gcombobox(htmGetImageQCs(htm), container=tmp)
  obj <- gbutton("Remove QC", container = tmp, handler = function(h,...) {
    htm <- get("htm", envir = globalenv()) 
    htm <- htmRemoveImageQCs(htm,which(htmGetImageQCs(htm)==svalue(imQCs)))
    imQCs[] <<- htmGetImageQCs(htm) # update
    svalue(imQCs) <<- imQCs[length(imQCs)]
    assign("htm", htm, envir = globalenv()) 
  })
  
  glabel("", container=gp)
  obj <- gbutton("Apply Image QCs Now", container = gp, handler = function(h,...) {
    htm <- get("htm", envir = globalenv()) 
    htm <- htmApplyImageQCs(htm)
    assign("htm", htm, envir = globalenv()) 
  })
    
  
  # intialise
  if(length(imQCs)==0) {
    svalue(imQCs) <<- c("No Image QC",0,0)
  }
  
  visible(w) <- T
  
}



guiHandler_AverageAndNormalise <- function(h,...){
  
  if(is.null(htm@settings@columns$treatment)) {
    gmessage("You need to first specify the treatment column [Main..Configure..Assay columns]!")
    return(NULL)
  }
  
  
  w <- gwindow("Statistical Analysis", visible=F)
  
  # gui_ListSetting <- function(text, setting, key, choices, container) {

  
  htm <- get("htm", envir = globalenv())
  
  gui_ListSettingDropdown(text = "  Measurement to be analysed:  ",
                          setting = "statistics",
                          key = "measurement",
                          choices = colnames(htm@data),
                          default = colnames(htm@data)[1],
                          container = w)
  
  gui_ListSettingDropdown(text = "  Method to average images within one position (well): ",
                          setting = "statistics",
                          key = "wellSummaryMethod",
                          choices = c("weighted_mean_of_images","mean_of_images","median_of_images"),
                          default = "weighted_mean_of_images",
                          container = w)
  
  # htmSetListSetting("statistics","wellSummaryMethod","weighted_mean_of_images")
  
   
  gui_ListSettingDropdown(text = "  Number of objects per image:  ",
                          setting = "statistics",
                          key = "objectCount",
                          choices = colnames(htm@data),
                          default = colnames(htm@data)[1],
                          container = w)
  
  #gui_ListSettingTextfield(text = " Well QC: Minimum number of valid images:  ",
  #                         setting = "statistics",
  #                         key = "WellQC_Minimum_Number_Valid_Images",
  #                         type = "numeric",
  #                         default = 1,
  #                         container = w)
  
  gui_ListSettingTextfield(text = "  Well QC: Minimum number of valid objects:  ",
                           setting = "statistics",
                           key = "WellQC_Minimum_Number_Objects",
                           type = "numeric",
                           default = 100,
                           container = w)
  
  #gui_AddRemoveVectorSetting(setting="ctrlsNeg",
  #                           name=" Negative controls  ",
  #                           choices = c("all treatments",sort(unique(htm@data[[htm@settings@columns$treatment]]))),
  #                           container=w)
  
  gui_ListSettingDropdown(text = "  Negative control  ",
                          setting = "statistics",
                          key = "negativeControl",
                          choices = c("all treatments",sort(unique(htm@data[[htm@settings@columns$treatment]]))),
                          default = colnames(htm@data)[1],
                          container = w)
   
  
  gui_ListSettingDropdown(text = "  Data transformation  ",
                          setting = "statistics",
                          key = "transformation",
                          choices = c("log2","none"),
                          default = "log2",
                          container = w)
  
 
#  gui_ListSettingDropdown(text = "  Method to compute statistics against the negative controls:  ",
#                          setting = "statistics",
#                          key = "normalisationMethod",
#                          #choices = c("robust_zScore_negCtrl_perExp","zScore_negCtrl_perExp","ratio_MedianOfNegCtrl_perExp","ratio_MeanOfNegCtrl_perExp","no_normalisation"),
#                          choices = c("ratio_ANOVA","ANOVA","None","zScore_negCtrl_perExp"),
#                          default = "ratio_ANOVA",
#                          container = w)
  
#  gui_ListSettingDropdown(text = "  Well averaging (method to average wells with same treatment on same plate): ",
#                          setting = "statistics",
#                          key = "treatmentWithinReplicateSummaryMethod",
#                          choices = c("mean_of_wells","median_of_wells"),
#                          default = "mean_of_wells",
#                          container = w)
  

 
  htmSetListSetting(htm, "statistics","treatmentWithinReplicateSummaryMethod","mean_of_wells", gui=T)
  
#  gui_ListSettingTextfield(text = "  Treatment QC: minimum number of valid replicates:  ",
#                           setting = "statistics",
##                           key = "TreatQC_Minimum_Number_Valid_Replicates",
#                           type = "numeric",
#                           default = 1,
#                           container = w)
  

  obj <- glabel("   ", container = w)
  gg <- ggroup(horizontal = TRUE, container=w, expand=T)
  obj <- gbutton(" Analyze", container = gg, handler = function(h,...) {
    
    # image QC
    htm <- get("htm", envir = globalenv())
    htm <- htmApplyImageQCs(htm)
    assign("htm", htm, envir = globalenv())
    
    # well summary
    htm <- get("htm", envir = globalenv())
    htm@wellSummary <- htmWellSummary(htm)
    assign("htm", htm, envir = globalenv())
    
    # image normalisation for image based statistics
    if( htmGetListSetting(htm,"statistics","compute_image_based_stats_TF", gui=T) == T ) {
      htm <- get("htm", envir = globalenv())
      htm@data <- htmImageNormalization(htm)
      assign("htm", htm, envir = globalenv())
    }
    
    # treatment summary
    htm <- get("htm", envir = globalenv())
    htm@treatmentSummary <- htmTreatmentSummary(htm)
    assign("htm", htm, envir = globalenv())    
    
    # save treatment summary
    path = gfile("Save as...", type="save", initialfilename = paste0("TreatmentSummary--",htmGetListSetting(htm,"statistics","transformation",gui=T),"--",htmGetListSetting(htm,"statistics","measurement",gui=T),".csv"))
    htmSaveDataTable(htm, "treatmentSummary", path)
  
    
    })
  
  obj <- glabel("   ", container = gg) 
  gbutton(" Help ", container = gg, handler = function(h,...) { guiShowHelpFile("statistical_analysis.md") })
  
  obj <- glabel("   ", container = gg) 
  gbutton(" Options ", container = gg, handler = guiHandler_AverageAndNormalise_Options)

  glabel("    ", container=gg)
  obj <- gbutton(" Close ", container = gg, handler = function(h,...) {
    dispose(w)
  })
  
  visible(w) <- T
  
}

guiHandler_AverageAndNormalise_Options <- function(h,...){

  w <- gwindow("Statistical Analysis", visible=F)
  
  gui_AddRemoveVectorSetting(name="  Experiments to be excluded  ",
                             setting="statistics$experiments_to_exclude",
                             choices = c(sort(unique(htm@data[[htm@settings@columns$experiment]]))),
                             container = w)
  
  gui_ListSettingDropdown(text = "  Positive control  ",
                          setting = "statistics",
                          key = "positiveControl",
                          choices = c("None selected",sort(unique(htm@data[[htm@settings@columns$treatment]]))),
                          default = colnames(htm@data)[1],
                          container = w)
  
  gui_ListSettingDropdown(text = "  Gradient correction  ",
                          setting = "statistics",
                          key = "gradientCorrection",
                          choices = c("None selected","medpolish"),
                          default = "None selected",
                          container = w)
  
  
  print("Compute image based statistics?")
  gcheckbox("Compute image based statistics?", 
            checked = htmGetListSetting(htm,"statistics","compute_image_based_stats_TF",gui=T), 
            container = w, 
            handler = function(h,...){
                htmSetListSetting(htm, "statistics","compute_image_based_stats_TF",svalue(h$obj), gui=T)
              })
  
  
  visible(w) <- T
  
  }

guiHandler_htmLoadSetttings <- function(h,...){
  
  .path = gfile("Select settings file", type="open")
  htm <<- htmLoadSetttings(htm,.path)
  
}

guiHandler_htmSaveSetttings <- function(h,...){
  
  .path = gfile("Save as...", type="save")
  htmSaveSetttings(htm,.path)
  
}





############
# MENU BAR #
############

mbl <- list()

mbl$Main$"Help"$handler =  function(h,...) { guiShowHelpFile("typical_usage.md") }
#mbl$Main$"Reinitialise"$handler = guiHandler_NewHTM
mbl$Main$"Load image table"$handler = guiHandler_LoadImageTable
mbl$Main$"Load configuration"$handler = guiHandler_htmLoadSetttings
mbl$Main$"Save configuration"$handler = guiHandler_htmSaveSetttings
mbl$Main$Configure$"Assay columns"$handler = guiHandler_SetColumns
mbl$Main$Configure$"Plotting"$handler = guiHandler_VisualisationSettings
#mbl$Main$Configure$"Load config"$handler = guiHandler_htmLoadSetttings
mbl$Main$Quit$handler = function(h,...) {dispose(w)}
mbl$Main$Quit$icon = "quit"

mbl$Plot$"Heatmap"$handler = guiHandler_Heatmap
mbl$Plot$"Scatter plot"$handler = guiHandler_ScatterPlot
mbl$Plot$"Jitter plot images"$handler = guiHandler_JitterPlotImages
mbl$Plot$"Jitter plot positions"$handler = guiHandler_JitterPlotWells
#mbl$Plot$"Treatment summary plot"$handler = guiHandler_TreatmentSummarySortedValuesPlot

mbl$Analysis$"Assay overview"$handler = guiHandler_htmOverview
mbl$Analysis$"Image QC"$handler = guiHandler_ImageQCs
mbl$Analysis$"Statistical analysis"$handler = guiHandler_AverageAndNormalise 

mbl$Tables$"Image table"$"View"$handler = function(h,...) { edit(htm@data) }
mbl$Tables$"Image table"$"Load"$handler = guiHandler_LoadImageTable
mbl$Tables$"Image table"$"Save"$handler = function(h,...) { path = gfile("Save as...", type="save"); htmSaveDataTable(htm, "data", path)}
#mbl$Tables$"Object Table"$"View"$handler = function(h,...) { edit(htm@objectdata) }
#mbl$Tables$"Object Table"$"Load"$handler = guiHandler_LoadObjectTable
#mbl$Tables$"Object Table"$"Save"$handler = guiHandler_SaveObjectTable
mbl$Tables$"Position table"$"View"$handler = function(h,...) { edit(htm@wellSummary) }
mbl$Tables$"Position table"$"Save"$handler = function(h,...) { path = gfile("Save as...", type="save"); htmSaveDataTable(htm, "wellSummary", path)}
mbl$Tables$"Treatment table"$"View"$handler = function(h,...) { edit(htm@treatmentSummary) }
mbl$Tables$"Treatment table"$"Save"$handler =  function(h,...) { path = gfile("Save as...", type="save"); htmSaveDataTable(htm, "treatmentSummary", path)}

#mbl$Special$"Add columns"$handler = guiHandler_AddColumns

#mbl$About$HTM_Explorer$handler = function(h,...) print("High Throughput Microscopy Explorer by ALMF@EMBL.DE")


w <- gwindow("HTM Explorer",visible=F,width=500,height=20)
mb <- gmenu(mbl, container=w)
visible(w) <- T



#tbl <- list()
#tbl$New <- list(icon="new",handler = function(...) print("new"))
#tbl$Print <- list(icon="print",handler = function(...) print("print"))
#tb <- gtoolbar(tbl, container=w)
