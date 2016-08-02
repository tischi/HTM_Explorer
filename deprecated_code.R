



handler_zoomJitterPlot <-  function(h, ...){
  loc = locator(n=2)
  jp.plot(jp.cx,jp.cy,ylabel=jp.cy,jp.xlim=sort(loc$x),jp.ylim=sort(loc$y) )
}


htmShowTreatmentsFromRow  <- function(htm, data, ir){
  
  treatment = data$treatment[ir]
  imageIds <- which((htm@data[[htm@settings@columns$treatment]]==treatment) & htm@data$HTM_qcImages)
  iMin = imageIds[which.min(htm@data$MDD_vsTreat[imageIds])]
  o <- order(htm@data$MDD_vsTreat[imageIds])
  minis <- imageIds[o[1:3]]
  
  cols <- which(grepl("__minusMeanCtrl__zScore",colnames(htm@data)))
  
  print(htm@data[minis,cols])
  
  #print(htm@data$Count_Cells[imageIds])
  #print(htm@data$Count_Cells[imageIds[iMin]])
  #iRandom <- floor(runif(1, 1, length(images)+1))
  
  # call htmShowImagesFromRow
  htmShowImagesFromRow(htm, htm@data, minis)
  
}

htmShowObjectsFromRow <- function(htm, data, irs){
  
  iObject = irs[1]
  
  print("Generating columns that link image and object table.")
  c1 = htmGetListSetting(htm,"columns","treatment")
  c2 = htmGetListSetting(htm,"columns","experiment")
  c3 = htmGetListSetting(htm,"columns","wellnum")
  c4 = htmGetListSetting(htm,"columns","posnum")  
  data$HTM_imageobjectlink = paste(data[[c1]],data[[c2]],data[[c3]],data[[c4]],sep="--")
  htm@data$HTM_imageobjectlink = paste(htm@data[[c1]],htm@data[[c2]],htm@data[[c3]],htm@data[[c4]],sep="--")
  data$HTM_imageID = unlist(lapply(data$HTM_imageobjectlink, function(x) which(htm@data$HTM_imageobjectlink==x)))    
  iImage = which(htm@data$HTM_imageobjectlink==data$HTM_imageobjectlink[iObject])
  #print(data[iObject,])
  #iImage = data$ImageNumber[iObject]
  print(paste("Image Number", iImage))
  xObject = round(data$Location_Center_X[iObject])
  yObject = round(data$Location_Center_Y[iObject])
  treatment = htm@data[[htmGetListSetting(htm,"columns","treatment")]][iImage]
  
  #cmd = paste0(' -eval \"run(\'setTool(\'point\')\');\"')
  #cmd = paste0('  -eval \"wait(500);\" ')
  
  cmd = paste0('  -eval \"rename(\'',treatment,'\');\" ')  
  cmd = paste0(cmd, ' -eval \"makePoint(',xObject,',',yObject,');\"')
  #print(cmd)
  
  htmShowImagesFromRow(htm,htm@data,iImage,cmd)
  
}

htmShowImagesFromRow <- function(htm,data,irs,appendCommand=""){
  
  #print(paste("image for viewing",imageforviewing))
  
  filenamePrefix = htm@settings@visualisation$image_filename_prefix
  foldernamePrefix = htm@settings@visualisation$image_foldername_prefix
  # convert to forward slashes immideately, otherwise gsub has problems later
  rootFolderTable = gsub("\\\\" ,"/",htm@settings@visualisation$image_root_foldername_in_table)
  rootFolderReal = gsub("\\\\" ,"/",htm@settings@visualisation$image_root_foldername_on_this_computer)  
  
  if( .Platform$OS.type == "unix" ) {
    #imageViewerCMD = "/Applications/Fiji.app/Contents/MacOS/fiji-macosx --no-splash"
    imageViewerCMD = '/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx -debug '
  } else if( .Platform$OS.type == "windows" ) {
    imageViewerCMD = paste("\"",htm@settings@visualisation$windows_path_to_fiji,"\""," -debug ",sep="")
    #imageViewerCMD = "\"c:\\Program Files\\FIJI\\fiji-win64.exe\" --no-splash -macro"
  } else {
    imageViewerCMD = "unkown"
  }
  
  images <- vector()
  
  for ( ir in irs) {
    
    for ( colname in colnames(data) ) {
      
      if( foldernamePrefix != "" ) {  
        # construct pathname from file- and folder-name
        
        if( grepl(filenamePrefix,colname) ) {
          
          imagename = strsplit(colname,filenamePrefix)[[1]][2]
          filename = data[[colname]][ir]
          cFolder = paste(foldernamePrefix,imagename,sep="")
          #print(cFolder)
          foldername = gsub("\\\\" ,"/",data[[cFolder]][ir])
          foldername = gsub(rootFolderTable, rootFolderReal, foldername)
          
          if ( .Platform$OS.type == "unix" ) {
            #foldername = gsub("\\\\" ,"/", foldername)
            #pathname = paste('"',foldername,"/",filename,'"',sep="")
            pathname = paste(foldername,"/",filename,sep="")
          } else if ( .Platform$OS.type == "windows" ) {
            #foldername = gsub("/", "\\\\", foldername)
            #pathname = paste("\"",foldername,"\\",filename,"\"",sep="")
            pathname = paste(foldername,"/",filename,sep="")
          }
          
          if(imagename %in% htmGetListSetting(htm,"visualisation","viewImages")) {
            images[length(images)+1] <- pathname
            print(pathname)
          }      
          
          
        }
        
      } else {
        
        # directly get the pathname=filename
        if( grepl(filenamePrefix,colname) ) {
          
          imagename = strsplit(colname,filenamePrefix)[[1]][2]
          if(imagename %in% htmGetListSetting(htm,"visualisation","viewImages")) {
            print(colname)
            pathname = gsub("\\\\" ,"/",data[[colname]][ir])
            #print(pathname)
            #print(rootFolderReal)
            #print(rootFolderTable)
            pathname = gsub(rootFolderTable, rootFolderReal, pathname)
            #print(pathname)
            
            if ( .Platform$OS.type == "unix" ) {
              pathname = gsub("\\\\" ,"/", pathname)
            } else if ( .Platform$OS.type == "windows" ) {
              pathname = gsub("/", "\\\\", pathname)
            }
            
            images[length(images)+1] <- pathname
            #print(pathname)
          }      
          
        }
        
      }
      
    }
    
  }
  
  
  if(length(images)) {
    
    #cmd = paste(imageViewerCMD,pathmacro,argument)
    cmd = imageViewerCMD
    for(image in images) {
      cmd = paste(cmd, paste0(' -eval \"open(\'',image,'\')\"') )
    }
    
    if(length(images)>1) {
      # make stack
      cmd = paste(cmd, paste0(' -eval \"run(\'Images to Stack\');\"'))
      # make composite
      #cmd = paste(cmd, paste0(' -eval \"run(\'Make Composite\', \'display=Composite\');\"'));  
    }
    
    
    cmd = paste(cmd,appendCommand)
    print(cmd)
    system(cmd,wait=F)
    
  } else {
    print("No images selected/found. Cannot open.")
  }
  
  
}


guiHandler_JitterPlot <- function(h,...){
  
  print("Jitter plot")
  
  #if(is.null(htm@settings@columns$experiment)) {
  #  gmessage("You need to specify the experiment and treatment columns [Main..Configure..Assay columns]")
  #  return(NULL)
  #}
  
  htm <- get("htm", envir = globalenv())
  
  w <- gwindow(paste("Jitter Plot"), visible = F)
  
  if (htmGetListSetting(htm,"visualisation","jitterPlot_datatype",gui=T)=="None selected")  {
    htmSetListSetting(htm,"visualisation","jitterPlot_datatype","images",gui=T)  
  }
  
  tmp <- ggroup(horizontal = TRUE, container=w)
  glabel("Data set:   ",cont=tmp)
  choices <- c("images","objects","positions")
  guiSelectedData <- gcombobox(choices, container=tmp, selected=htmGetListSetting(htm,"visualisation","jitterPlot_datatype",gui=T), handler = function(h,...){
    
    htmSetListSetting(htm, "visualisation","jitterPlot_datatype",svalue(h$obj),gui=T)
    
    if(svalue(h$obj)=="images") columns <- sort(c("None selected",colnames(htm@data)))      
    if(svalue(h$obj)=="objects") columns <- sort(c("None selected",colnames(htm@objectdata)))
    if(svalue(h$obj)=="positions") columns <-  sort(c("None selected",colnames(htm@wellSummary)))
    
    cx[] <<- columns
    cy[] <<- columns
    
  })
  
  
  datatype = htmGetListSetting(htm,"visualisation","jitterPlot_datatype",gui=T)
  print(paste("datatype",datatype))
  
  if(datatype=="images") columns <- sort(c("None selected",colnames(htm@data)))      
  if(datatype=="objects") columns <- sort(c("None selected",colnames(htm@objectdata)))
  if(datatype=="positions") columns <-  sort(c("None selected",colnames(htm@wellSummary)))
  
  experiments <- NULL
  treatments <- NULL
  if(!is.null(htm@settings@columns$experiment)) experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  if(!is.null(htm@settings@columns$treatment)) treatments <- sort(unique(htm@data[[htm@settings@columns$treatment]]))
  
  #} else if(datatype=="objects") {
  #  columns <- sort(colnames(htm@objectdata))
  #  experiments <- sort(unique(htm@objectdata[[htm@settings@columns$experiment]]))
  #  treatments <- sort(unique(htm@objectdata[[htm@settings@columns$treatment]]))    
  #} else if(datatype=="positions") {
  #  columns <- sort(colnames(htm@wellSummary))
  #  experiments <- sort(unique(htm@wellSummary$experiment))
  #  treatments <- sort(unique(htm@wellSummary$treatment))
  #}
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Label axis:  ", container=gp)
  cx <- gcombobox(c("None selected", columns), 
                  selected = htmGetListSetting(htm,"visualisation","jitterPlotX",gui=T), 
                  container = gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","jitterPlotX",svalue(h$obj),gui=T)
                  })
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Value axis:  ", container=gp)
  cy <- gcombobox(c("None selected", columns), 
                  selected = htmGetListSetting(htm,"visualisation","jitterPlotY",gui=T), 
                  container = gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","jitterPlotY",svalue(h$obj),gui=T)
                  })
  
  
  
  #gp <- ggroup(horizontal = T, container=w)
  
  glabel(" ", container=w)
  
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
                             choices = c("None selected",treatments),
                             container = w,
                             showSelected=F)  
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Sorting:  ", container=gp)
  guiSorting <- gcombobox(c("None selected","alphabetic","median value"), container=gp)
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Compute t-test against:  ", container=gp)
  guiReference <- gcombobox(c("None selected",treatments), container=gp)
  
  glabel(" ", container=w)
  gp <- ggroup(horizontal = T, container=w)
  obj <- gbutton("Plot", editable=FALSE, container = gp, handler = function(h,...){
    htm <- get("htm", envir = globalenv())
    # print(paste("treatmentSelectionForPlotting",htmGetVectorSettings("visualisation$treatmentSelectionForPlotting")))
    htmJitterplot(htm = htm,
                  cx = svalue(cx),
                  cy = svalue(cy),
                  .ylab = svalue(cy),
                  datatype = svalue(guiSelectedData),
                  experimentSubset = svalue(guiExpSubset),
                  treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                  sorting = svalue(guiSorting),
                  colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                  showMedian = htmGetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",gui=T),
                  showMean = htmGetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",gui=T),
                  reference = svalue(guiReference)
    )
  })
  
  
  obj <- gbutton("Zoom", editable=FALSE, container = gp, handler = function(h, ...){
    loc = locator(n=2)
    htm <- get("htm", envir = globalenv())
    htmJitterplot(htm, svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y),.ylab=svalue(cy),
                  experimentSubset = svalue(guiExpSubset), 
                  datatype = svalue(guiSelectedData),
                  colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                  sorting = svalue(guiSorting),
                  newdev = F,
                  treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                  showMedian = htmGetListSetting(htm,"visualisation","jitterPlot_showMedianAndMAD_TF",gui=T),
                  showMean = htmGetListSetting(htm,"visualisation","jitterPlot_showMeanAndSD_TF",gui=T)
    )
  })
  
  #if(datatype=="images" | datatype=="objects") {
  
  gbutton("Click & View", container = gp, handler = function(h, ...){
    if(dev.cur()==1) {
      print("No plot open.")
    } else {
      htm <- get("htm", envir = globalenv())
      htmJitterplot(htm,
                    svalue(cx),svalue(cy),.xlim=sort(loc$x),.ylim=sort(loc$y),
                    experimentSubset = svalue(guiExpSubset), 
                    colorizeTreatments = htmGetListSetting(htm,"visualisation","jitterPlot_colorizeTreatments_TF",gui=T), 
                    sorting = svalue(guiSorting),
                    datatype = svalue(guiSelectedData),
                    newdev = F, action="click",
                    treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting")
      )
    }
  })
  
  #}
  
  obj <- gbutton("Help", editable=FALSE, container = gp, handler = guiHandler_JitterPlot_Help)
  
  obj <- glabel("   ", container = gp) 
  gbutton(" Options ", container = gp, handler = guiHandler_JitterPlot_Options)
  
  visible(w) <- T
  
  #obj <- gbutton("Click and view image", editable=TRUE, container = gp, handler = handler_showImageJitterPlot )
  
  
  
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
  choices <- c("images","objects","positions")
  guiSelectedData <<- gcombobox(choices, container=tmp, selected=htmGetListSetting(htm,"visualisation","heatmap_datatype",gui=T), handler = function(h,...){
    htmSetListSetting(htm, "visualisation","heatmap_datatype",svalue(h$obj),gui=T)
    
    if(svalue(h$obj)=="images") guiSelectedMeasurement[] <<- colnames(htm@data)
    if(svalue(h$obj)=="positions") guiSelectedMeasurement[] <<- colnames(htm@wellSummary)
    if(svalue(h$obj)=="objects") guiSelectedMeasurement[] <<- colnames(htm@objectdata)
  })
  
  
  datatype = htmGetListSetting(htm,"visualisation","heatmap_datatype",gui=T)
  print(paste("datatype",datatype))
  
  if(datatype=="images") columns <- sort(c("None selected",colnames(htm@data)))      
  if(datatype=="objects") columns <- sort(c("None selected",colnames(htm@objectdata)))
  if(datatype=="positions") columns <-  sort(c("None selected",colnames(htm@wellSummary)))
  
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
    
    if(svalue(guiSelectedData)=="objects") {
      #print(paste("Setting LUT to Min&Max of",svalue(guiSelectedMeasurement),"in per_well data"))
      dat <- subset(htm@objectdata,htm@objectdata[[htm@settings@columns$experiment]]==svalue(guiPlateSubset),svalue(guiSelectedMeasurement))
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
    if(svalue(guiSelectedData)=="objects") {
      dat <- htm@objectdata[[svalue(guiSelectedMeasurement)]]
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

htmShowHeatmap <- function(htm, selectedExp="", selectedMeasurement, markQC = T, width = htm@settings@visualisation$heatmap_width, 
                           colorLUT.autoscale = F, colorLUT.min = 0, colorLUT.max = 100, newdevice = T, datatype="images",
                           show_gradient_correction = F)  {
  
  ### GET INFO FROM HTM
  
  if(datatype == "images") {
    #dat <- subset(htm@data,(htm@data[[htm@settings@columns$visualisation]]==selectedLayout) & (htm@data[[htm@settings@columns$platereplicate]]==selectedReplicate));
    dat <- subset(htm@data,(htm@data[[htm@settings@columns$experiment]]==selectedExp));
  }
  
  if(datatype == "wells") {
    dat <- subset(htm@wellSummary, (htm@wellSummary$experiment==selectedExp) );
  }
  
  if(datatype == "objects") { # convert to image frame
    
    dat <- subset(htm@data,(htm@data[[htm@settings@columns$experiment]]==selectedExp), select=c(selectedMeasurement, htm@settings@columns$wellnum, htm@settings@columns$posnum))
    
    dat <-  ddply(dat, c(htm@settings@columns$wellnum, htm@settings@columns$posnum), function(z) {
      if(is.numeric(z)) {
        apply(z, 2, mean)
      } else {
        apply(z, 2, function(z) {z[1]})
      }
    }
    )
    
    
    dat[[selectedMeasurement]] = as.numeric( dat[[selectedMeasurement]] )
    dat[[htm@settings@columns$posnum]] = as.numeric( dat[[htm@settings@columns$posnum]] )
    if(!any(grepl("(?i)[A-Z]", dat[[htm@settings@columns$wellnum]]))) {
      dat[[htm@settings@columns$wellnum]] = as.numeric( dat[[htm@settings@columns$wellnum]] )
    }
    
  }  
  
  
  if(colorLUT.autoscale == T) {
    colorLUT.min <- quantile(dat[[selectedMeasurement]], 0.03, na.rm=T) #min(dat, na.rm=T) 
    colorLUT.max <- quantile(dat[[selectedMeasurement]], 0.97, na.rm=T) #max(dat, na.rm=T)      
  }
  
  
  print("")
  print("Show heatmap:")
  print("*************")
  print(paste("selected Experiment =",selectedExp))
  print(paste("selected Measurement =",selectedMeasurement))
  print(paste("colorLUT.min =",colorLUT.min))
  print(paste("colorLUT.max =",colorLUT.max))
  
  dat$val = 255*(dat[[selectedMeasurement]]-as.numeric(colorLUT.min) )/(as.numeric(colorLUT.max)-as.numeric(colorLUT.min) )
  toosmall = which(dat$val<1)
  toolarge = which(dat$val>255)
  dat$val[toosmall]=1
  dat$val[toolarge]=255
  colpal <- colorRampPalette(c("blue","white","red"))(255)
  
  plate.nrow = htm@settings@visualisation$number_positions_y
  plate.ncol = htm@settings@visualisation$number_positions_x
  
  if(newdevice) { 
    dev.new(width = 1.5 * width, height =  plate.nrow / plate.ncol * width)
  } 
  
  op <- par(bg = "grey")
  
  #par(mar=c(5.1,6.1,6.1,2.1))
  
  if(datatype=="images" || datatype=="objects") {
    
    print("plotting per_image or mean_averaged per_object data")
    xy = htm_convert_wellNum_posNum_to_xy(dat[[htm@settings@columns$wellnum]],dat[[htm@settings@columns$posnum]])
    
    plot(x = xy$x, y = xy$y, col=colpal[dat$val], 
         ylim = rev(range(xy$y)),  
         xlim = round(range(xy$x)),
         xaxt = "n", yaxt = "n", xaxs = "i",
         pch = 15, cex= htm@settings@visualisation$heatmap_image_size_cex, xlab="", ylab="",
         asp = 1
    )
    title(paste(selectedExp,selectedMeasurement,sep="\n"),cex.main=1)
  } 
  
  
  if(datatype=="wells") {
    print("plotting per_well data")
    xy = htm_convert_wellNum_to_xy(dat$wellNum)
    plot(x=xy$x, y=xy$y, col=colpal[dat$val], 
         ylim = rev(range(xy$y)), 
         xaxt = "n", yaxt = "n", #  xaxs = "i",
         pch = 15, cex= 1.3 * htm@settings@visualisation$heatmap_image_size_cex * htm@settings@visualisation$number_subpositions_x, xlab="", ylab="",
         asp = 1)
    #repls = paste(sort(unique(htm@wellSummary$replicate[which(htm@wellSummary$layout==selectedLayout)])),collapse="-")
    title(paste(selectedExp,selectedMeasurement,sep="\n"),cex.main=1)
  } 
  
  
  
  axis(1, at=1:plate.ncol, labels=1:plate.ncol, las=1,  cex.axis = 1)
  axis(2, at=1:plate.nrow, labels=LETTERS[1:plate.nrow], las=2, cex.axis = 1)
  
  par(op)
  
  
  #todo: change qcImages to ImageQC
  
  if(markQC) {
    
    # image QC
    if(datatype == "images") {
      
      if(!is.null(dat$HTM_qcImages)) {    
        print("  adding image QC.")
        ids <- which(dat$HTM_qcImages==0)
        x <- vector(length=length(ids))
        y <- vector(length=length(ids))
        for (i in ids) {
          x[i] <- xy$x[i]
          y[i] <- xy$y[i]
        }
        points(x, y, pch=4, cex=1*htm@settings@visualisation$heatmap_image_size_cex)
        
      } else {
        print("  no image QC available.")
      }
      
      # well QC
      if(!is.null(dat$HTM_qcWells)) {
        print("  adding well QC.")
        for (i in which(dat$HTM_qcWells==0)) {
          points(xy$x[i], xy$y[i], pch=4, cex=htm@settings@visualisation$heatmap_image_size_cex * htm@settings@visualisation$number_subpositions_x) 
        }
      } else {
        print("  no well QC available.")
      }
    }
    
    
    if(datatype=="wells") {
      
      if(!is.null(dat$wellQC)) {
        print("  adding well QC.")
        for (i in which(dat$wellQC==0)) {
          points(dat$wellPlatePosX[i], dat$wellPlatePosY[i], pch=4, cex=htm@settings@visualisation$heatmap_image_size_cex * htm@settings@visualisation$number_subpositions_x)
        }
      } else {
        print("  no well QC available.")
      }
      
    }
    
    
    # plate QC
    if(!is.null(dat$HTM_qcPlates)) {    
      print("  adding plate QC.")
      if(dat$HTM_qcPlates[1]==0) {
        points(x=mean(xy$x),y=mean(xy$y), pch=4, cex=100) 
      }
    } else {
      print("  no plate QC available.")
    }
    
    
  } # if(markQC)
  
  
} #htmShowHeatmap




htmTreatmentSummary <- function(htm) {
  
  print("")
  print("Treatment Summary:")
  print("******************")
  print("")
  
  if(!(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF") == T) ) {
    
    # get all necessary information
    experiments <- unique(htm@wellSummary$experiment)
    well_z_score <- colnames(htm@wellSummary)[which(grepl("^zScore__",colnames(htm@wellSummary)))]
    well_robust_z_score <- colnames(htm@wellSummary)[which(grepl("^robust_z_score__",colnames(htm@wellSummary)))]  
    wellMinusMeanCtrlScores <- colnames(htm@wellSummary)[which(grepl("^minusMeanCtrl__",colnames(htm@wellSummary)))]
    # todo: make this clean: depends on well Summary method selected
    well_raw_score <- colnames(htm@wellSummary)[which(grepl("^wellscore__",colnames(htm@wellSummary)))]
    wellLogScore <- colnames(htm@wellSummary)[which(grepl("^log2__",colnames(htm@wellSummary)))]
    withinReplMethod <- htmGetListSetting(htm,"statistics","treatmentWithinReplicateSummaryMethod")
    minNumValidReplicates <- htmGetListSetting(htm,"statistics","TreatQC_Minimum_Number_Valid_Replicates")
    treatments <- sort(unique(htm@wellSummary$treatment))
    
  } else { # cell based analysis
    
    # get all necessary information
    data <- htm@objectdata
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    transformation <- htmGetListSetting(htm,"statistics","transformation")
    treatments <- sort(unique(data[[htm@settings@columns$treatment]]))
    
  }
  
  
  
  #ctrls <- htm@settings@ctrlsNeg
  negative_ctrl <- c(htmGetListSetting(htm,"statistics","negativeControl"))
  positive_ctrl <- c(htmGetListSetting(htm,"statistics","positiveControl"))
  
  normalisationMethod <- htmGetListSetting(htm,"statistics","normalisationMethod")
  
  # output
  print("");print("Experiments:")
  print(experiments)
  print("");print(paste("Number of Treatments:",length(treatments)))
  print("");print(paste("Negative Control:",negative_ctrl))
  print("");print(paste("Raw score column:",well_raw_score))
  print("");print(paste("Minus_mean_ctrls score column:",wellMinusMeanCtrlScores))
  
  
  print(""); print("")
  
  # check whether we know everything            
  #if( is.null(experiments) ||
  #  is.null(well_z_score) ||
  #  length(well_z_score) ==0
  #  methods=="None selected" ||
  #  is.na(minNumValidWells) ||
  #  is.null(htm@data[["HTM_qcWells"]]) ||
  #  length(treatments)==0
  #) 
  #{
  #  print("")
  #  print("  ERROR: cannot perform analysis due to lacking information (see above).")
  #  gmessage("Error: see R console output.")
  #  return(htm)
  #}
  
  numEntries = length(treatments)
  
  transformation <- htmGetListSetting(htm,"statistics","transformation")
  
  results <- data.frame(measurement__positions=rep(NA,numEntries),
                        controls=rep(NA,numEntries),  
                        treatment=rep(NA,numEntries),
                        t_test__positions__estimate=rep(NA,numEntries),
                        t_test__positions__p_value=rep(NA,numEntries),
                        t_test__positions__signCode=rep(NA,numEntries),
                        t_test__positions__contributing_replicates=rep(NA,numEntries),
                        #ANOVA_pValue=rep(NA,numEntries),
                        #ANOVA_signCode=rep(NA,numEntries), 
                        #ANOVA_estimate=rep(NA,numEntries), 
                        #ANOVA_std_error=rep(NA,numEntries),
                        median__z_score=rep(NA,numEntries),
                        median__raw_score=rep(NA,numEntries),
                        median__robust_z_score=rep(NA,numEntries),
                        #mean_of_controls=rep(NA,numEntries),
                        measurement__images=rep(NA,numEntries),
                        t_test__images__p_value=rep(NA,numEntries),
                        t_test__images__estimate=rep(NA,numEntries),
                        t_test__images__signCode=rep(NA,numEntries),    
                        mean_objects_per_image=rep(NA,numEntries),
                        #scorename=rep(NA,numEntries),
                        #mean=rep(NA,numEntries),
                        #sem=rep(NA,numEntries),
                        #QC=rep(NA,numEntries),
                        
                        z_score__allBatches=rep(NA,numEntries),
                        z_score__allBatches__per_object=rep(NA,numEntries),
                        
                        robust_z_score__allBatches=rep(NA,numEntries),
                        robust_z_score__allBatches__per_object=rep(NA,numEntries),
                        
                        numObjectsOK=rep(NA,numEntries),
                        numImagesOK=rep(NA,numEntries), 
                        numReplicatesOK=rep(NA,numEntries),
                        numPositionsOK=rep(NA,numEntries),
                        #numPositions=rep(NA,numEntries),
                        stringsAsFactors = FALSE
  )
  
  
  if(!(htmGetListSetting(htm,"statistics","compute_image_based_stats_TF") == T) ) {
    results$measurement__images = NULL
    results$t_test__images__p_value=NULL
    results$t_test__images__estimate=NULL
    results$t_test__images__signCode=NULL
  }
  
  
  if(!(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF") == T) ) {
    results$z_score__allBatches__per_object = NULL
  }
  
  
  ###################################
  # Well based stats
  ###################################
  
  
  if(!(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF") == T) ) {
    
    
    ids_treatments = split(1:nrow(htm@wellSummary), htm@wellSummary$treatment)
    
    i = 0
    res = c(0)
    
    
    print("Analyzing...")
    
    for(ids in ids_treatments) {
      
      wellQCs <- htm@wellSummary[ids,"wellQC"]
      idsOK = ids[ which( (htm@wellSummary[ids,"wellQC"]==1) & !(htm@wellSummary[ids,"experiment"] %in% htmGetVectorSettings("statistics$experiments_to_exclude"))) ] 
      objectsOK <- htm@wellSummary[idsOK,"numObjectsOK"]
      numImagesOK <- htm@wellSummary[idsOK,"numImagesOK"]
      
      if(withinReplMethod=="mean_of_wells") {
        z_scores <- tapply(htm@wellSummary[idsOK,well_z_score],htm@wellSummary$experiment[idsOK],mean)
        raw_scores <- tapply(htm@wellSummary[idsOK,well_raw_score],htm@wellSummary$experiment[idsOK],mean)
        robust_z_scores <- tapply(htm@wellSummary[idsOK,well_robust_z_score],htm@wellSummary$experiment[idsOK],mean)
      } else if(withinReplMethod=="median_of_wells") {  
        z_scores <- tapply(htm@wellSummary[idsOK,well_z_score],htm@wellSummary$experiment[idsOK],median)
        raw_scores <- tapply(htm@wellSummary[idsOK,well_raw_score],htm@wellSummary$experiment[idsOK],median)
        robust_z_scores <- tapply(htm@wellSummary[idsOK,well_robust_z_score],htm@wellSummary$experiment[idsOK],median)    
      }  
      
      # *****************
      # T-test positions
      # *****************
      
      # treatment name
      treat <- htm@wellSummary$treatment[ids[1]]
      
      # init
      t_test__positions__p_value = NA
      t_test__positions__signCode = NA
      t_test__positions__estimate = NA
      t_test__positions__contributing_replicates = NA
      
      z_score__allBatches = NA
      robust_z_score__allBatches = NA
      
      if(!(treat %in% negative_ctrl)) {
        # experiments containing this treatment
        exps <- unique(htm@wellSummary[ids,"experiment"])
        # subset the treatment wells that passed QC and controls that past QC
        d <- subset(htm@wellSummary, (treatment %in% c(treat,negative_ctrl)) 
                    & (experiment %in% exps) 
                    & !(experiment %in% htmGetVectorSettings("statistics$experiments_to_exclude")) 
                    & (wellQC==1), 
                    select = c("treatment","experiment",wellMinusMeanCtrlScores))
        # rearrange the data 
        d$treatment = ifelse(d$treatment %in% negative_ctrl, "control", d$treatment)
        
        t_test__positions__contributing_replicates = paste(unique(d$experiment),collapse=";")
        
        #if(treat=="CCDC15--s36890") {
        #  print(t_test__positions__contributing_replicates) 
        # print("execution stopped")
        #  print(treat)
        #  return(d)
        #}
        
        
        if ( (sum(d$treatment=="control")>1) & (sum(d$treatment==treat)>=1) & (length(exps)>0) ) {
          
          names(d)[names(d)==wellMinusMeanCtrlScores] <- "value"
          
          if(sum(is.na(d$value))>0) {
            print(d)
            print("NA detected")
            fffs
          }
          
          #d$experiment <- as.factor(substr(d$experiment, nchar(d$experiment)-7+1, nchar(d$experiment)))
          d$treatment <- as.factor(d$treatment)
          d$treatment <- relevel( d$treatment, "control" ) # control must be the 1st level for the linear model
          t <- t.test(d$value ~ d$treatment, var.equal=TRUE)
          #print(length(unique(d$exp)))
          nBlocks = length(unique(d$experiment)) 
          n = nrow(d)
          #print(2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 ))
          
          t_test__positions__p_value <- 2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 )
          t_test__positions__estimate <- t$estimate[2]
          t_test__positions__signCode <- ifelse(t_test__positions__p_value<0.001,"***",
                                                ifelse(t_test__positions__p_value<0.01,"**",
                                                       ifelse(t_test__positions__p_value<0.05,"*",
                                                              ifelse(t_test__positions__p_value<0.1,"."," "
                                                              ))))
          
          ctrlValues = subset(d, d$treatment=="control", select = "value" )
          treatedValues = subset(d, d$treatment==treat, select = "value" )
          
          z_score__allBatches = (mean(treatedValues$value) - mean(ctrlValues$value)) / sd(ctrlValues$value)
          robust_z_score__allBatches = (median(treatedValues$value) - median(ctrlValues$value)) / mad(ctrlValues$value)
          
          #mean_of_ctrls <- mean(d$value[d$treatment=="control"])
          #print(d$value[d$treatment=="control"])
          
          #if(0){
          if(grepl("PLK3",treat)) {  #CETN2--s2928
            print(d)
          }
        }
      }
      
      
      i=i+1
      results$treatment[i] <- htm@wellSummary$treatment[ids[1]]
      results$measurement__positions[i] <- wellMinusMeanCtrlScores 
      results$t_test__positions__p_value[i] = t_test__positions__p_value
      results$t_test__positions__signCode[i] = t_test__positions__signCode
      results$t_test__positions__estimate[i] = t_test__positions__estimate
      results$t_test__positions__contributing_replicates[i] = t_test__positions__contributing_replicates
      results$median__z_score[i] <- median(z_scores)
      results$median__raw_score[i] <- median(raw_scores)
      results$median__robust_z_score[i] <- median(robust_z_scores) 
      #results$mean_of_controls[i] <- mean_of_ctrls
      results$controls[i] <- paste(negative_ctrl,collapse=" ")
      
      results$z_score__allBatches[i] <- z_score__allBatches
      results$robust_z_score__allBatches[i] <- robust_z_score__allBatches
      
      results$numObjectsOK[i] <- sum(objectsOK)
      results$numImagesOK[i] <- sum(numImagesOK)
      results$mean_objects_per_image[i] <- sum(objectsOK)/sum(numImagesOK)
      results$numPositionsOK[i] <- sum(wellQCs)
      #results$numPositions[i] <- length(wellQCs)
      results$numReplicatesOK[i] <- length(z_scores)
      
    } # treatment loop
    
  } # position based stats 
  
  
  ########################
  ## object based stats
  ########################
  
  if( htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF") == T ) {
    
    data <- htm@objectdata
    
    htmNormName = "HTM_Norm"
    
    # select log2 data in case data transformation is selected
    if(transformation == "log2") {
      measurement_minusMeanCtrl = paste(htmNormName,measurement,"log2","minusMeanCtrl",sep="__")    
    } else {
      measurement_minusMeanCtrl = paste(htmNormName,measurement,"minusMeanCtrl",sep="__")    
    }
    
    print("Computing object based statistics...")
    print("")
    print(measurement_minusMeanCtrl)
    
    
    ids_treatments = split(1:nrow(data), data[[htm@settings@columns$treatment]])
    
    i=0
    
    for(ids in ids_treatments) {
      
      # ********************************
      # T-test objects 
      # ********************************
      
      # treatment name
      treat <- data[ids[1],htm@settings@columns$treatment]
      
      # init
      t_test__p_value = NA
      t_test__signCode = NA
      t_test__estimate = NA
      z_score__allBatches__per_object = NA
      robust_z_score__allBatches__per_object = NA
      
      # compute
      if(!(treat %in% negative_ctrl)) {
        
        # experiments containing this treatment
        exps <- unique(data[ids,htm@settings@columns$experiment])
        #print(exps)
        #print("subset the treatment images that passed QC and controls that past QC")
        #print(measurement_minusMeanCtrl)
        d <- subset(data, 
                    (data[[htm@settings@columns$treatment]] %in% c(treat,negative_ctrl))
                    & (data[[htm@settings@columns$experiment]] %in% exps) 
                    & !(data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")) 
                    & (data[["HTM_qcObjects"]]==1), 
                    select = c(htm@settings@columns$treatment,measurement_minusMeanCtrl,htm@settings@columns$experiment))
        names(d)[names(d)==measurement_minusMeanCtrl] <- "value"
        names(d)[names(d)==htm@settings@columns$treatment] <- "treatment"
        names(d)[names(d)==htm@settings@columns$experiment] <- "experiment"
        d$treatment = ifelse(d$treatment %in% negative_ctrl, "control", d$treatment)
        
        if ( (sum(d$treatment=="control")>1) & (sum(d$treatment==treat)>1) ) {
          
          #d$experiment <- as.factor(substr(d$experiment, nchar(d$experiment)-7+1, nchar(d$experiment)))
          d$treatment <- as.factor(d$treatment)
          d$treatment <- relevel( d$treatment, "control" ) # control must be the 1st level for the linear model
          t <- t.test(d$value ~ d$treatment)  # as there typically is enough data no equal variance is assumed
          
          nBlocks = length(unique(d$experiment)) 
          n = nrow(d)
          #print(2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 ))
          
          t_test__p_value <- 2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 )
          t_test__estimate <- t$estimate[2]
          t_test__signCode <- ifelse(t_test__p_value<0.001,"***",
                                     ifelse(t_test__p_value<0.01,"**",
                                            ifelse(t_test__p_value<0.05,"*",
                                                   ifelse(t_test__p_value<0.1,"."," "
                                                   ))))
          
          
          ctrlValues = subset(d, d$treatment=="control", select = "value" )
          treatedValues = subset(d, d$treatment==treat, select = "value" )
          
          z_score__allBatches__per_object = (mean(treatedValues$value) - mean(ctrlValues$value)) / sd(ctrlValues$value)
          robust_z_score__allBatches__per_object = (median(treatedValues$value) - median(ctrlValues$value)) / mad(ctrlValues$value)
          
        } 
        
      } # if not negative control
      
      
      #print(treat)
      #print(t_test__p_value)
      #print(t_test__estimate)
      
      i = i + 1
      results$treatment[i] <- treat
      results$measurement__objects[i] = measurement_minusMeanCtrl
      results$t_test__objects__p_value[i] = t_test__p_value
      results$t_test__objects__signCode[i] = t_test__signCode
      results$t_test__objects__estimate[i] = t_test__estimate
      results$z_score__allBatches__per_object[i] = z_score__allBatches__per_object
      results$robust_z_score__allBatches__per_object[i] = robust_z_score__allBatches__per_object
      
    }  # treatment loop   
    
  }  # object based stats
  
  
  
  ## image based stats
  
  if(F) { #if( htmGetListSetting(htm,"statistics","compute_image_based_stats_TF") == T ) {
    
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    transformation <- htmGetListSetting(htm,"statistics","transformation")
    
    # select log2 data in case data transformation is selected
    if(transformation == "log2") {
      measurement_minusMeanCtrl = paste(measurement,"log2","minusMeanCtrl",sep="__")    
    } else {
      measurement_minusMeanCtrl = paste(measurement,"minusMeanCtrl",sep="__")    
    }
    
    
    #for(wellscore in wellscores) {
    print("Computing sub-position (image) based statistics...")
    
    ids_treatments = split(1:nrow(htm@data), htm@data[[htm@settings@columns$treatment]])
    
    for(ids in ids_treatments) {
      
      # ********************************
      # T-test images 
      # ********************************
      
      
      # treatment name
      treat <- htm@data[ids[1],htm@settings@columns$treatment]
      #print(treat)
      #print(negative_ctrl)
      if(treat %in% negative_ctrl) {
        t_test__images__p_value = NA
        t_test__images__signCode = NA
        t_test__images__estimate = NA
      } else {  
        # experiments containing this treatment
        exps <- unique(htm@data[ids,htm@settings@columns$experiment])
        #print(exps)
        #print("subset the treatment images that passed QC and controls that past QC")
        #print(measurement_minusMeanCtrl)
        d <- subset(htm@data, 
                    (htm@data[[htm@settings@columns$treatment]] %in% c(treat,negative_ctrl))
                    & (htm@data[[htm@settings@columns$experiment]] %in% exps) 
                    & !(htm@data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")) 
                    & (htm@data[["HTM_qcImages"]]==1), 
                    select = c(htm@settings@columns$treatment,measurement_minusMeanCtrl,htm@settings@columns$experiment))
        names(d)[names(d)==measurement_minusMeanCtrl] <- "value"
        names(d)[names(d)==htm@settings@columns$treatment] <- "treatment"
        names(d)[names(d)==htm@settings@columns$experiment] <- "experiment"
        d$treatment = ifelse(d$treatment %in% negative_ctrl, "control", d$treatment)
        
        if ( (sum(d$treatment=="control")>1) & (sum(d$treatment==treat)>1) ) {
          
          #d$experiment <- as.factor(substr(d$experiment, nchar(d$experiment)-7+1, nchar(d$experiment)))
          d$treatment <- as.factor(d$treatment)
          d$treatment <- relevel( d$treatment, "control" ) # control must be the 1st level for the linear model
          t <- t.test(d$value ~ d$treatment)  # as there typically is enough data no equal variance is assumed
          
          nBlocks = length(unique(d$experiment)) 
          n = nrow(d)
          #print(2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 ))
          
          t_test__images__p_value <- 2-2*pt(abs(t$statistic), df = n - (nBlocks-1) - 2 )
          t_test__images__estimate <- t$estimate[2]
          t_test__images__signCode <- ifelse(t_test__images__p_value<0.001,"***",
                                             ifelse(t_test__images__p_value<0.01,"**",
                                                    ifelse(t_test__images__p_value<0.05,"*",
                                                           ifelse(t_test__images__p_value<0.1,"."," "
                                                           ))))
          
          #if(0){
          if(F & treat=="s36721--CEP76") {  #CETN2--s2928
            print(t)
            print(n)
            print(n - (nBlocks-1) - 2)
            print(t_test__p_value)
            return(t)
            dt <- d
          }
        } else {
          t_test__images__p_value = NA
          t_test__images__signCode = NA
          t_test__images__estimate = NA
        }
      }
      
      #print(treat)
      #print(t_test__images__p_value)
      #print(t_test__images__estimate)
      
      i = which(results$treatment == treat)
      results$measurement__images[i] = measurement_minusMeanCtrl
      results$t_test__images__p_value[i] = t_test__images__p_value
      results$t_test__images__signCode[i] = t_test__images__signCode
      results$t_test__images__estimate[i] = t_test__images__estimate
      
      
      
    } # image loop
    
  } # treatment loop
  
  print("")
  print("done. created Treatment Summary Table.")
  
  
  # Diagnostics 
  
  #hist(res)
  #dev.new()
  #qqnorm(res)
  #qqline(res)
  #print(wellScoreForANOVA)
  
  # display controls plot: raw well scores
  #print("Plot control scores...")
  #htmJitterplot(htm, cx="experiment", cy=well_raw_score, .ylab=well_raw_score, datatype="wells", 
  #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #              showMedian = F, showMean = T)
  
  # display controls plot: batch corrected well scores
  #print("Plot batch corrected control scores...")
  #htmJitterplot(htm, cx="experiment", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
  #              treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #              showMedian = F, showMean = T)
  
  
  # save controls plot
  #  htmJitterplot(htm, cx="experiment", cy=wellScoreForANOVA, .ylab=wellScoreForANOVA, datatype="wells", 
  #                treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"),
  #                showMedian = F, showMean = T, save2file = T, newdev = F)
  
  # save histogram as plot
  #  print(paste("histo:",wellMinusMeanCtrlScores))
  #  htmHisto(wellMinusMeanCtrlScores, datatype="wells", treatmentSubset = htmGetListSetting(htm,"statistics","negativeControl"), save2file=T)
  
  #htm@wellSummary$exp_treat = paste(htm@wellSummary$experiment,htm@wellSummary$treatment,sep="_")
  #edit(htm@wellSummary$exp_treat)
  #print(wellMinusMeanCtrlScores)
  # todo: plot only if makes sense
  #htmJitterplot(htm, cx="exp_treat", cy=wellMinusMeanCtrlScores, .ylab=wellMinusMeanCtrlScores, datatype="wells", 
  #                treatmentSubset = c(htmGetListSetting(htm,"statistics","negativeControl"),htmGetListSetting(htm,"statistics","positiveControl")),
  #                showMedian = F, showMean = T, save2file = F, newdev = F)
  
  
  #####
  
  if(positive_ctrl != "None selected") {
    
    print("")
    print("")
    print("Checking how well positive and negative controls are separated in each batch...")
    print(paste("Measurement:",wellMinusMeanCtrlScores))
    print(paste("Positive control:",positive_ctrl))
    print(paste("Negative control:",negative_ctrl))
    print("")
    
    z_scores = c()
    
    for(exp in experiments) {
      
      #print(exp)
      d <- subset(htm@wellSummary, (experiment==exp) & (wellQC==1), select = c("treatment",wellMinusMeanCtrlScores))
      names(d)[names(d)==wellMinusMeanCtrlScores] <- "value"
      
      d_neg <- subset(d,d$treatment == negative_ctrl)
      mean_neg = mean(d_neg$value)
      sd_neg = sd(d_neg$value)
      n_neg = length(d_neg$value)
      
      d_pos <- subset(d,d$treatment == positive_ctrl)
      mean_pos = mean(d_pos$value)
      sd_pos = sd(d_pos$value)
      n_pos = length(d_pos$value)
      
      #print(paste(min_neg,max_neg,mean_pos,sd_pos))
      #probability_of_pos_outside_neg = 1 - integrate( function(x) {dnorm(x,mean=mean_pos,sd=sd_pos)}, min_neg, max_neg)$value
      z_score = (mean_pos-mean_neg) / sd_neg
      z_scores = c(z_scores, z_score)
      #t_value = (mean_pos-mean_neg) / sqrt(sd_pos^2+sd_neg^2)
      
      
      #print(paste0(exp,"  N_neg: ",n_neg,"  N_pos: ",n_pos,"  Probability: ",round(probability_of_pos_outside_neg,3))) #,"  t-value: ",t_value))
      #quality = ifelse(abs(z_score)<1,"XX",
      #                 ifelse(abs(z_score)<2,"X",""
      #                               ))
      quality = ""
      
      if ( exp %in% htmGetVectorSettings("statistics$experiments_to_exclude") ) {
        comment = "(Excluded)  " 
      } else {
        comment = ""
      }
      
      print(paste0(comment,exp,"  N_neg: ",n_neg,"  N_pos: ",n_pos,"  z-score of positive controls: ",round(z_score,3)," ",quality)) #,"  t-value: ",t_value))
      
    }
  }
  
  print("")
  print(paste("Mean z-score",mean(z_scores, na.rm=T)))
  print(paste("SD z-score",sd(z_scores, na.rm=T)))
  print(paste("N z-score",length(z_scores)))
  
  
  
  
  # sorted hit-list with significance level * ** ***
  # plot colored according to significance level
  #with hits marked as sign
  
  results_ordered <- results[order(results$t_test__positions__p_value),]
  return(results_ordered)
  
}

