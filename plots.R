#
# Small functions
#


scaleForDisplay <- function(values,lut_min,lut_max){
    v = 255 * (values-as.numeric(lut_min) )/(as.numeric(lut_max)-as.numeric(lut_min) ) + 1
    toosmall = which(v<1)
    toolarge = which(v>255)
    v[toosmall]=1
    v[toolarge]=256
    return(v)
}

colorbar <- function(lut, min, max, nticks=11, ticks=seq(min, max, len=nticks), newdevice = T) {
    
    min = as.numeric(min)
    max = as.numeric(max)
    print ("updating color bar")
    print (min)
    print (max)
    
    scale = (length(lut)-1)/(max-min)
    
    
    #if(devColorBar == -1) {
    #  dev.new(width=1.75, height=5)
    #} else {
    #  dev.set(devColorBar)
    #}
    #devColorBar <<- dev.cur()
    #op <- par(mar = c(5,6,1,1) + 0.1) 
    
    
    if(newdevice) {
        currentdev = dev.cur()  # should be the plateview
        dev.new(width=1.75, height=5)
    }
    # todo: name of measurement
    plot(c(0,10), c(min,max), type='n', bty='n', xaxt='n', ylab='', yaxt='n', xlab='', main='')
    axis(2, round(ticks,2), las=1)  # rounding of the labels
    stepsize = 10
    for (i in seq(1,(length(lut)-1), by=stepsize) ) {
        y = (i-1)/scale + min
        rect(0, y - 0.5*(stepsize/scale), 10, y +  0.5*(stepsize/scale), col=lut[i], border=NA)
    }
    #par(op) 
    
    if(newdevice) {
        dev.set(currentdev)
    }
    
}


#
# Image Viewing
#

htmShowDataFromRow <- function(htm,data,irs,appendCommand=""){
  
  if(htmGetListSetting(htm,"visualisation","viewImages") == "None selected") {
    print("")
    print("No image selected for viewing; see [Configure > Configure visualisation settings]")
    return(0)
  }
  
  filenamePrefix = htm@settings@visualisation$image_filename_prefix
  foldernamePrefix = htm@settings@visualisation$image_foldername_prefix
  
  # convert to forward slashes immediately, otherwise gsub has problems later
  rootFolderTable = gsub("\\\\" ,"/",htmGetListSetting(htm,"visualisation","image_root_foldername_in_table"))
  rootFolderReal = gsub("\\\\" ,"/",htmGetListSetting(htm,"visualisation","image_root_foldername_on_this_computer"))  
  
  if( .Platform$OS.type == "unix" ) {
    #imageViewerCMD = "/Applications/Fiji.app/Contents/MacOS/fiji-macosx --no-splash"
    imageViewerCMD = '/Applications/Fiji.app/Contents/MacOS/ImageJ-macosx -debug '
  } else if( .Platform$OS.type == "windows" ) {
    imageViewerCMD = paste("\"",htm@settings@visualisation$windows_path_to_fiji,"\""," -debug ",sep="")
    #imageViewerCMD = "\"c:\\Program Files\\FIJI\\fiji-win64.exe\" --no-splash -macro"
  } else {
    imageViewerCMD = "unkown"
  }
  
  cmd = ""
  
  # check that the columns exist in general
  filename_found = F
  foldername_found = F
  for ( colname in colnames(data) ) {
    if( grepl(filenamePrefix,colname) ) {
      filename_found = T
      }
    if( grepl(foldernamePrefix,colname) ) {
      foldername_found = T
      }
    }
  
  if ( (filename_found != T) || (foldername_found != T) ) {
    gmessage("File- or Foldername columns not found. Please re-configure: [Configure > Configure Visualisation Settings]")
    return(NULL)
    }
  
  
  for ( ir in irs) {
 
    for ( colname in colnames(data) ) {
      
      if( foldernamePrefix != "" ) {  
        
        # construct pathname from file- and folder-name
        if( grepl(filenamePrefix, colname) ) {
          
          imagename = strsplit(colname,filenamePrefix)[[1]][2]
          filename = data[[colname]][ir]
          cFolder = paste(foldernamePrefix,imagename,sep="")
          #print(cFolder)
          foldername = gsub("\\\\" ,"/",data[[cFolder]][ir])
          foldername = gsub(rootFolderTable, rootFolderReal, foldername)
          
          pathname = paste(foldername,"/",filename,sep="")
          
          if(!grepl("^/", pathname)) {
            pathname = paste0("/",pathname)
          }
          
          if(imagename %in% htmGetListSetting(htm,"visualisation","viewImages")) {
            #cmd = paste(cmd, paste0(' -eval \"open(\'',pathname,'\')\"') )
            cmd = paste(cmd, paste0('open(\'',pathname,'\');\n') )
            
          }
          
          
        } # from file- and folder-name
        
      } else {
        
        # directly get the pathname=filename
        if( grepl(filenamePrefix, colname) ) {
          
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
            
           
            images = c(images, pathname)
            
            if(("Location_Center_X" %in% names(data)) && ("Location_Center_Y" %in% names(data))) {
              locX = c(locX, round(data$Location_Center_X[ir]))
              locY = c(locY, round(data$Location_Center_Y[ir]))
            } else {
              locX = c(locX, 0)
              locY = c(locY, 0)
            }
            
            #print(pathname)
          }      
          
        }
        
      } # from pathname
      
    } # colname loop (within one row)
    
    # make stack
    #cmd = paste(cmd, paste0(' -eval \"run(\'Images to Stack\');\"'))
    # make composite
    #cmd = paste(cmd, paste0(' -eval \"run(\'Make Composite\', \'display=Composite\');\"'));  
    
    # autoscale display
    #cmd = paste(cmd, paste0(' -eval \"run(\'Enhance Contrast\', \'saturated=0.01\')\"'))
    
    if(("Location_Center_X" %in% names(data)) && ("Location_Center_Y" %in% names(data))) {
      #cmd = paste(cmd,' -eval \"makePoint(',round(data$Location_Center_X[ir]),',',round(data$Location_Center_Y[ir]),');\"')
      x <- round(data$Location_Center_X[ir])
      y <- round(data$Location_Center_Y[ir])
      #cmd = paste(cmd, paste0(' -eval \"makeRectangle(',x-50,',',y-50,',',100,',',100,');\"'))
      #cmd = paste(cmd, paste0(' -eval \"run(\'Draw\', \'slice\');\"'))
      cmd = paste(cmd, paste0('makeRectangle(',x-50,',',y-50,',',100,',',100,');\n'))
      cmd = paste(cmd, paste0('run(\'Draw\', \'slice\');\n'))
      
      }
    
    
  } # row loop
  
  if(length(irs)>1) {
    # make stack
    #cmd = paste(cmd, paste0(' -eval \"run(\'Images to Stack\');\"'))
    cmd = paste(cmd, paste0('run(\'Images to Stack\');\n'))
    
  }
  
  tmp_path = paste0(htmPath,"/tmp") 
  dir.create(tmp_path, showWarnings = FALSE)
  tmp_file = paste0(tmp_path,"/open.ijm");
  write(cmd, file = tmp_file);
  
  if ( .Platform$OS.type == "unix" ) {
    tmp_file = gsub("\\\\" ,"/", tmp_file)
  } else if ( .Platform$OS.type == "windows" ) {
    tmp_file = gsub("/", "\\\\", tmp_file)
  }
  
  full_cmd = paste(imageViewerCMD,'-macro',paste0('\"',tmp_file,'\"'));
  print(full_cmd)
  system(full_cmd,wait=F)
  
  
}

htmShowWellsFromRow <- function(htm,data,ir){
  
  print("Click and View:")
  for(colname in colnames(data)) {
    print(paste("  ",colname,"=",data[[colname]][ir]))
  }   
  
}

#
# Heatmap
#


htmShowHeatmapData <- function(htm, selectedExp="", selectedMeasurement, markQC = T, width = htm@settings@visualisation$heatmap_width, 
                               colorLUT.autoscale = F, colorLUT.min = 0, colorLUT.max = 100, newdevice = T, 
                               show_gradient_correction = F, action="plot")  {
    
    
    #
    # CHECK
    #
    
    if( is.null(htm@settings@columns$wellnum) || !(htm@settings@columns$wellnum %in% colnames(htm@data)) )  {
        gmessage("Error: Position/Well column not found.")
        return(NULL)
    }
    if( is.null(htm@settings@columns$posnum) || !(htm@settings@columns$posnum %in% colnames(htm@data)) )  {
        gmessage("Error: Sub-position column not found.")
        return(NULL)
    }
    if( is.null(htm@settings@columns$experiment) ||!(htm@settings@columns$experiment %in% colnames(htm@data)) )  {
        gmessage("Error: Experimental batch column not found.")
        return(NULL)
    }
    if( is.null(htm@settings@columns$treatment) ||!(htm@settings@columns$treatment %in% colnames(htm@data)) )  {
        gmessage("Error: Treatment column not found.")
        return(NULL)
    }
    
    #
    # Get data
    #
    
    dat <- subset(htm@data,(htm@data[[htm@settings@columns$experiment]]==selectedExp))
    dat[[selectedMeasurement]] = as.numeric( dat[[selectedMeasurement]] )
    dat[[htm@settings@columns$posnum]] = as.numeric( dat[[htm@settings@columns$posnum]] )
    if(!any(grepl("(?i)[A-Z]", dat[[htm@settings@columns$wellnum]]))) {
        dat[[htm@settings@columns$wellnum]] = as.numeric( dat[[htm@settings@columns$wellnum]] )
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
    
    
    #par(mar=c(5.1,6.1,6.1,2.1))
    
    xy = htm_convert_wellNum_posNum_to_xy(dat[[htm@settings@columns$wellnum]],dat[[htm@settings@columns$posnum]])
    
    if(action=="plot") {  
        
        if(newdevice) { 
            dev.new(width = 1.5 * width, height =  plate.nrow / plate.ncol * width)
        } 
        
        op <- par(bg = "grey")
        
        plot(x = xy$x, y = xy$y, col=colpal[dat$val], 
             ylim = rev(range(xy$y)),  
             xlim = round(range(xy$x)),
             xaxt = "n", yaxt = "n", xaxs = "i",
             pch = 15, cex= htm@settings@visualisation$heatmap_image_size_cex, xlab="", ylab="",
             asp = 1
        )
        title(paste(selectedExp,selectedMeasurement,sep="\n"),cex.main=1)
        
        axis(1, at=1:plate.ncol, labels=1:plate.ncol, las=1,  cex.axis = 1)
        axis(2, at=1:plate.nrow, labels=LETTERS[1:plate.nrow], las=2, cex.axis = 1)
        
        par(op)
        
        
        #todo: change qcImages to ImageQC
        
        if(markQC) {
            
            if(!is.null(dat$HTM_qc)) {    
                print("  adding image QC.")
                ids <- which(dat$HTM_qc==0)
                x <- vector(length=length(ids))
                y <- vector(length=length(ids))
                for (i in seq(1,length(ids))) {
                    x[i] <- xy$x[ids[i]]
                    y[i] <- xy$y[ids[i]]
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
            
            
            
            # plate QC
            if(!is.null(dat$HTM_qcPlates)) {    
                print("  adding plate QC.")
                if(dat$HTM_qcPlates[1]==0) {
                    points(x=mean(xy$x),y=mean(xy$y), pch=4, cex=100) 
                }
            } else {
                print("  no plate QC available.")
            }
            
            
        } # if markQC
        
    } # if action=="plot"
    
    
    if (action=="click") {
        print("please select an image for viewing!")
        i <- identify(xy$x, xy$y, n = 1, plot = FALSE)
        print("       ")
        print(paste("experiment =", dat[[htm@settings@columns$experiment]][i]))
        print(paste("treatment =", dat[[htm@settings@columns$treatment]][i]))
        print(paste(selectedMeasurement,"=", dat[[selectedMeasurement]][i]))
        print("       ")
        htmShowDataFromRow(htm,dat,i)
    } 
    
    
} #htmShowHeatmapData


htmMakeAllHeatmaps <- function(htm, datatype = "images", measurement, path, markQC = T, colorLUT.min=0, colorLUT.max=1, colorLUT.autoscale=F, selectTreat1, selectTreat2, selectTreat3) {
  
  experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  print("");print("Experiments:")
  print(experiments)
  print(paste("Plot width:",htm@settings@visualisation$heatmap_width))
  #print(paste("Plot height:",htm@settings@visualisation$heatmap_height))
              
  # check experiments we know everything            
  if( is.null(experiments) |
      is.na(htm@settings@visualisation$heatmap_width) 
      #is.na(htm@settings@visualisation$heatmap_height)   
    ) {
    print("")
    print("  ERROR: cannot perform job due to lacking information (see above).")
    return(NULL)
  }

  pdf(file = path, 
      width = htm@settings@visualisation$heatmap_width,
      height = htm@settings@visualisation$number_positions_y / htm@settings@visualisation$number_positions_x * htm@settings@visualisation$heatmap_width
  )
  

  # plot color bar in front
  if(!(colorLUT.autoscale==T)) {
    colorbar(colorRampPalette(c("blue","white","red"))(255), min=colorLUT.min, max=colorLUT.max, newdevice=F)
  }
  
  
  for(experiment in experiments) {
    print("")
    print(paste("Experiment :",experiment))
    
    htmShowHeatmap(htm=htm,
                   datatype = datatype,
                   selectedExp = experiment,
                   selectedMeasurement = measurement,
                   markQC = markQC, 
                   colorLUT.autoscale = colorLUT.autoscale,
                   colorLUT.min = colorLUT.min,
                   colorLUT.max = colorLUT.max,
                   newdevice = F)
    
     
    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp=experiment, 
                                     selectedTreatment=selectTreat1,
                                     color = "black")
    
    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp=experiment, 
                                     selectedTreatment=selectTreat2,
                                     color = "white")

    htmHeatmap_MarkSelectedTreatment(htm=htm, 
                                     selectedExp=experiment, 
                                     selectedTreatment=selectTreat3,
                                     color = "green")
    
  } # exp
  
 
  dev.off()
  
}

htmHeatmap_MarkSelectedTreatment <- function(htm, selectedExp, selectedTreatment, color="black") {
  
  print(paste("selected treatment = ",selectedTreatment))

  if(!selectedTreatment=="None selected") {
    
    dat <- subset(htm@data,htm@data[[htm@settings@columns$experiment]]==selectedExp);
    
    ids = which(dat[[htm@settings@columns$treatment]]==selectedTreatment)
    
    n = length(ids)
    
    if(n>0) {
      
      x <- vector(length=n)
      y <- vector(length=n)
    
      xy = htm_convert_wellNum_to_xy(dat[[htm@settings@columns$wellnum]])
      
      
      for (i in seq(n)) {
        x[i] <- xy$x[ids[i]]
        y[i] <- xy$y[ids[i]]
        }
      
      print("mark")
      
      points(x, y, 
             pch = 22,
             lwd = 2.25,
             cex = 1.1 * htm@settings@visualisation$heatmap_image_size_cex * max(c(htm@settings@visualisation$number_subpositions_x,htm@settings@visualisation$number_subpositions_y)),
             col = color) #22 = empty rectangle
    }
  
  }
  
}

#
# Jitter-plot
#

htmJitterplot_Data <- function(htm=htm, cx, cy, .xlab="", .ylab="", treatmentSubset = "None selected", 
                          .xlim=NA, .ylim=NA, colorizeTreatments=F,  
                          sorting="None selected", experimentSubset="None selected", newdev = T, 
                          action="plot", printMeanSD = T,  showMean = T, showMedian = T, save2file = F,
                          scaleFromZero = F, reference="None selected") {
  
  print("")
  print("")
  print("")
  print("")
  print("Jitter Plot:")
  print(paste("  x =",cx))
  print(paste("  y =",cy))
  
  if("All treatments" %in% treatmentSubset) { 
    treatmentSubset = "None selected"
  }
  
  
  # subsample
  if(is.numeric(htmGetListSetting(htm,"visualisation","jitterPlot_subsample",gui=T))) {
    every_nth = htmGetListSetting(htm,"visualisation","jitterPlot_subsample",gui=T)
    data <- htm@data[seq(1,nrow(htm@data),every_nth), ]
  } else {
    data <- htm@data
  }
  
  if(experimentSubset[1] != "None selected") {
    print(paste("  selecting experiments:",experimentSubset))
    data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
  }
  if(treatmentSubset[1] != "None selected") {
    print(paste("  selecting treatments:",treatmentSubset))
    data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
  }
  
  if(!is.null(htm@settings@columns$treatment)) treatments <- data[[htm@settings@columns$treatment]]
  
  qc <- data$HTM_qc
  if(!is.null(qc)) {
    print("  qc column exists")
  } else {
    print("  no qc column => evaluate all data")
    qc = rep(1, nrow(data))
  }
  
  print(paste("  sorting:",sorting))
  if(sorting=="None selected") {
    ids = 1:nrow(data)
  }
  if(sorting=="alphabetic") {
    print("  applying alphabetic sorting")
    #print(factor(data[[cx]]))
    ids <- order(data[[cx]],na.last=NA)
  }
  if(sorting=="median value") {
    print("  applying median sorting")
    #print(unique(factor(data[[cx]])))
    ids_cx = split(1:nrow(data),data[[cx]])
    data$medians = rep(NA,nrow(data))
    for(ids in ids_cx) {    
      #values = subset(data[ids,],qc[ids]==1,select=cy)[[1]]
      #print(values)
      if(!is.null(qc)) {
        values = data[ids,cy][which(qc[ids]==1)]
      } else {
        values = data[ids,cy]
      }
      data$medians[ids] = rep(median(values, na.rm = T),length(ids))
    }
    ids <- order(data$medians,na.last=NA)
  }
  
  if(sorting=="mean value") {
    print("  applying mean sorting")
    #print(unique(factor(data[[cx]])))
    ids_cx = split(1:nrow(data),data[[cx]])
    data$means = rep(NA,nrow(data))
    for(ids in ids_cx) {    
      #values = subset(data[ids,],qc[ids]==1,select=cy)[[1]]
      #print(values)
      if(!is.null(qc)) {
        values = data[ids,cy][which(qc[ids]==1)]
      } else {
        values = data[ids,cy]
      }
      data$means[ids] = rep(mean(values, na.rm = T),length(ids))
    }
    ids <- order(data$means,na.last=NA)
  }
  
  
  # add jitter to sorted data
  print("  prepare plotting..")
  factors <- factor(data[ids,cx], levels=unique(data[ids,cx])); #print(factors)
  
  if(action=="plot") {
    print("  adding jitter")
    jp.x <- jitter(as.numeric(factors),htm@settings@visualisation$jitter)
    htm <- get("htm", envir = globalenv()) # remember for click&view
    htm@settings@visualisation$jp.x <- jp.x 
    assign("htm", htm, envir = globalenv())
  }
  
  if(action=="click") {
    print("  taking stored jitter")
    htm <- get("htm", envir = globalenv())
    jp.x <- htm@settings@visualisation$jp.x 
  }
  
  lx = levels(factors);
  jp.y <- data[ids,cy] 
  qc <- qc[ids]
  
  # log2 transform if wished
  if(htmGetListSetting(htm,"visualisation","jitterPlot_log2_TF")==T) {
    jp.y <- log2(jp.y)
    .ylab <- paste(.ylab,"[log2]")
  }
  
  
  print(paste("  colorizeTreatments:",colorizeTreatments==T))
  if(colorizeTreatments == T) {
    treatments <- treatments[ids]
    print("  colorizing by treatment")
    .colors = factor(treatments, levels=unique(treatments)) 
    n = length(levels(.colors))
    if(n<=8) {
      col_pal <- brewer.pal(8,"Set2")
    } else {
      col_pal <- rainbow(n)
    }
    .colors = col_pal[factor(.colors)]
    
  } else {
    .colors = rep("grey30", nrow(data))
  }  
  
  
  # compute some statistics for printing and plotting
  jp.y.qc <- ifelse(qc==1, jp.y, NA)
  jp.x.qc <- ifelse(qc==1, jp.x, NA)
  
  
  # compute stats for plotting
  jp.xMean = tapply(jp.x.qc, factors, function(z) {mean(z,na.rm=T)})
  jp.yMean = tapply(jp.y.qc, factors, function(z) {mean(z,na.rm=T)})
  jp.ySD = tapply(jp.y.qc, factors, function(z) {sd(z,na.rm=T)})
  jp.yMedian = tapply(jp.y.qc, factors, function(z) {median(z,na.rm=T)})
  jp.yMAD = tapply(jp.y.qc, factors, function(z) {mad(z,na.rm=T)})
  jp.ySEM = tapply(jp.y.qc, factors, function(z) {sem(z)})
  jp.ySEMabove = tapply(jp.y.qc, factors, function(z) {sem_above(z)})
  jp.ySEMbelow = tapply(jp.y.qc, factors, function(z) {sem_below(z)})
  jp.nOK = tapply(jp.y.qc, factors, function(z) {sum(z/z, na.rm=T)})
  jp.nAll = tapply(jp.y, factors, function(z) {length(z)})
  
  
  # compute t-tests
  if(reference != "None selected") {
    print("")
    print("### T-Test:")
    
    factors <- factor(data[ids,cx], levels=unique(data[ids,cx])); #print(factors)
    idsRef = which(factors==reference)
    
    if(sum(idsRef)>0) {
      # print(paste("Reference:",reference))
      print("")
      m.ctrl = mean(jp.y.qc[idsRef],na.rm=T)
      sd.ctrl = sd(jp.y.qc[idsRef],na.rm=T)
      median.ctrl = median(jp.y.qc[idsRef],na.rm=T)
      mad.ctrl = mad(jp.y.qc[idsRef],na.rm=T)
      
      #print(jp.y.qc[idsRef])
      
      print(paste(reference," mean =",m.ctrl," sd =",sd.ctrl))
      print("")
      
      for(factor in levels(factors)) {
        #if(!factor==reference) {
        idsTest = which(factors==factor)
        tt <- t.test(x=jp.y.qc[idsRef],y=jp.y.qc[idsTest])
        m.test = mean(jp.y.qc[idsTest],na.rm=T)
        sd.test = sd(jp.y.qc[idsTest],na.rm=T)
        #median.test = median(jp.y.qc[idsTest],na.rm=T)
        #mad.test = mad(jp.y.qc[idsTest],na.rm=T)
        #zFactor = round(1 - 3*(sd.ctrl+sd.test)/abs(m.ctrl-m.test),2)
        #zScore = round(abs(m.test-m.ctrl)/sd.ctrl,2)
        #robust_zScore = round(abs(median.test-median.ctrl)/mad.ctrl,2)
        significance = ""
        if(tt$p.value < 0.05) significance = "(*)"
        if(tt$p.value < 0.01) significance = "(**)"
        #print(paste(factor,"; Z-score = ",zScore,"; Robust Z-score = ",robust_zScore,"; Z-factor = ",zFactor,"; T-test = ",round(tt$p.value,4),significance))  
        #print(paste(factor,"; T-test = ",round(tt$p.value,10),significance))  
        #print(paste(factor," mean =",m.ctrl," sd =",sd.ctrl))
        print(paste(factor,"  mean =",m.test,"  t-test vs.",reference,"=",tt$p.value,significance))  
        #print(jp.y.qc[idsTest])
        
        #}
      }
    } else {
      print(paste("no reference treatments found for:",reference))
    } 
  } else {
    #print("no valid Reference selected.")
  }
  
  
  
  #
  # QC
  #
  if(htmGetListSetting(htm,"visualisation","plotting_showQCfailData_TF")==T) {  # label data points that did not pass QC
    pchQC = ifelse(qc==1, 16, 4)
  } else {  # remove data points that did not pass QC
    vxTmp = jp.x[which(qc==1)]
    vyTmp = jp.y[which(qc==1)]
    colorsTmp = .colors[which(qc==1)]
    print(length(ids))
    idsTmp = ids[which(qc==1)]
    ids = idsTmp
    print(length(ids))
    .colors = colorsTmp
    jp.x = vxTmp
    jp.y = vyTmp
    pchQC = rep(16, length(jp.x))
    print(paste("removed data points due to QC",sum(qc==0)))
  }
  
  
  if(action=="plot") {
    
    if(save2file) {
      path = gfile("Save as...", type="save", initialfilename = paste0("Controls_per_experiment--",htmGetListSetting(htm,"statistics","transformation"),"--",htmGetListSetting(htm,"statistics","measurement"),".pdf"))
      pdf(file=path)
      print(path)
    } else if(newdev){
      dev.new()
    }
    
    #cat(paste("\n\njitter-plotting",cy,"vs.",cx,"\n"))
    
    op <- par(mar=c(12.1, 4.1, 4.1, 2.1), xpd=TRUE) #par(mar = c(8,5,4,2) + 0.1) 
    
    dotsize = 0.5
    
    #pchQC = '.'
    
    if( is.na(.xlim) || is.na(.ylim) ) {
      if(htmGetListSetting(htm,"visualisation","jitterPlot_scaleFromZero_TF")==T) {
        print(paste("  scaling y-axis from 0 to",max(jp.y,na.rm=T)))
        plot(jp.x, jp.y, type = "p", xaxt = "n", xlab=.xlab, ylab=.ylab, ylim = c(0, max(jp.y,na.rm=T)), pch=pchQC, cex=dotsize, col = .colors,  cex.lab = 1)
      } else {
        print(unique(pchQC))
        plot(jp.x, jp.y, type = "p", xaxt = "n", xlab=.xlab, ylab=.ylab, pch = pchQC, cex=dotsize, col = .colors,  cex.lab = 1)        
      }
    } else {  # zooming  
      plot(jp.x, jp.y, type = "p", xaxt = "n", xlab=.xlab, ylab=.ylab, xlim=.xlim, ylim=.ylim, pch=pchQC, cex=dotsize, col=.colors,  cex.lab =1)
    }
    axis(1, at=1:length(lx), labels=lx, las=2,  cex.axis = 1)
    
    plotTitle = ""
    if(experimentSubset[1] != "None selected") {
      plotTitle = experimentSubset[1]
    } else {
      plotTitle = "All experiments"
    }
    
    if(treatmentSubset[1] != "None selected") {
      plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
    } else {
      plotTitle = paste(plotTitle,"All treatments",sep="\n")
    }
    
    if(is.numeric(htmGetListSetting(htm,"visualisation","jitterPlot_subsample",gui=T))) {
      plotTitle = paste(plotTitle,paste("subsampling by",htmGetListSetting(htm,"visualisation","jitterPlot_subsample",gui=T)),sep="\n")
    }
    
    title(plotTitle, cex.main = 0.8)
    
    if(colorizeTreatments == T) {
      print("  putting legend")
      treatments <- treatments[ids]
      legend("topleft", inset=c(0,0), legend = levels(factor(treatments)), bg="white", col=col_pal, pch=16, bty="n")
    }
    
    
    d = 0.2   
    if(showMedian==T) {  
      #print("    median=blue")
      print("  plotting Median and MAD in blue")
      segments(jp.xMean-d,jp.yMedian,jp.xMean+d,jp.yMedian,col="blue",lwd=4)
      segments(jp.xMean-d,jp.yMedian+jp.yMAD,jp.xMean+d,jp.yMedian+jp.yMAD,col="blue",lwd=2)
      segments(jp.xMean-d,jp.yMedian-jp.yMAD,jp.xMean+d,jp.yMedian-jp.yMAD,col="blue",lwd=2)    
    }
    if(showMean==T) {
      print("  ploting Mean and SD in green")
      segments(jp.xMean-d,jp.yMean,jp.xMean+d,jp.yMean,col="darkgreen",lwd=4)
      segments(jp.xMean-d,jp.yMean+jp.ySD,jp.xMean+d,jp.yMean+jp.ySD,col="green",lwd=2)
      segments(jp.xMean-d,jp.yMean-jp.ySD,jp.xMean+d,jp.yMean-jp.ySD,col="green",lwd=2)    
    }
    
    par(op)
    
  } # plot
  
  if(action=="click") {
    i <- identify(jp.x, jp.y, n = 1, plot = FALSE)
    print("")
    print(paste(cx,"=",data[ids[i],cx]))
    print(paste(cy,"=",data[ids[i],cy]))
    print(paste("treatment =",treatments[ids[i]]))
    htmShowDataFromRow(htm,data[ids,],i)
  }
  
  if(save2file) {
    dev.off()
  }
  
  
}


#
# Scatter-plot
#

htmScatterPlot_Data <- function(htm, cx, cy, .xlim=NA, .ylim=NA, colorize="None selected", aggregate = "None selected", experimentSubset="None selected", treatmentSubset="None selected", newdev=T, action="plot") {
  
  
  print("")
  print("")
  print("Scatter Plot")
  
  
  data <- htm@data
  
  if(experimentSubset[1] != "None selected")  data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
  if(treatmentSubset[1] != "None selected") data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
  
  qc <- data$HTM_qc
  treatments <- data[[htm@settings@columns$treatment]]
  experiments <- data[[htm@settings@columns$experiment]]
    
  if(aggregate != "None selected") {
    vx <- tapply(htm@data[[cx]], htm@data[[aggregate]], mean, na.rm=TRUE)
    vy <- tapply(htm@data[[cy]], htm@data[[aggregate]], mean, na.rm=TRUE)
  } else {
    vx <- data[[cx]]
    vy <- data[[cy]]
  }
  
  
  if( colorize != "None selected") {
    if(aggregate != "None selected") {
      .colors <- tapply(htm@data[[colorize]], htm@data[[aggregate]], function(z) {z[1]})
    } else {
      .colors = data[[colorize]]
    }
    n = length(unique(data[[colorize]]))
    if(n<=8) {
      col_pal <- brewer.pal(8,"Set2")
    } else {
      col_pal <- rainbow(n)
    }
    .colors = col_pal[factor(.colors)]
  } else {
    .colors = rep(1, length(vx))
  }  
  
  
  if(!is.null(qc)) {
    print("  ..qc column exists!")
    if(htmGetListSetting(htm,"visualisation","plotting_showQCfailData_TF")==T) {  # label data points that did not pass QC
      pchQC = ifelse(qc==1, 16, 4)
    } else {  # remove data points that did not pass QC
      print("  not showing data points that failed QC")
      vx = vx[which(qc==1)]
      vy = vy[which(qc==1)]
      data = data[which(qc==1),]
      .colors = .colors[which(qc==1)]
      treatments = treatments[which(qc==1)]
      qc <- rep(1,length(vx))
      pchQC = rep(16, length(vx))
    }
  } else {
    print("  ..no qc column => show all data")
    pchQC = rep(16,length(vx))
  }
  
  
  if(action=="plot") {
    
    #factors <- factor(treatments, levels=unique(treatments)) 
    #palette(rainbow(length(unique(treatments)))) 
    
    if(newdev){
      dev.new()
    }
    
    dotsize = 0.75
    op <- par(mar = c(10,10,4,2) + 0.1) 
    
    if(htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF")==T) {
      .ylim = c(0, max(vy))
      .xlim = c(0, max(vx))
    }
    
    type = "p"

    # actual plotting
    if( is.na(.xlim) || is.na(.ylim) ) {
      plot(vx, vy, xlab=cx, ylab=cy, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    } else {    
      plot(vx, vy, xlab=cx, ylab=cy, xlim=.xlim, ylim=.ylim, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    }
    
    if( htmGetListSetting(htm,"visualisation","scatterPlot_showTreatmentMean_TF")==T) {
      factors <- factor(treatments, levels=unique(treatments)) 
      
      # compute stats for plotting
      vxMean = tapply(vx[which(qc==1)], factors[which(qc==1)], function(z) {mean(z,na.rm=T)})
      vyMean = tapply(vy[which(qc==1)], factors[which(qc==1)], function(z) {mean(z,na.rm=T)})
      colorsMean = tapply(.colors[which(qc==1)], factors[which(qc==1)], function(z) {z[1]})
      
      points(vxMean, vyMean, pch = 16, type = type, cex = 5*dotsize, main="", col=colorsMean)
      cat("\nMean values:\n")
      print(vxMean)
      print(vyMean)
      
    } 
    
    plotTitle = ""
    if(experimentSubset[1] != "None selected") {
      plotTitle = experimentSubset[1]
    } else {
      plotTitle = "All experiments"
    }
    
    if(treatmentSubset[1] != "None selected") {
      plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
    } else {
      plotTitle = paste(plotTitle,"All treatments",sep="\n")
    }
    title(plotTitle, cex.main = 1.2)
    
    
    linefit = FALSE
    if(linefit) {
      fit <- lm(vy~vx)
      print(fit)
      yfit = predict(fit, list(vx=vx))
      lines(vx,yfit,col="red",lwd=2)
      #fit <- lm(vx~vy)
      #print(fit)
      #xfit = predict(fit, list(vy=vy))
      #lines(xfit,vy,col="blue",lwd=2)
      #pcaFit(vx,vy)
    }
    
    
    if( colorize != "None selected") {
      print("  putting legend")
      legend("topleft",legend = levels(factor(data[[colorize]])), bg="white", col=col_pal, pch=16)
      # x=max(vx,na.rm=T),y=max(vy,na.rm=T),
    }
    
    
    par(op)
  }
  

  if(action=="click") {
    print("")
    i <- identify(vx, vy, n = 1, plot = FALSE)
    print(paste(cx,"=",data[i,cx]))
    print(paste(cy,"=",data[i,cy]))
    print(paste("treatment =",treatments[i]))
    htmShowDataFromRow(htm, data, i)
  } 
  
}



#
# Correlation-plot
#

htmCorrelationPlot <- function(htm, cm, corder, batchX, batchY, .xlim=NA, .ylim=NA, colorize="None selected", aggregate = "None selected", treatmentSubset="None selected", newdev=T, action="plot") {
  
  
  print("")
  print("")
  print("Correlation Plot")
  
  # get the data
  data <- htm@data
  if(treatmentSubset[1] != "None selected") data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
  
  qc <- data$HTM_qc
  treatments <- data[[htm@settings@columns$treatment]]
  experiments <- data[[htm@settings@columns$experiment]]
  
  if(aggregate != "None selected") {
    vx <- tapply(htm@data[[cx]], htm@data[[aggregate]], mean, na.rm=TRUE)
    vy <- tapply(htm@data[[cy]], htm@data[[aggregate]], mean, na.rm=TRUE)
  } else {
    vx_tmp <- data[[cm]][which(data[[htm@settings@columns$experiment]]==batchX)]
    vx_order <- data[[corder]][which(data[[htm@settings@columns$experiment]]==batchX)]
    vx <- vx_tmp[order(vx_order)]
  
    vy_tmp <- data[[cm]][which(data[[htm@settings@columns$experiment]]==batchY)]
    vy_order <- data[[corder]][which(data[[htm@settings@columns$experiment]]==batchY)]
    vy <- vy_tmp[order(vy_order)]
  }
  
  cx = paste(cm,batchX,sep="__")
  cy = paste(cm,batchY,sep="__")
    
  if( colorize != "None selected") {
    if(aggregate != "None selected") {
      .colors <- tapply(htm@data[[colorize]], htm@data[[aggregate]], function(z) {z[1]})
    } else {
      .colors = data[[colorize]]
    }
    n = length(unique(data[[colorize]]))
    if(n<=8) {
      col_pal <- brewer.pal(8,"Set2")
    } else {
      col_pal <- rainbow(n)
    }
    .colors = col_pal[factor(.colors)]
  } else {
    .colors = rep(1, length(vx))
  }  
  
  
  if(!is.null(qc)) {
    print("  ..qc column exists!")
    if(htmGetListSetting(htm,"visualisation","plotting_showQCfailData_TF")==T) {  # label data points that did not pass QC
      pchQC = ifelse(qc==1, 16, 4)
    } else {  # remove data points that did not pass QC
      print("  not showing data points that failed QC")
      vx = vx[which(qc==1)]
      vy = vy[which(qc==1)]
      data = data[which(qc==1),]
      .colors = .colors[which(qc==1)]
      treatments = treatments[which(qc==1)]
      qc <- rep(1,length(vx))
      pchQC = rep(16, length(vx))
    }
  } else {
    print("  ..no qc column => show all data")
    pchQC = rep(16,length(vx))
  }
  
  
  if(action=="plot") {
    
    #factors <- factor(treatments, levels=unique(treatments)) 
    #palette(rainbow(length(unique(treatments)))) 
    
    if(newdev){
      dev.new()
    }
    
    dotsize = 0.75
    op <- par(mar = c(10,10,4,2) + 0.1) 
    
    if(htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF")==T) {
      .ylim = c(0, max(vy))
      .xlim = c(0, max(vx))
    }
    
    type = "p"
    
    # actual plotting
    if( is.na(.xlim) || is.na(.ylim) ) {
      plot(vx, vy, xlab=cx, ylab=cy, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    } else {    
      plot(vx, vy, xlab=cx, ylab=cy, xlim=.xlim, ylim=.ylim, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    }
    
    if( htmGetListSetting(htm,"visualisation","scatterPlot_showTreatmentMean_TF")==T) {
      factors <- factor(treatments, levels=unique(treatments)) 
      
      # compute stats for plotting
      vxMean = tapply(vx[which(qc==1)], factors[which(qc==1)], function(z) {mean(z,na.rm=T)})
      vyMean = tapply(vy[which(qc==1)], factors[which(qc==1)], function(z) {mean(z,na.rm=T)})
      colorsMean = tapply(.colors[which(qc==1)], factors[which(qc==1)], function(z) {z[1]})
      
      points(vxMean, vyMean, pch = 16, type = type, cex = 5*dotsize, main="", col=colorsMean)
      cat("\nMean values:\n")
      print(vxMean)
      print(vyMean)
      
    } 
    
    plotTitle = ""
    if(experimentSubset[1] != "None selected") {
      plotTitle = experimentSubset[1]
    } else {
      plotTitle = "All experiments"
    }
    
    if(treatmentSubset[1] != "None selected") {
      plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
    } else {
      plotTitle = paste(plotTitle,"All treatments",sep="\n")
    }
    title(plotTitle, cex.main = 1.2)
    
    
    linefit = FALSE
    if(linefit) {
      fit <- lm(vy~vx)
      print(fit)
      yfit = predict(fit, list(vx=vx))
      lines(vx,yfit,col="red",lwd=2)
      #fit <- lm(vx~vy)
      #print(fit)
      #xfit = predict(fit, list(vy=vy))
      #lines(xfit,vy,col="blue",lwd=2)
      #pcaFit(vx,vy)
    }
    
    
    if( colorize != "None selected") {
      print("  putting legend")
      legend("topleft",legend = levels(factor(data[[colorize]])), bg="white", col=col_pal, pch=16)
      # x=max(vx,na.rm=T),y=max(vy,na.rm=T),
    }
    
    
    par(op)
  }
  
  
  if(action=="click") {
    print("")
    i <- identify(vx, vy, n = 1, plot = FALSE)
    print(paste(cx,"=",data[i,cx]))
    print(paste(cy,"=",data[i,cy]))
    print(paste("treatment =",treatments[i]))
    htmShowDataFromRow(htm, data, i)
  } 
  
}



