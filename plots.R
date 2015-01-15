# Viewing

htmShowImagesFromRow <- function(htm,data,ir){
  
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

  
  if(length(images)) {
    
    #imlist <- data.frame(x=images)
    #edit(imlist)
    #print(getwd())
    #write.table(imlist,file="imagelist.txt",row.names=F,col.names=F,quote=F)
    
    #fileConn<-file("imagelist.txt", open="wt")
    #for(image in images) {
    #  if(image!=images[length(images)]) {
    #    writeChar(image, fileConn, eos="\n")
    #  } else {
    #    #print("last image")
    #    writeChar(image, fileConn, eos=NULL) 
    #  }
    #}
    #sink()
    #print(images)
    #writeLines(images,fileConn)
    #writeLines(images,fileConn)
    #close(fileConn)
    #Sys.sleep(0.5)
    
    #fileConn<-file("imagelist.txt", open="r")
    #print(readLines(fileConn))
    #close(fileConn)
       
    # java -jar ...jython.jar:.../ij.jar script.py argument
    
    #if ( .Platform$OS.type == "unix" ) {
    #  pathmacro = paste("'",getwd(),"/","stackfromlist.py","'",sep="")
    #  #argument = paste("'",getwd(),"/","imagelist.txt"," ",expname,"'",sep="")
    #  argument = paste("'",getwd(),"/","imagelist.txt","'",sep="")
    #  
    #} else if ( .Platform$OS.type == "windows" ) {
    #  pathmacro = paste("\"",getwd(),"\\","stackfromlist.py","\"",sep="")
    #  argument = paste("\"",getwd(),"\\","imagelist.txt","'",expname,"\"",sep="")
    #}
  
    #cmd = paste(imageViewerCMD,pathmacro,argument)
    cmd = imageViewerCMD
    for(image in images) {
      cmd = paste(cmd,paste(' -eval \"open(\'',image,'\')\"',sep=""))
    }
    print(cmd)
    system(cmd,wait=F)
    
  } else {
    print("No images selected/found. Cannot open.")
  }
  
  
  }

htmShowWellsFromRow <- function(htm,data,ir){
  
  print("Click and View:")
  for(colname in colnames(data)) {
    print(paste("  ",colname,"=",data[[colname]][ir]))
  }   
  
}




# d <- subset(htm@wellSummary, (experiment=="Bettencourt_centriole_assay_01_batch1_01"), select= c("minusMeanCtrl__log2__Mean_CentroCells_Math_CentrinNormTot__weighted_mean_of_images","wellNum" )
# d2<-d[order(d$wellNum),]
# d2s <- d2$minusMeanCtrl__log2__Mean_CentroCells_Math_CentrinNormTot__weighted_mean_of_images
# m <- matrix(d2s,ncol=8)
# image(m, col=gray((0:32)/32))
# med = medpolish(m)
# mp <-  med$overall + outer(med$row,med$col, "+")

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
    dev.new(width = width, height =  plate.nrow / plate.ncol * width)
  } 
  
  op <- par(bg = "grey")
  
  if(datatype=="images") {
    print("plotting per_image data")
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
    rect(0, y, 10, y+stepsize/scale, col=lut[i], border=NA)
  }
  #par(op) 
  
  if(newdevice) {
    dev.set(currentdev)
  }
  
}


scaleForDisplay <- function(values,lut_min,lut_max){
  v = 255*(values-as.numeric(lut_min) )/(as.numeric(lut_max)-as.numeric(lut_min) )
  toosmall = which(v<1)
  toolarge = which(v>255)
  v[toosmall]=1
  v[toolarge]=255
  return(v)
}


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
             lwd = 1.5,
             cex = 2.0 * htm@settings@visualisation$heatmap_image_size_cex * max(c(htm@settings@visualisation$number_subpositions_x,htm@settings@visualisation$number_subpositions_y)),
             col = color) #22 = empty rectangle
    }
  
  }
  
}



htmJitterplot <- function(htm=htm, cx, cy, .xlab="", .ylab="", treatmentSubset = "None selected", 
                          .xlim=NA, .ylim=NA, datatype="images", colorizeTreatments=F,  
                          sorting="none", experimentSubset="None selected", newdev = T, 
                          action="plot", printMeanSD = T,  showMean = T, showMedian = T, save2file = F,
                          scaleFromZero = F) {
  
  print("")
  print("")
  print("")
  print("")
  print("Jitter Plot:")
  print(paste("  datatype:",datatype))
  print(paste("  x =",cx))
  print(paste("  y =",cy))
  
  if("all treatments" %in% treatmentSubset) { 
    treatmentSubset = "None selected"
  }
  
  if(datatype=="images") {
 
    data <- htm@data
    
    if(experimentSubset[1] != "None selected") {
      print(paste("..selecting experiments:",experimentSubset))
      data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
    }
    if(treatmentSubset[1] != "None selected") {
      print(paste("..selecting treatments:",treatmentSubset))
      data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
    }
    qc <- data$HTM_qcImages
    treatments <- data[[htm@settings@columns$treatment]]
    
  }
  
  if(datatype=="positions") {
    
    data <- htm@wellSummary
    
    if(experimentSubset[1] != "None selected")  data <- subset(htm@wellSummary, htm@wellSummary$experiment==selectedExp )
    if(treatmentSubset[1] != "None selected") data <- subset(data, htm@wellSummary$treatment %in% treatmentSubset)
    
    qc <- data$wellQC
    treatments <- data$treatment
    
    }
   
  print(paste("  sorting:",sorting))
  if(sorting=="none") {
    ids = 1:nrow(data)
  }
  if(sorting=="alphabetic") {
    print("applying alphabetic sorting...")
    #print(factor(data[[cx]]))
    ids <- order(data[[cx]],na.last=NA)
  }
  if(sorting=="median value") {
    print("MEDIAN")
    #print(factor(data[[cx]]))
    factors <- data[[cx]]
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
      data$medians[ids] = rep(median(values),length(ids))
    }
    ids <- order(data$medians,na.last=NA)
  }
  
  # assign sorted data
  print("  prepare plotting..")
  factors <- factor(data[ids,cx], levels=unique(data[ids,cx])); #print(factors)
  jp.x <- jitter(as.numeric(factors),htm@settings@visualisation$jitter)
  lx = levels(factors); #print(lx)
  jp.y <- data[ids,cy] 
  qc <- qc[ids]
  treatments <- treatments[ids]
  #data <- data[ids,]
  
  
  print(paste("  colorizeTreatments:",colorizeTreatments==T))
  if(colorizeTreatments == T) {
    print("colorizing by treatment")
    .colors = factor(treatments, levels=unique(treatments)) 
  } else {
    .colors = rep("grey30", nrow(data))
  }  
  
  
  if(!is.null(qc)) {
    print("  ..qc column exists!")
    pchQC = ifelse(qc==1, 16, 4) 
    jp.y.qc <- ifelse(qc==1, jp.y, NA)
  } else {
    print("..no qc column => evaluate all data")
    pchQC = rep(16, nrow(data))
    jp.y.qc <- jp.y
  }
  
  
  #print(paste("..Mean SD:",printMeanSD))
  if(printMeanSD) {
    cat("\n\n\nLabel   Mean   SD\n")
    jp.yMean = tapply(jp.y.qc, factors, function(z) {mean(z,na.rm=T)})
    jp.ySD = tapply(jp.y.qc, factors, function(z) {sd(z,na.rm=T)})
    for(i in 1:length(jp.yMean)) {
      print(paste(names(jp.yMean)[i],jp.yMean[i],jp.ySD[i],sep="   "))     
    }
  }
  
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
     
    cat(paste("\n\njitter-plotting",cy,"vs.",cx,"\n"))
      
    op <- par(mar = c(8,5,4,2) + 0.1) 
    
    if( is.na(.xlim) || is.na(.ylim) ) {
      if(htmGetListSetting(htm,"visualisation","jitterPlot_scaleFromZero_TF")==T) {
        plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, ylim = c(0, max(jp.y)), pch=pchQC, cex=0.5, col = .colors)
      } else {
        plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, pch=pchQC, cex=0.5, col = .colors)        
      }
    } else {  # zooming  
      plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, xlim=.xlim, ylim=.ylim, pch=pchQC, cex=0.5, col=.colors)
    }
    axis(1, at=1:length(lx), labels=lx, las=2,  cex.axis = 0.75)
    
    plotTitle = ""
    if(experimentSubset[1] != "None selected") {
      plotTitle = experimentSubset[1]
    } else {
      plotTitle = "all experiments"
    }
    
    if(treatmentSubset[1] != "None selected") {
      plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
    } else {
      plotTitle = paste(plotTitle,"all treatments",sep="\n")
    }
    title(plotTitle)
     
    if(colorizeTreatments == T) {
      legend(min(jp.x),max(jp.y),unique(treatments),col=1:length(treatments),pch=16)
    }
    
    
    d = 0.2   
    if(showMedian==T) {  
      #print("    median=blue")
      print("..plotting Median and MAD in blue")
      segments(jp.xMean-d,jp.yMedian,jp.xMean+d,jp.yMedian,col="blue",lwd=4)
      segments(jp.xMean-d,jp.yMedian+jp.yMAD,jp.xMean+d,jp.yMedian+jp.yMAD,col="blue",lwd=2)
      segments(jp.xMean-d,jp.yMedian-jp.yMAD,jp.xMean+d,jp.yMedian-jp.yMAD,col="blue",lwd=2)    
    }
    if(showMean==T) {
      print("..ploting Mean and SD in green")
      segments(jp.xMean-d,jp.yMean,jp.xMean+d,jp.yMean,col="darkgreen",lwd=4)
      segments(jp.xMean-d,jp.yMean+jp.ySD,jp.xMean+d,jp.yMean+jp.ySD,col="green",lwd=2)
      segments(jp.xMean-d,jp.yMean-jp.ySD,jp.xMean+d,jp.yMean-jp.ySD,col="green",lwd=2)    
    }
    
    par(op)
    
  } # plot
  
  
  if(action=="click") {
    print("click and view")
    if(datatype=="images") {
      i <- identify(jp.x, jp.y, n = 1, plot = FALSE)
      print(paste("y-axis value =",data[ids[i],cy]))
      htmShowImagesFromRow(htm,data[ids,],i)
    } else if(datatype=="wells") {
      i <- identify(jp.x, jp.y, n = 1, plot = FALSE)
      htmShowWellsFromRow(htm,data[ids,],i)
    } else {
      print("WARNING: Click and View is not supported for this datatype.")
    }
  }
  
  if(save2file) {
   dev.off()
   }
  
    
}


htmHisto <- function(cx,  experimentSubset = "None selected", treatmentSubset= "None selected", datatype, save2file = F, newdev = T) {
  
  if(datatype=="wells") {
    data <- htm@wellSummary
    
    if(experimentSubset[1] != "None selected")  data <- subset(htm@wellSummary, htm@wellSummary$experiment==selectedExp )
    if(treatmentSubset[1] != "None selected") data <- subset(data, htm@wellSummary$treatment %in% treatmentSubset)
    
    qc <- data$wellQC
    treatments <- htm@wellSummary$treatment 
    
  }
  
  plotTitle = ""
  if(treatmentSubset[1] != "None selected") {
    plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
  } else {
    plotTitle = paste(plotTitle,"all treatments",sep="\n")
  }
  
  if(save2file) {
    path = gfile("Save as...", type="save", initialfilename = paste0("Histogram__Mean_Corrected_Controls--",htmGetListSetting(htm,"statistics","transformation"),"--",htmGetListSetting(htm,"statistics","measurement"),".pdf"))
    pdf(file=path)
    print(path)
  } else if(newdev){
    dev.new()
  }
  
  hist(data[[cx]], xlab=cx, main=plotTitle)
  
  if(save2file) {
    dev.off()
  }
  

}
  

htmScatterPlot <- function(htm, cx, cy, .xlim=NA, .ylim=NA, datatype="images", colorize="None selected", experimentSubset="None selected", treatmentSubset="None selected", newdev=T, action="plot") {
   
  
  print("")
  print("")
  print("Scatter Plot:")
  print(paste("  Data type =",datatype))
  
  
  if(datatype=="images") {
    
    data <- htm@data
    
    print(paste("  Treatment subset =",treatmentSubset))
    
    if(experimentSubset[1] != "None selected")  data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
    if(treatmentSubset[1] != "None selected") data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
    
    qc <- data$HTM_qcImages
    treatments <- data[[htm@settings@columns$treatment]]
    experiments <- data[[htm@settings@columns$experiment]]
  
  }
  
  if(datatype=="objects") {
    
    clink <- htm@settings@columns$objectimagelink
    
    # get all valid image ids for the experiment selection
    if(experimentSubset[1]!="None selected") {
      validIDs <- htm@data[[clink]][which(htm@data[[htm@settings@columns$experiment]] %in% experimentSubset)]
    } else {
      validIDs <- htm@data[[clink]]
    }
    
    # subset object rows with valid image ids
    data <- htm@objectdata
    print(nrow(data))
    data <- subset(data, data[[clink]] %in% validIDs)
    print(nrow(data))
    
    # get all valid image ids for the treatment selection
    if(treatmentSubset[1]!="None selected") {
      validIDs <- htm@data[[clink]][which(htm@data[[htm@settings@columns$treatment]] %in% treatmentSubset)]
    } else {
      validIDs <- htm@data[[clink]]
    } 
    data <- subset(data, data[[clink]] %in% validIDs)
    #print(nrow(data))
    
    finalIDs <- data[[clink]]
    if(!is.null(htm@data$HTM_qcImages)) {
      qc <- sapply(finalIDs,function(x) htm@data$HTM_qcImages[which(htm@data[[clink]]==x)])
    } else {
      qc <- NULL
    }
    treatments <- sapply(finalIDs, function(x) htm@data[[htm@settings@columns$treatment]][which(htm@data[[clink]]==x)]) 
    
  }
  
  #print(treatments)
  
  if(datatype=="wells") {
  
    data <- htm@wellSummary
    
    if(experimentSubset!="None selected")  data <- subset(data, data$experiment==experiments)
    if(treatmentSubset!="None selected") data <- subset(data, data$treatment %in% treatmentSubset)
    #edit(data)
    qc <- data$wellQC
    treatments <- data$treatment
    experiments <- data$experiment
    
  }
  
  
  vx <- data[[cx]]
  vy <- data[[cy]]

  summary(vx)
  summary(vy)
  
  
  if(colorize=="treatment") {
    print("colorizing by treatment")
    .colors = factor(treatments)
  } else if (colorize=="experiment") {
    print("colorizing by experiment")
    .colors = factor(experiments)
  } else {
    .colors = rep(1, nrow(data))
  }  

  
  if(!is.null(qc)) {
    print("  ..qc column exists!")
    if(htmGetListSetting(htm,"visualisation","plotting_showQCfailData_TF")==T) {  # label data points that did not pass QC
      pchQC = ifelse(qc==1, 16, 4)
    } else {  # remove data points that did not pass QC
      vxTmp = vx[which(qc==1)]
      vyTmp = vy[which(qc==1)]
      dataTmp = data[which(qc==1),]
      colorsTmp = .colors[which(qc==1)]
      .colors = colorsTmp
      vx = vxTmp
      vy = vyTmp
      data = dataTmp
      pchQC = rep(16, length(vx))
    }
  } else {
    print("  ..no qc column => show all data")
    pchQC = rep(16,length(vx))
  }
  
  
  if(action=="plot") {
    if(newdev){
      dev.new()
    }
    op <- par(mar = c(8,5,4,2) + 0.1) 
    if(htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF")==T) {
      .ylim = c(0, max(vy))
      .xlim = c(0, max(vx))
    }
    if( is.na(.xlim) || is.na(.ylim) ) {
      plot(vx, vy, xlab=cx, ylab=cy, pch=pchQC, cex=0.5, main="", col=.colors)
    } else {    
      plot(vx, vy, xlab=cx, ylab=cy, xlim=.xlim, ylim=.ylim, pch=pchQC, cex=0.5, main="", col=.colors)
    }
    par(op)
  }

  
  plotTitle = ""
  if(experimentSubset[1] != "None selected") {
    plotTitle = experimentSubset[1]
  } else {
    plotTitle = "all experiments"
  }
  
  if(treatmentSubset[1] != "None selected") {
    plotTitle = paste(plotTitle,paste(treatmentSubset,collapse=" "),sep="\n")
  } else {
    plotTitle = paste(plotTitle,"all treatments",sep="\n")
  }
  title(plotTitle)
  
  
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
  
  
  if(colorize=="treatment") {
    legend(x=min(vx,na.rm=T),y=max(vy,na.rm=T),levels(factor(treatments)),col=1:length(treatments),pch=16)
  }
  
  if(action=="click") {
    if(datatype=="images") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      print(paste("  x-axis value =",data[i,cx]))
      print(paste("  y-axis value =",data[i,cy]))
      htmShowImagesFromRow(htm,data,i)
    } else if(datatype=="wells") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      htmShowWellsFromRow(htm,data,i)
    } else {
      print("WARNING: Click and View is not supported for this datatype.")
    } 
  }

}



handler_zoomJitterPlot <-  function(h, ...){
  loc = locator(n=2)
  jp.plot(jp.cx,jp.cy,ylabel=jp.cy,jp.xlim=sort(loc$x),jp.ylim=sort(loc$y) )
}

handler_showImageJitterPlot <-  function(h, ...){
  print("please select a point for viewing!")
  ir = identify(x = jp.x, y = jp.y, n = 1, plot = FALSE)
  #print(ir)
  #showImagesFromRow(ir)
  showImagesFromRow2(ir)
}



