# Viewing

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
    dev.new(width = width, height =  plate.nrow / plate.ncol * width)
  } 
  
  op <- par(bg = "grey")
  
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


scatterLabelPlot_treatFeat <- function(d, cX, cY, subset="None selected", exclude="None selected", xlab="", ylab="") {
  
  #channels <- unique(unlist(lapply(strsplit(rownames(d),"_"), function(x) {x[1]})))
  
  #pdf(file = "/Users/tischi/Desktop/multiScatter.pdf") #, width = 500, height = 500)
  
  #for (channel in channels) {
    
    ds <- d
    
  #  subset = channel
  #  print(channel)
    
    if(subset != "None selected") {
      ids = which(grepl(subset,rownames(d)))
      ds <- ds[ids,]
    }
    
    if(exclude != "None selected") {
      print("excluding..")
      ids = which(grepl(exclude,rownames(d)))
      print(ids)
      ds <- ds[-ids,]
    }
    
    
    lables = rownames(ds)
    x = ds[[cX]]
    y = ds[[cY]]
    
    plot(x, y, #xlab=cX, ylab=cY, 
         main = "", type = "n", 
         xlim = c(0,max(x)*1.2),
         xlab = xlab,
         ylab = ylab
    ) #, 
    text(x, y, labels = lables, cex=.7)

  #}
  
  #dev.off()
      
}


plotHeatmap_treatFeat <- function(d,LUT.min,LUT.max,subset="None selected",rotate = F, rowSubset="None selected", readout="") {
  
  
  if(subset != "None selected") {
    ids = which(grepl(subset,colnames(d)))
    d <- d[,ids]
  }
    
  d$treatment <- NULL
  d$date <- NULL
  
  
  if(rowSubset[1] != "None selected") {
    ids = vector(length=0)
    for(rs in rowSubset) {
      #print(rs)
      idsNew = which(grepl(rs,rownames(d)))
      print(idsNew)
      ids = c(ids,idsNew)
    }
    #print(ids)
    d <- d[ids,]
  }
  
  
  

  #plot(x = p$col, y = p$row, col=colpal[p$val],
  #     xaxt = "n", yaxt = "n", #  xaxs = "i",
  #     pch = 15, cex = 5, xlab="", ylab="")
  
  featureNames = unlist(lapply(strsplit(colnames(d),readout), function(x) {x[2]}))
  #featureNames = unlist(lapply(strsplit(featureNames,"xx"), function(x) {paste0(x[1],x[3])}))
  featureNames <-  gsub("_NA","",featureNames)
  featureNames <-  gsub("NA","",featureNames)
  #featureNames <-  gsub("Children","_Object",featureNames)
  featureNames <-  gsub("Sum__AreaShape_Area","_Object__TotalArea",featureNames)
  #featureNames <-  gsub("_Intensity","",featureNames)
  featureNames <-  gsub("StdIntensity","IntensityVariation",featureNames)
  featureNames <-  gsub("MeanIntensity_LocalVar","LocalTexture",featureNames)
  featureNames <-  gsub("NormDistance__Nucleus","NormDistToNuclearPeriphery",featureNames)
  
  #featureNames <-  gsub("Mean_Cells_Mean_MaxGolgi_AreaShape_FormFactor","Compactness",featureNames)
  #featureNames <-  gsub("Mean_Cells_Children_Golgi_Count","Fragmentation",featureNames)
  #featureNames <-  gsub("Mean_Cells_Intensity_IntegratedIntensity_GolgiDotsMasked","Vesicularity",featureNames)
  #featureNames <-  gsub("Mean_Cells_Intensity_IntegratedIntensity_GolgiLinesMasked","Tubularity",featureNames)  
  #featureNames <-  gsub("Mean_Cells_Intensity_IntegratedIntensity_GolgiDiffuse","Diffuse",featureNames)  
  
  # general replacements
  featureNames <-  gsub("Masked","",featureNames)
  featureNames <-  gsub("Mean_Cells_","",featureNames)
  
  
  
  colnames(d) <- featureNames
  d <- d[,order(colnames(d), decreasing = TRUE)]
  
  rownames(d) <-  gsub("8h","_08h",rownames(d))
  rownames(d) <-  gsub("24h","_24h",rownames(d))
  rownames(d) <-  gsub("Control","AAAA",rownames(d))
  d <- d[order(rownames(d)),]
  rownames(d) <-  gsub("AAAA","Control",rownames(d))
  
  #print(d)

  p <- convertMatrixToVectorList(d)

  #print(length(p$val))
  # rescale values to 8-bit LUT
  p$val = scaleForDisplay(p$val,LUT.min,LUT.max)
  
  #print(length(p$val))
  colpal <- colorRampPalette(c("blue","white","red"))(256) 
  #print(length(colpal))
  #dev.new() 
  
  #print(p$val)
  #print(length(colpal[p$val]))
  
  
  if(rotate) {
    par(mar=c(15,15,1,1), bg="grey") # bg="white"
    x = p$row
    y = p$col
    axis1labels = rownames(d)   
    axis2labels = colnames(d)
    cex1 = 1
    cex2 = 1
    
  } else {
    par(mar=c(15,15,1,1), bg="grey") # bg="white"
    x = p$col
    y = p$row
    axis1labels = colnames(d)
    axis2labels = rownames(d)
    cex2 = 1
    cex1 = 1
  }
  
  
  plot(x = x, y = y,
       xaxt = "n", yaxt = "n",
       pch = 15, xlab="", ylab="", cex = 3, xlim=c(0,length(axis1labels)+1), col=colpal[p$val])
  #print(length(axis1labels))
  #print(length(axis2labels))
  
  #points(x = x, y = y, col=colpal[p$val],
  #     pch = 15,  cex = 2.6)
  
  axis(1, at=1:length(axis1labels), labels=axis1labels, las=2, cex.axis = cex1)
  axis(2, at=1:length(axis2labels), labels=axis2labels, las=1, cex.axis = cex2)
 
  abline(v = seq(1:84), col="lightgray", lty = "dotted")
  
  # Now plot the x axis, but without labels
#  axis(1, at=seq(1, 10, by=2), labels = FALSE) 
  
  # Now draw the textual axis labels
  #text(seq(1, ncol(d), by = 1), par("usr")[3] - 0.0, 
  #     labels = featureNames, 
  #     srt = 45, pos = 1, xpd = TRUE) 
  
  }



convertMatrixToVectorList <- function(m) {
  
  p = list()
  i = 1
  for (ir in seq(1,nrow(m))) {
    for (ic in seq(1,ncol(m))) {
      p$col[i] = ic
      p$row[i] = ir
      p$val[i] = m[ir,ic]
      i=i+1
    }
  }
  
  return(p)
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
             lwd = 2.25,
             cex = 1.1 * htm@settings@visualisation$heatmap_image_size_cex * max(c(htm@settings@visualisation$number_subpositions_x,htm@settings@visualisation$number_subpositions_y)),
             col = color) #22 = empty rectangle
    }
  
  }
  
}



htmJitterplot <- function(htm=htm, cx, cy, .xlab="", .ylab="", treatmentSubset = "None selected", 
                          .xlim=NA, .ylim=NA, datatype="images", colorizeTreatments=F,  
                          sorting="None selected", experimentSubset="None selected", newdev = T, 
                          action="plot", printMeanSD = T,  showMean = T, showMedian = T, save2file = F,
                          scaleFromZero = F, reference="None selected") {
  
  print("")
  print("")
  print("")
  print("")
  print("Jitter Plot:")
  print(paste("  datatype:",datatype))
  print(paste("  x =",cx))
  print(paste("  y =",cy))
  
  if("All treatments" %in% treatmentSubset) { 
    treatmentSubset = "None selected"
  }
  
  if(datatype=="images") {
 
    data <- htm@data
    
    if(experimentSubset[1] != "None selected") {
      print(paste("  selecting experiments:",experimentSubset))
      data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
    }
    if(treatmentSubset[1] != "None selected") {
      print(paste("  selecting treatments:",treatmentSubset))
      data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
    }
    
    qc <- data$HTM_qcImages
    if(!is.null(htm@settings@columns$treatment)) treatments <- data[[htm@settings@columns$treatment]]
    
  }

  if(datatype=="objects") {
    
    data <- htm@objectdata
    
    if(experimentSubset[1] != "None selected") {
      print(paste("  selecting experiments:",experimentSubset))
      data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
    }
    if(treatmentSubset[1] != "None selected") {
      print(paste("  selecting treatments:",treatmentSubset))
      data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
    }
    
    qc <- NULL
    if(!is.null(htm@settings@columns$treatment)) treatments <- data[[htm@settings@columns$treatment]]
    
  }
  
  if(datatype=="positions") {
    
    data <- htm@wellSummary
    
    if(experimentSubset[1] != "None selected")  data <- subset(htm@wellSummary, htm@wellSummary$experiment==selectedExp )
    if(treatmentSubset[1] != "None selected") data <- subset(data, htm@wellSummary$treatment %in% treatmentSubset)
    
    qc <- data$wellQC
    treatments <- data$treatment
    
  }
  
  
  if(!is.null(qc)) {
    print("  qc column exists => computing statistics only for points that passed QC")
  } else {
    print("  no qc column => evaluate all data")
    qc = rep(1, nrow(data))
  }
   
  print(paste("  sorting:",sorting))
  if(sorting=="None selected") {
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
  
  # log2 transform if wished
  if(htmGetListSetting(htm,"visualisation","jitterPlot_log2_TF")==T) {
    jp.y <- log2(jp.y)
    .ylab <- paste(.ylab,"[log2]")
  }
  
  
  print(paste("  colorizeTreatments:",colorizeTreatments==T))
  if(colorizeTreatments == T) {
    treatments <- treatments[ids]
    print("colorizing by treatment")
    .colors = factor(treatments, levels=unique(treatments)) 
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
  
  
  
  # remove non QC datapoints from the jp.x and jp.y by
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
      
    op <- par(mar=c(7.1, 4.1, 4.1, 2.1), xpd=TRUE) #par(mar = c(8,5,4,2) + 0.1) 
    
    dotsize = 0.75
    
    
    if( is.na(.xlim) || is.na(.ylim) ) {
      if(htmGetListSetting(htm,"visualisation","jitterPlot_scaleFromZero_TF")==T) {
        plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, ylim = c(0, max(jp.y)), pch=pchQC, cex=dotsize, col = .colors,  cex.lab = 1)
      } else {
        plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, pch=pchQC, cex=dotsize, col = .colors,  cex.lab = 1)        
      }
    } else {  # zooming  
      plot(jp.x, jp.y, xaxt = "n", xlab=.xlab, ylab=.ylab, xlim=.xlim, ylim=.ylim, pch=pchQC, cex=dotsize, col=.colors,  cex.lab =1)
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
    
    title(plotTitle, cex.main = 0.8)
     
    if(colorizeTreatments == T) {
      treatments <- treatments[ids]
      dx = -0.5
      legend("topright", inset=c(dx,0), legend = unique(treatments), col=1:length(treatments), pch=16, bty="n")
      #min(jp.x), max(jp.y), 
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
    } else if(datatype=="objects") {
      i <- identify(jp.x, jp.y, n = 1, plot = FALSE)
      print(paste("y-axis value =",data[ids[i],cy]))
      htmShowObjectsFromRow(htm, data[ids,], i)      
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
    plotTitle = paste(plotTitle,"All treatments",sep="\n")
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
    
     data <- htm@objectdata
    
    if(experimentSubset[1] != "None selected") {
      print(paste("  selecting experiments:",experimentSubset))
      data <- subset(data, data[[htm@settings@columns$experiment]] %in% experimentSubset)
    }
    if(treatmentSubset[1] != "None selected") {
      print(paste("  selecting treatments:",treatmentSubset))
      data <- subset(data, data[[htm@settings@columns$treatment]] %in% treatmentSubset)
    }
    
    qc <- data$HTM_qcObjects
    treatments <- data[[htm@settings@columns$treatment]]
  
    
  }
  
  if(datatype=="positions") {
  
    data <- htm@wellSummary
    
    if(experimentSubset!="None selected")  data <- subset(data, data$experiment==experiments)
    if(treatmentSubset!="None selected") data <- subset(data, data$treatment %in% treatmentSubset)
    #edit(data)
    qc <- data$wellQC
    treatments <- data$treatment
    experiments <- data$experiment
    
  }
  
  if(datatype=="MDS") {
    
    data <- htm@other$MDS
    
    #if(experimentSubset!="None selected")  data <- subset(data, data$experiment==experiments)
    #if(treatmentSubset!="None selected") data <- subset(data, data$treatment %in% treatmentSubset)
    #edit(data)
    qc <- rep(1,nrow(htm@other$MDS))
    treatments <- data$treatment
    experiments <- c("not applicable")
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
    
    dotsize = 0.75
    op <- par(mar = c(10,10,4,2) + 0.1) 
    
    if(htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF")==T) {
      .ylim = c(0, max(vy))
      .xlim = c(0, max(vx))
    }
    
    if(datatype=="MDS") {
      type = "n"
    } else {
      type = "p"
    }
    
    # actual plotting
    if( is.na(.xlim) || is.na(.ylim) ) {
      plot(vx, vy, xlab=cx, ylab=cy, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    } else {    
      plot(vx, vy, xlab=cx, ylab=cy, xlim=.xlim, ylim=.ylim, type = type, pch=pchQC, cex=dotsize, main="", col=.colors, cex.lab=1.2)
    }
    
    if(datatype=="MDS") {
      text(vx, vy, labels = data$treatment, cex=0.5)
    } 
    
    par(op)
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
  
  
  if(colorize=="treatment") {
    legend("topright",legend = levels(factor(treatments)),col=1:length(treatments),pch=16)
    # x=max(vx,na.rm=T),y=max(vy,na.rm=T),
  }
  
  if(action=="click") {
    if(datatype=="images") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      print(paste("  x-axis value =",data[i,cx]))
      print(paste("  y-axis value =",data[i,cy]))
      htmShowImagesFromRow(htm, data, i)
    } else if(datatype=="objects") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      htmShowObjectsFromRow(htm, data, i)
    } else if(datatype=="wells") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      htmShowWellsFromRow(htm,data,i)
    } else if(datatype=="MDS") {
      i <- identify(vx, vy, n = 1, plot = FALSE)
      print(paste("You clicked on",data$treatment[i]))
      htmShowTreatmentsFromRow(htm,data,i)
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



