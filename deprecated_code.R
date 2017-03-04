

htmImageNormalization <- function(htm) {
    
    print("")
    print("Image normalization")
    print("*******************")
    print("")
    
    # get all necessary information
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    transformation <- htmGetListSetting(htm,"statistics","transformation")
    
    cat("\nMeasurement:\n")
    print(measurement)
    cat("\nNegative Control:\n")
    print(negcontrols)
    
    
    
    # check whether we know everything            
    if( is.null(experiments) || measurement=="None selected") {
        print("")
        print("  ERROR: cannot perform analysis due to lacking information (see above).")
        gmessage("Error: see R console output.")
        return(htm)
    }
    
    if( is.null(htm@data$HTM_qcImages) ) {
        print(" WARNING: there was no Image QC column; all images with non NA values will be set tovalid!")
        htm@data$HTM_qcImages = !is.na(htm@data[[measurement]])
    } 
    
    
    if(transformation == "log2") {
        # compute log transformation
        # create new column name
        logScoreName = paste(measurement,"log2",sep="__")
        htm@data[[logScoreName]] <- log2(htm@data[[measurement]]) 
        
        # todo: this should be at a more prominent position
        print("Replacing -Inf in log scores ******************************")
        logScores = htm@data[[logScoreName]]
        finiteLogScores = subset(logScores,is.finite(logScores))
        minimum = min(finiteLogScores)
        idsInf = which(is.infinite(logScores))
        logScores[idsInf] <- minimum
        htm@data[[logScoreName]] <- logScores
        
        #htmAddLog("Replacing Infinities in Log2 Score by")
        #htmAddLog(minimum)
        #htmAddLog("Affected Wells:")
        #for(id in idsInf) {
        #  htmAddLog(htm@wellSummary$treatment[id])
        #  htmAddLog(htm@wellSummary$wellQC[id])
        #  htmAddLog(htm@wellSummary[id,logScoreName])
        #  htmAddLog("")
        #}
    } # if log transformation
    
    
    # select log2 data in case data transformation is selected
    if(transformation == "log2") {
        measurement = logScoreName
    } else {
        measurement = measurement
    }
    
    measurement_minusMeanCtrl = paste(measurement,"minusMeanCtrl",sep="__")
    
    # initialisation
    htm@data[[measurement_minusMeanCtrl]] = NA
    
    # computation
    cat("\nComputing Image Normalisations...\n")
    
    for(experiment in experiments) {
        
        if(experiment %in% experiments_to_exclude) next
        
        print("")
        print(paste("  Experiment:",experiment))
        
        indices_all <- which((htm@data[[htm@settings@columns$experiment]] == experiment))
        indices_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]))
        
        if("all treatments" %in% negcontrols) {
            indices_controls_ok <- indices_ok
        } else {
            indices_controls_ok <- which((htm@data[[htm@settings@columns$experiment]] == experiment) & (htm@data$HTM_qcImages) & !is.nan(htm@data[[measurement]]) & (htm@data[[htm@settings@columns$treatment]] %in% negcontrols))
        }
        
        print(paste("    Images Total", length(indices_all)))
        print(paste("    Images Valid", length(indices_ok)))      
        print(paste("    Images Valid Control", length(indices_controls_ok)))
        
        # here values are extracted 
        valuescontrol <- htm@data[indices_controls_ok, measurement]
        #print(valuescontrol)
        
        nr_of_controls <-  length(valuescontrol)
        meancontrol <- mean(valuescontrol)    
        sigmacontrol <- sd(valuescontrol) 
        mediancontrol <- median(valuescontrol)
        madcontrol <- mad(valuescontrol)  
        semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
        print(paste("    Control Mean:", meancontrol))
        print(paste("    Control SD:", sigmacontrol))
        print(paste("    Control Median:", mediancontrol))
        print(paste("    Control MAD:", madcontrol))
        
        htm@data[indices_all, measurement_minusMeanCtrl] <- ( htm@data[indices_all, measurement] - meancontrol )
        
        # t_test, image based
        
        
    } # experiment loop
    
    
    return(htm@data)
    
}

htmObjectNormalization <- function(htm) {
    
    print("")
    print("Object normalization")
    print("********************")
    print("")
    
    
    data <- htm@objectdata
    
    # get all necessary information
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    experiments <- sort(unique(data[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    transformation <- htmGetListSetting(htm,"statistics","transformation")
    
    cat("\nMeasurement:\n")
    print(measurement)
    cat("\nNegative Control:\n")
    print(negcontrols)
    
    # check whether we know everything            
    if( is.null(experiments) || measurement=="None selected") {
        print("")
        print("  ERROR: cannot perform analysis due to lacking information (see above).")
        gmessage("Error: see R console output.")
        return(htm)
    }
    
    
    
    #if( is.null(data$HTM_qcObjects) ) {
    #  print(" WARNING: there was no QC column; all objects with non NA values will be set to valid!")
    print("Performing quality control:")
    htm <- htmApplyImageQCs(htm)
    print("transferring image QC to the objects")
    htm <- htmGenerateImageObjectLink(htm)
    data$HTM_qcObjects = unlist(lapply(htm@objectdata$HTM_imageID,function(x) htm@data$HTM_qcImages[x]))
    data$HTM_qcObjects = data$HTM_qcObjects & (!is.nan(data[[measurement]]))
    print(paste("Total number of objects",length(data$HTM_qcObjects)))
    print(paste("Valid number of objects",sum(data$HTM_qcObjects)))
    
    
    #} else {
    #  data$HTM_qcObjects = (data$HTM_qcObjects) & (!is.nan(data[[measurement]]))
    #} 
    
    #
    htmNormName = "HTM_Norm"
    
    
    # remove previously computed columns
    idsRemove = which(grepl(htmNormName,colnames(data)))
    data[,idsRemove] <- NULL
    
    
    if(transformation == "log2") {
        # compute log transformation
        # create new column name
        logScoreName = paste(htmNormName,measurement,"log2",sep="__")
        data[[logScoreName]] <- log2(data[[measurement]]) 
        
        # todo: this should be at a more prominent position
        print("Replacing -Inf in log scores ******************************")
        logScores = data[[logScoreName]]
        finiteLogScores = subset(logScores,is.finite(logScores))
        minimum = min(finiteLogScores)
        idsInf = which(is.infinite(logScores))
        logScores[idsInf] <- minimum
        data[[logScoreName]] <- logScores
        
        #htmAddLog("Replacing Infinities in Log2 Score by")
        #htmAddLog(minimum)
        #htmAddLog("Affected Wells:")
        #for(id in idsInf) {
        #  htmAddLog(htm@wellSummary$treatment[id])
        #  htmAddLog(htm@wellSummary$wellQC[id])
        #  htmAddLog(htm@wellSummary[id,logScoreName])
        #  htmAddLog("")
        #}
    } # if log transformation
    
    
    # select log2 data in case data transformation is selected
    if(transformation == "log2") {
        measurement = logScoreName
    } else {
        measurement = measurement
    }
    
    measurement_minusMeanCtrl = paste(htmNormName,measurement,"minusMeanCtrl",sep="__")
    
    # initialisation
    data[[measurement_minusMeanCtrl]] = NA
    
    # computation
    cat("\nComputing normalisations...\n")
    
    for(experiment in experiments) {
        
        if(experiment %in% experiments_to_exclude) next
        
        print("")
        print(paste("  Experiment:",experiment))
        
        indices_all <- which((data[[htm@settings@columns$experiment]] == experiment))
        indices_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qcObjects))
        
        if("all treatments" %in% negcontrols) {
            indices_controls_ok <- indices_ok
        } else {
            indices_controls_ok <- which((data[[htm@settings@columns$experiment]] == experiment) & (data$HTM_qcObjects) & (data[[htm@settings@columns$treatment]] %in% negcontrols))
        }
        
        print(paste("    Objects Total", length(indices_all)))
        print(paste("    Objects Valid", length(indices_ok)))      
        print(paste("    Objects Valid Control", length(indices_controls_ok)))
        
        # here values are extracted 
        valuescontrol <- data[indices_controls_ok, measurement]
        #print(valuescontrol)
        
        nr_of_controls <-  length(valuescontrol)
        meancontrol <- mean(valuescontrol)    
        sigmacontrol <- sd(valuescontrol) 
        mediancontrol <- median(valuescontrol)
        madcontrol <- mad(valuescontrol)  
        semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
        print(paste("    Control Mean:", meancontrol))
        print(paste("    Control SD:", sigmacontrol))
        print(paste("    Control Median:", mediancontrol))
        print(paste("    Control MAD:", madcontrol))
        
        data[indices_all, measurement_minusMeanCtrl] <- ( data[indices_all, measurement] - meancontrol )
        
        # t_test, image based
        
        
    } # experiment loop
    
    
    return(data)
    
}


htmApplyImageQCs <- function(htm) {
    
    print("Performing Image QC:")
    
    # get image QC dataframe from htm object
    data = htm@data
    qc = htm@settings@qcImages
    
    if(qc[1,1]=="None selected") {
        print("  No image QCs selected. Setting all images to valid.")
        htm@data$HTM_qcImages <- rep(1,nrow(htm@data)) # at this point something happens to the memory of htm... 
    } else {
        # compute QC and put results into htm
        passedQC = dataframeQC(data,qc)  
        # return the modified htm
        htm@data$HTM_qcImages <- passedQC # at this point something happens to the memory of htm...
    }  
    
    print("  (The column HTM_qcImages has been updated or generated.)")
    print("")
    return(htm)
    
}



htmGenerateImageObjectLink <- function(htm, columns=NULL) {
    
    print("Generating columns that link image and object table.")
    if (is.null(columns)) {
        # columns to match images and objects
        c1 = htmGetListSetting(htm,"columns","treatment")
        c2 = htmGetListSetting(htm,"columns","experiment")
        c3 = htmGetListSetting(htm,"columns","wellnum")
        c4 = htmGetListSetting(htm,"columns","posnum")  
        htm@objectdata$HTM_imageobjectlink = paste(htm@objectdata[[c1]],htm@objectdata[[c2]],htm@objectdata[[c3]],htm@objectdata[[c4]],sep="--")
        htm@data$HTM_imageobjectlink = paste(htm@data[[c1]],htm@data[[c2]],htm@data[[c3]],htm@data[[c4]],sep="--")
        htm@objectdata$HTM_imageID = unlist(lapply(htm@objectdata$HTM_imageobjectlink, function(x) which(htm@data$HTM_imageobjectlink==x)))    
    }
    
    return(htm)
}




handler_showImageJitterPlot <-  function(h, ...){
    print("please select a point for viewing!")
    ir = identify(x = jp.x, y = jp.y, n = 1, plot = FALSE)
    #print(ir)
    #showImagesFromRow(ir)
    showImagesFromRow2(ir)
}


htmAddImageQC <- function(htm,.colname,.min,.max) {
    
    qc = htm@settings@qcImages
    
    if(qc[1,1]=="None selected") {
        htm@settings@qcImages <- data.frame(colname=.colname, min=.min, max=.max)
    } else {
        htm@settings@qcImages <- rbind(htm@settings@qcImages,data.frame(colname=.colname, min=.min, max=.max))
    }
    
    #print(paste("added image QC: colname =",.colname,"; min =",.min,"; max =",.max))
    return(htm)
    
}

htmGetImageQCs <- function(htm) {
    
    nImageQCs = nrow(htm@settings@qcImages)
    QCs = vector()
    for(i in 1:nImageQCs) {
        .colname = htm@settings@qcImages[i,1]
        .min =     htm@settings@qcImages[i,2]
        .max =     htm@settings@qcImages[i,3]
        QCs[length(QCs)+1]=paste(.colname,"  min=",.min,"  max=",.max,sep="")
    }
    return(QCs)
    
}

htmRemoveImageQCs <- function(htm, indices) { 
    
    htm@settings@qcImages <- htm@settings@qcImages[-indices,]
    if(nrow(htm@settings@qcImages)==0) {
        htm@settings@qcImages <- data.frame(colname="None selected", min=NA, max=NA)
    }
    
    #print(htm@settings@qcImages)
    return(htm)
    
}


#
# Select a subset of the data
#
# ids <- htmSelectData(htm, treatments=unique(htm@data$Metadata_Well), measurement="HTM__z_score__projection", r=c(1,100), method="random", n=3)
# htmShowDataFromRow(htm,htm@data,ids)

htmSelectData <- function(htm, treatments, measurement, r=c(2,100), method="random", n=5, save_to_disc=FALSE) {
    
    print("*")
    print("* Data selection")
    print("*" )
    print("")
    
    data <- htm@data
    
    cat("\nMeasurement:\n")
    print(measurement)
    cat("\nTreatments:\n")
    print(treatments)
    
    htm <- htmApplyQCs(htm)
    
    print(paste("Total data points",length(data$HTM_qc)))
    print(paste("Valid data points",sum(data$HTM_qc)))
    
    ids_selected = vector()
    
    for(treatment in treatments) {
        
        ids <- which( (data[[htm@settings@columns$treatment]] == treatment) &
                          (data$HTM_qc==1) & 
                          (data[[measurement]] > r[1]) &
                          (data[[measurement]] < r[2]) )
        
        if(method == "random") {
            ids <- sample(ids, min(n,length(ids)))
        }
        
        ids_selected = c(ids_selected, ids)
        print(paste(treatment,"selected",length(ids)))
        
    }
    
    if(save_to_disc) {
        data_subset <- data[ids_selected,]
        saveTable(data_subset)
    }
    
    return(ids_selected)
    
}


htmWellSummary <- function(htm) {
    
    print("")
    print("Image score -> Well score:")
    print("**************************")
    print("")
    
    # get all necessary information
    measurement <- htmGetListSetting(htm,"statistics","measurement")
    method <- htmGetListSetting(htm,"statistics","wellSummaryMethod")
    minNumValidImages <- 1 #htmGetListSetting(htm,"statistics","WellQC_Minimum_Number_Valid_Images")
    minNumObjects <- htmGetListSetting(htm,"statistics","WellQC_Minimum_Number_Objects")
    weighting <- htmGetListSetting(htm,"statistics","objectCount")
    colObjectCount <- htmGetListSetting(htm,"statistics","objectCount")
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    experiments_to_exculde <- htmGetVectorSettings("statistics$experiments_to_exclude")
    
    # output
    #print("");print("Experiments:")
    #print(experiments)
    print("");print("Well Summary Method:")
    print(method)
    print("");print("Measurement:")
    print(measurement)
    print("");print("Weighting with:")
    print(weighting)
    print("");print("Column containing the object count:")
    print(colObjectCount)
    
    
    print("");
    print(paste("Well QC: minimum number of valid images:",minNumValidImages))
    print(paste("Well QC: minimum number of objects",minNumObjects))
    
    
    if(htm@settings@columns$wellnum=="") {
        gmessage("Cannot proceed: you need to specify a Well Column!")
        return(htm)
    }
    
    # check whether we know everything            
    if( is.null(experiments) ||
        (measurement=="None selected") ||
        (method=="None selected") 
    ) {
        print("")
        print("  ERROR: cannot perform analysis due to lacking information (see above).")
        gmessage("Error: see R console output.")
        return(htm)
    }
    
    if( is.null(htm@data$HTM_qcImages) ) {
        print(" WARNING: there is no Image QC column; all images with non NA values will be considered valid!")
    } 
    
    ids_wells = split(1:nrow(htm@data), paste(htm@data[[htm@settings@columns$experiment]], htm@data[[htm@settings@columns$wellnum]]) )
    
    # removing "bad experiments" => why should this be here?
    
    #print(""); print("removing bad wells")
    #ids_to_exclude = vector()
    #for(i in 1:length(ids_wells)) {
    #  
    #  experiment = htm@data[ids_wells[[i]][1],htm@settings@columns$experiment]
    #  
    #  if( experiment %in% experiments_to_exculde ) {
    #    ids_to_exclude = append(ids_to_exclude,i)
    #  }
    #  
    #}
    #print(ids_to_exclude)
    #print(length(ids_wells))
    #if(length(ids_to_exclude)) {
    #  ids_wells = ids_wells[-ids_to_exclude]
    #}
    #print(length(ids_wells))
    
    
    numEntries = length(ids_wells) 
    
    results <- data.frame(wellNum=rep(NA,numEntries),
                          numObjectsOK=rep(NA,numEntries),
                          wellQC=rep(NA,numEntries),
                          numImages=rep(NA,numEntries),
                          numImagesOK=rep(NA,numEntries),
                          objectsPerImage=rep(NA,numEntries),
                          experiment=rep(NA,numEntries),
                          treatment=rep(NA,numEntries),
                          stringsAsFactors = FALSE)
    
    
    # init columns for the scores that are obtained by averaging the images in the well  
    
    # average
    scorename <- paste("wellscore",measurement,method,sep="__")
    results[[scorename]] <- rep(NA,numEntries)
    
    # average deviation
    if( method == "mean_of_images" ) {
        wellscoredevmethod <- "sd"
    } else if (method == "median_of_images") {              
        wellscoredevmethod <- "MAD"
    } else if (method== "weighted_mean_of_images") {
        wellscoredevmethod <- "NotSureYet"
    }
    print(method)
    #print(wellscoredevmethod)
    scoredevname <- paste("wellscore_deviation",measurement,method,wellscoredevmethod,sep="__")
    #print(scoredevname)
    results[[scoredevname]] <- rep(NA,numEntries)
    #print(colnames(results))
    
    # start computing the well average from the images 
    i = 0
    print("Analyzing.....")
    
    for(ids in ids_wells) {
        
        if( is.null(htm@data$HTM_qcImages) ) {
            idsOK = ids[which(!is.na(htm@data[ids,measurement]))]
        } else {
            idsOK = ids[ which( (htm@data$HTM_qcImages[ids]==1) & (!is.na(htm@data[ids,measurement])) ) ]
        }
        
        ## QC
        
        wellQC <- 1
        
        if( length(idsOK) < minNumValidImages ) wellQC <- 0
        
        if(colObjectCount != "None selected") {
            numObjectsOK <- sum(htm@data[idsOK,colObjectCount])
            if( is.na(numObjectsOK) || (numObjectsOK < minNumObjects) ) wellQC <- 0
        } else {
            numObjectsOK <- NA
        }
        
        
        i=i+1
        results$experiment[i] <- htm@data[ids[1],htm@settings@columns$experiment]
        results$treatment[i] <- htm@data[ids[1],htm@settings@columns$treatment]
        results$wellNum[i] <- htm@data[ids[1],htm@settings@columns$wellnum]
        results$numImagesOK[i] <- length(idsOK)
        results$numImages[i] <- length(ids)
        results$numObjectsOK[i] <- numObjectsOK
        results$objectsPerImage[i] <- numObjectsOK/length(idsOK)
        # print(scorename)
        # print(wellscore)
        
        
        if( method == "mean_of_images" ) {
            wellscore <- mean(htm@data[idsOK,measurement])
            wellscoredeviation <- sd(htm@data[idsOK,measurement])
        } else if (method == "median_of_images") {              
            wellscore <- median(htm@data[idsOK,measurement])
            wellscoredeviation <- mad(htm@data[idsOK,measurement])
        } else if (method== "weighted_mean_of_images") {
            wellscore <- sum( htm@data[idsOK,measurement]*htm@data[idsOK,weighting] ) / sum(htm@data[idsOK,weighting])
            wellscoredeviation <- NA
        } 
        
        results[i,scorename] <- wellscore
        results[i,scoredevname] <- wellscoredeviation
        
        
        if( is.na(wellscore)) wellQC <- 0
        results$wellQC[i] <- wellQC
        
    } # well loop
    
    measurement <- scorename
    
    
    # put wellscores into HTM object
    htm@wellSummary <- results
    
    print(paste("  Wells Total:",length(results$wellQC)))
    print(paste("  Wells OK:",sum(results$wellQC)))
    print(paste("  Images Total:",sum(results$numImages)))
    print(paste("  Images OK:",sum(results$numImagesOK)))
    
    
    htmAddLog("Well Normalisation:")
    htmAddLog("*******************")
    
    experiments <- unique(htm@data[[htm@settings@columns$experiment]])   
    #negcontrols <- htm@settings@ctrlsNeg
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    #normmethod <- htmGetListSetting(htm,"statistics","normalisationMethod")
    transformation <- htmGetListSetting(htm,"statistics","transformation")
    
    # output
    htmClearLog()
    htmAddLog("");htmAddLog("Experiments:")
    htmAddLog(experiments)
    htmAddLog("");htmAddLog("Negative controls:")
    htmAddLog(negcontrols)
    #htmAddLog("");htmAddLog("Well normalisation method:")
    #htmAddLog(normmethod)
    htmAddLog("");htmAddLog("Measurement:")
    htmAddLog(measurement);
    htmAddLog("");
    
    # check whether we know everything            
    if( is.null(experiments) ||
        (negcontrols=="None selected") ||
        #(normmethod=="None selected") || 
        (measurement=="None selected")
    ) 
    {
        print("")
        print("  ERROR: cannot perform analysis due to lacking information (see above).")
        gmessage("Error: see R console output.")
        
        print("")
        return(htm)
    }
    
    # todo:
    # replace below print functions with showinfo, which could be turned off
    
    
    # indicate the negative controls
    #htm@wellSummary$negCtrl <- rep(control,nrow(htm@wellSummary))
    htm@wellSummary$negCtrl <- rep(paste(negcontrols,collapse="--"),nrow(htm@wellSummary))
    
    if(transformation == "log2") {
        
        print("Computing log2 transformation")
        # compute log transformation
        logScoreName = paste("log2",measurement,sep="__")
        htm@wellSummary[[logScoreName]] <- log2(htm@wellSummary[[measurement]]) 
        
        # todo: this should be at a more prominent position
        logScores = htm@wellSummary[[logScoreName]]
        finiteLogScores = subset(logScores,is.finite(logScores))
        minimum = min(finiteLogScores)
        print(paste("  Replacing -Inf in log scores by ",minimum))
        
        idsInf = which(is.infinite(logScores))
        logScores[idsInf] <- minimum
        htm@wellSummary[[logScoreName]] <- logScores
        
        #htmAddLog("Replacing Infinities in Log2 Score by")
        #htmAddLog(minimum)
        #htmAddLog("Affected Wells:")
        #for(id in idsInf) {
        #  htmAddLog(htm@wellSummary$treatment[id])
        #  htmAddLog(htm@wellSummary$wellQC[id])
        #  htmAddLog(htm@wellSummary[id,logScoreName])
        #  htmAddLog("")
        #}
        
        measurement = logScoreName
        
    } # if log transformation
    
    
    if(htmGetListSetting(htm,"statistics","gradientCorrection") == "medpolish") {
        
        print("Apply medpolish")
        
        medpolish_name =  paste("medpolish",measurement,sep="__")
        
        htm@wellSummary[[medpolish_name]] = rep(NA,nrow(htm@wellSummary))
        
        for(experiment in experiments) {
            
            indices_all <- which((htm@wellSummary$experiment == experiment))
            
            # extract values
            htm@wellSummary[indices_all, measurement]
            xy = htm_convert_wellNum_to_xy(htm@wellSummary[indices_all, "wellNum"]) 
            print(length(xy$x))
            print(htm@wellSummary[indices_all, measurement])
            mp = htmMedpolish(x=xy$x, y=xy$y, val=htm@wellSummary[indices_all, measurement])
            print(mp$residuals)
            print(indices_all)
            htm@wellSummary[indices_all, medpolish_name] = mp$residuals
            
        } # experiment loop
        
        measurement = medpolish_name
        
    } #medpolish
    
    
    print("")
    print("Computing Normalisations...")
    
    zScoreName = paste("zScore",measurement,sep="__")
    robust_z_score_name = paste("robust_z_score",measurement,sep="__")
    minusMeanCtrlName = paste("minusMeanCtrl",measurement,sep="__")
    
    htm@wellSummary[[zScoreName]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[minusMeanCtrlName]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[robust_z_score_name]] <- rep(NA,nrow(htm@wellSummary))
    
    
    # init documentation of control values
    
    if(transformation == "log2") {
        ctrls_mean_name = "ctrls__log2__position_mean"
        ctrls_median_name = "ctrls__log2__position_median"
        ctrls_sd_name = "ctrls__log2__position_sd"
        ctrls_mad_name = "ctrls__log2__position_mad"        
    } else {
        ctrls_mean_name = "ctrls__position_mean"
        ctrls_median_name = "ctrls__position_median"
        ctrls_sd_name = "ctrls__position_sd"
        ctrls_mad_name = "ctrls__position_mad"      
    }
    
    htm@wellSummary[[ctrls_mean_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_median_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_sd_name]] <- rep(NA,nrow(htm@wellSummary))
    htm@wellSummary[[ctrls_mad_name]] <- rep(NA,nrow(htm@wellSummary))
    
    
    for(experiment in experiments) {
        print("")
        #print(paste("  Experiment:",experiment))
        
        indices_all <- (htm@wellSummary$experiment == experiment)
        indices_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$wellQC)
        if("all treatments" %in% negcontrols) {
            indices_controls_ok <- indices_ok
        } else {
            #indices_controls_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$treatment == control) & (htm@wellSummary$wellQC) 
            # combine different controls:
            indices_controls_ok <- (htm@wellSummary$experiment == experiment) & (htm@wellSummary$treatment %in% negcontrols) & (htm@wellSummary$wellQC)       
        }
        
        #print(paste("    Wells Total", sum(indices_all)))
        #print(paste("    Wells Valid", sum(indices_ok)))      
        #print(paste("    Wells Valid Control", sum(indices_controls_ok)))
        
        # here values are extracted 
        valuescontrol <- htm@wellSummary[indices_controls_ok, measurement]
        print(measurement)
        
        
        if(0) {
            if("all treatments" %in% negcontrols) {
                print("") 
            } else {
                print("    Valid Control Wells:")
                for (id in which(indices_controls_ok==T)) {
                    print(paste("      WellNum:",htm@wellSummary$wellNum[id]))
                    print(paste("        Treatment:",htm@wellSummary$treatment[id]))
                    print(paste("        Value:",htm@wellSummary[[measurement]][id]))
                    print(paste("        Objects_OK:",htm@wellSummary$numObjectsOK[id]))
                    print(paste("        Images_OK:",htm@wellSummary$numImagesOK[id]))
                }
            }
        }
        
        
        nr_of_controls <-  length(valuescontrol)
        meancontrol <- mean(valuescontrol)    
        sigmacontrol <- sd(valuescontrol) 
        mediancontrol <- median(valuescontrol)
        madcontrol <- mad(valuescontrol)  
        semcontrol <- sigmacontrol/sqrt(nr_of_controls)     
        print(paste("    Control Mean:", meancontrol))
        print(paste("    Control StdD:", sigmacontrol))
        print(paste("    Control Median:", mediancontrol))
        print(paste("    Control MAD:", madcontrol))
        
        htm@wellSummary[indices_all, ctrls_mean_name] <- meancontrol
        htm@wellSummary[indices_all, ctrls_sd_name] <- sigmacontrol
        htm@wellSummary[indices_all, ctrls_median_name] <- mediancontrol
        htm@wellSummary[indices_all, ctrls_mad_name] <- madcontrol
        
        # computed scores on a well level
        htm@wellSummary[indices_all, zScoreName] <- ( htm@wellSummary[indices_all, measurement] - meancontrol ) / ( sigmacontrol )
        htm@wellSummary[indices_all, robust_z_score_name] <- ( htm@wellSummary[indices_all, measurement] - mediancontrol ) / ( madcontrol )
        htm@wellSummary[indices_all, minusMeanCtrlName] <- ( htm@wellSummary[indices_all, measurement] - meancontrol )
        
    } # experiment loop
    
    
    
    # output
    print("");print("Experiments:")
    print(experiments)
    print("");print("Negative controls:")
    print(negcontrols)
    #print("");print("Well normalisation method:")
    #print(normmethod)
    print("");print("Measurement:")
    print(measurement)
    
    return(htm@wellSummary)
    
}




#

guiHandler_AverageAndNormaliseMultipleFeatures <- function(h,...){
    
    if(is.null(htm@settings@columns$treatment)) {
        gmessage("You need to first specify the treatment column [Main..Configure..Assay columns]!")
        return(NULL)
    }
    
    
    w <- gwindow("Statistical Analysis", visible=F)
    
    # gui_ListSetting <- function(text, setting, key, choices, container) {
    
    
    htm <- get("htm", envir = globalenv())
    
    if( htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T)
    { 
        
        gui_AddRemoveVectorSetting(setting="statistics$ObjectFeatureSelection",
                                   name=" Object features to be analyzed: ",
                                   choices = colnames(htm@objectdata),
                                   container = w, showSelected=F)  
        
    } else {
        
        gui_AddRemoveVectorSetting(setting="statistics$ImageFeatureSelection",
                                   name=" Image features to be analyzed: ",
                                   choices = colnames(htm@data),
                                   container = w, showSelected=F)  
        
        gui_ListSettingDropdown(text = "  Method to average images within one position (well): ",
                                setting = "statistics",
                                key = "wellSummaryMethod",
                                choices = c("weighted_mean_of_images","mean_of_images","median_of_images"),
                                default = "weighted_mean_of_images",
                                container = w)
        
        gui_ListSettingDropdown(text = "  Number of objects per image:  ",
                                setting = "statistics",
                                key = "objectCount",
                                choices = colnames(htm@data),
                                default = colnames(htm@data)[1],
                                container = w)
        
        gui_ListSettingTextfield(text = "  Well QC: Minimum number of valid objects:  ",
                                 setting = "statistics",
                                 key = "WellQC_Minimum_Number_Objects",
                                 type = "numeric",
                                 default = 100,
                                 container = w)
        
        
        htmSetListSetting(htm, "statistics","treatmentWithinReplicateSummaryMethod","mean_of_wells", gui=T)
        
    } # image/well based stats
    
    
    
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
    
    
    obj <- glabel("   ", container = w)
    gg <- ggroup(horizontal = TRUE, container=w, expand=T)
    obj <- gbutton(" Analyze", container = gg, handler = function(h,...) {
        
        if(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T) { 
            featureList = htmGetVectorSettings("statistics$ObjectFeatureSelection") 
        } else {
            featureList = htmGetVectorSettings("statistics$ImageFeatureSelection") 
        }
        
        # initialisation of the treatment stats
        htm <- get("htm", envir = globalenv())
        htm@other$treatmentSummaryList <- NULL; htm@other$treatmentSummaryList = list()
        htm@other$treatmentSummaryMerge <- NULL; htm@other$treatmentSummaryMerge = data.frame()
        assign("htm", htm, envir = globalenv())
        
        for (feature in featureList) {
            
            # image QC
            htm <- get("htm", envir = globalenv())
            htm <- htmApplyImageQCs(htm)
            assign("htm", htm, envir = globalenv())
            
            print(feature)
            htmSetListSetting(htm,"statistics","measurement",feature,gui=T)
            
            # well summary and normalisation (based on per_image values)
            if(!(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T)) { 
                htm <- get("htm", envir = globalenv())
                htm@wellSummary <- htmWellSummary(htm)
                assign("htm", htm, envir = globalenv())
            }
            
            # image normalisation for image based statistics
            if( htmGetListSetting(htm,"statistics","compute_image_based_stats_TF", gui=T) == T ) {
                htm <- get("htm", envir = globalenv())
                htm@data <- htmImageNormalization(htm)
                assign("htm", htm, envir = globalenv())
            }
            
            # cell normalisation for cell based statistics
            if(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T ) {
                htm <- get("htm", envir = globalenv())
                htm@objectdata <- htmObjectNormalization(htm)
                assign("htm", htm, envir = globalenv())
            }
            
            # treatment statistics: well based, image based and cell based
            htm <- get("htm", envir = globalenv())
            htm@treatmentSummary <- htmTreatmentSummary(htm)
            assign("htm", htm, envir = globalenv())    
            
            # save treatment summary
            path = gfile("Save as...", type="save", initialfilename = paste0("TreatmentSummary--",htmGetListSetting(htm,"statistics","transformation",gui=T),"--",htmGetListSetting(htm,"statistics","measurement",gui=T),".csv"))
            htmSaveDataTable(htm, "treatmentSummary", path)
            
            # store all the treatment summaries in a list
            if( htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T ) {
                readouts = c('t_test__objects__p_value','z_score__allBatches__per_object')
            } else {
                readouts = c('z_score__allBatches')
            }
            
            tablename = paste0("TreatmentSummary__",feature)
            htm <- get("htm", envir = globalenv())
            htm@other$treatmentSummaryList[[tablename]] <- data.frame(htm@treatmentSummary) # this syntax ensures that the data really is copied and not only a pointer to htm is stored in the list
            
            for(readout in readouts) { # add featurename to columnname
                colnames(htm@other$treatmentSummaryList[[tablename]])[which(names(htm@other$treatmentSummaryList[[tablename]] ) == readout)] <- paste(readout,feature,sep="__")
            }
            #print(readout)
            #print(colnames(htm@other$treatmentSummaryList[[tablename]]))
            assign("htm", htm, envir = globalenv())    
            
        }
        
        
        #if(0) {
        #  l <- htmImageMultiFeatureAnalysis(htm, readout)
        #  htm@data <- l$images
        #  htm@other$treatFeat <- l$treatFeat
        #  htm@other$MDS <- htmMDStreatFeat(htm@other$treatFeat, negCtrl=htmGetListSetting(htm,"statistics","negativeControl"))
        #  assign("htm", htm, envir = globalenv())    
        #  htmHeatmap_treatFeat(htm@other$treatFeat,-3,3)
        #}
        
        
        # compute summary table with all features
        if(length(readouts)>1) {
            print("")
            print("More than one readout selected. Merging into one table...")
            
            for (readout in readouts) {
                
                mergeTableName = paste0(readout,"__merged")
                
                # compute summary table with all features
                print("Joining results...")
                htm <- get("htm", envir = globalenv())
                d <- join_all(htm@other$treatmentSummaryList,"treatment")
                htm@other[[mergeTableName]] <- subset(d,select=c("treatment",colnames(d)[which(grepl(paste0("^",readout),colnames(d)))]))
                # move treatment annotation from colunm to rownames
                rownames(htm@other[[mergeTableName]]) <- htm@other[[mergeTableName]]$treatment
                htm@other[[mergeTableName]]$treatment <- NULL
                assign("htm", htm, envir = globalenv())    
                
                # show heatmap
                print(readout)
                
                # save table
                if(!(htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF", gui=T) == T)) { 
                    plotHeatmap_treatFeat(htm@other[[mergeTableName]], -3, 3, rotate=T, readout=readout)
                    initialfilename = paste0("MultiFeatureAnalysis__",readout,"__per_well_stats.csv")
                } else {
                    plotHeatmap_treatFeat(htm@other[[mergeTableName]], -2, 2, rotate=T, readout=readout)
                    initialfilename = paste0("MultiFeatureAnalysis__",readout,"__per_cell_stats.csv")
                }
                path = gfile("Save as...", type="save", initialfilename = initialfilename)
                htmSaveDataTable(htm, paste0("other$",mergeTableName), path)
            }
            
            
            # compute MDS
            #htm@other$MDS <- htmMDStreatFeat(htm@other$treatSumMerge,negCtrl=htmGetListSetting(htm,"statistics","negativeControl"))
            #scatterLabelPlot_treatFeat(htm@other$MDS,"x","y")
            
            #htm@other$treatSumMerge <- subset(d,select=c("treatment",colnames(d)[which(grepl(paste0("^",readout),colnames(d)))]))
            
        }
        
        ## image based multidimensional analysis
        
        
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


# old
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

# old
guiHandler_AverageAndNormalise_Options <- function(h,...){
  
  w <- gwindow("Statistical Analysis", visible=F)
  
  gui_ListSettingDropdown(text = "  Positive control  ",
                          setting = "statistics",
                          key = "positiveControl",
                          choices = c("None selected",sort(unique(htm@data[[htm@settings@columns$treatment]]))),
                          default = "None selected",
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
  
  print("Compute cell based statistics?")
  gcheckbox("Compute cell based statistics?", 
            checked = htmGetListSetting(htm,"statistics","compute_cell_based_stats_TF",gui=T), 
            container = w, 
            handler = function(h,...){
              htmSetListSetting(htm, "statistics","compute_cell_based_stats_TF",svalue(h$obj), gui=T)
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
  
  experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
  treatments <- sort(unique(htm@data[[htm@settings@columns$treatment]]))
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("x axis:", container=gp)
  cx <- gcombobox(c("None selected",columns), 
                  selected = htmGetListSetting(htm,"visualisation","scatterPlotX",gui=T), container=gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm, "visualisation","scatterPlotX",svalue(h$obj),gui=T)
                  })
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("y axis:", container=gp)
  cy <- gcombobox(c("None selected",columns), 
                  selected = htmGetListSetting(htm,"visualisation","scatterPlotY",gui=T), container=gp, 
                  handler = function(h,...){
                    htmSetListSetting(htm,"visualisation","scatterPlotY",svalue(h$obj),gui=T)
                  })
  
  
  gp <- ggroup(horizontal = T, container=w)
  glabel("Experiment selection:  ", container=gp)
  guiExpSubset <- gcombobox(c("None selected", experiments), 
                            selected = htmGetListSetting(htm,"visualisation","scatterPlotExpSubset",gui=T), 
                            container = gp, 
                            handler = function(h,...){
                              htmSetListSetting(htm,"visualisation","scatterPlotExpSubset",svalue(h$obj),gui=T)
                            })
  
  glabel(" ", container=w)
  gui_AddRemoveVectorSetting(setting="visualisation$treatmentSelectionForPlotting",
                             name=" Treatment selection: ",
                             choices = c("None selected", treatments),
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
  
  gbutton("Click & View", container = gp, handler = function(h, ...){
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

