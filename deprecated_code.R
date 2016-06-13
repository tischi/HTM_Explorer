

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

