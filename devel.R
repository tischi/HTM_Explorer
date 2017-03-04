
guiHandler_ScatterPlotly_Data <- function(h,...){
    
    
    if(is.null(htm@settings@columns$experiment)) {
        gmessage("You need to specify the experiment and treatment columns [Main..Configure..Assay columns]")
        return(NULL)
    }
    
    
    w <- gwindow("Scatter Plotly", visible = F)
    print("Scatter Plot") 
    
    
    if (htmGetListSetting(htm,"visualisation","jitterPlot_datatype",gui=T)=="None selected")  {
        htmSetListSetting(htm,"visualisation","jitterPlot_datatype","images",gui=T)  
    }
    
    
    columns <- c("None selected", sort(colnames(htm@data)))   
    experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    treatments <- sort(unique(htm@data[[htm@settings@columns$treatment]]))
    
    
    gp <- ggroup(horizontal = T, container=w)
    glabel("x axis:  ", container=gp)
    cx <- gcombobox(columns, 
                    selected = htmGetListSetting(htm,"visualisation","scatterPlotX",gui=T), container=gp, 
                    handler = function(h,...){
                        htmSetListSetting(htm, "visualisation","scatterPlotX",svalue(h$obj),gui=T)
                    })
    
    gp <- ggroup(horizontal = T, container=w)
    glabel("y axis:  ", container=gp)
    cy <- gcombobox(columns, 
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
    
    
    gp <- ggroup(horizontal = T, container=w)
    glabel("color by:  ", container=gp)
    gcombobox(columns, 
              selected = htmGetListSetting(htm,"visualisation","scatterPlotColor",gui=T), container=gp, 
              handler = function(h,...){
                  htmSetListSetting(htm,"visualisation","scatterPlotColor",svalue(h$obj),gui=T)
              })
    
    
    gp <- ggroup(horizontal = T, container=w)
    glabel("aggregate by:  ", container=gp)
    gcombobox(columns, 
              selected = htmGetListSetting(htm,"visualisation","scatterPlotAggregate",gui=T), container=gp, 
              handler = function(h,...){
                  htmSetListSetting(htm,"visualisation","scatterPlotAggregate",svalue(h$obj),gui=T)
              })
    
    
    glabel(" ", container=w)
    gp <- ggroup(horizontal = T, container=w)
    gbutton("Show plot", container = gp, handler = function(h,...) {
        
        print("sourcing")
        htm <- get("htm", envir = globalenv()) 
        htm@settings@visualisation$cx = svalue(cx)
        htm@settings@visualisation$cy = svalue(cy)
        assign("htm", htm, envir = globalenv())  
        print(shinyApp(ui = ui_scatter_plot, server = server_scatter_plot))
        
        
        #htmScatterPlotly_Data(get("htm", envir = globalenv()), 
        #                    svalue(cx), svalue(cy), 
        #                    experimentSubset = svalue(guiExpSubset),
        #                    treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
        #                    aggregate = htmGetVectorSettings("visualisation$scatterPlotAggregate"),
        #                    colorize = htmGetVectorSettings("visualisation$scatterPlotColor"),
        #                    newdev = T) 
    })
    
    gbutton("Zoom", container = gp, handler = function(h, ...){
        loc = locator(n=2)
        htmScatterPlot_Data(get("htm", envir = globalenv()), 
                            svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y), 
                            treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                            aggregate = htmGetVectorSettings("visualisation$scatterPlotAggregate"),
                            colorize = htmGetVectorSettings("visualisation$scatterPlotColor"),
                            newdev = F)
    })
    
    gbutton("Click & View", container = gp, handler = function(h, ...){
        if(dev.cur()==1) {
            print("No plot open.")
        } else {
            htmScatterPlot_Data(get("htm", envir = globalenv()), 
                                svalue(cx), svalue(cy), .xlim=sort(loc$x),.ylim=sort(loc$y), 
                                treatmentSubset = htmGetVectorSettings("visualisation$treatmentSelectionForPlotting"),
                                aggregate = htmGetVectorSettings("visualisation$scatterPlotAggregate"),
                                colorize = htmGetVectorSettings("visualisation$scatterPlotColor"),
                                newdev=F , action="click")
        }
    })
    
    gbutton(" Options ", container = gp, handler = guiHandler_ScatterPlot_Options)
    
    #obj <- gbutton("Zoom (not working yet)", editable=FALSE, container = gp, handler = handler_zoomJitterPlot )
    #obj <- gbutton("Click and view image", editable=TRUE, container = gp, handler = handler_showImageJitterPlot )
    
    visible(w) <- T
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



htmScatterPlotly_Data <- function(htm, cx, cy, .xlim=NA, .ylim=NA, colorize="None selected", aggregate = "None selected", experimentSubset="None selected", treatmentSubset="None selected", newdev=T, action="plot") {
    
    
    print("")
    print("")
    print("Scatter Plotly")
    
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
        
        #if(newdev){
        #  dev.new()
        #}
        
        dotsize = 0.75
        #op <- par(mar = c(10,10,4,2) + 0.1) 
        
        if(htmGetListSetting(htm,"visualisation","scatterPlot_scaleFromZero_TF")==T) {
            .ylim = c(0, max(vy))
            .xlim = c(0, max(vx))
        }
        
        
        
        # actual plotting
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
        
        
        if( colorize != "None selected") {
            p <- plot_ly(x=vx, y=vy, mode = "markers", text=treatments, color=.colors)
        } else {
            p <- plot_ly(x=vx, y=vy, mode = "markers", text=treatments)
        }
        p <- layout(p, title = plotTitle, xaxis = list(title = cx), yaxis = list(title = cy)) 
        print(p)
        
        
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
        
        return(0)
        
        
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



server_scatter_plot <- function(input, output, session) {
    
    output$plot <- renderPlotly({
        vx <- htm@data[[htm@settings@visualisation$cx]]
        vy <- htm@data[[htm@settings@visualisation$cy]]
        plot_ly(x = vx, y = vy, mode = "markers") 
        layout(p, xaxis = list(title = ""), 
               yaxis = list(title = ""))
    })
    
    output$info <- renderPrint({
        print(htm@settings@visualisation$cx)
        print(htm@settings@visualisation$cy)
    })
    
    output$selection <- renderPrint({
        s <- event_data("plotly_click")
        if (length(s) == 0) {
            "Click on a data point!"
        } else {
            print("You selected:")
            print(s)
            i = s[['pointNumber']]
            print(data[i,])
            htmShowDataFromRow(htm,data,i)
            #print("Launching Fiji now....")
            #system(fiji_binary,wait=F)
        }
    })
    
}


ui_scatter_plot <- fluidPage(
    mainPanel(
        plotlyOutput("plot")
    ),
    verbatimTextOutput("selection"),
    verbatimTextOutput("info")
    
)


htmMaxDeviationPerChannel <- function(treatFeat) {
    
    treatFeat = htm@other$treatFeat
    treatFeat[apply(treatFeat, 2, Negate(is.finite))] <- NA 
    treatFeat<-treatFeat[,-which(grepl("Max",colnames(treatFeat)))]
    #treatFeat<-treatFeat[,which(grepl("IntegratedIntensity_",colnames(treatFeat)))]
    treatFeat<-treatFeat[,which(grepl("Texture_",colnames(treatFeat)))]
    
    
    idsChannels = split(1:ncol(treatFeat), unlist(lapply(strsplit(colnames(treatFeat),"_"), function(x) {x[3]})))
    print(idsChannels)
    
    for (ichannel in 1:nrow(treatFeat)) {
        print(rownames(treatFeat)[ichannel])
        
        for (ids in idsChannels) {
            iMax = ids[which.max(abs(treatFeat[ichannel,ids]))]
            print(paste(colnames(treatFeat)[iMax], treatFeat[ichannel,iMax]))
        }
        
    }
    
}

htmAnalyseMahalanobis <- function(htm, d, selectSubset = "") {
    # input:
    # rows: treat_cells
    # cols: features
    
    treatments <- unique(unlist(lapply(strsplit(rownames(d),"_"), function(x) x[1])))
    channels <- unique(unlist(lapply(strsplit(colnames(d),"xx"), function(x) {x[2]})))
    
    N = ncol(d)
    
    
    #for (channel in channels) {
    #  selectSubset = ""
    #  print(channel)
    
    d$SelfMahalanobis <- NA
    
    for (treatment in treatments) {
        
        idsTreatment = which(grepl(treatment,rownames(d)))
        x <- d[idsTreatment,]
        
        if(selectSubset != "") {
            ids = which(grepl(selectSubset, colnames(x)))
            x <- x[,ids]
        }
        
        ids = which( !grepl("SelfMahalanobis", colnames(x)))
        x <- x[,ids]
        
        N = ncol(x)
        
        Sx <- cov(x)
        D2 <- mahalanobis(x, colMeans(x), Sx)
        d[names(D2),"SelfMahalanobis"] = D2 / N
        
        # minimum:
        i = which.min(d[idsTreatment,"SelfMahalanobis"])
        iObject = as.numeric(strsplit(rownames(d)[idsTreatment[i]],"_")[[1]][2])
        print(paste(treatment,"i",idsTreatment[i],"iObjectdata",iObject))
        #htmShowObjectsFromRow(htm,iObject)
        
        # maximum:
        #i = which.max(d[idsTreatment,"SelfMahalanobis"])
        #iObject = as.numeric(strsplit(rownames(d)[idsTreatment[i]],"_")[[1]][2])
        #print(paste(treatment,"i",idsTreatment[i],"iObjectdata",iObject))
        #htmShowObjectsFromRow(htm,iObject)
        
    }  
    
    #}# channel loop
    
    return(d)
    
}

htmObjectMultiFeatureAnalysis <- function(htm, centerChannel, stats="classic", perBatchNorm = F, divideByCellArea = F) {
    
    print("")
    print("Image-based multi-feature analysis:")
    print("***********************************")
    print("")
    
    data <- htm@objectdata
    
    # get all necessary information
    treatments <- sort(unique(htm@objectdata[[htm@settings@columns$treatment]]))
    features_channels <- htmGetVectorSettings("statistics$multipleFeatureSelection")
    negative_ctrl <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    experiments <- sort(unique(htm@objectdata[[htm@settings@columns$experiment]]))
    experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    
    # output
    print("");print("Experiments:")
    print(experiments)
    print("");print(paste("Number of Treatments:",length(treatments)))
    print("");print(paste("Negative Control:",negative_ctrl))
    print(""); print("")
    
    numEntries = length(treatments) 
    
    #channels <- unique(unlist(lapply(strsplit(features_channels,"xx"), function(x) {x[2]})))
    #features <- unique(unlist(lapply(strsplit(features_channels,"xx"), function(x) {paste(x[1],x[3],sep=".*")})))
    
    channels_to_exclude = c("Vimentin","Cla")
    
    
    if(divideByCellArea) {
        
        htm@objectdata[[htm@settings@columns$treatment]]
        
        cNorm <- htm@objectdata[["Shape_xxCELLxx_Area"]]
        
        for (feature_channel in features_channels) {
            
            htm@objectdata[[feature_channel]] = htm@objectdata[[feature_channel]] / cNorm
        }
        
        # htm@objectdata[["Shape_xxCELLxx_Area"]] <- cNorm
        
    }
    
    for (feature_channel in features_channels) {
        
        #feature_channels = colnames(htm@objectdata)[which(grepl(feature,colnames(htm@objectdata)))]
        
        #varTotal = 0
        
        #vars = vector()
        #for(feature_channel in feature_channels) {
        
        #  vars <- append(vars, sd(htm@objectdata[[feature_channel]])^2)
        #  print(paste(feature_channel, sqrt(sd(htm@objectdata[[feature_channel]])^2)))
        
        for(experiment in experiments) {
            
            # normalise to control
            # find control cells
            if(perBatchNorm) {
                idsCtrl = which( (htm@objectdata[[htm@settings@columns$treatment]]==negative_ctrl) 
                                 & (htm@objectdata[[htm@settings@columns$experiment]]==experiment) )
            } else {
                idsCtrl = which( (htm@objectdata[[htm@settings@columns$treatment]]==negative_ctrl))
            }
            
            
            meanCtrlLog2 = mean(log2(htm@objectdata[idsCtrl, feature_channel]))
            meanCtrl = mean(htm@objectdata[idsCtrl, feature_channel])
            sdCtrl = sd(htm@objectdata[idsCtrl, feature_channel])
            medianCtrl = median(htm@objectdata[idsCtrl, feature_channel])
            madCtrl = mad(htm@objectdata[idsCtrl, feature_channel])
            
            ids = which( (htm@objectdata[[htm@settings@columns$experiment]]==experiment) )
            
            channel = strsplit(feature_channel,"xx")[[1]][2]
            feature_channel_Norm = paste0(channel,"_",feature_channel,"__Norm")
            feature_channel_raw = paste0(channel,"_",feature_channel,"__raw")
            
            htm@objectdata[ids, feature_channel_raw] = htm@objectdata[ids, feature_channel]
            
            if(stats=="classic") {
                htm@objectdata[ids, feature_channel_Norm] = (htm@objectdata[ids, feature_channel] - meanCtrl) / sdCtrl
            }
            
            if(stats=="robust") {
                htm@objectdata[ids, feature_channel_Norm] = (htm@objectdata[ids, feature_channel] - medianCtrl) / madCtrl
            } 
            
            if(stats=="log2") {
                htm@objectdata[ids, feature_channel_Norm] = log2(htm@objectdata[ids, feature_channel]) - meanCtrlLog2 
            } 
            
            if(stats=="none") {
                htm@objectdata[ids, feature_channel_Norm] = htm@objectdata[ids, feature_channel] 
            } 
            
            
        }
        
        #}
        
        
        # variance normalisation
        #iCenter = which(grepl(centerChannel,feature_channels))
        #meanCenter = mean(htm@objectdata[[feature_channels[iCenter]]])
        #print(paste("Centering around",feature_channels[iCenter]))
        #print(paste("*** Standard deviation",feature,sqrt(mean(vars)),length(vars)))
        #print("")
        
        #for(feature_channel in feature_channels) {
        
        #  # channel = strsplit(feature_channel,"xx")[[1]][2]
        #  feature_channel_varNorm = paste0(feature_channel,"__VarNorm")
        #  htm@objectdata[[feature_channel_varNorm]] = (htm@objectdata[[feature_channel]] - meanCenter) / sqrt(mean(vars))
        
        #}
        
        
    }
    
    
    l = list()
    
    # varNorm
    #ids = which(grepl("__VarNorm",colnames(htm@objectdata)) )
    #d = htm@objectdata[ids]
    #print(paste(htm@objectdata[[htm@settings@columns$treatment]],ids,sep="_"))
    #rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]],seq(1:nrow(htm@objectdata)),sep="_")
    #d <- d[,order(colnames(d))]
    #d <- d[order(rownames(d)),]
    #l$cells_varNorm <- d
    
    # zScoreNorm
    ids = which( grepl("__Norm",colnames(htm@objectdata))  )
    d = htm@objectdata[ids]
    d$treatment = htm@objectdata[[htm@settings@columns$treatment]]
    d$date = htm@objectdata[[htm@settings@columns$experiment]]
    rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]], htm@objectdata[[htm@settings@columns$experiment]], formatC(seq(1:nrow(htm@objectdata)), width = 3, format = "d", flag = "0"),sep="_")
    d <- d[,order(colnames(d))]
    d <- d[order(rownames(d)),]
    l$cells_Norm <- d
    
    
    # raw
    ids = which(  grepl("__raw",colnames(htm@objectdata))  )
    d = htm@objectdata[ids]
    d$treatment = htm@objectdata[[htm@settings@columns$treatment]]
    d$date = htm@objectdata[[htm@settings@columns$experiment]]
    rownames(d) <- paste(htm@objectdata[[htm@settings@columns$treatment]],seq(1:nrow(htm@objectdata)),sep="_")
    d <- d[,order(colnames(d))]
    d <- d[order(rownames(d)),]
    l$cells_raw <- d
    
    
    return(l)
    
    
    #print(colnames(htm@data))
    
    
    if(0) {
        
        # initialisation
        d <- data.frame(treatment = rep(NA,numEntries))
        for (feature in features) {
            d[[feature]] <- NA 
            htm@data[[paste0(feature,"__zScore")]] <- NA
        }
        
        
        htm@data$MDD_vsCtrl = 0
        htm@data$MDD_vsTreat = 0
        
        
        
        # computation
        for(i in 1:length(treatments)) {
            
            treatment = treatments[i]
            #print(treatment)
            
            for (measurement in measurements) {
                
                indices_ok_treat <- which( ( htm@data[[htm@settings@columns$treatment]] == treatment ) 
                                           & (htm@data$HTM_qcImages==1) 
                                           & !is.nan(htm@data[[measurement]]) & !(htm@data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")))
                
                
                indices_ok_ctrl <- which( ( htm@data[[htm@settings@columns$treatment]] == negative_ctrl) 
                                          & (htm@data$HTM_qcImages==1) 
                                          & !is.nan(htm@data[[measurement]]) & !(htm@data[[htm@settings@columns$experiment]] %in% htmGetVectorSettings("statistics$experiments_to_exclude")))
                
                
                #print(paste("    Treatment", treatment))
                #print(paste("    Images Valid Treatment", length(indices_ok_treat)))      
                #print(paste("    Images Valid Control", length(indices_ok_ctrl)))
                
                mean_treat = mean(htm@data[indices_ok_treat,measurement])
                mean_ctrl = mean(htm@data[indices_ok_ctrl,measurement])
                sd_ctrl = sd(htm@data[indices_ok_ctrl,measurement])
                
                d[i,measurement] = ( mean_treat - mean_ctrl ) / sd_ctrl
                
                htm@data[indices_ok_treat,"MDD_vsCtrl"] =  htm@data[indices_ok_treat,"MDD_vsCtrl"] + ((htm@data[indices_ok_treat,measurement] - mean_ctrl) / sd_ctrl)^2
                htm@data[indices_ok_treat,"MDD_vsTreat"] =  htm@data[indices_ok_treat,"MDD_vsTreat"] + ((htm@data[indices_ok_treat,measurement] - mean_treat) / sd_ctrl)^2
                htm@data[indices_ok_treat,paste0(measurement,"__zScore")]  = (htm@data[indices_ok_treat,measurement] - mean_ctrl) / sd_ctrl
                
            }
            
            d[i,"treatment"] = treatment
            
        }
        
        htm@data$MDD_vsCtrl = sqrt(htm@data$MDD_vsCtrl / length(measurements))
        htm@data$MDD_vsTreat = sqrt(htm@data$MDD_vsTreat / length(measurements))
        
        rownames(d) <- d$treatment
        d$treatment <- NULL
        
        l = list()
        l$treatFeat <- d
        l$images <- htm@data
    }
    
    
    # return(l)
    
}


htmMakeFeatureFrame <- function(d) {
    
    
    channels <- unique(unlist(lapply(strsplit(colnames(d),"xx"), function(x) {x[2]})))
    
    i = 0
    
    for (channel in channels) {
        
        print(channel)
        ids = which(grepl(channel,colnames(d)))
        ds = d[,ids]
        rownames(ds) <- paste(rep(channel,nrow(ds)),rownames(ds),sep="_")
        colnames(ds) <- unlist(lapply(colnames(ds), function(x) {gsub(channel,"",x)}))
        
        #print( rownames(ds))
        #print( colnames(ds))
        
        if(i==0) {
            dc = ds 
        } else { 
            dc = rbind(dc, ds)
        }
        
        i = i + 1
        
    }
    
    dc2 <- dc
    dc2$channeltreat = unlist(lapply(strsplit(rownames(dc),"_"), function(x) {paste(x[1],x[2],sep="_")}))
    dc2mean <- ddply(dc2, .(channeltreat), numcolwise(mean))
    rownames(dc2mean) <- dc2mean$channeltreat
    dc2mean$channeltreat <- NULL
    
    l = list()
    l$df <- dc
    l$dfmean <- dc2mean
    return(l)
    
}

htmMDStreatFeat <- function(dTreatFeat, negCtrl) {
    
    print("htmMDStreatFeat...")
    # MDS
    # Classical MDS
    # N rows (objects) x p columns (variables)
    # each row identified by a unique row name
    rCtrl = which(rownames(dTreatFeat)==negCtrl)
    dTreatFeat[rCtrl,] = 0
    
    d <- (dist(dTreatFeat)) # euclidean distances between the rows
    #print(d)
    
    fit <- cmdscale(d, eig=TRUE, k=2) # k is the number of dim
    
    pc = fit$points[negCtrl,1:2]  
    x <- fit$points[,1]
    y <- fit$points[,2]
    
    # center the negative control
    x <- x - pc[1]
    y <- y - pc[2]
    
    #print(fit)
    dMDS = dist(fit$points)
    dDiff = dMDS - d
    #print(d)
    #print(dDiff)
    # plot solution 
    #dev.new()
    
    #plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
    #     main="Metric  MDS",  type="n", asp=1)
    #text(x, y, labels = rownames(dTreatFeat), cex=.7)
    
    MDS = data.frame(treatment=rownames(dTreatFeat), x=x, y=y) 
    
    
    return(MDS)
    
}

htmMDStreatFeat2 <- function(d) {
    
    # MDS
    # Classical MDS
    # N rows (objects) x p columns (variables)
    # each row identified by a unique row name
    #ids = which(grepl("Control",rownames(d)))
    #d <- d[ids,]
    
    dd <- sqrt(dist(d)) # euclidean distances between the rows
    
    fit <- cmdscale(dd, eig=TRUE, k=2) # k is the number of dim
    x <- fit$points[,1]
    y <- fit$points[,2]
    
    dMDS = dist(fit$points)
    dDiff = dMDS - dd
    
    lables = unlist(lapply(strsplit(rownames(d),"_"), function(x) {paste(x[1],"",sep="_")}))
    lables = rownames(d)
    
    
    dev.new()
    plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2", 
         main="Metric  MDS",  type="n", asp=1)
    text(x, y, labels = lables, cex=.7)
    
    MDS = data.frame(treatment=rownames(d), x=x, y=y) 
    
    return(MDS)
    
}


htmCellsToTreatments <- function(d, stats = "classic", keepdate = F){
    
    if(keepdate) {
        d$treatment = unlist(lapply(strsplit(rownames(d),"_"), function(x) paste(x[1],x[2])))
    } else {
        d$treatment = unlist(lapply(strsplit(rownames(d),"_"), function(x) x[1]))  
    }
    
    
    if(stats =="classic") {
        dmean <- ddply(d, .(treatment), numcolwise(mean))
        rownames(dmean) <- dmean$treatment
        dmean$treatment <- NULL
        return(dmean)
    }
    
    if(stats =="robust") {
        dmean <- ddply(d, .(treatment), numcolwise(median))
        rownames(dmean) <- dmean$treatment
        dmean$treatment <- NULL
        return(dmean)
    }
    
    
}

#
# Assay overview
#

htmOverview <- function(htm) {
    cExp = htm@settings@columns$experiment
    cWell = htm@settings@columns$wellnum
    cPos = htm@settings@columns$posnum
    
    nExp = length(unique(paste(htm@data[[cExp]])))
    nWell = length(unique(paste(htm@data[[cExp]],htm@data[[cWell]],sep="_")))
    nPos =  length(unique(paste(htm@data[[cExp]],htm@data[[cWell]],htm@data[[cPos]],sep="_")))
    
    print("")
    print("Overiew of the contents of the image table:")
    print(paste("experiments =",nExp))
    print(paste("wells =",nWell))
    print(paste("wells/experiment =",nWell/nExp))
    print(paste("positions =",nPos))
    print(paste("positions/well =",nPos/nWell))
    
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

htmMultiChannelFeatureNormalization <- function(htm) {
    
    print("")
    print("Feature normalization")
    print("**********************")
    print("")
    
    # get all necessary information
    #measurement <- htmGetListSetting(htm,"statistics","measurement")
    #experiments <- sort(unique(htm@data[[htm@settings@columns$experiment]]))
    #experiments_to_exclude <- htmGetVectorSettings("statistics$experiments_to_exclude")
    negcontrols <- c(htmGetListSetting(htm,"statistics","negativeControl"))
    #transformation <- htmGetListSetting(htm,"statistics","transformation")
    
    cat("\nNegative Control:\n")
    print(negcontrols)
    
    if( is.null(htm@data$HTM_qcImages) ) {
        print(" WARNING: there was no Image QC column; all images with non NA values will be set tovalid!")
        htm@data$HTM_qcImages = !is.na(htm@data[[measurement]])
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


#
# Logging
#


htmPrintLog <- function(){
    htm <- get("htm", envir = globalenv()) 
    cat(htm@log)
}

htmAddLog <- function(text){
    htm <- get("htm", envir = globalenv()) 
    htm@log = paste(htm@log,text,sep="\n")
    assign("htm", htm, envir = globalenv())            
}

htmClearLog <- function(text){
    htm <- get("htm", envir = globalenv()) 
    htm@log = ""
    assign("htm", htm, envir = globalenv())            
}


