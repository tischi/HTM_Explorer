
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

