
### define htm classes

htmSettingsConstructor = setClass("htmSettings",
                                    representation = representation(
                                    visualisation = "list",
                                    statistics = "list",
                                    columns = "list",
                                    qc = "data.frame"
                                  ))


## ------------------------

htmConstructor = setClass("htm",
                          representation = representation(
                            data = "data.frame",
                            objectdata = "data.frame",
                            wellSummary = "data.frame",
                            treatmentSummary = "data.frame", 
                            settings = "htmSettings",
                            log = "character",
                            other = "list"
                          ))



### make class 

htmMake <- function(data = data.frame()) {
  
  print("Making new htm object.")
  
  .settings = htmSettingsConstructor(
    
    columns = list(),
    
    qc       = data.frame(colname = "None selected", 
                          min = NA, 
                          max = NA, 
                          stringsAsFactors = FALSE),
    
    visualisation = list(image_pathname_prefix = "",
                         image_filename_prefix = "FileName_",
                         image_foldername_prefix = "PathName_",
                         heatmap_width = 10,
                         heatmap_image_size_cex = 0.9,
                         heatmap_well_size_cex = 0.9,
                         jitter = 1,
                         jitterPlot_colorizeTreatments_TF = F,
                         jitterPlot_showMeanAndSD_TF = F,
                         jitterPlot_showMedianAndMAD_TF = F,
                         plotting_showQCfailData_TF = T,
                         jitterPlot_scaleFromZero_TF = F
    )
    
    
  )
  
  htm <- htmConstructor(
    data = data, 
    objectdata = data.frame(),
    wellSummary = data.frame(),
    treatmentSummary = data.frame(), 
    settings = .settings,
    other = list(),
    log = date()
  )  
  
  
  return(htm)
  
}


# setMethod("show", "htm", function(object) {
#             cat(" data:",dim(object@data),"\n")
#             cat(" wellSummary:",dim(object@wellSummary),"\n")
#             cat(" treatmentSummary:",dim(object@treatmentSummary),"\n")
#             cat("\n")
#             cat(" plate layout info:\n")
#             for(name in names(object@settings@platelayout)) {
#               cat("  ",as.character(name),"=",as.character(object@settings@platelayout[[name]]) )
#               cat("\n")
#             }
#             cat(" special columns:\n")
#             for(name in names(object@settings@columns)) {
#               cat("  ",as.character(name),"=",as.character(object@settings@columns[[name]]) )
#               cat("\n")
#             }
#             cat(" QC per image:\n")
#             for(i in 1:nrow(object@settings@qcImages)) {
#               cat("    column =",as.character(object@settings@qcImages[i,"colname"]) )
#               cat("  min =",object@settings@qcImages[i,"min"] )
#               cat("  max =",object@settings@qcImages[i,"max"] )
#               cat("\n")
#             }
#             cat(" negative controls:\n")
#             for(i in 1:length(object@settings@ctrlsNeg)) {
#               cat("  ",as.character(object@settings@ctrlsNeg[i]) )
#               cat("\n")
#             }
#             cat(" selected measurement columns:\n")
#             for(i in 1:length(object@settings@measurements)) {
#               cat("  ",as.character(object@settings@measurements[i]) )
#               cat("\n")
#             }
#             cat(" selected normalisation methods:\n")
#             for(i in 1:length(object@settings@normMethodsSelected)) {
#               cat("  ",as.character(object@settings@normMethodsSelected[i]) )
#               cat("\n")
#             }
#             cat(" log:",object@log, "\n")
#           })


