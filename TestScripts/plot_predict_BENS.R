plot_predict_BENS <- function (input, plot_cols = c("#8B3A3A", "#CCCCCC", "#0000CD", 
                               "#FFFF00"), alpha = input$p_critical, cref0 = "EPSG:4326", 
          cref1 = NULL, lower_lrr = NULL, upper_lrr = NULL, digits = 1, 
          ...) 
{
  if (alpha >= 1 | alpha <= 0) {
    stop("The argument 'alpha' must be a numeric value between 0 and 1")
  }
  if (length(plot_cols) != 4) {
    stop("The argument 'plot_cols' must have 4 colors")
  }
  op <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(op))
  predict_risk <- data.frame(x = input$out$predict$predict_locs.x, 
                             y = input$out$predict$predict_locs.y, v = input$out$predict$rr)
  naband <- predict_risk
  sp::coordinates(predict_risk) <- ~x + y
  sp::gridded(predict_risk) <- TRUE
  predict_risk_raster <- raster::raster(predict_risk)
  raster::crs(predict_risk_raster) <- sp::CRS(SRS_string = cref0)
  if (!is.null(cref1)) {
    predict_risk_raster <- raster::projectRaster(predict_risk_raster, 
                                                 crs = sp::CRS(SRS_string = cref1), method = "ngb", 
                                                 legacy = TRUE)
  }
  
  #first try (ended up not aligning with temp raster, even after removing rows/cols)
  # #adjust resolution
  # predict_risk_raster <- raster::projectRaster(predict_risk_raster, predict_risk_raster, 
  #                                                     res=.02)
  
  
  #second try
  #reproject and resample
  #following from https://gis.stackexchange.com/questions/339797/downsampling-projecting-and-aligning-a-raster-to-fit-another-one-in-r-aggregat
  # test <- projectRaster(from = predict_risk_raster, crs = crs(median_sed_thick_IDW))
  # test <- resample(x=test, y=median_sed_thick_IDW, method="bilinear")
  # 
  
                                               
  
  naband$v <- ifelse(is.na(naband$v), 9999, naband$v)
  sp::coordinates(naband) <- ~x + y
  sp::gridded(naband) <- TRUE
  NA_risk_raster <- raster::raster(naband)
  raster::crs(NA_risk_raster) <- sp::CRS(SRS_string = cref0)
  if (!is.null(cref1)) {
    NA_risk_raster <- raster::projectRaster(NA_risk_raster, 
                                            crs = sp::CRS(SRS_string = cref1), method = "ngb", 
                                            legacy = TRUE)
  }
  naband_reclass <- raster::reclassify(NA_risk_raster, c(-Inf, 
                                                         9998, NA, 9998, Inf, 1))
  if (all(is.na(raster::values(naband_reclass)))) {
    naband_reclass <- NULL
  }
  predict_tol <- data.frame(x = input$out$predict$predict_locs.x, 
                            y = input$out$predict$predict_locs.y, v = input$out$predict$pval)
  sp::coordinates(predict_tol) <- ~x + y
  sp::gridded(predict_tol) <- TRUE
  predict_tol_raster <- raster::raster(predict_tol)
  raster::crs(predict_tol_raster) <- sp::CRS(SRS_string = cref0)
  if (!is.null(cref1)) {
    predict_tol_raster <- raster::projectRaster(predict_tol_raster, 
                                                crs = sp::CRS(SRS_string = cref1), method = "ngb", 
                                                legacy = TRUE)
  }
  reclass_tol <- raster::cut(predict_tol_raster, breaks = c(-Inf, 
                                                            alpha/2, 1 - alpha/2, Inf), right = FALSE)
  
  library(envi)
  rrp <- envi:::div_plot(input = predict_risk_raster, cols = plot_cols[1:3], 
                  midpoint = 0, thresh_low = lower_lrr, thresh_up = upper_lrr, 
                  digits = digits)
 #  View(rrp$v)
 #  print(class(rrp$v))
 #  View(rrp$v@data@values)
 # print(class(rrp$v@data@values))
 #  
  
  graphics::par(pty = "s")
p1 <- fields::image.plot(rrp$v, breaks = rrp$breaks, col = rrp$cols,
                          axes = TRUE, main = "log relative risk", xlab = "longitude",
                         ylab = "latitude", legend.mar = 3.1, axis.args = list(at = rrp$at,
                                                                              las = 0, labels = rrp$labels, cex.axis = 0.67))


 # p3 <-plot(predict_risk_raster)
  

if (!is.null(naband_reclass)) {
    raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
  }
  if (all(raster::values(reclass_tol)[!is.na(raster::values(reclass_tol))] == 
          2)) {
    pcols <- plot_cols[2]
    brp <- c(1, 3)
    atp <- 2
    labp <- "insignificant"
  }
  else {
    pcols <- plot_cols[1:3]
    brp <- c(1, 1.67, 2.33, 3)
    atp <- c(1.33, 2, 2.67)
    labp <- c("presence", "insignificant", "absence")
  }
  
  p2 <- fields::image.plot(reclass_tol, breaks = brp, col = pcols, 
                           axes = TRUE, main = paste("significant p-values\nalpha =", 
                                                     formatC(alpha, format = "e", digits = 2), sep = " "), 
                           xlab = "longitude", ylab = "latitude", legend.mar = 3.1, 
                           axis.args = list(at = atp, labels = labp, las = 0, cex.axis = 0.67))
  if (!is.null(naband_reclass)) {
    raster::image(naband_reclass, col = plot_cols[4], add = TRUE)
  }
  
  plot_predict_BENS_output <- list(out = rrp, predict_risk_ras = predict_risk_raster, PR=predict_risk)
  
}

