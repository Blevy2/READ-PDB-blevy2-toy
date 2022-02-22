lrren_Bens <- function (obs_locs, predict = FALSE, predict_locs = NULL, conserve = TRUE, 
          alpha = 0.05, p_correct = "none", cv = FALSE, kfold = 10, 
          balance = FALSE, parallel = FALSE, n_core = 2, poly_buffer = NULL, 
          obs_window = NULL, verbose = FALSE, ...) 
{
  if (verbose == TRUE) {
    message("Estimating relative risk surfaces\n")
  }
  match.arg(p_correct, choices = c("none", "FDR", "Sidak", 
                                   "Bonferroni"))
  inner_chull <- concaveman::concaveman(as.matrix(obs_locs[, 
                                                           5:6]))
  inner_chull_pts <- sp::coordinates(inner_chull)
  inner_chull_pts <- rbind(inner_chull_pts, inner_chull_pts[1, 
  ])
  inner_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(inner_chull_pts)), 
                                                            1)))
  if (is.null(poly_buffer)) {
    poly_buffer <- abs(min(diff(sp::bbox(inner_chull_poly)[1, 
    ]), diff(sp::bbox(inner_chull_poly)[2, ]))/100)
  }
  inner_chull_poly_buffer <- rgeos::gBuffer(inner_chull_poly, 
                                            width = poly_buffer, byid = TRUE)
  inner_poly <- inner_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords
  if (is.null(predict_locs)) {
    outer_chull_poly <- inner_chull_poly_buffer
    outer_poly <- inner_poly
  }
  else {
    if (nrow(predict_locs) > 5e+06) {
      outer_chull <- grDevices::chull(x = stats::na.omit(predict_locs)[, 
                                                                       3], y = stats::na.omit(predict_locs)[, 4])
      outer_chull_pts <- predict_locs[c(outer_chull, outer_chull[1]), 
                                      3:4]
    }
    else {
      outer_chull <- concaveman::concaveman(as.matrix(stats::na.omit(predict_locs)[, 
                                                                                   3:4]))
      outer_chull_pts <- sp::coordinates(outer_chull)
    }
    outer_chull_pts <- rbind(outer_chull_pts, outer_chull_pts[1, 
    ])
    outer_chull_poly <- sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(outer_chull_pts)), 
                                                              1)))
    outer_chull_poly_buffer <- rgeos::gBuffer(outer_chull_poly, 
                                              width = poly_buffer, byid = TRUE)
    outer_poly <- outer_chull_poly_buffer@polygons[[1]]@Polygons[[1]]@coords
  }
  if (conserve == FALSE & is.null(predict_locs)) {
    stop("If the argument 'conserve' is FALSE, must specify the argument 'predict_locs'")
  }
  if (conserve == TRUE) {
    window_poly <- inner_poly
  }
  else {
    window_poly <- outer_poly
  }
  if (is.null(obs_window)) {
    wind <- spatstat.geom::owin(poly = list(x = rev(window_poly[, 
                                                                1]), y = rev(window_poly[, 2])))
  }
  else {
    wind <- obs_window
  }
  presence_locs <- subset(obs_locs, obs_locs[, 4] == 1)
  absence_locs <- subset(obs_locs, obs_locs[, 4] == 0)
  ppp_presence <- spatstat.geom::ppp(x = presence_locs[, 5], 
                                     y = presence_locs[, 6], window = wind, checkdup = FALSE)
  ppp_absence <- spatstat.geom::ppp(x = absence_locs[, 5], 
                                    y = absence_locs[, 6], window = wind, checkdup = FALSE)
  obs <- sparr::risk(f = ppp_presence, g = ppp_absence, tolerate = TRUE, 
                     verbose = verbose, ...)
  bandw <- obs$f$h0
  if (p_correct == "none") {
    p_critical <- alpha
  }
  else {
    p_critical <- pval_correct(input = as.vector(t(obs$P$v)), 
                               type = p_correct, alpha = alpha)
  }
  if (predict == FALSE) {
    output <- list(obs = obs, presence = ppp_presence, absence = ppp_absence, 
                   outer_poly = outer_poly, inner_poly = inner_poly)
  }
  else {
    if (verbose == TRUE) {
      message("Predicting area of interest")
    }
    rr_raster <- raster::raster(obs$rr)
    pval_raster <- raster::raster(obs$P)
    extract_points <- cbind(predict_locs[, 3], predict_locs[, 
                                                            4])
    extract_predict <- data.frame(predict_locs = predict_locs, 
                                  rr = raster::extract(rr_raster, extract_points), 
                                  pval = raster::extract(pval_raster, extract_points))
    output <- list(obs = obs, presence = ppp_presence, absence = ppp_absence, 
                   outer_poly = outer_poly, inner_poly = inner_poly, 
                   predict = extract_predict)
  }
  if (cv == FALSE) {
    cv_results <- NULL
  }
  else {
    if (kfold < 1) {
      stop("The 'kfold' argument must be an integer of at least 1")
    }
    cv_predictions_rank <- list()
    cv_predictions_quant <- list()
    cv_labels <- list()
    cv_pvals <- list()
    if (balance == FALSE) {
      cv_segments <- pls::cvsegments(nrow(obs_locs), kfold)
      cv_seg_cas <- NULL
      cv_seg_con <- NULL
    }
    else {
      cv_seg_cas <- pls::cvsegments(nrow(presence_locs), 
                                    kfold)
      cv_seg_con <- pls::cvsegments(nrow(absence_locs), 
                                    kfold)
      cv_segments <- NULL
    }
    if (verbose == TRUE) {
      message("Cross-validation in progress")
    }
    if (parallel == TRUE) {
      oldplan <- doFuture::registerDoFuture()
      on.exit(with(oldplan, foreach::setDoPar(fun = fun, 
                                              data = data, info = info)), add = TRUE)
      future::plan(future::multisession, workers = n_core)
      `%fun%` <- doRNG::`%dorng%`
    }
    else {
      `%fun%` <- foreach::`%do%`
    }
    out_par <- foreach::foreach(k = 1:kfold, kk = iterators::icount(), 
                                .combine = comb, .multicombine = TRUE, .init = list(list(), 
                                                                                    list())) %fun% {
                                                                                      if (verbose == TRUE) {
                                                                                        progBar(kk, kfold)
                                                                                      }
                                                                                      if (balance == FALSE) {
                                                                                        testing <- obs_locs[cv_segments[k]$V, ]
                                                                                        training <- obs_locs[-(cv_segments[k]$V), ]
                                                                                      }
                                                                                      else {
                                                                                        ind <- 1:length(cv_seg_con[k]$V)
                                                                                        randind <- sample(ind, length(cv_seg_cas[k]$V), 
                                                                                                          replace = FALSE)
                                                                                        testing_cas <- presence_locs[cv_seg_cas[k]$V, 
                                                                                        ]
                                                                                        testing_con <- absence_locs[cv_seg_con[k]$V, 
                                                                                        ]
                                                                                        testing_con <- testing_con[randind, ]
                                                                                        testing <- rbind(testing_cas, testing_con)
                                                                                        training_cas <- presence_locs[-(cv_seg_cas[k]$V), 
                                                                                        ]
                                                                                        training_con <- absence_locs[-(cv_seg_con[k]$V), 
                                                                                        ]
                                                                                        training <- rbind(training_cas, training_con)
                                                                                      }
                                                                                      ppp_presence_training <- spatstat.geom::ppp(x = training[, 
                                                                                                                                               5][training[, 4] == 1], y = training[, 6][training[, 
                                                                                                                                                                                                  4] == 1], window = wind, checkdup = FALSE)
                                                                                      ppp_absence_training <- spatstat.geom::ppp(x = training[, 
                                                                                                                                              5][training[, 4] == 0], y = training[, 6][training[, 
                                                                                                                                                                                                 4] == 0], window = wind, checkdup = FALSE)
                                                                                      rand_lrr <- sparr::risk(f = ppp_presence_training, 
                                                                                                              g = ppp_absence_training, tolerate = TRUE, verbose = FALSE, 
                                                                                                              ...)
                                                                                      rr_raster <- raster::raster(rand_lrr$rr)
                                                                                      rr_raster[is.na(rr_raster[])] <- 0
                                                                                      extract_testing <- testing[, 5:6]
                                                                                      cv_predictions_rr <- raster::extract(rr_raster, 
                                                                                                                           extract_testing)
                                                                                      cv_labels <- testing[, 4]
                                                                                      par_results <- list(cv_predictions_rr = cv_predictions_rr, 
                                                                                                          cv_labels = cv_labels)
                                                                                      return(par_results)
                                                                                    }
    if (verbose == TRUE) {
      message("\nCalculating Cross-Validation Statistics")
    }
    cv_predictions_rr <- out_par[[1]]
    cv_labels <- out_par[[2]]
    cv_results <- list(cv_predictions_rr = cv_predictions_rr, 
                       cv_labels = cv_labels)
  }
  lrren_output <- list(out = output, cv = cv_results, dat = obs_locs, 
                       p_critical = p_critical, rr_ras = rr_raster)
}
