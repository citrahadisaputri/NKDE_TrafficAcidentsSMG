#CROSS VALIDATION
##CONTINUOUS
library(future)
future::plan(future::multisession(workers=3))
cv_scores <- bw_cv_likelihood_calc(c(3000,100),100,
                                      simple_lines_valid, plots_datalaka,
                                      rep(1,nrow(plots_datalaka)),
                                      "quartic", "continuous",
                                      diggle_correction = FALSE, study_area = NULL,
                                      max_depth = 10,
                                      digits=3, tol=0.1, agg=1,
                                      sparse= FALSE, grid_shape=c(150,150),
                                      sub_sample = 1, verbose=TRUE, check=TRUE)
## make sure any open connections are closed afterward
if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)

##DISCONTINUOUS
future::plan(future::multisession(workers=1))
cv_scores1 <- bw_cv_likelihood_calc(c(3000,100),100,
                                   simple_lines_valid, plots_datalaka,
                                   rep(1,nrow(plots_datalaka)),
                                   "quartic", "discontinuous",
                                   diggle_correction = FALSE, study_area = NULL,
                                   max_depth = 15,
                                   digits=3, tol=0.1, agg=1,
                                   sparse= FALSE, grid_shape=c(200,200),
                                   sub_sample = 1, verbose=TRUE, check=TRUE)
if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)

#SIMPLE
future::plan(future::multisession(workers=1))
cv_scores3 <- bw_cv_likelihood_calc(c(3000,100),100,
                                   simple_lines_valid, plots_datalaka,
                                   rep(1,nrow(plots_datalaka)),
                                   "quartic", "simple",
                                   diggle_correction = FALSE, study_area = NULL,
                                   max_depth = 15,
                                   digits=3, tol=0.1, agg=1,
                                   sparse= FALSE, grid_shape=c(200,200),
                                   sub_sample = 1, verbose=TRUE, check=TRUE)
## make sure any open connections are closed afterward
if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)
