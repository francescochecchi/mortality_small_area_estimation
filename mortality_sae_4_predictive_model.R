#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## --- R CODE TO FIT AND EVALUATE HOUSEHOLD REGRESSION MODELS TO PREDICT DEATH RATE ---- ##
#..........................................................................................

                                       # Written by Francesco Checchi, LSHTM (November 2020)
                                       # francesco.checchi@lshtm.ac.uk 


#place1#
#.........................................................................................
### Reading in required files
#.........................................................................................
    
  #...................................
  ## Predictor data (from earlier code)
   
    # Read values of predictors for model fitting
    x_obs <- read.csv(paste(country, "_x_obs.csv", sep=""), sep="," )

#place2#
  #...................................
  ## Survey observation datasets (from earlier code)
    
    # Read household-level survey observations
    hh_y_obs <- read.csv(paste(country, "_hh_obs.csv", sep=""), sep="," )

#place3#
#.........................................................................................
### Merge mortality and predictor data
#.........................................................................................
  
  #...................................
  ## Select relevant columns in the data depending on which dependent variable is being modelled
      
    # if CDR is being estimated...
    if (y_hat == "cdr") {
      hh_y_obs <- hh_y_obs[, c("survey_id", "Cluster", "stratum", "n_died", "ptime", "quality_score", "sampling_coverage")]
    }
      
    # if U5DR is being estimated...
    if (y_hat == "cdr_u5") {
      hh_y_obs <- hh_y_obs[, c("survey_id", "Cluster", "stratum", "n_died_u5", "ptime_u5", "quality_score", "sampling_coverage")]
        
    # rename as for CDR to simplify code below
      colnames(hh_y_obs) <- c("survey_id", "Cluster", "stratum", "n_died", "ptime", "quality_score", "sampling_coverage")
    }
      
  #...................................
  ## Household-level observations: merge dependent and predictor variables      
  hh_obs <- merge(hh_y_obs, x_obs, by = c("survey_id", "stratum") )
  
    # Eliminate any observations with zero person-time (if U5DR is being analysed, this could occur)
    hh_obs <- subset(hh_obs, ptime > 0)
            
        
#.........................................................................................
### Prepare and explore the dependent variable
#.........................................................................................
    
  #...................................
  ## Calculate model weights
  hh_obs[, "wt"] <- hh_obs[, "quality_score"] * hh_obs[, "sampling_coverage"]
   

  #...................................
  ## Tabulate observations
  table(hh_obs$n_died)


#.........................................................................................
### Exploring and preparing predictor variables
#.........................................................................................
      
  #...................................
  ## Identify "parent" variables (i.e. from which lags were derived)
    # Identify variables that are not parent
    x1 <- na.omit(grep("_lag", colnames(x_obs)))
    vars_in <- colnames(x_obs)[-x1]
    
    # Also eliminate variables that are not potential predictor variables...
    vars_in <- vars_in[! vars_in %in% c("survey_id", "stratum", "admin1")]

    #...any that are not included in the list of variable parameters...
    vars_in <- vars_in[vars_in %in% var_pars$variable]
    
    # ...and any that are being forced out of the model
    vars_in <- vars_in[! vars_in %in% subset(var_pars, force=="out")[, "variable"] ]
#place4#   
  #...................................
  ## Explore distributions of predictors

    # For each parent variable...
    for (i in vars_in) { 
      # full range  
      f_hist(i, x_obs, c(NA, NA)) 
      
      # zooming in
      f_hist(i, x_obs, c(NA, quantile(hh_obs[, i], 0.90, na.rm = TRUE))) 
      
    }
    
#place5#
  #...................................
  ## Categorise predictors if needed (including lags)
    
    # For each variable in the var_pars data frame...
    for (i in 1:nrow(var_pars) ) {
      
      # if the variable is among the parent variables and should be categorised...  
      if (var_pars[i, "variable"] %in% vars_in & is.na(var_pars[i, "cat_only"]) == FALSE ) {
       
        # work out the indices of the variable and any lags
        ind <- grep(var_pars[i, "variable"], colnames(hh_obs) )
        names <- colnames(hh_obs)[ind]
          
        # if the variable should be categorised according to cut-offs...
        if (var_pars[i, "cat_method"] == "cut") {
          
          # work out cut-offs and corresponding labels
          x1 <- as.numeric(unlist(strsplit(var_pars[i, "cat_cutoffs"], split=",")))
          x2 <- unlist(strsplit(var_pars[i, "cat_labels"], split=", "))
          
          # for the variable and each of its lags, if any...
          for (j in names) {
            # create new categorical variables
            hh_obs[, paste(j , "_cat", sep="")] <- cut(hh_obs[, j], breaks = x1, labels = x2, include.lowest = TRUE)
          } 
          print(paste("categorisation of variable ", var_pars[i, "variable"], " :", sep=""))
          print(table(hh_obs[, paste(var_pars[i, "variable"] , "_cat", sep="")] ))
            
        }
        
        # if the variable should be categorised according to values...
        if (var_pars[i, "cat_method"] == "values") {
          
          # work out categories for values
          x1 <- cbind(unlist(strsplit(var_pars[i, "cat_values"], split=",")),
            unlist(strsplit(var_pars[i, "cat_labels"], split=", ")) )
          
          # for the variable and each of its lags, if any...
          for (j in names) {
            
            # assign corresponding column names
            colnames(x1) <- c(j, paste(j , "_cat", sep="") )
            
            # create new categorical variable in training dataset
            hh_obs <- merge(hh_obs, x1, by = paste(colnames(x1)[1]), all.x = TRUE)
          }
          print(paste("categorisation of variable ", var_pars[i, "variable"], " :", sep=""))
          print(table(hh_obs[, paste(var_pars[i, "variable"] , "_cat", sep="")] ))
          
        }

        # add categorical variable to list of parent variables to be considered in model
        vars_in <- c(vars_in, paste(names , "_cat", sep=""))
        
        # if only the categorical variables are to be retained...
        if ( var_pars[i, "cat_only"] == "Y"  ) {
          #...remove the continuous ones and from list of variables to be considered in model
          vars_in <- vars_in[! vars_in %in% names ]
          
        }
        
      }
      
    }


  #...................................
  ## Convert categorical variables to factors
  for (i in colnames(hh_obs)) {
    if (typeof(hh_obs[, i]) == "character") {hh_obs[, i] <- as.factor(hh_obs[, i])}
  }  
    
  #...................................
  ## Change reference categories, if needed
    # For each variable in the var_pars data frame...
    for (i in 1:nrow(var_pars) ) {
      # if the variable is among the parent variables and should be categorised...  
      if (var_pars[i, "variable"] %in% vars_in & is.na(var_pars[i, "cat_only"]) == FALSE ) {
        
        # work out the indices of the categorised variable and any lags
        ind_1 <- grep(var_pars[i, "variable"], colnames(hh_obs))
        ind_2 <- grep("_cat", colnames(hh_obs))
        ind <- intersect(ind_1, ind_2)
        names <- colnames(hh_obs)[ind]
          
        # if the reference category should be specified...
        if (is.na(var_pars[i, "cat_ref"]) == FALSE) {
          # for the variable and each of its lags, if any...
          for (j in names) {
            # specify reference category in the training dataset
            hh_obs[, j] <- relevel(hh_obs[, j], ref = var_pars[i, "cat_ref"])
          } 
        }
      }
  }

#place6#
  #...................................
  ## Update the list of predictors to include in modelling, to make sure lags are still included
  x1 <- c()
  for (i in vars_in) {x1 <- c(x1, grep(i, colnames(hh_obs))) }
  vars_in <- colnames(hh_obs)[x1]
  vars_in <- unique(vars_in)
  

#.........................................................................................
### Univariate analysis
#.........................................................................................
  
  # #...................................
  # ## Graphical investigation of correlation between mortality and predictors (optional, a bit slow)
  #   
  #   # Calculate household-level crude death rate
  #   hh_obs[, "cdr"] <- hh_obs$n_died * 10000 / hh_obs$ptime
  #     # categorise it to better see correlations
  #     f_hist("cdr", hh_obs, c(0.0001,NA))
  #     hh_obs[, "cdr_cat"] <- cut(hh_obs[, "cdr"], breaks = c(0,0.0001, 10, 20, 10000),
  #       labels = c("0", "0.1 to 9.9", "10.0 to 19.9", ">= 20.0"), include.lowest = TRUE)
  #     table(hh_obs$cdr_cat)
  #       
  #   # Continuous variables
  #     # identify continuous versions of predictors
  #     x1 <- vars_in[- which(sapply(hh_obs[, vars_in], is.factor))]
  #     
  #     # visualise correlations
  #       # create ridgeplots
  #       for (i in x1) {
  #         x2 <- hh_obs[, c("cdr_cat", i)]
  #         colnames(x2) <- c("cdr_cat", "var_i")
  #         assign(paste("plot_", i, sep = ""), ggplot(x2, aes(y = cdr_cat, x = var_i, fill = cdr_cat) ) +
  #           # geom_density_ridges() + 
  #           stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  #           theme_ridges() +
  #           labs(title = i) +
  #           theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
  #             plot.title = element_text(hjust = 0.5, size = 9))
  #         )
  #       }
  #     
  #       # identify plot names
  #       x2 <- paste("plot_", x1, sep = "" )
  #     
  #       # arrange plots in a grid and save
  #       plot <- ggarrange(plotlist = mget(x2), ncol = 3, nrow = ceiling(length(x1) / 3),
  #         common.legend = TRUE, legend = "bottom", align = "v")
  #       plot
  #       ggsave(paste(country, "_", y_hat, "_univar_cont.png", sep=""), height = 70, width = 25, units = "cm", dpi = "print")
  #         
  # 
  #   # Categorical variables
  #     # identify categorical versions of predictors
  #     x1 <- vars_in[which(sapply(hh_obs[, vars_in], is.factor))]
  #     
  #       # create bar charts
  #       for (i in x1) {
  #         x2 <- hh_obs[, c("cdr_cat", i)]
  #         colnames(x2) <- c("cdr_cat", "var_i")
  #         x2 <- melt(table(x2))
  #         assign(paste("plot_", i, sep = ""), ggplot(x2, aes(fill = var_i, x = cdr_cat, y = value) ) +
  #           geom_bar(position="fill", stat="identity") + coord_flip() +
  #           labs(title = i) +
  #           theme_bw() +
  #           theme(legend.position = "bottom") + theme(legend.title = element_blank()) +
  #           scale_fill_discrete(name = i) +
  #           theme(axis.title.y = element_blank(), axis.title.x = element_blank(), 
  #             plot.title = element_text(hjust = 0.5, size = 9))
  #         )
  #       }
  #     
  #       # identify plot names
  #       x2 <- paste("plot_", x1, sep = "" )
  #     
  #       # arrange plots in a grid and save
  #       plot <- ggarrange(plotlist = mget(x2), ncol = 3, nrow = ceiling(length(x1) / 3),
  #         common.legend = FALSE, align = "v")
  #       plot
  #       ggsave(paste(country, "_", y_hat, "_univar_cat.png", sep=""), height = 25, width = 25, units = "cm", dpi = "print")
       
    
  #...................................
  ## Observe collinearity among pairs of predictors
    
    # Select all continuous variables and continuous versions of those that have been categorised
    x1 <- unique(c(vars_in[- which(sapply(hh_obs[, vars_in], is.factor))], gsub("_cat", "", vars_in)) )
        
    # Collinearity heatmap
    plot <- ggcorr(hh_obs[, x1], method = c("pairwise.complete.obs", "pearson"),
      low = "steelblue", mid = "grey90", high = "darkred", geom = "tile", nbreaks = 5, min_size = 0, max_size = 6, 
      label = TRUE, label_size = 3, label_round = 2, size = 3, hjust = 0.75, layout.exp = 1, legend.position = "off")
    print(plot)
    ggsave(paste(country, "_", y_hat, "_collinearity_heatmap.png", sep=""), plot, height = 45, width = 45, units = "cm", dpi = "print")    
        
                    
  #...................................
  ## Univariate analysis
    
    # Retain any specific lags if desired
    x1 <- subset(var_pars, ! is.na(force_lag) & ! force %in% c("out"))[, c("variable", "force_lag")]
    x2 <- rep(TRUE, times = length(vars_in))
    for (i in x1$variable) {
      x2[grep(i, vars_in)] <- FALSE
      if (x1[x1$variable == i, "force_lag"] == 0) {x2[grepl(i, vars_in) & ! grepl("_lag", vars_in)] <- TRUE}
      if (x1[x1$variable == i, "force_lag"] != 0) {x2[grepl(i, vars_in) & 
          grepl(paste("_lag", x1[x1$variable == i, "force_lag"], sep=""), vars_in)] <- TRUE}
    }
    
    vars_in <- vars_in[x2]
    
    # Fit univariate models for each predictor
    out <- data.frame(matrix(NA, nrow = length(vars_in), ncol = 13)) 
    colnames(out) <- c("predictor", "aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")   
    out[, "predictor"] <- vars_in
    
    for (i in 1:nrow(out)) {
      print (paste("now fitting univariate models for predictor ", out[i, "predictor"], sep="") )
      out[i, c("aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")] <- 
        f_val(out[i, "predictor"], hh_obs, re, fit_family, FALSE, part_unit, k_folds, FALSE, FALSE)
    }

    # Select best-fitting lags and categorical vs continuous versions, for the same base variable
      # first need to identify base variables
      out[, "base_var"] <- NA
      for (i in var_pars$variable) { out[grep(i, out$predictor), "base_var"] <- i }
    
      # then identify version with lowest deviance p-test
      x1 <- tapply(out$f_test_glm, out$base_var, which.min)
      out[, "keep"] <- FALSE
      for (i in unique(out$base_var)) {
        x2 <- rep(FALSE, nrow(out[out$base_var == i, ]) )
        x2[x1[names(x1) == i]] <- TRUE
        out[out$base_var == i, "keep"] <- x2
      }
      vars_in <- out[out$keep == TRUE, "predictor"]
      
    # Screen out variables that don't pass the univariate screening p-value threshold test
    vars_in <- vars_in[vars_in %in% out[out$f_test_glm < f_univar, "predictor"]]

    # Save output
    write.csv(out, paste(country, "_est_", y_hat, "_univar_models.csv", sep = ""), row.names = FALSE)
      
    
    
#.........................................................................................
### Fit, evaluate and select among candidate models
#.........................................................................................
    
  #...................................
  ## Specify data frame of all possible candidate models
    
    # Create all possible models
    mods <- as.data.frame(matrix(c(0,1), nrow=2, ncol=length(vars_in)))
    colnames(mods) <- vars_in
    mods <- expand.grid(mods[, vars_in])
      # remove first row (null model)
      mods <- mods[-1, ]
    
      # remove models where variables to be forced in are left out (i.e. == 0)
      if (nrow(subset(var_pars, force=="in")) > 0) {
        x1 <- grep(paste(subset(var_pars, force=="in")[, "variable"], collapse="|"), colnames(mods), value=FALSE)
        x2 <- apply(mods, 1, function(x, x1) {if (all(x[x1]==1)) return(TRUE) else return(FALSE)}, x1 ) 
        mods <- mods[which(x2 == TRUE), ]
      }
      
    # Control statement  
    print(paste("the number of possible models at this stage is: ", nrow(mods), sep=""))

  #...................................  
  ## Fit all possible fixed effects models and retain the most promising few
    ### NOTE: COMPUTATIONALLY INTENSIVE IF LARGE NUMBER OF CANDIDATE MODELS ARE FIT
    
    # Fit all models
    out <- as.data.frame(matrix(NA, ncol=ncol(mods) + 12, nrow=nrow(mods)))
    colnames(out) <- c(colnames(mods), "aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")
    for (i in 1:nrow(mods)) {
      print(paste("now fitting model ", i, " of ", nrow(mods), ":", sep="") )
      x1 <- names(mods)[mods[i, ] == 1]
      out[i, colnames(mods)] <- mods[i, ]
      out[i, c("aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")] <- 
        f_val(x1, hh_obs, re, fit_family, FALSE, part_unit, k_folds, FALSE, FALSE)
    }  

    # Save output
    write.csv(out, paste(country, "_est_", y_hat, "_all_models.csv", sep = ""), row.names = FALSE)
    
    # Keep the best models, based on their DSS being in the bottom n% (f_multivar) of all models
    x1 <- quantile(out$dss_glm, f_multivar)
    out <- out[order(out[, "dss_glm"]), ]
    out_best <- out[out$dss_glm <= x1, ]
    View(out_best)
    
  #...................................  
  ## Evaluate best candidate models for overfitting and predictive power on cross-validation
    ### NOTE: COMPUTATIONALLY INTENSIVE IF LARGE NUMBER OF CANDIDATE MODELS ARE PUT THROUGH CV, OR IF LOOCV IS USED
    
    # Prepare output
    out_best_stats <- as.data.frame(matrix(NA, ncol=ncol(mods) + 12, nrow=nrow(out_best)))
    colnames(out_best_stats) <- c(colnames(mods), "aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")
    
    # For each of the models...
    for (i in 1:nrow(out_best)) {
      print(paste("now fitting model ", i, " of ", nrow(out_best), ":", sep="") )
      
      # specify model formula 
      x1 <- names(mods)[out_best[i, colnames(mods)] == 1]
      out_best_stats[i, colnames(mods)] <- out_best[i, colnames(mods)]
      
      # fit model with cross-validation and compute fit statistics
      out_best_stats[i, c("aic_glm", "dss_glm", "mse_glm", "f_test_glm", "dss_cv_glm", "mse_cv_glm", 
      "aic_glmm", "dss_glmm", "mse_glmm", "f_test_glmm", "dss_cv_glmm", "mse_cv_glmm")] <- 
        f_val(x1, hh_obs, re, fit_family, TRUE, part_unit, k_folds, TRUE, TRUE)
      
      # compute loss in predictive scores on cross-validation
      out_best_stats[i, "dss_loss_cv_glm"] <- out_best_stats[i, "dss_cv_glm"] - out_best_stats[i, "dss_glm"]
      out_best_stats[i, "mse_loss_cv_glm"] <- out_best_stats[i, "mse_cv_glm"] - out_best_stats[i, "mse_glm"]
      out_best_stats[i, "dss_loss_cv_glmm"] <- out_best_stats[i, "dss_cv_glmm"] - out_best_stats[i, "dss_glmm"]
      out_best_stats[i, "mse_loss_cv_re"] <- out_best_stats[i, "mse_cv_glmm"] - out_best_stats[i, "mse_glmm"]
      
    }  
    
    View(out_best_stats)
    
    # Save output
    write.csv(out_best_stats, paste(country, "_est_", y_hat, "_best_models.csv", sep = ""), row.names = FALSE)
    
    # Fit best fixed-effects model
      # identify best model based on dss on cross-validation
      x1 <- which.min(out_best_stats$dss_cv_glm)
    
      # select predictors
      vars_in <- names(mods)[out_best_stats[x1, colnames(mods)] == 1]      
      
      # fixed effect GLM with cross-validation
      f_val(vars_in, hh_obs, FALSE, fit_family, TRUE, part_unit, k_folds, TRUE, TRUE)
      form <- as.formula( paste("n_died", "~", paste(vars_in, collapse="+"), sep="")  )
      fit_glm <- glm(form, family = fit_family, data = hh_obs, weights = wt, offset = log(ptime) )
      tidy(fit_glm, exponentiate = TRUE)
      glance(fit_glm)
      f_plot_pred(fit_glm, part_unit, FALSE)
      
      # calculate predictive scores
        # on full training dataset
        scores_glm <- c(NA, NA)
        scores_glm <- out_best_stats[x1, c("dss_glm", "mse_glm")]

        # on cross-validation
        scores_cv_glm <- c(NA, NA)
        scores_cv_glm <- out_best_stats[x1, c("dss_cv_glm", "mse_cv_glm")]
    
      # calculate deviance F test p-value
      f_test_glm <- anova(fit_glm, test = "F")[2, "Pr(>F)"]
      
  #...................................
  ## Explore plausible interactions and update GLM fit accordingly
  if (gen_pars[gen_pars$parameter == "interactions_in", "value"] == TRUE) {  
    
    # Identify any predictors involved in plausible two-way interactions
    int_vars <- subset(var_pars, is.na(interactions) == FALSE )[, c("variable", "interactions")]

    # Check whether these predictors are in the dataset and identify the right variant of the predictors
    if (nrow(int_vars) > 0) {int_vars[, "actual_variable"] <- NA}
    for (i in int_vars[, "variable"]) {
      
      # find all the variants of this variable in the dataset
      x1 <- grep(i, colnames(hh_obs), value = TRUE )
      
      # if the variable is not in the dataset, break
      if (length(x1) == 0) next;
      
      # if the variable is categorical but there is a continuous variant, use the latter
      if (length(x1) > 1 & sum(sapply(hh_obs[, x1], is.numeric)) > 0) {x1 <- gsub("_cat", "", x1 ) }
      x1 <- unique(x1)
      
      # if there is only one variant, use that
      if (length(x1) == 1) {int_vars[int_vars$variable == i, "actual_variable"] <- x1; next}
      
      # if there are several variants...
        # if one is in the model formula, choose that
        if (length(x1[x1 %in% all.vars(formula(fit_glm))] ) > 0 )
          {int_vars[int_vars$variable == i, "actual_variable"] <- x1[x1 %in% all.vars(formula(fit_glm))]; next}
        
        # if none is in the model formula...
          # choose the categorised variant (by this stage, this would cover instances in which a factor is recategorised)
          if (length(grep("_cat", x1)) > 0 ) {x1 <- grep("_cat", x1, value = TRUE)}    
      
          # if a specific lag has been specified, choose that
          if (! is.na(var_pars[var_pars$variable == i, "force_lag"]) &  var_pars[var_pars$variable == i, "force_lag"] != 0) 
          {int_vars[int_vars$variable == i, "actual_variable"] <- 
            grep(paste("lag", var_pars[var_pars$variable == i, "force_lag"], sep =""), x1, value = TRUE); next
          }
      
          # otherwise choose the variant without lag
          if (length(grep("lag", x1)) > 0 ) {int_vars[int_vars$variable == i, "actual_variable"] <- x1[- grep("lag", x1)]}
          if (length(grep("lag", x1)) == 0 ) {int_vars[int_vars$variable == i, "actual_variable"] <- x1}
        
    }
    
        
    # Test each interaction in turn and record fit statistics
    if (nrow(int_vars) > 0) {
     
      # prepare
      x1 <- c()
      for (i in 1:max(int_vars$interactions)) {
        x1 <- c(x1, paste(as.vector(subset(int_vars, interactions == i)$actual_variable ), collapse=":") ) }

      # for each potential interaction term...
      for (i in x1) {

        # list of predictors and interactions
        x2 <- c(vars_in, i)

        # fit fixed-effects model with interactions
        print(paste("now fitting model with interaction term  ", i, sep="") )
        form <- as.formula( paste("n_died", "~", paste(x2, collapse="+"), sep="")  )
        fit <- glm(form, family = fit_family, data = hh_obs, weights = wt, offset = log(ptime) )
        tidy(fit, exponentiate = TRUE)
        glance(fit)
        
        # calculate predictive scores on cross-validation
        scores_cv_int <- f_cv(fit, hh_obs, part_unit, k_folds, TRUE, TRUE)
        names(scores_cv_int) <- c("dss_cv_int", "mse_cv_int")
        print(scores_cv_int)
        
        # keep model with interaction term, if desired and if it improves fit
        if (interactions_in == TRUE & scores_cv_int["dss_cv_int"] < scores_cv_glm["dss_cv_glm"]) 
          { fit_glm <- fit; vars_in <- c(vars_in, i); scores_cv_glm <- scores_cv_int }
      }
    }
    
    # Best model after testing for interactions
    tidy(fit_glm, exponentiate = TRUE)
    glance(fit_glm)

    # Recalculate predictive scores and F-test
      # calculate elements required for predictive scores
      lambdas <- predict(fit_glm, type = "response")
      variances <- (exp(predict(fit_glm) * (1 + summary(fit_glm)$dispersion) ))^2
      ys <- hh_obs[complete.cases(hh_obs[, all.vars(formula(fit_glm))]), paste(formula(fit_glm)[[2]]) ]

      # aggregate data and predictions
      x1 <- data.frame(hh_obs[complete.cases(hh_obs[, all.vars(formula(fit_glm)) ]), part_unit], 
        ys, lambdas, variances)
      colnames(x1) <- c(part_unit, "ys", "lambdas", "variances")
      x1 <- aggregate(x1[, c("ys", "lambdas", "variances")], by=list(x1[, part_unit]), FUN = sum, na.rm = TRUE)
      colnames(x1) <- c(part_unit, "ys", "lambdas", "variances")

      # calculate Dawid-Sebastiani score
      scores_glm["dss_glm"] <- mean((x1$ys - x1$lambdas)^2 / x1$variances + 2*log(x1$variances), na.rm = TRUE)
        
      # calculate mean square error
      scores_glm["mse_glm"] <- mean((x1$ys - x1$lambdas)^2, na.rm = TRUE)

      # calculate deviance F test p-value
      f_test_glm <- anova(fit_glm, test = "F")[2, "Pr(>F)"]
  }    
    
  #...................................  
  ## Evaluate mixed model option
    ### NOTE: COMPUTATIONALLY INTENSIVE, ESPECIALLY IF THE DATASET IS LARGE
  if (gen_pars[gen_pars$parameter == "force_glm", "value"] != "glm") {  
    
    # Fit mixed model
    form <- as.formula( paste("n_died", "~", paste(vars_in, collapse="+"), "+ (1|", part_unit, ")", sep="")  )
    x1 <- ifelse(fit_family == "quasipoisson", "nbinom1", "poisson")
    fit_glmm <- glmmTMB(form, family = x1, data = hh_obs, weights = wt, offset = log(ptime) )
    broom.mixed::tidy(fit_glmm, exponentiate = TRUE)
    broom.mixed::glance(fit_glmm)    
    f_plot_pred(fit_glmm, part_unit, TRUE)
    
    # Calculate predictive scores
      # scores
      scores_glmm <- c(NA, NA)
      names(scores_glmm) <- c("dss_glmm", "mse_glmm")

      # calculate elements required for predictive scores
      lambdas <- predict(fit_glmm, type = "response")
      variances <- (exp(predict(fit_glmm) * (1 + summary(fit_glmm)[["sigma"]]) ))^2
      ys <- hh_obs[complete.cases(hh_obs[, vars_in]), paste(formula(fit_glmm)[[2]]) ]
      
      # aggregate data and predictions
      x1 <- data.frame(hh_obs[complete.cases(hh_obs[, all.vars(formula(fit_glm)) ]), part_unit], 
        ys, lambdas, variances)
      colnames(x1) <- c(part_unit, "ys", "lambdas", "variances")
      x1 <- aggregate(x1[, c("ys", "lambdas", "variances")], by=list(x1[, part_unit]), FUN = sum, na.rm = TRUE)
      colnames(x1) <- c(part_unit, "ys", "lambdas", "variances")

      # calculate Dawid-Sebastiani score
      scores_glmm["dss_glmm"] <- mean((x1$ys - x1$lambdas)^2 / x1$variances + 2*log(x1$variances), na.rm = TRUE)
        
      # calculate mean square error
      scores_glmm["mse_glmm"] <- mean((x1$ys - x1$lambdas)^2, na.rm = TRUE)
      
    # Do cross-validation
      # predictive scores on CV
      scores_cv_glmm <- c(NA, NA)
      names(scores_cv_glmm) <- c("dss_cv_glmm", "mse_cv_glmm")
      scores_cv_glmm <- f_cv(fit_glmm, hh_obs, part_unit, k_folds, TRUE, TRUE)
      
      # loss of DSS/MSE from CV
      scores_cv_glmm["dss_cv_glmm"] - scores_glmm["dss_glmm"]
      scores_cv_glmm["mse_cv_glmm"] - scores_glmm["mse_glmm"]
  }          
        
    # Select between fixed-effects only and mixed model and save fit statistics
      # if the choice should be based on relative MSE...
      if (force_glm == "either") {
        if (scores_cv_glmm["mse_cv_glmm"] < scores_cv_glm["mse_cv_glm"]) {fit_best <- fit_glmm; 
          fit_best_stats <- c(AIC(fit_best), NA, scores_glmm, scores_cv_glmm)} 
        if (scores_cv_glmm["mse_cv_glmm"] >= scores_cv_glm["mse_cv_glm"]) {fit_best <- fit_glm; 
          fit_best_stats <- c(AIC(fit_best), f_test_glm, scores_glm, scores_cv_glm)}
      } 
      
      # if the GLM (fixed-effects only) model should be adopted...
      if (force_glm == "glm") {
        fit_best <- fit_glm; 
        fit_best_stats <- c(AIC(fit_glm), f_test_glm, scores_glm, scores_cv_glm)
      } 
      
      # if the GLMM (mixed-effects) model should be adopted...
      if (force_glm == "glmm") {
        fit_best <- fit_glmm; 
        fit_best_stats <- c(AIC(fit_glmm), NA, scores_glmm, scores_cv_glmm)
      } 
          
  #...................................  
  ## Calculate robust standard errors (only if a fixed-effect model is selected)
  
  if (! part_unit %in% all.vars(formula(fit_best)) ) {      
    # Generate variance-covariance matrix by specifying the desired (unique) cluster variable(s)
    hh_obs[, cluster_vars] <- paste(hh_obs[, part_unit], "_", hh_obs[, cluster_vars], sep = "")
    vcov_cl <- cluster.vcov(fit_best, hh_obs[, cluster_vars])     
    
    # Calculate and save model output with robust standard errors
    out <- f_rob(fit_best, vcov_cl, "linear")
    out
  }

  #...................................  
  ## Calculate additional model performance metrics on cross-validation; save output
    ### NOTE: VERY COMPUTATIONALLY INTENSIVE IF RANDOM EFFECTS MODEL IS SELECTED
      
    # Do cross-validation and compute metrics by partition unit
    x2 <- f_cv_metrics(fit_best, hh_obs, part_unit, k_folds, FALSE)
    
    # Save output
    write.csv(x2, paste(country, "_est_", y_hat, "_final_model_cv_metrics", ".csv", sep = ""), row.names = FALSE)
      
    # Plot performance accuracy
    plot <- ggplot(x2) + 
      geom_point(aes(x = obs, y = pred), size = 2, colour = palette_cb[6], alpha = 0.5) +
      theme_bw() +
      scale_x_continuous("observed number of deaths", limits = c(0, 1.1 * max(x2[, c("obs", "pred")], na.rm=TRUE) )) +
      scale_y_continuous("predicted number of deaths", limits = c(0, 1.1 * max(x2[, c("obs", "pred")], na.rm=TRUE) )) +  
      geom_abline(intercept = 0, slope = 1, colour = palette_cb[7], size = 1, alpha = 0.5) +
      theme(axis.title = element_text(colour = "grey20"))
    print(plot)
    ggsave(paste(country, "_", y_hat, "_est_final_model_cv.png", sep=""), plot, height = 13, width = 20, units = "cm", dpi = "print")    

    # Compute overall metrics
    x3 <- c((sum(x2$pred) - sum(x2$obs)) / sum(x2$obs), colMeans(x2[, c("rel_precision95", "coverage95", "coverage80")], na.rm = TRUE))
    names(x3) <- c("rel_bias", "rel_precision95", "coverage95", "coverage80")

    
  #...................................  
  ## Write final model to file
        
    # Write output to file
    x1 <- paste(country, "_est_", y_hat, "_final_model", ".csv", sep ="")
    if (part_unit %in% all.vars(formula(fit_best)) ) 
    {write.table(cbind(broom.mixed::tidy(fit_best, exponentiate = TRUE), exp(confint(fit_best))),
      x1, sep = ",", row.names = FALSE)}
    if (! part_unit %in% all.vars(formula(fit_best)) ) 
      {write.table(out, x1, sep = ",", row.names = TRUE) }
    write.table("--------------------", x1, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    write.table("Fit statistics:", x1, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    write.table(rbind(c("aic", "f_test","dss", "mse", "dss_cv", "mse_cv") , 
      fit_best_stats), x1, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
    write.table("--------------------", x1, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    write.table("Additional performance metrics:", x1, sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
    write.table(rbind(names(x3), x3), x1, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)


#.........................................................................................
### Alternative: Boosted regression tree model or Random Forest (with random effects)
#.........................................................................................
    
    ### NOT YET IMPLEMENTED (waiting for GPBoost package to become available on CRAN)
    
#.........................................................................................
### Saving results
#.........................................................................................
    
  # Save fit object
  saveRDS(fit_best, paste(country, "_", y_hat, "_fit_best", ".rds", sep="")) 

          
#.........................................................................................
### ENDS
#.........................................................................................
