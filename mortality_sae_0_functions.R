#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## --------------------------------- FUNCTIONS FOR ANALYSIS ---------------------------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Nov 2020)
                                          # francesco.checchi@lshtm.ac.uk 



#.........................................................................................
### Function that calculates the proportion of a given month's days that is covered by a survey's recall period
#.........................................................................................

f_calc_days <- function(f_surveys_cov, f_df) {
  # select survey
  s <- subset(f_df, survey_id == f_surveys_cov["survey_id"])
  tm_now <- as.integer(f_surveys_cov["tm"])
  c1 <- as.integer(s["tm_recall_start"]) - tm_now
  c2 <- as.integer(s["tm_recall_end"])- tm_now
  x1 <- 0
  
  # calculate proportion of the month's days that are covered by the survey's recall period
  if (c1 > 0) { x1 <- 0.0 }
  if (c1 == 0) { x1 <- (as.integer(s["days_in_month_start"]) - as.integer(s["day_start"]) ) / 
    as.integer(s["days_in_month_start"]) }
  if (c1 < 0 & c2 > 0) { x1 <- 1.0 }
  if (c2 == 0) { x1 <- as.integer(s["day_end"]) / as.integer(s["days_in_month_end"]) }
  if (c2 < 0) { x1 <- 0.0 }  
    
  return(x1)
  }
  

#.........................................................................................
### Function that calculates and formats median, range and number of observations for any quantity, overall and by year
#.........................................................................................

f_calc_svy <- function(f_quantity, f_df, f_digits, f_years) {
    
    # output of n elements, where n = 1 (total) + number of years
    x1 <- rep(NA, times=length(f_years + 1))
  
    # overall (all years)
    x1[1] <- paste( round(median(f_df[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), " (", 
                     round(min(f_df[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), " to ",
                     round(max(f_df[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), ", ", 
                     length(na.omit(f_df[, paste(f_quantity)]) ), ")"
                     , sep="" )
    
    x1_pos <- 1
    # for each year...
    for (i in f_years) {
      x1_pos <- x1_pos + 1
      x1[x1_pos] <- paste(round(median(subset(f_df, year_survey == i)[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), " (", 
                      round(min(subset(f_df, year_survey == i)[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), " to ",
                      round(max(subset(f_df, year_survey == i)[, paste(f_quantity)], na.rm=TRUE), digits=f_digits), ", ", 
                      length(na.omit(subset(f_df, year_survey == i)[, paste(f_quantity)]) ), ")", sep="" )
      }
    return(x1)
} 


#.........................................................................................
### Function to perform K-fold cross-validation, compute predictive scores and plot results
#.........................................................................................

f_cv <- function(f_fit, f_data, f_part_unit, f_k_folds, f_plot, f_scores) {
  
  # Remove missing observations from data
  f_data <- f_data[complete.cases(f_data[, all.vars(formula(f_fit)) ]), ]
  
  # Determine number of folds if f_k_folds = NA (i.e. LOOCV case)
  if (is.na(f_k_folds) == TRUE) {
    if (f_part_unit == "household") {f_k_folds <- nrow(f_data) }
    if (f_part_unit != "household") {f_k_folds <- length(unique(f_data[, f_part_unit])) }
  }
    
  # If partition unit = household...
  if (f_part_unit == "household") {
    # shuffle dataset
    f_data <- f_data[sample(nrow(f_data), nrow(f_data), replace=FALSE), ]
    # split data into K folds
      # remove a few rows so as to come up with a n row divisible by K
      f_data <- f_data[1:(floor(nrow(f_data)/f_k_folds)*f_k_folds), ]
      
      # split
      folds <- split(f_data, (1:nrow(f_data) %/% (nrow(f_data)/f_k_folds)))
    }
    
  # If partition unit != household...
  if (f_part_unit != "household") {
    # shuffle dataset
      # attribute a random value to each unique partition unit
      x1 <- unique(f_data[, f_part_unit])
      x2 <- data.frame(x1, sample(c(1:length(x1)), length(x1), replace=FALSE) )
      colnames(x2) <- c(f_part_unit, "rand_rank")
      f_data <- merge(f_data, x2, by = f_part_unit)
      
      # sort dataset on the basis of that random value
      f_data <- f_data[order(f_data[, "rand_rank"]), ]
      
    # split data into K folds
      # split unique partition units into the desired number of folds
      x1 <- split(x2[, "rand_rank"], sort(x2[, "rand_rank"]%%f_k_folds))
      
      # reshape as dataframe with fold ID as factor and partition unit rank
      x2 <- data.frame(fold = rep(names(x1), sapply(x1, length)), rand_rank = unlist(x1))
      
      # merge with dataset
      f_data <- merge(f_data, x2, by="rand_rank")
      
      # now split dataset by fold as the factor
      folds <- split(f_data, f_data$fold)
  }
        
  # Fit model on all the unfolded sets and track predictive scores of model fit on each fold  
    # vector to hold the partition units
    part_units <- c()
    
    # vector to hold the observations
    obs <- c()
    
    # vector to hold the predictions
    pred <- c()
    
    # database to hold the components to calculate DSS scores
    scores <- data.frame()
    
  for (i in 1:length(folds) ) {	
    # control statement
    print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))
    
    # fit on all data but the fold
    cv_fit <- update(f_fit, formula=formula(f_fit),  family = family(f_fit)[[1]], data = do.call(rbind, folds[-i]))
    
    # calculate components of predictive scores of model when predicting fold data
    lambdas <- predict(cv_fit, newdata = folds[[i]], type = "response", allow.new.levels = TRUE)
    if (f_part_unit %in% all.vars(formula(f_fit))) {overdisp <- summary(cv_fit)[["sigma"]]}
    if (! f_part_unit %in% all.vars(formula(f_fit))) {overdisp <-summary(cv_fit)$dispersion}      
    variances <- (exp(predict(cv_fit, newdata = folds[[i]], allow.new.levels = TRUE ) * (1 + overdisp) ))^2
    ys <- folds[[i]][, paste(formula(f_fit)[[2]]) ]
    x3 <- cbind(ys, lambdas, variances)
    
    # add fold results to output vectors
    part_units <- c(part_units, folds[[i]][, f_part_unit])
    obs <- c(obs, ys)
    pred <- c(pred, lambdas)
    scores <- rbind(scores , x3 )

  }
  
  # Plot predictive accuracy if desired
  if (f_plot == TRUE) {
    # aggregate data and predictions
    x1 <- as.data.frame(cbind(part_units, obs, pred))
    colnames(x1) <- c(f_part_unit, "observed", "predicted")
    x1 <- aggregate(x1[, c("observed", "predicted")], by=list(x1[, f_part_unit]), FUN=sum, na.rm=TRUE)
    colnames(x1) <- c(f_part_unit, "observed", "predicted")
    
    # plot
    plot <- ggplot(x1) + geom_point(aes(x = observed, y = predicted), size=2, colour="steelblue") + theme_bw() +
      scale_x_continuous("observed number of deaths", limits=c(0, 1.1*max(x1[, 2:3], na.rm=TRUE))) +
      scale_y_continuous("predicted number of deaths", limits=c(0, 1.1*max(x1[, 2:3], na.rm=TRUE))) +  
      geom_abline(intercept = 0, slope = 1, colour="red") +
      theme(axis.title = element_text(colour="grey20")) +
      ggtitle(paste("Cross-validation results by ", f_part_unit, sep = "")) +
      labs(subtitle = formula(f_fit)) +
      theme(plot.title = element_text(colour="grey20", face = "plain", size = 8))
    print(plot)
  }
    
  # Return prediction scores (DSS and MSE) across all folds, if desired
  if (f_scores == TRUE) {
    # aggregate data and predictions
    x1 <- as.data.frame(cbind(part_units, scores))
    colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")
    x1 <- aggregate(x1[, c("ys", "lambdas", "variances")], by=list(x1[, f_part_unit]), FUN = sum, na.rm = TRUE)
    colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")
    
    # calculate and return scores
      # Dawid-Sebastiani score
      dss <- (x1$ys - x1$lambdas)^2 / x1$variances + 2*log(x1$variances)
      
      # mean square error
      mse <- (x1$ys - x1$lambdas)^2
    
      out <- c(mean(dss, na.rm=TRUE), mean(mse, na.rm=TRUE))
      if (f_part_unit %in% all.vars(formula(f_fit))) {names(out) <- c("dss_cv_glmm", "mse_cv_glmm")}
      if (! f_part_unit %in% all.vars(formula(f_fit))) {names(out) <- c("dss_cv_glm", "mse_cv_glm")}    

    return(out)
  }
    
  # Alternatively, return the cross-validation predictions and observations for each partition unit
  if (f_scores == FALSE) { return(x1) }
  
  }
  

#.........................................................................................
### Function to generate additional metrics of fit performance, based on cross-validation results
#.........................................................................................
 
f_cv_metrics <- function(f_fit, f_data, f_part_unit, f_k_folds, f_overall) {
  
  # Remove missing observations from data
  f_data <- f_data[complete.cases(f_data[, all.vars(formula(f_fit)) ]), ]
  
  # Determine number of folds if f_k_folds = NA (i.e. LOOCV case)
  if (is.na(f_k_folds) == TRUE) {
    if (f_part_unit == "household") {f_k_folds <- nrow(f_data) }
    if (f_part_unit != "household") {f_k_folds <- length(unique(f_data[, f_part_unit])) }
  }
    
  # If partition unit = household...
  if (f_part_unit == "household") {
    # shuffle dataset
    f_data <- f_data[sample(nrow(f_data), nrow(f_data), replace=FALSE), ]
    # split data into K folds
      # remove a few rows so as to come up with a n row divisible by K
      f_data <- f_data[1:(floor(nrow(f_data)/f_k_folds)*f_k_folds), ]
      # split
      folds <- split(f_data, (1:nrow(f_data) %/% (nrow(f_data)/f_k_folds)))
    }
    
  # If partition unit != household...
  if (f_part_unit != "household") {
    # shuffle dataset
      # attribute a random value to each unique partition unit
      x1 <- unique(f_data[, f_part_unit])
      x2 <- data.frame(x1, sample(c(1:length(x1)), length(x1), replace=FALSE) )
      colnames(x2) <- c(f_part_unit, "rand_rank")
      f_data <- merge(f_data, x2, by=f_part_unit)
      
      # sort dataset on the basis of that random value
      f_data <- f_data[order(f_data[, "rand_rank"]), ]
      
    # split data into K folds
      # split unique partition units into the desired number of folds
      x1 <- split(x2[, "rand_rank"], sort(x2[, "rand_rank"]%%f_k_folds))
      
      # reshape as dataframe with fold ID as factor and partition unit rank
      x2 <- data.frame(fold = rep(names(x1), sapply(x1, length)), rand_rank = unlist(x1))
      # merge with dataset
      f_data <- merge(f_data, x2, by="rand_rank")
      
      # now split dataset by fold as the factor
      folds <- split(f_data, f_data$fold)
  }
        
  # Fit model on all the unfolded sets and compute point estimates and confidence intervals for the predictions  
    # dataframe to hold results
    out <- c()
   
  for (i in 1:length(folds) ) {	
    # control statement
    print(paste("now working on fold  ", i, " of  ", length(folds), sep=""))
    
    # fit on all data but the fold
    cv_fit <- update(f_fit, formula=formula(f_fit),  family=family(f_fit)[[1]], data = do.call(rbind, folds[-i]))
    
    # calculate point estimates and standard errors of predictions
    lambdas <- predict(cv_fit, newdata = folds[[i]], type="link", allow.new.levels = TRUE)
    ses <- predict(cv_fit, newdata = folds[[i]], se.fit = TRUE, allow.new.levels = TRUE)$se.fit
    ys <- folds[[i]][complete.cases(folds[[i]][, all.vars(formula(f_fit))]), paste(formula(f_fit)[[2]]) ]
    part_units <- folds[[i]][complete.cases(folds[[i]][, all.vars(formula(f_fit))]), f_part_unit ]
    
    # calculate bootstrap 80% and 95% confidence intervals
    x1 <- t(apply(cbind(lambdas, ses), 1, FUN = function(x) {rnorm(1000, mean = x[1], sd = x[2]) } ) ) 
    if (as.character(family(f_fit))[1] != "gaussian") {x1 <- exp(x1)}
    x1 <- t(apply(x1, 1, sort) )
    x2 <- t(apply(x1, 1, quantile, c(0.025, 0.10, 0.90, 0.975)))

    # add fold results to output vectors
    x2 <- cbind(part_units, ys, exp(lambdas), x2)
    out <- rbind(out, x2)

  }
  
  # Aggregate predictions and calculate performance metrics by partition unit
  out <- as.data.frame(out)
  colnames(out) <- c(f_part_unit, "obs", "pred", "pred_l95", "pred_l80", "pred_u80", "pred_u95")
  x1 <- aggregate(out[, c("obs", "pred", "pred_l95", "pred_l80", "pred_u80", "pred_u95")], 
    by = list(as.character(out[, f_part_unit])), FUN=sum, na.rm=TRUE)
  colnames(x1)[1] <- f_part_unit
  x1[, "rel_precision95"] <- (abs(x1[, "pred_u95"] - x1[, "pred_l95"] ) / 2) / x1[, "pred"]
  x1[, "coverage95"] <- between(x1[, "obs"], x1[, "pred_l95"], x1[, "pred_u95"])
  x1[, "coverage80"] <- between(x1[, "obs"], x1[, "pred_l80"], x1[, "pred_u80"])

  # Return performance metrics
    # by partition unit if desired...
    if (f_overall == FALSE) {return(x1)}
  
    # ...or overall otherwise
    if (f_overall == TRUE) {
      x2 <- c((sum(x1$pred) - sum(x1$obs)) / sum(x1$obs), colMeans(x1[, c("rel_precision95", "coverage95", "coverage80")], na.rm = TRUE))
      names(x2) <- c("rel_bias", "rel_precision95", "coverage95", "coverage80")
      return(x2)
    }
}


#.........................................................................................
### Function to aggregate based on any groups and calculate point estimates + 95%CIs for death tolls and rates
#.........................................................................................

f_est <- function(f_scenarios, f_runs, f_ag_groups, f_tm_start, f_tm_end) {

  # Prepare output
  out <- data.frame(unique(subset(ts_ac, tm %in% c(f_tm_start : f_tm_end) )[, f_ag_groups]))
  colnames(out) <- f_ag_groups

  # Aggregate each scenario and compute point estimates and 95%CIs
  for (i in f_scenarios) {
    
    # for all ages and children under 5y...
    for (j in c("", "_u5")) {

      # access bootstrap sample and restrict timespan as desired
      x1 <- get(paste("toll", j, "_", i, sep=""))
      x1 <- subset(x1, tm %in% c(f_tm_start : f_tm_end) )

      # death tolls
        # aggregate bootstrap sample of death tolls
        if (length(f_ag_groups) == 1 ) { x2 <- aggregate(x1[, c(as.character(seq(1:f_runs))) ] , 
          by = list(x1[, f_ag_groups]), FUN = sum ) }
        if (length(f_ag_groups) > 1 ) { x2 <- aggregate(x1[, c(as.character(seq(1:f_runs))) ] , 
          by = x1[, f_ag_groups], FUN = sum ) }
        colnames(x2) <- c(f_ag_groups, as.character(seq(1:f_runs)) )
        
        # calculate point estimate and 95%CIs of death tolls
        x3 <- t(apply(x2[, as.character(seq(1:f_runs))], 1, 
          FUN = function(x) {return(quantile(x, c(0.5, 0.025, 0.975)))} ))
        out_sub <- data.frame(x2[, f_ag_groups], x3)
        colnames(out_sub) <- c(f_ag_groups, c(paste("toll", j, "_", i, c("_est", "_lci", "_uci"), sep="")) )
        
      # death rates
        # generate person-time
        x1[, paste("ptime", j, sep = "")] <- x1[, paste("pop_average", j, sep = "")] * x1[, "days_in_month"]
        
        # aggregate person-time
        if (length(f_ag_groups) == 1 ) { x2 <- aggregate(x1[, paste("ptime", j, sep = "")] , 
          by = list(x1[, f_ag_groups]), FUN = sum  )  }
        if (length(f_ag_groups) > 1 ) { x2 <- aggregate(x1[, paste("ptime", j, sep = "")] , 
          by = x1[, f_ag_groups], FUN = sum  ) }
        
        # calculate point estimate and 95%CIs of death rates (per 10,000 person-days)
        out_sub[, c(paste("cdr", j, "_", i, c("_est", "_lci", "_uci"), sep=""))] <- 
          out_sub[, grepl("toll", colnames(out_sub))] * 10000 / x2[, ncol(x2)]

      # merge output
      out <- merge(out, out_sub, by = f_ag_groups, all.x = TRUE, sort = TRUE)
    }        
  }

  # Re-calculate excess death rates as difference between actual and counterfactual
  for (i in grep("ex", f_scenarios, value = TRUE) ) {
    
    # for all ages and children under 5y...
    for (j in c("", "_u5")) {
      out[, c(paste("cdr", j, "_", i, c("_est", "_lci", "_uci"), sep=""))] <-
        out[, c(paste("cdr", j, "_", "ac", c("_est", "_lci", "_uci"), sep=""))] -
            out[, c(paste("cdr", j, "_", gsub("ex", "cf", i), c("_est", "_lci", "_uci"), sep=""))]
    }            
  }
  
  # Format output
  out[, grepl("toll", colnames(out))] <- format(round(out[, grepl("toll", colnames(out))] , digits=-2) , big.mark="," , scientific=FALSE)
    
  # Return output
  return(out)
} 
 

#.........................................................................................
### Function to plot histograms of variables
#.........................................................................................  

f_hist <- function(f_var, f_data, f_lims) {
    
  plot <- ggplot(f_data)
      
    # if the variable has >= 20 unique values...
      if (length(unique(na.omit(f_data[, f_var]))) >= 20) {
        plot <- plot + geom_histogram(aes(x = as.numeric(f_data[, f_var]) ), 
          color="seagreen", fill="seagreen3", alpha = 0.5 ) +
          theme_bw() + xlab(f_var) + scale_x_continuous(expand = c(0, 0), limits = f_lims )
      }
 
    # otherwise...
      if (length(unique(na.omit(f_data[, f_var]))) < 20) {
        plot <- plot + geom_histogram(aes(x = as.factor(f_data[, f_var]) ), stat="count", 
          color="seagreen", fill="seagreen3", alpha = 0.5) +
          theme_bw() + xlab(f_var)
      }
        
    print(plot)
  }


#.........................................................................................
### Function to standardise livelihood type nomenclature
#.........................................................................................   

f_liv <- function(f_ts, f_livelihood_substrings) {
  # Agriculturalists
  if (length(grep(paste(f_livelihood_substrings$agriculturalists, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[2]) )
  
  # Pastoralists (slightly different to avoid confusion with agropastoralists)
  if (length(grep(paste(f_livelihood_substrings$pastoralists, collapse="|"), f_ts )) > 0  & 
    length(grep(paste(f_livelihood_substrings$agropastoralists, collapse="|"), f_ts )) == 0
    ) 
    return( paste(names(livelihood_substrings)[3]) )
  
  # Agropastoralists       
  if (length(grep(paste(f_livelihood_substrings$agropastoralists, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[4]) )
  
  # Riverine        
  if (length(grep(paste(f_livelihood_substrings$riverine, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[5]) )
  
  # Fishing       
  if (length(grep(paste(f_livelihood_substrings$fishing, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[6]) )
  
  # Urban        
  if (length(grep(paste(f_livelihood_substrings$urban, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[7]) )
  
  # Displaced        
  if (length(grep(paste(f_livelihood_substrings$displaced, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[8]) )
  
  # Refugee       
  if (length(grep(paste(f_livelihood_substrings$refugee, collapse="|"), f_ts )) > 0 ) 
    return( paste(names(livelihood_substrings)[9]) )
  
  # Otherwise return NA
  if (length(grep(paste(unlist(f_livelihood_substrings), collapse="|"), f_ts )) == 0 ) 
    return( NA )
    
}      


#.........................................................................................
### Function to graph predictions versus observations for a fitted model
#.........................................................................................

f_plot_pred <- function(f_fit, f_part_unit, f_re) {

  # source data needed
  obs <- model.extract(model.frame(f_fit), "response")
  pred <- predict(f_fit, type = "response")
  if (f_re == TRUE) {part_units <- f_fit[["frame"]][f_part_unit]}
  if (f_re == FALSE) {x1 <- f_fit[["data"]];
    x1 <- x1[complete.cases(x1[, all.vars(formula(f_fit))]), ];
    part_units <- x1[, f_part_unit]
    }

  # aggregate data and predictions by partitioning unit (e.g. stratum)
  x1 <- as.data.frame(cbind(part_units, obs, pred))
  colnames(x1) <- c(f_part_unit, "observed", "predicted")
  x1 <- aggregate(x1[, c("observed", "predicted")], by=list(x1[, f_part_unit]), FUN=sum, na.rm=TRUE)
  colnames(x1) <- c(f_part_unit, "observed", "predicted")
    
  # plot
  plot <- ggplot(x1) + geom_point(aes(x = observed, y = predicted), size=2, colour="steelblue") + theme_bw() +
    scale_x_continuous("observed number of deaths", limits=c(0, 1.1*max(x1[, 2:3], na.rm=TRUE))) +
    scale_y_continuous("predicted number of deaths", limits=c(0, 1.1*max(x1[, 2:3], na.rm=TRUE))) +  
    geom_abline(intercept = 0, slope = 1, colour="red") +
    theme(axis.title = element_text(colour="grey20")) +
    ggtitle(formula(f_fit)) +
    theme(plot.title = element_text(size = 9, face = "plain", colour = "grey20"))
  print(plot)
  return(plot)
  }
    

#.........................................................................................
### Function to predict on new data, given a robust variance-covariance matrix (for fixed-effects models only)
  # (based on https://stackoverflow.com/questions/3790116/using-clustered-covariance-matrix-in-predict-lm?noredirect=1&lq=1 )
#.........................................................................................

f_predict <- function(f_fit, f_vcov_cl, f_newdata, f_se_fit) {
    
  # if new data are missing, revert to predicting on training dataset
  if (missing(f_newdata)) { f_newdata <- f_fit$model }
    
  # identify terms of the model and remove the response term
  fit_terms <- delete.response( terms(f_fit) )
    
  # construct model matrix
  m_mat<- model.matrix(fit_terms, model.frame(fit_terms, f_newdata, na.action = "na.pass"))
    
  # access model coefficients
  m_coef <- f_fit$coef
    
  # generate predictions on the desired scale, as well as standard errors for the predictions
  if (family(f_fit)[2] == "log") {out <- as.vector(m_mat %*% f_fit$coef)}
  if (family(f_fit)[2] == "linear") {out <- exp(as.vector(m_mat %*% f_fit$coef))}
  se_fit <- sqrt(diag(m_mat %*% f_vcov_cl %*% t(m_mat)))
    
  # return predictions or standard errors, as desired      
  if (f_se_fit == TRUE) {return(se_fit)} else {return(out)}
}


#.........................................................................................
### Function to estimate robust standard errors for model coefficients, given a robust variance-covariance matrix
#.........................................................................................
  
f_rob <- function(f_fit, f_vcov_cl, f_scale) {
  
  # Calculate robust standard errors
  std.err <- sqrt(diag(f_vcov_cl))
    
  # Output on the log scale
  if (f_scale == "log") {
    r_est <- data.frame("Rate ratio" = round(coef(f_fit), 3),
      "95%CI - lower" = round(coef(f_fit) - 1.96 * std.err, 3),
      "95%CI - upper" = round(coef(f_fit) + 1.96 * std.err, 3),
      "Pr(>|z|)" = round(2 * pnorm(abs(coef(f_fit)/std.err), lower.tail=FALSE), 3)
    )
  }
  
  # Output on the linear scale
  if (f_scale == "linear") {
    r_est <- data.frame("Rate ratio" = round(exp(coef(f_fit)), 3),
      "95%CI - lower" = round(exp(coef(f_fit) - 1.96 * std.err), 3),
      "95%CI - upper" = round(exp(coef(f_fit) + 1.96 * std.err), 3),
      "Pr(>|z|)" = round(2 * pnorm(abs(coef(f_fit)/std.err), lower.tail=FALSE), 3)
    )
  }
  
  # Return output
  return(r_est)
  
}


#.........................................................................................
### Function to execute sections of code 
  # (based on https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file )
#.........................................................................................

f_source_part <- function(f_script, f_start_tag, f_end_tag) {

  # Identify lines with start and end tags
  st <- grep(f_start_tag, f_script)
  en <- grep(f_end_tag, f_script)
  
  # Set up a connection
  tc <- textConnection(f_script[(st + 1):(en - 1)])
  
  # Run the script
  source(tc)
  
  # Close the connection
  close(tc)
}



#.........................................................................................
### Function to fit any model formula (with/out random effects), generate model fit statistics and plot CV 
#.........................................................................................

f_val <- function(f_preds, f_data, f_re, f_family, f_cv, f_part_unit, f_k_folds, f_plot, f_show_output) {

  # Select non-missing data
  f_data <- f_data[complete.cases(f_data[, f_preds]), ]
  
  # Prepare output
  out <- rep(NA, times = 12)
  
  # Fixed effects only model (always)
    # write the model formula
    form <- as.formula( paste("n_died", "~", paste(f_preds, collapse="+"), sep="")  )
      
    # fit GLM
    fit <- glm(form, data = f_data, family = f_family, weights = wt, offset = log(ptime) )
      # print model summary
      if (f_show_output == TRUE) {
        print(tidy(fit, exponentiate = TRUE));
        print(glance(fit))
      }
      
      # record AIC
      out[1] <- fit$aic
      
      # generate elements to calculate predictive scores
      lambdas <- predict(fit, type = "response")
      variances <- (exp(predict(fit) * (1 + summary(fit)$dispersion) ))^2
      ys <- f_data[, paste(formula(fit)[[2]]) ]

      # aggregate data and predictions
      x1 <- as.data.frame(cbind(f_data[, f_part_unit], ys, lambdas, variances))
      colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")
      x1 <- aggregate(x1[, c("ys", "lambdas", "variances")], by=list(x1[, f_part_unit]), FUN = sum, na.rm = TRUE)
      colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")

      # calculate Dawid-Sebastiani score
      out[2] <- mean((x1$ys - x1$lambdas)^2 / x1$variances + 2*log(x1$variances), na.rm = TRUE)
      
      # calculate mean square error
      out[3] <- mean((x1$ys - x1$lambdas)^2, na.rm = TRUE)

      # calculate deviance F test p-value
      out[4] <- anova(fit, test = "F")[2, "Pr(>F)"]
    
    # if desired, do cross-validation and extract sum DSS
    if (f_cv == TRUE) { out[c(5,6)] <- f_cv(fit, f_data, f_part_unit, f_k_folds, f_plot, TRUE) }
      
  # Random effects model, if desired
  if (f_re == TRUE) {
    # write the model formula
    form <- as.formula( paste("n_died", "~", paste(f_preds, collapse="+"), "+(1|stratum)", sep="")  )
    
    # specify family
    family_re <- "poisson"
    if (f_family == "quasipoisson") {family_re <- "nbinom1"}
      
    # fit GLM with random effect
    fit <- glmmTMB(form, data = f_data, family = family_re, weights = wt, offset=log(ptime) )
      
      # print model summary
      if (f_show_output == TRUE) {
        print(summary(fit))
        print(broom.mixed::tidy(fit, exponentiate = TRUE))
        print(broom.mixed::glance(fit))
      }
    
      # record AIC
      out[7] <- AIC(fit)
      
      # generate elements to calculate predictive scores        
      lambdas <- predict(fit, type = "response")
      variances <- (exp(predict(fit) * (1 + summary(fit)[["sigma"]]) ))^2
      ys <- f_data[, paste(formula(fit)[[2]]) ]
      
      # aggregate data and predictions
      x1 <- as.data.frame(cbind(f_data[, f_part_unit], ys, lambdas, variances))
      colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")
      x1 <- aggregate(x1[, c("ys", "lambdas", "variances")], by=list(x1[, f_part_unit]), FUN = sum, na.rm = TRUE)
      colnames(x1) <- c(f_part_unit, "ys", "lambdas", "variances")

      # calculate Dawid-Sebastiani score
      out[8] <- mean((x1$ys - x1$lambdas)^2 / x1$variances + 2*log(x1$variances), na.rm = TRUE)
      
      # calculate mean square error
      out[9] <- mean((x1$ys - x1$lambdas)^2, na.rm = TRUE)

      # calculate deviance F test p-value / not possible for mixed models
      out[10] <- NA
    
    # if desired, do cross-validation and extract mean DSS and MSE
    if (f_cv == TRUE) { out[c(11,12)] <- f_cv(fit, f_data, f_part_unit, f_k_folds, f_plot, TRUE) }
  }
      
  # Return results
  return(out)
}    
  
    
  
#.........................................................................................
### ENDS
#.........................................................................................
