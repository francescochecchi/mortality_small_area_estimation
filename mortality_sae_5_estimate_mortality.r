#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## ----------- R CODE TO ESTIMATE EXCESS MORTALITY ACROSS STRATA AND OVERALL ----------- ##
#..........................................................................................

                                      # Written by Francesco Checchi, LSHTM (November 2020)
                                      # francesco.checchi@lshtm_ac_uk 




#.........................................................................................
### Reading in required files
#.........................................................................................

  #...................................
  ## Predictor data (actual scenario; from earlier code)
  ts_ac <- read.csv(paste(country, "_ts.csv", sep=""), sep=",")
#place1#  
    # Rename any variables as needed to harmonise with model terms (from x_obs)
    x1 <- subset(var_pars, is.na(var_name_ts) == FALSE)
    ts_ac[, x1$variable] <- ts_ac[, x1$var_name_ts]

#place2#       
  #...................................
  ## Final model fits (from earlier code)
  fit_cdr <- readRDS(paste(country, "_cdr_fit_best.rds", sep=""))
  fit_cdr_u5 <- readRDS(paste(country, "_cdr_u5_fit_best.rds", sep=""))
    
    # recalculate robust variance-covariance matrices of final fits, given cluster variable(s)
      # (only if fits are fixed-effects only)
    if (! part_unit %in% all.vars(formula(fit_cdr))) {
      vcov_cl_cdr <- cluster.vcov(fit_cdr, fit_cdr$data[, cluster_vars])
      vcov_cl_cdr_u5 <- cluster.vcov(fit_cdr_u5, fit_cdr_u5$data[, cluster_vars])  
    }
  
  #...................................
  ## Denote whether this is sensitivity analysis (see script 6) 
  sens <- FALSE
   
#place3#  
#...........................................................................................
### Preparing counterfactual scenarios: Predictors
#...........................................................................................

  #...................................
  ## Create counter-factual predictor datasets for each scenario
  for (i in scenarios[grepl("cf", scenarios)] ) {
    
    # Replicate actual time series
    ts_cf <- ts_ac
      # sort
      ts_cf <- ts_cf[order(ts_cf[, "admin1"], ts_cf[, "stratum"], ts_cf[, "tm"]), ]
    
    # Select scenario predictors
    cf_pars_s <- subset(cf_pars, scenario == gsub("cf_", "", i) & component == "predictors" )
    
    # For each scenario predictor...
    for (j in 1:nrow(cf_pars_s)) {
      
      # if the quantile of a reference baseline period should be used...
      if (is.na(cf_pars_s[j, "quantile"]) == FALSE) {
          
        # identify start and end time points of the reference period (if no dates are given, assume start/end of analysis period)
        m_start <- ifelse(is.na(cf_pars_s[j, "date_start"]) == TRUE, m_analysis_start, month(cf_pars_s[j, "date_start"]) )
        m_end <- ifelse(is.na(cf_pars_s[j, "date_end"]) == TRUE, m_analysis_end, month(cf_pars_s[j, "date_end"]) )
        y_start <- ifelse(is.na(cf_pars_s[j, "date_start"]) == TRUE, y_analysis_start, year(cf_pars_s[j, "date_start"]) )
        y_end <- ifelse(is.na(cf_pars_s[j, "date_end"]) == TRUE, y_analysis_end, year(cf_pars_s[j, "date_end"]) )
          # convert to corresponding tm values    
          tm_start  <-  t_units[t_units$y == y_start & t_units$m == m_start, "tm" ]
          tm_end  <-  t_units[t_units$y == y_end & t_units$m == m_end, "tm" ]

        # for each geographic unit of reference...
        for (k in unique(strata[, cf_pars_s[j, "geo_ref"]]) ) {
            
          # reference values
            # if the time unit is month, calculate a month-specific counterfactual
            if ( is.na(cf_pars_s[j, "time_ref"]) == FALSE & cf_pars_s[j, "time_ref"] == "month") {
              x1 <- subset(ts_cf, ts_cf[, cf_pars_s[j, "geo_ref"]] == k & tm >= tm_start & tm <= tm_end )[, c("m", cf_pars_s[j, "variable"])]
              x1 <- aggregate(x1[, cf_pars_s[j, "variable"] ], by = list(x1$m), 
                FUN = "quantile", probs = cf_pars_s[j, "quantile"], na.rm = TRUE)
              colnames(x1) <- c("m", cf_pars_s[j, "variable"])

              # substitute values with counterfactual value(s)
              x2 <- ts_cf[ts_cf[, cf_pars_s[j, "geo_ref"]] == k, c("tm", "m")]
              x2 <- merge(x2, x1, by = "m", all.x=TRUE)
              x2 <- x2[order(x2[, "tm"]), ]
              ts_cf[ts_cf[, cf_pars_s[j, "geo_ref"]] == k, cf_pars_s[j, "variable"]] <- x2[, cf_pars_s[j, "variable"]]
                
            }
                
            # else, calculate a constant counterfactual across time
            if (is.na(cf_pars_s[j, "time_ref"]) == TRUE) {
              x1 <- subset(ts_cf, ts_cf[, cf_pars_s[j, "geo_ref"]] == k & tm >= tm_start & tm <= tm_end )[, cf_pars_s[j, "variable"]]
              x1 <- quantile(x1, cf_pars_s[j, "quantile"], na.rm=TRUE)
              
              # substitute values with counterfactual value(s)
              ts_cf[ts_cf[, cf_pars_s[j, "geo_ref"]] == k, cf_pars_s[j, "variable"]] <- x1
            }
          }
        }      
        
      # if an absolute value should be assigned...
      if (is.na(cf_pars_s[j, "abs_value"]) == FALSE) {
        
        # substitute values with counterfactual value(s)
        ts_cf[, cf_pars_s[j, "variable"]]  <- cf_pars_s[j, "abs_value"]
      }
        
      # if a multiplying factor should be used...
      if (is.na(cf_pars_s[j, "rel_factor"]) == FALSE) {
          
        # substitute values with counterfactual value(s)
        ts_cf[, cf_pars_s[j, "variable"]]  <- ts_cf[, cf_pars_s[j, "variable"]] * cf_pars_s[j, "rel_factor"]
      }
      
      # if the counterfactual values need to be sourced from a dataset...
      if (is.na(cf_pars_s[j, "dataset"]) == FALSE) {
        
        # file name
        x1 <- paste(cf_pars_s[j, "dataset"])
        
        # read the dataset
        if (length(grep(".csv", x1)) > 0 ) {x2 <- read.csv(x1, header= TRUE)}
        if (length(grep(".xls", x1)) > 0 ) {x2 <- read_excel(x1)}
        
        # determine lowest levels of stratification
        x3 <- c()
          # time strata
          x4 <- c()
          if ("m" %in% colnames(x2) ) {x4 <- "m"}
          if ("y" %in% colnames(x2) ) {x4 <- c(x4, "y")}
          if ("tm" %in% colnames(x2) ) {x4 <- "tm"}
          
          # geo strata
          x5 <- c()
          if (admin2_name %in% colnames(x2) | "stratum" %in% colnames(x2) ) {x5 <- "stratum"}
          if (! admin2_name %in% colnames(x2) & ! "stratum" %in% colnames(x2) & 
              (admin1_name %in% colnames(x2) | "admin1" %in% colnames(x2) ) ) {x5 <- "admin1"}
        x3 <- c(x4, x5)

        # prepare database for merging with time series
          # rename columns in database to generic admin names
          colnames(x2)[colnames(x2) == admin2_name] <- "stratum"
          colnames(x2)[colnames(x2) == admin1_name] <- "admin1"
          
          # identify counterfactual scenario value column and remove other unnecessary columns
          x2 <- x2[, colnames(x2) %in% c(x3, gsub("cf_", "", i))]
          colnames(x2)[colnames(x2) == gsub("cf_", "", i)] <- cf_pars_s[j, "variable"]
                    
        # substitute values with counterfactual value(s)
          # remove column from existing time series
          ts_cf <- ts_cf[, colnames(ts_cf) != cf_pars_s[j, "variable"] ]
          
          # merge with counterfactual dataset 
          ts_cf <- merge(ts_cf, x2, by = x3, all.x = TRUE)
      }
      
    }
  
    # Lastly, name the counterfactual dataset as per the scenario being worked on
    assign(paste("ts_", i, sep=""), ts_cf )
  }    
  rm(ts_cf)

#place4#  
#...........................................................................................
### Preparing counterfactual scenarios: Population denominators
#...........................................................................................

for (i in scenarios[grepl("cf", scenarios)] ) {     

  print(paste("now reconstructing population for scenario: ", gsub("cf_", "", i), sep = "") )
  
  #...................................
  ## Identify demographic data/variables that are subject to counterfactual assumptions in this scenario
  scen <- i
  cf_pop_s <- subset(cf_pars, component == "population" & scenario == gsub("cf_", "", scen) )
  
  #...................................
  ## Modify input data based on counterfactual assumptions

    # Read in population data and parameters again (unless sensitivity analysis is being run)
    if (sens == FALSE) {
      script <- scan("mortality_sae_0_control_code.r", what=character(), sep="\n", quiet=TRUE)
      f_source_part(script, "#place3", "#place4") 
    }
    
    # For each variable that is subject to counterfactual assumptions...
    for (j in 1:nrow(cf_pop_s) ) {
      
      # identify variable and dataset that contains the variable
      x1 <- get(cf_pop_s[j, "object"])
      var <- cf_pop_s[j, "variable"]
      
      # if the quantile of a reference baseline period should be used...
      if (is.na(cf_pop_s[j, "quantile"]) == FALSE) {
          
        # identify start and end time points of the reference period (if no dates are given, assume start/end of analysis period)
        m_start <- ifelse(is.na(cf_pop_s[j, "date_start"]) == TRUE, m_analysis_start, month(cf_pop_s[j, "date_start"]) )
        m_end <- ifelse(is.na(cf_pop_s[j, "date_end"]) == TRUE, m_analysis_end, month(cf_pop_s[j, "date_end"]) )
        y_start <- ifelse(is.na(cf_pop_s[j, "date_start"]) == TRUE, y_analysis_start, year(cf_pop_s[j, "date_start"]) )
        y_end <- ifelse(is.na(cf_pop_s[j, "date_end"]) == TRUE, y_analysis_end, year(cf_pop_s[j, "date_end"]) )
          # convert to corresponding tm values    
          tm_start  <-  t_units[t_units$y == y_start & t_units$m == m_start, "tm" ]
          tm_end  <-  t_units[t_units$y == y_end & t_units$m == m_end, "tm" ]

        # for each geographic unit of reference...
        for (k in unique(strata[, cf_pop_s[j, "geo_ref"]]) ) {
            
          # reference values
            # if the time unit is month, calculate a month-specific counterfactual
            if ( is.na(cf_pop_s[j, "time_ref"]) == FALSE & cf_pop_s[j, "time_ref"] == "month") {
              x2 <- subset(x1, x1[, cf_pop_s[j, "geo_ref"]] == k & tm %in% c(tm_start : tm_end) )[, c("m", var)]
              x2 <- aggregate(x2[, var ], by=list(x2$m), 
                FUN = "quantile", probs = cf_pop_s[j, "quantile"], na.rm=TRUE)
              colnames(x2) <- c("m", var)

              # substitute values with counterfactual value(s)
              x3 <- x1[x1[, cf_pop_s[j, "geo_ref"]] == k, c("tm", "m")]
              x3 <- merge(x3, x2, by = "m", all.x=TRUE)
              x3 <- x3[order(x3[, "tm"]), ]
              x1[x1[, cf_pop_s[j, "geo_ref"]] == k, var] <- x3[, var]
                
            }
                
            # else, calculate a constant counterfactual across time
            if (is.na(cf_pop_s[j, "time_ref"]) == TRUE) {
              x2 <- subset(x1, x1[, cf_pop_s[j, "geo_ref"]] == k & tm %in% c(tm_start : tm_end) )[, var]
              x2 <- quantile(x2, cf_pop_s[j, "quantile"], na.rm=TRUE)
              
              # substitute values with counterfactual value(s)
              x1[x1[, cf_pop_s[j, "geo_ref"]] == k, var] <- x2
            }
          }
        }      
        
      # if an absolute value should be assigned...
      if (is.na(cf_pop_s[j, "abs_value"]) == FALSE) {
        
        # substitute values with counterfactual value(s)
        x1[, var]  <- cf_pop_s[j, "abs_value"]
      }
        
      # if a multiplying factor should be used...
      if (is.na(cf_pop_s[j, "rel_factor"]) == FALSE) {
          
        # substitute values with counterfactual value(s)
        x1[, var]  <- x1[, var] * cf_pop_s[j, "rel_factor"]
      }
    
    # re-assign modified dataset
    assign(cf_pop_s[j, "object"], x1)
    
    }
      
  #...................................
  ## Estimate counterfactual population denominators
    
    # Re-run estimation
    script <- scan(paste("mortality_sae_2_reconstruct_pop_", country, ".r", sep = ""), what=character(), sep="\n", quiet=TRUE)
    f_source_part(script, "#place1", "#place2")
    f_source_part(script, "#place3", "#place6")
    f_source_part(script, "#place7", "#place8")

    # Write output, unless this is sensitivity analysis
    if (sens == FALSE) {write.csv(pop[, x1], paste(country, "_pop_denoms_", scen, ".csv", sep=""), 
      row.names = FALSE) }

    # Merge with corresponding scenario time series
    x2 <- get(paste("ts_", scen, sep = ""))
    x2 <- x2[, ! colnames(x2) %in% c("pop_average", "pop_average_u5", "prop_idp", "dep_rate", "arr_rate")]
    x2 <- merge(x2, pop[, x1[! x1 %in% c("m", "y")]], by = c("admin1", "stratum", "tm"), all.x = TRUE)
    assign(paste("ts_", scen, sep = ""), x2)
  
}


#place5#    
#...........................................................................................
### Categorising predictors as needed across all scenarios
#...........................................................................................
  
  #...................................
  ## Identify fit predictors
    # List fit predictors across both CDR and U5DR fits
    preds <- unique(c(all.vars(formula(fit_cdr)), all.vars(formula(fit_cdr_u5))))
    preds <- preds[! preds %in% c("n_died", part_unit)]

  #...................................
  ## Categorise fit predictors if needed, for each scenario
  
  for (i in scenarios[! grepl("ex", scenarios)] ) {     
    
    # Get scenario data
    x1 <- get(paste("ts_", i, sep = ""))
    
    # For each predictor...
    for (j in preds) {
    
      # if the predictor is categorical and is not yet in the actual scenario time series...
      if (grepl("_cat", j) == TRUE & ! j %in% colnames(x1) ) {
       
        # find index of variable in var_pars
        x2 <- sapply(var_pars[, "variable"], function(x, y) {startsWith(y, x)}, j)
            
        # find continuous version of predictor in the time series
        x3 <- ifelse(is.na(var_pars[x2, "var_name_ts"]) == FALSE, var_pars[x2, "var_name_ts"] , gsub("_cat", "", j) )
            
        # if the variable should be categorised according to cut-offs...
        if (var_pars[x2, "cat_method"] == "cut") {
              
          # work out cut-offs and corresponding labels
          x4 <- as.numeric(unlist(strsplit(var_pars[x2, "cat_cutoffs"], split = ",")))
          x5 <- unlist(strsplit(var_pars[x2, "cat_labels"], split = ", "))
              
          # create new categorical variable
          x1[, j] <- cut(as.numeric(x1[, x3]), breaks = x4, labels = x5, include.lowest = TRUE)
        } 
            
        # if the variable should be categorised according to values...
        if (var_pars[x2, "cat_method"] == "values") {
              
          # work out categories for values
          x4 <- data.frame(as.numeric(unlist(strsplit(var_pars[x2, "cat_values"], split = ","))),
            unlist(strsplit(var_pars[x2, "cat_labels"], split = ", ")) )
              
          # assign corresponding column names
          colnames(x4) <- c(x3, j)
              
          # create new categorical variable
          x1 <- merge(x1, x4, by = colnames(x4)[1], all.x=TRUE)
        }
      }
    }
    
    # re-assign
    assign(paste("ts_", i, sep = ""), x1)
    
  }     
 
     
#...........................................................................................
### Generating bootstrap samples of death toll predictions, for every scenario
#...........................................................................................
    
for (i in scenarios[! grepl("ex", scenarios)]) {     

  print(paste("now generating bootstrap death toll sample for scenario: ", i, sep = "") )
    
  #...................................  
  ## Specify data for this scenario
  
    # Get scenario data
    x1 <- get(paste("ts_", i, sep = ""))

    # Specify person-time at risk for every scenario as 1 in order to predict rates
    x1[, c("ptime", "ptime_u5")] <- 1

    # Calculate how many days are in each month of the time series
    x1[, "days_in_month"] <- days_in_month( as.Date(paste(x1[, "y"], x1[, "m"], 1, sep="-" ) ) )

    
  #...................................
  ## Predict death rates and draw bootstrap death toll samples or all ages and children under 5y
  for (j in c("", "_u5")) {

    # Predict death rates and standard errors for the actual and counterfactual scenarios (in the model link metric, e.g. log for Poisson)
      
      # control statement
      print(paste("  now predicting cdr", j, " and standard errors", sep = ""))
    
      # retain any predictor counterfactuals that apply to the age group; otherwise, use the actual scenario predictor values
      if (i != "ac") {
        x2 <- unique(cf_pars[cf_pars[, paste("applies_cdr", j, sep = "")] == FALSE, "variable"])
        if (length(x2) > 0) {
          ind <- grep(paste(x2, collapse="|", sep=""), colnames(x1), value = TRUE)
          x1 <- merge(x1[, ! colnames(x1) %in% ind], ts_ac[, c("stratum", "tm", ind)], by=c("stratum", "tm"))
        }
      }  
      
      # predict log death rate and its SE
        # get fit object
        x2 <- get(paste("fit_cdr", j, sep = ""))
        
        # get terms apart from the response
        x3 <- all.vars(formula(x2))[all.vars(formula(x2)) != "n_died"]
        
        # if the model is fixed-effects only...
        if (! part_unit %in% x3 ) { 
          x1[, paste("log_cdr", j, "_", i, sep="")] <- f_predict(x2, get(paste("vcov_cl_cdr", j, sep = "")), x1, FALSE)
          x1[, paste("log_cdr", j, "_se_", i, sep="")] <- f_predict(x2, get(paste("vcov_cl_cdr", j, sep = "")), x1, TRUE)
        }      
      
        # if the model has random effects...
        if (part_unit %in% x3 ) { 
          x1[, "wt"] <- 1
          x1[, paste("log_cdr", j, "_", i, sep="")] <- NA
          x1[, paste("log_cdr", j, "_se_", i, sep="")] <- NA
          x4 <- complete.cases(x1[, x3])
          x1[x4, paste("log_cdr", j, "_", i, sep="")] <- predict(x2, newdata = x1[x4, ], allow.new.levels = TRUE)
          x1[x4, paste("log_cdr", j, "_se_", i, sep="")] <- predict(x2, newdata = x1[x4, ], 
            allow.new.levels = TRUE, se.fit = TRUE)[2]
        }      
       
    # Draw bootstrap sample of random values of the death toll, for each scenario

      # sort time series
      x1 <- x1[order(x1[, "admin1"], x1[, "stratum"], x1[, "tm"]), ]
      
      # generate bootstrap sample
      x2 <- suppressWarnings(exp(replicate(bootstrap_runs, {rnorm(nrow(x1), x1[, paste("log_cdr", j, "_", i, sep="")], 
        x1[, paste("log_cdr", j, "_se_", i, sep="")]  )} ) )) * x1[, "days_in_month"] *
        x1[, paste("pop_average", j, sep = "")]
      x2[is.nan(x2)] <- NA
        
      # sort sample ascendingly (for CI calculation when aggregating)
      x2 <- t(apply(x2, 1, function(x) {if (all(is.na(x))) return(x) else return(sort(x)) }))
        
      # assign
      assign(paste("toll", j, "_", i, sep=""), x2)
  }

  # Re-assign
  assign(paste("ts", "_", i, sep=""), x1)
          
  # Clean up  
  rm(x1, x2)
}
 

  #...................................  
  ## Compute excess death tolls, for each counterfactual scenario
  for (i in scenarios[grepl("cf", scenarios)] ) {
    
    # For all ages and children under 5y
    for (j in c("", "_u5")) {
      
      # calculate excess death toll
      x1 <- get(paste("toll", j, "_ac", sep = "")) - get(paste("toll", j, "_", i, sep = ""))

      # assign object
      assign(paste("toll", j, "_", gsub("cf", "ex", i), sep=""), x1)
    }
  }  
  
  
  #...................................  
  ## Prepare bootstrap samples for analysis
  for (i in scenarios ) {
    
    # For all ages and children under 5y
    for (j in c("", "_u5")) {
      
      # get scenario bootstrap sample
      x1 <- get(paste("toll", j, "_", i, sep = ""))
      
      # get fit object
      x2 <- get(paste("fit_cdr", j, sep = ""))
      
      # get model terms
      x3 <- all.vars(formula(x2))[all.vars(formula(x2)) != "n_died"]
      
      # add geographic, time units and population to each death toll sample
        # use actual population denominators for the actual and excess scenarios, and the counterfactual ones otherwise
      if (grepl("ac", i) == TRUE | grepl("ex", i) == TRUE) {
       x1 <- cbind(x1, ts_ac[, c("admin1", "stratum", "y", "m", "tm", "days_in_month", paste("pop_average", j, sep = ""))] )
      }
      if (grepl("cf", i) == TRUE) {
        x1 <- cbind(x1, get(paste("ts_", i, sep=""))[, c("admin1", "stratum", "y", "m", "tm", "days_in_month", paste("pop_average", j, sep = ""))] )
      }

      # restrict to span of availability of predictors, given the maximum lag introduced into the model
      x4 <- as.integer(regexpr(paste(paste("lag", c(1:10), sep = ""), collapse = "|"), x3))
      x4 <- x4
      x5 <- c()
      for (k in 1:length(x4) ) {if (x4[k] > 0) {x5 <- c(x5, as.integer(substr(x3[k], x4[k]+3, x4[k]+3))) } }
      if (length(x5) > 0) { x1 <- subset(x1, tm >= (tm_analysis_start + max(x5) ) ) }
      
      # assign
      assign(paste("toll", j, "_", i, sep = ""), x1)
      
    }
  }  
    
  
#place6#  
#...........................................................................................
### Computing point estimates and confidence intervals for different aggregations; tables and figures
#...........................................................................................
  
  #...................................  
  ## Aggregate by stratum-month (i.e. no aggregation)
    # Output
    out <- f_est(scenarios, bootstrap_runs, c("stratum", "tm"), tm_analysis_start, tm_analysis_end)
    
    # Write output
    write.csv(out, paste(country, "_out_est_by_stratum_month.csv", sep = ""), row.names = FALSE, na = "")
    
  #...................................  
  ## Aggregate by stratum     
    # Output
    out <- f_est(scenarios, bootstrap_runs, "stratum", tm_analysis_start, tm_analysis_end)
    
    # Write output
    write.csv(out, paste(country, "_out_est_by_stratum.csv", sep = ""), row.names = FALSE, na = "")


  #...................................  
  ## Aggregate by admin1     
    # Output
    out <- f_est(scenarios, bootstrap_runs, "admin1", tm_analysis_start, tm_analysis_end)
      # format death rates
      out[, grepl("cdr", colnames(out))] <- format(round(out[, grepl("cdr", colnames(out))], digits=2), nsmall=2, scientific=FALSE)

    # Write output
    write.csv(out, paste(country, "_out_est_by_admin1.csv", sep = ""), row.names = FALSE, na = "")
            
    # Tables and graphs
      # total and excess death tolls
      tab <- data.frame(out[, "admin1"], 
        paste(out[, "toll_ac_est"], " (", out[, "toll_ac_lci"], " to ", out[, "toll_ac_uci"], ")", sep=""),
        paste(out[, "toll_ex_likely_est"], " (", out[, "toll_ex_likely_lci"], " to ", out[, "toll_ex_likely_uci"], ")", sep=""),
        paste(out[, "toll_u5_ac_est"], " (", out[, "toll_u5_ac_lci"], " to ", out[, "toll_u5_ac_uci"], ")", sep=""),
        paste(out[, "toll_u5_ex_likely_est"], " (", out[, "toll_u5_ex_likely_lci"], " to ", out[, "toll_u5_ex_likely_uci"], ")", sep="")      
        )
      colnames(tab) <- c(admin1_name, 
        "total deaths (95%CI)", "excess deaths (95%CI)",
        "total deaths, children under 5y (95%CI)", "excess deaths, children under 5y (95%CI)"
        )
        # clean up white spaces
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("\\s+"," ", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("[:(:]\\s", "(", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(trimws(x, "both"))})

      write.csv(tab, paste(country, "_out_est_by_admin1_tab1.csv", sep = ""), row.names = FALSE, na = "")
    
      # total and excess death rates
      tab <- data.frame(out[, "admin1"], 
        paste(out[, "cdr_ac_est"], " (", out[, "cdr_ac_lci"], " to ", out[, "cdr_ac_uci"], ")", sep=""),
        paste(out[, "cdr_ex_likely_est"], " (", out[, "cdr_ex_likely_lci"], " to ", out[, "cdr_ex_likely_uci"], ")", sep=""),
        paste(out[, "toll_ex_likely_est"], " (", out[, "toll_ex_likely_lci"], " to ", out[, "toll_ex_likely_uci"], ")", sep=""),
        paste(out[, "cdr_u5_ac_est"], " (", out[, "cdr_u5_ac_lci"], " to ", out[, "cdr_u5_ac_uci"], ")", sep=""),
        paste(out[, "cdr_u5_ex_likely_est"], " (", out[, "cdr_u5_ex_likely_lci"], " to ", out[, "cdr_u5_ex_likely_uci"], ")", sep=""),
        paste(out[, "toll_u5_ex_likely_est"], " (", out[, "toll_u5_ex_likely_lci"], " to ", out[, "toll_u5_ex_likely_uci"], ")", sep="")      
        )
      colnames(tab) <- c(admin1_name, 
        "crude death rate (95%CI)", "excess death rate (95% CI)", "excess deaths (95%CI)",
        "under 5y death rate (95%CI)", "excess death rate, children under 5y (95%CI)", "excess deaths, children under 5y (95%CI)"
        )
      
        # clean up white spaces
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("\\s+"," ", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("[:(:]\\s", "(", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(trimws(x, "both"))})
      
      write.csv(tab, paste(country, "_out_est_by_admin1_tab2.csv", sep = ""), row.names = FALSE, na = "")  
      
  #...................................  
  ## Aggregate by month     
    # Output
    out <- f_est(scenarios, bootstrap_runs, "tm", tm_analysis_start, tm_analysis_end)
    
    # Write output
    write.csv(out, paste(country, "_out_est_by_month.csv", sep = ""), row.names = FALSE, na = "")
    
    # Tables and graphs
      # restrict counterfactual to crisis period
      out[! out$tm %in% c(tm_excess_start : tm_excess_end), grepl("cf_", colnames(out))] <- NA

      # add dates
      out <- merge(out, t_units, by = "tm", all.x = TRUE, sort = TRUE)
      out[, "date"] <- dmy(paste("1", out[, "m"], out[, "y"], sep="/"))
    
      # evolution of actual and counterfactual death rate - all ages
      plot_cdr <- ggplot(out, aes(x = date) ) +
        geom_point(aes(y = cdr_ac_est), colour = "indianred3", size = 2, alpha = 0.5) +
        geom_line(aes(y = cdr_ac_est), colour = "indianred3", size = 1, alpha = 0.5) +
        geom_ribbon(aes(x = date, ymin = as.numeric(out[, "cdr_ac_lci"]), ymax = as.numeric(out[, "cdr_ac_uci"] )), 
          fill = "indianred3", alpha = 0.3) +
        geom_line(aes(y = cdr_cf_likely_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "longdash") +
        geom_line(aes(y = cdr_cf_best_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "dotted") +
        geom_line(aes(y = cdr_cf_worst_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "dotted") +
        theme_bw() + 
        theme(plot.margin = unit(c(1, 0, 0, 0.5), "cm") ) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand=c(0,0)) +
        scale_y_continuous("crude death rate (per 10,000 person-days)", limits=c(0, max(out$cdr_ac_uci) + 0.1)) +
        theme(axis.text = element_text(size = 11, colour = "grey20"), 
          axis.title = element_text(size = 11, colour = "grey20"), axis.text.x = element_text(angle = 30, hjust = 1))

      plot_cdr
      ggsave(paste(country, "_", "cdr", "_trend.png", sep=""), height = 15, width = 22, units = "cm", dpi = "print")

      # evolution of actual and counterfactual death rate - children under 5y
      plot_cdr_u5 <- ggplot(out, aes(x = date) ) +
        geom_point(aes(y = cdr_u5_ac_est), colour = "indianred3", size = 2, alpha = 0.5) +
        geom_line(aes(y = cdr_u5_ac_est), colour = "indianred3", size = 1, alpha = 0.5) +
        geom_ribbon(aes(x = date, ymin = as.numeric(out[, "cdr_u5_ac_lci"]), ymax = as.numeric(out[, "cdr_u5_ac_uci"] )), 
          fill = "indianred3", alpha = 0.3) +
        geom_line(aes(y = cdr_u5_cf_likely_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "longdash") +
        geom_line(aes(y = cdr_u5_cf_best_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "dotted") +
        geom_line(aes(y = cdr_u5_cf_worst_est), colour = palette_cb[4], alpha = 0.8, 
          size = 1, linetype = "dotted") +
        theme_bw() + 
        theme(plot.margin = unit(c(1, 0, 0, 0.5), "cm") ) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
        scale_y_continuous("under 5y death rate (per 10,000 child-days)", limits = c(0, max(out$cdr_u5_ac_uci) + 0.1)) +
        theme(axis.text = element_text(size = 11, colour = "grey20"), 
          axis.title = element_text(size = 11, colour = "grey20"), axis.text.x = element_text(angle = 30, hjust = 1))

      plot_cdr_u5
      ggsave(paste(country, "_", "cdr_u5", "_trend.png", sep=""), height = 15, width = 22, units = "cm", dpi = "print")
  
      # combine plots and save
      plot <- ggarrange(plot_cdr + theme(axis.text.x = element_blank() ), plot_cdr_u5, nrow = 2, labels = c("all ages", "children under 5y"),
        font.label = list(size = 10.5, color = "grey20"), common.legend = TRUE, align = "v", vjust = 1, hjust = 0)
      print(plot)
      ggsave(paste(country, "_", "cdr_cdr_u5_trends_pop_wide.png", sep=""), height = 20, width = 30, 
        units = "cm", dpi = "print")
      ggsave(paste(country, "_", "cdr_cdr_u5_trends_pop_long.png", sep=""), height = 20, width = 15, 
        units = "cm", dpi = "print")
      
  #...................................  
  ## Aggregate by year     
    # Output
    out <- f_est(scenarios, bootstrap_runs, "y", tm_analysis_start, tm_analysis_end)
    
    # Write output
    write.csv(out, paste(country, "_out_est_by_year.csv", sep = ""), row.names = FALSE, na = "")
    
    # Tables and graphs
      # total and excess death tolls
      tab <- data.frame(out[, "y"], 
        paste(out[, "toll_ac_est"], " (", out[, "toll_ac_lci"], " to ", out[, "toll_ac_uci"], ")", sep=""),
        paste(out[, "toll_ex_likely_est"], " (", out[, "toll_ex_likely_lci"], " to ", out[, "toll_ex_likely_uci"], ")", sep=""),
        paste(out[, "toll_u5_ac_est"], " (", out[, "toll_u5_ac_lci"], " to ", out[, "toll_u5_ac_uci"], ")", sep=""),
        paste(out[, "toll_u5_ex_likely_est"], " (", out[, "toll_u5_ex_likely_lci"], " to ", out[, "toll_u5_ex_likely_uci"], ")", sep="")      
        )
      colnames(tab) <- c("year", 
        "total deaths (95%CI)", "excess deaths (95%CI)",
        "total deaths, children under 5y (95%CI)", "excess deaths, children under 5y (95%CI)"
        )
        
        # clean up white spaces
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("\\s+"," ", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("[:(:]\\s", "(", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(trimws(x, "both"))})
      
      write.csv(tab, paste(country, "_out_est_by_year_tab1.csv", sep = ""), row.names = FALSE, na = "")

  #...................................  
  ## Aggregate by admin1 and year     
    # Output
    out <- f_est(scenarios, bootstrap_runs, c("admin1", "y"), tm_analysis_start, tm_analysis_end)
    
    # Write output
    write.csv(out, paste(country, "_out_est_by_admin1_year.csv", sep=""), row.names=FALSE, na = "")
    
#place7#
  #...................................  
  ## Aggregate overall
    # Output
    out <- c()
    
    # Aggregate each scenario and compute point estimates and 95%CIs
    for (i in scenarios) {
      
      # for all ages and children under 5y
      for (j in c("", "_u5")) {      

        # access bootstrap sample and restrict timespan as desired
        x1 <- get(paste("toll", j, "_", i, sep=""))
        x1 <- subset(x1, tm %in% c(tm_analysis_start : tm_analysis_end) )

        # death tolls
          # aggregate bootstrap sample of death tolls
          x2 <- colSums(x1[, as.character(seq(1:bootstrap_runs))] )

          # calculate point estimate and 95%CIs of death tolls
          out_sub <- quantile(x2, c(0.5, 0.025, 0.975))
          
        # death rates
          # generate person-time
          x1[, paste("ptime", j, sep = "")] <- x1[, paste("pop_average", j, sep = "")] * x1[, "days_in_month"]
          
          # aggregate person-time
          x2 <- sum(x1[, paste("ptime", j, sep = "")] )
          
          # calculate point estimate and 95%CIs of death rates (per 10,000 person-days)
          out_sub <- c(out_sub, out_sub * 10000 / x2)
          
        # name values
        names(out_sub) <- c(paste("toll", j, "_", i, c("_est", "_lci", "_uci"), sep=""), 
          c(paste("cdr", j, "_", i, c("_est", "_lci", "_uci"), sep="")))
        
        # merge output
        out <- c(out, out_sub)
      }     
    }
    
    # Round estimates
      # death tolls to nearest hundred, formatted with thousand commas
      out[grepl("toll", names(out))] <- format(round(out[grepl("toll", names(out))] , digits=-2) , big.mark="," , scientific=FALSE)

      # death rates to 2 sig figs
      out[grepl("cdr", names(out))] <- format( out[grepl("cdr", names(out))] , digits=2, nsmall=2 , scientific=FALSE)
#place8#      
    # Write output
    write.csv(out, paste(country, "_out_est_toll_overall.csv", sep = ""), row.names = TRUE, col.names = FALSE, na = "")

    # Tables and graphs
      # total and excess death tolls
      tab <- data.frame(
        paste(out["toll_ac_est"], " (", out["toll_ac_lci"], " to ", out["toll_ac_uci"], ")", sep=""),
        paste(out["toll_ex_likely_est"], " (", out["toll_ex_likely_lci"], " to ", out["toll_ex_likely_uci"], ")", sep=""),
        paste(out["toll_u5_ac_est"], " (", out["toll_u5_ac_lci"], " to ", out["toll_u5_ac_uci"], ")", sep=""),
        paste(out["toll_u5_ex_likely_est"], " (", out["toll_u5_ex_likely_lci"], " to ", out["toll_u5_ex_likely_uci"], ")", sep="")      
      )
      colnames(tab) <- c(
        "total deaths (95%CI)", "excess deaths (95%CI)",
        "total deaths, children under 5y (95%CI)", "excess deaths, children under 5y (95%CI)"
      )
        
        # clean up white spaces
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("\\s+"," ", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(gsub("[:(:]\\s", "(", x))})
        tab <- apply(tab, c(1, 2), function(x) {return(trimws(x, "both"))})
        
      write.csv(tab, paste(country, "_out_est_overall_tab1.csv", sep=""), row.names = FALSE, na = "")
    
#...........................................................................................
### ENDS        

      g<-rbind(d, c)    
g <- as.data.frame(g)      


for (i in scenarios) {g[, paste("toll_", i, sep = "")] <- paste(
  g[, paste("toll_", i, "_est", sep = "")],
  " (",
  g[, paste("toll_", i, "_lci", sep = "")],  
  " to ",
  g[, paste("toll_", i, "_uci", sep = "")],  
  ")",
  sep = ""
  )
}      
 
g<-g[, -grep("_est", colnames(g))]
g<-g[, -grep("_lci", colnames(g))]
g<-g[, -grep("_uci", colnames(g))]
write.csv(g, "bah2.csv", row.names = FALSE)
