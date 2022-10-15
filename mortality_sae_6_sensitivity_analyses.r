#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## ------ R CODE TO IMPLEMENT SENSITIVITY ANALYSES OF EXCESS MORTALITY ESTIMATES ------- ##
#..........................................................................................

                                        # Written by Francesco Checchi, LSHTM (May 2021)
                                        # francesco.checchi@lshtm.ac.uk 


#...........................................................................................
### Reading in required files
#...........................................................................................

  #...................................
  ## Source previous R scripts     
    # Read functions
    source("mortality_sae_0_functions.r")
  
    # Source control script
    script_0 <- scan("mortality_sae_0_control_code.r", what=character(), sep="\n", quiet=TRUE)
      # Read in all files and parameters...    
      f_source_part(script_0, "#place1#", "#place6#")
  
    # Source all scripts
    script_0 <- scan("mortality_sae_0_control_code.r", what=character(), sep="\n", quiet=TRUE)
    script_1 <- scan("mortality_sae_1_manage_surveys.r", what=character(), sep="\n", quiet=TRUE)
    script_2 <- scan(paste("mortality_sae_2_reconstruct_pop_", country, ".r", sep=""), what=character(), sep="\n", quiet=TRUE)  
    script_3 <- scan("mortality_sae_3_manage_predictors.r", what=character(), sep="\n", quiet=TRUE)
    script_4 <- scan("mortality_sae_4_predictive_model.r", what=character(), sep="\n", quiet=TRUE)
    script_5 <- scan("mortality_sae_5_estimate_mortality.r", what=character(), sep="\n", quiet=TRUE) 

  #...................................
  ## Define scenarios for sensitivity analysis
  scenarios <- c("ac", "cf_likely", "ex_likely")
  sens <- TRUE # denotes we are in sensitivity analysis (to avoid certain steps in script 5)
      
  #...................................
  ## Read datasets arising from previous scripts
  hh_y_obs_base <- read.csv(paste(country, "_hh_obs.csv", sep=""), sep="," )
  x_obs_base <- read.csv(paste(country, "_x_obs.csv", sep=""), sep="," )
  surveys <- read.csv(paste(country, "_survey_metadata_reanalysed.csv", sep=""), sep="," )
  survey_cover <- read.csv(paste(country, "_survey_stratum_month_cover.csv", sep=""), sep="," )
  ts_ac_base <- read.csv(paste(country, "_ts.csv", sep=""), sep=",")
  pop_base <- read.csv(paste(country, "_pop_denoms.csv", sep=""), sep=",")
  for (i in grep("cf", scenarios, value = TRUE) ) {
    assign(paste("pop_", i, sep = ""), read.csv(paste(country, "_pop_denoms_", i, ".csv", sep=""), sep=",") )
  }
  
  #...................................
  ## Read model fits
  fit_cdr_base <- readRDS(paste(country, "_cdr_fit_best.rds", sep="")) 
  fit_cdr_u5_base <- readRDS(paste(country, "_cdr_u5_fit_best.rds", sep=""))
   
  
#..........................................................................................
### Sensitivity analysis: displacement and population denominators
#..........................................................................................

if (unique(sens_pars[sens_pars[, "sens_analysis"] == "bias_population_data", "implement"]) == "Y") {
  
  #...................................    
  ## Source sensitivity parameters
  sens_pop <- subset(sens_pars, sens_analysis == "bias_population_data")

  #...................................    
  ## Set up output as all possible combinations of sensitivity values
  out_cols <- c("toll_ac_est", "toll_ex_likely_est", "toll_u5_ac_est", "toll_u5_ex_likely_est")
  x1 <- list()
  for (i in unique(sens_pop[, "group"]) ) {
    x1[[i]] <- seq(
      unique(sens_pop[sens_pop[, "group"] == i, "range_min"]), 
      unique(sens_pop[sens_pop[, "group"] == i, "range_max"]), 
      by = unique(sens_pop[sens_pop[, "group"] == i, "range_step"])
      )
  }
  out_sens_pop <- expand.grid(x1)
  names(out_sens_pop) <- unique(sens_pop[, "group_name"])
  out_sens_pop[, out_cols] <- NA  

  #...................................  
  ## Prepare base input data for models
  
    # Merge survey-stratum-month data coverage with population denominators and calculate survey-stratum mean population
    x1 <- merge(survey_cover, pop_base[, c("stratum", "tm", "pop_average")], by = c("stratum", "tm"), all.x = TRUE)
    x1[, "sum_wt"] <- x1[, "month_coverage"] * x1[, "pop_average"]
    x1 <- aggregate(x1[, c("month_coverage", "sum_wt")], by = x1[, c("survey_id", "stratum")], FUN = sum)
    x1[, "pop_recall_base"] <- x1[, "sum_wt"] / x1[, "month_coverage"]
    x_obs_base <- merge(x_obs_base, x1[, c("survey_id", "stratum", "pop_recall_base")], 
      by = c("survey_id", "stratum"), all.x = TRUE)
  
  #...................................  
  ## For each combination of sensitivity parameters...
  for (ii in 1:nrow(out_sens_pop)) {
    
    # Print control statement
    print(paste("now working on sensitivity analysis for bias in population data; combination ", 
      ii, " of ", nrow(out_sens_pop), sep = ""))
    
    # Source data that are subject to sensitivity analysis, and alter them as desired
    f_source_part(script_0, "#place3#", "#place4#")
    for (jj in unique(sens_pop[, "group_name"]) ) {
      x1 <- sens_pop[sens_pop[, "group_name"] == jj, c("object", "variable")]
      for (kk in 1:nrow(x1)) {
        x2 <- get(x1[kk, "object"])
        x2[, x1[kk, "variable"]] <- round(x2[, x1[kk, "variable"]] * out_sens_pop[ii, jj], digits = 0)
        assign(x1[kk, "object"], x2)
      }
    }
    
    # Execute population estimation script
    f_source_part(script_2, "#place1#", "#place2#")
    f_source_part(script_2, "#place3#", "#place6#")
    f_source_part(script_2, "#place7#", "#place8#")

    # Update predictor data accordingly
      # using new population estimates, compute mean population and IDP proportion 
        # over recall period of each survey-stratum
      x1 <- merge(survey_cover, pop[, c("stratum", "tm", "pop_average", "prop_idp")], 
        by = c("stratum", "tm"), all.x = TRUE)
      x1[, "sum_wt_pop"] <- x1[, "month_coverage"] * x1[, "pop_average"]
      x1[, "sum_wt_idp"] <- x1[, "month_coverage"] * x1[, "prop_idp"]
      x1 <- aggregate(x1[, c("month_coverage", "sum_wt_pop", "sum_wt_idp")], 
        by = x1[, c("survey_id", "stratum")], FUN = sum)
      x1[, "pop_recall_sens"] <- x1[, "sum_wt_pop"] / x1[, "month_coverage"]
      x1[, "prop_idp"] <- x1[, "sum_wt_idp"] / x1[, "month_coverage"]
      
      # merge with predictor observations, replacing the proportion of IDPs altogether
      x_obs <- merge(x_obs_base[, colnames(x_obs_base) != "prop_idp"], 
        x1[, c("survey_id", "stratum", "pop_recall_sens", "prop_idp")], by = c("survey_id", "stratum"), all.x = TRUE)      
    
      # update all the predictors that are per capita rates based on the ratio of base to sensitivity population
      x_obs[, grep("rate", colnames(x_obs))] <- x_obs[, grep("rate", colnames(x_obs))] * x_obs[, "pop_recall_base"] /
        x_obs[, "pop_recall_sens"]
    
    # Update model fits using new predictor data, for both CDR and U5DR
    for (jj in c("", "_u5"))  {
      # CDR or U5DR
      y_hat <- paste("cdr", jj, sep = "")
      
      # merge with dependent data and categorise so as to be ready for model fitting
      f_source_part(script_4, "#place2#", "#place4#")
      f_source_part(script_4, "#place5#", "#place6#")
      
      # update model fit and recalculate robust SEs for fixed-effect models
      if (y_hat == "cdr") {
        fit_cdr <- update(fit_cdr_base, family = family(fit_cdr_base), data = hh_obs, weights = wt, offset = log(ptime))
      }
      if (y_hat == "cdr_u5") {
        fit_cdr_u5 <- update(fit_cdr_u5_base, family = family(fit_cdr_u5_base), data = hh_obs, weights = wt, offset = log(ptime))
      }
      if (! part_unit %in% all.vars(formula(fit_cdr))) {
        vcov_cl_cdr <- cluster.vcov(fit_cdr, fit_cdr$data[, cluster_vars])
        vcov_cl_cdr_u5 <- cluster.vcov(fit_cdr_u5, fit_cdr_u5$data[, cluster_vars])  
      }
    }

    # Update time series data in a similar way
    ts_ac <- ts_ac_base[, colnames(ts_ac_base) != "prop_idp"] 
    colnames(ts_ac)[colnames(ts_ac) %in% c("pop_average", "pop_average_u5")] <- c("pop_average_base", "pop_average_u5_base")
    ts_ac <- merge(ts_ac, pop[, c("stratum", "tm", "pop_average", "pop_average_u5", "prop_idp")])
    ts_ac[, grep("rate", colnames(ts_ac))] <- ts_ac[, grep("rate", colnames(ts_ac))] * ts_ac[, "pop_average_base"] /
      ts_ac[, "pop_average"]
    ts_ac <- ts_ac[, ! colnames(ts_ac) %in% c("pop_average_base", "pop_average_u5_base")]

    # Estimate mortality (only for likely scenario and for entire population-period)
    f_source_part(script_5, "#place1#", "#place2#")    
    f_source_part(script_5, "#place3#", "#place6#")
    f_source_part(script_5, "#place7#", "#place8#")

    # Add result to output
    out_sens_pop[ii, out_cols] <- out[out_cols]
  }  

  #...................................  
  ## Write and graph output
    
    # Write output
    write.csv(out_sens_pop, paste(country, "_out_sens_population.csv", sep=""), row.names=FALSE)
    
    # Graph output (point estimates only)
      # prepare data  
      x1 <- out_sens_pop
      labs <- colnames(x1)[! colnames(x1) %in% out_cols]
      x1 <- data.frame(x1)
      x1 <- x1[complete.cases(x1), ]
      colnames(x1)[! colnames(x1) %in% out_cols] <- paste("var", 1:length(labs), sep = "")
      x1[, out_cols] <- apply(x1[, out_cols], c(1,2), function(x) {as.numeric(trimws(gsub(",", "", x)))})
      x1 <- melt(x1, id.vars = grep("var", colnames(x1)))
      x1[, grep("var", colnames(x1))] <- lapply(x1[, grep("var", colnames(x1))], factor)
      if ("var2" %in% colnames(x1)) {x1[, "group_var"] <- paste(x1[, "var2"], x1[, "variable"], sep = "_")}
      labs <- gsub("[.]", " ", labs)
      
      # plot actual and excess death toll, by age
      if (length(labs == 1)) {
        for (jj in c("", "_u5") ) {
          plot <- ggplot(subset(x1, variable %in% 
              c(paste("toll", jj, "_ac_est", sep = ""), paste("toll", jj, "_ex_likely_est", sep = "") ) ), 
            aes(y = value, x = var1, colour = variable)) +
            geom_point(size = 3, alpha = 0.50) +
            geom_line(size = 1.5, alpha = 0.50) +
            scale_colour_manual(values = c(palette_cb[7], palette_cb[8]) , guide = FALSE) +
            scale_y_continuous("estimated death toll", labels = scales::comma) +
            scale_x_discrete(labs[1]) +
            theme_bw() +
            theme(axis.title = element_text(size = 10, colour = "grey20"))
          assign(paste("plot_cdr", jj, sep = ""), plot)
        }
      }
      
      if (length(labs == 2)) {
        for (jj in c("", "_u5") ) {
          x2 <- subset(x1, variable %in% 
              c(paste("toll", jj, "_ac_est", sep = ""), paste("toll", jj, "_ex_likely_est", sep = "") ) )
          plot <- ggplot(x2, 
            aes(y = value, x = var1, group = group_var, colour = variable, linetype = var2)) +
            geom_point(size = 3, alpha = 0.50) +
            geom_line(size = 1.5, alpha = 0.50) +
            scale_colour_manual(values = c(palette_cb[7], palette_cb[8]) , guide = FALSE) +
            scale_y_continuous("estimated death toll", labels = scales::comma) +
            scale_x_discrete(labs[1]) +
            labs(linetype = labs[2]) +
            theme_bw() +
            theme(plot.margin = margin(c(1, 0, 0, 0), unit = "cm")) +        
            theme(axis.title = element_text(size = 10, colour = "grey20")) +
            theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "none") +
            annotate("text", x = levels(x2[, "var1"])[1], y = subset(x2, var1 == levels(x2[, "var1"])[1])[, "value"],
              label = subset(x2, var1 == levels(x2[, "var1"])[1])[, "var2"], size = 4,
              hjust = 2, colour = c(rep(palette_cb[7], length(subset(x2, var1 == levels(x2[, "var1"])[1])[, "var2"]) / 2),
                rep(palette_cb[8], length(subset(x2, var1 == levels(x2[, "var1"])[1])[, "var2"]) / 2)) )
          assign(paste("plot_cdr", jj, sep = ""), plot)
        }
      }

      # arrange both plots side by side and save
      plot <- ggarrange(plot_cdr+ theme(axis.title.x = element_blank()), plot_cdr_u5, nrow = 2, 
        labels = c("all ages", "children under 5y"),
        font.label = list(size = 10.5, color = "grey20"), align = "v", vjust = 1.5, hjust = 0)
      print(plot)
      ggsave(paste(country, "_", "sensitivity_pop.png", sep=""), height = 15, width = 22, 
        units = "cm", dpi = "print")

}            
 
         
#..........................................................................................
### Sensitivity analysis: Under-estimation of under 5y death rate
#..........................................................................................

if (unique(sens_pars[sens_pars[, "sens_analysis"] == "bias_u5dr_data", "implement"]) == "Y") {
      
  #...................................    
  ## Source sensitivity parameters
  sens_u5dr <- subset(sens_pars, sens_analysis == "bias_u5dr_data")

  #...................................    
  ## Set up output as all possible sensitivity values, for each iteration
  out_cols <- c("toll_ac_est", "toll_ex_likely_est", "toll_u5_ac_est", "toll_u5_ex_likely_est")
  x1 <- list(seq(sens_u5dr[sens_u5dr[, "parameter"] == "prop_u", "range_min"], 
    sens_u5dr[sens_u5dr[, "parameter"] == "prop_u", "range_max"],
    by = sens_u5dr[sens_u5dr[, "parameter"] == "prop_u", "range_step"]),
    1:sens_u5dr[sens_u5dr[, "parameter"] == "n_runs", "value"]
    )
  out_sens_u5dr <- expand.grid(x1)
  names(out_sens_u5dr) <- c("prop_u", "run")
  out_sens_u5dr[, out_cols] <- NA  
  out_sens_u5dr <- out_sens_u5dr[order(out_sens_u5dr[, "prop_u"]), ]
      
  #...................................    
  ## Set up dataset for sensitivity analysis
      
    # Aggregate number of deaths by survey
    hh_y_obs_base[, "n_hh"] <- 1
    svy_y_obs <- aggregate(hh_y_obs_base[, c("n_died_u5", "n_hh")], 
      by = list("survey_id" = hh_y_obs_base$survey_id), FUN = sum, na.rm = TRUE)
    
    # Merge in recall period of surveys
    hh_y_obs_base <- merge(hh_y_obs_base, surveys[, c("survey_id", "recall_days")], by = "survey_id")
    

  #...................................  
  ## For each value of under 5y mortality under-reporting and individual run...
  for (ii in 1:nrow(out_sens_u5dr)) {

    # Print control statement
    print(paste("now working on under-reporting value and run", ii, " of ", nrow(out_sens_u5dr), sep = ""))

    # Generate augmented dataset of household mortality observations
      # replicate dataset
      hh_y_obs_aug <- hh_y_obs_base
      
      # for each survey, figure out...
        # how many undetected deaths U5
        svy_y_obs[, "n_died_u5_u"] <- round(svy_y_obs[, "n_died_u5"] * 
          (out_sens_u5dr[ii, "prop_u"]/(1 - out_sens_u5dr[ii, "prop_u"])), 0)      
        
        # how many undetected deaths U5 per household on average
        svy_y_obs[, "n_died_u5_u_mean"] <- svy_y_obs[, "n_died_u5_u"] / svy_y_obs[, "n_hh"]     
      
      # merge the above with the augmented dataset  
      hh_y_obs_aug <- merge(hh_y_obs_aug, svy_y_obs[, c("survey_id", "n_died_u5_u_mean")], by = "survey_id")  
        
      # for each household, generate a random number of undetected deaths, and thus total number of deaths (U5 and all age)
      hh_y_obs_aug[, "n_died_u5_u"] <- rpois(nrow(hh_y_obs_aug), hh_y_obs_aug[, "n_died_u5_u_mean"])
      hh_y_obs_aug[, "n_died_u5"] <- hh_y_obs_aug[, "n_died_u5"] + hh_y_obs_aug[, "n_died_u5_u"]
      hh_y_obs_aug[, "n_died"] <- hh_y_obs_aug[, "n_died"] + hh_y_obs_aug[, "n_died_u5_u"]
      
      # for each household, generate a corresponding random person-time (U5 and all age) taking into account the undetected deaths
      hh_y_obs_aug[, "ptime_u5_u"] <- hh_y_obs_aug[, "n_died_u5_u"] * 
        hh_y_obs_aug[, "recall_days"] * as.numeric(sens_u5dr[sens_u5dr[, "parameter"] == "recall_prop", "value"])
      hh_y_obs_aug[, "ptime_u5"] <- round(hh_y_obs_aug[, "ptime_u5"] + hh_y_obs_aug[, "ptime_u5_u"], 0)
      hh_y_obs_aug[, "ptime"] <- round(hh_y_obs_aug[, "ptime"] + hh_y_obs_aug[, "ptime_u5_u"], 0)
      
    # Update model fits using new independent data, for both CDR and U5DR
    for (jj in c("", "_u5"))  {
      # CDR or U5DR
      y_hat <- paste("cdr", jj, sep = "")
      hh_y_obs <- hh_y_obs_aug

      # merge with dependent data and categorise so as to be ready for model fitting
      f_source_part(script_4, "#place1#", "#place2#")
      f_source_part(script_4, "#place3#", "#place4#")
      f_source_part(script_4, "#place5#", "#place6#")
      
      # update model fit and recalculate robust SEs for fixed-effect models
      if (y_hat == "cdr") {
        fit_cdr <- update(fit_cdr_base, family = family(fit_cdr_base), data = hh_obs, weights = wt, offset = log(ptime))
      }
      if (y_hat == "cdr_u5") {
        fit_cdr_u5 <- update(fit_cdr_u5_base, family = family(fit_cdr_u5_base), data = hh_obs, weights = wt, offset = log(ptime))
      }
      if (! part_unit %in% all.vars(formula(fit_cdr))) {
        vcov_cl_cdr <- cluster.vcov(fit_cdr, fit_cdr$data[, cluster_vars])
        vcov_cl_cdr_u5 <- cluster.vcov(fit_cdr_u5, fit_cdr_u5$data[, cluster_vars])  
      }
    }

    # Estimate mortality (only for likely scenario and for entire population-period)
    ts_ac <- ts_ac_base
    f_source_part(script_5, "#place1#", "#place2#")    
    f_source_part(script_5, "#place3#", "#place4#")
      # merge counterfactual population with corresponding scenario time series
      for (jj in grep("cf", scenarios, value = TRUE)) {
        x2 <- get(paste("ts_", jj, sep = ""))
        x2 <- x2[, ! colnames(x2) %in% c("pop_average", "pop_average_u5", "prop_idp", "dep_rate", "arr_rate")]
        pop <- get(paste("pop_", jj, sep = ""))
        x2 <- merge(x2[, ! colnames(x2) %in% c("m", "y")], pop, by = c("admin1", "stratum", "tm"), all.x = TRUE)
        x2 <- subset(x2, tm %in% tm_analysis_start:tm_analysis_end)
        assign(paste("ts_", jj, sep = ""), x2)
      }
    ts_ac <- subset(ts_ac, tm %in% tm_analysis_start:tm_analysis_end)     
    f_source_part(script_5, "#place5#", "#place6#")
    f_source_part(script_5, "#place7#", "#place8#") 
    
    # Add result to output
    out_sens_u5dr[ii, out_cols] <- out[out_cols]
  }  

  #...................................  
  ## Write and graph output

    # Aggregate runs of output
    out_sens_u5dr[, out_cols] <- apply(out_sens_u5dr[, out_cols], c(1,2), function(x) {as.numeric(trimws(gsub(",", "", x)))})
    out_sens_u5dr <- aggregate(out_sens_u5dr[, out_cols], by = list(out_sens_u5dr$prop_u), median, na.rm = TRUE)
    colnames(out_sens_u5dr)[1] <- "prop_u"
    
    # Write output
    write.csv(out_sens_u5dr, paste(country, "_out_sens_u5dr.csv", sep=""), row.names=FALSE)
    
    # Graph output (point estimates only)
      # prepare data  
      x1 <- out_sens_u5dr
      labs <- colnames(x1)[! colnames(x1) %in% out_cols]
      x1 <- data.frame(x1)
      x1 <- x1[complete.cases(x1), ]
      x1[, out_cols] <- lapply(x1[, out_cols], trimws)
      x1[, out_cols] <- lapply(x1[, out_cols], function(x) gsub(",", "", x))
      x1[, out_cols] <- lapply(x1[, out_cols], as.numeric)
      x1 <- melt(x1, id.vars = "prop_u")

      # plot actual and excess death toll, by age
      for (jj in c("", "_u5") ) {
        x2 <- subset(x1, variable %in% 
          c(paste("toll", jj, "_ac_est", sep = ""), paste("toll", jj, "_ex_likely_est", sep = "") ) )
        x2[grep("ac", x2[, "variable"]), "variable2"] <- "deaths under observed conditions  "
        x2[grep("ex", x2[, "variable"]), "variable2"] <- "excess deaths"
        plot <- ggplot(x2, aes(y = value, x = prop_u, colour = variable2)) +
          geom_point(size = 3, alpha = 0.50) +
          geom_line(size = 1.5, alpha = 0.50) +
          scale_colour_manual(values = c(palette_cb[7], palette_cb[8])) +
          scale_y_continuous("estimated death toll", labels = scales::comma, 
            breaks = seq(0, 5000000, by = 100000), limits = c(min(x2[, "value"], na.rm = TRUE) - 50000,
              max(x2[, "value"], na.rm = TRUE) + 50000) ) +
          scale_x_continuous("percentage of under 5y deaths not detected", limits = range(x1[, "prop_u"]),
            labels = label_percent(accuracy = 1)) +
          theme_bw() +
          theme(axis.title = element_text(size = 10, colour = "grey20"), legend.position = "top",
            legend.title = element_blank(), legend.text = element_text(size = 10, colour = "grey20"))
        assign(paste("plot_cdr", jj, sep = ""), plot)
      }

      # arrange both plots side by side and save
      plot <- ggarrange(plot_cdr + theme(axis.title.x = element_blank()), plot_cdr_u5, 
        nrow = 2, labels = c("all ages", "children under 5y"),
        font.label = list(size = 10.5, color = "grey20"), common.legend = TRUE, align = "v", vjust = 0, hjust = 0)
      print(plot)
      ggsave(paste(country, "_", "sensitivity_u5dr.png", sep=""), height = 15, width = 22, 
        units = "cm", dpi = "print")
    
}    

#.........................................................................................
### ENDS
#.........................................................................................