#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## ----- R CODE TO IMPLEMENT ADDITIONAL ANALYSES FOR NORTH-EAST NIGERIA (2016-19) ------ ##
#..........................................................................................

                                        # Written by Francesco Checchi, LSHTM (May 2021)
                                        # francesco.checchi@lshtm.ac.uk 


#...........................................................................................
### Reading in required files
#...........................................................................................
      
  #...................................
  ## Read datasets arising from previous scripts
  x_obs <- read.csv(paste(country, "_x_obs.csv", sep=""), sep="," )
  surveys <- read.csv(paste(country, "_survey_metadata_reanalysed.csv", sep=""), sep="," )
  survey_cover <- read.csv(paste(country, "_survey_stratum_month_cover.csv", sep=""), sep="," )
  ts_ac <- read.csv(paste(country, "_ts.csv", sep=""), sep=",")
  est_tm <- read.csv(paste(country, "_out_est_by_stratum_month.csv", sep=""), sep=",") 
  est_stratum <- read.csv(paste(country, "_out_est_by_stratum.csv", sep=""), sep=",") 


#...........................................................................................
### Extra sensitivity analysis: what if mortality had been higher in inaccessible areas?
#...........................................................................................

  #...................................
  ## sensitivity range
  bias_range <- seq(1, 2, by = 0.2) # 1 means no bias, >1 means relative risk of dying in partially/not
    # accessible areas, compared to estimated

  #...................................
  ## Restrict to death tolls and likely counterfactual scenario
  out <- est_tm[, c("stratum", "tm", "toll_ac_est", "toll_cf_likely_est", 
    "toll_u5_ac_est", "toll_u5_cf_likely_est")]
  
  out[, c("toll_ac_est", "toll_cf_likely_est", 
    "toll_u5_ac_est", "toll_u5_cf_likely_est")] <- lapply(out[, c("toll_ac_est", "toll_cf_likely_est", 
    "toll_u5_ac_est", "toll_u5_cf_likely_est")], function(x) as.numeric(gsub(",", "", x))  )
    
  #...................................
  ## Merge in accessibility data
  out <- merge(out, ts_ac[, c("admin1", "stratum", "tm", "accessibility_ipol")], 
    by = c ("stratum", "tm"), all.x = TRUE)

  #...................................
  ## Inflate actual death tolls per sensitivity range, where accessibility was partial or none
  for (i in bias_range) {
    # All ages
    out[, paste("toll_ac_est_bias_", i, sep = "")] <- out[, "toll_ac_est"]
    out[out[, "accessibility_ipol"] != "full", paste("toll_ac_est_bias_", i, sep = "")] <- 
      out[out[, "accessibility_ipol"] != "full", "toll_ac_est"] * i
    
    # Children under 5y
    out[, paste("toll_u5_ac_est_bias_", i, sep = "")] <- out[, "toll_u5_ac_est"]
    out[out[, "accessibility_ipol"] != "full", paste("toll_u5_ac_est_bias_", i, sep = "")] <- 
      out[out[, "accessibility_ipol"] != "full", "toll_u5_ac_est"] * i
  }

  #...................................
  ## Compute corresponding excess death tolls
  for (i in bias_range) {
    # All ages
    out[, paste("toll_ex_likely_est_bias_", i, sep = "")] <- 
    out[, paste("toll_ac_est_bias_", i, sep = "")] - out[, "toll_cf_likely_est"]
    
    # Children under 5y
    out[, paste("toll_u5_ex_likely_est_bias_", i, sep = "")] <- 
    out[, paste("toll_u5_ac_est_bias_", i, sep = "")] - out[, "toll_u5_cf_likely_est"]
  }
  
  #...................................
  ## Aggregate, save and graph
    # Aggregate and format
    out <- aggregate(out[, grep("bias", colnames(out))], by = list("state" = out[, "admin1"]), FUN = sum, na.rm = TRUE)
    x1 <- melt(out, id.vars = "state")
    x1[, "age_group"] <- ifelse(grepl("u5", x1[, "variable"]), "children under 5y", "all ages")
    x1[, "est_group"] <- ifelse(grepl("ac", x1[, "variable"]), "total deaths", "excess deaths")
    x1[, "inaccessibility_bias"] <- rep(rep(bias_range, each = 6), 2)    
    
    # Save
    write.csv(x1, paste(country, "_out_sens_inaccessibility.csv", sep = ""), row.names = FALSE, na = "")
    
    # Graph
      # one plot for ages and one for children, by state
      for (i in c("", "_u5") ) {
        if (i == "") {x2 <- "all ages"}
        if (i == "_u5") {x2 <- "children under 5y"}
        
        x3 <- subset(x1, age_group == x2)
        plot <- ggplot(x3,aes(y = value, x = inaccessibility_bias, colour = est_group)) +
          geom_point(size = 3, alpha = 0.50) +
          geom_line(size = 1.5, alpha = 0.50) +
          scale_colour_manual(values = c(palette_cb[8], palette_cb[7]) ) +
          facet_wrap(.~state, ncol = 3) +
          scale_y_continuous("estimated death toll", labels = scales::comma, 
            breaks = seq(round(min(x3$value) / 100000, 0) * 100000 - 100000, 
              round(max(x3$value) / 100000, 0) * 100000 + 100000, 
              by = 50000 ) ) +
          scale_x_continuous("inaccessibility bias", breaks = bias_range) +
          theme_bw() +
          theme(axis.title = element_text(size = 10, colour = "grey20")) +
          theme(legend.title = element_blank()) +
          theme(plot.margin = margin(0,0,1,0, unit = "cm") )
        assign(paste("plot_cdr", i, sep = ""), plot)
      }    
    
      # arrange both plots side by side and save
      plot <- ggarrange(plot_cdr + theme(axis.title.x = element_blank(), axis.text.x = element_blank() ),
        plot_cdr_u5, nrow = 2, labels = c("all ages", "children under 5y"),
        font.label = list(size = 10.5, color = "grey20"), common.legend = TRUE, 
        align = "v", vjust = -0.5, hjust = 0)
      print(plot)
      ggsave(paste(country, "_", "sensitivity_inaccessibility.png", sep=""), height = 15, width = 22, 
        units = "cm", dpi = "print")
    
    
#...........................................................................................
### Comparison of different insecurity monitoring sources, by LGA accessibility level
#...........................................................................................

  #...................................
  ## Prepare data
    # Select columns of interest
    sources <- c("acled_fatalities", "cfr_killed", "gtd_killed", "nwc_killed")  
    x1 <- ts_ac[, c("stratum", "y", "m", "pop_average", paste(sources, "_rate", sep = ""), "accessibility_ipol")]
  
    # Reconstitute actual numbers killed  
    x1[, sources] <- x1[, paste(sources, "_rate", sep = "")] * x1[, "pop_average"] / 100000
  
      # how many killed overall, by source?
      colSums(subset(x1, y %in% 2016:2019)[, c("acled_fatalities", "cfr_killed", "gtd_killed", "nwc_killed")] , na.rm = TRUE)  
            
    # Recategorise accessibility
    x1[, "accessibility"] <- NA
    x1[, "accessibility"] <- ifelse(x1[, "accessibility_ipol"] == "full", 
      "accessible LGAs", x1[, "accessibility"])
    x1[, "accessibility"] <- ifelse(x1[, "accessibility_ipol"] == "none", 
      "partially or fully inaccessible LGAs", x1[, "accessibility"])
    x1[, "accessibility"] <- ifelse(x1[, "accessibility_ipol"] == "partial", 
      "partially or fully inaccessible LGAs", x1[, "accessibility"])
    x1[, "accessibility"] <- factor(x1[, "accessibility"], 
      levels = c("accessible LGAs", "partially or fully inaccessible LGAs"))
    
    # Aggregate by accessibility and month / year, then compute rates again
    x1 <- aggregate(x1[, c("pop_average", sources)], by = x1[, c("y", "m", "accessibility")], 
      FUN = sum, na.rm = TRUE)
    x1[, paste(sources, "_rate", sep = "")] <- x1[, sources] * 100000 / x1[, "pop_average"]
    
    # Create date
    x1[, "date"] <- ymd(paste(x1[, "y"], x1[, "m"], 15, sep = "-"))
    
    # Move to long format
    x1 <- reshape2::melt(x1[, c("date", "accessibility", paste(sources, "_rate", sep = ""))], 
      id.vars = c("date", "accessibility") )
    x1[, "source"] <- NA
    x1[, "source"] <- ifelse(grepl("acled", x1[, "variable"]), "Armed Conflict Location & Event Data", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("gtd", x1[, "variable"]), "Global Terrorism Database", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("cfr", x1[, "variable"]), "Nigeria Security Tracker", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("nwc", x1[, "variable"]), "Nigeria Watch", x1[, "source"])
    x1[, "value"] <- x1[, "value"] + 0.00001
    
  #...................................
  ## Graph and save
    # Version 1
    plot <- ggplot(x1, aes(x = date, y = value, group = source, colour = source)) +
      geom_line(size = 1, alpha = 0.5) +
      facet_wrap(.~accessibility, nrow = length(levels(x1$accessibility)), scales = "free") +
      theme_bw() +
      scale_colour_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_y_continuous("people killed per 100,000 person-months\n") +
      scale_x_date("", breaks = "6 months", date_labels = "%b-%Y", limits = c(ymd("2016-01-01"), NA), 
        expand = c(0, 0)) +
      theme(axis.title = element_text(size = 10, colour = "grey20")) +
      theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "bottom")
      
    plot
    ggsave(paste(country, "_additional_insecurity_databases.png", sep = ""), height = 15, width = 22, 
        units = "cm", dpi = "print")  
    
    # # Version 2
    # plot <- ggplot(x1, aes(x = date, y = value, group = accessibility, colour = accessibility)) +
    #   geom_line(size = 1) +
    #   facet_wrap(.~source, nrow = length(unique(x1$source)), scales = "free") +
    #   theme_bw() +
    #   scale_colour_manual(values = palette_cb[c(2, 4)]) +
    #   scale_y_continuous("people killed per 100,000 person-months") +
    #   scale_x_date("", breaks = "6 months", date_labels = "%b-%Y") +
    #   theme(axis.title = element_text(size = 10, colour = "grey20")) +
    #   theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "bottom")
    #   
    # plot
    # ggsave(paste(country, "_additional_insecurity_databases.png", sep = ""), height = 15, width = 22, 
    #     units = "cm", dpi = "print")      
  
    
#...........................................................................................
### Comparison of different insecurity monitoring sources with CDR and proportion of injury deaths
#...........................................................................................

  #...................................
  ## Prepare data
    # Select columns of interest
    sources <- c("acled_fatalities", "cfr_killed", "gtd_killed", "nwc_killed")  
    x1 <- ts_ac[, c("stratum", "tm", "pop_average", paste(sources, "_rate", sep = ""))]
  
    # Reconstitute actual numbers killed  
    x1[, sources] <- x1[, paste(sources, "_rate", sep = "")] * x1[, "pop_average"] / 100000

  #...................................
  ## Figure out mean rates of people killed for each survey recall period
  x1 <- merge(survey_cover, x1[, c("stratum", "tm", "pop_average", sources)], by = c("stratum", "tm") )
  x1[, "sum_wt_pop"] <- x1[, "month_coverage"] * x1[, "pop_average"]
  x1[, "sum_wt_acled"] <- x1[, "month_coverage"] * x1[, "acled_fatalities"]
  x1[, "sum_wt_gtd"] <- x1[, "month_coverage"] * x1[, "gtd_killed"]
  x1[, "sum_wt_cfr"] <- x1[, "month_coverage"] * x1[, "cfr_killed"]
  x1[, "sum_wt_nwc"] <- x1[, "month_coverage"] * x1[, "nwc_killed"]
  x1 <- aggregate(x1[, c("month_coverage", grep("sum_wt", colnames(x1), value = TRUE))], 
    by = list("survey_id" = x1[, "survey_id"]), FUN = sum, na.rm = TRUE)
  x1[, paste(sources, "_rate", sep = "")] <- x1[, c("sum_wt_acled", "sum_wt_cfr", "sum_wt_gtd", "sum_wt_nwc")] *
    100000 / x1[, "sum_wt_pop"]
    
  #...................................
  ## Merge proportion of injury deaths and CDR
  x1 <- merge(x1, surveys[, c("survey_id", "final_cdr_est", "final_prop_inj",
    "final_cdr_inj_est")], by = "survey_id")  
  
  #...................................
  ## Graph and save
    # Prepare data
    x1 <- x1[, c("final_cdr_est", "final_prop_inj", "final_cdr_inj_est", paste(sources, "_rate", sep = ""))]
    x1 <- melt(x1, id.vars = c("final_cdr_est", "final_prop_inj", "final_cdr_inj_est") )
    x1[, "source"] <- NA
    x1[, "source"] <- ifelse(grepl("acled", x1[, "variable"]), "ACLED", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("gtd", x1[, "variable"]), "Global Terrorism Database", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("cfr", x1[, "variable"]), "Nigeria Security Tracker", x1[, "source"])
    x1[, "source"] <- ifelse(grepl("nwc", x1[, "variable"]), "Nigeria Watch", x1[, "source"])
    x1[, "value_cat"] <- cut(x1[, "value"], breaks = c(0, 0.000001, 1, 2, 100), 
      labels = c("0", "0.01 to 0.99", "1.00 to 1.99", ">= 2.00") , include.lowest = TRUE)
    
    # Plot CDR
    x2 <- x1[, c("final_cdr_est", "value_cat", "source")]
    plot_cdr <- ggplot(x2, aes(x = value_cat, y = final_cdr_est, fill = source, colour = source)) +
      geom_boxplot(size = 1, alpha = 0.5) +
      theme_bw() +
      facet_wrap(.~ source) +
      scale_colour_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_fill_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_y_continuous("crude death rate per 10,000 person-days") +
      scale_x_discrete("people killed per 100,000 person-months") +
      theme(axis.title = element_text(size = 10, colour = "grey20")) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1) ) +
      theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "bottom")
      
    plot_cdr
    
    # Plot injury death rate
    x2 <- x1[, c("final_cdr_inj_est", "value_cat", "source")]
    plot_cdr_inj <- ggplot(x2, aes(x = value_cat, y = final_cdr_inj_est, fill = source, colour = source)) +
      geom_boxplot(size = 1, alpha = 0.5) +
      theme_bw() +
      facet_wrap(.~ source) +
      scale_colour_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_fill_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_y_continuous("injury-specific death rate per 10,000 person-days") +
      scale_x_discrete("people killed per 100,000 person-months") +
      theme(axis.title = element_text(size = 10, colour = "grey20")) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1) ) +
      theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "bottom")
      
    plot_cdr_inj
    
    # Plot proportion of deaths due to injury
    x2 <- x1[, c("final_prop_inj", "value_cat", "source")]
    plot_prop_inj <- ggplot(x2, aes(x = value_cat, y = final_prop_inj, fill = source, colour = source)) +
      geom_boxplot(size = 1, alpha = 0.5) +
      theme_bw() +
      facet_wrap(.~ source) +
      scale_colour_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_fill_manual(values = palette_cb[c(4, 6, 7, 8)]) +
      scale_y_continuous("proportion of deaths due to injury") +
      scale_x_discrete("people killed per 100,000 person-months") +
      theme(axis.title = element_text(size = 10, colour = "grey20")) +
      theme(axis.text.x = element_text(angle = 30, hjust = 1) ) +
      theme(legend.title = element_text(size = 10, colour = "grey20"), legend.position = "bottom")
      
    plot_prop_inj
    
    # Plot CDR vs proportion of deaths due to injury
    x2 <- subset(x1, source == "Global Terrorism Database")[, c("final_cdr_est", "final_prop_inj")]
    plot_cdr_vs_prop_inj <- ggplot(x2, aes(x = final_prop_inj, y = final_cdr_est)) +
      geom_point(size = 4, alpha = 0.5, colour = palette_cb[6], fill = palette_cb[6]) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2, colour = palette_cb[6], fill = palette_cb[6]) +
      theme_bw() +
      scale_x_continuous("proportion of deaths due to injury") +
      scale_y_continuous("crude death rate per 10,000 person-days") +
      theme(axis.title = element_text(size = 10, colour = "grey20"))

    plot_cdr_vs_prop_inj
     
    # Plot CDR vs death rate due to injury
    x2 <- subset(x1, source == "Global Terrorism Database")[, c("final_cdr_est", "final_cdr_inj_est")]
    plot_cdr_vs_cdr_inj <- ggplot(x2, aes(x = final_cdr_inj_est, y = final_cdr_est)) +
      geom_point(size = 4, alpha = 0.5, colour = palette_cb[6], fill = palette_cb[6]) +
      geom_smooth(method = "lm", se = TRUE, alpha = 0.2, colour = palette_cb[6], fill = palette_cb[6]) +
      theme_bw() +
      scale_x_continuous("injury-specific death rate per 10,000 person-days") +
      scale_y_continuous("crude death rate per 10,000 person-days") +
      theme(axis.title = element_text(size = 10, colour = "grey20"))

    plot_cdr_vs_cdr_inj
    
    # Arrange plots and save
    plot <- ggarrange(plot_cdr, plot_prop_inj, plot_cdr_vs_prop_inj, plot_cdr_vs_cdr_inj, 
      common.legend = TRUE)
    
    plot
    ggsave(paste(country, "_additional_insecurity_correlations.png", sep = ""), height = 22, width = 22, 
        units = "cm", dpi = "print")  
    
    
#...........................................................................................
### Mortality and accessibility estimate maps
#...........................................................................................
    
    # Add accessibility data (% of months inaccessible over analysis period)
    x1 <- ts_ac[, c("stratum", "accessibility_ipol")]
    x1[, "months_inaccessible"] <- ifelse(x1[, "accessibility_ipol"] == "full", 0, 1)
    x1 <- aggregate(x1[, "months_inaccessible"], by = list("stratum" = x1$stratum),
      FUN = sum, na.rm = TRUE)
    colnames(x1) <- c("ADM2_EN", "months_inaccessible")
    x1[, "months_inaccessible"] <- x1[, "months_inaccessible"] * 100 / 
      length(unique(ts_ac[complete.cases(ts_ac[, "accessibility_ipol"]), "tm"]))
    adm2 <- merge(adm2, x1, by = "ADM2_EN", all.x = TRUE)

    # Add mortality estimates
    colnames(est_stratum)[colnames(est_stratum) == "stratum"] <- "ADM2_EN"
    adm2 <- merge(adm2, est_stratum, by = "ADM2_EN")
    
  #...................................
  ## Graph CDR and save
  pos_lga <- rep(0, times <- length(adm2$ADM2_EN))
  names(pos_lga) <- adm2$ADM2_EN
  pos_lga[c("Kwaya Kusar", "Yola North", "Shelleng")] <- 0.3
  pos_lga["Fufore"] <- -0.3
    
  map_cdr <- tm_shape(adm2) + 
    tm_text(col = "black", "ADM2_EN", size = 0.5, ymod = pos_lga, alpha = 1) + 
    tm_borders(col = "grey20") + 
    tm_fill("cdr_ac_est", alpha = 0.5, palette="Reds",
      title = "crude death rate\n (per 10,000 person-days)") +
    tm_shape(adm1) + 
    tm_text("ADM1_EN", col = palette_cb[6], size = 0.6, xmod = c(0, -0.5, -1),
      ymod = c(1, 0.5, 0), fontface = "bold", alpha = 1) +
    tm_borders(palette_cb[6], lwd = 2, alpha = 0.5) + 
    tm_layout(legend.title.size = 0.7, legend.text.size =  0.5, legend.text.color = "grey20", 
      legend.title.color = "grey20")
  
  map_cdr
  tmap_save(map_cdr, paste(country, "_out_cdr_map_long.png", sep = ""), 
    height = 15, width = 15, units = "cm", dpi = 300)     

  
  #...................................
  ## Graph accessibility and save
  pos_lga <- rep(0, times <- length(adm2$ADM2_EN))
  names(pos_lga) <- adm2$ADM2_EN
  pos_lga[c("Kwaya Kusar", "Yola North", "Shelleng")] <- 0.3
  pos_lga["Fufore"] <- -0.3
    
  map_access <- tm_shape(adm2) + 
    tm_text(col = "black", "ADM2_EN", size = 0.5, ymod = pos_lga, alpha = 1) + 
    tm_borders(col = "grey20") + 
    tm_fill("months_inaccessible", alpha = 0.5, palette = "Greys",
      title = "% of analysis months with limited accessibility") +
    tm_shape(adm1) + 
    tm_text("ADM1_EN", col = palette_cb[6], size = 0.6, xmod = c(0, -0.5, -1),
      ymod = c(1, 0.5, 0), fontface = "bold", alpha = 1) +
    tm_borders(palette_cb[6], lwd = 2, alpha = 0.5) + 
    tm_layout(legend.title.size = 0.7, legend.text.size =  0.5, legend.text.color = "grey20", 
      legend.title.color = "grey20")
  
  map_access
  tmap_save(map_access, paste(country, "_out_access_map_long.png", sep = ""), 
    height = 15, width = 15, units = "cm", dpi = 300)     

  
  

#...........................................................................................
### ENDS
#...........................................................................................
