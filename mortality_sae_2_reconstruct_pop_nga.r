#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## ------------- R CODE TO RECONSTRUCT POPULATION DENOMINATORS IN NIGERIA -------------- ##
#..........................................................................................

                                        # Written by Francesco Checchi, LSHTM (August 2019)
                                        # francesco.checchi@lshtm_ac.uk 

#place1#
#..........................................................................................    
### Preparing population source datasets 
#..........................................................................................

  #.........................................
  ## Prepare strata /time metadata
  
  tm_pop <- unique(t_units$tm)
  stratum_names_pop <- c(sort(stratum_names), "other_state", "other_country")
    
  #.........................................
   ## Population source metadata
    
    # Keep only population sources (not IDP or refugee datasets) and relevant variables
    pop_sources_df <- pop_sources[grep("pop_", pop_sources$worksheet), ]
    pop_sources_df <- pop_sources_df[, c("worksheet", "quality_score", "m", "y")]
    colnames(pop_sources_df)[colnames(pop_sources_df) == "worksheet"] <- "pop_source"
    
    # Figure out tm point
    pop_sources_df <- merge(pop_sources_df, t_units, by=c("y", "m"), all.x=TRUE)

  #.........................................
  ## Population source datasets
        
    # Sort each dataset
    for (i in c(pop_sources_df[, "pop_source"]) ) { 
      x1 <- get(i)
      x1[, "stratum"] <- as.character(x1[, "stratum"])
      assign(i, x1[order(x1[, "stratum"]), ])
    }

  #.........................................
  ## Set assumed growth rate per month, by LGA
  g <- strata[, c("admin1", "stratum")]
  g[g$admin1 == "Adamawa", "g"] <- g_adamawa / 12
  g[g$admin1 == "Borno", "g"] <- g_borno / 12
  g[g$admin1 == "Yobe", "g"] <- g_yobe / 12
  g <- g[order(g[, "stratum"]), ]

  
#..........................................................................................    
### Preparing IDP dataset
#..........................................................................................
    
  #.........................................
  ## Delete observations
    
    # Delete excluded observations
      idp_df <- subset(idp_iom, excluded == "N")
    
    # Delete observations for which the LGA is missing (very few)
      idp_df <- subset(idp_df, ! stratum %in% c("Borno_lga_missing", "Adamawa_lga_missing", "Yobe_lga_missing") )
      idp_df <- subset(idp_df, ! stratum_origin %in% c("Borno_lga_missing", "Adamawa_lga_missing", "Yobe_lga_missing") )
    
    # Delete observations for which idp_alt is missing (only use idp_alt as the number of IDPs)
      idp_df <- subset(idp_df, is.na(idps_alt) == FALSE)
#place2#       
  #.........................................
  ## Graph IDP data availability (number of sites that were surveyed by IOM by stratum and month)

    # Prepare data for plotting
    x1 <- idp_df

      # aggregate number of sites surveyed by time unit and stratum
      x1[, "n_sites"] <- 1
      x1 <- aggregate(x1[, "n_sites"], by = x1[, c("y", "m", "stratum")], FUN=sum)
      colnames(x1) <- c("y", "m", "stratum", "n_sites")

      # create all unique combinations of stratum and tm
      x1 <- merge(ts[, c("stratum", "tm", "m", "y")], x1, by = c("stratum", "m", "y"), all.x = TRUE, sort = TRUE)
      x1[, "stratum"] <- factor(x1[, "stratum"])
      
      # add dates and admin1
      x1[, "date"] <- dmy(paste("1", x1[, "m"], x1[, "y"], sep="/"))
      x1 <- merge(x1, strata, by="stratum", all.x=TRUE)

    # Plot
      plot <- ggplot(subset(x1, y >= y_analysis_start ), aes(x = date, y = stratum, fill = n_sites)) +
        geom_tile(color = "gray70") +
        geom_text(aes(label = n_sites), size = 3, colour = "grey20") +
        theme_bw() +
        scale_y_discrete("LGA", expand = c(0,0) ) +
        scale_fill_gradient(na.value = "white", low = "darkseagreen1", high = "darkseagreen4" ) +
        labs(x = "date", y = "Number of IDP sites surveyed", fill = "number of sites") +
        scale_x_date(date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
        facet_grid(rows=vars(admin1), space = "free", scales = "free", switch = "x") +
        theme(plot.margin = unit(c(0,1,0,0), "cm"), legend.position="bottom") +
        theme(axis.text = element_text(size = 9, colour = "grey20"),
        axis.title = element_text(size = 10, colour = "grey20") )

      plot
      ggsave(paste(country, "_pop_idp_data_coverage.png", sep=""), height = 35, width = 30, units = "cm", dpi = "print")

#place3#      
  #.........................................
  ## Aggregate database by LGA of destination, LGA of origin, time and year of displacement
    
    # Earliest year of displacement for which data are available
    y_idp_start <- min(idp_df[, "year_dis"], na.rm = TRUE)
      
    # Add time units
    idp_df <- merge(idp_df, t_units, by=c("y", "m"))  
    
    # Recode the year of displacement as year of DTM assessment, if missing
    idp_df[, "year_dis"] <- ifelse(is.na(idp_df[, "year_dis"]), idp_df[, "y"], idp_df[, "year_dis"])
    
    # Aggregate to only contain unique instances of stratum, stratum of origin, year of displacement and time unit
    idp_df <- aggregate(idp_df[, "idps_alt"], by = idp_df[, c("stratum", "stratum_origin", "tm", "year_dis")], FUN = sum)
    colnames(idp_df) <- c("stratum", "stratum_origin", "tm", "year_dis", "idps")
      
    # Reshape wide so as to have years of origin as columns
    idp_df <- dcast(idp_df, stratum + stratum_origin + tm ~ year_dis, value.var = "idps")
    
    # Expand data frame to contain all strata, all strata of origin and all time units
    x1 <- expand.grid(stratum_names_pop, stratum_names_pop, tm_pop)
    colnames(x1) <- c("stratum", "stratum_origin", "tm")
    idp_df <- merge(x1, idp_df, by=c("stratum", "stratum_origin", "tm"), all.x=TRUE, sort=TRUE)

    # Add years and months
    idp_df <- merge(idp_df, t_units, by = "tm", all.x = TRUE)

        
  #.........................................
  ## Figure out IDP totals at the start of each year, for each LGA (destination) : LGA (origin) combination
          
    # Aggregate database by years of arrival prior to the year of interest 
    
      # first, sum up all IDP groups that arrived before the start of the analysis period...
      idp_df[, paste("pre_", y_analysis_start, sep = "")] <- rowSums(idp_df[, paste(y_idp_start:(y_analysis_start-1) )], na.rm = TRUE)
        # ...but convert result to NA if all year values are NA
        x1 <- apply(idp_df, 1, function(x) {all(is.na(x[paste(y_idp_start : (y_analysis_start-1))]))} )
        idp_df[x1, paste("pre_", y_analysis_start, sep = "")] <- NA

      # next, sum IDPs over all years of origin...
      idp_df[, "idps"] <- rowSums(idp_df[, paste(y_idp_start : y_analysis_end)], na.rm=TRUE)
        # ...but convert result to NA if all year values are NA
        x1 <- apply(idp_df, 1, function(x) {all(is.na(x[paste(y_idp_start : y_analysis_end)]))} )
        idp_df[x1, "idps"] <- NA
      
    # Apply assumptions about number of IDPs at start of each year, and set silent years to 0
      # for each stratum...
      for (i in stratum_names_pop) {
        print(paste("now attributing year-start IDP values for stratum...", i, sep = ""))
        
        # select stratum time series
        x1 <- subset(idp_df, stratum == i)
          
        # for each stratum of origin...
        for (j in stratum_names_pop) {
            
          # select stratum of origin time series
          x2 <- subset(x1, stratum_origin == j)
              
          # sort
          x2 <- x2[order(x2[, "tm"]), ]
              
          # establish year and time point of first assessment (= NA if the entire time series has NA values)
          if (all(is.na(x2[, "idps"])) ) {x3 <- c(NA, NA, NA) }
          if (! all(is.na(x2[, "idps"])) ) {x3 <- as.numeric(x2[min(which(! is.na(x2[, "idps"]))), c("y", "m", "tm")] )}

          # establish starting IDP figures for each January prior the date of first assessment
            # if the first assessment was in 2015, set Jan 2015 value = mean pre-2015 value
            # if the first assessment was in 2016, set Jan 2015 value = mean pre-2015 value;
              # Jan 2016 value = mean pre-2015 value + mean 2016 value; etc.
          if (! is.na(x3[1])) {  
            # January time points for which to set starting figure
            x4 <- t_units[t_units$y %in% c(y_analysis_start : x3[1]) & t_units$m == 1, ]
            
            # starting January values
            x5 <- paste("pre_", y_analysis_start, sep = "")
            if (all(is.na(x2[, x5])) )    {x6 <- 0}
            if (! all(is.na(x2[, x5])) )  {x6 <- mean(x2[, x5], na.rm = TRUE) }
            
            for (k in 1:nrow(x4) ) {
              x7 <- paste(x4[k, "y"])
              idp_df[idp_df[, "stratum"] == i & idp_df[, "stratum_origin"] == j & idp_df[, "tm"] == x4[k, "tm"], "idps"] <- x6
              if (all(is.na(x2[, x7])))   {x8 <- 0}
              if (! all(is.na(x2[, x7]))) {x8 <- mean(x2[, x7], na.rm = TRUE) }
              x6 <- x6 + x8
            }

          }

        }

      }
    
 
  #.........................................
  ## Interpolate and extrapolate each time series

    # Preparatory steps
      # create interpolation variable
      idp_df[, "idps_ipol"] <- NA

      # sort
      idp_df <- idp_df[order(idp_df[, "stratum"], idp_df[, "stratum_origin"], idp_df[, "tm"]), ]
    
    # Interpolate each LGA (destination) : LGA (origin) time series      
      # for each stratum...
      for (i in stratum_names_pop) {
        print(paste("now interpolating IDPs in stratum...", i, sep = "") )
        
        # select stratum time series
        x1 <- subset(idp_df, stratum == i)
        
        # for each stratum of origin...
        for (j in stratum_names_pop) {
          
          # select stratum of origin time series
          x2 <- subset(x1, stratum_origin == j)
          
          # sort
          x2 <- x2[order(x2[, "tm"]), ]
          
          # if time series has no non-zero or non-NA values, set all IDP values to 0
          if (all(is.na(x2[, "idps"]))) {x3 <- rep(0, nrow(x2))}
          if (sum(x2[, "idps"], na.rm=TRUE) == 0) {x3 <- rep(0, nrow(x2))}
          
          # otherwise, use linear interpolation...
          if (sum(x2[, "idps"], na.rm=TRUE) > 0) {
            x3 <- approx(x=na.omit(x2[, "tm"]), y=x2[, "idps"], xout=x2[, "tm"], method="linear", rule = 2)$y
          
          }
              
        # attribute interpolated data, rounded to nearest 10 IDPs
        idp_df[idp_df[, "stratum"]==i & idp_df[, "stratum_origin"]==j, "idps_ipol"] <- round(x3/10, digits=0)*10
      }
        
    }

  #.........................................
  ## Clean up and reshape output
        
    # Clean up and sort
    idp_df[, "idps"] <- idp_df[, "idps_ipol"]
    idp_df <- idp_df[, ! colnames(idp_df) %in% c("idps_ipol")]
    idp_df[, "stratum"] <- as.character(idp_df[, "stratum"] )
    idp_df[, "stratum_origin"] <- as.character(idp_df[, "stratum_origin"] )
    idp_df <- idp_df[order(idp_df[, "stratum"], idp_df[, "stratum_origin"], idp_df[, "tm"]), ]

#place4#      
#..........................................................................................    
### Preparing refugee dataset
#..........................................................................................
      
  #.........................................
  ## Preliminary steps
    # Figure out time units
    refugees_df <- merge(refugees, t_units, by = c("y", "m"), sort=TRUE)
    
    # Select and rename columns
    refugees_df <- refugees_df[, c("stratum_origin", "tm", "refugees")]

    # Prepare for combining with IDP data
    refugees_df[, "stratum"] <- "other_country"
    

#place5#
#..........................................................................................    
### Preparing overall displacement matrix and adjusting for implausible displacement figures
#..........................................................................................
      
  #.........................................
  ## Combine IDP and refugee data
    
    # Merge IDP and refugee data into a single matrix
    disp_matrix <- merge(idp_df, refugees_df, by = c("stratum", "stratum_origin", "tm"), all.x = TRUE)
    disp_matrix[is.na(disp_matrix$refugees_df), "refugees"] <- 0
    
    # Add refugees_df to people leaving
    disp_matrix[, "n_disp"] <- disp_matrix[, "idps"] + disp_matrix[, "refugees"]
    
    # Prepare dataset
    disp_matrix <- disp_matrix[, c("stratum", "stratum_origin", "y", "m", "tm", "n_disp")]
    disp_matrix[is.na(disp_matrix$n_disp), "n_disp"] <- 0
    
  #.........................................
  ## Forward- and backward-project population sources as if displacement had not occurred
    
    # For each population source...
    for (i in 1:nrow(pop_sources_df)) {
      
      # capture population dataset
      pop_source <- get(paste(pop_sources_df[i, "pop_source"]))
      pop_source <- pop_source[order(pop_source[, "stratum"]), ]
      tm_pop_source <- pop_sources_df[i, "tm"]      
      
      # temporary dataframe to hold estimated figures for this population source
      x1 <- as.data.frame(matrix(nrow=length(stratum_names_pop) - 2, ncol=length(tm_pop) + 1))
      colnames(x1) <- c("stratum", paste(tm_pop) )
      x1[, "stratum"] <- sort(stratum_names_pop[! stratum_names_pop %in% c("other_state", "other_country")] )
    
      # at the time unit from which calculation starts, population figures come from the known source
      x1[, paste(tm_pop_source)] <- pop_source[, "pop"]
      
      # forward calculation
      for (j in (tm_pop_source + 1) : max(tm_pop)) {
        if (tm_pop_source >= max(tm_pop)) break;
        x1[, paste(j)] <- (1 + g[, "g"]) * x1[, paste(j-1)]
      }
      
      # backward calculation
      for (j in (tm_pop_source - 1) : min(tm_pop)) {
        x1[, paste(j)] <- (x1[, paste(j+1)] ) / (1 + g[, "g"])
      }
    
      # assign projection
      assign(paste(pop_sources_df[i, "pop_source"], "_unadj", sep = ""), x1)
            
    }

    
  #.........................................
  ## Work out maximum ratio of population and people displaced from the stratum, for each population source
    
    # Prepare output
    out <- as.data.frame(matrix(nrow = length(stratum_names_pop) - 2, ncol = 1 + nrow(pop_sources_df)))
    colnames(out) <- c("stratum", paste(pop_sources_df$pop_source) )
    out[, "stratum"] <- sort(stratum_names_pop[! stratum_names_pop %in% c("other_state", "other_country")] )
    
    # For each population source...
    for (i in 1:nrow(pop_sources_df)) {
      
      # aggregate displacement matrix into totals displaced from each stratum, for any time point
      x2 <- aggregate(disp_matrix[, c("n_disp")], by = disp_matrix[, c("stratum_origin", "tm")], FUN = sum)
      colnames(x2) <- c("stratum", "tm", "n_disp")
      x2 <- x2[! x2[, "stratum"] %in% c("other_state", "other_country"), ]
      x2 <- x2[order(x2$stratum, x2$tm), ]
      x2 <- dcast(x2, stratum ~ tm, value.var = "n_disp")
      
      # compute highest ratio of totals displaced from the stratum : stratum population, across all the time points  
      x3 <- get( paste(pop_sources_df[i, "pop_source"], "_unadj", sep = "") )
      x3 <- x3[order(x3$stratum), ]
      x4 <- apply(x2[, 2:ncol(x2)] / x3[, 2:ncol(x3)], 1, FUN=max)
      
      # record scaling ratio in output
      out[, pop_sources_df[i, "pop_source"] ] <- x4
      
    }
#place6#    
    # Write output
    write.csv(out, paste(country, "_pop_implausible_strata.csv", sep = ""), row.names = FALSE)
#place7#     
  #.........................................
  ## Come up with plausible displacement matrices for each population source
    # by scaling down instances in which the number displaced from the stratum exceeds the stratum population
    
    # Apply scaling ratio to displacement figures, for strata where the ratio > 1
    for (i in 1:nrow(pop_sources_df)) {
      
      # population source
      x1 <- pop_sources_df[i, "pop_source"]
      
      # set ratios < 1 ratio = 1
      x2 <- out[, c("stratum", x1)]
      x2[x2[, x1] <= 1, x1] <- 1
      colnames(x2) <- c("stratum_origin", "ratio")
      
      # apply ratios
      x3 <- merge(disp_matrix, x2, by = "stratum_origin", all.x = TRUE)
      x3[is.na(x3[, "ratio"]), "ratio"] <- 1
      x3[, "n_disp"] <- x3[, "n_disp"] / x3[, "ratio"]
      
      # assign output as displacement matrix specific to the population source
      assign(paste("disp_matrix_", x1, sep = "") , x3)
      
    }  
     
    
        
#..........................................................................................    
### Estimating total population across the time series, for each alternative population source
#..........................................................................................
    
  #.........................................
  ## Initialise dataframe to hold total population for each alternative population source
  pop <- ts
    
for (i in 1:nrow(pop_sources_df)) {

  #.........................................
  ## Capture the population source data
  x1 <- get(paste(pop_sources_df[i, "pop_source"], "_unadj", sep = ""))
  
  #.........................................
  ## Prepare displacement data
    # Get displacement matrix
    x2 <- get(paste("disp_matrix_", pop_sources_df[i, "pop_source"], sep = ""))
      
    # Data frame of people displaced to each stratum, for any time point ("where people have gone to")
    disp_to <- aggregate(x2[, "n_disp"], by = x2[, c("stratum", "tm")], FUN = sum)
    colnames(disp_to) <- c("stratum", "tm", "n_disp")
    disp_to <- disp_to[! disp_to[, "stratum"] %in% c("other_state", "other_country"), ]
    disp_to <- disp_to[order(disp_to$stratum, disp_to$tm), ]

      # reshape wide
      disp_to <- dcast(disp_to, stratum ~ tm, value.var = "n_disp")
      
      # convert into net change during time period
      disp_to_change <- data.frame(disp_to[, "stratum"], t(apply(disp_to[, 2:ncol(disp_to)], 1, diff) ), rep(0, nrow(disp_to)))
      colnames(disp_to_change) <- c("stratum", paste(tm_pop) )
      disp_to_change <- disp_to_change[order(disp_to_change[, "stratum"]), ]
      
    # Data frame of people displaced from each stratum, for any time point ("where people are coming from")
    disp_from <- aggregate(x2[, "n_disp"], by = x2[, c("stratum_origin", "tm")], FUN = sum)
    colnames(disp_from) <- c("stratum", "tm", "n_disp")
    disp_from <- disp_from[! disp_from[, "stratum"] %in% c("other_state", "other_country"), ]
    disp_from <- disp_from[order(disp_from$stratum, disp_from$tm), ]

      # reshape wide
      disp_from <- dcast(disp_from, stratum ~ tm, value.var = "n_disp")
      
      # convert into net change during time period
      disp_from_change <- data.frame(disp_from[, "stratum"], t(apply(disp_from[, 2:ncol(disp_from)], 1, diff) ), rep(0, nrow(disp_from)))
      colnames(disp_from_change) <- c("stratum", paste(tm_pop) )
      disp_from_change <- disp_from_change[order(disp_from_change[, "stratum"]), ]
      
       
  #.........................................
  ## Apply displacement matrix to population data
  x1[, paste(tm_pop)] <- x1[, paste(tm_pop)] + disp_to[, paste(tm_pop)] - disp_from[, paste(tm_pop)]
      
  #.........................................
  ## Pass results into main population data frame 
    
    # Reshape long
      # population estimates
      x1 <- melt(x1)
      colnames(x1) <- c("stratum", "tm", pop_sources_df[i, "pop_source"])
      
      # IDPs within strata
      x2 <- melt(disp_to)
      colnames(x2) <- c("stratum", "tm", paste("idp_", pop_sources_df[i, "pop_source"], sep = "") )
      
      # People arriving into strata
      x3 <- melt(disp_to_change)
      colnames(x3) <- c("stratum", "tm", paste("n_arr_", pop_sources_df[i, "pop_source"], sep = "") )
      
      # People departing strata
      x4 <- melt(disp_from_change)
      colnames(x4) <- c("stratum", "tm", paste("n_dep_", pop_sources_df[i, "pop_source"], sep = "") )
      
    # Add to output
    pop <- merge(pop, x1, by = c("stratum", "tm"), all.x = TRUE)
    pop <- merge(pop, x2, by = c("stratum", "tm"), all.x = TRUE)
    pop <- merge(pop, x3, by = c("stratum", "tm"), all.x = TRUE)
    pop <- merge(pop, x4, by = c("stratum", "tm"), all.x = TRUE)
      
}


  #.........................................
  ## Come up with weighted population averages, by combining all sources and their relative quality weights
    
    # Define weights (quality scores)
    pop_wts <- as.vector(pop_sources_df[, "quality_score"])
  
    # Weighted mean population
    pop[, "pop_average"] <- round (apply(pop[, paste(pop_sources_df$pop_source) ], 1, 
      function(x, pop_wts) {return(weighted.mean(x, pop_wts, na.rm=TRUE))} ) , digits = -2)

    # Weighted mean IDPs
    pop[, "idp_average"] <- round (apply(pop[, grep("idp_", colnames(pop) )], 1, 
      function(x, pop_wts) {return(weighted.mean(x, pop_wts, na.rm=TRUE))} ) , digits = -2)
    
    # Weighted mean people departing
    pop[, "n_dep_average"] <- round (apply(pop[, grep("n_dep_", colnames(pop) )], 1, 
      function(x, pop_wts) {return(weighted.mean(x, pop_wts, na.rm=TRUE))} ) , digits = -2)
    
    # Weighted mean people arriving
    pop[, "n_arr_average"] <- round (apply(pop[, grep("n_arr_", colnames(pop) )], 1, 
      function(x, pop_wts) {return(weighted.mean(x, pop_wts, na.rm=TRUE))} ) , digits = -2)
    
  #.........................................
  ## Avoid rare instances of zero population (make sure population is at least 100 anywhere, anytime)
  print(paste("there are ", nrow(pop[pop$pop_average %in% c(0:100), ]), 
    " instances of population < 100: convert these to 100", sep = ""))  
  pop[pop$pop_average %in% c(0:100), "pop_average"] <- 100
            
  #.........................................
  ## Add under 5 population
  pop[, "pop_average_u5"] <- round(pop[, "pop_average"] * u5_prop, digits = -1)

  
#..........................................................................................    
### Computing IDP proportions and flow rates (exits/returns) for each stratum
#..........................................................................................

  #.........................................
  ## Compute IDP proportion
  pop[, "prop_idp"] <- pop[, "idp_average"] / pop[, "pop_average"]

  #.........................................
  ## Compute IDP flow rates
          
    # Departure rate
    pop[, "dep_rate"] <- pop[, "n_dep_average"] * 10000 / pop[, "pop_average"]
      
    # Arrival rate
    pop[, "arr_rate"] <- pop[, "n_arr_average"] * 10000 / pop[, "pop_average"]
        
  
#..........................................................................................    
### Writing output and generating relevant graphs
#..........................................................................................
        
  #.........................................
  ## Write population estimates
  x1 <- c("stratum", "admin1", "tm", "y", "m", paste(pop_sources_df$pop_source), grep("average", colnames(pop), value = TRUE), 
    "prop_idp", grep("rate", colnames(pop), value = TRUE) )
#place8#  
  write.csv(pop[,  x1], paste(country, "_pop_denoms.csv", sep=""), row.names=FALSE)
  

  #.........................................
  ## Graph population estimates
  
    # reshape long
    x1 <- melt(pop[, c("admin1", "stratum", "m", "y", pop_sources_df$pop_source, "pop_average")], 
      id.vars=c("admin1", "stratum", "m", "y"), variable.name = "pop_source", value.name = "pop")
    
    # graph by LGA (average only)
      # prepare
      x2 <- subset(x1, pop_source == "pop_average" & y <= y_analysis_end)
      x2[, "date"] <- as.Date.character(paste(x2[, "y"], x2[, "m"], "01", sep="-"))
    
      # plot
      plot <- ggplot(x2, aes(x = date, y = pop, fill = admin1, colour = admin1)) + 
        geom_area(alpha = 0.5) +
        geom_line(alpha = 0.2) +
        scale_x_date("", date_labels = "%Y", breaks = "12 months", expand=c(0,0)) +
        scale_y_continuous("population", labels = scales::comma) + 
        scale_fill_manual(values = palette_cb[c(4,6,7)]) +
        scale_colour_manual(values = palette_cb[c(4,6,7)]) +
        theme_bw() +
        labs (fill = "state:", colour = "state:") +
        theme(axis.text = element_text(size = 8, colour = "grey20"), axis.title = element_text(size = 9, colour="grey20")) +
        theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1)) +
        facet_wrap(~stratum, ncol = 6, scales = "free_y") +
        theme(strip.text = element_text(size = 8, colour = "grey20")) +
        theme(legend.title = element_text(size = 9, colour = "grey20"), legend.position = "bottom")
      
      print(plot)
      ggsave(paste(country, "_pop_trends_stratum.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")

    # graph by admin1
      # aggregate by admin1
      x2 <- aggregate(x1[, "pop"], by = x1[, c("admin1", "y", "m", "pop_source")], FUN = sum)
      x2 <- subset(x2, y <= y_analysis_end)
      colnames(x2) <- c("admin1", "y", "m", "pop_source", "pop")
      x2[, "date"] <- as.Date.character(paste(x2[, "y"], x2[, "m"], "01", sep="-"))
      plot_labs <- c("Facebook (2016)", "NBS (2016)", "EOC (2018)", "GRID3 (2019)", "weighted average")
      
      # graph
      plot <- ggplot(x2, aes(x = date, y = pop, colour = admin1, fill = admin1,
        shape = pop_source, size = pop_source, alpha = pop_source)) + 
        geom_point() +
        geom_line(size = 1, alpha = 0.4) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
        scale_y_continuous("estimated population", labels = scales::comma) + 
        guides(fill = FALSE, colour = FALSE) +
        theme_bw() + 
        theme(axis.text = element_text(size = 10, colour = "grey20"), axis.title = element_text(size = 10, colour = "grey20")) +
        theme(legend.title = element_text(size = 10), axis.text.x = element_text(size = 9, angle = 30, hjust = 1, vjust = 1)) +
        theme(legend.position = "bottom") +
        scale_size_manual(name = "source:", values = c(2, 2, 2, 2, 3), labels = plot_labs ) +
        scale_shape_manual(name = "source:", values = c(0, 2, 3, 5, 16), labels = plot_labs) +
        scale_colour_manual(values = palette_cb[c(4, 6, 7)]) +
        scale_fill_manual(values = palette_cb[c(4, 6, 7)]) +
        scale_alpha_manual(name = "source:", values = c(0.7, 0.7, 0.7, 0.7, 1), labels = plot_labs ) +
        facet_wrap(~admin1, scales = "free_y", nrow = 3) +
        theme(strip.text = element_text(size = 9, colour = "grey20"))
      
      plot
      ggsave(paste(country, "_pop_trends_by_admin1.png", sep = ""), height = 20, width = 22, units = "cm", dpi = "print")
      
    # countrywide graph
      # aggregate by country
      x2 <- aggregate(x1[, "pop"], by = x1[, c("y", "m", "pop_source")], FUN = sum)
      x2 <- subset(x2, y <= y_analysis_end)
      colnames(x2) <- c("y", "m", "pop_source", "pop")
      x2[, "date"] <- as.Date.character(paste(x2[, "y"], x2[, "m"], "01", sep="-"))
 
      # graph
      plot <- ggplot(x2, aes(x = date, y = pop, shape = pop_source, size = pop_source, alpha = pop_source)) + 
        geom_point(colour = palette_cb[1]) +
        geom_line(colour = palette_cb[1], size = 1) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
        scale_y_continuous("estimated population", labels = scales::comma) + 
        theme_bw() + 
        theme(axis.text = element_text(size = 10, colour = "grey20"), axis.title = element_text(size = 10, colour = "grey20")) +
        theme(legend.title = element_text(size = 10), axis.text.x = element_text(size = 9, angle = 30, hjust = 1, vjust = 1)) +
        theme(legend.position = "bottom") +
        scale_size_manual(name = "source:", values = c(2, 2, 2, 2, 4), labels = plot_labs ) +
        scale_shape_manual(name = "source:", values = c(0, 2, 3, 5, 16), labels = plot_labs ) +
        scale_alpha_manual(name = "source:", values = c(0.7, 0.7, 0.7, 0.7, 1), labels = plot_labs )
      
      plot
      ggsave(paste(country, "_pop_trends_overall.png", sep = ""), height = 15, width = 20, units = "cm", dpi = "print")
      

  #.........................................
  ## Graph IDP numbers (only average)
    
    # Prepare data
    x1 <- pop[, c("admin1", "stratum", "m", "y", "idp_average")]
    x1 <- subset(x1, y <= y_analysis_end)
    
    # Graph by admin1
      # aggregate by admin1
      x3 <- aggregate(x1[, "idp_average"], by = x1[, c("admin1", "y", "m")], FUN = sum)
      colnames(x3) <- c("admin1", "y", "m", "idp_average")
      x3[, "date"] <- as.Date.character(paste(x3[, "y"], x3[, "m"], "01", sep="-"))
    
      # graph
      plot <- ggplot(x3, aes(x = date, y = idp_average, fill = admin1, colour = admin1) ) + 
        geom_histogram(stat = "identity", alpha = 0.5) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
        scale_y_continuous("Number of IDPs", labels = scales::comma) + 
        theme_bw() + 
        theme(plot.margin = unit(c(0,1,0,0), "cm") ) +
        theme(axis.text = element_text(size=10, colour="grey20"), axis.title = element_text(size = 10, colour = "grey20")) +
        scale_fill_manual(values = palette_cb[c(4,6,7)]) +
        scale_colour_manual(values = palette_cb[c(4,6,7)]) +
        theme(legend.title = element_text(size = 10)) +
        theme(legend.position = "bottom") +
        facet_wrap(~admin1, scales = "free_y", ncol = 1) +
        theme(strip.placement = "outside",
          strip.background = element_rect(fill = "grey80", colour = "grey50"),
          panel.spacing=unit(0.2, "cm"), strip.text.y = element_text(angle = 0), strip.text=element_text(size = 9, colour = "grey20"))
      
      plot
      ggsave(paste(country, "_pop_idp_total_by_admin1.png", sep = ""), height = 15, width = 25, units = "cm", dpi = "print")
            
          
  #.........................................
  ## Graph IDP proportions (only average)
    
    # Prepare data
    x1 <- pop[, c("admin1", "stratum", "m", "y", "idp_average", "pop_average")]
    x1 <- subset(x1, y <= y_analysis_end)
    
    # Graph by admin1
      # aggregate by admin1
      x3 <- aggregate(x1[, c("idp_average", "pop_average")], by = x1[, c("admin1", "y", "m")], FUN = sum)
      x3[, "prop_idp"] <- x3[, "idp_average"] / x3[, "pop_average"]
      x3[, "date"] <- as.Date.character(paste(x3[, "y"], x3[, "m"], "01", sep="-"))
    
      # graph
      plot <- ggplot(x3, aes(x = date, y = prop_idp, group = admin1, colour = admin1) ) + 
        geom_line(alpha = 0.3, size = 2) +
        geom_point(alpha = 0.5, size = 3) +
        scale_x_date("", date_labels = "%b %Y", breaks = "6 months", expand = c(0,0) ) +
        scale_y_continuous("IDP percentage among population", labels = scales::percent, expand = c(0,0), limits = c(0, 0.50)) + 
        scale_colour_manual(values = palette_cb[c(4,6,7)]) +
        theme_bw() + 
        theme(axis.text = element_text(size=10, colour="grey20"), axis.title = element_text(size=10, colour="grey20"),
          axis.text.x = element_text(angle = 30, hjust = 1) ) +
        theme(legend.title = element_text(size=10)) +
        theme(legend.position = "bottom") +
        labs(colour = "state:")
     
      plot
      ggsave(paste(country, "_pop_idp_prop_by_admin1.png", sep = ""), height = 13, width = 20, units = "cm", dpi = "print")
      
  
#.........................................................................................
### ENDS
#.........................................................................................
  
  