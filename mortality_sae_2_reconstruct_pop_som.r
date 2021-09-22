#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## ------------- R CODE TO RECONSTRUCT POPULATION DENOMINATORS IN SOMALIA -------------- ##
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
  stratum_names_pop <- sort(stratum_names)
    
  #.........................................
   ## Population source metadata
    
    # Keep only population sources (not IDP or refugee datasets) and relevant variables
    pop_sources <- pop_sources[grep("pop_", pop_sources$worksheet), ]
    pop_sources <- pop_sources[, c("worksheet", "quality_score", "m", "y")]
    colnames(pop_sources)[colnames(pop_sources) == "worksheet"] <- "pop_source"
    
    # Figure out tm point
    pop_sources <- merge(pop_sources, t_units, by=c("y", "m"), all.x=TRUE)

  #.........................................
  ## Population source datasets
    
  for (i in c(pop_sources[, "pop_source"]) ) {
    
    # Get dataset
    x1 <- get(i)
    
    # Make sure all strata are featured in the datasets
    x1 <- merge(x1, strata, by = "stratum", all.y = TRUE, sort = TRUE)

    # Sort each dataset and re-assign
    x1[, "stratum"] <- as.character(x1[, "stratum"])
    assign(i, x1[order(x1[, "stratum"]), ])
  }

  #.........................................
  ## Set assumed growth rate per month
  g <- (demog_pars[which(demog_pars$parameter == "assumed_cbr"), "value"] - 
    demog_pars[which(demog_pars$parameter == "assumed_cdr"), "value"] ) / (1000 * 12)

 
#..........................................................................................    
### Preparing IDP and refugee datasets
#..........................................................................................

  #.........................................
  ## UNPESS IDP proportions dataset
  idp_unpess_prop <- idp_unpess_prop[order(idp_unpess_prop[, "stratum"]), ]
  idp_unpess_prop[, "prop_idps"] <- as.numeric(idp_unpess_prop[, "prop_idps"])

        
  #.........................................
  ## PRMN dataset
    
    # Figure out time units
    idp_prmn <- merge(idp_prmn, t_units, by = c("y", "m"))
 
    # Aggregate dataset, irrespective of reason for displacement
    idp_prmn <- aggregate(idp_prmn[, "idps"], by = idp_prmn[, c("stratum_destination", "stratum_origin", "tm")], FUN = sum)
    colnames(idp_prmn) <- c("stratum_destination", "stratum_origin", "tm", "idps")
        
    # Reshape wide into a matrix
      # first expand so as to make sure that all combinations of districts and time points are featured
      x1 <- expand.grid(stratum_names_pop, stratum_names_pop, tm)
      colnames(x1) <- c("stratum_destination", "stratum_origin", "tm")
      idp_matrix <- merge(x1, idp_prmn, by = c("stratum_destination", "stratum_origin", "tm"), all.x = TRUE, sort = TRUE)
        # set all NA values to 0
        idp_matrix[is.na(idp_matrix)] <- 0
        
      # then reshape wide so as to form a matrix of stratum by stratum, with tm as a further dimension
      out <- data.frame()
        
      for (i in 1:max(tm)) {
        # select data for one time unit
        x1 <- subset(idp_matrix, tm == i)
        x1 <- subset(x1, select = -tm)
        
        # sort
        x1 <- x1[order(x1[, "stratum_destination"], x1[, "stratum_origin"]), ]
        
        # reshape wide
        x1 <- reshape(x1, idvar = "stratum_destination", timevar = "stratum_origin", direction = "wide")
        colnames(x1)[2:ncol(x1)] <- substring(colnames(x1)[2:ncol(x1)], 6)
        
        # flip so that origin is rows, destination columns
        x1 <- t(x1)
        
        # remove first row and use it to rename columns
        colnames(x1) <- x1[1, ]
        x1 <- x1[-1, ]
        
        # bind to output
        out <- rbind(out, x1)
        rm(x1)
        
      }
      
      # prepare matrix
      idp_matrix <- out
      rm(out)
      idp_matrix <- apply(idp_matrix, c(1,2), as.numeric)
#place2#
#place3# 
  #.........................................
  ## Refugee dataset
    # Prepare data
      # figure out time units
      ref_flow_source <- merge(refugees, t_units, by = c("y", "m"), sort = TRUE)
      
      # remove unknown stratum values
      ref_flow_source <- subset(ref_flow_source, stratum!= "other")
      
      # aggregate
      ref_flow_source <- aggregate(ref_flow_source[, "net_ref"], by = ref_flow_source[, c("stratum", "tm")], FUN = sum)
      colnames(ref_flow_source) <- c("stratum", "tm", "net_ref")
      
      # make sure all strata and time units are featured
      ref_flow_source <- merge(ref_flow_source, ts[, c("stratum", "tm")], by = c("stratum", "tm"), all.y = TRUE, sort = TRUE)
      
      # reshape wide
      ref_flow_source <- reshape(ref_flow_source, idvar = "stratum", timevar = "tm", direction = "wide")
      colnames(ref_flow_source)[2:ncol(ref_flow_source)] <- substring(colnames(ref_flow_source)[2:ncol(ref_flow_source)], 9)
    
    # Prepare matrix
    ref_flow_source <- ref_flow_source[order(ref_flow_source[, "stratum"]), ]
    ref_flow_source <- subset(ref_flow_source, select = -stratum)
      
      # all NAs to 0
      ref_flow_source[is.na(ref_flow_source)] <- 0
      ref_flow_source <- data.matrix(ref_flow_source)

#place4#
#..........................................................................................    
### Preparing population compartment and flow matrices
#..........................................................................................

  #.........................................
  ## Population compartments
  
    # Make sure data are numeric
    for (i in c(pop_sources[, "pop_source"]) ) {
      x1 <- get(i)
      x1 <- as.numeric(x1[, "pop"])   
      assign(i, x1)
    }
      
    # Total population for each alternative population source
    pop <- data.frame() 

    # Proportion of internally displaced persons and returnees within each stratum - estimated
    prop_idp_in <- matrix(NA, nrow = length(stratum_names_pop), ncol = length(tm))
    rownames(prop_idp_in) <- stratum_names
    colnames(prop_idp_in) <-  tm
    
  #.........................................
  ## Population flows
    
    # Refugee net flow
    ref_flow <- matrix(ref_flow_source, nrow = length(stratum_names_pop), ncol = length(tm))
    rownames(ref_flow) <- stratum_names_pop
    colnames(ref_flow) <-  tm

    # IDP and returnee flows
      # total movement (rows = from, cols = to)
      flow <- array(NA, dim = c(length(stratum_names_pop), length(stratum_names_pop), length(tm)),
        dimnames = list(stratum_names_pop, stratum_names_pop, tm))
      # pass data into array
      for (i in 1:max(tm)) {
        flow[,,i] <- idp_matrix[(1 + length(stratum_names_pop) * (i-1)):(length(stratum_names_pop) * i), ]
      }

#place5#

#..........................................................................................    
### Estimating total population across the time series, for each alternative population source
#..........................................................................................
      
for (i in 1:nrow(pop_sources)) {
  
  #.........................................
  ## Work out specifics of the population source
  pop_source <- get(paste(pop_sources[i, "pop_source"]))
  tm_pop_source <- pop_sources[i, "tm"]
    # Temporary matrix to hold estimated figures for this population source
    x1 <- as.data.frame(matrix(nrow = length(stratum_names_pop), ncol = length(tm)))
  
  #.........................................
  ## Forward calculation from known time point
    # at the start of forward calculation, population figures come from the known source
    x1[, tm_pop_source] <- pop_source
    # forward loop
    for (j in (tm_pop_source + 1) : max(tm)) {
      if (tm_pop_source >= max(tm)) break;
      x1[, j] <- (1 + g) * x1[, j-1] + rowSums(t(flow[, , j-1])) - rowSums(flow[, , j-1]) + ref_flow[, j-1]
    }
    
  #.........................................
  ## Back-calculation from known time point
    # at the start of back-calculation, population figures come from the known source
    x1[, tm_pop_source] <- pop_source
    # backward loop  
    for (j in (tm_pop_source - 1) : min(tm)) {
      x1[, j] <- ( x1[, j+1] - rowSums(t(flow[, , j])) + rowSums(flow[, , j]) - ref_flow[, j] ) / (1 + g)
    }
    
  #.........................................
  ## Pass results into main population data frame 
    
    # add stratum information
    x1 <- cbind(as.character(stratum_names_pop), x1)
    colnames(x1) <- c("stratum", paste("tm", tm, sep = ""))
    
    # reshape long
    x1 <- reshape(data = x1, idvar = "stratum", timevar = "tm", v.names = "pop", 
      varying = list(names(x1)[2:73]), direction = "long")
      
    # add population source
    x1[, "pop_source"] <- pop_sources[i, "pop_source"]
    
    # row bind to output
    pop <- rbind(x1, pop)
      
}

  #.........................................
  ## Come up with weighted population average, by combining all sources and their relative quality weights
    
    # For any source, set to 0 any value that is a negative number (happens for a handful of strata)
    pop[, "pop"] <- ifelse(pop[, "pop"] < 0, 0, pop[, "pop"])
  
    # Transform into integer figures
    pop[, "pop"] <- round(pop[, "pop"], digits = 0)
      
    # Reshape wide so as to have one source per column
    pop <- reshape(pop, idvar = c("stratum", "tm"), timevar = "pop_source", direction = "wide")
  
    # Weights (quality scores)
    pop_wts <- rev(as.vector(pop_sources[, "quality_score"])) # reversed because for some reason the order of pop sources has changed
  
    # Weighted mean
    pop[, "pop_average"] <- round(apply(pop[, ! colnames(pop) %in% c("tm", "stratum")], 1, 
      function(x, pop_wts) {return(weighted.mean(x, pop_wts, na.rm = TRUE))}, pop_wts ) , digits = 0)
    
    # Improve column names
    colnames(pop) <- c("stratum", "tm", rev(pop_sources$pop_source) , "pop_average")
   
  #.........................................
  ## Add under 5 population
  pop[, paste(colnames(pop)[! colnames(pop) %in% c("stratum", "tm")], "_u5", sep = "") ] <-  round(
    pop[, colnames(pop)[! colnames(pop) %in% c("stratum", "tm")] ] * 
      demog_pars[which(demog_pars$parameter == "u5_prop"), "value"] , digits = 0)
  


#..........................................................................................    
### Computing IDP proportions and departures, based on UNPESS (baseline) and PRMN figures
#..........................................................................................

    # prepare output
    prop_idp_in_all <- data.frame()    
  
  #.........................................
  ## Reconstruct IDP proportions per stratum, over time
  for (i in 1:nrow(pop_sources)) {
  
    # Work out specifics of the population source
    pop_loop <- pop[, c("stratum", "tm", pop_sources[i, "pop_source"]) ]
      # reshape wide  
      pop_loop <- reshape(pop_loop, idvar = "stratum", timevar = "tm", direction = "wide")
      pop_loop <- subset(pop_loop, select = -stratum)
    
    # Reset IDP matrix
    prop_idp_in <- matrix(NA, nrow = length(stratum_names_pop), ncol = length(tm))
    rownames(prop_idp_in) <- stratum_names_pop
    colnames(prop_idp_in) <-  tm
    
    # Supply starting conditions at Jan 2016
    prop_idp_in[, 37] <- idp_unpess_prop[, 2]
    
    # Forward calculate IDP compartments from Jan 2016 to the end of the period
    for (k in 38:max(tm)) {
      prop_idp_in[, k] <- prop_idp_in[, k-1] + colSums(flow[, , k-1]) / pop_loop[, k-1] - 
        (rowSums(flow[, , k-1]) / pop_loop[, k-1]) * prop_idp_in[, k-1]
    }
    
    # Pass results into data frame containing all alternative IDP estimates
      # add stratum information
      prop_idp_in <- as.data.frame(prop_idp_in)
      prop_idp_in <- cbind(as.character(stratum_names_pop), prop_idp_in)
      colnames(prop_idp_in) <- c("stratum", paste("tm", tm, sep = ""))
      
      # reshape long
      prop_idp_in <- reshape(data = prop_idp_in, idvar = "stratum", timevar = "tm", 
        v.names = "prop_idp", varying = list(names(prop_idp_in)[2:73]), direction = "long")
      
      # add population source
      prop_idp_in[, "pop_source"] <- pop_sources[i, "pop_source"]
      
      # row bind to output
      prop_idp_in_all <- rbind(prop_idp_in, prop_idp_in_all)
    
  }
  
  
  #.........................................
  ## Come up with weighted IDP average, by combining all sources and their relative quality weights
    
    # For any source, set to NA any stratum time series that are not in 0-1 range
    x1 <- unique(subset(prop_idp_in_all, is.na(prop_idp) == FALSE & (prop_idp < 0 | prop_idp > 1))[, c("stratum", "pop_source")])
    for (i in 1:nrow(x1) ){
      prop_idp_in_all[prop_idp_in_all$stratum %in% x1[i, "stratum"] & prop_idp_in_all$pop_source %in% x1[i, "pop_source"], "prop_idp"] <- NA
    }
  
    # Reshape wide so as to have one source per column
    prop_idp_in_all <- reshape(prop_idp_in_all, idvar = c("stratum", "tm"), timevar = "pop_source", direction = "wide")
    
    # Weights (quality scores)
    prop_idp_in_all_wts <- rev(as.vector(pop_sources[, "quality_score"])) # reversed because for some reason the order of pop sources has changed
    
    # Weighted mean
    prop_idp_in_all[, "prop_idp_average"] <- apply(prop_idp_in_all[, ! colnames(prop_idp_in_all) %in% c("tm", "stratum")], 1, 
      function(x, prop_idp_in_all_wts) {return(weighted.mean(x, prop_idp_in_all_wts, na.rm = TRUE))} )
    
      # replace NaN with NA
      prop_idp_in_all[, "prop_idp_average"] <- ifelse(is.nan(prop_idp_in_all[, "prop_idp_average"]), NA, 
        prop_idp_in_all[, "prop_idp_average"])
  
      
  #.........................................
  ## Number of new IDPs per time unit, by stratum of origin, based on PRMN figures
    # Aggregate
    idp_lv <- aggregate(idp_prmn[, "idps"], by = idp_prmn[, c("stratum_origin", "tm")], FUN = sum )
    colnames(idp_lv) <- c("stratum", "tm", "idp_lv")
    
    # Merge with overall time series
    idp_lv <- merge(ts, idp_lv, by=c("stratum", "tm"), all = TRUE, sort = TRUE)
    
    # Restrict date range to that available
    idp_lv <- subset(idp_lv, y > 2015)
    
    # Assume that if IDP = NA, it means zero
    idp_lv[, "idp_lv"] <- ifelse(is.na(idp_lv[, "idp_lv"]) == TRUE, 0, idp_lv[, "idp_lv"]  )
#place6#    
#place7# 
#..........................................................................................    
### Writing output and generating graphs
#..........................................................................................

  #.........................................
  ## Merge IDP proportions, IDPs leaving and population estimates
  pop <- merge(pop, prop_idp_in_all, by = c("stratum", "tm"), all = TRUE, sort = TRUE)
  pop <- merge(pop, idp_lv, by = c("stratum", "tm"), all = TRUE, sort = TRUE)
  
  #.........................................
  ## Compute IDP proportion
  colnames(pop)[colnames(pop) == "prop_idp_average"] <- "prop_idp"

  #.........................................
  ## Compute IDP flow rates
          
    # Departure rate
    pop[, "dep_rate"] <- pop[, "idp_lv"] * 10000 / pop[, "pop_average"]

    
  #.........................................
  ## Write population estimates
  pop <- pop[, ! colnames(pop) %in% c("admin1", "m", "y")]
  pop <- merge(pop, strata, by = "stratum", all.x = TRUE)
  pop <- merge(pop, t_units, by = "tm", all.x = TRUE)
    
  x1 <- c("stratum", "admin1", "tm", "y", "m", paste(pop_sources$pop_source), grep("average", colnames(pop), value = TRUE), 
    "prop_idp", grep("rate", colnames(pop), value = TRUE) )
#place8#  
  write.csv(pop[,  x1], paste(country, "_pop_denoms.csv", sep=""), row.names=FALSE)
    
  #.........................................
  ## Graph population estimates

    # Graph by admin1  
      x1 <- pop
    
      # aggregate by admin1
      x2 <- aggregate(x1[, pop_sources$pop_source], by = x1[, c("admin1", "tm")], sum)

      # recalculate weighted average
      pop_wts <- pop_sources$quality_score
      x3 <- apply(x2[, 3:6], 1, function(x, pop_wts) {weighted.mean(x, pop_wts, na.rm = TRUE)} , pop_wts )
      x2[, "pop_average"] <- x3
      # reshape long
      x3 <- melt(x2, id.vars = c("admin1", "tm"), variable.name = "pop_source", value.name = "pop")
      
      # add dates
      x3 <- merge(x3, t_units, by ="tm")
      x3[, "date"] <- as.Date.character(paste(x3[, "y"], x3[, "m"], "01", sep = "-"))
    
      # graph
      plot <- ggplot(x3, aes(x = date, y = pop, linetype = pop_source, colour = pop_source, size = pop_source)) + 
        geom_line(alpha = 0.7) +
        scale_x_date("year", date_labels = "%Y", breaks = "12 months", expand = c(0,0)) +
        scale_y_continuous("population", labels = scales::comma) + 
        theme_bw() + 
        theme(axis.text = element_text(size = 9, colour = "grey20"), 
          axis.title = element_text(size = 9, colour = "grey20"),
          legend.title = element_text(size = 10),
          legend.position = "bottom") +
        scale_linetype_manual(name="Source:", values = c("solid", "solid", "solid", "solid", "solid"), 
          labels=c("UNPESS (2014)", "AfriPop (2015)","WHO EPI (2018)", "WHO Polio (2018)", "weighted average") ) +
        scale_size_manual(name = "Source:", values = c(1,1,1,1,2),
          labels = c("UNPESS (2014)", "AfriPop (2015)", "WHO EPI (2018)", "WHO Polio (2018)", "weighted average")) +
        scale_colour_manual(name="Source:", values=c("maroon", "forestgreen", "royalblue4", "red1", "grey40"),
          labels = c("UNPESS (2014)", "AfriPop (2015)", "WHO EPI (2018)", "WHO Polio (2018)", "weighted average")) +
        facet_wrap(~admin1, scales = "free", switch = "x", ncol = 4) +
        theme(strip.placement = "outside",
          strip.background = element_rect(fill = NA, colour = "grey50"),
          panel.spacing = unit(0.2,"cm"), strip.text.y = element_text(angle = 0), 
          strip.text = element_text(size = 9, colour = "grey20"))
      
      plot      
      ggsave(paste(country, "_pop_trends_admin1.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")

    # Countrywide graph
      # aggregate
      x2 <- aggregate(x1[, pop_sources$pop_source], by = list("tm" = x1[, "tm"]), sum)
      
      # reshape long
      x3 <- melt(x2, id.vars = c("tm"), variable.name = "pop_source", value.name = "pop")
      
      # add dates
      x3 <- merge(x3, t_units, by = "tm")
      x3[, "date"] <- as.Date.character(paste(x3[, "y"], x3[, "m"], "01", sep = "-"))
      
      # graph
      plot <- ggplot(x3, aes(x = date, y = pop, linetype = pop_source, colour = pop_source, size = pop_source)) + 
      geom_line(alpha = 0.7) +
      scale_x_date(date_labels = "%b %Y", breaks = "6 months", expand = c(0,0)) +
      scale_y_continuous("population", breaks = seq(10000000, 16000000, by = 1000000), labels = scales::comma) + 
      theme_bw() + coord_cartesian(ylim = c(10000000, 16000000)) +
      theme(axis.text = element_text(size = 11, colour = "grey20"), 
        axis.title = element_text(size = 11, colour = "grey20"),
        legend.title = element_text(size = 11), 
        legend.text = element_text(size = 11)) +
      scale_linetype_manual(name = "Source:", values = c("solid", "solid", "solid", "solid", "solid"), 
        labels = c("UNPESS (2014)", "AfriPop (2015)", "WHO EPI (2018)", "WHO Polio (2018)", "weighted average")) +
      scale_size_manual(name = "Source:", values = c(1,1,1,1,2),
        labels = c("UNPESS (2014)", "AfriPop (2015)", "WHO EPI (2018)", "WHO Polio (2018)", "weighted average")) +
      scale_colour_manual(name = "Source:", values = c("maroon", "forestgreen", "royalblue4", "red1", "grey40"),
        labels = c("UNPESS (2014)", "AfriPop (2015)", "WHO EPI (2018)", "WHO Polio (2018)", "weighted average"))
      
      plot
      ggsave(paste(country, "_pop_trends_country.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")

    
  #.........................................
  ## Graph IDP proportions (only average)

    # Graph by admin1
      x1 <- pop
      
      # aggregate by admin1
      x2 <- aggregate(x1[, "prop_idp"], by = x1[, c("admin1", "tm")], mean)
      colnames(x2)[3] <- "prop_idp"
      x2 <- merge(x2, t_units, by = "tm")
      x2[, "date"] <- as.Date.character(paste(x2[, "y"], x2[, "m"], "01", sep = "-"))
      
      # graph
      plot <- ggplot(x2, aes(x = date, y = prop_idp)) + 
        geom_bar(stat = "identity", fill = "orangered3", alpha = 0.5) +
        scale_x_date("year", limits = c(as.Date("2016-01-01"), as.Date("2018-12-01")), date_labels = "%Y", 
           breaks = "12 months", expand = c(0,0)) +
        scale_y_continuous("proportion of IDPs and returnees", labels = scales::percent) + 
        theme_bw() + 
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
        theme(axis.text = element_text(size = 9, colour = "grey20"), 
          axis.title = element_text(size = 10, colour = "grey20"),
          legend.position="none") +
        facet_wrap(~admin1, scales = "free", switch = "x", ncol = 4) +
        theme(strip.placement = "outside",
          strip.background = element_rect(fill = NA, colour = "grey50"),
          panel.spacing = unit(0.2,"cm"), strip.text.y = element_text(angle = 0), 
          strip.text = element_text(size = 9, colour = "grey20"))
      
      plot      
      ggsave(paste(country, "_idp_trends_admin1.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")
    
    # countrywide graph
      # aggregate by country
      x3 <- aggregate(x1[, "prop_idp"], by = list("tm" = x1[, "tm"]), mean)
      colnames(x3)[2] <- "prop_idp"
      x3 <- merge(x3, t_units, by = "tm")
      x3[, "date"] <- as.Date.character(paste(x3[, "y"], x3[, "m"], "01", sep = "-"))    
      
      # graph
      plot <- ggplot(x3, aes(x = date, y = prop_idp)) + 
        geom_bar(stat = "identity", fill = "orangered3", alpha = 0.5) +
        scale_x_date(limits = c(as.Date("2016-01-01"), as.Date("2018-12-01")), date_labels = "%b %Y", 
          breaks = "6 months", expand = c(0,0)) +
        scale_y_continuous("proportion of IDPs and returnees", labels = scales::percent) +
        theme_bw() + 
        coord_cartesian(ylim = c(0, 0.3)) + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
        theme(axis.text = element_text(size = 11, colour = "grey20"), 
          axis.title = element_text(size = 11, colour = "grey20"),
          legend.title = element_text(size = 10))
      
      plot
      ggsave(paste(country, "_idp_trends_country.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")
    
  
#.........................................................................................
### ENDS
#.........................................................................................
  
  