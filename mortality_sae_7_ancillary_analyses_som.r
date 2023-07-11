#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## --------------------- R CODE FOR ANCILLARY ANALYSES IN SOMALIA ---------------------- ##
#..........................................................................................

                                        # Written by Francesco Checchi, LSHTM (December 2019)
                                        # francesco.checchi@lshtm.ac.uk 



#..........................................................................................    
### Preparatory steps
#..........................................................................................    

  #...................................
  ## Source necessary R scripts     
    
    # Set working directory to where this file is stored
    current_path = rstudioapi::getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )

    # Read functions
    source("mortality_sae_0_functions.r")
  
    # Source control script
    script_0 <- scan("mortality_sae_0_control_code.r", what=character(), sep="\n", quiet=TRUE)
      # Read in all files and parameters...    
      f_source_part(script_0, "#place1#", "#place6#")

  #...................................
  ## Reading in additional files
    # Terms of trade (2006-2012)
    tot_2006 <- read_excel("som_tot_2006-2012.xlsx", sheet = "Sheet1")
      #get rid of tibble
      tot_2006 <- data.frame(tot_2006)
    
    # Historical rainfall departures
    rain_spi <- read_excel("som_predictor_data.xlsx", sheet = "rainfall_historical")
      #get rid of tibble
      rain_spi <- data.frame(rain_spi)
        
    # Predictor time series dataset
    ts <- read.csv(paste(country, "_ts.csv", sep=""))
      # Rename administrative units
      colnames(ts)[colnames(ts) == "stratum"] <- admin2_name
      colnames(ts)[colnames(ts) == "admin1"] <- admin1_name

    # Correlation between excess death rate and IPC (2017-2018) 
      ipc <- read_excel("som_correlation_excess_dr_ipc.xlsx")
        #get rid of tibble
        ipc <- data.frame(ipc)
     
    # Estimated death rates by stratum
    est_stratum <- read.csv(paste(country, "_out_est_by_stratum.csv", sep=""), sep=",") 
    
    # Shape files for districts and regions
    dir_maps <- paste(getwd( ), "/mapping", sep = "")
    adm2 <- sf::st_read(paste(dir_maps, "/Som_Admbnda_Adm2_UNDP.shp", sep = ""))
    adm1 <- sf::st_read(paste(dir_maps, "/Som_Admbnda_Adm1_UNDP.shp", sep = ""))
    

#.........................................................................................                            
### Correlation between excess death rate and proportion of people in IPC phases 3-5 (2017-2018)
#.........................................................................................  
        
  #...................................  
  ## Prepare dataset
  x1 <- ipc[, c("Region", "Total.in.Crisis.and.Emergency.as....of.Total.population", "cdr.ex.likely.est")]
  colnames(x1) <- c("region", "prop_phase345", "excess_dr")      
  x1[, "nudge_x"] <- 0
  x1[, "nudge_y"] <- 0.005
  x1[x1$region == "Woqooyi Galbeed", c("nudge_x", "nudge_y")] <- c(0.01, -0.005)
  x1[x1$region == "Lower Shabelle", c("nudge_x", "nudge_y")] <- c(-0.015, -0.005)
  x1[x1$region == "Lower Juba", c("nudge_x", "nudge_y")] <- c(0.015, 0.005)
  x1[x1$region == "Togdheer", c("nudge_x", "nudge_y")] <- c(-0.015, -0.005)
  x1[x1$region == "Galgaduud", c("nudge_x", "nudge_y")] <- c(0, -0.005)
  
  #...................................  
  ## Graph
  plot <- ggplot(data = x1, mapping = aes(x = prop_phase345, y = excess_dr, label = region)) +
    geom_point(colour = palette_cb[7], fill = palette_cb[7], size = 5, alpha = 0.5) +
    scale_y_continuous(name = "excess death rate (all ages, per 10,000 person-days), 2017-2018",
      breaks = seq(-0.02, 0.12, by = 0.02), limits = c(-0.025, 0.125)) +
    scale_x_continuous(name = "percentage of people in IPC phase 3, 4 or 5, early 2017",
      breaks = seq(0, 0.5, 0.05), labels = scales::percent) +
    geom_text(size = 4, nudge_y = x1$nudge_y, nudge_x = x1$nudge_x, colour = "grey20") +
    theme_bw() +
    geom_hline(yintercept = 0, colour = palette_cb[7], alpha = 0.5, size = 1) +
    geom_smooth(method = "lm", se = FALSE, colour = palette_cb[6], size = 1, linetype = "21") +
    theme(axis.text = element_text(colour = "grey20", size = 11),
      axis.title = element_text(colour = "grey20", size = 11) )
  
  plot                
  ggsave(paste(country, "_excess_dr_ipc_corr.png", sep = ""), height = 15, width = 25, units = "cm", dpi = "print")
  
     
#.........................................................................................                            
### Describing trends in terms of trade from 2010 to 2018
#.........................................................................................  
  
  #...................................  
  ## Prepare datasets
    # Prepare 2006-2012 data
      # rename variable names
      colnames(tot_2006) <- c("market", "region", "district", "y", "m", "tot_wage_cereal", "tot_goat_cereal", "market_wt")
      
      # aggregate market data to regional level
        # create variables for product of ToT and market weight
        tot_2006[, "tot_wage_cereal"] <- tot_2006[, "tot_wage_cereal"] * tot_2006[, "market_wt"]
        tot_2006[, "tot_goat_cereal"] <- tot_2006[, "tot_goat_cereal"] * tot_2006[, "market_wt"]
        
        # sum ToT*weight and market weights
        x1 <- aggregate(tot_2006[, c("tot_wage_cereal", "tot_goat_cereal", "market_wt")], by=list(tot_2006$region, tot_2006$y, tot_2006$m), FUN=sum )
        colnames(x1) <- c("region", "y", "m", "tot_wage_cereal", "tot_goat_cereal", "market_wt")
        
        # then divide ToT by weights to get weighted mean
        x1[, "tot_wage_cereal"] <- x1[, "tot_wage_cereal"] / x1[, "market_wt"]
        x1[, "tot_goat_cereal"] <- x1[, "tot_goat_cereal"] / x1[, "market_wt"]
        
        # clean up
        tot_2006 <- x1
        tot_2006 <- subset(tot_2006, select = -market_wt)
        
    # Prepare 2013-2018 data
     # aggregate to regional level  
      tot_2013 <- aggregate(ts[, c("tot_wage_cereal_smooth", "tot_goat_cereal_smooth")], by = list(ts$region, ts$y, ts$m), 
        FUN = mean )
      colnames(tot_2013) <- c("region", "y", "m", "tot_wage_cereal", "tot_goat_cereal")
      
     # check that regions have the same names
      setdiff(tot_2006$region, tot_2013$region)  
 
    # Append the two datasets
      tot <- rbind(tot_2006, tot_2013)
      # delete observations before 2007
      tot <- subset(tot, y != 2006)
  
  #...................................  
  ## Graph ToT trends by region
    # Create a date variable for the x axis
    tot[, "date"] <- dmy(paste("1", tot[, "m"], tot[, "y"], sep="/"))
          
      # create breaks for years
      year_breaks <- subset(tot, m == 1)[, "date"]

      # create scaling parameters
      tot_wage_max <- max(tot$tot_wage_cereal_smooth, na.rm = TRUE)
      tot_goat_max <- max(tot$tot_goat_cereal, na.rm = TRUE)
      
    # plot
      plot <- ggplot(tot, aes(x = date) ) +
      geom_point(mapping = aes(x = date, y = tot_wage_cereal), stat = "identity", colour = palette_cb[7], 
        size = 1, alpha = 0.3) +
      geom_line(mapping = aes(x = date, y = tot_wage_cereal), stat = "identity", colour = palette_cb[7],
        alpha = 0.5) +
      geom_point(mapping = aes(x = date, y = tot_goat_cereal*(tot_wage_max/tot_goat_max) ), colour = palette_cb[6], 
        size = 1, alpha = 0.3) +
      geom_line(mapping = aes(x = date, y = tot_goat_cereal*(tot_wage_max/tot_goat_max) ), colour = palette_cb[6],
        size = 1, alpha = 0.5) +
      scale_y_continuous("Terms of trade - Kcal cereal equivalent of daily wage", labels = function(x) format(x, big.mark = ",", scientific = FALSE),
        breaks = c(0, 10000, seq(20000, max(tot$tot_wage_cereal_smooth, na.rm = TRUE), by = 20000)),
        limits = c(0, 50000),
        sec.axis = sec_axis(~ (./(tot_wage_max/tot_goat_max)), labels = function(x) format(x, big.mark = ",", scientific = FALSE),
          breaks = seq(0, max(tot$tot_goat_cereal, na.rm = TRUE), by=100000), name="Terms of trade - Kcal cereal equivalent of medium goat")) +
      theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
      geom_vline(xintercept = year_breaks, color = "grey80") +
      geom_hline(yintercept = 10000, color = palette_cb[7], linetype = "21", alpha = 0.7) +
      facet_wrap(~region, ncol = 3, scales = "free_y" ) +
      scale_x_date("year", expand = c(0,0) , minor_breaks = NULL, date_breaks = "12 months", date_labels = "%Y") +
      theme(plot.title = element_text(color = "grey20"), legend.title = element_text(color = "grey20"),
            axis.title.x = element_text(color = "grey20"),
            axis.text.x = element_text(angle = 45, hjust = 1, size = 8, color = "grey20"),      
            axis.line.y = element_line(color = palette_cb[7]), axis.ticks.y = element_line(color = palette_cb[7]),
            axis.text.y = element_text(color = palette_cb[7]), axis.title.y = element_text(color=palette_cb[7], margin = margin(r = 10) ),
            axis.line.y.right = element_line(color = palette_cb[6]), axis.ticks.y.right = element_line(color = palette_cb[6]),
            axis.text.y.right = element_text(color = palette_cb[6]), axis.title.y.right = element_text(color=palette_cb[6], margin = margin(l = 10) ),
            strip.background = element_rect(fill = "white")
           )
          
    # call and save plot
    plot
    ggsave(paste(country, "_tot_trends.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")
    
  
#.........................................................................................                            
### Mapping estimated death rates
#.........................................................................................  

  #...................................      
  ## Prepare dataset
    # Add mortality estimates to shape file
    colnames(est_stratum)[colnames(est_stratum) == "stratum"] <- "admin2Name"
      # check that geographic names are the same
      setdiff(unique(est_stratum$admin2Name), unique(adm2$admin2Name))
    adm2 <- merge(adm2, est_stratum, by = "admin2Name")
    
  #...................................
  ## Graph CDR and save
  pos_dis <- rep(0, times <- length(adm2$admin2Name))
  names(pos_dis) <- adm2$admin2Name
  pos_dis[c("Ceerigaabo", "Qoryooley", "Bu'aale", "Xudur")] <- 0.5  
  pos_dis[c("Lughaye", "Baki", "Burco", "Cadale")] <- 0.3
  pos_dis[c("Jowhar", "Banadir", "Gebiley")] <- -0.3
    
  map_cdr <- tm_shape(adm2) + 
    tm_text(col = "black", "admin2Name", size = 0.5, ymod = pos_dis, alpha = 1) + 
    tm_borders(col = "grey60", alpha = 0.5) + 
    tm_fill("cdr_ac_est", alpha = 0.5, palette = "Reds",
      title = "crude death rate\n (per 10,000 person-days)") +
    tm_shape(adm1) + 
    tm_text("admin1Name", col = "darkred", size = 0.6, fontface = "bold", alpha = 1) +
    tm_borders("darkred", lwd = 2, alpha = 0.5) + 
    tm_layout(
      legend.width = 0.5,
      legend.title.size = 0.8, 
      legend.text.size =  0.7, 
      legend.text.color = "grey20", 
      legend.title.color = "grey20"
    )
  
  map_cdr
  tmap_save(map_cdr, paste(country, "_out_cdr_map_long.png", sep = ""), 
    height = 20, width = 15, units = "cm", dpi = 300)     

    
  #...................................
  ## Graph excess death rate and save
  pos_dis <- rep(0, times <- length(adm2$admin2Name))
  names(pos_dis) <- adm2$admin2Name
  pos_dis[c("Ceerigaabo", "Qoryooley", "Bu'aale", "Xudur")] <- 0.5  
  pos_dis[c("Lughaye", "Baki", "Burco", "Cadale")] <- 0.3
  pos_dis[c("Jowhar", "Banadir", "Gebiley")] <- -0.3
    
  map_xdr <- tm_shape(adm2) + 
    tm_text(col = "black", "admin2Name", size = 0.5, ymod = pos_dis, alpha = 1) + 
    tm_borders(col = "grey60", alpha = 0.5) + 
    tm_fill("cdr_ex_best_est", alpha = 0.5, palette = "Oranges",
      title = "excess death rate\n (per 10,000 person-days)") +
    tm_shape(adm1) + 
    tm_text("admin1Name", col = "darkred", size = 0.6, fontface = "bold", alpha = 1) +
    tm_borders("darkred", lwd = 2, alpha = 0.5) + 
    tm_layout(
      legend.width = 0.5,
      legend.title.size = 0.8, 
      legend.text.size =  0.7, 
      legend.text.color = "grey20", 
      legend.title.color = "grey20"
    )
  
  map_xdr
  tmap_save(map_xdr, paste(country, "_out_xdr_map_long.png", sep = ""), 
    height = 20, width = 15, units = "cm", dpi = 300)     

    
  #...................................
  ## Graph U5DR and save
  pos_dis <- rep(0, times <- length(adm2$admin2Name))
  names(pos_dis) <- adm2$admin2Name
  pos_dis[c("Ceerigaabo", "Qoryooley", "Bu'aale", "Xudur")] <- 0.5  
  pos_dis[c("Lughaye", "Baki", "Burco", "Cadale")] <- 0.3
  pos_dis[c("Jowhar", "Banadir", "Gebiley")] <- -0.3
  
  map_cdr <- tm_shape(adm2) + 
    tm_text(col = "black", "admin2Name", size = 0.5, ymod = pos_dis, alpha = 1) + 
    tm_borders(col = "grey60", alpha = 0.5) + 
    tm_fill("cdr_u5_ac_est", alpha = 0.5, palette = "Reds",
            title = "under 5y death rate\n (per 10,000 person-days)") +
    tm_shape(adm1) + 
    tm_text("admin1Name", col = "darkred", size = 0.6, fontface = "bold", alpha = 1) +
    tm_borders("darkred", lwd = 2, alpha = 0.5) + 
    tm_layout(
      legend.width = 0.5,
      legend.title.size = 0.8, 
      legend.text.size =  0.7, 
      legend.text.color = "grey20", 
      legend.title.color = "grey20"
    )
  
  map_cdr
  tmap_save(map_cdr, paste(country, "_out_cdr_u5_map_long.png", sep = ""), 
            height = 20, width = 15, units = "cm", dpi = 300)     
  
  
  #...................................
  ## Graph excess under 5y death rate and save
  pos_dis <- rep(0, times <- length(adm2$admin2Name))
  names(pos_dis) <- adm2$admin2Name
  pos_dis[c("Ceerigaabo", "Qoryooley", "Bu'aale", "Xudur")] <- 0.5  
  pos_dis[c("Lughaye", "Baki", "Burco", "Cadale")] <- 0.3
  pos_dis[c("Jowhar", "Banadir", "Gebiley")] <- -0.3
  
  map_xdr <- tm_shape(adm2) + 
    tm_text(col = "black", "admin2Name", size = 0.5, ymod = pos_dis, alpha = 1) + 
    tm_borders(col = "grey60", alpha = 0.5) + 
    tm_fill("cdr_u5_ex_best_est", alpha = 0.5, palette = "Oranges",
            title = "excess under 5y death rate\n (per 10,000 person-days)") +
    tm_shape(adm1) + 
    tm_text("admin1Name", col = "darkred", size = 0.6, fontface = "bold", alpha = 1) +
    tm_borders("darkred", lwd = 2, alpha = 0.5) + 
    tm_layout(
      legend.width = 0.5,
      legend.title.size = 0.8, 
      legend.text.size =  0.7, 
      legend.text.color = "grey20", 
      legend.title.color = "grey20"
    )
  
  map_xdr
  tmap_save(map_xdr, paste(country, "_out_xdr_u5_map_long.png", sep = ""), 
            height = 20, width = 15, units = "cm", dpi = 300)     
  
  
    
#.........................................................................................                            
### Exploring the relationships between different crisis indicators
#.........................................................................................  
    
  #...................................  
  ## Prepare data for analysis
 
    # PRMN displacement data
      # aggregate PRMN dataset by district of origin, date and reason for displacement
      idp_prmn <- aggregate(idp_prmn[, "idps"], 
        by = list(idp_prmn$stratum_origin, idp_prmn$y, idp_prmn$m, idp_prmn$reason), FUN=sum , na.rm = TRUE)
      colnames(idp_prmn) <- c("district", "y", "m", "reason", "idp_lv")
      
      # make sure all stratum-months are represented, year = 2016+ and that all NA values = 0
      x1 <- dcast(idp_prmn, district + y + m ~ reason, value.var = "idp_lv")
      x2 <- merge(ts[, c("region", "district", "y", "m")], x1, by = c("district", "y", "m"), all.x = TRUE)
      x2 <- subset(x2, y > 2015)
      x2[, c("drought", "flooding", "insecurity", "other")] <- 
        na.replace(x2[, c("drought", "flooding", "insecurity", "other")], 0)
      
      # aggregate to region
      x2 <- aggregate(x2[, c("drought", "flooding", "insecurity", "other")], 
        by = x2[, c("region", "y", "m")], FUN = sum, na.rm = TRUE)

      #remelt
      x2 <- melt(x2, id.vars = c("region", "y", "m"), variable.name = "reason", value.name = "idp_lv")      
      idp_prmn <- x2

    # Historical rainfall data
      # make sure all stratum-months are represented and year = 2016+
      x1 <- merge(ts[, c("region", "district", "y", "m")], rain_spi, by = c("district", "y", "m"), all.x = TRUE)
      x1 <- subset(x1, y > 2015)

      # aggregate to region
      x1 <- aggregate(x1[, "mean_spi"], by = x1[, c("region", "y", "m")], FUN = mean, na.rm = TRUE)
      colnames(x1)[4] <- "rainfall_spi"
      rain_spi <- x1
              
      # compute 3-month rolling means
      rain_spi <- rain_spi[order(rain_spi$region, rain_spi$y, rain_spi$m), ]
      x2 <- c()
      for (i in unique(rain_spi$region)) {
        x1 <- subset(rain_spi, region == i)
        x2 <- c(x2, rollmean(x1$rainfall_spi, k = 3, fill = NA, align = "right") )
      }
      rain_spi[, "rainfall_spi_rollmean"] <- x2      

      # create extra columns for rainfall values below historical median
      rain_spi[, "rainfall_spi_rollmean_neg"] <- ifelse(rain_spi[, "rainfall_spi_rollmean"] > 0, 
        0, rain_spi[, "rainfall_spi_rollmean"])

    # Overall predictor dataset
      # aggregate to regional level
      ts_aggr <- aggregate(ts[, c("rainfall_rollmean", "tot_wage_cereal_smooth", "prop_idp", "dep_rate", 
        "sam_admissions_rate", "measles_cases_rate")], by = list(ts$region, ts$y, ts$m), FUN = mean, na.rm = TRUE )
      colnames(ts_aggr)[1:3] <- c("region", "y", "m")
      
      # restrict date range to that available
      ts_aggr <- subset(ts_aggr, y > 2015)
      
  #...................................  
  ## Graph relationships
    # Preparatory steps  
      # create a date variable for the x axis
      rain_spi[, "date"] <- dmy(paste("1", rain_spi[, "m"], rain_spi[, "y"], sep="/"))
      ts_aggr[, "date"] <- dmy(paste("1", ts_aggr[, "m"], ts_aggr[, "y"], sep="/"))
      idp_prmn[, "date"] <- dmy(paste("1", idp_prmn[, "m"], idp_prmn[, "y"], sep="/"))
      
      # create breaks for years
      year_breaks <- subset(ts_aggr, m == 1)[, "date"]

      # create scaling parameters
      rain_min <- abs(min(range(rain_spi$rainfall_spi_rollmean, na.rm = TRUE)))
      rain_range <- sum(abs(range(rain_spi$rainfall_spi_rollmean, na.rm = TRUE)))
      
      tot_min <- abs(min(range(ts_aggr$tot_wage_cereal_smooth, na.rm = TRUE)))
      tot_range <- sum(abs(range(ts_aggr$tot_wage_cereal_smooth, na.rm = TRUE)))

      x1 <- aggregate(idp_prmn[, "idp_lv"], by = list(idp_prmn$region, idp_prmn$y, idp_prmn$m), FUN=sum , na.rm = TRUE)
      colnames(x1) <- c("region", "y", "m", "idp_lv")
      idp_lv_min <- abs(min(range(x1$idp_lv, na.rm = TRUE)))
      idp_lv_range <- sum(abs(range(x1$idp_lv, na.rm = TRUE)))
      
      dep_rate_min <- abs(min(range(ts_aggr$dep_rate, na.rm = TRUE)))
      dep_rate_range <- sum(abs(range(ts_aggr$dep_rate, na.rm = TRUE)))
      
      prop_idp_min <- abs(min(range(ts_aggr$prop_idp, na.rm = TRUE)))
      prop_idp_range <- sum(abs(range(ts_aggr$prop_idp, na.rm = TRUE)))
      
      sam_min <- abs(min(range(ts_aggr$sam_admissions_rate, na.rm = TRUE)))
      sam_range <- sum(abs(range(ts_aggr$sam_admissions_rate, na.rm = TRUE)))

      measles_min <- abs(min(range(ts_aggr$measles_cases_rate, na.rm = TRUE)))
      measles_range <- sum(abs(range(ts_aggr$measles_cases_rate, na.rm = TRUE)))
  
            
    # Rainfall vs. displacement
      # draw plot
      plot <- ggplot(data = idp_prmn) +
        geom_bar(aes(x = date, y = idp_lv, fill = reason), colour = "grey20", stat = "identity", 
          position = "stack", alpha = 0.7) +
        geom_line(data = rain_spi, aes(x = date, y = (rainfall_spi_rollmean + rain_min) * (100000/rain_range)), 
          colour = palette_cb[6], alpha = 0.7, size = 1) +
        geom_ribbon(data = rain_spi, aes(x = date, ymax = (0 + rain_min) * (100000/rain_range), 
          ymin = (rainfall_spi_rollmean_neg + rain_min) * (100000/rain_range)), 
          alpha = 0.2, fill = "red", outline.type = "full") +
        scale_y_continuous("People newly internally displaced from region", limits=c(0, 110000), 
          labels = function(x) format(x, big.mark = ",", scientific = FALSE),
          breaks = seq(0, 100000, by = 20000),
          sec.axis = sec_axis(~ (./(100000/rain_range)) - rain_min, name = "Rainfall standard deviations from the historical median (3-month moving average)", 
            breaks = seq(-1.5, 2.0, by=0.5) )
        ) +
        scale_fill_manual("Reason for displacement:", values = palette_cb[c(7, 3, 4, 1)]) +
        theme_bw() + 
        theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
        geom_vline(xintercept = year_breaks, color = "grey50") +
        geom_hline(yintercept = (0 + rain_min) * (100000/rain_range), color = palette_cb[6], linetype = "21") +
        facet_wrap(~region, ncol = 3) +
        scale_x_date("\nmonth - year", expand = c(0, 0) , minor_breaks = NULL, date_breaks = "3 months", 
          date_labels = "%b-%Y") +
        theme(
          axis.title.x = element_text(color = "grey20"),
          axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.line.y = element_line(color = "grey20"), 
          axis.ticks.y = element_line(color = "grey20"),
          axis.text.y = element_text(color = "grey20"), 
          axis.title.y = element_text(color="grey20", margin = margin(r = 10) ),
          axis.line.y.right = element_line(color = palette_cb[6]), 
          axis.ticks.y.right = element_line(color = palette_cb[6]),
          axis.text.y.right = element_text(color = palette_cb[6]), 
          axis.title.y.right = element_text(color = palette_cb[6], margin = margin(l = 10) ),
          strip.background = element_rect(fill = "white"),
          legend.position = "top",
          legend.text = element_text(color = "grey20", size = 9),
          legend.title = element_text(color = "grey20", size = 9)
        )
          
      # call and save plot
      plot    
      ggsave(paste(country, "_rainfall_idp_trends.png", sep = ""), height = 28, width = 22, units = "cm", dpi = "print")
     

#####NOTE: THE FOLLOWING GRAPHS HAVE NOT YET BEEN REVISED - MAY CONTAIN BUGS      
    # # Rainfall vs. ToT
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #         geom_line(mapping = aes(x = date, y = tot_wage_cereal_smooth), stat = "identity", colour = "indianred3") +
    #         geom_line(mapping = aes(x = date, y = (rainfall_rollmean + rain_min)*(dep_rate_range/rain_range) ), colour = "dodgerblue4") +
    #         scale_y_continuous("Terms of trade - Kcal cereal equivalent of daily wage", labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    #           breaks = seq(0, 100000, by = 20000), 
    #           sec.axis = sec_axis(~ (./(dep_rate_range/rain_range))-rain_min, breaks = seq(-1.5, 2.0, by=0.5), name="Rainfall - departure from the historical median")) +
    #         theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #         labs(x = "\nmonth", y = ts_aggr$y ) +
    #         geom_vline(xintercept = year_breaks, color="grey50") +
    #         geom_hline(yintercept = (0 + rain_min)*(dep_rate_range/rain_range), color="dodgerblue3", linetype = "longdash") +
    #         facet_wrap(~region, ncol=3, scales = "free_y") +
    #         theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #         scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #         theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #               axis.title.x = element_text(color="grey30"), 
    #               axis.line.y = element_line(color = "indianred3"), axis.ticks.y = element_line(color = "indianred3"),
    #               axis.text.y = element_text(color = "indianred3"), axis.title.y = element_text(color="indianred3", margin = margin(r = 10) ),
    #               axis.line.y.right = element_line(color = "dodgerblue4"), axis.ticks.y.right = element_line(color = "dodgerblue4"),
    #               axis.text.y.right = element_text(color = "dodgerblue4"), axis.title.y.right = element_text(color="dodgerblue4", margin = margin(l = 10) ) 
    #              )
    #       
    #   # call plot
    #   plot
    # 
    # # ToT vs. displacement
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #         geom_bar(mapping = aes(x = date, y = dep_rate), stat = "identity", fill = "hotpink4") +
    #         geom_line(mapping = aes(x = date, y = tot_wage_cereal ), colour = "indianred3") +
    #         scale_y_continuous("Number of new dep_rate from the region", labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    #           sec.axis = sec_axis(~ ., labels = derive(), name="Terms of trade - Kcal cereal equivalent of daily wage")) +
    #         theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #         labs(x = "\nmonth", y = ts_aggr$y ) +
    #         geom_vline(xintercept = year_breaks, color="grey50") +
    #         facet_wrap(~region, ncol=3, scales = "free_y") +
    #         theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #         scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #         theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #               axis.title.x = element_text(color="grey30"), 
    #               axis.line.y = element_line(color = "hotpink4"), axis.ticks.y = element_line(color = "hotpink4"),
    #               axis.text.y = element_text(color = "hotpink4"), axis.title.y = element_text(color="hotpink4", margin = margin(r = 10) ),
    #               axis.line.y.right = element_line(color = "indianred3"), axis.ticks.y.right = element_line(color = "indianred3"),
    #               axis.text.y.right = element_text(color = "indianred3"), axis.title.y.right = element_text(color="indianred3", margin = margin(l = 10) ) 
    #              )
    #       
    #   # call plot
    #   print(plot)    
    # 
    # 
    # # Proportion of IDPs and returnees vs. SAM admissions rate
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #     geom_line(mapping = aes(x = date, y = prop_idp), stat = "identity", colour = "orangered3") +
    #     geom_line(mapping = aes(x = date, y = (sam + sam_min)*(prop_idp_range/sam_range) ), colour = "aquamarine4") +
    #     scale_y_continuous("Proportion of IDPs and refugees among general population", labels = scales::percent,
    #                        sec.axis = sec_axis(~ (./(prop_idp_range/sam_range))-sam_min, name="SAM admissions rate (per 100,000 population)")) +
    #     theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #     labs(x = "\nmonth", y = ts_aggr$y ) +
    #     geom_vline(xintercept = year_breaks, color="grey50") +
    #     facet_wrap(~region, ncol=3, scales = "free_y") +
    #     theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #     scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #     theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #           axis.title.x = element_text(color="grey30"), 
    #           axis.line.y = element_line(color = "orangered3"), axis.ticks.y = element_line(color = "orangered3"),
    #           axis.text.y = element_text(color = "orangered3"), axis.title.y = element_text(color="orangered3", margin = margin(r = 10) ),
    #           axis.line.y.right = element_line(color = "aquamarine4"), axis.ticks.y.right = element_line(color = "aquamarine4"),
    #           axis.text.y.right = element_text(color = "aquamarine4"), axis.title.y.right = element_text(color="aquamarine4", margin = margin(l = 10) ) 
    #     )
    #   
    #   # call plot
    #   print(plot)
    #   
    # 
    # # ToT vs. SAM admissions rate
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #     geom_line(mapping = aes(x = date, y = tot_wage_cereal), stat = "identity", colour = "indianred3") +
    #     geom_line(mapping = aes(x = date, y = (sam + sam_min)*(tot_range/sam_range) ), colour = "aquamarine4") +
    #     scale_y_continuous("Terms of trade - Kcal cereal equivalent of daily wage", labels = function(x) format(x, big.mark = ",", scientific = FALSE),
    #                        breaks = seq(0, 100000, by = 20000), 
    #                        sec.axis = sec_axis(~ (./(tot_range/sam_range))-sam_min, name="SAM admissions rate (per 100,000 population)")) +
    #     theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #     labs(x = "\nmonth", y = ts_aggr$y ) +
    #     geom_vline(xintercept = year_breaks, color="grey50") +
    #     facet_wrap(~region, ncol=3, scales = "free_y") +
    #     theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #     scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #     theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #           axis.title.x = element_text(color="grey30"), 
    #           axis.line.y = element_line(color = "indianred3"), axis.ticks.y = element_line(color = "indianred3"),
    #           axis.text.y = element_text(color = "indianred3"), axis.title.y = element_text(color="indianred3", margin = margin(r = 10) ),
    #           axis.line.y.right = element_line(color = "aquamarine4"), axis.ticks.y.right = element_line(color = "aquamarine4"),
    #           axis.text.y.right = element_text(color = "aquamarine4"), axis.title.y.right = element_text(color="aquamarine4", margin = margin(l = 10) ) 
    #     )
    #   
    #   # call plot
    #   print(plot)
    #   
    # 
    # # Proportion of IDPs and returnees vs. measles incidence rate
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #     geom_line(mapping = aes(x = date, y = prop_idp), stat = "identity", colour = "orangered3") +
    #     geom_bar(mapping = aes(x = date, y = (measles + measles_min)*(prop_idp_range/measles_range) ), stat = "identity", fill = "maroon", linetype = "longdash") +
    #     scale_y_continuous("Proportion of IDPs and refugees among general population", labels = scales::percent,
    #                        sec.axis = sec_axis(~ (./(prop_idp_range/measles_range))-measles_min, name="measles incidence rate (per 100,000 population)")) +
    #     theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #     labs(x = "\nmonth", y = ts_aggr$y ) +
    #     geom_vline(xintercept = year_breaks, color="grey50") +
    #     facet_wrap(~region, ncol=3, scales = "free_y") +
    #     theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #     scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #     theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #           axis.title.x = element_text(color="grey30"), 
    #           axis.line.y = element_line(color = "orangered3"), axis.ticks.y = element_line(color = "orangered3"),
    #           axis.text.y = element_text(color = "orangered3"), axis.title.y = element_text(color="orangered3", margin = margin(r = 10) ),
    #           axis.line.y.right = element_line(color = "maroon"), axis.ticks.y.right = element_line(color = "maroon"),
    #           axis.text.y.right = element_text(color = "maroon"), axis.title.y.right = element_text(color="maroon", margin = margin(l = 10) ) 
    #     )
    #   
    #   # call plot
    #   print(plot)
    #   
    #   
    # # SAM admissions rate vs. measles incidence rate
    #   # draw plot
    #   plot <- ggplot(ts_aggr) +
    #     geom_line(mapping = aes(x = date, y = sam), stat = "identity", colour = "aquamarine4") +
    #     geom_bar(mapping = aes(x = date, y = (measles + measles_min)*(sam_range/measles_range) ), stat = "identity", fill = "maroon", linetype = "longdash") +
    #     scale_y_continuous("SAM admissions rate (per 100,000 population)",
    #                        sec.axis = sec_axis(~ (./(sam_range/measles_range))-measles_min, name="measles incidence rate (per 100,000 population)")) +
    #     theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm") ) +
    #     labs(x = "\nmonth", y = ts_aggr$y ) +
    #     geom_vline(xintercept = year_breaks, color="grey50") +
    #     facet_wrap(~region, ncol=3, scales = "free_y") +
    #     theme(axis.text.x=element_text(angle = -90, vjust=0.5)) +
    #     scale_x_date("\nmonth - year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
    #     theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
    #           axis.title.x = element_text(color="grey30"), 
    #           axis.line.y = element_line(color = "aquamarine4"), axis.ticks.y = element_line(color = "aquamarine4"),
    #           axis.text.y = element_text(color = "aquamarine4"), axis.title.y = element_text(color="aquamarine4", margin = margin(r = 10) ),
    #           axis.line.y.right = element_line(color = "maroon"), axis.ticks.y.right = element_line(color = "maroon"),
    #           axis.text.y.right = element_text(color = "maroon"), axis.title.y.right = element_text(color="maroon", margin = margin(l = 10) ) 
    #     )
    #   
    #   # call plot
    #   print(plot)
    #       
      
            
#..........................................................................................
### ENDS


