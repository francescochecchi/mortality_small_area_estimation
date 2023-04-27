#..........................................................................................
### +++++++++++++ SMALL-AREA ESTIMATION OF CRISIS-ATTRIBUTABLE MORTALITY ++++++++++++++ ###
#..........................................................................................

#..........................................................................................
## --------- CODE TO RE-ANALYSE AND DESCRIBE TRENDS IN SMART MORTALITY SURVEYS --------- ##
#..........................................................................................

                                          # Written by Francesco Checchi, LSHTM (Nov 2020)
                                          # francesco.checchi@lshtm.ac.uk 


#.........................................................................................
### Preparing output
#.........................................................................................

  #...................................  
  ## Assign extra columns of meta-data dataframe that will store re-analysis results for each survey
  out_col_names <- c("lshtm_survey_design",
    "lshtm_cdr_est", "lshtm_cdr_lci", "lshtm_cdr_uci", "lshtm_cdr_log_se",
    "lshtm_cdr_f_est", "lshtm_cdr_f_lci", "lshtm_cdr_f_uci", "lshtm_cdr_f_log_se",
    "lshtm_cdr_m_est", "lshtm_cdr_m_lci", "lshtm_cdr_m_uci", "lshtm_cdr_m_log_se",
    "lshtm_cdr_u5_est", "lshtm_cdr_u5_lci", "lshtm_cdr_u5_uci", "lshtm_cdr_u5_log_se",
    "lshtm_cdr_inj_est", "lshtm_cdr_inj_log_se",
    "lshtm_cdr_viol_est", "lshtm_cdr_viol_log_se",
    "lshtm_prop_unk", "lshtm_prop_inj", "lshtm_prop_oth", "lshtm_prop_viol", "lshtm_prop_viol_u18", "lshtm_prop_viol_f_o18",
    "lshtm_prop_inj_m", "lshtm_prop_died_u1",
    "lshtm_cbr_est", "lshtm_in_migration_rate_est", "lshtm_out_migration_rate_est", "lshtm_net_migration_rate_est",
    "lshtm_mean_n", "lshtm_prop_f", "lshtm_prop_u5"
    )
  
  surveys[, out_col_names] <- NA
  
  
  #...................................  
  ## Create (empty) dataset that will contain all household observations from all surveys with datasets
    # columns to be retained in the main household-level dataset (for surveys using an aggregate household questionnaire)
    cols_aggr_questionnaire <- c("survey_id", "Cluster", "HH",
      "n", "n_u5" ,"n_join", "n_join_u5", "n_left", "n_left_u5",
      "n_born", "n_died", "n_died_u5", "ptime", "ptime_u5")
    
    # columns to be retained in the main household-level dataset (for surveys using an individual household questionnaire)
    cols_ind_questionnaire <- c(cols_aggr_questionnaire, "Team", 
      "n_f", "n_m", "n_f_u5", "n_m_u5", "n_join_f", "n_join_m",
      "n_left_f", "n_left_m", "n_born_f", "n_born_m", "n_died_m", "n_died_f", 
      "n_died_u1" , "n_died_inj", "n_died_inj_f", "n_died_inj_m", "n_died_inj_u5",
      "n_died_unk", "n_died_oth", "n_died_viol", "n_died_viol_u18", "n_died_viol_f_o18",
      "ptime_f", "ptime_m")
    
    # initialise dataset of all survey observations (with individual questionnaire columns)
        # (aggregate questionnaire surveys will also go into this dataset, but only feature data for some of the columns)
    hh_obs <- data.frame(matrix(NA, ncol = length(cols_ind_questionnaire) ))
    colnames(hh_obs) <- cols_ind_questionnaire

 

#.........................................................................................                            
### Re-analysing each survey, one by one, and adding surveys with datasets to the all-household dataset
#.........................................................................................
    
  #...................................    
  ## Set working directory for this part of the code
  setwd(dir_surveys)
  
for (i in 1:nrow(surveys) ) {
  
  #...................................  
  ## Read survey id
  survey_id <- surveys[i, "survey_id"]
    
    # Control message showing progress of loop
    print(paste ("now working on survey...", i, " of ", nrow(surveys), "...ID ", survey_id, sep=""))

  ##..........................................
  ## >>>OPTION 0: If the survey is excluded....
      
  if (surveys[i, "exclude"] == "Y") { next }  # skip to next survey

  ##..........................................
  ## >>>OPTION 1: If the survey does not have a dataset available for re-analysis....
      
  if (surveys[i, "dataset"] == "N") {

    # Check plausibility of standard errors / confidence intervals and correct these if they seem asymmetric or are missing altogether
      
      # compute absolute difference between upper and lower interval of log rate estimate (should be close to 0) for CDR...
      cdr_ci_asymmetry  <- abs( ( log(surveys[i, "report_cdr_uci"]) - log(surveys[i, "report_cdr_est"]) ) -
        ( log(surveys[i, "report_cdr_est"]) - log(surveys[i, "report_cdr_lci"]) ) )
        
      #...and for U5DR
      cdr_u5_ci_asymmetry <- abs( (log(surveys[i, "report_cdr_u5_uci"]) - log(surveys[i, "report_cdr_u5_est"]) ) -
        (log(surveys[i, "report_cdr_u5_est"]) - log(surveys[i, "report_cdr_u5_lci"]) ) )
        
      # if absolute value of asymmetry for CDR is >= tolerated, or asymmetry = NA (meaning there is no 95CI)...
      if (cdr_ci_asymmetry >= tol_discr_cdr | is.na(cdr_ci_asymmetry) == TRUE)	{
          
        # correct CIs using point estimate, reported deaths and assumed design effect to generate a...
          #...corrected standard error:
          cdr_log_se <- (1 / sqrt(surveys[i, "report_deaths"]) ) * adj_cdr_deff ;
          
          #...corrected lower CI:
          cdr_lci <- exp(log(surveys[i, "report_cdr_est"]) - 1.96 * cdr_log_se) ;
          
          #...and corrected upper CI:
          cdr_uci <- exp(log(surveys[i, "report_cdr_est"]) + 1.96 * cdr_log_se)
          
        # ...and update LSHTM outputs accordingly:
          surveys[i, "lshtm_cdr_est"] <- surveys[i, "report_cdr_est"]
          surveys[i, "lshtm_cdr_lci"] <- cdr_lci
          surveys[i, "lshtm_cdr_uci"] <- cdr_uci
          surveys[i, "lshtm_cdr_log_se"] <- cdr_log_se
          
      }

      # if absolute value of asymmetry for U5DR is >= tolerated, or asymmetry = NA (meaning there is no 95CI)...
      
      if (cdr_u5_ci_asymmetry >= tol_discr_cdr_u5 | is.na(cdr_u5_ci_asymmetry) == TRUE)	{
        
        # correct CIs using point estimate, reported deaths and assumed design effect to generate a...
          #...corrected standard error:
          cdr_u5_log_se <- (1 / sqrt(surveys[i, "report_deaths_u5"]) ) * adj_cdr_u5_deff ;
        
          #...corrected lower CI:
          cdr_u5_lci <- exp(log(surveys[i, "report_cdr_u5_est"]) - 1.96 * cdr_u5_log_se) ;
        
          #...and corrected upper CI:
          cdr_u5_uci <- exp(log(surveys[i, "report_cdr_u5_est"]) + 1.96 * cdr_u5_log_se)
        
        # ...and update LSHTM outputs accordingly:
          surveys[i, "lshtm_cdr_u5_est"] <- surveys[i, "report_cdr_u5_est"]
          surveys[i, "lshtm_cdr_u5_lci"] <- cdr_u5_lci
          surveys[i, "lshtm_cdr_u5_uci"] <- cdr_u5_uci
          surveys[i, "lshtm_cdr_u5_log_se"] <- cdr_u5_log_se
          
      }
      
      # if data are sufficient, also calculate estimates and SEs for injury- and violence-specific death rates (assume deff twice that for CDR)
      surveys[i, "lshtm_cdr_inj_est"] <- ifelse( (is.na(surveys[i, "report_cdr_est"]) == TRUE | is.na(surveys[i, "report_prop_inj"]) == TRUE), NA,  
                                                    surveys[i, "report_cdr_est"] * surveys[i, "report_prop_inj"] )
      surveys[i, "lshtm_cdr_inj_log_se"] <- ifelse( (is.na(surveys[i, "report_deaths"]) == TRUE | is.na(surveys[i, "report_prop_inj"]) == TRUE), NA,  
                                                   (1 / sqrt(surveys[i, "report_deaths"] * surveys[i, "report_prop_inj"]) ) * adj_cdr_deff * 2 )

      surveys[i, "lshtm_cdr_viol_est"] <- ifelse( (is.na(surveys[i, "report_cdr_est"]) == TRUE | is.na(surveys[i, "report_prop_viol"]) == TRUE), NA,  
                                                 surveys[i, "report_cdr_est"] * surveys[i, "report_prop_viol"] )      
      surveys[i, "lshtm_cdr_viol_log_se"] <- ifelse( (is.na(surveys[i, "report_deaths"]) == TRUE | is.na(surveys[i, "report_prop_viol"]) == TRUE), NA,  
                                                    (1 / sqrt(surveys[i, "report_deaths"] * surveys[i, "report_prop_viol"]) ) * adj_cdr_deff * 2 )    
  }
    
  ##..........................................
  ## >>>OPTION 2: If the survey has an aggregate questionnaire dataset...
    
  if (surveys[i, "dataset"] == "Y" & surveys[i, "dataset_type"] == "aggregate") {
      
    # Specify the survey's recall period (in days)
    recall_period <- surveys[i, "recall_days"]    
      
    # Read in the survey dataset
    df <- read.csv(paste(survey_id, ".csv", sep=""), header=FALSE, sep=",", skip=2,
            col.names = c("HH", "Cluster", "n", "n_u5" ,"n_join", "n_join_u5", "n_left", "n_left_u5", "n_born", "n_died", "n_died_u5") )
    
    # Parse any non-numeric values or values with non-numeric characters that may have crept in
    df <- apply(df, c(1,2), as.character)
    df <- as.data.frame(apply(t(df), 1, parse_vector, col_integer() )) 
    
    # Omit observations if Cluster is missing
      # (unless it's missing across the entire database, which may mean an exhaustive or random sampling survey)
    if (length(unique(df[, "Cluster"])) > 1) { df <- subset(df, ! is.na(Cluster) ) }
    
    # Recode all missing values / blanks as 0's  
    df[is.na(df)] <- 0
      
    # Calculate person-time (in days) for all individuals and children under 5y
      # assume births, deaths and in-/out-migrations occurred at mid-point of period (see Methods Note)
    df[, "ptime"] <- (df$n - df$n_join * 0.5 + df$n_left * 0.5 - df$n_born * 0.5 + df$n_died * 0.5 ) * recall_period
    df[, "ptime_u5"] <- (df$n_u5 - df$n_join_u5 * 0.5 + df$n_left_u5 * 0.5 - 
      df$n_born * 0.5 + df$n_died_u5 * 0.5 + df$n_u5 * 0.5 * recall_period / (60 * 30.41) ) * recall_period

    # Specify survey designs
      # if the survey seemed to do simple/systematic random sampling (zero or only one cluster)...
      if (length(unique(df[, "Cluster"])) == 1 | length(unique(df[, "Cluster"])) == 0) {
        surveys[i, "lshtm_survey_design"] <- "SRS or exhaustive"
        survey_design <- svydesign(id = ~0, data = subset(df, ptime > 0) )  # rates among all ages (only households with non-zero person-time)
        survey_design_u5 <- svydesign(id = ~0, data = subset(df, ptime_u5 > 0) ) # rates among children under 5y (only households with non-zero under 5y person-time)   
      }
      # if there is evidence of cluster sampling (at least two clusters)...
      if (length(unique(df[, "Cluster"])) > 1) {
        surveys[i, "lshtm_survey_design"] <- "multi-stage cluster"
        survey_design <- svydesign(id = ~Cluster, data = subset(df, ptime > 0) )  # rates among all ages (only households with non-zero person-time)
        survey_design_u5 <- svydesign(id = ~Cluster, data = subset(df, ptime_u5 > 0) ) # rates among children under 5y (only households with non-zero under 5y person-time)   
      }
    
    # Compute death rate point estimates (per 10,000 person-days), standard errors and confidence intervals  
      # among all ages
        fit <- svyglm(n_died~NULL, survey_design, family="poisson", offset=log(ptime) )
        surveys[i, "lshtm_cdr_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
        surveys[i, "lshtm_cdr_log_se"] <- summary(fit)$coefficients[[2]]
        surveys[i, "lshtm_cdr_lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
        surveys[i, "lshtm_cdr_uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000
        
      # among children under 5y
        fit <- svyglm(n_died_u5~NULL, survey_design_u5, family="poisson", offset=log(ptime_u5) )
        surveys[i, "lshtm_cdr_u5_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
        surveys[i, "lshtm_cdr_u5_log_se"] <- summary(fit)$coefficients[[2]]
        surveys[i, "lshtm_cdr_u5_lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
        surveys[i, "lshtm_cdr_u5_uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000

    # Compute crude birth rate (per 1000 person-years)
      fit <- svyglm(n_born~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_cbr_est"] <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365
      
    # Compute migration rates (per 1000 person-years)
      # in-migration
      fit <- svyglm(n_join~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_in_migration_rate_est"] <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365
      
      # out-migration
      fit <- svyglm(n_left~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_out_migration_rate_est"] <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365
      
      # net migration
      surveys[i, "lshtm_net_migration_rate_est"] <- surveys[i, "lshtm_in_migration_rate_est"] - 
        surveys[i, "lshtm_out_migration_rate_est"]
      
    # Compute other survey statistics
      # mean current household size
      surveys[i, "lshtm_mean_n"] <- mean(df[, "n"], na.rm = TRUE)
      
      # proportion of children under 5y
      surveys[i, "lshtm_prop_u5"] <- sum(df[, "n_u5"]) / sum(df[, "n"])    

    # If data are sufficient, also calculate estimates and SEs for injury- and violence-specific death rates (assume deff twice that for CDR)
      # do this calculation only based on the report, as aggregate SMART survey datasets won't contain this information
      surveys[i, "lshtm_cdr_inj_est"] <- ifelse( (is.na(surveys[i, "report_cdr_est"]) == TRUE | is.na(surveys[i, "report_prop_inj"]) == TRUE), NA,  
                                                 surveys[i, "report_cdr_est"] * surveys[i, "report_prop_inj"] )
      surveys[i, "lshtm_cdr_inj_log_se"] <- ifelse( (is.na(surveys[i, "report_deaths"]) == TRUE | is.na(surveys[i, "report_prop_inj"]) == TRUE), NA,  
                                                    (1 / sqrt(surveys[i, "report_deaths"] * surveys[i, "report_prop_inj"]) ) * adj_cdr_deff * 2 )
      
      surveys[i, "lshtm_cdr_viol_est"] <- ifelse( (is.na(surveys[i, "report_cdr_est"]) == TRUE | is.na(surveys[i, "report_prop_viol"]) == TRUE), NA,  
                                                  surveys[i, "report_cdr_est"] * surveys[i, "report_prop_viol"] )      
      surveys[i, "lshtm_cdr_viol_log_se"] <- ifelse( (is.na(surveys[i, "report_deaths"]) == TRUE | is.na(surveys[i, "report_prop_viol"]) == TRUE), NA,  
                                                     (1 / sqrt(surveys[i, "report_deaths"] * surveys[i, "report_prop_viol"]) ) * adj_cdr_deff * 2 )       
      
      
    # Add this survey's dataset to all-household dataset
      # specify survey ID of survey
      df[, "survey_id"] <- survey_id
      
      # add blank columns for variables that aggregate surveys don't collect
      df[, c("Team", "n_f", "n_m", "n_f_u5", "n_m_u5", "n_join_f", "n_join_m", "n_left_f", "n_left_m", "n_born_f", 
              "n_born_m", "n_died_m", "n_died_f", "n_died_u1" , "n_died_inj", "n_died_inj_f", "n_died_inj_m", "n_died_inj_u5",
               "n_died_unk", "n_died_oth", "n_died_viol", "n_died_viol_u18", "n_died_viol_f_o18", "ptime_f", "ptime_m")] <- NA
      
      # order columns
      df <- df[, cols_ind_questionnaire]
      
      # add dataset
      hh_obs <- rbind(hh_obs, df)
      
  }  
      
  ##..........................................
  ## >>>OPTION 3: If the survey has an individual questionnaire dataset...
    
  if (surveys[i, "dataset"] == "Y" & surveys[i, "dataset_type"] == "individual") {
        
    # Specify the survey's recall period (in days)
    recall_period <- surveys[i, "recall_days"]  
    
    # Specify which cause-of-death values correspond to which cause of death
    unk_code <- surveys[i, "unk_code"] # unknown cause
    inj_codes <- c(surveys[i, "inj_code"], surveys[i, "viol_code"])  # trauma/injury , violence/conflict
    viol_code <- surveys[i, "viol_code"] # violence/conflict
    dis_codes <- c(surveys[i, "dis_code1"], surveys[i, "dis_code2"], 
      surveys[i, "dis_code3"], surveys[i, "dis_code4"],
      surveys[i, "dis_code5"], 8:99) # disease codes 1 through 5, plus other codes that may be used instead
      
    # Read in the survey dataset, specifying any string columns as character in R
    df <- read.csv(paste(survey_id, ".csv", sep=""), sep=",",
      colClasses = c("character", "numeric", "numeric", "numeric", 
      rep(c("character", "numeric", "character", "character", "character", "character", "integer", "integer"), times = p_max) ) )
    
    # Omit observations if Cluster is missing
      # (unless it's missing across the entire database, which may mean an exhaustive or random sampling survey)
    if (length(unique(df[, "Cluster"])) > 1) {
      df <- subset(df, is.na(Cluster)==FALSE )
    }
    
    # Define individuals as eligible for analysis if both age and sex are recorded
    for (j in 1:p_max) {
      df[, paste("P",j,"_eligible", sep="")] <- 
        ifelse(df[, paste("P",j,"_sex", sep="")]!="" & is.na(df[, paste("P",j,"_age", sep="")])==FALSE , TRUE, FALSE )
      }
    
    # Set to zero the numbers of individuals by gender, age, migration, birth and death status
      df[, "n"] <- 0 #people
      df[, "n_f"] <- 0 #females
      df[, "n_m"] <- 0 #males
      df[, "n_u5"] <- 0 #children under 5y
      df[, "n_f_u5"] <- 0 #females under 5y
      df[, "n_m_u5"] <- 0 #males under 5y
      df[, "n.5"] <- 0 # children aged 5y
    
      df[, "n_join"] <- 0 #people who joined households
      df[, "n_join_f"] <- 0 #females who joined households
      df[, "n_join_m"] <- 0 #males who joined households
      df[, "n_join_u5"] <- 0 #children under 5y who joined households
      df[, "n_join.5"] <- 0 # children aged 5y who joined households
      
      df[, "n_left"] <- 0 #people who left households
      df[, "n_left_f"] <- 0 #females who left households
      df[, "n_left_m"] <- 0 #males who left households
      df[, "n_left_u5"] <- 0 #children under 5y who left households
      df[, "n_left.5"] <- 0 # children aged 5y who left households
      
      df[, "n_born"] <- 0 #births
      df[, "n_born_f"] <- 0 #female births
      df[, "n_born_m"] <- 0 #male births
      
      df[, "n_died"] <- 0 #deaths
      df[, "n_died_m"] <- 0 #deaths among males
      df[, "n_died_f"] <- 0 #deaths among females
      df[, "n_died_u5"] <- 0 #deaths under 5y
      df[, "n_died_u1"] <- 0 #deaths under 1y
      df[, "n_died.5"] <- 0 #deaths aged 5y      
      df[, "n_died_inj"] <- 0 #deaths due to trauma / injury
      df[, "n_died_inj_f"] <- 0 #deaths due to trauma / injury among females
      df[, "n_died_inj_m"] <- 0 #deaths due to trauma / injury among males
      df[, "n_died_inj_u5"] <- 0 #deaths due to trauma / injury among children under 5y
      df[, "n_died_unk"] <- 0 #deaths due to unknown cause
      df[, "n_died_oth"] <- 0 #deaths due to other causes
      df[, "n_died_viol"] <- 0 #deaths due to violence/conflict (part of total trauma/injury deaths)
      df[, "n_died_viol_u18"] <- 0 #deaths due to violence/conflict among children under 18y (part of total trauma/injury deaths)
      df[, "n_died_viol_f_o18"] <- 0 #deaths due to violence/conflict among women aged 18y or older (part of total trauma/injury deaths)
            
    # tally all individuals by categories above
    for (j in 1:p_max) {
        
      df[, "n"] <- df[, "n"] + 
        ifelse(df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_f"] <- df[, "n_f"] + 
        ifelse(df[, paste("P",j,"_sex", sep="")]=="f" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_m"] <- df[, "n_m"] + 
        ifelse(df[, paste("P",j,"_sex", sep="")]=="m" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_u5"] <- df[, "n_u5"] + 
        ifelse(df[, paste("P",j,"_age", sep="")] <5 & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_f_u5"] <- df[, "n_f_u5"] + 
        ifelse(df[, paste("P",j,"_sex", sep="")]=="f" & df[, paste("P",j,"_age", sep="")] <5 &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_m_u5"] <- df[, "n_m_u5"] + 
        ifelse(df[, paste("P",j,"_sex", sep="")]=="m" & df[, paste("P",j,"_age", sep="")] <5 &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n.5"] <- df[, "n.5"] + 
        ifelse(df[, paste("P",j,"_age", sep="")] ==5 & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
        
      df[, "n_join"] <- df[, "n_join"] + 
        ifelse(df[, paste("P",j,"_join", sep="")]=="y" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_join_f"] <- df[, "n_join_f"] + 
        ifelse(df[, paste("P",j,"_join", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="f"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )    
      df[, "n_join_m"] <- df[, "n_join_m"] + 
        ifelse(df[, paste("P",j,"_join", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="m"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 ) 
      df[, "n_join_u5"] <- df[, "n_join_u5"] + 
        ifelse(df[, paste("P",j,"_join", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] <5
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )     
      df[, "n_join.5"] <- df[, "n_join.5"] + 
        ifelse(df[, paste("P",j,"_join", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] ==5
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )  
      
      df[, "n_left"] <- df[, "n_left"] + 
        ifelse(df[, paste("P",j,"_left", sep="")]=="y" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_left_f"] <- df[, "n_left_f"] + 
        ifelse(df[, paste("P",j,"_left", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="f"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )    
      df[, "n_left_m"] <- df[, "n_left_m"] + 
        ifelse(df[, paste("P",j,"_left", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="m"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 ) 
      df[, "n_left_u5"] <- df[, "n_left_u5"] + 
        ifelse(df[, paste("P",j,"_left", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] <5
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )  
      df[, "n_left.5"] <- df[, "n_left.5"] + 
        ifelse(df[, paste("P",j,"_left", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] ==5
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )          
      
      df[, "n_born"] <- df[, "n_born"] + 
        ifelse(df[, paste("P",j,"_born", sep="")]=="y" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_born_f"] <- df[, "n_born_f"] + 
        ifelse(df[, paste("P",j,"_born", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="f"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_born_m"] <- df[, "n_born_m"] + 
        ifelse(df[, paste("P",j,"_born", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="m"
        & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
         
      df[, "n_died"] <- df[, "n_died"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_m"] <- df[, "n_died_m"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="m" &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_f"] <- df[, "n_died_f"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_sex", sep="")] =="f" &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_u5"] <- df[, "n_died_u5"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] <5 &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_u1"] <- df[, "n_died_u1"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] <1 &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died.5"] <- df[, "n_died.5"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_age", sep="")] ==5 &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_inj"] <- df[, "n_died_inj"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & (df[, paste("P",j,"_cause", sep="")] %in% inj_codes ) &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_inj_f"] <- df[, "n_died_inj_f"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & (df[, paste("P",j,"_cause", sep="")] %in% inj_codes ) &
        df[, paste("P",j,"_sex", sep="")] =="f" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_inj_m"] <- df[, "n_died_inj_m"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & (df[, paste("P",j,"_cause", sep="")] %in% inj_codes ) &
        df[, paste("P",j,"_sex", sep="")] =="m" & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      df[, "n_died_inj_u5"] <- df[, "n_died_inj_u5"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & (df[, paste("P",j,"_cause", sep="")] %in% inj_codes ) &
        df[, paste("P",j,"_age", sep="")] <5 & df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
    
      if (is.na(unk_code)==FALSE) { df[, "n_died_unk"] <- df[, "n_died_unk"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_cause", sep="")] == unk_code &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 ) }
      df[, "n_died_oth"] <- df[, "n_died_oth"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & (df[, paste("P",j,"_cause", sep="")] %in% dis_codes) &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 )
      if (is.na(viol_code)==FALSE) { df[, "n_died_viol"] <- df[, "n_died_viol"] + 
        ifelse(df[, paste("P",j,"_died", sep="")]=="y" & df[, paste("P",j,"_cause", sep="")] == viol_code &
        df[, paste("P",j,"_eligible", sep="")]==TRUE, 1, 0 ) }   
      if (is.na(viol_code)==FALSE) { df[, "n_died_viol_u18"] <- df[, "n_died_viol_u18"] + 
        ifelse( (df[, paste("P",j,"_died", sep="")]=="y") & (df[, paste("P",j,"_cause", sep="")] == viol_code) &
        df[, paste("P",j,"_age", sep="")] <18 & (df[, paste("P",j,"_eligible", sep="")]==TRUE), 1, 0 ) }       
      if (is.na(viol_code)==FALSE) { df[, "n_died_viol_f_o18"] <- df[, "n_died_viol_f_o18"] + 
        ifelse( (df[, paste("P",j,"_died", sep="")]=="y") & (df[, paste("P",j,"_cause", sep="")] == viol_code) &
        df[, paste("P",j,"_age", sep="")] >=18 & df[, paste("P",j,"_sex", sep="")] =="f" & (df[, paste("P",j,"_eligible", sep="")]==TRUE), 1, 0 ) }       
         
      }

    # Calculate person-time (in days) for all individuals, females, males and children under 5y
      # assume births, deaths and in-/out-migrations occurred at mid-point of period (see Methods Note)
      
    df[, "ptime"] <- (df[, "n"] - 0.5 * df[, "n_join"] + 0.5 * df[, "n_left"] - 0.5 * df[, "n_born"] + 0.5 * df[, "n_died"] ) * recall_period
    
    df[, "ptime_f"] <- (df[, "n_f"] - 0.5 * df[, "n_join_f"] + 0.5 * df[, "n_left_f"] - 0.5 * df[, "n_born_f"] + 0.5 * df[, "n_died_f"] ) * recall_period
      
    df[, "ptime_m"] <- (df[, "n_m"] - 0.5 * df[, "n_join_m"] + 0.5 * df[, "n_left_m"] - 0.5 * df[, "n_born_m"] + 0.5 * df[, "n_died_m"] ) * recall_period
      
    df[, "ptime_u5"] <- (df[, "n_u5"] - 0.5 * df[, "n_join_u5"] + 0.5 * df[, "n_left_u5"] - 0.5 * df[, "n_born"] + 0.5 * df[, "n_died_u5"] ) * recall_period +
      (df[, "n.5"] - 0.5 * df[, "n_join.5"] + 0.5 * df[, "n_left.5"] + 0.5 * df[, "n_died.5"] ) * recall_period / 365

    # Specify survey designs...
      # if the survey seemed to do simple/systematic random sampling (zero or only one cluster)...
      if (length(unique(df[, "Cluster"])) == 1 | length(unique(df[, "Cluster"])) == 0) {
        surveys[i, "lshtm_survey_design"] <- "SRS or exhaustive"
        # ...for rates among all ages (only households with non-zero person-time)
        survey_design <- svydesign(id = ~0, data = subset(df, ptime > 0) )  
        # ...for rates among children under 5y (only households with non-zero under 5y person-time)
        survey_design_u5 <- svydesign(id = ~0, data = subset(df, ptime_u5 > 0) )
        # ...for rates among females (only households with non-zero female person-time)
        survey_design.f <- svydesign(id = ~0, data = subset(df, ptime_f > 0) )  
        # ...for rates among males (only households with non-zero male person-time)
        survey_design.m <- svydesign(id = ~0, data = subset(df, ptime_m > 0) )       }
      
      # if there is evidence of cluster sampling (at least two clusters)...
      if (length(unique(df[, "Cluster"])) > 1) {
        surveys[i, "lshtm_survey_design"] <- "multi-stage cluster"
        # ...for rates among all ages (only households with non-zero person-time)
        survey_design <- svydesign(id = ~Cluster, data = subset(df, ptime > 0) )  
        # ...for rates among children under 5y (only households with non-zero under 5y person-time)
        survey_design_u5 <- svydesign(id = ~Cluster, data = subset(df, ptime_u5 > 0) )
        # ...for rates among females (only households with non-zero female person-time)
        survey_design.f <- svydesign(id = ~Cluster, data = subset(df, ptime_f > 0) )  
        # ...for rates among males (only households with non-zero male person-time)
        survey_design.m <- svydesign(id = ~Cluster, data = subset(df, ptime_m > 0) )  
      }    
    
    # Compute death rate point estimates (per 10,000 person-days), standard error and confidence intervals
      # among all ages
      fit <- svyglm(n_died~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_cdr_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
      surveys[i, "lshtm_cdr_log_se"] <- summary(fit)$coefficients[[2]]
      surveys[i, "lshtm_cdr_lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      surveys[i, "lshtm_cdr_uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      
      # among children under 5y
      fit <- svyglm(n_died_u5~NULL, survey_design_u5, family="poisson", offset=log(ptime_u5) )
      surveys[i, "lshtm_cdr_u5_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
      surveys[i, "lshtm_cdr_u5_log_se"] <- summary(fit)$coefficients[[2]]
      surveys[i, "lshtm_cdr_u5_lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      surveys[i, "lshtm_cdr_u5_uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      
      # among females
      fit <- svyglm(n_died_f~NULL, survey_design.f, family="poisson", offset=log(ptime_f) )
      surveys[i, "lshtm_cdr.f.est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
      surveys[i, "lshtm_cdr.f.log.se"] <- summary(fit)$coefficients[[2]]
      surveys[i, "lshtm_cdr.f.lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      surveys[i, "lshtm_cdr.f.uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      
      # among males
      fit <- svyglm(n_died_m~NULL, survey_design.m, family="poisson", offset=log(ptime_m) )
      surveys[i, "lshtm_cdr.m.est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
      surveys[i, "lshtm_cdr.m.log.se"] <- summary(fit)$coefficients[[2]]
      surveys[i, "lshtm_cdr.m.lci"] <- exp(summary(fit)$coefficients[[1]] - 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      surveys[i, "lshtm_cdr.m.uci"] <- exp(summary(fit)$coefficients[[1]] + 1.96 * summary(fit)$coefficients[[2]] ) * 10000
      
      # due to injury/trauma (only if there are data)
      if (is.na(inj_codes)==FALSE) {
        fit <- svyglm(n_died_inj~NULL, survey_design, family="poisson", offset=log(ptime) )  
        surveys[i, "lshtm_cdr_inj_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
        surveys[i, "lshtm_cdr_inj_log_se"] <- summary(fit)$coefficients[[2]]
      }
      
      # due to violence (only if there are data)
      if (is.na(viol_code)==FALSE) {
        fit <- svyglm(n_died_viol~NULL, survey_design, family="poisson", offset=log(ptime) )  
        surveys[i, "lshtm_cdr_viol_est"] <- exp(summary(fit)$coefficients[[1]] ) * 10000
        surveys[i, "lshtm_cdr_viol_log_se"] <- summary(fit)$coefficients[[2]]
      }
        
    # Compute crude birth rate (per 1000 person-years)
      fit <- svyglm(n_born~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_cbr_est"] <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365
      
    # Compute migration rates (per 1000 person-years)
      # in-migration
      fit <- svyglm(n_join~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_in_migration_rate_est"]  <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365

      # out-migration
      fit <- svyglm(n_left~NULL, survey_design, family="poisson", offset=log(ptime) )
      surveys[i, "lshtm_out_migration_rate_est"]  <- exp(summary(fit)$coefficients[[1]] ) * 1000 * 365
        
      # net migration
      surveys[i, "lshtm_net_migration_rate_est"] <- surveys[i, "lshtm_in_migration_rate_est"] - 
        surveys[i, "lshtm_out_migration_rate_est"]
      
    # Compute other survey statistics
      # mean current household size
      surveys[i, "lshtm_mean_n"] <- mean(df[, "n"] - df[, "n_left"] - df[, "n_died"], na.rm = TRUE)
          
      # proportion of females
      surveys[i, "lshtm_prop_f"] <- sum(df[, "ptime_f"]) / sum(df[, "ptime"])
          
      # proportion of children under 5y
      surveys[i, "lshtm_prop_u5"] <- sum(df[, "ptime_u5"]) / sum(df[, "ptime"])    
          
      # proportional mortality
      sum <- sum(df[, "n_died_unk"]) + sum(df[, "n_died_inj"]) + sum(df[, "n_died_oth"])
      surveys[i, "lshtm_prop_unk"] <- ifelse(is.na(unk_code) == TRUE, NA, sum(df[, "n_died_unk"]) / sum )
      surveys[i, "lshtm_prop_inj"] <- ifelse(all(is.na(inj_codes)) == TRUE, NA, sum(df[, "n_died_inj"]) / sum )
      surveys[i, "lshtm_prop_oth"] <- ifelse(all(is.na(dis_codes[1:5])) == TRUE, NA, sum(df[, "n_died_oth"]) / sum )
      surveys[i, "lshtm_prop_viol"] <- ifelse(is.na(viol_code) == TRUE, NA, sum(df[, "n_died_viol"]) / sum )
      # proportion of infant deaths among all deaths under 5y
      surveys[i, "lshtm_prop_died_u1"] <- sum(df[, "n_died_u1"]) / sum(df[, "n_died_u5"])
          
      # proportion of injury/trauma deaths that were among males
      surveys[i, "lshtm_prop_inj_m"] <- sum(df[, "n_died_inj_m"]) / sum(df[, "n_died_inj"])
        
      # proportion of violent deaths that were among children or women
      surveys[i, "lshtm_prop_viol_u18"] <- ifelse(is.na(viol_code)==TRUE, NA, sum(df[, "n_died_viol_u18"]) / sum(df[, "n_died_viol"]) )
      surveys[i, "lshtm_prop_viol_f_o18"] <- ifelse(is.na(viol_code)==TRUE, NA, sum(df[, "n_died_viol_f_o18"]) / sum(df[, "n_died_viol"]) )

    # Add this survey's dataset to all-household dataset
      # specify survey ID of survey
      df[, "survey_id"] <- survey_id
      
      # order columns
      df <- df[, cols_ind_questionnaire]
      
      # add dataset
      hh_obs <- rbind(hh_obs, df)
      
  }  
    
  ##..........................................
    
  ## Close big loop, end of reanalysis
}    
      
      
#.........................................................................................
### Consolidating and saving results of re-analysis
#.........................................................................................  
  
  #...................................  
  ## Consolidated output of reanalysis (where LSHTM estimate is available, use that, else stick with original)
  
    # CDR
    surveys[, "final_cdr_est"] <- ifelse( is.na(surveys[, "lshtm_cdr_est"]) == TRUE, surveys[, "report_cdr_est"], 
                                           surveys[, "lshtm_cdr_est"])   
    surveys[, "final_cdr_lci"] <- ifelse( is.na(surveys[, "lshtm_cdr_lci"]) == TRUE, surveys[, "report_cdr_lci"], 
                                           surveys[, "lshtm_cdr_lci"])   
    surveys[, "final_cdr_uci"] <- ifelse( is.na(surveys[, "lshtm_cdr_uci"]) == TRUE, surveys[, "report_cdr_uci"], 
                                           surveys[, "lshtm_cdr_uci"])   
    surveys[, "final_cdr_log_se"] <- ifelse( is.na(surveys[, "lshtm_cdr_log_se"]) == TRUE, NA, surveys[, "lshtm_cdr_log_se"])   
    
    # U5DR
    surveys[, "final_cdr_u5_est"] <- ifelse( is.na(surveys[, "lshtm_cdr_u5_est"]) == TRUE, surveys[, "report_cdr_u5_est"], 
                                                 surveys[, "lshtm_cdr_u5_est"])   
    surveys[, "final_cdr_u5_lci"] <- ifelse( is.na(surveys[, "lshtm_cdr_u5_lci"]) == TRUE, surveys[, "report_cdr_u5_lci"], 
                                              surveys[, "lshtm_cdr_u5_lci"])   
    surveys[, "final_cdr_u5_uci"] <- ifelse( is.na(surveys[, "lshtm_cdr_u5_uci"]) == TRUE, surveys[, "report_cdr_u5_uci"], 
                                              surveys[, "lshtm_cdr_u5_uci"])   
    surveys[, "final_cdr_u5_log_se"] <- ifelse( is.na(surveys[, "lshtm_cdr_u5_log_se"]) == TRUE, NA, surveys[, "lshtm_cdr_u5_log_se"])   
    
    # Other interesting statistics
    surveys[, "final_cdr_rr_males"] <- ifelse(surveys[, "lshtm_cdr.f.est"] > 0.01, 
                                              surveys[, "lshtm_cdr.m.est"] / surveys[, "lshtm_cdr.f.est"], NA)
    surveys[, "final_prop_inj"] <- ifelse( is.na(surveys[, "lshtm_prop_inj"]) == TRUE, surveys[, "report_prop_inj"], 
                                            surveys[, "lshtm_prop_inj"])
    surveys[, "final_cdr_inj_est"] <- surveys[, "lshtm_cdr_inj_est"]
    surveys[, "final_cdr_inj_log_se"] <- surveys[, "lshtm_cdr_inj_log_se"]
    surveys[, "final_prop_viol"] <- ifelse( is.na(surveys[, "lshtm_prop_viol"]) == TRUE, surveys[, "report_prop_viol"], 
                                             surveys[, "lshtm_prop_viol"])
    surveys[, "final_cdr_viol_est"] <- surveys[, "lshtm_cdr_viol_est"]
    surveys[, "final_cdr_viol_log_se"] <- surveys[, "lshtm_cdr_viol_log_se"]
  
  #...................................      
  ## Flag discrepancies between reported results and re-analysis  
    # CDR
    surveys[, "discrepancy_cdr"] <- ifelse(is.na(surveys[, "lshtm_cdr_est"]) == FALSE & 
      abs(surveys[, "lshtm_cdr_est"] - surveys[, "report_cdr_est"]) >= tol_discr_cdr, TRUE, FALSE)

    # U5DR
    surveys[, "discrepancy_cdr_u5"] <- ifelse(is.na(surveys[, "lshtm_cdr_u5_est"]) == FALSE & 
      abs(surveys[, "lshtm_cdr_u5_est"] - surveys[, "report_cdr_u5_est"]) >= tol_discr_cdr_u5, TRUE, FALSE)
  
  #...................................    
  ## Write results of re-analysis
    # Set working directory
    setwd(dirname(current_path ))
    
    write.csv(surveys, paste(country, "_survey_metadata_reanalysed.csv", sep=""), row.names=FALSE)


#.........................................................................................
### Consolidating and saving dataset of all survey observations for household-level regression
#.........................................................................................
    
  #...................................  
  ## Merge in relevant meta-data for each survey (all.y=FALSE to only merge data from suveys with a dataset, i.e. with hh observations)
  hh_obs <- merge(hh_obs, subset(surveys, exclude=="N")[, c("survey_id", "stratum", "quality_score", "sampling_coverage")],
    by="survey_id", all.y=FALSE, sort=TRUE)
  
    # Nigeria only: Add LGA-cluster information for Unicef surveys, since these covered multiple strata
    if (country == "nga") {

      # merge for observations that have no stratum value
      part1 <- subset(hh_obs, is.na(stratum) == TRUE)
      part1 <- merge(subset(part1, select = -stratum), cluster_to_lgas, by=c("Cluster", "survey_id"), sort=TRUE)

      # recompose full dataset
      part2 <- subset(hh_obs, is.na(stratum) == FALSE)
      hh_obs <- rbind(part1, part2)
      }

    # Create unique Cluster variable
    hh_obs[, "Cluster"] <- paste(hh_obs[, "survey_id"], "_", hh_obs[, "Cluster"], sep="")
      
  #...................................  
  ## Write dataset
  
  write.csv(hh_obs, paste(country, "_hh_obs.csv", sep=""), row.names=FALSE)  


#.........................................................................................
### Consolidating and saving dataset of all survey observations for stratum-level regression
#.........................................................................................  
  # (difference here is we can include surveys without datasets)
  
  #...................................  
  ## Select relevant meta-data for each survey
  relevant_cols <- c("survey_id", "stratum", "quality_score", "sampling_coverage",
    "year_survey",	"month_start",	"month_end","date_start",	"date_end",	"recall_days",
    "final_cdr_est", "final_cdr_log_se", "final_cdr_u5_est", "final_cdr_u5_log_se",
    "final_cdr_inj_est", "final_cdr_inj_log_se", "final_prop_inj", 
    "final_cdr_viol_est", "final_cdr_viol_log_se", "final_prop_viol")
  
  #...................................  
  ## Only include eligible surveys and relevant meta-data
  stratum_obs <- subset(surveys, exclude=="N")[, relevant_cols] 
  
  #...................................                         
  ## Write dataset
  write.csv(stratum_obs, paste(country, "_stratum_obs.csv", sep=""), row.names=FALSE)
      
  
#.........................................................................................
### Generating dataset of recall period month coverage, for each survey  
#.........................................................................................
  
  #...................................  
  ## Preparatory steps
    # exclude non-eligible surveys and select only needed columns
    df <- subset(surveys, exclude=="N")[, c("date_end", "date_start", "month_end", "year_survey", "recall_days", "survey_id")] 
        
    # fix date format
    df[, "date_start"] <- ymd(as.character(df[, "date_start"]))
    df[, "date_end"] <- ymd(as.character(df[, "date_end"]))
    
    # calculate survey data collection mid-point date (i.e. average end date of recall period)
      # assume survey mid-point dates are in the middle of the month (15th) if actual start/end dates are missing
    for (i in 1:nrow(df) ) {
      
      if (is.na(df[i, "date_start"]) | is.na(df[i, "date_end"]) )
        df[i, "date_recall_end"] <- ymd( paste(df[i, "year_survey"], df[i, "month_end"], 15, sep="/") )
        else
        df[i, "date_recall_end"] <- df[i, "date_end"] - (df[i, "date_end"] - df[i, "date_start"]) / 2
    }

    # calculate start date of recall period 
    df[, "date_recall_start"] <- df[, "date_recall_end"] - df[, "recall_days"]
      # fix data formats again
      df[, "date_recall_start"] <- as.Date(df[, "date_recall_start"], origin ="1970-01-01")
      df[, "date_recall_end"] <- as.Date(df[, "date_recall_end"], origin ="1970-01-01")
      
    # calculate time increment variable from 1 to end of time series, for months of recall start and end
    df[, "tm_recall_start"] <- apply(df, 1, function(f_df, f_t_units)
      {f_t_units[f_t_units$y == year(f_df["date_recall_start"]) & 
       f_t_units$m == month(f_df["date_recall_start"]), "tm"] }, t_units )
    df[, "tm_recall_end"] <- apply(df, 1, function(f_df, f_t_units)
      {f_t_units[f_t_units$y == year(f_df["date_recall_end"]) & 
       f_t_units$m == month(f_df["date_recall_end"]), "tm"] }, t_units )

    # create data frame of surveys * time series in which to store % of month covered  
    surveys_cov <- expand.grid(df[, "survey_id"], sort(t_units[, "tm"]) )
    colnames(surveys_cov)<- c("survey_id", "tm")
    surveys_cov[, "month_coverage"] <- 0 # all values set to 0 for now
    surveys_cov <- surveys_cov[order(surveys_cov[, "survey_id"], surveys_cov[, "tm"]), ]

  #...................................    
  ## Calculate proportion of days in each month in the time series that are covered by each survey's recall period
    
    # Generate necessary time quantities for calculation
    df[, "days_in_month_start"] <- days_in_month(df[, "date_recall_start"])
    df[, "days_in_month_end"] <- days_in_month(df[, "date_recall_end"])
    
    df[, "day_start"] <- day(df[, "date_recall_start"])
    df[, "day_end"] <- day(df[, "date_recall_end"])

    # Calculate proportion for each month in the time series and each survey, update corresponding survey coverage column
    surveys_cov[, "month_coverage"] <- apply(surveys_cov, 1, f_calc_days, df)

  #...................................    
  ## Calculate survey time coverage per stratum and time period
    
    # Merge stratum into survey-time coverage data frame to give a data frame of survey-stratum-time coverage
      # this will have the same n of rows as the above for S Sudan and somalia, since each survey covers 1 stratum,
      # but in Nigeria it's a one to many merge
      
      # first merge in survey-stratum combinations that can be analysed at household level...
      x1 <- merge(surveys_cov, unique(hh_obs[, c("survey_id", "stratum")]), by="survey_id", sort=TRUE)
      
      # ...then supplement with the other surveys that are only analysable at stratum level
      x2 <- merge(surveys_cov, unique(stratum_obs[, c("survey_id", "stratum")]), by="survey_id", sort=TRUE)
      
      # and lastly append the two, removing duplicates:
      survey_stratum_cover <- rbind(x1, x2)
      survey_stratum_cover <- na.omit(survey_stratum_cover) #removing observations with stratum = NA (Nigeria)
      survey_stratum_cover <- unique(survey_stratum_cover)

    # Write file
    write.csv(survey_stratum_cover, paste(country, "_survey_stratum_month_cover.csv", sep=""), row.names=FALSE)     

    
#.........................................................................................
### Quantifying availability of mortality survey data for household or stratum-level regression 
#.........................................................................................    
    
  #...................................      
  ## Compute availability of surveys for household-level regression, by stratum-month
    # temporary aggregate dataset of household observations, by survey and stratum
    x1 <- aggregate(hh_obs[, c("ptime", "quality_score", "sampling_coverage") ] ,
            by = list(stratum=hh_obs[, "stratum"], survey_id=hh_obs[, "survey_id"]), FUN = sum )
  
    # compute a single data availability index, combining person-time and weights
    x1[, "data_availability"] <- x1[, "ptime"] * x1[, "quality_score"] * x1[, "sampling_coverage"] 
    
    # temporary dataframe of all surveys * full stratum-month time series per survey
    x2 <- expand.grid(df[, "survey_id"], sort(stratum_names), sort(t_units[, "tm"]) )
    colnames(x2) <- c("survey_id", "stratum", "tm")
    
    # merge with survey availability aggregate data frame
    x2 <- merge(x2, x1[, c("stratum", "survey_id", "data_availability")], by=c("stratum", "survey_id"), sort=TRUE)
    
    # merge with data frame of survey-month coverage (but only surveys with hh obs, i.e. a dataset, hence all.y = FALSE)
    x2 <- merge(x2, surveys_cov[, c("survey_id", "tm", "month_coverage")], by=c("survey_id", "tm"), all.y=FALSE, sort=TRUE)
    
    # recalculate data availability index by taking into account proportion of month actually covered
    x2[, "data_availability"] <- x2[, "data_availability"] * x2[, "month_coverage"] 
    x2[, "data_availability"] <- x2[, "data_availability"] / max(x2[, "data_availability"], na.rm=TRUE) # rescale to relative to max
    
    # now aggregate by stratum (remove survey level) - this sequence is needed partly to cope with multiple surveys covering the same stratum-month
    x2 <- aggregate(x2[, "data_availability" ] , by=list(stratum=x2[, "stratum"], tm=x2[, "tm"] ), FUN=sum )
    colnames(x2) <- c("stratum", "tm", "data_availability")
    
    # merge all strata, months, years and admin1 back in
    x2 <- merge(x2, ts, by=c("stratum", "tm"), all.y = TRUE, sort=TRUE)
      # set data availability to 0 for stratum time series without any survey
      x2[is.na(x2)] <- 0
      
    # sort
    x2 <- x2[order(x2[, "stratum"], x2[, "tm"]), ]

    # name and write dataframe  
    hh_data_avail <- x2
    write.csv(hh_data_avail, paste(country, "_hh_obs_data_avail.csv", sep=""), row.names=FALSE)     
    
  # #...................................    
  # ## Compute availability of surveys for stratum-level regression, by stratum-month
  #   # (difference here is we can include surveys without datasets)
  #   
  #   # temporary dataset of survey observations - exclude any that are not done at stratum level
  #   x1 <- subset(stratum_obs, stratum!= "")[, c("survey_id", "stratum", "quality_score", "sampling_coverage") ]
  # 
  #   # compute a single data availability index, combining weights
  #   x1[, "data_availability"] <- x1[, "quality_score"] * x1[, "sampling_coverage"] 
  #   
  #   # temporary dataframe of all surveys * full stratum-month time series per survey
  #   x2 <- expand.grid(x1[, "survey_id"], sort(stratum_names), sort(t_units[, "tm"]) )
  #   colnames(x2) <- c("survey_id", "stratum", "tm")
  #   
  #   # merge with survey availability data frame
  #   x2 <- merge(x2, x1[, c("stratum", "survey_id", "data_availability")], by=c("stratum", "survey_id"), sort=TRUE)
  #   
  #   # merge with data frame of survey-month coverage
  #   x2 <- merge(x2, surveys_cov[, c("survey_id", "tm", "month_coverage")], by=c("survey_id", "tm"), sort=TRUE)
  #   
  #   # recalculate data availability index by taking into account proportion of month actually covered
  #   x2[, "data_availability"] <- x2[, "data_availability"] * x2[, "month_coverage"]
  #   x2[, "data_availability"] <- x2[, "data_availability"] / max(x2[, "data_availability"], na.rm=TRUE) # rescale to relative to max
  # 
  #   # now aggregate by stratum (remove survey level) - this sequence is needed partly to cope with multiple surveys covering the same stratum-month
  #   x2 <- aggregate(x2[, "data_availability" ] , by=list(stratum=x2[, "stratum"], tm=x2[, "tm"] ), FUN=sum )
  #   colnames(x2) <- c("stratum", "tm", "data_availability")
  #   
  #   # merge all strata, months, years and admin1 back in
  #   x2 <- merge(x2, ts, by=c("stratum", "tm"), all.y = TRUE, sort=TRUE)
  #     # set data availability to 0 for stratum time series without any survey
  #     x2[is.na(x2)] <- 0
  #   
  #   # sort
  #   x2 <- x2[order(x2[, "stratum"], x2[, "tm"]), ]
  #   
  #   # Name and write dataframe  
  #   stratum_data_avail <- x2
  #   write.csv(stratum_data_avail, paste(country, "_stratum_obs_data_avail.csv", sep=""), row.names=FALSE)
  #   
    
#.........................................................................................
### Computing summary statistics from mortality surveys, overall and by year
#.........................................................................................
    
  #...................................  
  ## Create table that will hold output    
  years <- unique(df[, "year_survey"])
  out <- data.frame(matrix(NA, nrow = 10, ncol = 1 + length(years) ) )
  colnames(out) <- c("Overall", as.character(years))
  rownames(out) <- c("Eligible surveys (N)", "Crude death rate", "Under 5 years death rate",
    "Proportion of <5yo deaths that were among infants <1yo","Household size",
    "Proportion of children aged under 5y", "Proportion of females in household", 
    "Crude birth rate", "Net migration rate", "Injury-specific death rate")
  
  
  #...................................          
  ## Filling table
  df <- subset(surveys, exclude=="N")
  
  out["Eligible surveys (N)", ] <- c(nrow(df), table(df[, "year_survey"]) )

  out["Crude death rate", ] <- f_calc_svy("final_cdr_est", df, 2, years)
  out["Under 5 years death rate", ] <- f_calc_svy("final_cdr_u5_est", df, 2, years)
  out["Proportion of <5yo deaths that were among infants <1yo", ] <- f_calc_svy("lshtm_prop_died_u1", df, 2, years)
  out["Household size", ] <- f_calc_svy("lshtm_mean_n", df, 1, years)
  out["Proportion of children aged under 5y", ] <- f_calc_svy("lshtm_prop_u5", df, 2, years)
  out["Proportion of females in household", ] <- f_calc_svy("lshtm_prop_f", df, 2, years)
  out["Crude birth rate", ] <- f_calc_svy("lshtm_cbr_est", df, 1, years)
  out["Net migration rate", ] <- f_calc_svy("lshtm_net_migration_rate_est", df, 1, years)
  out["Injury-specific death rate", ] <- f_calc_svy("final_cdr_inj_est", df, 2, years)

  #...................................  
  ## Writing output to a file
  write.csv(out, paste(country, "_out_crude_survey_statistics.csv", sep="") )    

    
#.........................................................................................
### Producing graphs and figures of interesting statistics
#.........................................................................................
  
  #...................................  
  ## Survey data availability - household-level regression
  x_labels_m <- t_units[, "m"]
  x_labels_m[seq(1, length(x_labels_m), by=2)] <- ""
  x_labels_y <- unique(ts[, c("tm", "y")])[, "y"]
  
  plot <- ggplot(subset(hh_data_avail, y >= y_excess_start), aes(x = tm, y = stratum) ) +
    geom_tile(aes(fill=data_availability), colour = "grey80", show.legend = TRUE) + 
    scale_x_continuous("month, year", expand = c(0,0), breaks = unique(ts[, c("tm", "m")])[, "tm"], labels = x_labels_m ) + 
    scale_y_discrete(admin2_name, expand = c(0,0) ) + 
    scale_fill_gradientn("data availability", colours = c("grey90", palette_cb[5], palette_cb[7]), values = c(0, 0.0000001, 1 )) +
    facet_grid(admin1 ~ y, space = "free", scales = "free", switch="x") +
    theme_bw() +
    theme(legend.position = "top", strip.placement = "outside",
          strip.background = element_rect(fill = NA, colour = "grey50"),
          panel.spacing=unit(0,"cm"), strip.text.y = element_text(angle = 0))
  
  plot
  ggsave(paste(country, "_svy_coverage_hh.png", sep=""), height = 35, width = 30, units = "cm", dpi = "print")
         
  
  # #...................................  
  # ## Survey data availability - stratum-level regression  
  # plot <- ggplot(subset(stratum_data_avail, y >= y_excess_start), aes(x = tm, y = stratum) )
  # plot <- plot + geom_tile(aes(fill=data_availability), colour = "grey80", show.legend = FALSE) + 
  #   scale_x_continuous("month, year", expand=c(0,0), breaks = unique(ts[, c("tm", "m")])[, "tm"], labels = x_labels_m ) + 
  #   scale_y_discrete(admin2_name, expand=c(0,0) ) + 
  #   scale_fill_gradientn(colours = c("grey90", "yellow", "red"), 
  #                        values = c(0, 0.0000001, 1 )) +
  #   facet_grid(admin1 ~ y, space="free", scales="free", switch="x") +
  #   theme_bw() +
  #   theme(strip.placement = "outside",
  #         strip.background = element_rect(fill=NA, colour="grey50"),
  #         panel.spacing=unit(0,"cm"), strip.text.y = element_text(angle = 0))
  # 
  # plot
  # ggsave(paste(country, "_svy_coverage_stratum.png", sep=""), height = 35, width = 30, units = "cm", dpi = "print")
  
  #...................................   
  ## Histogram of survey quality score  
  plot <- ggplot(subset(surveys, exclude == "N"), aes(x = quality_score))
  plot <- plot + geom_histogram(fill = palette_cb[3], colour = palette_cb[6] , alpha = 0.5) + theme_bw() +
    labs(x = "survey quality score", y = "number of surveys")
  plot
  ggsave(paste(country, "_svy_quality.png", sep=""), height = 10, width = 15, units = "cm", dpi = "print")

  
  #...................................  
  ## Trends over time in key demographic indicators
    # List of indicators of interest
    indicators <- c("final_cdr_est", "final_cdr_u5_est", "final_cdr_inj_est",	"final_cdr_rr_males", 
                    "lshtm_prop_f", "lshtm_net_migration_rate_est")
    names(indicators) <- c("crude death rate (per 10,000 person-days)", "under 5 years death rate (per 10,000 child-days)", 
                           "injury-specific death rate (per 10,000 person-days)", 
                           "relative risk of dying (males vs. females)", "proportion of females in household", 
                           "net household migration rate (per 1000 person-years)")
    
    # First need to generate dataframe in which every survey's estimates of each indicator are distributed across the recall period
    x1 <- surveys_cov
      # turn month_coverage into a binary variable (yes/no)
      x1[, "month_coverage"] <- ifelse(x1[, "month_coverage"] == 0, 0, 1)
    
      # merge in indicators for each survey, deleting values for months that the survey did not cover
      for (i in indicators) {
        x1 <- merge(x1, surveys[, c("survey_id", paste(i) )], by="survey_id", sort=TRUE)
        x1[, paste(i)] <- ifelse(x1[, "month_coverage"] == 0, NA, x1[, paste(i)])
      
      }
      
      # also merge admin1 as well as year and month variables to group observations in graph  
      if (! "admin1" %in% colnames(surveys)) {
        surveys <- merge(surveys, unique(strata[, c("stratum", "admin1")]), by = "stratum", 
                       all.y=FALSE, all.x=TRUE, sort=TRUE) }
      x1 <- merge(x1, surveys[, c("survey_id", "admin1")], by="survey_id", sort=TRUE)
      x1 <- merge(x1, strata, by="admin1", sort=TRUE)
      x1 <- merge(x1, unique(ts))
      
      # create dates
      x1[, "date_recall"] <- date(paste(x1[, "y"], x1[, "m"], 1, sep="/"))
      
    # Plot for each indicator  
      # create breaks for years
        # create unique instances of dates
        x2 <- unique(x1[, "date_recall"])
        # extract their month values
        x3 <- month(x2)
        # cbind the two and select only January months
        x2 <- data.frame(x2, x3)
        year_breaks <- subset(x2, x3==1)[, "x2"]
      
      # for each indicator, generate the same plot structure...
      for (i in 1:length(indicators) )  {
       x2 <- subset(x1, y %in% c(y_analysis_start:y_analysis_end) )[, c("y", "m", "date_recall", "survey_id", "admin1", indicators[i])]
       colnames(x2)[colnames(x2) == indicators[i]] <- "var" 
       plot <- ggplot(subset(x2, y >= y_excess_start), aes(x = date_recall, y = var, group = survey_id) )
       plot <- plot + geom_point( aes(color = admin1) ) + geom_line( aes(color = admin1) ) + 
         theme_bw() + theme(plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm") ) +
         labs(x = "\nmonth", y = names(indicators[i]) ) +
         geom_vline(xintercept = year_breaks, color="grey50") +
         scale_colour_manual(values = c(palette_cb[c(4,6,7)], brewer.pal(8, "Dark2"))[1:length(unique(x2$admin1))] ) +
         guides(color = guide_legend(admin1_name)) +
         scale_x_date("\nmonth, year", expand=c(0,0) , minor_breaks=NULL, date_breaks="3 months", date_labels = "%b-%Y") +
         theme(plot.title = element_text(color="grey30"), legend.title = element_text(color="grey30"),
          axis.title.x = element_text(color="grey30", size = 10),
          axis.text.x = element_text(size = 10, angle=45, hjust = 1),
          axis.title.y = element_text(color="grey30", size = 10),
          legend.text = element_text(color="grey30", size = 10)
          )
       
       # add horizontal line denoting RR = 1 for corresponding graph
       if ( indicators[i] == "final_cdr_rr_males") { plot <- plot + geom_hline(yintercept = 1, color="red") }
       
       print(plot)

      # Assign plot name and save
        # plot name
        x9 <- paste(country, "_svy_trends_", indicators[i], ".png", sep = "")
        # assign name
        assign(x9, plot)
        # save
        ggsave(x9, height = 20, width = 20, units = "cm", dpi = "print")

      }  

    # Composite plot showing all indicators
    plot <- ggarrange(
      get(paste(country, "_svy_trends_", indicators[1], ".png", sep = "")) + rremove("x.title") + theme(axis.text.x=element_blank() ), 
      get(paste(country, "_svy_trends_", indicators[2], ".png", sep = "")) + rremove("x.title") + theme(axis.text.x=element_blank() ), 
      get(paste(country, "_svy_trends_", indicators[3], ".png", sep = "")) + rremove("x.title") + theme(axis.text.x=element_blank() ),
      get(paste(country, "_svy_trends_", indicators[4], ".png", sep = "")) + rremove("x.title") + theme(axis.text.x=element_blank() ),
      get(paste(country, "_svy_trends_", indicators[5], ".png", sep = "")) + rremove("x.title"), 
      get(paste(country, "_svy_trends_", indicators[6], ".png", sep = "")) + rremove("x.title"), 
      ncol = 2, nrow = 3, common.legend = TRUE, legend = "bottom", align = "v") 
    plot
    
    ggsave(paste(country, "_svy_trends.png", sep = ""), height = 32, width = 25, units = "cm", dpi = "print")
        
  
#.........................................................................................
### ENDS
#.........................................................................................
