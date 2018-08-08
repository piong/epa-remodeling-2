###############################################
##  FOR RIVERS AND STREAMS ONLY, T1VT2 RISK  ##
###############################################

## This script is for Risk data only, for Rivers and Streams

#////////////////////////////////////////////#
#           PREPARING RISK FILES             #
#////////////////////////////////////////////#

    # We only want to use these columns out of the whole dataset
    risk_filter <- c("Estimate", "LCB95Pct", "UCB95Pct")
    
    # Reading in the Risk data (both attributable and relative)
    attrisk <- NRSA14_Attributable_Risk_Estimates_20171024.csv %>%
      subset(select=c(col_suffix, risk_filter))
    
    relrisk <- NRSA14_Relative_Risk_Estimates_20171024.csv %>%
      subset(select=c(col_suffix, risk_filter))
    
    
    # Reading in the Risk data for the new Mississippi Basin data
    attrisk_MB <- NRSA1314_AttributableRisk_MissBasin_MM_11302017.csv %>%
      subset(select=c(col_suffix, risk_filter))
    
    relrisk_MB <- NRSA1314_RelativeRisk_MissBasin_MM_11302017.csv %>%
      subset(select=c(col_suffix, risk_filter))
    
    # Combining Risk files
    attrisk <- bind_rows(attrisk, attrisk_MB)
    
    relrisk <- bind_rows(relrisk, relrisk_MB)
    
    #################### 
    # RENAMING COLUMNS #
    #################### 
    
    ## Appending column names to Risk files
    ## Doing this step first as .AttRisk and .RelRisk is associated with the files.
    ## This way I don't have to create a Source column.
    ## Make sure to update if T-periods change or are updated
    nam <- colnames(attrisk)
    names(attrisk) <- ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".P.T2.Status.AttRisk"))
    
    nam <- colnames(relrisk)
    names(relrisk) <- ifelse(nam %in% col_suffix, str_c(nam), str_c(nam, ".Ratio.T2.Status.RelRisk"))
    
    risk <- left_join(attrisk, relrisk, by=c(col_suffix))
    names(risk) <- sub("Pct", "", names(risk))
    names(risk) <- sub("NResp", "Count.NResp", names(risk))
    
    ## Replacing indicator names
    risk$ResponsePlainLanguage <- ifelse(risk$Response == "Fish_MMI", "Fish Condition",
                                         ifelse(risk$Response == "MacroInvert_MMI", "Macroinvertebrate Condition", ""))
    

#////////////////////////////////////////////#
#     JOINING RISK FILES TO PLAIN LANG       #
#////////////////////////////////////////////#
    
    risk2 <- left_join(risk, pl_subpoptype, by=c("Type" = "Type", "Subpopulation" = "Subpopulation")) %>%
      left_join(pl_only_stressor, by="Stressor") %>%
      unique()
        
    risk3 <- left_join(risk2, metric_categories, by= c("StressorPlainLanguage" = "IndicatorPlainLanguage")) %>%
      filter(SubpopulationFlag == "Keep") %>%
      subset(., select = -c(SubpopulationFlag))
    
    # Rearranging for easier human readability
    risk3 <- risk3[c("Type",
                     "Subpopulation",
                     "Stressor",
                     "Response",
                     "TypePlainLanguage",
                     "SubpopulationPlainLanguage",
                     "StressorPlainLanguage",
                     "ResponsePlainLanguage",
                     "MetricCategory",
                     "Estimate.P.T2.Status.AttRisk",
                     "LCB95.P.T2.Status.AttRisk",
                     "UCB95.P.T2.Status.AttRisk",
                     "Estimate.Ratio.T2.Status.RelRisk",
                     "LCB95.Ratio.T2.Status.RelRisk",                  
                     "UCB95.Ratio.T2.Status.RelRisk")] 

writecsv(risk3, "NRSA14_RiskOnly")
