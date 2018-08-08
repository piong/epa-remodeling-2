########################################
##  FOR STREAMS ONLY ALL COMPARISONS  ##
########################################

# Reading in the data for Streams Only
s_change <- NRSA14_Change_Estimates_Streams_20171127_withMSBasin_AcidMD_combEnte.csv

s_condition <- NRSA1314_StreamsCondition_WGTSP_vsWSA_Estimates_030818_MM.csv

#//////////////////////////////////////////////#
#           CLEANING CONDITION FILE            #
#//////////////////////////////////////////////#

    ## Renaming Enterococci variable to Enterococci_combined, to facilitate joining
    s_condition$Indicator <- recode(s_condition$Indicator, "Enterococci" = "Enterococci_combined" )

    ####################################################
    ##  Extracting Condition column from Change data  ##
    ####################################################

    # # Columns with _2 in the Streams Only change dataset become the condition data for that set
    # s_condition <- filter(s_change, Survey_1 == T0, Survey_2 == T2) # Only looking at T0vT2
    # # As of March 12 2018, Streams Condition Status data was provided separately for T2 (using T0 v T2). Data includes previously missing indicators not
    # # found in Change T0 v T2.
    
    ## Confidence interval calculation for Condition
    s_condition$CI95.P <- (s_condition$UCB95Pct.P-s_condition$LCB95Pct.P)
    
    s_condition2 <- subset(s_condition, select=c("Type", "Subpopulation", "Indicator", "Category", "NResp", "Estimate.P", "StdError.P", "LCB95Pct.P", "UCB95Pct.P", "CI95.P", "Estimate.U", "StdError.U", "LCB95Pct.U", "UCB95Pct.U")) %>%
      rename(c("NResp" = "Count.NResp.T2.Status.Condition",
               "Estimate.P" = "Estimate.P.T2.Status.Condition",
               "StdError.P" = "StdError.P.T2.Status.Condition", 
               "LCB95Pct.P" = "LCB95.P.T2.Status.Condition", 
               "UCB95Pct.P" = "UCB95.P.T2.Status.Condition",
               "CI95.P" = "CI95.P.T2.Status.Condition",
               "Estimate.U" = "Estimate.U.T2.Status.Condition", 
               "StdError.U" = "StdError.U.T2.Status.Condition", 
               "LCB95Pct.U" = "LCB95.U.T2.Status.Condition", 
               "UCB95Pct.U" = "UCB95.U.T2.Status.Condition"))
    
    ## Matching to Plain Language names
    s_condition3 <- matchplainlang(s_condition2)

    ######################################
    ##  Creating National data columns  ##
    ######################################

    ## THIS SECTION EDITED OUT ON MAY 24 2018.
    ## This is due to the NationalRef columns being joined on the raw indicator names, leaving the values to be incorrect for some rows.
    ## The creation and joining of the NationalRef data has now been moved to after the JOINING .ALL COLUMNS section, and just before REARRANGING COLUMNS.
    ## 
    ## Adding national data to every row for creation of grey bands as national comparisons
    # s_national <- filter(s_condition, Subpopulation == 'National') %>%
    #   # Extracting Indicator, Category, Estimate.P.stat, LCB95Pct.P.stat, UCB95Pct.P.stat, LCB95Pct.U.stat, UCB95Pct.U.stat
    #   subset(select=c("Indicator", "Category", "Estimate.P", "LCB95Pct.P", "UCB95Pct.P", "LCB95Pct.U", "UCB95Pct.U")) %>%
    #   # Renaming columns
    #   rename(c("Estimate.P" = "NationalRef.Estimate.P.T2.Status.Condition",
    #            "LCB95Pct.P" = "NationalRef.LCB95.P.T2.Status.Condition",
    #            "UCB95Pct.P" = "NationalRef.UCB95.P.T2.Status.Condition",
    #            "LCB95Pct.U" = "NationalRef.LCB95.U.T2.Status.Condition",
    #            "UCB95Pct.U" = "NationalRef.UCB95.U.T2.Status.Condition"))
    
    
    ## Condition file with pivoted National data
    # s_condition3 <- left_join(s_condition3, s_national, by = c("Category" = "Category", "Indicator" = "Indicator"))
    # s_condition3 <- s_condition2
    s_condition3$ID2Condition <- paste(s_condition3$Type, s_condition3$Subpopulation, s_condition3$Indicator,  sep="-")
    
    ## Renaming file for joining
    s_condition_join <- s_condition3

    # View(filter(s_condition_join, Indicator == "Enterococci_combined"))

#//////////////////////////////////////////////#
#             CLEANING CHANGE FILE             #
#//////////////////////////////////////////////#

    ## Significance flag calculation for Change
    s_change$Sig95.Flag.Diff <- ifelse(s_change$LCB95Pct.P*s_change$UCB95Pct.P>0, "Yes", "No") # Column 34
    
    ## Confidence Interval calculation for Change
    s_change2 <- mutate(s_change, CI95.P.Diff = (UCB95Pct.P - LCB95Pct.P)) # Column 35
    
    ## Matching to Plain Language names
    s_change3 <- matchplainlang(s_change2)


    ## Splitting up by time comparison periods
    s_change_T1vT2 <- subsetsurvey(s_change3, T1, T2) %>%
      cleannames(., T1, T2) %>%
      assignnames(., T1, T2)
    
    s_change_T0vT1 <- subsetsurvey(s_change3, T0, T1) %>%
      cleannames(., T0, T1) %>%
      assignnames(., T0, T1)
    
    s_change_T0vT2 <- subsetsurvey(s_change3, T0, T2) %>%
      cleannames(., T0, T2) %>%
      assignnames(., T0, T2)
    
    ## Horizontally joining all three time period comparisons together
    s_change4 <- full_join(s_change_T1vT2, s_change_T0vT1, by = c(names_other, names_raw, names_pl)) %>%
      full_join(s_change_T0vT2, by = c(names_other, names_raw, names_pl))
    
    ## Renaming file for joining
    s_change_join <- s_change4

    
#//////////////////////////////////////////////#
#     JOINING CONDITION AND CHANGE FILES       #
#//////////////////////////////////////////////#

    # Joining padding separately with condition and change. To ensure that there's no bias in order of operations.
    s_join_conditionpad <- left_join(padding_raw, s_condition_join, by=names(padding_raw)) %>%
      rename(c("Indicator" = "IndicatorCondition"))
    
    s_join_changepad <- left_join(padding_raw_keep, s_change_join, by=names(padding_raw)) %>%
      rename(c("Indicator" = "IndicatorChange"))
    
    s_change_and_condition <- inner_join(s_join_changepad, s_join_conditionpad, by=names(padding_raw))
    
    # Joining with padding (redundant step as of 19th dec 2017 update)
    s_change_and_condition2 <- s_change_and_condition
    
    # Creating another dummy Flag column that removes duplicate Riparian Disturbance. Not amending it in the reference tables file yet.
    s_change_and_condition2$RD_IndicatorConditionFlag <- s_change_and_condition2$IndicatorConditionFlag
    s_change_and_condition2$RD_IndicatorConditionFlag[s_change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & s_change_and_condition2$Category == "High"] <- "Remove"
    s_change_and_condition2$RD_IndicatorConditionFlag[s_change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & s_change_and_condition2$Category == "Low"] <- "Remove"
    s_change_and_condition2$RD_IndicatorConditionFlag[s_change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & s_change_and_condition2$Category == "Moderate"] <- "Remove"
    
    # RD_IndicatorConditionFlag removes imputed RD rows along with other "Remove" rows for indicators.
    s_change_and_condition2 <- filter(s_change_and_condition2, RD_IndicatorConditionFlag == "Keep", SubpopulationFlag == "Keep") %>%
      subset(., select = -c(IndicatorConditionFlag, SubpopulationFlag, RD_IndicatorConditionFlag, ID2Condition))
    
    s_change_and_condition2$ID3ConditionChange <- paste(s_change_and_condition2$Type, s_change_and_condition2$Subpopulation, s_change_and_condition2$IndicatorPlainLanguage,  sep="-")
    
    # Connecting to subpopulation abbreviation column
    s_change_and_condition2 <- left_join(s_change_and_condition2, abbr_subpop, by=c("Subpopulation", "SubpopulationPlainLanguage"))
    
#//////////////////////////////////////////////#
#           CREATING NEW .ALL COLUMNS          #
#//////////////////////////////////////////////#    

# For T0 use either of the P_1 values from the two comparisons involving WSA (either T0 v T1, or T0 v T2), since those values are equivalent
# For T1, use P_2 from the WSA and NRSA 0809 (T0 v T1) comparison
# For T2, use P_2 from the WSA and NRSA 1314 (T0 v T2) comparison
    
#####        
    # all_df_colname <- c("Count.NResp.T0.All.Condition",
    #                     "Count.NResp.T1.All.Condition",
    #                     "Count.NResp.T2.All.Condition",
    #                     "Estimate.P.T0.All.Condition",
    #                     "Estimate.P.T1.All.Condition",
    #                     "Estimate.P.T2.All.Condition",
    #                     "Estimate.U.T0.All.Condition",
    #                     "Estimate.U.T1.All.Condition",
    #                     "Estimate.U.T2.All.Condition",
    #                     "LCB95.P.T0.All.Condition",
    #                     "LCB95.P.T1.All.Condition",
    #                     "LCB95.P.T2.All.Condition",
    #                     "LCB95.U.T0.All.Condition",
    #                     "LCB95.U.T1.All.Condition",
    #                     "LCB95.U.T2.All.Condition",
    #                     "UCB95.P.T0.All.Condition",
    #                     "UCB95.P.T1.All.Condition",
    #                     "UCB95.P.T2.All.Condition",
    #                     "UCB95.U.T0.All.Condition",
    #                     "UCB95.U.T1.All.Condition",
    #                     "UCB95.U.T2.All.Condition")
#####    

    all_df <- subset(s_change_and_condition2, select=c("Type",
                                                       "TypePlainLanguage",
                                                       "Subpopulation",
                                                       "SubpopulationPlainLanguage",
                                                       "MetricCategory",
                                                       "IndicatorPlainLanguage",
                                                       "Category",
                                                       "CategoryConditionPlainLanguage",
                                                       "Count.NResp.T0.T0vT1.Condition",
                                                       "Count.NResp.T1.T0vT1.Condition",
                                                       "Count.NResp.T2.T0vT2.Condition",
                                                       "Estimate.P.T0.T0vT1.Condition",
                                                       "Estimate.P.T1.T0vT1.Condition",
                                                       "Estimate.P.T2.T0vT2.Condition",
                                                       "Estimate.U.T0.T0vT1.Condition",
                                                       "Estimate.U.T1.T0vT1.Condition",
                                                       "Estimate.U.T2.T0vT2.Condition",
                                                       "LCB95.P.T0.T0vT1.Condition",
                                                       "LCB95.P.T1.T0vT1.Condition",
                                                       "LCB95.P.T2.T0vT2.Condition",
                                                       "LCB95.U.T0.T0vT1.Condition",
                                                       "LCB95.U.T1.T0vT1.Condition",
                                                       "LCB95.U.T2.T0vT2.Condition",
                                                       "UCB95.P.T0.T0vT1.Condition",
                                                       "UCB95.P.T1.T0vT1.Condition",
                                                       "UCB95.P.T2.T0vT2.Condition",
                                                       "UCB95.U.T0.T0vT1.Condition",
                                                       "UCB95.U.T1.T0vT1.Condition",
                                                       "UCB95.U.T2.T0vT2.Condition"))

    names(all_df) <- sub("T0vT1|T0vT2", "All", names(all_df))

    all_df2 <- all_df

#//////////////////////////////////////////////#
#             JOINING .ALL COLUMNS             #
#//////////////////////////////////////////////#
    
    #all_df2 dataframe created in Padding.R script
    s_change_and_condition3 <- left_join(s_change_and_condition2, all_df2, by=c("Type", "TypePlainLanguage", "Subpopulation", "SubpopulationPlainLanguage", "MetricCategory", "IndicatorPlainLanguage", "Category", "CategoryConditionPlainLanguage"))

        
#//////////////////////////////////////////////#
#          CREATING NATIONAL COLUMNS           #
#//////////////////////////////////////////////#
## This new section was added on May 24th 2018. It was moved down here from the front, where the order of executions is critical in carrying out the data modeling.    

## Adding national data to every row for creation of grey bands as national comparisons

## Creating the National dataset separately

## Adding national data to every row for creation of grey bands as national comparisons
s_national <- filter(s_condition, Subpopulation == 'National') %>%
  # Extracting Indicator, Category, Estimate.P.stat, LCB95Pct.P.stat, UCB95Pct.P.stat, LCB95Pct.U.stat, UCB95Pct.U.stat
  subset(select=c("Indicator", "Category", "Estimate.P", "LCB95Pct.P", "UCB95Pct.P", "LCB95Pct.U", "UCB95Pct.U")) %>%
  # Renaming columns
  rename(c("Estimate.P" = "NationalRef.Estimate.P.T2.Status.Condition",
           "LCB95Pct.P" = "NationalRef.LCB95.P.T2.Status.Condition",
           "UCB95Pct.P" = "NationalRef.UCB95.P.T2.Status.Condition",
           "LCB95Pct.U" = "NationalRef.LCB95.U.T2.Status.Condition",
           "UCB95Pct.U" = "NationalRef.UCB95.U.T2.Status.Condition"))

## Joining the National dataset to plain language columns
s_national2 <- left_join(s_national, pl_only_indicator) %>% 
  left_join(pl_only_category) %>% 
  unique(.) %>% 
  filter(Category!="Total") %>% 
  subset(select=-c(Category))

## Joining the new National dataset to the rest of the previously modelled data
s_change_and_condition4 <- left_join(s_change_and_condition3, s_national2, by = c("CategoryConditionPlainLanguage" = "CategoryConditionPlainLanguage", "IndicatorPlainLanguage" = "IndicatorPlainLanguage"))
    
    
#//////////////////////////////////////////////#
#             REARRANGING COLUMNS              #
#//////////////////////////////////////////////#
    
    # Rearranging for easier human readability
    s_change_and_condition5 <- s_change_and_condition4[c("Type",                          
                                                         "Subpopulation",                 
                                                         "IndicatorCondition",
                                                         "IndicatorChange",
                                                         "TypePlainLanguage",
                                                         "SubpopulationPlainLanguage",
                                                         "SubpopAbbrev",
                                                         "IndicatorPlainLanguage",
                                                         "Category", 
                                                         "CategoryConditionPlainLanguage",
                                                         "CategoryConditionMostDisturbed",
                                                         "MetricCategory",
                                                         "ID3ConditionChange", 
                                                         
                                                         # Condition
                                                         "Count.NResp.T2.Status.Condition", 
                                                         "Estimate.P.T2.Status.Condition",  
                                                         "StdError.P.T2.Status.Condition",  
                                                         "LCB95.P.T2.Status.Condition",     
                                                         "UCB95.P.T2.Status.Condition",     
                                                         "CI95.P.T2.Status.Condition",
                                                         "Estimate.U.T2.Status.Condition",  
                                                         "StdError.U.T2.Status.Condition",  
                                                         "LCB95.U.T2.Status.Condition",     
                                                         "UCB95.U.T2.Status.Condition",
                                                         
                                                         # National Reference Range                   
                                                         "NationalRef.Estimate.P.T2.Status.Condition",
                                                         "NationalRef.LCB95.P.T2.Status.Condition",
                                                         "NationalRef.UCB95.P.T2.Status.Condition",
                                                         "NationalRef.LCB95.U.T2.Status.Condition",
                                                         "NationalRef.UCB95.U.T2.Status.Condition",           
                                                         
                                                         #.All
                                                         "Count.NResp.T0.All.Condition",
                                                         "Count.NResp.T1.All.Condition",
                                                         "Count.NResp.T2.All.Condition",

                                                         "Estimate.P.T0.All.Condition",
                                                         "Estimate.P.T1.All.Condition",
                                                         "Estimate.P.T2.All.Condition",

                                                         "Estimate.U.T0.All.Condition",
                                                         "Estimate.U.T1.All.Condition",
                                                         "Estimate.U.T2.All.Condition",

                                                         "LCB95.P.T0.All.Condition",
                                                         "LCB95.P.T1.All.Condition",
                                                         "LCB95.P.T2.All.Condition",

                                                         "LCB95.U.T0.All.Condition",
                                                         "LCB95.U.T1.All.Condition",
                                                         "LCB95.U.T2.All.Condition",

                                                         "UCB95.P.T0.All.Condition",
                                                         "UCB95.P.T1.All.Condition",
                                                         "UCB95.P.T2.All.Condition",

                                                         "UCB95.U.T0.All.Condition",
                                                         "UCB95.U.T1.All.Condition",
                                                         "UCB95.U.T2.All.Condition",
                                                         
                                                         # T1vT2                 
                                                         "Estimate.P.Diff.T1vT2.Condition", 
                                                         "StdError.P.Diff.T1vT2.Condition", 
                                                         "LCB95.P.Diff.T1vT2.Condition",    
                                                         "UCB95.P.Diff.T1vT2.Condition",    
                                                         "CI95.P.Diff.T1vT2.Condition",
                                                         "Estimate.U.Diff.T1vT2.Condition", 
                                                         "Sig95.Flag.Diff.T1vT2.Condition", 
                                                         
                                                         "Count.NResp.T1.T1vT2.Condition",  
                                                         "Estimate.P.T1.T1vT2.Condition",   
                                                         "StdError.P.T1.T1vT2.Condition",   
                                                         "LCB95.P.T1.T1vT2.Condition",      
                                                         "UCB95.P.T1.T1vT2.Condition",      
                                                         "Estimate.U.T1.T1vT2.Condition",   
                                                         "StdError.U.T1.T1vT2.Condition",   
                                                         "LCB95.U.T1.T1vT2.Condition",      
                                                         "UCB95.U.T1.T1vT2.Condition",      
                                                         
                                                         "Count.NResp.T2.T1vT2.Condition",  
                                                         "Estimate.P.T2.T1vT2.Condition",   
                                                         "StdError.P.T2.T1vT2.Condition",   
                                                         "LCB95.P.T2.T1vT2.Condition",      
                                                         "UCB95.P.T2.T1vT2.Condition",      
                                                         "Estimate.U.T2.T1vT2.Condition",   
                                                         "StdError.U.T2.T1vT2.Condition",   
                                                         "LCB95.U.T2.T1vT2.Condition",      
                                                         "UCB95.U.T2.T1vT2.Condition",      
                                                         
                                                         
                                                         # T0vT1
                                                         "Estimate.P.Diff.T0vT1.Condition",  
                                                         "StdError.P.Diff.T0vT1.Condition", 
                                                         "LCB95.P.Diff.T0vT1.Condition",    
                                                         "UCB95.P.Diff.T0vT1.Condition",    
                                                         "Estimate.U.Diff.T0vT1.Condition",  
                                                         "Sig95.Flag.Diff.T0vT1.Condition", 
                                                         "CI95.P.Diff.T0vT1.Condition",
                                                         
                                                         "Count.NResp.T0.T0vT1.Condition",  
                                                         "Estimate.P.T0.T0vT1.Condition",   
                                                         "StdError.P.T0.T0vT1.Condition",   
                                                         "LCB95.P.T0.T0vT1.Condition",      
                                                         "UCB95.P.T0.T0vT1.Condition",      
                                                         "Estimate.U.T0.T0vT1.Condition",   
                                                         "StdError.U.T0.T0vT1.Condition",   
                                                         "LCB95.U.T0.T0vT1.Condition",      
                                                         "UCB95.U.T0.T0vT1.Condition",      
                                                         
                                                         "Count.NResp.T1.T0vT1.Condition",  
                                                         "Estimate.P.T1.T0vT1.Condition",   
                                                         "StdError.P.T1.T0vT1.Condition",   
                                                         "LCB95.P.T1.T0vT1.Condition",      
                                                         "UCB95.P.T1.T0vT1.Condition",      
                                                         "Estimate.U.T1.T0vT1.Condition",   
                                                         "StdError.U.T1.T0vT1.Condition",   
                                                         "LCB95.U.T1.T0vT1.Condition",      
                                                         "UCB95.U.T1.T0vT1.Condition",
                                                         
                                                         
                                                         # T0vT2
                                                         "Estimate.P.Diff.T0vT2.Condition",   
                                                         "StdError.P.Diff.T0vT2.Condition", 
                                                         "LCB95.P.Diff.T0vT2.Condition",    
                                                         "UCB95.P.Diff.T0vT2.Condition",    
                                                         "Estimate.U.Diff.T0vT2.Condition",  
                                                         "Sig95.Flag.Diff.T0vT2.Condition", 
                                                         "CI95.P.Diff.T0vT2.Condition",
                                                         
                                                         "Count.NResp.T0.T0vT2.Condition",  
                                                         "Estimate.P.T0.T0vT2.Condition",   
                                                         "StdError.P.T0.T0vT2.Condition",   
                                                         "LCB95.P.T0.T0vT2.Condition",      
                                                         "UCB95.P.T0.T0vT2.Condition",      
                                                         "Estimate.U.T0.T0vT2.Condition",   
                                                         "StdError.U.T0.T0vT2.Condition",   
                                                         "LCB95.U.T0.T0vT2.Condition",      
                                                         "UCB95.U.T0.T0vT2.Condition",      
                                                         
                                                         "Count.NResp.T2.T0vT2.Condition",  
                                                         "Estimate.P.T2.T0vT2.Condition",   
                                                         "StdError.P.T2.T0vT2.Condition",   
                                                         "LCB95.P.T2.T0vT2.Condition",      
                                                         "UCB95.P.T2.T0vT2.Condition",      
                                                         "Estimate.U.T2.T0vT2.Condition",   
                                                         "StdError.U.T2.T0vT2.Condition",   
                                                         "LCB95.U.T2.T0vT2.Condition",      
                                                         "UCB95.U.T2.T0vT2.Condition")]


    ## Saving
    ## writecsv(s_change_and_condition4, "NRSA14_Change_and_Condition_Streams_AllT_NotImputed_withAll")


#//////////////////////////////////////////////#
#               IMPUTING VALUES                #
#//////////////////////////////////////////////#
    
    #############################################
    ##    IMPUTING NA'S TO 0'S  (VERTICALLY)   ##
    #############################################

    # This looks for instances where, within the condition options for an indicator at a specific site (i.e. Categories for a Type-Subpopulation-Indicator combination),
    # there are partial NA's. As those NA's should actually be 0's, this converts those accordingly. For example, if N. Appalachians, Phosphorus only has data for Good and Poor, but NA for Fair,
    # we will convert the NA for Fair into 0.
    # This is done for the Count.NResp.T3.Status.Condition, Estimate.P, and Estimate.U columns.
    
    df <- s_change_and_condition5
    ID2 <- unique(df$ID3ConditionChange)
    
    ## For Status in T2
    # Duplicating .Status columns that will be imputed. oldcol is a custom function defined in the Functions script.
    list_status <- as.character()
    list_status <- imputecollist(front, "T2", "Status", "Condition")
    
    df2 <- oldcol(df, list_status)

    # Imputing 0's for columns listed in list_status
    df3 <- imputezero1(df2, list_status)
    
    ## For .All columns
    list_all <- names(all_df2)
    list_all <- list_all[-c(grep("Type|TypePlainLanguage|Subpopulation|SubpopulationPlainLanguage|MetricCategory|IndicatorPlainLanguage|Category|LCB95|UCB95", list_all))]

    df3 <- imputezero1(df3, list_all)
    
    
    # For all Change columns listed in list_change
    # Duplicating .Status columns that will be imputed. oldcol is a custom function defined in the Functions script.
    list_change <- as.character()
    list_change <- imputecollist(front, "", tvt, "Condition")
    
    df3 <- oldcol(df3, list_change)
    
    df4 <- imputezero1(df3, list_change)

    
    # Imputing NationalRef column
    list_national <- as.character()
    list_national <- "NationalRef.Estimate.P.T2.Status.Condition"
    
    df4 <- oldcol (df4, list_national)
    df5 <- imputezero1(df4, list_national)
        
    ###############################################
    ##    IMPUTING NA'S TO 0'S  (HORIZONTALLY)   ##
    ###############################################
    
    # This compares columns within the same time comparison period, and ensures that either both Survey_1 and Survey_2 are NA or have data.
    # If only one of the surveys is NA, it should be imputed to 0.
    # If the dataset is formatted properly, there should be no new imputations in this step as it would be caught by the previous round.
    
    # df5 <- df4
    # 
    # df6 <- imputezero2(df5, tvt, front, list_change)
    
    
#//////////////////////////////////////////////#
#            RECALCULATING VALUES              #
#//////////////////////////////////////////////#
    
    ########################################
    ##     RECALCULATING .DIFF VALUES     ##
    ########################################
    
    # This step recalculates the .Diff value between Survey_1 and Survey_2 for each time period comparison
    
    # Duplicating old columns before imputing
    list_diff <- as.character()
    list_diff <- imputecollist(front, "Diff", tvt, "Condition")
    list_diff <- list_diff[-c(grep("Count.NResp", list_diff))] # Removing Count.NResp as the .Diff columns are not included in the raw data
    
    df7 <- oldcol(df5, list_diff) # Using df4 after decision on 2018-02-15 to do away with the horizontal imputation step.
    
    # Recalculating the difference for columns combined with items in the front and tvt lists
    df8 <- recalcdiff(df7, front, tvt)

    # Before recalculation, there are ~186 records (per time period comparison) where .Diff has NA when it should have a number. I use this to check that the recalculation has been done.
    # Console should print out 0 after column names
    # dummy <- recalcdiff(df8, front, tvt)
    
    #############################################
    ##     RECALCULATING SIGNIFICANCE FLAG     ##
    #############################################
    
    # Imputing "No" for Significance flag after other columns have been imputed. Check Acidification
    # This is likely to yield no actual changes if the data received is already fairly clean. This step is performed as an added precaution
    list_sig <- as.character()
    list_sig <- imputecollist("Sig95.Flag", "Diff", tvt, "Condition")
    list_sig <- list_sig[-c(grep("NA", list_sig))]
    
    df9 <- oldcol(df8, list_sig)
    
    df10 <- recalcsig(df9, tvt)
    
    ## Removing ID3ConditionChange column
    df11 <- subset(df10, select = -c(ID3ConditionChange))
    

#//////////////////////////////////////////////#
#              FILTERING COLUMNS               #
#//////////////////////////////////////////////#

    # Removing columns containin .U, StdError, or _OLD for a more concise dataset
    
    df12 <- subset(df11, select=-c(grep("\\.U\\.", names(df11))))%>%
      subset(., select=-c(grep("StdError", names(.)))) %>%
      subset(., select=-c(grep("_OLD", names(.)))) %>%
      subset(., select=-c(grep("\\T1vT2\\.", names(.)))) %>% 
      subset(., select=-c(grep("\\T0vT1\\.", names(.)))) %>% 
      unique(.)
    
    
    # Saving
    writecsv(df12, "NRSA14_Condition_StatusChange_Streams_T0-T2")
