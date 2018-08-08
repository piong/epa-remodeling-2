##########################################
##  FOR RIVERS AND STREAMS ONLY, T1VT2  ##
##########################################

## This file is for T1vT2 condition only, for Rivers and Streams
condition <- NRSA14_Condition_Estimates_20171020_withAcidMDandWholeMissBasin_FinalHg.csv

#//////////////////////////////////////////////#
#           CLEANING CONDITION FILE            #
#//////////////////////////////////////////////#


    ## Confidence interval calculation for Condition
    condition$CI95.P <- (condition$UCB95Pct.P-condition$LCB95Pct.P)
    
    ## THIS SECTION EDITED OUT ON MAY 24 2018.
    ## This is due to the NationalRef columns being joined on the raw indicator names, leaving the values to be incorrect for some rows.
    ## The creation and joining of the NationalRef data has now been moved to after the JOINING .ALL COLUMNS section, and just before REARRANGING COLUMNS.
    ##
    ## Adding national data to every row for creation of grey bands as national comparisons
    # national <- filter(condition, Subpopulation == 'National') %>%
    #   # Extracting Indicator, Category, Estimate.P.stat, LCB95Pct.P.stat, UCB95Pct.P.stat, LCB95Pct.U.stat, UCB95Pct.U.stat
    #   subset(select=c("Indicator", "Category", "Estimate.P", "LCB95Pct.P", "UCB95Pct.P", "LCB95Pct.U", "UCB95Pct.U")) %>%
    #   # Renaming columns
    #   rename(c("Estimate.P" = "NationalRef.Estimate.P",
    #            "LCB95Pct.P" = "NationalRef.LCB95.P",
    #            "UCB95Pct.P" = "NationalRef.UCB95.P",
    #            "LCB95Pct.U" = "NationalRef.LCB95.U",
    #            "UCB95Pct.U" = "NationalRef.UCB95.U"))
    
    ## Condition file with pivoted National data
    # condition2 <- left_join(condition, national, by = c("Category" = "Category", "Indicator" = "Indicator"))
    condition2 <- condition
    condition2$ID2Condition <- paste(condition2$Type, condition2$Subpopulation, condition2$Indicator,  sep="-")
    
    ## Appending '.T2.Status.Condition' to all columns in Condition data
    nam <- names(condition2)
    names(condition2) <- ifelse(nam %in% c(col_suffix,"Category", "Indicator", "ID2Condition"), str_c(nam), str_c(nam, '.T2.Status.Condition'))
    
    condition2$Indicator <- revalue(condition2$Indicator, c("Enterococci" = "Enterococci_combined"))
    
    ## Removing "Pct" from column names
    names(condition2) <- sub("Pct", "", names(condition2))
    
    ## Renaming "NResp" to "Count.NResp"
    names(condition2) <- sub("NResp", "Count.NResp", names(condition2))
    
    ## Matching to Plain Language names
    condition3 <- matchplainlang(condition2)


## Saving
# writecsv(condition3, "NRSA14_ConditionStatusOnly_")



#//////////////////////////////////////////////#
#             CLEANING CHANGE FILE             #
#//////////////////////////////////////////////#
    
    change <- NRSA14_Change_Estimates_RiversAndStreams_20171122_withMSBasin_AcidMD_combEnte_12182017.csv #T1vT2 Rivers and Streams only
    
    ## Significance flag calculation for Change
    change$Sig95.Flag.Diff <- ifelse(change$LCB95Pct.P*change$UCB95Pct.P>0, "Yes", "No") # Column 34
    
    ## Confidence Interval calculation for Change
    change <- mutate(change, CI95.P.Diff = (UCB95Pct.P - LCB95Pct.P)) # Column 35
    
    ## Matching to Plain Language names
    ## matchplainlang is a function defined in the Functions.R script
    change2 <- matchplainlang(change)
    
    ## Duplicate to keep steps separate
    change3 <- change2
    
    
    #################### 
    # RENAMING COLUMNS #
    #################### 
    
    # Splitting up data as EPA gave them to us combined, with all 3 time periods and comparisons
    # cleannames and assignnames are both functions defined in the Functions.R script
    change_T1vT2 <- subsetsurvey(change3, T1, T2) %>%
      cleannames(., T1, T2) %>%
      assignnames(., T1, T2)
    
    change_T0vT1 <- subsetsurvey(change3, T0, T1) %>%
      cleannames(., T0, T1) %>%
      assignnames(., T0, T1)
    
    change_T0vT2 <- subsetsurvey(change3, T0, T2) %>%
      cleannames(., T0, T2) %>%
      assignnames(., T0, T2)

    
    ## Horizontally joining all three time period comparisons together
    change4 <- full_join(change_T1vT2, change_T0vT1, by = c(names_other, names_raw, names_pl)) %>%
      full_join(change_T0vT2, by = c(names_other, names_raw, names_pl))

    ## Saving
    # writecsv(change4, "NRSA14_Change_Only_")


#//////////////////////////////////////////////#
#     JOINING CONDITION AND CHANGE FILES       #
#//////////////////////////////////////////////#

    #######################################
    ## FOR RIVERS AND STREAMS T1VT2 ONLY ##
    #######################################

    join_conditionpad <- left_join(padding_raw_keep, condition3, by=names(padding_raw)) %>%
      rename(c("Indicator" = "IndicatorCondition"))
    
    join_changepad <- left_join(padding_raw_keep, change4, by=names(padding_raw)) %>%
      rename(c("Indicator" = "IndicatorChange"))
    
    # Change4 does not have an ID2Condition column, so ID2Condition is combined with the P and N naming found in the Condition file
    change_and_condition <- inner_join(join_changepad, join_conditionpad, by=names(padding_raw))
    
    # Joining with padding (redundant step as of 19th dec 2017 update)
    change_and_condition2 <- change_and_condition
    
    # Creating another dummy Flag column that removes duplicate Riparian Disturbance. Not amending it in the reference tables file yet.
    change_and_condition2$RD_IndicatorConditionFlag <- change_and_condition2$IndicatorConditionFlag
    change_and_condition2$RD_IndicatorConditionFlag[change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & change_and_condition2$Category == "High"] <- "Remove"
    change_and_condition2$RD_IndicatorConditionFlag[change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & change_and_condition2$Category == "Low"] <- "Remove"
    change_and_condition2$RD_IndicatorConditionFlag[change_and_condition2$IndicatorPlainLanguage == "Riparian Disturbance" & change_and_condition2$Category == "Moderate"] <- "Remove"
    
    # RD_IndicatorConditionFlag removes imputed RD rows along with other "Remove" rows for indicators.
    change_and_condition2 <- filter(change_and_condition2, RD_IndicatorConditionFlag == "Keep", SubpopulationFlag == "Keep") %>%
      subset(., select = -c(IndicatorConditionFlag, SubpopulationFlag, RD_IndicatorConditionFlag))
    
    change_and_condition2$ID3ConditionChange <- paste(change_and_condition2$Type, change_and_condition2$Subpopulation, change_and_condition2$IndicatorPlainLanguage,  sep="-")
    
    # Connecting to subpopulation abbreviation column
    change_and_condition2 <- left_join(change_and_condition2, abbr_subpop, by=c("Subpopulation", "SubpopulationPlainLanguage"))
    
#//////////////////////////////////////////////#
#           CREATING NEW .ALL COLUMNS          #
#//////////////////////////////////////////////#
    
    ## .All columns are not yet provided in the raw rivers and streams data. This step just ensures that the columns are present despite data being NA.
    
    all_df_colname <- c("Count.NResp.T0.All.Condition",
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
                        "UCB95.U.T2.All.Condition")
    # 
    # all_df_values <- c(12,16,5,80,456,98,54,23,19,120,765,911,85,15,27,81,54,201,3,46,22)
    # 
    # all_df <- data.frame(x = all_df_colname, y= all_df_values)
    # all_df2 <- spread(all_df, key = x, value = y)
    # 
    
    # Creating a blank data frame with the right column names to pad the Rivers and Streams data in the meantime
    all_df_RS <- data.frame(matrix(ncol = 21, nrow = 1))
    colnames(all_df_RS) <- all_df_colname
    
    
#//////////////////////////////////////////////#
#             JOINING .ALL COLUMNS             #
#//////////////////////////////////////////////#
    
    change_and_condition3 <- merge(change_and_condition2, all_df_RS)

    
#//////////////////////////////////////////////#
#          CREATING NATIONAL COLUMNS           #
#//////////////////////////////////////////////#

## This new section was added on May 24th 2018. It was moved down here from the front, where the order of executions is critical in carrying out the data modeling.    
    
## Adding national data to every row for creation of grey bands as national comparisons

## Creating the National dataset separately
national <- filter(condition, Subpopulation == 'National') %>%
  # Extracting Indicator, Category, Estimate.P.stat, LCB95Pct.P.stat, UCB95Pct.P.stat, LCB95Pct.U.stat, UCB95Pct.U.stat
  subset(select=c("Indicator", "Category", "Estimate.P", "LCB95Pct.P", "UCB95Pct.P", "LCB95Pct.U", "UCB95Pct.U")) %>%
  # Renaming columns
  rename(c("Estimate.P" = "NationalRef.Estimate.P.T2.Status.Condition",
           "LCB95Pct.P" = "NationalRef.LCB95.P.T2.Status.Condition",
           "UCB95Pct.P" = "NationalRef.UCB95.P.T2.Status.Condition",
           "LCB95Pct.U" = "NationalRef.LCB95.U.T2.Status.Condition",
           "UCB95Pct.U" = "NationalRef.UCB95.U.T2.Status.Condition"))

## Joining the National dataset to plain language columns
national2 <- left_join(national, pl_only_indicator) %>% 
  left_join(pl_only_category) %>% 
  unique(.) %>% 
  filter(Category!="Total") %>% 
  subset(select=-c(Category)) ## The column named "Category" by this point has been renamed to "CategoryCondition" and "CategoryChange", so there is no longer one named just "Category" and keeping it will mess up the join.

## Joining the new National dataset to the rest of the previously modelled data
change_and_condition4 <- left_join(change_and_condition3, national2, by = c("CategoryConditionPlainLanguage" = "CategoryConditionPlainLanguage", "IndicatorPlainLanguage" = "IndicatorPlainLanguage"))

    
        
#//////////////////////////////////////////////#
#             REARRANGING COLUMNS              #
#//////////////////////////////////////////////#
    
    # Rearranging for easier human readability
    change_and_condition5 <- change_and_condition4[c("Type",
                                                     "Subpopulation",
                                                     "IndicatorCondition",
                                                     "IndicatorChange",
                                                     "TypePlainLanguage",
                                                     "SubpopulationPlainLanguage",
                                                     # "SubpopulationFlag",
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
                                                     "Estimate.U.T2.Status.Condition",
                                                     "StdError.U.T2.Status.Condition",
                                                     "LCB95.U.T2.Status.Condition",
                                                     "UCB95.U.T2.Status.Condition",
                                                     "CI95.P.T2.Status.Condition",
                                                     "NationalRef.Estimate.P.T2.Status.Condition",
                                                     "NationalRef.LCB95.P.T2.Status.Condition",
                                                     "NationalRef.UCB95.P.T2.Status.Condition",
                                                     "NationalRef.LCB95.U.T2.Status.Condition",
                                                     "NationalRef.UCB95.U.T2.Status.Condition",
                                                     
                                                     # Diff
                                                     "Estimate.P.Diff.T1vT2.Condition",
                                                     "StdError.P.Diff.T1vT2.Condition",
                                                     "LCB95.P.Diff.T1vT2.Condition",
                                                     "UCB95.P.Diff.T1vT2.Condition",
                                                     "Estimate.U.Diff.T1vT2.Condition",
                                                     #"StdError.U.Diff.T1vT2.Condition",
                                                     #"LCB95.U.Diff.T1vT2.Condition",         # .U's for these three were not included in the Oct/Nov dataset
                                                     #"UCB95.U.Diff.T1vT2.Condition",
                                                     
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
                                                     "Sig95.Flag.Diff.T1vT2.Condition",
                                                     "CI95.P.Diff.T1vT2.Condition")]
    
    #####
    # list <- c("Type",
    #           "Subpopulation",
    #           "IndicatorCondition",
    #           "IndicatorChange",
    #           "TypePlainLanguage",
    #           "SubpopulationPlainLanguage",
    #           "SubpopulationFlag",
    #           "IndicatorPlainLanguage",
    #           "Category",
    #           "CategoryConditionPlainLanguage",
    #           "MetricCategory",
    #           "ID2Condition",
    #           "Count.NResp.T2.Status.Condition",
    #           "Estimate.P.T2.Status.Condition",
    #           "StdError.P.T2.Status.Condition",
    #           "LCB95.P.T2.Status.Condition",
    #           "UCB95.P.T2.Status.Condition",
    #           "Estimate.U.T2.Status.Condition",
    #           "StdError.U.T2.Status.Condition",
    #           "LCB95.U.T2.Status.Condition",
    #           "UCB95.U.T2.Status.Condition",
    #           "CI95.P.T2.Status.Condition",
    #           "NationalRef.Estimate.P.T2.Status.Condition",
    #           "NationalRef.LCB95.P.T2.Status.Condition",
    #           "NationalRef.UCB95.P.T2.Status.Condition",
    #           "NationalRef.LCB95.U.T2.Status.Condition",
    #           "NationalRef.UCB95.U.T2.Status.Condition",
    #           "Estimate.P.Diff.T1vT2.Condition",
    #           "StdError.P.Diff.T1vT2.Condition",
    #           "LCB95.P.Diff.T1vT2.Condition",
    #           "UCB95.P.Diff.T1vT2.Condition",
    #           "Estimate.U.Diff.T1vT2.Condition",
    #           "StdError.U.Diff.T1vT2.Condition",
    #           "LCB95.U.Diff.T1vT2.Condition",
    #           "UCB95.U.Diff.T1vT2.Condition",
    #           "Count.NResp.T1.T1vT2.Condition",
    #           "Estimate.P.T1.T1vT2.Condition",
    #           "StdError.P.T1.T1vT2.Condition",
    #           "LCB95.P.T1.T1vT2.Condition",
    #           "UCB95.P.T1.T1vT2.Condition",
    #           "Estimate.U.T1.T1vT2.Condition",
    #           "StdError.U.T1.T1vT2.Condition",
    #           "LCB95.U.T1.T1vT2.Condition",
    #           "UCB95.U.T1.T1vT2.Condition",
    #           "Count.NResp.T2.T1vT2.Condition",
    #           "Estimate.P.T2.T1vT2.Condition",
    #           "StdError.P.T2.T1vT2.Condition",
    #           "LCB95.P.T2.T1vT2.Condition",
    #           "UCB95.P.T2.T1vT2.Condition",
    #           "Estimate.U.T2.T1vT2.Condition",
    #           "StdError.U.T2.T1vT2.Condition",
    #           "LCB95.U.T2.T1vT2.Condition",
    #           "UCB95.U.T2.T1vT2.Condition",
    #           "Sig95.Flag.Diff.T1vT2.Condition",
    #           "CI95.P.Diff.T1vT2.Condition")
    #####



## Saving
# writecsv(change_and_condition4, "NRSA14_Change_and_Condition_RS_T1vT2_")


#//////////////////////////////////////////////#
#               IMPUTING VALUES                #
#//////////////////////////////////////////////#

    #############################################
    ##    IMPUTING NA'S TO 0'S  (VERTICALLY)   ##
    #############################################
    
    # This looks for instances where, within the condition options for an indicator at a specific site (i.e. Categories for a Type-Subpopulation-Indicator combination),
    # there are partial NA's. As those NA's should actually be 0's, this converts those accordingly. For example, if N. Appalachians, Phosphorus only has data for Good and Poor, but NA for Fair,
    # we will convert the NA for Fair into 0.
    # This is done for the Count.NResp.T2.Status.Condition, Estimate.P, and Estimate.U columns.
    # All functions used are defined in the Functions.R script: imputecollist, oldcol, imputezero1

    df <- change_and_condition5
    ID2 <- df$ID3ConditionChange
    
    ## For Status in T2
    # Duplicating .Status columns that will be imputed. oldcol is a custom function defined in the Functions script.
    list_status <- as.character()
    list_status <- imputecollist(front, "T2", "Status", "Condition")
    
    df2 <- oldcol(df, list_status)

    # Imputing 0's for columns listed in list_status
    df3 <- imputezero1(df2, list_status)

        
    # For all Change columns listed in list_change
    # Duplicating .Status columns that will be imputed. oldcol is a custom function defined in the Functions script.
    list_change <- as.character()
    list_change <- imputecollist(front, "", "T1vT2", "Condition")
    
    df4 <- oldcol(df3, list_change)
    
    df5 <- imputezero1(df4, list_change)
    
    # Imputing NationalRef column
    list_national <- as.character()
    list_national <- c("NationalRef.Estimate.P.T2.Status.Condition")
    
    df5 <- oldcol (df5, list_national)
    df6 <- imputezero1(df5, list_national)
    
    ###############################################
    ##    IMPUTING NA'S TO 0'S  (HORIZONTALLY)   ##
    ###############################################
    
    # This compares columns within the same time comparison period, and ensures that either both Survey_1 and Survey_2 are NA or have data.
    # If only one of the surveys is NA, it should be imputed to 0.
    # If the dataset is formatted properly, there should be no new imputations in this step as it would be caught by the previous round.
    # This is done for the Count.NResp, Estimate.P, and Estimate.U columns.
    # If the dataset is formatted properly, there should be no new imputations in this step as it would be caught by the previous round.
    
    # UPDATE: As of February 2018, this step has been deemed unnecessary as the first vertical imputation already encapsulates this step.
    # df6 <- imputezero2(df5, "T1vT2", front, list_change)
    

#//////////////////////////////////////////////#
#            RECALCULATING VALUES              #
#//////////////////////////////////////////////#
    
    
    ########################################
    ##     RECALCULATING .DIFF VALUES     ##
    ########################################
    
    # This step recalculates the .Diff value between Survey_1 and Survey_2 for each time period comparison
    
    # Duplicating old columns before imputing
    list_diff <- as.character()
    list_diff <- imputecollist(front, "Diff", "T1vT2", "Condition")
    list_diff <- list_diff[-c(grep("Count.NResp", list_diff))] # Removing Count.NResp as the .Diff columns are not included in the raw data
    
    df7 <- oldcol(df6, list_diff)

    # recalcdiff uses list_diff and list_change. Make sure those lists are updated to reflect the Rivers and Streams dataset
    # Before recalculation, there are 153 records where .Diff has NA when it should have a number. I use this to check that the recalculation has been done
    df8 <- recalcdiff(df7, front, "T1vT2")
    
    
    #############################################
    ##     RECALCULATING SIGNIFICANCE FLAG     ##
    #############################################
    
    # Imputing "No" for Significance flag after other columns have been imputed. Check Acidification
    list_sig <- as.character()
    list_sig <- imputecollist("Sig95.Flag", "Diff","T1vT2", "Condition")
    list_sig <- list_sig[-c(grep("NA", list_sig))]
    
    df9 <- oldcol(df8, list_sig)
    names(df9)
    
    # recalcsig uses list_sig and list_change. Make sure those lists are updated to reflect the Rivers and Streams dataset
    df10 <- recalcsig(df9, "T1vT2") 

    # Not too sure why this throws an error:
    # Warning message:
    # In `[<-.data.frame`(`*tmp*`, sig_list[1], value = list(c("Yes",  :
    # provided 1700 variables to replace 1 variables
    
    # df10 <- df9 %>% 
    #   mutate(Sig95.Flag.Diff.T1vT2.Condition = ifelse(df9$Estimate.P.T1.T1vT2.Condition == 0 & df9$Estimate.P.T2.T1vT2.Condition == 0, "No", df9$Sig95.Flag.Diff.T1vT2.Condition))
    
    ## Removing ID3ConditionChange column
    df11 <- subset(df10, select = -c(ID3ConditionChange))


#//////////////////////////////////////////////#
#              FILTERING COLUMNS               #
#//////////////////////////////////////////////#

    # Removing columns containin .U, StdError, or _OLD for a more concise dataset
    
    df12 <- subset(df11, select=-c(grep("\\.U\\.", names(df11))))%>%
      subset(., select=-c(grep("StdError", names(.)))) %>%
      subset(., select=-c(grep("_OLD", names(.)))) %>% 
      unique(.)
    
    ## Saving
    writecsv(df12, "NRSA14_Condition_StatusChange_RS_T1-T2")
