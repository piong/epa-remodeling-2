#######################
### RUN THIS SCRIPT ###
#######################
###     FOURTH      ###
#######################

### This script creates the padding of all subpopulation-indicator-condition combinations in order to pad any "missing" rows of data


#////////////////////////////#
#     CREATING PADDING       #
#////////////////////////////#

# Padding should have a total of 3835 rows
padding <- merge(pl_indcat, pl_subpoptype) %>%
  filter(Indicator != "Acidification") %>% # Removing old Acidification indicator from padding
  select(TypePlainLanguage, SubpopulationPlainLanguage, SubpopulationFlag, IndicatorPlainLanguage, CategoryConditionPlainLanguage, CategoryConditionMostDisturbed) %>%
  # left_join(metric_categories, by=c("IndicatorPlainLanguage")) %>%
  unique()

# Padding without the problematic Indicator column
padding_raw <- right_join(padding, pl_merge, by=names(padding)) %>%
  subset(., select = -Indicator) %>%
  unique()

## Only subpopulations we are keeping in the dashboard
padding_raw_keep <- filter(padding_raw, SubpopulationFlag == "Keep") %>%
  filter(IndicatorConditionFlag == "Keep")

# writecsv(padding_raw, "NRSA14_Padding_")


#//////////////////////////////////////////////#
#             CREATING .ALL COLUMNS            #
#//////////////////////////////////////////////#

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
# 
# all_df_values <- c(12,16,5,80,456,98,54,23,19,120,765,911,85,15,27,81,54,201,3,46,22)
# 
# all_df <- data.frame(x = all_df_colname, y= all_df_values)
# all_df2 <- spread(all_df, key = x, value = y)
# 
# all_df_RS <- data.frame(matrix(ncol = 21, nrow = 1))
# colnames(all_df_RS) <- all_df_colname

