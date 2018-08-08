#######################
### RUN THIS SCRIPT ###
#######################
###      THIRD      ###
#######################

### This script contains all the functions used throughout this R Project as well as some examples.


#///////////////////////////////////#
#  MATCHING DATA TO PLAIN LANGUAGE  #
#///////////////////////////////////#

## This matches your data df to plain language names for category, indicator, subpopulation, and type.
## This also ensures that unwanted indicators such as P and N Concentrated Classes and Macroinvertebrate_OE are filtered out
## (even though they may no longer be in the original dataset or may be marked "Remove" in the reference tables)
matchplainlang <- function(x) {
  filter(x, Category != "Total") %>%
    left_join(pl_indcat, by=c("Category" = "Category", "Indicator" = "Indicator")) %>%
    left_join(pl_subpoptype, by=c("Type" = "Type", "Subpopulation" = "Subpopulation")) %>%
    left_join(metric_categories, by=c("IndicatorPlainLanguage")) %>%
    filter(Indicator!= "Phosphorus_Conc_Class" & Indicator != "Nitrogen_Conc_Class" & Indicator != "MacroInvert_OE") 
    # %>% filter(CategoryConditionPlainLanguage == "Most Disturbed")
}

#///////////////////////////////////#
#       CLEANING COLUMN NAMES       #
#///////////////////////////////////#

# Define time periods first. Also defined in References.R
T0 <- "WSA/EMAP West 2000-2004"
T1 <- "NRSA 2008-2009"
T2 <- "NRSA 2013-2014"

subsetsurvey <- function(df, s1, s2) {
  filter(df, Survey_1 == s1, Survey_2 == s2) %>%
  subset(., select = -c(Survey_1, Survey_2) )
}

cleannames <- function(df, s1, s2) {
  x1 <- deparse(substitute(s1))
  x2 <- deparse(substitute(s2))
  names(df) <- sub("DiffEst.UP", "DiffEst.U", names(df)) %>% # Fixes mistake in the data where DiffEst.UP instead of DiffEst.U
  sub(".P$", ".P.Diff", .) %>%
  sub(".U$", ".U.Diff", .) %>%
  sub("_1", paste(".", x1, sep=""), .) %>%
  sub("_2", paste(".", x2, sep=""), .) %>%
  sub("NResp", "Count.NResp", .) %>%
  sub("DiffEst", "Estimate", .) %>%
  sub("Pct", "", .)
  return(df)
}

## Note: deparse(substitute(x)) returns the name of the variable being passed through. See: https://stackoverflow.com/questions/10520772/in-r-how-to-get-an-objects-name-after-it-is-sent-to-a-function
assignnames <- function(df, s1, s2) {
  nam <- names(df)
  names(df) <- ifelse(nam %in% c(names_raw, names_pl, names_other, "ID2Condition"), str_c(nam), str_c(nam, ".", paste(deparse(substitute(s1)), "v", deparse(substitute(s2)), ".Condition", sep="")))
  return(df)
}


## e.g
# change_T1vT2 <- subsetsurvey(change3, T1, T2) %>%
#   cleannames(.) %>%
#   assignnames(., T1, T2)



#///////////////////////////////////#
#           WRITE TO CSV            #
#///////////////////////////////////#

writecsv <- function(df, input) {
  setwd(paste(root, "/Combined", sep=""))
  date <- format(Sys.Date(), format="%Y_%m_%d")
  write.csv(df, paste(input, "_", date, ".csv", sep = ""), row.names = FALSE)
  setwd(root)  
}

## e.g.
# writecsv(df, "NRSA14_Output")
# File Output:
# > NRSA14_Output_2018_01_01.csv



#///////////////////////////////////#
#             IMPUTING              #
#///////////////////////////////////#

# Creates a list of columns that need to be imputed
# Arguments are:
# tvt -- a list of all possible time period comparisons (i.e. "T0vT1", "T1vT2", "T0vT2")
# estimate -- a list of column beginnings (i.e. c("Count.NResp", "Estimate.P", "Estimate.U")
# data_type -- a string of column endings (i.e. "Condition")

## e.g.
# tvt <- c("T1vT2", "T0vT2", "T0vT1")
# front <- c("Count.NResp", "Estimate.P", "Estimate.U")

# First create an empty character variable
list_impute <- as.character()

# This function will generate a list of all possible combinations between the four arguments specified

imputecollist <- function(estimate, tt, time, data_type) {
  for (x in 1:length(front)) {
    if (grepl("*.v.*", time[1]) & tt != "Diff") {
        for (i in 1:length(time)) {
        time1 <- gsub("v..$", "", time[i]) 
        time2 <- gsub("^..v", "", time[i])
        # print(c(time[i], time1, time2))
        list_impute <- c(list_impute, paste(estimate[x], time1, time[i], data_type, sep="."))
        list_impute <- c(list_impute, paste(estimate[x], time2, time[i], data_type, sep="."))
        }
    }
    else {
        list_impute <- c(list_impute, paste(estimate[x], tt, time, data_type, sep="."))
    }
  }
  return(list_impute)
}
    
## e.g.
# list_status <- as.character()
# list_status <- imputecollist(front, "T2", "Status", "Condition")
# 
# list_diff <- as.character()
# list_diff <- imputecollist(front, "Diff", tvt, "Condition")
# list_diff <- list_diff[-c(grep("Count.NResp", list_diff))] # Removing Count.NResp as the .Diff columns are not included in the raw data
# 
# list_change <- as.character()
# list_change <- imputecollist(front, "", tvt, "Condition")
#
# list_sig <- as.character()
# list_sig <- imputecollist("Sig95.Flag", "Diff", tvt, "Condition")
# list_sig <- list_sig[-c(grep("NA", list_sig))]

#------------

# Duplicates target columns into old columns so that raw data is preserved. Easier to make comparisons and see changes later.
# Takes 4 arguments
# df = your data frame
# estimate = (list) Count.NResp, Estimate.P, Estimate.U, etc
# time = (list) T1vT2, T0vT2, T0vT1, etc
# measure = "Status" or can also repeat list used for time above

oldcol <- function(df, col_list) {
  for (i in 1:length(col_list)) {
    col <- paste(col_list[i])
    col_OLD <- paste(col, "_OLD", sep="")
    df[,col_OLD] <- df[,col]
      
    # # For survey_2
    # col <- paste(estimate, s2, measure[i], data_type, sep=".")
    # col_OLD <- paste(col, "_OLD", sep="")
    # df[,col_OLD] <- df[,col]
  }
  return(df)
}

## e.g.
## For T2 in Status only
# df2 <- oldcol(df, list_status)    
# names(df2)
# 
## For all permutations of estimate, time, and measure. 
# df3 <- oldcol(df2, list_change)    
# names(df3)

#------------

# Imputing zero for when at least one row in a group of observations/rows (by Type-Subpopulation-CategoryCondition combo, i.e. ID3ConditionChange) is NA
# whereas data are present for the other CategoryConditions

# Time period comparison list (i.e. tvt) and column header beginnings (i.e. front) need to be defined first, to generate another list of all columns that need imputation
## e.g.
# tvt <- c("T0vT1", "T1vT2", "T0vT2")
# front <- c("Count.NResp", "Estimate.P", "Estimate.U")
# list_impute <- imputecollist(tvt, front, "Condition")

imputezero1 <- function(df, col_list) {
  for (i in 1:length(ID2)) {
    for (x in 1:length(col_list)) {
      dftemp <- filter(df, ID3ConditionChange == paste(ID2[i]))
      num_cat <- length(dftemp$CategoryConditionPlainLanguage)
      num_na <- sum(is.na(dftemp[col_list[x]]) == TRUE)
      if (num_na == num_cat | num_na == 0) {
        next
      }
      df[col_list[x]][with(df, ID3ConditionChange == paste(ID2[i]) & is.na(df[col_list[x]]))] <- 0
      
    }
  }
  return(df)
}

## e.g.
# df4 <- imputezero1(df3, list_impute)

# This compares columns within the same time comparison period, and ensures that either both Survey_1 and Survey_2 are NA or have data.
# If only one of the surveys is NA, it should be imputed to 0.
# If the dataset is formatted properly, there should be no new imputations in this step as it would be caught by the previous round in imputezero1

imputezero2 <- function(df, tvt, front, col_list) {
  for (i in 1:length(front)) {
    for (x in 1:length(tvt)) {
      templist <- grep(paste(front[i], "*", tvt[x], sep="."), c(col_list), value=TRUE)
      #print(c(templist[1], templist[2]))
      df[templist[1]][is.na(df[templist[1]]) & !is.na(df[templist[2]])] <- 0
      df[templist[2]][is.na(df[templist[2]]) & !is.na(df[templist[1]])] <- 0
    }
  }
  return(df)
}

## e.g.
# df6 <- imputezero2(df5, tvt, front, list_change)



#///////////////////////////////////#
#          RECALCULATING            #
#///////////////////////////////////#

# Recalculates the .Diff value between the two related Estimate.P values
recalcdiff <- function(df, estimate, time) {
  for (i in 1:length(estimate)) {
    for (x in 1:length(time)) {
      diff_list <- list_diff[c(grep(paste(estimate[i], "*", time[x], sep="."), list_diff))]
      change_list <- list_change[c(grep(paste(estimate[i], "*", time[x], sep="."), list_change))]
      print(c(diff_list, change_list))
      
      if (length(diff_list) > 0) {
        index <- is.na(df[diff_list[1]]) & !is.na(df[change_list[1]]) & !is.na(df[change_list[2]])
        print(sum(index))
        df[diff_list[1]][index] <- df[change_list[2]][index] - df[change_list[1]][index]
      }
    }
  }
  return(df)
}


## e.g.
# df8 <- recalcdiff(df7, front, tvt)

# Recalculates the Significance flag. Finds the matching trios of Estimate.P (both Survey_1 and Survey_2) and Sig95.Flag.Diff, and performs the recalculation
recalcsig <- function(df, time) {
  for (x in 1:length(time)) {
    change_list <- list_change[c(grep(paste("Estimate.P", "*", time[x], sep="."), list_change))]
    sig_list <- list_sig[c(grep(paste("Sig95.Flag", "*", time[x], sep="."), list_sig))]
    print(c(change_list, sig_list))
    
    if (length(change_list) > 0) {
      index <- df[change_list[1]] == 0 & df[change_list[2]] == 0
      print(sum(index))
      df[sig_list[1]][index] <- "No"
      
      # df <- df %>% 
      #   mutate(.[paste(sig_list[1])] = ifelse(df$Estimate.P.T1.T1vT2.Condition == 0 & df$Estimate.P.T2.T1vT2.Condition == 0, "No", df$Sig95.Flag.Diff.T1vT2.Condition))
      
    }
  }
  return(df)
}

## e.g.
# df10 <- recalcsig(df9, tvt)
