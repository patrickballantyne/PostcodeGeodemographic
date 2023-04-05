library(tidyverse)
library(vroom)

# 1. Data Cleaning --------------------------------------------------------

## Read in the list of variables we get from Alex's Git repo
table <- read_csv("https://github.com/alexsingleton/Census_2021_Output_Areas/raw/main/Table_Metadata.csv",show_col_types = FALSE)

## Keep only the variables that aren't totals - i.e. for males, females and total keep only males and females
names <- table %>%
  select(Table_ID, Table_Name, new_names, Variable_Name) %>%
  filter(!grepl("Total", Variable_Name)) %>%
  filter(!grepl("All persons", Variable_Name)) %>%
  distinct()

##  List of tables to pull from Git repo
table_list <- names %>% select(Table_ID) %>% unique() %>% pull()

## Function to pull down OA level census variables
pullCensus <- function(tableID) {
  tmp <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/",tableID,".csv?raw=true"),show_col_types = FALSE)
  tmp_OA <- tmp[, c("OA")]
  tmp <- tmp %>%
    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(tableID,"0001"))*100))) #%>%
    # select(ends_with("_PCT")) %>%
    # rename_with(~str_remove(., "_PCT"))
  
  ## Build lookup for old/new names
  oldnames <- as.data.frame(colnames(tmp))
  oldnames <- oldnames %>% setNames(c("new_names"))
  oldnames <- merge(oldnames, names, by = "new_names", all.x = TRUE)
    
  ## Build output
  names(tmp) = names$Variable_Name[match(names(tmp), names$new_names)]
  names(tmp) = paste0(names(tmp), " ", "(", tableID, ")")
  names(tmp)[1] = "OA"
  return(tmp)
  tmp <- tmp %>%
    select(-c(2))
  
  ## Add for lookup
  tmp_list <- as.data.frame(colnames(tmp))
  lookup <- cbind(tmp_list, oldnames)
  lookup <- lookup %>%
    setNames(c("Variable_Name_Full", "new_names", "Table_ID", "Table_Name", "Variable_Name")) %>%
    select(Table_ID, Table_Name, Variable_Name, Variable_Name_Full)
  #return(lookup) ## This is where we can get the full list of names out
  
  ## Construct OA-level table
  tmp_OA <- cbind(tmp_OA, tmp)
  assign(tableID,tmp_OA)
}


## Function to pull in the denominators for each table
pullTotals <- function(tableID) {
  tmp <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/",tableID,".csv?raw=true"),show_col_types = FALSE)
  tmp <- tmp %>%
    select(1:2) %>%
    setNames(c("OA", paste0("total", " ", "(", tableID, ")")))
  
}


## Pull out the lookup - make sure the return(lookup) statement is selected
lookup <- do.call(rbind, lapply(table_list, pullCensus))
write.csv(lookup, "Output Data/1. Variable Selection/VariableLookup_NoPC.csv")

## Pull out OA level data
tables <- lapply(table_list, pullCensus)
inputs <- tables %>%
  reduce(left_join, by = "OA", ) %>%
  select(OA, everything())
write.csv(inputs, "Output Data/1. Variable Selection/OA_inputs_NEW_noPC.csv")

## Pull in the denominators for each table, so we can calculate %s at postcode level
denoms <- lapply(table_list, pullTotals)
denoms <- denoms %>%
  reduce(left_join, by = "OA", ) %>%
  select(OA, everything())
denoms_clean <- denoms %>%
  select(-c(`total (ts020)`, `total (ts033)`, `total (ts034)`, 
            `total (ts035)`, `total (ts036)`))
write.csv(denoms_clean, "Output Data/1. Variable Selection/OA_denoms.csv")

# 2. Variable Reduction ---------------------------------------------------

rm(list = ls())
## At this stage examine the variables present in VariableLookup.csv, consider their usefulness and whether they need to be merged/deleted,
## and examine their overall distribution to see if they will have value in the geodemographic. 

## Read in the VariableLookup file once extra columns have been added detailing these merges and keeps etc.
notes <- readxl::read_xlsx("Output Data/1. Variable Selection/VariableLookup.xlsx")

## Read in the OA inputs and do the deletions, merges etc. 
oa <- vroom("Output Data/1. Variable Selection/OA_inputs_NEW_noPC.csv")
oa <- oa %>%
  select(-c("...1"))

## Identify different uses for variables 
toKeep <- notes %>% 
  filter(KEEP == "1.0") %>%
  select(Variable_Name_Full) %>%
  pull()
keepOut <- oa %>%
  select(toKeep)

toDelete <- notes %>% filter(KEEP == "0.0")
toMerge <- notes %>% filter(KEEP == "M")

## Do the merges
mergeDF <- toMerge %>%
  select(MERGE, Variable_Name_Full)
merge_ls <- toMerge %>%
  select(MERGE) %>%
  distinct() %>%
  pull()

## Function that merges columns together
createMerge <- function(ls) {
  
  ## Get list of columns that need to be merged
  t <- mergeDF %>%
    filter(MERGE == ls) %>%
    select(Variable_Name_Full) %>%
    pull()
  ## Get the columns that need to be merged
  d <- oa %>%
    select(t) %>%
    mutate(ls = rowSums(., na.rm=TRUE)) %>%
    select(ls) %>%
    setNames(c(ls))
  return(d)
}

## Apply function to all columns that need merging
test <- lapply(merge_ls, createMerge)
mergeOut <- do.call(cbind, test)

## Reassemble the selected and merged variables 
varsFinal <- cbind(keepOut, mergeOut)
varsFinal_EX <- as.data.frame(colnames(varsFinal))
varsFinal_EX <- varsFinal_EX %>%
  setNames(c("Variable_Name_Full")) %>%
  mutate(Variable_Name_Full = factor(Variable_Name_Full)) %>%
  arrange(Variable_Name_Full)

## Build a lookup of new variables to old table names, for ordering purposes
notesOG <- notes %>%
  filter(KEEP == 1) %>%
  select(Table_ID, Table_Name, Variable_Name_Full) %>%
  distinct()
notesMG <- notes %>%
  filter(KEEP == "M") %>%
  select(Table_ID, Table_Name, MERGE) %>%
  rename(Variable_Name_Full = MERGE) %>%
  distinct()
finalLookup <- rbind(notesOG, notesMG)

## Attach table information
out <- merge(varsFinal_EX, finalLookup, by = "Variable_Name_Full", all.x = TRUE)
out <- out %>%
  select(Table_ID, Table_Name, Variable_Name_Full) %>%
  arrange(Table_ID)

## Reattach OA ID's
oa_ls <- oa %>% select(OA)
varsFinal_out <- cbind(oa_ls, varsFinal)

## Writing out files

### OA level inputs, post reduction
write.csv(varsFinal_out, "Output Data/1. Variable Selection/OA_inputs_noPC.csv")

## Inputs list, post reduction (incl. table names)
write.csv(out, "Output Data/Variable Selection/1. OA_inputs_info.csv")

