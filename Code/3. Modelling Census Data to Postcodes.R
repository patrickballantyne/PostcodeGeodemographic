library(tidyverse)
library(vroom)
library(lsr)
library(corrr)
library(ggcorrplot)
library(tidygraph)
library(ggraph)
library(sf)
library(reshape2)
library(corrplot)
options(scipen = 999)


# 1. Data  ----------------------------------------------------------------

## Load up the census data
inputs <- vroom("Output Data/1. Variable Selection/OA_inputs_noPC.csv")
inputs <- inputs %>%
  select(-c(1)) %>%
  rename(OA21CD = OA)

## Load up the denominators
denoms <- vroom("Output Data/1. Variable Selection/OA_denoms.csv")
denoms <- denoms %>%
  select(-c(1)) %>%
  rename(OA21CD = OA)

## Load up the postcode weights
weights <- vroom("Output Data/2. Building Postcode Weights/OA-PCD-weights.csv")
weights <- weights %>% select(OA21CD, PCDS, weight)

## Merge the census estimates onto the weights
db <- merge(weights, inputs, by = "OA21CD", all.x = TRUE)

## Weight all census variables to extract values modelled down to postcode level
db_w <- db %>%
  mutate(across(4:144, ~ .x* weight)) %>%
  select(-c(weight)) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na() 

## Check distributions, missing values etc. 
summary(db_w)

## Merge the denominators onto the weights
dn <- merge(weights, denoms, by = "OA21CD", all.x = TRUE)

## Weight all the denominators to extract values modelled down to postcode level
dn_w <- dn %>%
  mutate(across(4:50, ~ .x* weight)) %>%
  select(-c(weight)) %>%
  select(-c(`total (ts032)`)) %>%
  mutate_if(is.character, as.factor) %>%
  drop_na()

## Convert all the individual postcode level census variables into proportions by dividing by their weighted denominator

## Function for converting census variables to proportions
convProp <- function(tableID) {
  ## Get denominator for table ID
  a <- dn_w %>%
    select(ends_with(tableID))
  ## Get census variables for table ID
  b <- db_w %>%
    select(ends_with(tableID))
  ## Merge
  c <- cbind(a, b)
  ## Convert all variables to proportions
  d <- b %>%
    mutate(across(everything(), ~ (.x / c[[1]]) * 100))
  return(d)
  
}

## Create a list of tables used in analysis
ls <- c("(ts001)", "(ts002)", "(ts003)", "(ts004)", "(ts005)", "(ts007a)", "(ts011)",
        "(ts016)", "(ts017)", "(ts019)", "(ts021)", "(ts023)", "(ts025)", "(ts026)",
        "(ts030)", "(ts059)", "(ts061)", "(ts062)", "(ts066)", "(ts044)", "(ts045)",
        "(ts046)", "(ts050)", "(ts052)", "(ts054)", "(ts055)", "(ts056)", "(ts067)",
        "(ts037)")

## Run the function across each individual table
props <- lapply(ls, convProp)
props_df <- do.call(cbind, props)

## Reattach postcode geographies
out <- cbind(db_w$OA21CD, db_w$PCDS, props_df)
out <- out %>%
  rename(OA21CD = `db_w$OA21CD`, PCDS = `db_w$PCDS`)

## Write out
vroom_write(out, "Output Data/3. Postcode Modelling/PCD_inputs_PROP.tsv")

# 2. Exploring Distribution of Postcode-level Inputs ----------------------


## Postcode data - pre-processing
pcds <- vroom("Input Data/ukpostcodes.csv")
pcds <- pcds %>%
  select(postcode, latitude, longitude) %>%
  filter(!is.na(latitude))
pcds_sf <- st_transform(st_as_sf(pcds, coords = c("longitude", "latitude"), crs = 4326), 27700)
st_write(pcds_sf, "Output Data/postcodes.gpkg")

pcds <- st_read("Output Data/postcodes.gpkg")
list <- c("L1", "L2", "L3", "L4", "L5", "L6", "L7", "L8", "L9")
pcd_sub <- pcds %>%
  filter(startsWith(postcode, "L")) %>%
  arrange(postcode)

## Liverpool bbox 
lad <- st_read("Input Data/LAD/LAD_DEC_2022_UK_BFC.shp")
lad <- lad %>%
  filter(LAD22NM == "Liverpool")

## Clip postcodes
pcd_liv <- st_intersection(pcd_sub, lad)

## Read in the postcode-level input
inputs <- vroom("Output Data/PCD_inputs.tsv")
inputs_sub <- inputs %>%
  filter(PCDS %in% pcd_liv$postcode) %>%
  distinct() %>%
  select(OA21CD, PCDS,
        `Bad health`, `4 or more bedrooms`, `Works mainly from home`, `Muslim`, `Single family household`, 
        `Disabled under the Equality Act`, `Terraced`, `Household is deprived in four dimensions`, `Student`) %>%
  arrange(PCDS)


## Merge with postcodes
out <- merge(pcd_sub, inputs_sub, by.x = "postcode", by.y = "PCDS")
st_write(out, "Output Data/3. Postcode Modelling/Liverpool-Inputs.gpkg")

