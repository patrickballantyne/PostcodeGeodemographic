library(sf)
library(arrow)
library(vroom)
library(tidyverse)
library(tmap)

# 1. Data ----------------------------------------------------------------------

## The main dataset for creating the method is the ONS UPRN Directory
## It comes from: https://geoportal.statistics.gov.uk/datasets/ons-uprn-directory-october-2022/about
## It contains UPRNS, postcodes and OAs

## These files contain the individual UPRNs, postcodes and output areas
uprn_loc <- list.files("Input Data/ONS_UPRN_Directory/", pattern = "*.csv", full.names = TRUE)

## Function that reads in individual regions worth of UPRNs
cleanUPRN <- function(x) {
  m <- read_csv_arrow(x)
  m <- m %>%
    select(UPRN, PCDS, OA21CD)
}

## Loop through the different regions and clean out UPRNs
res <- lapply(uprn_loc, cleanUPRN)

## Assemble into one large lookup and write out
res_out <- do.call(rbind, res)
write.csv(res_out, "Output Data/2. Building Postcode Weights/UPRN-PCD-OA.csv")

# 2. Method ---------------------------------------------------------------

## Read in the database of UPRNs-PCDs-OAs
dat <- read_csv_arrow("Output Data/2. Building Postcode Weights/UPRN-PCD-OA.csv")
dat <- dat %>%
  select(UPRN, PCDS, OA21CD)

## Calculate total UPRNs by Output Area and Postcode
totOA <- dat %>%
  select(UPRN, OA21CD) %>%
  group_by(OA21CD) %>%
  summarise(UPRNtotalOA = n_distinct(UPRN))
totPC <- dat %>%
  select(UPRN, OA21CD, PCDS) %>%
  group_by(PCDS, OA21CD) %>%
  summarise(UPRNtotalPCD = n_distinct(UPRN)) %>%
  filter(UPRNtotalPCD == max(UPRNtotalPCD))

## Join total UPRNs by postcode and Output Area together
dat2 <- merge(dat, totOA, by = "OA21CD", all.x = TRUE)
dat2 <- merge(dat2, totPC, by = c("OA21CD", "PCDS"), all.x = TRUE)

## Calculate proportion of UPRNs in an OA allocated to individual postcodes
out <- dat2 %>%
  drop_na(UPRNtotalPCD) %>%
  mutate(propUPRN = (UPRNtotalPCD / UPRNtotalOA) * 100) %>%
  select(-c(UPRN)) %>%
  distinct() %>%
  mutate(weight = propUPRN / 100)
write.csv(out, "Output Data/2. Building Postcode Weights/OA-PCD-weights.csv")

