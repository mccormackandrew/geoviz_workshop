library(readxl)
library(translateR)
library(dplyr)
library(stringr)

qc <- read_xls("riding_data/recensement-2016-CEP.xls", sheet = 2)

# Riding names are colnames 
# Sometimes riding names are split across two rows when they are long
# These lines of code address this, make all riding names contained in one cell
second_row <- t(qc[1, ])
second_row[is.na(second_row)] <- ""
colnames(qc) <- paste0(trimws(colnames(qc)), trimws(second_row))

colnames(qc)[1:2] <- c("irrelevant_column", "variable")

# Add row numbers
qc <- qc %>%
  mutate(rownumber = seq(1, nrow(.))) %>%
  dplyr::select(rownumber, everything())




# To filter the right variables (which are in the rows)
# Often the whole variable name isn't in the cell, so we use a combination
# of the first characters/word of the partial variable name
# and the first value of that variable (for the Province column)
# Nature of dataset such that this needs to be done one variable at a time
# Values are stored in list
# THIS REQUIRES ACTUALLY LOOKING AT THE EXCEL DOCUMENT, which is unfortunate

qc_values <- list()

qc_values$mother_tongue_english <- qc %>%
  filter(str_detect(variable, "Anglais") & str_detect(Province, "0.076")) %>%
  gather(value = mother_tongue_english) 

qc_values$mother_tongue_french <- qc %>%
  filter(str_detect(variable, "Français") & str_detect(Province, "0.789")) %>%
  gather(value = mother_tongue_french) %>%
  select(mother_tongue_french)

qc_values$immigrant_share <- qc %>%
  filter(str_detect(variable, "Immigrants") & str_detect(Province, "0.137")) %>%
  gather(value = immigrant_share) %>%
  select(immigrant_share)

qc_values$aboriginal_share <- qc %>%
  filter(str_detect(variable, "Identité") & str_detect(Province, "0.0229")) %>%
  gather(value = aboriginal_share) %>%
  select(aboriginal_share)

qc_values$vismin_population <- qc %>%
  filter(str_detect(variable, "Total de la population") & str_detect(Province, "0.1296")) %>%
  gather(value = vismin_population) %>%
  select(vismin_population)

qc_values$low_income_share <- qc %>%
  filter(str_detect(variable, "18 à 64") & str_detect(Province, "0.1019")) %>%
  gather(value = low_income_share) %>%
  select(low_income_share)

# Total Population aged 15 years and over according to the activity situation - unemployment rate
qc_values$unemployment_rate <- qc %>%
  filter(str_detect(variable, "Taux de ch") & str_detect(Province, "7.2")) %>%
  gather(value = unemployment_rate) %>%
  select(unemployment_rate)

## Clean into data frame
qc_values <- do.call("cbind", qc_values) 

names(qc_values)[1:2] <- c("riding_name", "mother_tongue_english")

# The first few rows are irrelevant, let's start at first riding name
# To do so, must get index value of that row
first_riding <- which(qc_values[ , "riding_name"] == "Abitibi-Est")

qc_values <- qc_values[first_riding:nrow(qc_values), ]

head(qc_values)

write.csv(qc_values, "qc_values.csv")
