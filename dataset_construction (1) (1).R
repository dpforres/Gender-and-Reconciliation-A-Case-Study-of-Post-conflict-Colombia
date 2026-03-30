###### Dataset from LAPOP datasets 2013, 2014, 2016 & 2018
library(haven)
library(dplyr)
library(tibble)
library(stringi)
library(stringdist)


#### Setwd

setwd('/Users/Desktop')

#### Upload Data

lapop_2013 <- read_dta('/Users/Desktop/Colombia_2013.dta')
lapop_2014 <- read_dta('/Users/Desktop/Colombia_2014.dta')
lapop_2016 <- read_dta('/Users/Desktop/Colombia_2016.dta')
lapop_2018 <- read_dta('/Users/Desktop/Colombia_2018.dta')

#### Insert year column

lapop_2013 <- lapop_2013 %>%
  mutate(year = 2013)

lapop_2014 <- lapop_2014 %>%
  mutate(year = 2014)

lapop_2016 <- lapop_2016 %>%
  mutate(year = 2016)

lapop_2018 <- lapop_2018 %>%
  mutate(year = 2018)

# Standardizing 2013, 2014, 2016 datasets q2y (Age) with 2018. Subtract year born from 
# year of survey to get age
lapop_2013$q2y <- 2013 - lapop_2013$q2y
lapop_2014$q2y <- 2014 - lapop_2014$q2y
lapop_2016$q2y <- 2016 - lapop_2016$q2y

lapop_2018$q2y <- lapop_2018$q2

# Change idnum column to character
lapop_2013 <- lapop_2013 %>%
  mutate(idnum = as.character(idnum))

lapop_2014 <- lapop_2014 %>%
  mutate(idnum = as.character(idnum))

lapop_2016 <- lapop_2016 %>%
  mutate(idnum = as.character(idnum))

lapop_2018 <- lapop_2018 %>%
  mutate(idnum = as.character(idnum))

# Convert fecha to date
lapop_2013$fecha <- as.Date(lapop_2013$fecha)

########### Change q5a (Religiosity) in lapop_2013 to merge responses "1" and "2"


# Convert column to character
lapop_2013$q5a <- as.character(lapop_2013$q5a)

# Change "2" to "1"
lapop_2013$q5a <- ifelse(lapop_2013$q5a == "2", "1", lapop_2013$q5a)

# Change "3" to "2"
lapop_2013$q5a <- ifelse(lapop_2013$q5a == "3", "2", lapop_2013$q5a)

# Change "4" to "3"
lapop_2013$q5a <- ifelse(lapop_2013$q5a == "4", "3", lapop_2013$q5a)

# Change "5" to "4"
lapop_2013$q5a <- ifelse(lapop_2013$q5a == "5", "4", lapop_2013$q5a)

lapop_2013$q5a <- as.numeric(lapop_2013$q5a)

# Rename q5a to q5b so that it is same as the other years
lapop_2013$q5b <- lapop_2013$q5a

# Set as numeric
lapop_2013$q5b <- as.numeric(lapop_2013$q5b)
lapop_2014$q5b <- as.numeric(lapop_2014$q5b)
lapop_2016$q5b <- as.numeric(lapop_2016$q5b)
lapop_2018$q5b <- as.numeric(lapop_2018$q5b)

lapop_2013 <- lapop_2013[, !names(lapop_2013) %in% c("q5a")]
lapop_2014 <- lapop_2014[, !names(lapop_2014) %in% c("q5a")]
lapop_2016 <- lapop_2016[, !names(lapop_2016) %in% c("q5a")]


######## Convert all Political Affiliation Data to Dummy Variables ###########

# 2013 vb20

# "3" indicates support for the Centro Democratico presidential candidate. 
# To create a dummy variable, replace "3"with "1" and other values with "0"

lapop_2013$vb20 <- ifelse(lapop_2013$vb20 == 3, 1, 0)

lapop_2013$vb20 <- as.numeric(lapop_2013$vb20)

# 2014 colvb20 

# Replace "3"with "1" and other values with "0"
lapop_2014$colvb20 <- ifelse(lapop_2014$colvb20 == 3, 1, 0)

lapop_2014$colvb20 <- as.numeric(lapop_2014$colvb20)

# 2016 vb3n

# "805" indicates support for the Centro Democratico presidential candidate. 
# To create a dummy variable, replace "805" with "1" and other values with "0"

lapop_2016$vb3n <- ifelse(lapop_2016$vb3n == 805, 1, 0)

lapop_2016$vb3n <- as.numeric(lapop_2016$vb3n)

### 2018 vb3n

# "801" indicates support for the Centro Democratico presidential candidate. 
# To create a dummy variable, replace "801" with "1" and other values with "0"

lapop_2018$vb3n <- ifelse(lapop_2018$vb3n == 801, 1, 0)

lapop_2018$vb3n <- as.numeric(lapop_2018$vb3n)

#### Rename political affiliation variable to "centro"
lapop_2013$centro <- lapop_2013$vb20
lapop_2014$centro <- lapop_2014$colvb20
lapop_2016$centro <- lapop_2016$vb3n
lapop_2018$centro <- lapop_2018$vb3n

# Create municipio_label column
lapop_2013 <- lapop_2013 %>%
  mutate(municipio_label = as.character(haven::as_factor(municipio)))

lapop_2014 <- lapop_2014 %>%
  mutate(municipio_label = as.character(haven::as_factor(municipio)))

lapop_2016 <- lapop_2016 %>%
  mutate(municipio_label = as.character(haven::as_factor(municipio)))

lapop_2018 <- lapop_2018 %>%
  mutate(municipio_label = as.character(haven::as_factor(municipio)))

# Save prov labels for later
prov_lookup_2013 <- tibble(
  prov = as.character(lapop_2013$prov),
  prov_label = as.character(haven::as_factor(lapop_2013$prov))
) %>%
  mutate(
    prov = paste0("8", prov),
    prov = ifelse(prov == "85", "805", prov),
    prov = ifelse(prov == "88", "808", prov)
  ) %>%
  distinct()

prov_lookup_2014 <- tibble(
  prov = as.character(lapop_2014$prov),
  prov_label = as.character(haven::as_factor(lapop_2014$prov))
) %>%
  distinct()

prov_lookup_2016 <- tibble(
  prov = as.character(lapop_2016$prov),
  prov_label = as.character(haven::as_factor(lapop_2016$prov))
) %>%
  distinct()

prov_lookup_2018 <- tibble(
  prov = as.character(lapop_2018$prov),
  prov_label = as.character(haven::as_factor(lapop_2018$prov))
) %>%
  distinct()

# Standardize prov in the datasets
lapop_2013$prov <- as.character(lapop_2013$prov)
lapop_2013$prov <- paste0("8", lapop_2013$prov)
lapop_2013$prov <- ifelse(lapop_2013$prov == "85", "805", lapop_2013$prov)
lapop_2013$prov <- ifelse(lapop_2013$prov == "88", "808", lapop_2013$prov)

lapop_2014$prov <- as.character(lapop_2014$prov)
lapop_2016$prov <- as.character(lapop_2016$prov)
lapop_2018$prov <- as.character(lapop_2018$prov)

# Change name of colpaz4an (reintegration) in 2016 to colpaz4a
colnames(lapop_2016)[colnames(lapop_2016) == "colpaz4an"] <- "colpaz4a"

# Create common columns label
common_cols <- c("uniq_id", "year", "q1", "q2y", "ed", "wc1", "wc3", "wc2", 
                 "r4a", "r5", "r6", "r12", "r15", "q5b", "prov", "municipio", "municipio_label", "colrecon6", "colrecon7", 
                 "colrecon8", "colrecon18", "centro", "ur", "l1", "colpaz4a", "colpaz6a", "colpropaz5",
                 "colwc5", "colwc6", "colwc7", "colwc8", "colwc9", "colpropaz6", "coljp5", "colconce1",
                 "colrecon3", "colconce7", "coldis35f", "colrecon12", "colpropaz11a", "colpact1",
                 "colpropaz13k", "coljepaa1", "colconce6")

# Bind rows and select only common columns
lapop_13_14_16_18 <- bind_rows(lapop_2013, lapop_2014, lapop_2016, lapop_2018) %>%
  dplyr::select(all_of(common_cols))

# Change gender column to factor and rename categories
lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  mutate(q1 = as.character(q1)) %>%
  mutate(q1 = factor(q1, levels = c("1", "2"), labels = c("Men", "Women")))

# Collapse r5 into binary 0,1 for income variable. If respondent has more than one
# car, mark as one
# Set values of 1, 2, or 3 in r5 to 1
lapop_13_14_16_18$r5[lapop_13_14_16_18$r5 %in% c(1, 2, 3)] <- 1

# Return prov labels
prov_lookup_all <- bind_rows(
  prov_lookup_2013,
  prov_lookup_2014,
  prov_lookup_2016,
  prov_lookup_2018
) %>%
  distinct(prov, .keep_all = TRUE)

lapop_13_14_16_18 <- lapop_13_14_16_18 %>%
  left_join(prov_lookup_all, by = "prov")

prov_lookup_all %>%
  distinct(prov, prov_label) %>%
  count(prov) %>%
  filter(n > 1)


### Add column for rebel-controlled territory
# Standardize names in both datasets
clean_name <- function(x) {
  x %>%
    as.character() %>%
    stringi::stri_trans_general("Latin-ASCII") %>%
    tolower() %>%
    trimws()
}

# Add a unique row ID
lapop_clean <- lapop_13_14_16_18 %>%
  mutate(
    row_id = row_number(),
    survey_year = year,
    municipio_label_clean = clean_name(municipio_label),
    prov_label_clean = clean_name(prov_label)
  )

rebel_clean <- rebel_munis_by_year %>%
  mutate(
    rebel_year = year,
    ADM2_ES_clean = clean_name(ADM2_ES),
    ADM1_ES_clean = clean_name(ADM1_ES)
  ) %>%
  distinct(rebel_year, ADM1_ES_clean, ADM2_ES_clean)

lapop_rebel_joined <- lapop_clean %>%
  left_join(
    rebel_clean,
    by = c(
      "municipio_label_clean" = "ADM2_ES_clean",
      "prov_label_clean" = "ADM1_ES_clean"
    ),
    relationship = "many-to-many"
  ) %>%
  mutate(prior_rebel = !is.na(rebel_year) & rebel_year < survey_year)

# Preserve all original rows
lapop_13_14_16_18 <- lapop_rebel_joined %>%
  group_by(row_id) %>%
  summarise(
    across(c(-rebel_year, -prior_rebel), ~ first(.x)),
    rebel_controlled = as.integer(any(prior_rebel)),
    .groups = "drop"
  ) %>%
  dplyr::select(
    -any_of(c(
      "row_id",
      "survey_year",
      "municipio_label_clean",
      "prov_label_clean",
      "ADM2_PCODE"
    ))
  )

#Export final dataset
file_path <- "/Users/Desktop/lapop_13_14_16_18.csv"  

# Export the dataset to CSV
write.csv(lapop_13_14_16_18, file = file_path, row.names = FALSE)



###### Dataset from LAPOP dataset 2021
#### Setwd

#### Upload Data

lapop_2021 <- read_dta('/Users/Desktop/Colombia_2021.dta')

# Subsetting the dataset
lapop_2021 <- lapop_2021 %>%
  dplyr::select(colpaz6a, q1tb, r6, r15, r16, r18, coll1, edr, q2, colwc10, prov1t, ur1new)

# Change gender column to factor and rename categories
lapop_2021 <- lapop_2021 %>%
  mutate(q1tb = as.character(q1tb)) %>%
  filter(q1tb != "3") %>%
  mutate(q1tb = factor(q1tb, levels = c("1", "2"), labels = c("Men", "Women")))

#Export final dataset
file_path <- "/Users/Desktop/lapop_2021.csv"  

# Export the dataset to CSV
write.csv(lapop_2021, file = file_path, row.names = FALSE)




