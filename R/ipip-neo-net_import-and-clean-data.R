################################################################################

# IPIP-NEO Network Analysis - Importation and Cleaning of Data

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", "readxl", "osfr")

lapply(packages, library, character.only = TRUE)

# Download data ----------------------------------------------------------------

# The following code automatically downloads IPIP-NEO data from Johnson's
# repository on OSF (https://osf.io/tbmh5/). If you already have the data in the
# intended directory, this code is effectively ignored.

# If a data directory did not exist, it would be necessary to create it

if (!dir.exists("data")) {
  
  dir.create("data")
  
}

# Johnson (2014)

if (!file.exists("data/IPIP120.dat")) {
  
  osf_retrieve_file("563a5c9d8c5e4a40e2c1bf4e") %>% 
    osf_download(
      path = "data"
    )
  
}

if (!file.exists("data/IPIP300.dat")) {
  
  osf_retrieve_file("563a59ba8c5e4a40e6c1db75") %>% 
    osf_download(
      path = "data"
    )
  
}

# Johnson (2005)

if (!file.exists("data/ipip20993.dat")) {
  
  osf_retrieve_file("563a757a8c5e4a3d3a115560") %>% 
    osf_download(
      path = "data"
    )
  
}

# Item Key

if (!file.exists("data/IPIP-NEO-ItemKey.xls")) {
  
  osf_retrieve_file("563a674f8c5e4a127b77d070") %>% 
    osf_download(
      path = "data"
    )
  
}


# Load data --------------------------------------------------------------------

# Johnson (2014)

## IPIP 120

ipip_120 <- read_fwf("data/IPIP120.dat",
                     col_positions = fwf_positions(
                       start = c(1, 7, 8, 10, 12, 14, 16, 18, 20, 23, 32:151),
                       end   = c(6, 7, 9, 11, 13, 15, 17, 19, 22, 31, 32:151),
                       col_names = c(
                         "case", "sex", "age", "sec", "min", "hour", "day", 
                         "month", "year", "country",
                         paste("i", 1:120,
                               sep = "")
                       )
                       )
                     )

## IPI 300

ipip_300 <- read_fwf("data/IPIP300.dat",
                     col_positions = fwf_positions(
                       start = c(1, 7, 8, 10, 12, 14, 16, 18, 20, 23, 34:333),
                       end   = c(6, 7, 9, 11, 13, 15, 17, 19, 22, 33, 34:333),
                       col_names = c(
                         "case", "sex", "age", "sec", "min", "hour", "day", 
                         "month", "year", "country",
                         paste("i", 1:300,
                               sep = "")
                       )
                       )
                     )

## Johnson (2005)

ipip_2005 <- read_tsv("data/ipip20993.dat")

colnames(ipip_2005) <- tolower(colnames(ipip_2005))

# Item key

ipip_key <- read_xls("data/IPIP-NEO-ItemKey.xls")

colnames(ipip_key) <- tolower(str_remove_all(colnames(ipip_key), "#"))

# Country codes

iso_countries <- read_csv("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv")

colnames(iso_countries) <- str_replace_all(colnames(iso_countries), "-", "_")

# Data cleaning ----------------------------------------------------------------

# Find the short form items on the long form IPIP-NEO

ipip_short <- paste("i", ipip_key$full[1:120], sep = "")

# Extract short form data from each dataset

ipip_300_short <- ipip_300 %>% 
  select(
    case, sex, age, country,
    all_of(ipip_short)
  )

colnames(ipip_300_short) <- c("case", "sex", "age", "country",
                              paste("i", 1:120, sep = ""))

ipip_300_short$study <- "johnson_2014_ipip-300"

ipip_2005_short <- ipip_2005 %>% 
  select(
    sex, age,
    all_of(ipip_short)
  )

colnames(ipip_2005_short) <- c("sex", "age",
                              paste("i", 1:120, sep = ""))

ipip_2005_short$study <- "johnson_2005_ipip-300"

ipip_120_select <- ipip_120 %>% 
  select(
    case, sex, age, country,
    all_of(paste("i", 1:120, sep = ""))
  )

ipip_120_select$study <- "johnson_2014_ipip-120"

# The country names from the IPIP-NEO 300 data are longer than those in the
# IPIP-NEO 120 data, so a crude way of lining up the country names is to simply
# cut the longer names to match the shorter ones.

ipip_300_short$country <- str_trunc(ipip_300_short$country, 9, side = "right", ellipsis = "")

# Combine data

ipip <- bind_rows(ipip_120_select, ipip_300_short, ipip_2005_short)

ipip$country <- str_trim(ipip$country, side = "right")

# Export cleaned data ----------------------------------------------------------

write_csv(ipip, "data/ipip-neo_cleaned.csv")
