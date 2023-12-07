################################################################################

# IPIP-NEO Network Analysis - Central Build Script

################################################################################

# (Re)Build the project --------------------------------------------------------

# Import and clean the data

if (!file.exists("data/ipip-neo_cleaned.csv")) {
  
  source("R/ipip-neo-net_import-and-clean-data.R")
  
} else {
  
  if (!file.exists("data/IPIP-NEO-ItemKey.xls")) {
    
    osf_retrieve_file("563a674f8c5e4a127b77d070") %>% 
      osf_download(
        path = "data"
      )
    
  }
  
}

# Network and factor model comparison

if (!file.exists("output/ipip-neo_model-comparison-data.rds") | !file.exists("output/ipip-neo_cfa-fits.rds")) {
  
  source("R/ipip-neo-net_network-modeling.R")
  
}

source("R/ipip-neo-net_ffm-esem.R")
source("R/ipip-neo-net_acquiescence.R")

# Confirmatory modeling for closer examination

source("R/ipip-neo-net_confirmatory-modeling.R")

# Cross-country network model fitting

source("R/ipip-neo-net_cross-country-modeling.R")