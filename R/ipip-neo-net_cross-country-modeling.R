################################################################################

# IPIP-NEO Network Analysis - Cross-Country Network Fitting

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "psychonetrics", 
              "foreach",
              "doParallel")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

ipip <- read_csv("data/ipip-neo_cleaned.csv")

# Load network modeling output

## If reproducing from scratch, you will need to have run the network vs. factor
## model script to generate the comparison output

ipip_comparison <- read_rds("output/ipip-neo_model-comparison-data.rds")

# Load item key

ipip_key <- read_xls("data/IPIP-NEO-ItemKey.xls")

colnames(ipip_key) <- tolower(str_remove_all(colnames(ipip_key), "#"))

ipip_key <- ipip_key %>% 
  extract(
    col    = "key",
    into   = "trait",
    regex  = "(.).",
    remove = FALSE
  ) %>% 
  filter(!is.na(short))

# Procedural set up ------------------------------------------------------------

# Countries and pairs for comparison

countries <- ipip_comparison$country

country_pairs_1 <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs_1) <- c("country_1", "country_2")

country_pairs_2 <- country_pairs_1 %>% 
  relocate(country_2, .before = country_1) %>% 
  rename(
    country_1 = country_2,
    country_2 = country_1
  )

country_pairs <- bind_rows(country_pairs_1, country_pairs_2)

# Parallel computing set up

cores <- detectCores()

registerDoParallel(cores)

# Set up empty data

fit_data <- data.frame(
  country_1     = country_pairs$country_1,
  country_2     = country_pairs$country_2,
  cfi_network   = rep(NA, nrow(country_pairs)),   
  tli_network   = rep(NA, nrow(country_pairs)),   
  rmsea_network = rep(NA, nrow(country_pairs)), 
  bic_network   = rep(NA, nrow(country_pairs))
)

# Cross-country confirmatory network modeling ----------------------------------

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) from each country to each other country,
# thereby testing how well each country's specific network model performs in the
# data for each other country. This process is computationally intensive and is
# set up for parallel computation using all available cores.

# Note that these are "confirmatory" in the sense that the model is
# prespecified, having been estimated from a training subset of the data. But
# they are not confirmatory in the sense of strong hypothesis testing.

if (!file.exists("/output/ipip-neo_cross-country-data.rds")) {
  
  cross_country_data <- foreach(i = 1:nrow(country_pairs), .packages = packages) %dopar% {
    
    # Subset data
    
    country_1 <- country_pairs$country_1[i]
    country_2 <- country_pairs$country_2[i]
    
    ipip_subset <- ipip %>% 
      filter(country == country_2) %>% 
      select(starts_with("i")) %>% 
      filter(complete.cases(.))
    
    # Retrieve omega matrix skeleton for Country 1
    
    omega_skeleton <- ipip_comparison$omega_list[ipip_comparison$country == country_pairs$country_1[i]]
    
    # Fit network from Country 1 to data from Country 2
    
    cross_country_network <- 
      varcov(data  = ipip_subset,
             type  = "ggm",
             omega = omega_skeleton[[1]]) %>% 
      runmodel()
    
    # Fit indices
    
    test_net_fit <- fit(cross_country_network)
    
    ## Extract indices
    
    fit_data$cfi_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    fit_data$tli_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    fit_data$rmsea_network[i]  <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    fit_data$bic_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    
    # Store networks
    
    getmatrix(cross_country_network,
              matrix = "omega")
    
  }
  
  fit_data$omega_matrix <- cross_country_data
  
  write_rds(fit_data, "/output/ipip-neo_cross-country-data.rds")
  
}

