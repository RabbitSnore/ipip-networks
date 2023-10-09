################################################################################

# IPIP-NEO Network Analysis - Network Modeling vs. Factor Modeling

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
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

# Country-level network comparisons --------------------------------------------

# Procedural set up

countries <- ipip_comparison$country

country_pairs <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs) <- c("country_1", "country_2")

nct_iterations <- 100

## Parallel computing set up

cores <- detectCores()

registerDoParallel(cores)

# Fit confirmatory models

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) for each country, using the full data set for
# that country (excluding incomplete cases), instead of the just the training or
# test sets. This process is computationally intensive and is set up for
# parallel computation using all available cores.

confirmatory_networks <- foreach(i = 1:length(countries), .packages = packages) %dopar% {
  
  # Retrieve omega matrix skeleton
  
  omega_skeleton <- ipip_comparison$omega_list[ipip_comparison$country == countries[i]]
  
  # Subset data
  
  net_data <- ipip %>% 
    filter(country == countries[i]) %>% 
    select(starts_with("i")) %>% 
    filter(complete.cases(.))
  
  # Country-level network
  
  country_conf_network <- 
    varcov(data  = net_data,
           type  = "ggm",
           omega = omega_skeleton[[1]]) %>% 
    runmodel()
  
  ## Store networks
  
  getmatrix(country_conf_network,
            matrix = "omega")
  
}

names(confirmatory_networks) <- countries

# Network comparison tests

foreach(i = 1:nrow(country_pairs), .combine = bind_rows, .packages = packages) %dopar% {
  
  # Subset data
  
  country_1 <- country_pairs$country_1[i]
  country_2 <- country_pairs$country_2[i]
  
  ipip_subset <- ipip %>% 
    filter(country == country_1 | country == country_2) %>% 
    select(country, starts_with("i")) %>% 
    filter(complete.cases(.))
  
  # Get skeletons
  
  omega_skeleton_1 <- ipip_comparison$omega_list[ipip_comparison$country == country_1]
  omega_skeleton_2 <- ipip_comparison$omega_list[ipip_comparison$country == country_2]
  
  # Network comparison iterations
  
  nct_structure <- rep(NA, nct_iterations)
  nct_strength  <- rep(NA, nct_iterations)
  
  foreach(j = 1:nct_iterations, .combine = bind_rows, .packages = packages) %dopar% {
    
    # Sample permutation
    
    # Fit models
    
    ## Country 1
    
    country_1_network <- 
      varcov(data  = ipip_subset %>% 
               filter(country == country_1) %>% 
               select(starts_with("i")),
             type  = "ggm",
             omega = omega_skeleton_1[[1]]) %>% 
      runmodel()
    
    ## Country 2
    
    country_2_network <- 
      varcov(data  = ipip_subset %>% 
               filter(country == country_2) %>% 
               select(starts_with("i")),
             type  = "ggm",
             omega = omega_skeleton_2[[1]]) %>% 
      runmodel()
    
    # Calculate test statistics
    
    ## Network structure
    
    ## Global network strength
    
    ## Store test statistics
    
    nct_structure[j] <- x
    nct_strength[j]  <- x
    
  }
  
  # Compute p-values
  
  # Return relevant values
  
  
  
}

