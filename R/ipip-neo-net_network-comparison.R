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

country_pairs <- combn(countries, 2, simplify = TRUE)

nct_iterations <- 100

## Parallel computing set up

cores <- detectCores()

registerDoParallel(cores)

# Network comparison tests

foreach(i = 1:length(country_pairs), .combine = bind_rows) %dopar% {
  
  # Subset data
  
  ipip_subset <- ipip %>% 
    filter(country == country_1 | country == country_2) %>% 
    select(country, starts_with("i")) %>% 
    filter(complete.cases(.))
  
  # Fit confirmatory models
  
  ## Retrieve omega matrix skeletons
  
  ## Country 1
  
  ## Country 2
  
  ## Store networks
  
  # Network comparison iterations
  
  nct_structure <- rep(NA, nct_iterations)
  nct_strength  <- rep(NA, nct_iterations)
  
  foreach(j = 1:nct_iterations, .combine = bind_rows) %dopar% {
    
    # Sample permutation
    
    # Fit models
    
    ## Country 1
    
    country_1_network <- 
      varcov(data  = ipip_subset %>% 
               filter(country == country_1) %>% 
               select(starts_with("i")),
             type  = "ggm",
             omega = omega_skeleton) %>% 
      runmodel()
    
    ## Country 2
    
    country_2_network <- 
      varcov(data  = ipip_subset %>% 
               filter(country == country_2) %>% 
               select(starts_with("i")),
             type  = "ggm",
             omega = omega_skeleton) %>% 
      runmodel()
    
    # Calculate test statistics
    
    ## Network structure
    
    ## Global network strength
    
    ## Store test statistics
    
    nct_structure[j] <- x
    nct_strength[j]  <- x
    
  }
  
  # Compute p-values
  
  # Save network comparison test statistics
  
}

