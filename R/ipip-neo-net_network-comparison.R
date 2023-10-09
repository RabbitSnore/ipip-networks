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

## Countries and pairs for comparison

countries <- ipip_comparison$country

country_pairs <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs) <- c("country_1", "country_2")

## Iterations for network comparison tests

nct_iterations <- 1000

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

## Add country names

names(confirmatory_networks) <- countries

## Save estimated confirmatory networks

write_rds(confirmatory_networks, "output/ipip-neo_confirmatory-networks.rds")

# Network comparison tests

# This loop will compute network comparison tests, comparing each country to
# each other country. This process is computationally intensive and is set up
# for parallel computation using all available cores. Note that even with
# parallel processing, doing hundreds of comparisons with many iterations will
# be extremely time consuming without a very large number of parallel processes.

foreach(i = 1:nrow(country_pairs), .combine = bind_rows, .packages = packages) %dopar% {
  
  # Subset data
  
  country_1 <- country_pairs$country_1[i]
  country_2 <- country_pairs$country_2[i]
  
  ipip_subset <- ipip %>% 
    filter(country == country_1 | country == country_2) %>% 
    select(country, starts_with("i")) %>% 
    filter(complete.cases(.))

  # Network comparison iterations
  
  seed_list <- round(runif(nct_iterations, 1000, 10000))
  
  nct_data <- foreach(j = 1:nct_iterations, .combine = bind_rows, .packages = packages) %dopar% {
    
    # Sample permutation
    
    set.seed(seed_list[j])
    
    ipip_subset$country <- sample(ipip_subset$country)
    
    ipip_1 <- ipip_subset %>% 
      filter(country == country_1) %>% 
      select(starts_with("i"))
    
    ipip_2 <- ipip_subset %>% 
      filter(country == country_2) %>% 
      select(starts_with("i"))
    
    # Fit models
    
    # This model estimation procedure follows the same procedure used to
    # identify the retained model for each country.
    
    ## Country 1
    
    country_1_network <- EBICglasso(cov(ipip_1),
                                   n = nrow(ipip_1),
                                   nlambda = 1000,
                                   lambda.min.ratio = 0.01,
                                   # Refit without regularization to be
                                   # comparable to the psychonetrics estimation
                                   refit = TRUE,
                                   returnAllResults = TRUE)
    
    ### Repeat training fit if sparsity may be violated
    
    lambda_index <- which(country_1_network$ebic == min(country_1_network$ebic))
    lambda_opt   <- country_1_network$lambda[lambda_index]
    
    if (lambda_opt == min(country_1_network$lambda)) {
      
      country_1_network <- EBICglasso(cov(ipip_1),
                                     n = nrow(ipip_1),
                                     nlambda = 10000,
                                     lambda.min.ratio = 0.1,
                                     refit = TRUE,
                                     returnAllResults = TRUE)
      
      lambda_index <- which(country_1_network$ebic == min(country_1_network$ebic))
      lambda_opt   <- country_1_network$lambda[lambda_index]
      
      if (lambda_opt == min(country_1_network$lambda)) {
        
        country_1_network <- EBICglasso(cov(ipip_1),
                                       n = nrow(ipip_1),
                                       nlambda = 100000,
                                       threshold = TRUE,
                                       lambda.min.ratio = 0.1,
                                       refit = TRUE,
                                       returnAllResults = TRUE)
        
      }
      
    }
    
    ## Country 2
    
    country_2_network <- EBICglasso(cov(ipip_1),
                                    n = nrow(ipip_1),
                                    nlambda = 1000,
                                    lambda.min.ratio = 0.01,
                                    refit = TRUE,
                                    returnAllResults = TRUE)
      
    ### Repeat training fit if sparsity may be violated
    
    lambda_index <- which(country_2_network$ebic == min(country_2_network$ebic))
    lambda_opt   <- country_2_network$lambda[lambda_index]
    
    if (lambda_opt == min(country_2_network$lambda)) {
      
      country_2_network <- EBICglasso(cov(ipip_1),
                                      n = nrow(ipip_1),
                                      nlambda = 10000,
                                      lambda.min.ratio = 0.1,
                                      refit = TRUE,
                                      returnAllResults = TRUE)
      
      lambda_index <- which(country_2_network$ebic == min(country_2_network$ebic))
      lambda_opt   <- country_2_network$lambda[lambda_index]
      
      if (lambda_opt == min(country_2_network$lambda)) {
        
        country_2_network <- EBICglasso(cov(ipip_1),
                                        n = nrow(ipip_1),
                                        nlambda = 100000,
                                        threshold = TRUE,
                                        lambda.min.ratio = 0.1,
                                        refit = TRUE,
                                        returnAllResults = TRUE)
        
      }
      
    }
  
    country_1_network <- country_1_network$optnet
    country_2_network <- country_2_network$optnet
    
    # Calculate test statistics
    
    ## Network structure
    
    network_diff <- country_1_network - country_2_network
    
    max_diff     <- max(network_diff) 
    
    ## Global network strength
    
    strength_diff <- sum(abs(country_1_network)) - sum(abs(country_2_network))
    
    ## Store test statistics
    
    list(
      nct_structure_test = max_diff, 
      nct_strength_test  = strength_diff,
      network_diff       = network_diff,
      country_1_network  = country_1_network,
      country_2_network  = country_2_network,
      seed               = seed_list[j]
      )
    
  }
  
  # Compute p-values
  
  # Return relevant values
  
  
  
}

