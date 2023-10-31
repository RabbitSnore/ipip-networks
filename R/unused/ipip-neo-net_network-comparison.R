################################################################################

# IPIP-NEO Network Analysis - Network Comparison Tests

################################################################################

# THIS SCRIPT HAS NEVER BEEN SUCCESSFULLY COMPLETED.

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
              "foreach",
              "doParallel",
              "readxl",
              "lme4",
              "performance",
              "cowplot")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

ipip <- read_csv("data/ipip-neo_cleaned.csv")

## If reproducing from scratch, you will need to have run the network vs. factor
## model script to generate the comparison output

ipip_comparison <- read_rds("output/ipip-neo_model-comparison-data.rds")

# Load confirmatory models

confirmatory_networks <- read_rds("output/ipip-neo_confirmatory-networks.rds")

# Procedural set up ------------------------------------------------------------

# Countries and pairs for comparison

countries <- ipip_comparison$country

country_pairs <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs) <- c("country_1", "country_2")

# Iterations for network comparison tests

nct_iterations <- 1000

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cores)

# Network comparison tests -----------------------------------------------------

# This loop will compute network comparison tests, comparing each country to
# each other country. This process is computationally intensive and is set up
# for parallel computation using all available cores. Note that even with
# parallel processing, doing hundreds of comparisons with many iterations will
# be extremely time consuming without a very large number of parallel processes
# or an exceptionally powerful computer.
#
# Also note that this procedure is extremely memory intensive in addition to
# demanding processing power.
#
# In fact, it is so intensive that we have never gotten it to complete
# successfully. After failing a few times and trying some smaller scale code, my
# back of the envelope estimation, based on the hardware I had access to, is
# that this would take around 4 or 5 years to complete. So if you have that kind
# of time or a sufficiently powerful computer to make it go much faster, maybe
# you can actually run this.

if (!file.exists("output/ipip-neo_nct-data_complete.rds")) {
  
  nct_volume <- foreach(i = 1:nrow(country_pairs), .packages = packages) %dopar% {
    
    # Subset data
    
    country_1 <- country_pairs$country_1[i]
    country_2 <- country_pairs$country_2[i]
    
    ipip_subset <- ipip %>% 
      filter(country == country_1 | country == country_2) %>% 
      select(country, starts_with("i")) %>% 
      filter(complete.cases(.))
    
    # Retrieve relevant observed test statistics
    
    ## Retrieve relevant confirmatory (observed) networks
    
    country_1_conf_network <- confirmatory_networks[names(confirmatory_networks) == country_1][[1]]
    country_2_conf_network <- confirmatory_networks[names(confirmatory_networks) == country_2][[1]]
    
    ## Network structure
    
    network_diff_obs <- country_1_conf_network - country_2_conf_network
    
    max_diff_obs     <- max(abs(network_diff_obs)) 
    
    ## Global network strength
    
    strength_diff_obs <- sum(abs(country_1_conf_network)) - sum(abs(country_2_conf_network))
    
    # Detect incomplete iterations
    
    if (dir.exists(
      paste("output/nct-output_", country_1, "_", country_2, sep = "")
    )) {
      
      files <- dir(paste("output/nct-output_", country_1, "_", country_2, sep = ""))
      
      start_iter <- length(files) + 1
      
    } else {
      
      start_iter <- 1
      
    }
    
    # Create directory for output
    
    if (!dir.exists(
      paste("output/nct-output_", country_1, "_", country_2, sep = "")
    )) {
      
      dir.create(
        paste("output/nct-output_", country_1, "_", country_2, sep = "")
      )
      
    }
    
    # Network comparison iterations
    
    seed_list <- round(runif(nct_iterations, 1000, 10000))
    
    nct_data <- foreach(j = start_iter:nct_iterations, .combine = bind_rows, .packages = packages) %do% {
      
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
      
      max_diff     <- max(abs(network_diff)) 
      
      ## Global network strength
      
      strength_diff <- sum(abs(country_1_network)) - sum(abs(country_2_network))
      
      ## Store test statistics
      
      nct_out <- data.frame(
        country_1          = country_1,
        country_2          = country_2,
        nct_structure_test = max_diff, 
        nct_strength_test  = strength_diff,
        seed               = seed_list[j]
      )
      
      nct_out$network_diff       <- list(network_diff)
      
      nct_out$country_1_network  <- list(country_1_network)
      nct_out$country_2_network  <- list(country_2_network)
      
      write_rds(nct_out,
                paste("output/nct-output_",
                      country_1, 
                      "_", 
                      country_2,
                      "/nct-out_",
                      country_1, 
                      "_", 
                      country_2,
                      str_pad(j, width = 4, pad = "0"),
                      ".rds",
                      sep = ""))
      
      nct_out
      
    }
    
    # Compute p-values
    
    ## Network structure
    
    ecdf_net_struc  <- ecdf(nct_data$nct_structure_test)
    
    nct_structure_p <- ecdf_net_struc(network_diff_obs)
    
    ## Global network strength
    
    ecdf_strength  <- ecdf(nct_data$nct_strength_test)
    
    nct_strength_p <- ecdf_net_struc(strength_diff_obs)
    
    # Return data
    
    # This will return a large amount of information, but given the computational
    # demands of this procedure, it is desirable to collect as much as possible
    # that might be relevant, to avoid doing it again.
    
    list(
      countries = c(country_1 = country_1,
                    country_2 = country_2),
      obs_tests = c(max_diff_obs = max_diff_obs,
                    strength_diff_obs = strength_diff_obs),
      p_values  = c(nct_structure_p = nct_structure_p, 
                    nct_strength_p  = nct_strength_p),
      nct_data  = nct_data
    )
    
  }
  
  ## Save NCT output
  
  write_rds(nct_volume, "output/ipip-neo_nct-data_complete.rds")
  
} else {
  
  nct_volume <- read_rds("output/ipip-neo_nct-data_complete.rds")
  
}
