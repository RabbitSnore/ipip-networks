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
              "doParallel",
              "readxl",
              "lme4",
              "performance",
              "cowplot")

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

country_pairs <- combn(countries, 2) %>%
  t() %>% 
  as.data.frame()

colnames(country_pairs) <- c("country_1", "country_2")

# Iterations for network comparison tests

nct_iterations <- 1000

# Parallel computing set up

cores <- detectCores()

registerDoParallel(cores)

# Fit confirmatory models ------------------------------------------------------

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) for each country, using the full data set for
# that country (excluding incomplete cases), instead of the just the training or
# test sets. This process is computationally intensive and is set up for
# parallel computation using all available cores.

# Note that these are "confirmatory" in the sense that the model is
# prespecified, having been estimated from a training subset of the data. But
# they are not confirmatory in the sense of strong hypothesis testing.

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

# Walktrap community identification

walktrap_communities <- foreach(i = 1:length(countries), .packages = packages) %do% {
  
  walktrap.community(
    as.igraph(qgraph(confirmatory_networks[[i]])), 
    weights = abs(E(as.igraph(qgraph(confirmatory_networks[[i]]))))
    )
  
}

## Add country names

names(walktrap_communities) <- countries

# Create network visualizations

if (!dir.exists("figures")) {
  
  dir.create("figures")
  
}

## Plot networks with walktrap communities indicated by color

network_graphs_walktrap <- foreach(i = 1:length(countries)) %do% {
  
  qgraph(confirmatory_networks[[i]],
         layout    = "spring",
         color     = walktrap_communities[[i]]$membership,
         theme     = "colorblind",
         filename  = paste("figures/ipip-neo_network-plot_", 
                           str_replace_all(tolower(countries[i]),
                                           " ",
                                           "_"),
                           "_walktrap",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5
  )
  
}

## Plot networks with Big Five traits indicated by color

network_graphs_bigfive <- foreach(i = 1:length(countries)) %do% {
  
  qgraph(confirmatory_networks[[i]],
         layout    = "spring",
         groups    = as.factor(ipip_key$trait),
         theme     = "colorblind",
         filename  = paste("figures/ipip-neo_network-plot_", 
                           str_replace_all(tolower(countries[i]),
                                           " ",
                                           "_"),
                           "_bigfive",
                           sep = ""),
         filetype  = "png",
         height    = 5,
         width     = 5,
         legend    = FALSE
  )
  
}

#  Centrality measures ---------------------------------------------------------

## Calculate centrality measures for each country's network

centrality_data <- foreach(i = 1:length(countries), .packages = packages, .combine = bind_rows) %do% {
  
  strength    <- centrality(confirmatory_networks[[i]])$InDegree
  closeness   <- centrality(confirmatory_networks[[i]])$Closeness
  betweenness <- centrality(confirmatory_networks[[i]])$Betweenness
  
  data.frame(
    country     = countries[i],
    item        = 1:120,
    strength    = strength,
    closeness   = closeness,
    betweenness = betweenness
  )
  
}

centrality_data_long <- centrality_data %>% 
  pivot_longer(
    cols      = c("strength", "closeness", "betweenness"),
    names_to  = "measure",
    values_to = "centrality"
  )

## Visualization of centrality

centrality_plot <- 
ggplot(centrality_data_long %>% 
         # Remove cases from networks that are not fully connected
         filter(centrality != 0),
       aes(
         x = centrality,
         y = item,
         group = country
       )) +
  facet_wrap(~ measure, 
             scales = "free_x") +
  geom_line(
    orientation = "y",
    alpha = .10
    ) +
  scale_y_continuous(
    breaks = 1:120
  ) +
  labs(
    x = "",
    y = "Item"
  ) +
  theme_classic() +
  theme(
    axis.text.y = element_text(size = 7)
  )

### Save centrality figure

save_plot("figures/ipip-neo_centrality-plot.png", centrality_plot,
          base_height = 10, base_width = 14)

## Model centrality

### Unconditional linear mixed effects models

lmm_strength     <- lmer(strength 
                         ~ (1|country) 
                         + (1|item), 
                         data = centrality_data)

lmm_closeness    <- lmer(closeness 
                         ~ (1|country) 
                         + (1|item), 
                         # Remove cases from networks that are not fully connected
                         data = filter(centrality_data, closeness != 0))

lmm_betweenness  <- lmer(betweenness 
                         ~ (1|country) 
                         + (1|item), 
                         data = centrality_data)

### Intraclass correlation coefficients

# These can be interpreted as estimates of the variance in centrality explained
# by items and by country membership. These measures cannot give a nuanced
# assessment of the structures of the networks. Rather, the ICCs for country
# membership can be thought of as answering the question, "How much of the
# overall variation in network strength/closeness/betweenness can be attributed
# to the country being modeled?"

icc_strength    <- icc(lmm_strength, by_group = TRUE)
icc_closeness   <- icc(lmm_closeness, by_group = TRUE)
icc_betweenness <- icc(lmm_betweenness, by_group = TRUE)

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
    
    max_diff     <- max(abs(network_diff)) 
    
    ## Global network strength
    
    strength_diff <- sum(abs(country_1_network)) - sum(abs(country_2_network))
    
    ## Store test statistics
    
    nct_out <- data.frame(
      country_1          = country_1,
      country_2          = country_2,
      nct_structure_test = max_diff, 
      nct_strength_test  = strength_diff,
      network_diff       = network_diff,
      seed               = seed_list[j]
      )
    
    nct_out$country_1_network  <- country_1_network
    nct_out$country_2_network  <- country_2_network
    
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

write_rds(nct_volume, "output/ipip-neo_nct-data.rds")
