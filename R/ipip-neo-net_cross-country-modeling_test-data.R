################################################################################

# IPIP-NEO Network Analysis - Cross-Country Network Fitting (Test Data)

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "psychonetrics", 
              "foreach",
              "doParallel",
              "readxl",
              "ggbeeswarm",
              "cowplot")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

ipip <- read_csv("data/ipip-neo_cleaned.csv")

# Load network modeling output

## If reproducing from scratch, you will need to have run the network vs. factor
## model script to generate the comparison output

ipip_comparison <- read_rds("output/ipip-neo_model-comparison-data.rds")

# Load confirmatory network data

confirmatory_networks <- read_rds("output/ipip-neo_confirmatory-networks.rds")

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

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cores)

# Cross-country confirmatory network modeling ----------------------------------

# This loop will fit a confirmatory network (i.e., the same network used for the
# network vs. factor comparisons) from each country to each other country,
# thereby testing how well each country's specific network model performs in the
# data for each other country. This process is computationally intensive and is
# set up for parallel computation using all available cores.

if (!file.exists("output/ipip-neo_cross-country-test-data.rds")) {
  
  cross_country_data <- foreach(i = 1:nrow(country_pairs), 
                                .packages = packages,
                                .combine  = bind_rows) %dopar% {
    
    # Subset data
    
    country_1 <- country_pairs$country_1[i]
    country_2 <- country_pairs$country_2[i]
    
    ipip_subset <- read_csv(paste("data/test/ipip-neo_test_", 
                                  tolower(country_2), 
                                  ".csv", 
                                  sep = ""))
    
    # Set up empty data
    
    fit_data <- data.frame(
      country_1     = country_1,
      country_2     = country_2,
      cfi_network   = NA,   
      tli_network   = NA,   
      rmsea_network = NA, 
      bic_network   = NA
    )
    
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
    
    fit_data$cfi_network    <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    fit_data$tli_network    <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    fit_data$rmsea_network  <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    fit_data$bic_network    <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    
    # Store networks
    
    fit_data$omega_matrix <- list(getmatrix(cross_country_network,
                                            matrix = "omega"))
    
    fit_data
    
  }
  
  # Store data
  
  write_rds(cross_country_data, "output/ipip-neo_cross-country-test-data.rds")
  
  ## Store simplified data
  
  write_csv(cross_country_data %>% 
              select(-omega_matrix), 
            file = "output/ipip-neo_cross-country-test-data.csv")
  
} else {
  
  cross_country_data <- read_rds("output/ipip-neo_cross-country-test-data.rds")
  
}

# Wrangle ----------------------------------------------------------------------

# Combine cross-country fit measures with within-country fit measures

ipip_network_fit <- ipip_comparison %>% 
  select(
    country_1 = country,
    country_2 = country,
    ends_with("network")
  )

cross_country_fit <- bind_rows(
  cross_country_data %>% 
    select(-omega_matrix), 
  ipip_network_fit
  ) %>% 
  arrange(by = country_2) %>% 
  arrange(by = country_1)

# Fit measure matrices

## Create matrices

matrix_cfi   <- matrix(cross_country_fit$cfi_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_tli   <- matrix(cross_country_fit$tli_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_rmsea <- matrix(cross_country_fit$rmsea_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

matrix_bic   <- matrix(cross_country_fit$bic_network, 
                       nrow = 27,
                       dimnames = list(unique(cross_country_fit$country_1),
                                       unique(cross_country_fit$country_1)))

## Store matrices

matrix_cfi   <- as.data.frame(matrix_cfi) 
matrix_tli   <- as.data.frame(matrix_tli)  
matrix_rmsea <- as.data.frame(matrix_rmsea) 
matrix_bic   <- as.data.frame(matrix_bic) 

### Full

write.csv(matrix_cfi,   "output/ipip-neo_matrix-cfi-test.csv")
write.csv(matrix_tli,   "output/ipip-neo_matrix-tli-test.csv")
write.csv(matrix_rmsea, "output/ipip-neo_matrix-rmsea-test.csv")
write.csv(matrix_bic,   "output/ipip-neo_matrix-bic-test.csv")

### Readable (rounded to three digits)

write.csv(round(matrix_cfi, 3),   "output/ipip-neo_matrix-cfi-test-rounded.csv")
write.csv(round(matrix_tli, 3),   "output/ipip-neo_matrix-tli-test-rounded.csv")
write.csv(round(matrix_rmsea, 3), "output/ipip-neo_matrix-rmsea-test-rounded.csv")

# Visualization and description ------------------------------------------------

# Data for visualization

## Country names

country_names <- c(
  "Afghanistan",
  "Albania",
  "Australia",
  "Canada", 
  "China", 
  "Denmark", 
  "Finland", 
  "France", 
  "Germany", 
  "Greece", 
  "Hong Kong", 
  "India", 
  "Ireland", 
  "Malaysia", 
  "Mexico", 
  "Netherlands", 
  "New Zealand", 
  "Norway", 
  "Philippines", 
  "Romania", 
  "Singapore", 
  "South Africa", 
  "South Korea", 
  "Sweden", 
  "Thailand", 
  "UK", 
  "USA")

## Identify whether fit measures correspond to origin country

cross_country_fit <- cross_country_fit %>% 
  mutate(
    model_source = case_when(
      country_1 == country_2 ~ 1,
      country_1 != country_2 ~ 0
    )
  )

## Cross country BIC comparison

cross_country_bic <- cross_country_fit %>%
  group_by(country_2) %>% 
  mutate(
    bic_scaled = as.numeric(scale(bic_network))
  ) %>% 
  ungroup()

bic_summary <- cross_country_bic %>% 
  group_by(country_2) %>% 
  summarise(
    mean_bic   = mean(bic_network, na.rm = TRUE),
    sd_bic     = sd(bic_network, na.rm = TRUE)
  ) %>% 
  ungroup()

## Long form BIC data for model comparison

test_data_bic_long <- ipip_comparison %>% 
  pivot_longer(
    cols = starts_with("bic"),
    names_to = "model",
    values_to = "bic"
  ) %>% 
  left_join(
    select(bic_summary, country = country_2, mean_bic, sd_bic),
    by = "country"
  ) %>% 
  group_by(country) %>% 
  mutate(
    # Scaled for cross-country network comparison
    bic_scaled      = (bic - mean_bic) / sd_bic,
    # Scaled for network to factor model comparison
    bic_scaled_comp = as.numeric(scale(bic))
  ) %>% 
  ungroup()

## Identify best fitting models

cross_country_best <- cross_country_fit %>% 
  group_by(country_2) %>% 
  summarise(
    best_model_bic   = country_1[which(bic_network == min(bic_network))],
    best_model_rmsea = country_1[which(rmsea_network == min(rmsea_network))],
    best_model_cfi   = country_1[which(cfi_network == max(cfi_network))]
  ) %>% 
  rename(
    country = country_2
  )

## Bayes factors for each country's model vs. origin's model

cross_country_bf <- cross_country_fit %>% 
  left_join(
    select(
      filter(cross_country_fit,
             country_1 == country_2),
      country_2,
      bic_origin = bic_network
    ), 
    by = "country_2"
  ) %>% 
  mutate(
    bf_origin  = exp( (bic_network - bic_origin) / 2 ),
    bf_e_power = (bic_network - bic_origin) / 2
  ) %>% 
  filter(country_1 != country_2)

cross_country_bf_descriptives <- cross_country_bf %>% 
  group_by(country_2) %>% 
  summarise(
    origin_in_favor = sum(bf_e_power > 0),
    prop_origin     = sum(bf_e_power > 0)/n()
  )

# Swarm plots of fit statistics

## BIC

swarm_bic_cross_country <- 
ggplot(cross_country_bic,
       aes(
         x     = bic_scaled,
         y     = country_2,
         color = as.factor(model_source),
         size  = as.factor(model_source),
       )) +
  geom_quasirandom(
    alpha = .50
  ) +
  geom_point(
    data = test_data_bic_long,
    aes(
      y = country
    ),
    color = "#B5446E",
    size = .75,
    alpha = .50
  ) +
  scale_color_manual(
    values = c("#355070", "#53131E"),
    labels = c("Other Countries", "Origin")
  ) +
  scale_size_discrete(
    range = c(.50, 1.5),
    labels = c("Other Countries", "Origin")
  ) +
  scale_y_discrete(
    labels = country_names
  ) +
  labs(
    y     = "Data Origin",
    x     = "BIC (standardized within country)",
    color = "Model Origin",
    size  = "Model Origin",
    subtitle = "Cross-Country Network Invariance"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

swarm_bic_model_comparison <- 
  ggplot(test_data_bic_long,
         aes(
           x     = bic_scaled_comp,
           y     = country,
           color = as.factor(model)
         )) +
  geom_point(
    size = 1
  ) +
  scale_color_manual(
    labels = c("Bifactor", "Big Five", "Higher Order", "Network"),
    values = c("#37123C", "#FE7F2D", "#5995ED", "#619B8A")
  ) +
  scale_y_discrete(
    labels = country_names
  ) +
  labs(
    color = "Model",
    y = "",
    x = "BIC (standardized within country)",
    subtitle = "Comparison of models"
  ) +
  theme_classic() +
  theme(
    legend.position = "bottom"
  )

## Combine plots

swarm_plots_bic <- plot_grid(swarm_bic_model_comparison, 
                             swarm_bic_cross_country, 
                             nrow = 1, rel_widths = c(1, 1.1))

## Save figure

save_plot("figures/ipip-neo_bic_test-data_model-comparison-swarms.png", swarm_plots_bic,
          base_width = 10.5, base_height = 8)
