################################################################################

# IPIP-NEO Network Analysis - Five Factor and Unconstrained ESEM Approach

################################################################################

# Set up environment -----------------------------------------------------------

packages <- c("tidyverse", 
              "qgraph", 
              "igraph", 
              "psychonetrics", 
              "lavaan",
              "foreach",
              "doParallel",
              "psych",
              "esem")

lapply(packages, library, character.only = TRUE)

# Load data --------------------------------------------------------------------

# Load precleaned data

ipip <- read_csv("data/ipip-neo_cleaned.csv")

# Network models vs. Factor models ---------------------------------------------

# Procedural set up

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

if (!dir.exists("output/ffm-esem/")) {
  
  dir.create("output/ffm-esem/")
  
}

# Sort country sample sizes

sample_cutoff <- 1500

countries <- sort(table(ipip$country), decreasing = TRUE)
countries <- countries[countries > sample_cutoff]

## Five factor model target loadings

big_five_target_loadings <- list(
  n = paste("i", seq(1, 120, 5), sep = ""),
  e = paste("i", seq(2, 120, 5), sep = ""),
  o = paste("i", seq(3, 120, 5), sep = ""),
  a = paste("i", seq(4, 120, 5), sep = ""),
  c = paste("i", seq(5, 120, 5), sep = "")
)

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cl = (cores - 2)/2, cores = cores - 2)

# Modeling ---------------------------------------------------------------------

# Five factor ESEM

if (!file.exists("output/ipip-neo_ffm-esem-model-fit.rds")) {
  
  esem_data <- foreach(i = 1:length(countries), 
                       .packages = packages,
                       .combine = bind_rows) %dopar% {
                               
   country_current <- names(countries[i])
   
   # Load training and test data
   
   training_data <- read_csv(paste("data/training/ipip-neo_training_", 
                                   str_replace_all(tolower(country_current), " ", "_"), 
                                   ".csv", 
                                   sep = ""))
   
   test_data     <- read_csv(paste("data/test/ipip-neo_test_", 
                                   str_replace_all(tolower(country_current), " ", "_"), 
                                   ".csv", 
                                   sep = ""))

   # Fit training model
   
   ffm_target   <- make_target(training_data,
                               big_five_target_loadings)
   
   training_efa <- esem_efa(data     = training_data,
                            fm       = "ml",
                            nfactors = 5,
                            rotate   = "TargetQ",
                            Target   = ffm_target)
   
   esem_specs   <- esem_syntax(training_efa)
   
   ## Store ESEM model syntax
   
   write_lines(esem_specs,
               paste("output/ffm-esem/ipip-neo_esem-syntax_", 
                     str_replace_all(tolower(country_current), " ", "_"),
                     ".txt",
                     sep = ""))
  
   # Fit test model
   
   esem_model <- cfa(esem_specs,
                     test_data,
                     std.lv  = TRUE,
                     estimator = "ML")
   
   ## Store standardized solution
   
   if (esem_model@optim$converged == TRUE) {
     
     esem_std <- standardizedsolution(esem_model)
     
     write_rds(esem_std,
               paste("output/ffm-esem/ipip-neo_esem-std-solution_", 
                     str_replace_all(tolower(country_current), " ", "_"),
                     ".rds",
                     sep = ""))
     
   }
   
   ## Calculate fit indices
   
   ### Big Five
   
   if (esem_model@optim$converged == TRUE) {
     
     esem_fit <- fitmeasures(esem_model, 
                             fit.measures = c("cfi", "tli", "rmsea", "bic"))
     
   } else {
     
     esem_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
     
   }
   
   # Record fit indices
   
   data.frame(
     country       = country_current,
     cfi_esem      = esem_fit[names(esem_fit) == "cfi"],   
     tli_esem      = esem_fit[names(esem_fit) == "tli"],   
     rmsea_esem    = esem_fit[names(esem_fit) == "rmsea"], 
     bic_esem      = esem_fit[names(esem_fit) == "bic"]
   )
   
 }
  
  write_rds(esem_data, 
            "output/ipip-neo_ffm-esem-model-fit.rds")
  
  write_csv(esem_data, 
            "output/ipip-neo_ffm-esem-model-fit.csv")
  
} else {
  
  esem_data <- read_rds("output/ipip-neo_ffm-esem-model-fit.rds")
  
}

# Unconstrained ESEM

# This approach allows the training data to determine the number of factors used
# in the ESEM in the test data. It is highly flexible but may substantially
# deviate from the Five Factor Model of personality.

if (!file.exists("output/ipip-neo_uc-esem-model-fit.rds")) {
  
  esem_uc_data <- foreach(i = 1:length(countries), 
                          .packages = packages,
                          .combine = bind_rows) %dopar% {
                         
   country_current <- names(countries[i])
   
   # Load training and test data
   
   training_data <- read_csv(paste("data/training/ipip-neo_training_", 
                                   str_replace_all(tolower(country_current), " ", "_"), 
                                   ".csv", 
                                   sep = ""))
   
   test_data     <- read_csv(paste("data/test/ipip-neo_test_", 
                                   str_replace_all(tolower(country_current), " ", "_"), 
                                   ".csv", 
                                   sep = ""))
   
   # Fit training model
   
   training_pa  <- fa.parallel(training_data, 
                               fm = "ml", 
                               fa = "fa")
  
   training_efa <- esem_efa(data     = training_data,
                            fm       = "ml",
                            nfactors = training_pa$nfact,
                            rotate   = "geominT")
   
   esem_specs   <- esem_syntax(training_efa)
   
   ## Store ESEM model syntax
   
   write_lines(esem_specs,
               paste("output/ffm-esem/ipip-neo_uc-esem-syntax_", 
                     str_replace_all(tolower(country_current), " ", "_"),
                     ".txt",
                     sep = ""))
   
   # Fit test model
   
   esem_model <- cfa(esem_specs,
                     test_data,
                     std.lv  = TRUE,
                     estimator = "ML")
   
   ## Store standardized solution
   
   if (esem_model@optim$converged == TRUE) {
     
     esem_std <- standardizedsolution(esem_model)
     
     write_rds(esem_std,
               paste("output/ffm-esem/ipip-neo_uc-esem-std-solution_", 
                     str_replace_all(tolower(country_current), " ", "_"),
                     ".rds",
                     sep = ""))
     
   }
   
   ## Calculate fit indices
   
   ### Big Five
   
   if (esem_model@optim$converged == TRUE) {
     
     esem_fit <- fitmeasures(esem_model, 
                             fit.measures = c("cfi", "tli", "rmsea", "bic"))
     
   } else {
     
     esem_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
     
   }
   
   # Record fit indices
   
   data.frame(
     country       = country_current,
     cfi_ucesem    = esem_fit[names(esem_fit) == "cfi"],   
     tli_ucesem    = esem_fit[names(esem_fit) == "tli"],   
     rmsea_ucesem  = esem_fit[names(esem_fit) == "rmsea"], 
     bic_ucesem    = esem_fit[names(esem_fit) == "bic"],
     nfactors      = training_pa$nfact
   )
   
 }
  
  write_rds(esem_uc_data, 
            "output/ipip-neo_uc-esem-model-fit.rds")
  
  write_csv(esem_uc_data, 
            "output/ipip-neo_uc-esem-model-fit.csv")
  
} else {
  
  esem_uc_data <- read_rds("output/ipip-neo_uc-esem-model-fit.rds")
  
}

