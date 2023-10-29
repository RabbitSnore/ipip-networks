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

# Network models vs. Factor models ---------------------------------------------

# Procedural set up

train_test_ratio <- c(.50, .50)

sample_cutoff <- 1500

if (!dir.exists("output")) {
  
  dir.create("output")
  
}

## Five factor models

big_five <- 
'

o =~ i3 + i8 + i13 + i18 + i23 + i28 + i33 + i38 + i43 + i48 + i53 + i58 + i63 + i68 + i73 + i78 + i83 + i88 + i93 + i98 + i103 + i108 + i113 + i118

c =~ i5 + i10 + i15 + i20 + i25 + i30 + i35 + i40 + i45 + i50 + i55 + i60 + i65 + i70 + i75 + i80 + i85 + i90 + i95 + i100 + i105 + i110 + i115 + i120

e =~ i2 + i7 + i12 + i17 + i22 + i27 + i32 + i37 + i42 + i47 + i52 + i57 + i62 + i67 + i72 + i77 + i82 + i87 + i92 + i97 + i102 + i107 + i112 + i117

a =~ i4 + i9 + i14 + i19 + i24 + i29 + i34 + i39 + i44 + i49 + i54 + i59 + i64 + i69 + i74 + i79 + i84 + i89 + i94 + i99 + i104 + i109 + i114 + i119

n =~ i1 + i6 + i11 + i16 + i21 + i26 + i31 + i36 + i41 + i46 + i51 + i56 + i61 + i66 + i71 + i76 + i81 + i86 + i91 + i96 + i101 + i106 + i111 + i116

'

bifactor <- 
'

o =~ i3 + i8 + i13 + i18 + i23 + i28 + i33 + i38 + i43 + i48 + i53 + i58 + i63 + i68 + i73 + i78 + i83 + i88 + i93 + i98 + i103 + i108 + i113 + i118

c =~ i5 + i10 + i15 + i20 + i25 + i30 + i35 + i40 + i45 + i50 + i55 + i60 + i65 + i70 + i75 + i80 + i85 + i90 + i95 + i100 + i105 + i110 + i115 + i120

e =~ i2 + i7 + i12 + i17 + i22 + i27 + i32 + i37 + i42 + i47 + i52 + i57 + i62 + i67 + i72 + i77 + i82 + i87 + i92 + i97 + i102 + i107 + i112 + i117

a =~ i4 + i9 + i14 + i19 + i24 + i29 + i34 + i39 + i44 + i49 + i54 + i59 + i64 + i69 + i74 + i79 + i84 + i89 + i94 + i99 + i104 + i109 + i114 + i119

n =~ i1 + i6 + i11 + i16 + i21 + i26 + i31 + i36 + i41 + i46 + i51 + i56 + i61 + i66 + i71 + i76 + i81 + i86 + i91 + i96 + i101 + i106 + i111 + i116

anxiety              =~ i1 + i31 + i61 + i91
friendliness         =~ i2 + i32 + i62 + i92
imagination          =~ i3 + i33 + i63 + i93
trust                =~ i4 + i34 + i64 + i94
self_efficacy        =~ i5 + i35 + i65 + i95
anger                =~ i6 + i36 + i66 + i96
gregariousness       =~ i7 + i37 + i67 + i97
artistic_interests   =~ i8 + i38 + i68 + i98
morality             =~ i9 + i39 + i69 + i99
orderliness          =~ i10 + i40 + i70 + i100
depression           =~ i11 + i41 + i71 + i101
assertiveness        =~ i12 + i42 + i72 + i102
emotionality         =~ i13 + i43 + i73 + i103
altruism             =~ i14 + i44 + i74 + i104
dutifulness          =~ i15 + i45 + i75 + i105
self_consciousness   =~ i16 + i46 + i76 + i106
activity_level       =~ i17 + i47 + i77 + i107
adventurousness      =~ i18 + i48 + i78 + i108
cooperation          =~ i19 + i49 + i79 + i109
achievement_striving =~ i20 + i50 + i80 + i110
immoderation         =~ i21 + i51 + i81 + i111
excitement_seeking   =~ i22 + i52 + i82 + i112
intellect            =~ i23 + i53 + i83 + i113
modesty              =~ i24 + i54 + i84 + i114
self_discipline      =~ i25 + i55 + i85 + i115
vulnerability        =~ i26 + i56 + i86 + i116
cheerfulness         =~ i27 + i57 + i87 + i117
liberalism           =~ i28 + i58 + i88 + i118
sympathy             =~ i29 + i59 + i89 + i119
cautiousness         =~ i30 + i60 + i90 + i120

anxiety              ~~ 0*anxiety             
friendliness         ~~ 0*friendliness        
imagination          ~~ 0*imagination         
trust                ~~ 0*trust               
self_efficacy        ~~ 0*self_efficacy       
anger                ~~ 0*anger               
gregariousness       ~~ 0*gregariousness      
artistic_interests   ~~ 0*artistic_interests  
morality             ~~ 0*morality            
orderliness          ~~ 0*orderliness         
depression           ~~ 0*depression          
assertiveness        ~~ 0*assertiveness       
emotionality         ~~ 0*emotionality        
altruism             ~~ 0*altruism            
dutifulness          ~~ 0*dutifulness         
self_consciousness   ~~ 0*self_consciousness  
activity_level       ~~ 0*activity_level      
adventurousness      ~~ 0*adventurousness     
cooperation          ~~ 0*cooperation         
achievement_striving ~~ 0*achievement_striving
immoderation         ~~ 0*immoderation        
excitement_seeking   ~~ 0*excitement_seeking  
intellect            ~~ 0*intellect           
modesty              ~~ 0*modesty             
self_discipline      ~~ 0*self_discipline     
vulnerability        ~~ 0*vulnerability       
cheerfulness         ~~ 0*cheerfulness        
liberalism           ~~ 0*liberalism          
sympathy             ~~ 0*sympathy            
cautiousness         ~~ 0*cautiousness        

anxiety              ~~ 0*o
friendliness         ~~ 0*o
imagination          ~~ 0*o
trust                ~~ 0*o
self_efficacy        ~~ 0*o
anger                ~~ 0*o
gregariousness       ~~ 0*o
artistic_interests   ~~ 0*o
morality             ~~ 0*o
orderliness          ~~ 0*o
depression           ~~ 0*o
assertiveness        ~~ 0*o
emotionality         ~~ 0*o
altruism             ~~ 0*o
dutifulness          ~~ 0*o
self_consciousness   ~~ 0*o
activity_level       ~~ 0*o
adventurousness      ~~ 0*o
cooperation          ~~ 0*o
achievement_striving ~~ 0*o
immoderation         ~~ 0*o
excitement_seeking   ~~ 0*o
intellect            ~~ 0*o
modesty              ~~ 0*o
self_discipline      ~~ 0*o
vulnerability        ~~ 0*o
cheerfulness         ~~ 0*o
liberalism           ~~ 0*o
sympathy             ~~ 0*o
cautiousness         ~~ 0*o

anxiety              ~~ 0*c
friendliness         ~~ 0*c
imagination          ~~ 0*c
trust                ~~ 0*c
self_efficacy        ~~ 0*c
anger                ~~ 0*c
gregariousness       ~~ 0*c
artistic_interests   ~~ 0*c
morality             ~~ 0*c
orderliness          ~~ 0*c
depression           ~~ 0*c
assertiveness        ~~ 0*c
emotionality         ~~ 0*c
altruism             ~~ 0*c
dutifulness          ~~ 0*c
self_consciousness   ~~ 0*c
activity_level       ~~ 0*c
adventurousness      ~~ 0*c
cooperation          ~~ 0*c
achievement_striving ~~ 0*c
immoderation         ~~ 0*c
excitement_seeking   ~~ 0*c
intellect            ~~ 0*c
modesty              ~~ 0*c
self_discipline      ~~ 0*c
vulnerability        ~~ 0*c
cheerfulness         ~~ 0*c
liberalism           ~~ 0*c
sympathy             ~~ 0*c
cautiousness         ~~ 0*c

anxiety              ~~ 0*e
friendliness         ~~ 0*e
imagination          ~~ 0*e
trust                ~~ 0*e
self_efficacy        ~~ 0*e
anger                ~~ 0*e
gregariousness       ~~ 0*e
artistic_interests   ~~ 0*e
morality             ~~ 0*e
orderliness          ~~ 0*e
depression           ~~ 0*e
assertiveness        ~~ 0*e
emotionality         ~~ 0*e
altruism             ~~ 0*e
dutifulness          ~~ 0*e
self_consciousness   ~~ 0*e
activity_level       ~~ 0*e
adventurousness      ~~ 0*e
cooperation          ~~ 0*e
achievement_striving ~~ 0*e
immoderation         ~~ 0*e
excitement_seeking   ~~ 0*e
intellect            ~~ 0*e
modesty              ~~ 0*e
self_discipline      ~~ 0*e
vulnerability        ~~ 0*e
cheerfulness         ~~ 0*e
liberalism           ~~ 0*e
sympathy             ~~ 0*e
cautiousness         ~~ 0*e

anxiety              ~~ 0*a
friendliness         ~~ 0*a
imagination          ~~ 0*a
trust                ~~ 0*a
self_efficacy        ~~ 0*a
anger                ~~ 0*a
gregariousness       ~~ 0*a
artistic_interests   ~~ 0*a
morality             ~~ 0*a
orderliness          ~~ 0*a
depression           ~~ 0*a
assertiveness        ~~ 0*a
emotionality         ~~ 0*a
altruism             ~~ 0*a
dutifulness          ~~ 0*a
self_consciousness   ~~ 0*a
activity_level       ~~ 0*a
adventurousness      ~~ 0*a
cooperation          ~~ 0*a
achievement_striving ~~ 0*a
immoderation         ~~ 0*a
excitement_seeking   ~~ 0*a
intellect            ~~ 0*a
modesty              ~~ 0*a
self_discipline      ~~ 0*a
vulnerability        ~~ 0*a
cheerfulness         ~~ 0*a
liberalism           ~~ 0*a
sympathy             ~~ 0*a
cautiousness         ~~ 0*a

anxiety              ~~ 0*n
friendliness         ~~ 0*n
imagination          ~~ 0*n
trust                ~~ 0*n
self_efficacy        ~~ 0*n
anger                ~~ 0*n
gregariousness       ~~ 0*n
artistic_interests   ~~ 0*n
morality             ~~ 0*n
orderliness          ~~ 0*n
depression           ~~ 0*n
assertiveness        ~~ 0*n
emotionality         ~~ 0*n
altruism             ~~ 0*n
dutifulness          ~~ 0*n
self_consciousness   ~~ 0*n
activity_level       ~~ 0*n
adventurousness      ~~ 0*n
cooperation          ~~ 0*n
achievement_striving ~~ 0*n
immoderation         ~~ 0*n
excitement_seeking   ~~ 0*n
intellect            ~~ 0*n
modesty              ~~ 0*n
self_discipline      ~~ 0*n
vulnerability        ~~ 0*n
cheerfulness         ~~ 0*n
liberalism           ~~ 0*n
sympathy             ~~ 0*n
cautiousness         ~~ 0*n

'

higher_order <- 
'

anxiety              =~ i1 + i31 + i61 + i91
friendliness         =~ i2 + i32 + i62 + i92
imagination          =~ i3 + i33 + i63 + i93
trust                =~ i4 + i34 + i64 + i94
self_efficacy        =~ i5 + i35 + i65 + i95
anger                =~ i6 + i36 + i66 + i96
gregariousness       =~ i7 + i37 + i67 + i97
artistic_interests   =~ i8 + i38 + i68 + i98
morality             =~ i9 + i39 + i69 + i99
orderliness          =~ i10 + i40 + i70 + i100
depression           =~ i11 + i41 + i71 + i101
assertiveness        =~ i12 + i42 + i72 + i102
emotionality         =~ i13 + i43 + i73 + i103
altruism             =~ i14 + i44 + i74 + i104
dutifulness          =~ i15 + i45 + i75 + i105
self_consciousness   =~ i16 + i46 + i76 + i106
activity_level       =~ i17 + i47 + i77 + i107
adventurousness      =~ i18 + i48 + i78 + i108
cooperation          =~ i19 + i49 + i79 + i109
achievement_striving =~ i20 + i50 + i80 + i110
immoderation         =~ i21 + i51 + i81 + i111
excitement_seeking   =~ i22 + i52 + i82 + i112
intellect            =~ i23 + i53 + i83 + i113
modesty              =~ i24 + i54 + i84 + i114
self_discipline      =~ i25 + i55 + i85 + i115
vulnerability        =~ i26 + i56 + i86 + i116
cheerfulness         =~ i27 + i57 + i87 + i117
liberalism           =~ i28 + i58 + i88 + i118
sympathy             =~ i29 + i59 + i89 + i119
cautiousness         =~ i30 + i60 + i90 + i120

o =~ imagination + artistic_interests + emotionality + adventurousness + intellect + liberalism
c =~ self_efficacy + orderliness + dutifulness + achievement_striving + self_discipline + cautiousness
e =~ friendliness + gregariousness + assertiveness + activity_level + excitement_seeking + cheerfulness
a =~ trust + morality + altruism + cooperation + modesty + sympathy
n =~ anxiety + anger + depression + self_consciousness + immoderation + vulnerability

'

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cores)

# Sort country sample sizes

countries <- sort(table(ipip$country), decreasing = TRUE)
countries <- countries[countries > sample_cutoff]

# Descriptive table

country_descriptives <- ipip %>% 
  group_by(country) %>% 
  summarise(
    N        = n(),
    female   = sum(sex == 2)/n(),
    age_mean = mean(age),
    age_sd   = sd(age),
    age_med  = median(age)
  ) %>% 
  filter(N > sample_cutoff) %>% 
  filter(!is.na(country))

write_csv(country_descriptives, "output/ipip-neo_country-descriptives.csv")

# Create data table

set.seed(1111)

seed_list <- round(runif(length(countries), 1000, 10000))

## Set up empty vectors

### Sample sizes

n_train        <- rep(NA, length(seed_list))
n_test         <- rep(NA, length(seed_list))

### Model fit statistics

cfi_network    <- rep(NA, length(seed_list))
cfi_big_five   <- rep(NA, length(seed_list))
cfi_bifactor   <- rep(NA, length(seed_list))
cfi_higher     <- rep(NA, length(seed_list))

tli_network    <- rep(NA, length(seed_list))
tli_big_five   <- rep(NA, length(seed_list))
tli_bifactor   <- rep(NA, length(seed_list))
tli_higher     <- rep(NA, length(seed_list))

rmsea_network  <- rep(NA, length(seed_list))
rmsea_big_five <- rep(NA, length(seed_list))
rmsea_bifactor <- rep(NA, length(seed_list))
rmsea_higher   <- rep(NA, length(seed_list))

bic_network    <- rep(NA, length(seed_list))
bic_big_five   <- rep(NA, length(seed_list))
bic_bifactor   <- rep(NA, length(seed_list))
bic_higher     <- rep(NA, length(seed_list))

### Extracted network model skeletons

omega_list <- vector("list", length(seed_list))

### Data frame

comparison_data <- data.frame(
  country = names(countries), 
  n_train,       
  n_test,
  cfi_network,   
  cfi_big_five,  
  cfi_bifactor,  
  cfi_higher,    
  tli_network,   
  tli_big_five,  
  tli_bifactor,  
  tli_higher,    
  rmsea_network, 
  rmsea_big_five,
  rmsea_bifactor,
  rmsea_higher,
  bic_network, 
  bic_big_five,
  bic_bifactor,
  bic_higher  
)

comparison_data$omega_list <- omega_list

# Iterate network analysis and factor modeling over countries

# Note that this is not set up to be parallelized, but it could be (and maybe
# should be). On a typical personal computer, you can expect this loop to take
# many hours.

if (!file.exists("output/ipip-neo_model-comparison-data.rds")) {


  for (i in 1:length(countries)) {
    
    country_current <- names(countries[i])
    
    # Select relevant cases
    
    model_data <- ipip %>% 
      filter(country == country_current) %>%
      select(starts_with("i")) %>% 
      filter(complete.cases(.))
    
    # Update sample size in the countries vector
    
    countries[i] <- nrow(model_data)
    
    # Split into training and test sets
    
    set.seed(seed_list[i])
    
    training_indices <- sample(1:countries[i], 
                               size = round(countries[i]*train_test_ratio[1]), 
                               replace = FALSE)
    
    training_data    <- model_data[ training_indices, ] 
    test_data        <- model_data[-training_indices, ] 
    
    comparison_data$n_train[i] <- nrow(training_data)
    comparison_data$n_test[i]  <- nrow(test_data)
    
    # Fit training model
    
    training_network <- EBICglasso(cov(training_data),
                                   n = nrow(training_data),
                                   nlambda = 1000,
                                   lambda.min.ratio = 0.01,
                                   returnAllResults = TRUE)
    
    ## Repeat training fit if sparsity may be violated
    
    lambda_index <- which(training_network$ebic == min(training_network$ebic))
    lambda_opt   <- training_network$lambda[lambda_index]
    
    if (lambda_opt == min(training_network$lambda)) {
      
      training_network <- EBICglasso(cov(training_data),
                                     n = nrow(training_data),
                                     nlambda = 10000, # Increase tested lambdas
                                     lambda.min.ratio = 0.1, # Increase ratio
                                     returnAllResults = TRUE)
      
      lambda_index <- which(training_network$ebic == min(training_network$ebic))
      lambda_opt   <- training_network$lambda[lambda_index]
      
      if (lambda_opt == min(training_network$lambda)) {
        
        training_network <- EBICglasso(cov(training_data),
                                       n = nrow(training_data),
                                       nlambda = 100000, # Increase tested lambdas
                                       threshold = TRUE, # Enforce threshold
                                       lambda.min.ratio = 0.1,
                                       returnAllResults = TRUE)
        
      }
      
    }
    
    ## Extract training model skeleton
    
    omega_skeleton <- training_network$optnet
    
    omega_skeleton[omega_skeleton != 0] <- 1
    
    comparison_data$omega_list[[i]] <- omega_skeleton
    
    # Fit test model
    
    test_network <- 
      varcov(data  = test_data,
             type  = "ggm",
             omega = omega_skeleton) %>% 
      runmodel()
    
    ## Calculate fit indices
    
    test_net_fit <- fit(test_network)
    
    # Fit five-factor models
    
    test_big_five <- cfa(big_five,
                         data = test_data,
                         estimator = "ML")
    
    test_bifactor <- cfa(bifactor,
                         data = test_data,
                         estimator = "ML")
    
    test_higher   <- cfa(higher_order,
                         data = test_data,
                         estimator  = "ML",
                         orthogonal.y = TRUE)
    
    ## Calculate fit indices
    
    ### Big Five
    
    if (test_big_five@optim$converged == TRUE) {
      
      test_big_five_fit <- fitmeasures(test_big_five, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_big_five_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    if (test_big_five@optim$converged == TRUE) {
      
      test_big_five_fit <- fitmeasures(test_big_five, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_big_five_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ### Bifactor
    
    if (test_bifactor@optim$converged == TRUE) {
      
      test_bifactor_fit <- fitmeasures(test_bifactor, 
                                     fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_bifactor_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ### Higher Order
    
    if (test_higher@optim$converged == TRUE) {
      
      test_higher_fit <- fitmeasures(test_higher, 
                                     fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_higher_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    # Record fit indices
    
    comparison_data$cfi_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "cfi"]
    comparison_data$cfi_big_five[i]   <- test_big_five_fit[names(test_big_five_fit) == "cfi"]
    comparison_data$cfi_bifactor[i]   <- test_bifactor_fit[names(test_bifactor_fit) == "cfi"]
    comparison_data$cfi_higher[i]     <- test_higher_fit[names(test_higher_fit) == "cfi"]
    
    comparison_data$tli_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "tli"]
    comparison_data$tli_big_five[i]   <- test_big_five_fit[names(test_big_five_fit) == "tli"]
    comparison_data$tli_bifactor[i]   <- test_bifactor_fit[names(test_bifactor_fit) == "tli"]
    comparison_data$tli_higher[i]     <- test_higher_fit[names(test_higher_fit) == "tli"]
    
    comparison_data$rmsea_network[i]  <- test_net_fit$Value[test_net_fit$Measure == "rmsea"]
    comparison_data$rmsea_big_five[i] <- test_big_five_fit[names(test_big_five_fit) == "rmsea"]
    comparison_data$rmsea_bifactor[i] <- test_bifactor_fit[names(test_bifactor_fit) == "rmsea"]
    comparison_data$rmsea_higher[i]   <- test_higher_fit[names(test_higher_fit) == "rmsea"]
    
    comparison_data$bic_network[i]    <- test_net_fit$Value[test_net_fit$Measure == "bic"]
    comparison_data$bic_big_five[i]   <- test_big_five_fit[names(test_big_five_fit) == "bic"]
    comparison_data$bic_bifactor[i]   <- test_bifactor_fit[names(test_bifactor_fit) == "bic"]
    comparison_data$bic_higher[i]     <- test_higher_fit[names(test_higher_fit) == "bic"]
    
    # Save current iteration
    
    write_rds(comparison_data, 
              file = "output/ipip-neo_model-comparison-data.rds")
      
  
  }

} else {
  
  comparison_data <- write_rds("output/ipip-neo_model-comparison-data.rds")
  
}
  
if (!file.exists("output/ipip-neo_model-comparison-data.csv")) {
  
  # Export simplified output
  
  write_csv(comparison_data %>% 
              select(-omega_list),
            file = "output/ipip-neo_model-comparison-data.csv")
  
}

# Factor model fit statistics from full data -----------------------------------

# This loop fits the factor models to the full data for each country, for later
# use comparing to the fit statistics for networks fit to the full data from
# each country.

if (!file.exists("output/ipip-neo_cfa-fits.rds")) {
  
  cfa_fits <- foreach(i = 1:length(countries), 
                      .packages = packages, 
                      .combine = bind_rows) %dopar% {
  
    # Subset data
   
    test_data <- ipip %>% 
      filter(country == names(countries)[i]) %>% 
      select(starts_with("i")) %>% 
      filter(complete.cases(.))
    
    # Fit five factor models
   
    test_big_five <- cfa(big_five,
                         data = test_data,
                         estimator = "ML")
   
    test_bifactor <- cfa(bifactor,
                         data = test_data,
                         estimator = "ML")
   
    test_higher   <- cfa(higher_order,
                         data = test_data,
                         estimator  = "ML",
                         orthogonal.y = TRUE)
   
    # Set up empty data
    
    fit_data <- data.frame(
      country        = names(countries)[i],
      cfi_big_five   = NA,   
      tli_big_five   = NA,   
      rmsea_big_five = NA, 
      bic_big_five   = NA,
      cfi_bifactor   = NA,   
      tli_bifactor   = NA,   
      rmsea_bifactor = NA, 
      bic_bifactor   = NA,
      cfi_higher     = NA,   
      tli_higher     = NA,   
      rmsea_higher   = NA, 
      bic_higher     = NA
    )
   
    # Calculate fit indices
    
    ## Big Five
    
    if (test_big_five@optim$converged == TRUE) {
      
      test_big_five_fit <- fitmeasures(test_big_five, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_big_five_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    if (test_big_five@optim$converged == TRUE) {
      
      test_big_five_fit <- fitmeasures(test_big_five, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_big_five_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ## Bifactor
    
    if (test_bifactor@optim$converged == TRUE) {
      
      test_bifactor_fit <- fitmeasures(test_bifactor, 
                                       fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_bifactor_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ## Higher Order
    
    if (test_higher@optim$converged == TRUE) {
      
      test_higher_fit <- fitmeasures(test_higher, 
                                     fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      test_higher_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    ## Extract indices
    
    fit_data$cfi_big_five   <- test_big_five_fit[names(test_big_five_fit) == "cfi"]
    fit_data$cfi_bifactor   <- test_bifactor_fit[names(test_bifactor_fit) == "cfi"]
    fit_data$cfi_higher     <- test_higher_fit[names(test_higher_fit) == "cfi"]
    
    fit_data$tli_big_five   <- test_big_five_fit[names(test_big_five_fit) == "tli"]
    fit_data$tli_bifactor   <- test_bifactor_fit[names(test_bifactor_fit) == "tli"]
    fit_data$tli_higher     <- test_higher_fit[names(test_higher_fit) == "tli"]
    
    fit_data$rmsea_big_five <- test_big_five_fit[names(test_big_five_fit) == "rmsea"]
    fit_data$rmsea_bifactor <- test_bifactor_fit[names(test_bifactor_fit) == "rmsea"]
    fit_data$rmsea_higher   <- test_higher_fit[names(test_higher_fit) == "rmsea"]
    
    fit_data$bic_big_five   <- test_big_five_fit[names(test_big_five_fit) == "bic"]
    fit_data$bic_bifactor   <- test_bifactor_fit[names(test_bifactor_fit) == "bic"]
    fit_data$bic_higher     <- test_higher_fit[names(test_higher_fit) == "bic"]
    
    fit_data
   
 }
  
  ## Save estimated confirmatory networks
  
  write_rds(cfa_fits, "output/ipip-neo_cfa-fits.rds")
  
} else {
  
  cfa_fits <- read_rds("output/ipip-neo_cfa-fits.rds")
  
}
