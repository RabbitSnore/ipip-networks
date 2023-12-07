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

# Sort country sample sizes

sample_cutoff <- 1500

countries <- sort(table(ipip$country), decreasing = TRUE)
countries <- countries[countries > sample_cutoff]

## Five factor acquiescence model

acquiescence <- 
'

o =~ 1*i3 + 1*i8  + 1*i13 + 1*i18 + 1*i23 + 1*i28 + 1*i33 + 1*i38 + 1*i43 + 1*i48 + 1*i53 + 1*i58 + 1*i63 + 1*i68 + 1*i73 + 1*i78 + 1*i83 + 1*i88 + 1*i93 + 1*i98  + 1*i103 + 1*i108 + 1*i113 + 1*i118
c =~ 1*i5 + 1*i10 + 1*i15 + 1*i20 + 1*i25 + 1*i30 + 1*i35 + 1*i40 + 1*i45 + 1*i50 + 1*i55 + 1*i60 + 1*i65 + 1*i70 + 1*i75 + 1*i80 + 1*i85 + 1*i90 + 1*i95 + 1*i100 + 1*i105 + 1*i110 + 1*i115 + 1*i120
e =~ 1*i2 + 1*i7  + 1*i12 + 1*i17 + 1*i22 + 1*i27 + 1*i32 + 1*i37 + 1*i42 + 1*i47 + 1*i52 + 1*i57 + 1*i62 + 1*i67 + 1*i72 + 1*i77 + 1*i82 + 1*i87 + 1*i92 + 1*i97  + 1*i102 + 1*i107 + 1*i112 + 1*i117
a =~ 1*i4 + 1*i9  + 1*i14 + 1*i19 + 1*i24 + 1*i29 + 1*i34 + 1*i39 + 1*i44 + 1*i49 + 1*i54 + 1*i59 + 1*i64 + 1*i69 + 1*i74 + 1*i79 + 1*i84 + 1*i89 + 1*i94 + 1*i99  + 1*i104 + 1*i109 + 1*i114 + 1*i119
n =~ 1*i1 + 1*i6  + 1*i11 + 1*i16 + 1*i21 + 1*i26 + 1*i31 + 1*i36 + 1*i41 + 1*i46 + 1*i51 + 1*i56 + 1*i61 + 1*i66 + 1*i71 + 1*i76 + 1*i81 + 1*i86 + 1*i91 + 1*i96  + 1*i101 + 1*i106 + 1*i111 + 1*i116

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

# Parallel computing set up

# IMPORTANT! Modify this for your system. Do not assume this default will work
# if you are reproducing the analyses. Running this code and not running a
# parallelized process will not be harmful, but you could have a suboptimal
# experience using this code without modifications tailored for your computing
# environment.

cores <- detectCores()

registerDoParallel(cl = (cores - 2)/2, cores = cores - 2)

# Modeling ---------------------------------------------------------------------

if (!file.exists("output/ipip-neo_acquiescence-model-fit.rds")) {
  
  acq_data <- foreach(i = 1:length(countries), 
                          .packages = packages,
                          .combine = bind_rows) %dopar% {
                            
    country_current <- names(countries[i])
    
    # Load test data
    
    test_data     <- read_csv(paste("data/test/ipip-neo_test_", 
                                    str_replace_all(tolower(country_current), " ", "_"), 
                                    ".csv", 
                                    sep = ""))
    
    # Fit test model
    
    test_acquiescence <- cfa(acquiescence,
                             data = test_data,
                             estimator = "ML")
    
    ## Calculate fit indices
    
    ### Acquiescence
    
    if (test_acquiescence@optim$converged == TRUE) {
      
      acq_fit <- fitmeasures(test_acquiescence, 
                              fit.measures = c("cfi", "tli", "rmsea", "bic"))
      
    } else {
      
      acq_fit <- c("cfi" = NA, "tli" = NA, "rmsea" = NA, "bic" = NA)
      
    }
    
    # Record fit indices
    
    data.frame(
      country       = country_current,
      cfi_acquiescence    = acq_fit[names(acq_fit) == "cfi"],   
      tli_acquiescence    = acq_fit[names(acq_fit) == "tli"],   
      rmsea_acquiescence  = acq_fit[names(acq_fit) == "rmsea"], 
      bic_acquiescence    = acq_fit[names(acq_fit) == "bic"]
    )
    
  }
  
  write_rds(acq_data, 
            "output/ipip-neo_acquiescence-model-fit.rds")
  
  write_csv(acq_data, 
            "output/ipip-neo_acquiescence-model-fit.csv")
  
} else {
  
  acq_data <- read_rds("output/ipip-neo_acquiescence-model-fit.rds")
  
}

