# Output Data Files

The files in this directory are produced by the R code in this repository. If
reproducing the analyses from scratch, it is recommended that you delete your
copies of these output files, since much of the code is set up to skip processes
if the output files already exist. It is set up this way to avoid running very
long processes when they are not necessary.

You will notice that the cross country data and the model comparison data have
both RDS and CSV files. The RDS files contain a list column with the omega
matrix from the network models (i.e., the network structure), and the CSV files
omit this column. If you just want to glance through things, the CSV files are
more convenient. But the RDS files are more complete.

# Manifest

```
"ffm-esem"                                 Folder containing ESEM output and syntax
"ipip-neo_cfa-fits.rds"                    Fit statistics for factor models fit to full country data (unused)  
"ipip-neo_confirmatory-networks.rds"       Network models fit to full country data
"ipip-neo_country-descriptives.csv"        Descriptives for the included sample
"ipip-neo_cross-country-test-data.rds"     Fit statistics for cross-country network models   
"ipip-neo_ffm-esem-model-fit.csv"          Fit statistics for ESEMs
"ipip-neo_ffm-esem-model-fit.rds"          Fit statistics for ESEMs
"ipip-neo_matrix-bic-test.csv"             Matrix of cross-country models' BICs
"ipip-neo_matrix-cfi-test-rounded.csv"     Matrix of cross-country models' CFIs, rounded for readability
"ipip-neo_matrix-cfi-test.csv"             Matrix of cross-country models' CFIs
"ipip-neo_matrix-rmsea-test-rounded.csv"   Matrix of cross-country models' RMSEAs, rounded for readability
"ipip-neo_matrix-rmsea-test.csv"           Matrix of cross-country models' RMSEAs
"ipip-neo_matrix-tli-test-rounded.csv"     Matrix of cross-country models' TLIs, rounded for readability
"ipip-neo_matrix-tli-test.csv"             Matrix of cross-country models' TLIs
"ipip-neo_model-comparison-data.csv"       Fit statistics for factor and network models in test data
"ipip-neo_model-comparison-data.rds"       Fit statistics for factor and network models in test data, with adjacency matrices 
"ipip-neo_origin-model-performance.csv"    Model comparison statistics of country's own network vs other countries'
"ipip-neo_similarity-network-weighted.csv" Matrix of sum of all edge weights across country-specific models  
"ipip-neo_similarity-network.csv"          Matrix of frequency of edges across country-specific models     
"ipip-neo_uc-esem-model-fit.csv"           Fit statistics for ESEMs (parallel analysis)
"ipip-neo_uc-esem-model-fit.rds"           Fit statistics for ESEMs (parallel analysis)
```