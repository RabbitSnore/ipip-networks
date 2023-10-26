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