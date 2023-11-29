# Network Analysis of Open IPIP-NEO-120 Data

This repository contains the code for the statistical analyses for a project
examining Johnson's IPIP-NEO data. The data are available on the Open Science
Framework:

Johnson, J. A. (2020, October 13). Johnson’s IPIP-NEO data repository. Retrieved
from https://osf.io/tbmh5

The contents of this repository are licensed under the GPL-3.

A preprint for this project is available here:
https://osf.io/preprints/psyarxiv/vfjcx/

## Repository Structure

- `R` contains the code for the quantitative analyses and visualizations
- `output` contains data produced by the analyses, including network models and fit statistics
- `data` will be created by a script that downloads, cleans, and wrangles the data
- `figures` contains the stored data visualizations, with the exception of those that are too large for GitHub

## Reproducing the Analyses

The most efficient way of reproducing the results is to run
`source("R/ipip-neo-net_build-project.R")`, which will run all the code
necessary to download, clean, and wrangle the data and to fit all the models (or
load the stored model output). If you want to reproduce the results completely
from scratch, rather than loading the stored output (which is the default), you
should delete the contents of the `output` folder before running the build
script.

Look, I'm going to level with you. This one is a doozy. The quantity of data,
the number of items, and the number of models that need to be fit all make this
project extremely computationally intensive. On an ordinary personal computer
(in the year 2023, when we did these analyses), you should expect computation
times measured in days for some of these processes. If you have access to a
high-performance computing environment, I strongly recommend using it if you
want to efficiently reproduce the analyses. The existing scripts include code to
use parallelized processes, but the code is written with defaults that may not
work well for your specific computing environment (it's a very blunt instrument
by default). Be sure to edit the parallelization code to suit your specific
situation.

Everything you need to reproduce the analyses is here, but doing so is
expensive. If you just want a closer look at what we have already done, I
recommend running the build script, which will load all of the completed work
and let you examine it more closely. I recommend reproducing the analyses from
scratch only if you have a good reason to spend the computational resources
(e.g., error-checking, reuse and extension of the work).