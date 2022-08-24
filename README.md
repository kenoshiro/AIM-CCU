# Global CCU scenario analysis by AIM/Technology

## Introduction

- This repository includes source code for data analysis and figure production for the CCU-scenario analysis by AIM/Technology.

## How to use

- To run this script, scenario data file `scenario_data.csv` needs to be downloaded and copied to `./data/`. The instruction for the data file download can be found in the Data Availability statement in the paper.
- The IPCC AR6 scenario data also needs to be downloaded from [here](https://data.ene.iiasa.ac.at/ar6/) and copied to `./data/AR6/`.
- Execute `./prog/main.R` on the command line or main console on RStudio. The figures are generated in `./output/`.
- Following R packages are required: tidyverse, cowplot, ggalluvial.
