# penPass

An R package for hosting code and data, and running the Atlantic salmon PenPass model.

## Installation

The development version of `penPass` can be installed with  [`devtools`](https://www.rstudio.com/products/rpackages/devtools/) in R using the repository url:

`devtools::install_github("danStich/penPass")`

To install `penPass`, you will need to have `devtools` installed ahead of time in R, but that requires some special tools. To install on **Windows**, you will need to download and install the appropriate version of [Rtools](https://cran.r-project.org/bin/windows/Rtools/). To install on **Mac**, you will need to have the [XCode command-line tools](http://osxdaily.com/2014/02/12/install-command-line-tools-mac-os-x/) installed. And, if running from **Linux**, you will need to install the developer version of R (`r-base-dev`) if you have not already.

## Directories

`data/` Contains built-in data sets for the package

`man/`  help files and documentation

`R/`    R functions in scripts

`inst/shiny-examples/penPass_shiny/` includes server and ui files for `penPass_shiny()`, a shiny-based user interface

`tests/` Includes package tests for default parameter accuracy conducted on package build

## Getting started
There are two primary user-facing functions within the `penPass` package. The `run_one_year()` function can be called within an Rscript to run the penPass model using default or user-defined inputs for a single year. The `penPass_shiny()`function launches a graphical user interface (GUI) that opens in a web browser and is written with the `shiny` package. The former provides functionality for large-scale simulation studies in R whereas the latter provides and intuitive interface for running smaller numbers of management scenarios.

### `run_one_year()`
The `run_one_year()` function provides the primary user-facing function for the `penPass`package. The purpose of the function is to run the PenPass analysis for a selected year (1970-2020). The output of this function is a numerical vector containing number of smolts reaching the ocean. All default argument values (model inputs) are based on the most-recent version of the NOAA PenPass Excel-based model (Stevens et al. 2019).

The `run_one_year()` function allows user to specify year, downstream passage survival through dams, natural survival per kilometer, habitat productivity, habitat saturation, and probability that fish migrate through the Stillwater Branch during outmigration.

You can find the help file for the `run_one_year()` function in R by running:`?penPass::run_one_year`. This file contains all accepted user arguments, explanations of what they mean and what are default values, and examples of how to use the `run_one_year()` function. Examples are shown at the bottom of this page.

### `penPass_shiny()`
The `penPass_shiny()` function provides and intuitive user interface for running the penPass model. Though more limited in flexibility that `run_one_year()`, the simplified interface should allow full exploration of management scenarios. The primary difference is that `penPass_shiny()` does not allow the user to input custom distributions or values for some input parameters implemented in the `run_one_year()` function. 

Currently, the shiny interface for the penPass model can only be called directly from R or Rstudio by running the following lines of code:
```
library(penPass)
penPass_shiny()
```

This will open the user-interface in a local instance of the default web browser on the computer where further instructions are provided.

## Examples using `run_one_year()`

### Single simulation
An example of a single simulation using `run_one_year()` is demonstrated below, using the default values based on Stevens et al. (2019).

```
library(penPass)
run_one_year(
  year = 2017,
  downstream = list(
    seeboomook = NA, 
    ripogenus = NA, 
    north_twin = NA, 
    quakish = NA,
    dolby = NA, 
    east_millinocket = NA, 
    medway = NA, 
    matagamon = 1, 
    guilford = NA,
    moosehead = NA, 
    browns_mills = NA, 
    howland = NA, 
    mattaseunk = NA, 
    west_enfield = NA,
    milford = NA, 
    great_works = NA, 
    gilman_falls = NA, 
    stillwater = NA, 
    orono = NA,
    veazie = NA, 
    bangor_waterworks = 1),
  km_surv = NULL,
  prod = c(
    WPN_prod = 3, 
    EPN_prod = 3, 
    Matt_prod = 3, 
    PISC_prod = 3, 
    PN_prod = 3),
  sat = c(
    WPN_sat = 0, 
    EPN_sat = 0, 
    Matt_sat = 0, 
    PISC_sat = 0, 
    PN_sat = 0),
  p_stillwater = NA
)
```

### Multiple simulations for a single year
An example of a single simulation using `run_one_year()` is demonstrated below, using the default values from Stevens et al. (2019) and the year 2017.

```
library(foreach)
library(tidyverse)
library(penPass)

# Choose number of iterations
n <- 100

# Create a list holding model output
outlist <- foreach(1:n) %do%
  run_one_year(
    year = 2017
  )
  
# Collect results into a single dataframe
names(outlist) <- paste0("run_", seq(1, length(outlist)))
out_df <- data.frame(do.call(rbind, outlist))
names(out_df)[1] = "Smolts"

# Summarize results across number of runs
summary_2017 <- out_df %>% 
  summarize(abund = median(Smolts),
            lwr = quantile(Smolts, 0.025),
            upr = quantile(Smolts, 0.975)
            )

# Plot results
ggplot(out_df, aes(x = Smolts)) +
  geom_histogram()

```

### Multiple simulations for multiple years
```
# Library load
library(penPass)
library(doParallel)
library(foreach)
library(tidyverse)

# Number of simulations
n_sims <- 5000

# Define a function to call
sim <- function(x){
  years <- seq(1970, 2017, 1)
  sim_year <- sample(years[1:length(years)], 1, replace = TRUE)
  out <- run_one_year(year = sim_year)
  data.frame(year = sim_year, smolts_out = out)
}

# Repeat the function for number of iterations (runs the simulation)
registerDoParallel(cores = 7)
out_list <- foreach(1:n_sims, .packages = "penPass") %dopar% sim()

# Compile result year and result
result <- do.call(rbind, out_list)

# Summarize results
plotter <- result %>% 
  group_by(year) %>% 
  summarize(avg = median(smolts_out),
            lwr = quantile(smolts_out, 0.05),
            upr = quantile(smolts_out, 0.95)
            )

# Summarize stocking data
stocker <- penPass::stocking_data %>% 
  filter(Year %in% plotter$year) %>% 
  group_by(Year) %>% 
  summarize(stocked = sum(n_stocked))

# Plot the result
ggplot(plotter, aes(year, avg)) +
  geom_point(pch = 15, size = 2) +
  geom_segment(aes(xend = year, y = lwr, yend = upr)) +
  geom_point(data = stocker, aes(x = Year, y = stocked), size = 2) +
  xlab("Year") +
  ylab("Number of smolts (thousands)") +
  scale_y_continuous(
    breaks = seq(0, 1e6, 2e5),
    labels = seq(0, 1000, 200),
    limits = c(0, 1e6)
  ) +
  theme_classic() +
  theme(
    axis.title.x = element_text(vjust = -1),
    axis.title.y = element_text(vjust = 3),
    axis.text = element_text(color = "black")
  )
```  