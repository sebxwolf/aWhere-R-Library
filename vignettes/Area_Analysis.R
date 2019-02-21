## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      eval=TRUE,
                      message = FALSE,
                      warning = FALSE,
                      results='hide')

## ----load_packages-------------------------------------------------------
# load required packages
library(aWhereAPI)
library(dplyr)

# load credentials to access aWhere API
aWhereAPI::load_credentials("credentials_trial.txt")

## ----detectCores, results=TRUE-------------------------------------------
parallel::detectCores()

