## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## ----install------------------------------------------------------------------
#  ## Install from CRAN
#  install.packages("LCMSQA")
#  
#  ## Load LCMSQA pacakge
#  library(LCMSQA)

## ----unix parallel------------------------------------------------------------
#  ## Unix-based systems
#  library(BiocParallel)
#  register(bpstart(MulticoreParam()))

## ----windows parallel---------------------------------------------------------
#  ## Windows system
#  library(BiocParallel)
#  register(bpstart(SnowParam()))

## ----shiny app----------------------------------------------------------------
#  runQA()

