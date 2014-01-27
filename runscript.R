# Sybil runscript
#  uses sybil to load a model, run GDMO on it, and put it back into a data frame

# Load libraries
library(sybil)
library(sybilSBML)

# Load data
model <- readSBMLmod('testdata/toy.xml')
