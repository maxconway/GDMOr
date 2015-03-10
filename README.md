<!-- README.md is generated from README.Rmd. Please edit that file -->



metabex
========================
This package is for Cytoscape-based analysis of populations of metabolic models, and their properties.
The key datatype is a `data.frame`, with some columns prefixed with `genotype.` and some with `phenotype.`
Various graphs can be plotted to identify genes or reactions with unusual properties, and, once these properties are found, a suitable sybil model can be displayed in cytoscape and subsets of the metabolic network visualized.

## Installation

metabex is not yet available on CRAN. You can install the development version directly from github with:


```r
if (packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("maxconway/metabex")
```

## Usage

For usage, see `vignette('Introduction', package='metabex')`
