#!/usr/bin/Rscript
"usage: runif.R [--n=<int> --min=<float> --max=<float> --seed=<float>]\n\noptions:\n --n=<int>        number of observations. If length(n) > 1, the length is taken to be the number required [default: 1].\n --min=<float>   lower limits of the distribution. Must be finite [default: 0].\n --max=<float>   upper limits of the distribution. Must be finite [default: 1].\n --seed=<float>  seed for set.seed() function [default: 1]" -> doc

library("docopt")
opts <- docopt(doc)
set.seed(opts$seed)
runif(
  n = as.integer(opts$n),
  min = as.numeric(opts$min),
  max = as.numeric(opts$max)
)