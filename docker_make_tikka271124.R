dput(runif)
set.seed(1001)
runif(10)
install.packages("docopt")
library("docopt")
"usage: runif.R [--n=<int> --min=<float> --max=<float> --seed=<float>]\n\noptions:\n --n=<int>        number of observations. If length(n) > 1, the length is taken to be the number required [default: 1].\n --min=<float>   lower limits of the distribution. Must be 
finite [default: 0].\n --max=<float>   upper limits of the distribution. Must be finite [default: 1].\n --seed=<float>  seed for set.seed() function [default: 1]" -> doc
docopt(doc)
docopt(doc, "--n 10 --min=3 --max=5")

opts <- docopt(doc)
set.seed(opts$seed)
runif(
  n = as.integer(opts$n),
  min = as.numeric(opts$min),
  max = as.numeric(opts$max)
)


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


fl <- system.file("docker/sevenbridges/src", "runif2spin.R", package = "sevenbridges")
cat(readLines(fl), sep = "\n")

library("sevenbridges")

fd <- fileDef(
  name = "runif.R",
  content = readr::read_file(fl)
)

rbx <- Tool(
  id = "runif",
  label = "runif",
  hints = requirements(docker(pull = "rocker/r-base"), cpu(1), mem(2000)),
  requirements = requirements(fd),
  baseCommand = "Rscript runif.R",
  stdout = "output.txt",
  inputs = list(
    input(
      id = "number",
      type = "integer",
      position = 1
    ),
    input(
      id = "min",
      type = "float",
      position = 2
    ),
    input(
      id = "max",
      type = "float",
      position = 3
    )
  ),
  outputs = output(id = "random", glob = "output.txt")
)
# Now copy-paste 