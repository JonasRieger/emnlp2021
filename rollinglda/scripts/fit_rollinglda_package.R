# We did not use our package rollinglda for modeling because it was not 
# completely implemented.
# The code below is an example of how to model a RollingLDA using the package.

library(tosca)
library(rollinglda)

obj = readRDS("cleannytsample15.rds")

roll_lda = RollingLDA(texts = obj$text,
                      dates = obj$meta$dates[match(obj$meta$id, names(obj$text))],
                      chunks = "quarter",
                      memory = "3 quarter",
                      init = as.Date("1985-01-01")-1,
                      K = 80)
