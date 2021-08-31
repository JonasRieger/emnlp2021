starttime = Sys.time()

library(ldaPrototype)
library(tosca)
library(lubridate)

obj = textmeta(meta = readRDS("cleannytsample15.rds")$meta[, c("id", "date", "title")])

interval = c("year", "quarter")
memory = 4:1
start = c(1981, 1985, 2000)
type = c("", "_changing")

source("funs.R")
funs = c("cosine", "jacc", "manhattan", "chi", "hellinger", "js")

for(i in interval){
  for(m in memory){
    for(s in start){
      for(t in type){
        time_folder = Sys.time()
        folder = paste0("rolling_", i, "_", s, "-01-01_memory", m, t)
        n_char = nchar(folder)
        message("\n",
                rep("#", 50), "\n",
                rep("#", (48-n_char)/2), " ", folder, " ", rep("#", round((48-n_char)/2 + 0.1)), "\n",
                rep("#", 50),
                "\n")
        docs = readRDS(file.path(folder, "docsall.rds"))
        dates = obj$meta$date[match(names(docs), obj$meta$id)]
        lda = readRDS(file.path(folder, "ldachunked.rds"))
        assignments = getAssignments(lda)
        vocab = colnames(getTopics(lda))
        K = getK(lda)
        
        unit = c("quarter", "month", "year")
        for(u in unit){
          floored = floor_date(dates, u)
          xunit = sort(unique(floored))
          nunit = length(xunit)
          
          message(u, "ly sims")
          val = lapply(funs, function(x) matrix(data = NA_real_, nrow = nunit, ncol = K))
          names(val) = funs
          val_first = val_last = val
          
          a = Sys.time()
          for(k in 1:K){
            tmp = do.call(cbind, lapply(xunit, function(x){
              tmp = unlist(assignments[floored == x])+1 == k
              tmp = tabulate((unlist(lapply(docs[floored == x], function(y) y[1,]))+1)[tmp],
                             nbins = length(vocab))
              tmp
            }))
            for(fun in funs){
              val[[fun]][,k] = c(NA, sapply(2:nunit, function(i) do.call(fun, list(tmp[,i], tmp[,i-1]))))
              val_first[[fun]][,k] = c(NA, sapply(2:nunit, function(i) do.call(fun, list(tmp[,i], tmp[,1]))))
              val_last[[fun]][,k] = c(sapply(1:(nunit-1), function(i) do.call(fun, list(tmp[,i], tmp[,nunit]))), NA)
            }
          }
          b = Sys.time()
          saveRDS(val, file.path(folder, paste0("sim_", u, ".rds")))
          saveRDS(val_first, file.path(folder, paste0("sim_", u, "_first.rds")))
          saveRDS(val_last, file.path(folder, paste0("sim_", u, "_last.rds")))
          message(difftime(b, a, units = "hours"), " hours")
          gc()
        }
        message("\n", folder, ": ", difftime(Sys.time(), time_folder, units = "hours"), " hours")
      }
    }
  }
}

message("\nOverall : ", difftime(Sys.time(), starttime, units = "hours"), " hours")
