library(tosca)
library(ldaPrototype)
library(lubridate)
library(batchtools)

obj = readRDS("cleannytsample15.rds")

K = 80
alpha = eta = 1/K
num.iterations = 200
vocab.limit = 5 # consider sampling -> relative threshold
vocab.rel = 1e-5 # 1 per 100 000
vocab.max = 100 # words that appear more often than vocab.max are included either way

#start = as.Date("1981-01-01") # init 1980
start = as.Date("1985-01-01") # init 1980-1984
#start = as.Date("2000-01-01") # init 1980-1999

dir.create("init")
setwd("init")
dir.create(as.character(start))
setwd(as.character(start))

obj = filterDate(obj, e.date = start-1)
wl = makeWordlist(obj$text)
vocab = wl$words[wl$wordtable > vocab.limit & wl$wordtable > min(vocab.rel*sum(wl$wordtable), vocab.max)]

docs = LDAprep(obj$text, vocab)
gc(TRUE)

a = Sys.time()
message("FIT INITIAL PROTOTYPE...")
batch = LDABatch(docs, vocab, K = K, alpha = alpha, eta = eta, num.iterations = num.iterations,
                 resources = list(walltime = 2*60*60, memory = 4096))
saveRDS(batch, "batch.rds")
message("WAIT FOR JOBS...")
waitForJobs(sleep = 600)
b = Sys.time()
message("LDAs: ", difftime(b, a, unit = "hours"), " hours")
gc(TRUE)

a = Sys.time()
init = getPrototype(batch, pm.backend = "socket", ncpus = 4)
b = Sys.time()
message("Prototype: ", difftime(b, a, unit = "hours"), " hours")
gc(TRUE)

saveRDS(vocab, file = "vocab0.rds")
saveRDS(docs, file = "docs0.rds")
saveRDS(init, file = "proto_init.rds")
