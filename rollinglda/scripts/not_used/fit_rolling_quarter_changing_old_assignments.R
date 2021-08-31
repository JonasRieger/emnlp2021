starttime.all = Sys.time()

library(tosca)
library(ldaPrototype)
library(lda)
library(lubridate)

obj = readRDS("cleannytsample15.rds")

K = 80
alpha = eta = 1/K
num.iterations = 200
vocab.limit = 5 # consider sampling -> relative threshold
vocab.rel = 1e-5 # 1 per 100 000
vocab.max = 100 # words that appear more often than vocab.max are included either way

interval = "quarter"

for(memory in 4:1){
  for(startdate in c("1981-01-01", "1985-01-01", "2000-01-01")){
    starttime = Sys.time()
    start = as.Date(startdate)
    docs = docs_all = readRDS(file.path("init", startdate, "docs0.rds"))
    vocab = readRDS(file.path("init", startdate, "vocab0.rds"))
    init = readRDS(file.path("init", startdate, "proto_init.rds"))
    
    wd = paste0("rolling_", interval, "_", start, "_", "memory", memory, "_changing")
    message("CREATE ", wd, " DIRECTORY...")
    dir.create(wd)
    setwd(wd)
    
    end = as.Date("2021-01-01") # = (last day observed + 1)
    
    cuts = seq.Date(start, end, interval)
    cache = head(seq.Date(start - months(memory*3), end - months(memory*3), interval), -1)
    
    lda = getLDA(init)
    assignments = getAssignments(lda)
    saveRDS(vocab, file = "vocab0.rds")
    saveRDS(docs, file = "docs0.rds")
    saveRDS(init, file = "proto_init.rds")
    
    message("FIT SEQUENTIAL LDAS...")
    a = Sys.time()
    
    for(i in seq_len(length(cuts)-1)){
      b = Sys.time()
      message("### ", i, "/", length(cuts)-1, ", eta: ",
              (difftime(b, a, unit = "hours")/(i-1)) * (length(cuts)-i)," hours ###")
      chunk = filterDate(obj, s.date = cuts[i], e.date = cuts[i+1]-1)
      wl = makeWordlist(chunk$text)
      tmp = wl$words[wl$wordtable > vocab.limit & wl$wordtable > min(vocab.rel*sum(wl$wordtable), vocab.max)]
      ind = !(tmp %in% vocab)
      if(any(ind)) vocab = c(vocab, tmp[ind])
      docs_neu = LDAprep(chunk$text, vocab)
      
      n.docs = length(docs_all)
      docs_all[(n.docs+1):(n.docs+length(docs_neu))] = docs_neu
      names(docs_all)[(n.docs+1):length(docs_all)] = names(docs_neu)
      
      assignments[(n.docs+1):length(docs_all)] = lapply(
        sapply(docs_all[(n.docs+1):length(docs_all)], ncol),
        function(n) as.integer(sample(K, n, replace = TRUE)-1))
      
      ind_chunk = names(docs_all) %in% obj$meta$id[obj$meta$date >= cache[i] & obj$meta$date < cuts[i+1]]
      n.init = sum(names(docs_all) %in% obj$meta$id[obj$meta$date >= cache[i] & obj$meta$date < cuts[i]])
      docs = docs_all[ind_chunk]
      
      topics = matrix(as.integer(
        table(unlist(assignments[ind_chunk]) + 1, #Topics
              factor(unlist(lapply(docs, function(x) x[1,])) + 1, seq_len(length(vocab)), vocab)) #Vocab
      ), nrow = K)
      colnames(topics) = vocab
      
      initial = list(
        assignments = assignments[ind_chunk],
        topics = topics,
        topic_sums = matrix(as.integer(rowSums(topics))))
      
      #res = ldaGibbs(docs, K, vocab, num.iterations, alpha, eta, initial = initial, n.init = n.init)
      res = lda.collapsed.gibbs.sampler(docs, K, vocab, num.iterations, alpha, eta, initial = initial)
      
      assignments[(n.docs+1):length(docs_all)] = res$assignments[(n.init+1):length(docs)]
      document_sums = cbind(getDocument_sums(lda), res$document_sums[,(n.init+1):length(docs)])
      
      lda = LDA(assignments = assignments, document_sums = document_sums,
                param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations))
      saveRDS(vocab, file = paste0("vocab", i, ".rds"))
      saveRDS(docs, file = paste0("docs", i, ".rds"))
      saveRDS(docs_all, file = paste0("docsall.rds"))
      saveRDS(lda, file = paste0("ldachunked.rds"))
    }
    rm(docs, init, chunk, res)
    gc(TRUE)
    topics = matrix(as.integer(
      table(unlist(assignments) + 1, #Topics
            factor(unlist(lapply(docs_all, function(x) x[1,])) + 1, seq_len(length(vocab)), vocab)) #Vocab
    ), nrow = K)
    colnames(topics) = vocab
    lda = LDA(assignments = assignments, topics = topics, document_sums = document_sums,
              param = list(K = K, alpha = alpha, eta = eta, num.iterations = num.iterations))
    saveRDS(lda, file = paste0("ldachunked.rds"))
    b = Sys.time()
    
    message(difftime(b, a, units = "hours"), " hours")
    message("Overall time ", wd, ": ", difftime(Sys.time(), starttime, units = "hours"), " hours")
    
    gc(TRUE)
    setwd("..")
  }
}

message("\n\nOverall time: ", difftime(Sys.time(), starttime.all, units = "hours"), " hours")
