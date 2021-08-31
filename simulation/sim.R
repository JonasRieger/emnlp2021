library(data.table)
library(ggplot2)
library(ggpubr)
library(GGally)

set.seed(1895)

kld = function(q1, q2){
  sum(q1 * log(q1/q2))
}

js = function(a, b, epsilon = 1e-06){
  a = a + epsilon
  b = b + epsilon
  p1 = a/sum(a)
  p2 = b/sum(b)
  1 - kld(p1, p1+p2)/2 - kld(p2, p1+p2)/2 - log(2)
}

jacc = function(a, b, limit.rel = 0.002){
  i1 = a > sum(a) * limit.rel
  i2 = b > sum(b) * limit.rel
  sum(i1 * i2) / sum(i1 + i2 > 0)
}

cosine = function(a, b) sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))
hellinger = function(a, b) 1 - as.numeric(dist(rbind(sqrt(a/sum(a)), sqrt(b/sum(b))))/sqrt(2))
manhattan = function(a, b) 1 - as.numeric(dist(rbind(a/sum(a), b/sum(b)), method = "manhattan")/2)
chi = function(a, b, epsilon = 1e-06){
  a = a + epsilon
  b = b + epsilon
  a = a/sum(a)
  b = b/sum(b)
  1 - sum((a-b)^2/(a+b))/2
}

funs = c("cosine", "jacc", "manhattan", "chi", "hellinger", "js")
label = c("Cosine", "Jaccard (0.002)", "Manhattan", "Chi-Squared", "Hellinger", "Jensen-Shannon")
label2 = c("cos", "TJ", "MH", "Chi-Sq", "HL", "JS")

v = 10000

a = 1/(1:v * log(1.78*v)) * 7500
a = sort(as.integer(a) + rbinom(a, 1, a - as.integer(a)), decreasing = TRUE)

l = list()
for(fun in funs){
  z = sapply(1:max(a), function(x){
    b = a
    b[v] = x
    do.call(fun, list(a, b))
  })
  val = z
  
  for(i in 1:19){
    z = sapply(1:max(a), function(x){
      b = a
      b[v:(v-i)] = x
      do.call(fun, list(a, b))
    })
    val = c(val, z)
  }
  l[[fun]] = data.table(x = rep(1:max(a), 20), sim = val, k = rep(1:20, each = max(a)), measure = fun)
}
low = rbindlist(l)


l = list()
for(fun in funs){
  z = sapply(c(seq(0.003,1,0.001), seq(1,60,0.1)), function(x){
    b = a
    b[1] = b[1]*x
    do.call(fun, list(a, b))
  })
  val = z
  
  for(i in 2:10){
    z = sapply(c(seq(0.003,1,0.001), seq(1,60,0.1)), function(x){
      b = a
      b[1:i] = b[1:i]*x
      do.call(fun, list(a, b))
    })
    val = c(val, z)
  }
  l[[fun]] = data.table(x = rep(c(seq(0.003,1,0.001), seq(1,60,0.1)), 10),
                        sim = val, k = rep(1:10, each = length(c(seq(0.003,1,0.001), seq(1,60,0.1)))),
                        measure = fun)
}
topfirstfollowing = rbindlist(l)


l = list()
for(fun in funs){
  z = sapply(c(seq(0.003,1,0.001), seq(1,60,0.1)), function(x){
    b = a
    b[1] = b[1]*x
    do.call(fun, list(a, b))
  })
  val = z
  
  for(i in 2:10){
    z = sapply(c(seq(0.003,1,0.001), seq(1,60,0.1)), function(x){
      b = a
      b[i] = b[i]*x
      do.call(fun, list(a, b))
    })
    val = c(val, z)
  }
  l[[fun]] = data.table(x = rep(c(seq(0.003,1,0.001), seq(1,60,0.1)), 10),
                        sim = val, k = rep(1:10, each = length(c(seq(0.003,1,0.001), seq(1,60,0.1)))),
                        measure = fun)
}
topsingle = rbindlist(l)

plots1 = ggmatrix(lapply(funs, function(fun){
  ggplot(low[measure == fun]) +
    aes(x = x, y = sim, color = k, group = k) +
    geom_line() + ylim(c(0,1)) + labs(color = "Changed Words\nin Scenario a)") +
    scale_x_continuous(breaks = c(0,250,500,750)) +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.45, 'cm'))
}), ncol = length(funs), nrow = 1,
xlab = "Frequency", ylab = "Similarity", legend = 1,
xAxisLabels = label)

plots2 = ggmatrix(lapply(funs, function(fun){
  ggplot(topfirstfollowing[measure == fun]) +
    aes(x = log(x), y = sim, color = k, group = k) +
    geom_line() + ylim(c(0,1)) + labs(color = "Changed Words\nin Scenario b)") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.45, 'cm'))
}), ncol = length(funs), nrow = 1,
xlab = "Multiplication (log scaled)", ylab = "Similarity", legend = 1)

plots3 = ggmatrix(lapply(funs, function(fun){
  ggplot(topsingle[measure == fun]) +
    aes(x = log(x), y = sim, color = k, group = k) +
    geom_line() + ylim(c(0,1)) + labs(color = "Changed Word  \nin Scenario c)") +
    theme(legend.title = element_text(size = 10),
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.45, 'cm'))
}), ncol = length(funs), nrow = 1,
xlab = "Multiplication (log scaled)", ylab = "Similarity", legend = 1)

pdf("sim.pdf", 10, 7)
ggarrange(plotlist = list(ggmatrix_gtable(plots1),
                          ggmatrix_gtable(plots2),
                          ggmatrix_gtable(plots3)), nrow = 3,
          heights = c(1.18,1,1))
# 850x480
# 850x400

funs = factor(funs, levels = funs)
nrep = 500
unc = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    do.call(as.character(fun), list(a,b))
  })
})
unc = data.table(sim = unlist(unc), measure = rep(funs, each = nrep), type = "d) unc.")
sam = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = sample(a)
    do.call(as.character(fun), list(a,b))
  })
})
sam = data.table(sim = unlist(sam), measure = rep(funs, each = nrep), type = "e) all")
shuff10 = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = a
    b[1:10] = sample(b[1:10])
    do.call(as.character(fun), list(a,b))
  })
})
shuff10 = data.table(sim = unlist(shuff10), measure = rep(funs, each = nrep), type = "f) 1-10")
shuff20 = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = a
    b[11:20] = sample(b[11:20])
    do.call(as.character(fun), list(a,b))
  })
})
shuff20 = data.table(sim = unlist(shuff20), measure = rep(funs, each = nrep), type = "i) 11-20")
shuff30 = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = a
    b[21:50] = sample(b[21:50])
    do.call(as.character(fun), list(a,b))
  })
})
shuff30 = data.table(sim = unlist(shuff30), measure = rep(funs, each = nrep), type = "j) 21-50")
shuff50 = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = a
    b[1:50] = sample(b[1:50])
    do.call(as.character(fun), list(a,b))
  })
})
shuff50 = data.table(sim = unlist(shuff50), measure = rep(funs, each = nrep), type = "g) 1-50")
shuff100 = lapply(funs, function(fun){
  replicate(nrep,{
    a = tabulate(sample(1:v, 7500, replace = TRUE, prob = 1/(1:v * log(1.78*v))), nbins = v)
    b = a
    b[1:100] = sample(b[1:100])
    do.call(as.character(fun), list(a,b))
  })
})
shuff100 = data.table(sim = unlist(shuff100), measure = rep(funs, each = nrep), type = "h) 1-100")
both = rbind(unc, sam, shuff10, shuff20, shuff30, shuff50, shuff100)

pdf("sim_points_overlay.pdf", height = 2.5, width = 4)
ggplot(both[sample(.N)]) + aes(x = measure, y = sim, col = type) +
  geom_jitter(alpha = 0.2) + labs(color = "") +
  xlab("Similarity Measure") + ylab("Similarity") +
  scale_x_discrete(breaks = funs, labels = label2) +
  theme(legend.position = "top") +
  scale_color_discrete(breaks = c("unc.", "all", "1-10", "11-20", "1-50", "21-50", "1-100")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
dev.off()

both[, jitter.mid := c(-0.33, -0.22, -0.11, 0, 0.11, 0.22, 0.33)[match(type, c("d) unc.", "i) 11-20", "j) 21-50", "f) 1-10", "g) 1-50", "h) 1-100", "e) all"))]]
both[, x := match(measure, funs) + jitter.mid]

pdf("sim_points_2.pdf", height = 2.5, width = 4)
ggplot(both) + aes(x = x, y = sim, col = type) +
  geom_jitter(alpha = 0.2) + labs(color = "") +
  xlab("Similarity Measure") + ylab("Similarity") +
  scale_x_continuous(breaks = 1:6, labels = label2) +
  theme(legend.position = "top") +
  scale_color_discrete(breaks = c("d) unc.", "e) all", "f) 1-10", "i) 11-20", "g) 1-50", "j) 21-50", "h) 1-100")) +
  guides(colour = guide_legend(override.aes = list(alpha = 1)))
dev.off()

# comparison to zero vector
tab = sapply(funs, function(fun){
  c(
    do.call(as.character(fun), list(a, c(0, rep(0, 9999)))),
    do.call(as.character(fun), list(a, c(1, rep(0, 9999)))),
    do.call(as.character(fun), list(a, c(rep(0, 9999), 1)))
  )
})
row.names(tab) = c("zero", "first 1", "last 1")
tab = as.data.table(melt(tab))
tab[, sim := value]
tab[, measure := rep(as.character(funs), each = 3)]
tab[, type := as.character(Var1)]
#tab[, type := factor(as.character(Var1), levels = c("unc.", "shuffle", "zero", "first 1", "last 1"))]
tab[, Var1 := NULL]
tab[, Var2 := NULL]
tab[, value := NULL]
#both[, type := factor(type, levels = c("unc.", "shuffle", "zero", "first 1", "last 1"))]

set.seed(101)
#pdf("sim_unc.pdf", height = 2.5, width = 4)
ggplot(both[sample(.N)]) + aes(x = measure, y = sim, col = type) +
  geom_jitter(alpha = 0.2, height = 0) + labs(color = "") +
  xlab("Similarity Measure") + ylab("Similarity") +
  scale_x_discrete(breaks = funs, labels = label2) +
  geom_jitter(data = tab, width = 0.05, height = 0) +
  theme(legend.position = "top") +
  scale_color_discrete(breaks = c("unc.", "shuffle", "shuffle1:50", "shuffle51:100", "shuffle1:100", "zero", "first 1", "last 1"))
#dev.off()
dev.off()

