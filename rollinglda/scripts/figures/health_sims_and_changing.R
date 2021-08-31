library(tosca)
library(ldaPrototype)
library(lubridate)
library(ggplot2)
library(GGally)
library(ggthemes)
library(data.table)
library(xtable)

obj = readRDS("cleannytsample15.rds")

xmonth = sort(unique(floor_date(obj$meta$date, unit = "month")))
xquarter = sort(unique(floor_date(obj$meta$date, unit = "quarter")))
xyear = sort(unique(floor_date(obj$meta$date, unit = "year")))

docs = readRDS("rolling_quarter_1985-01-01_memory3/docsall.rds")
lda = readRDS("rolling_quarter_1985-01-01_memory3/ldachunked.rds")
topWords(getTopics(lda))

funs = c("cosine", "jacc", "manhattan", "chi", "hellinger", "js")
label = c("Cosine", "Jaccard (0.002)", "Manhattan", "Chi-Squared", "Hellinger", "Jensen-Shannon")

# Comparison Similarity Measures
type = "rolling_quarter_1985-01-01_memory3"

tmp = CJ(c("year", "quarter", "month"), c("", "_first", "_last"))
tmp[, name := paste0("sim_", V1, V2, ".rds")]
setorderv(tmp, "V1", -1)

l_health = lapply(tmp$name, function(x){
  tmp = readRDS(file.path(type, x))
  tmp = rbindlist(lapply(names(tmp), function(x){
    data.table(value = tmp[[x]][,76], sim = label[x == funs],
               date = get(switch(as.character(nrow(tmp$cosine)),
                                 "41" = "xyear",
                                 "163" = "xquarter",
                                 "487" = "xmonth")))
  })) 
})
l_tech = lapply(tmp$name, function(x){
  tmp = readRDS(file.path(type, x))
  tmp = rbindlist(lapply(names(tmp), function(x){
    data.table(value = tmp[[x]][,44], sim = label[x == funs],
               date = get(switch(as.character(nrow(tmp$cosine)),
                                 "41" = "xyear",
                                 "163" = "xquarter",
                                 "487" = "xmonth")))
  })) 
})
l_stop = lapply(tmp$name, function(x){
  tmp = readRDS(file.path(type, x))
  tmp = rbindlist(lapply(names(tmp), function(x){
    data.table(value = tmp[[x]][,46], sim = label[x == funs],
               date = get(switch(as.character(nrow(tmp$cosine)),
                                 "41" = "xyear",
                                 "163" = "xquarter",
                                 "487" = "xmonth")))
  })) 
})
names(l_health) = names(l_tech) = names(l_stop) = tmp$name

ggmatrix(
  lapply(l_health, function(i){
    ggplot() +
      geom_line(aes(x = date, y = value, group = sim, col = sim), data = i) + ylim(0, 1)
  }), nrow = 3, ncol = 3)

ggmatrix(
  lapply(l_health[c("sim_year.rds", "sim_quarter.rds", "sim_month.rds")], function(i){
    ggplot() +
      geom_line(aes(x = date, y = value, group = sim, col = sim), data = i) + ylim(0, 1) +
      guides(col = guide_legend(title = "", nrow = 2)) + theme(legend.position = "top")
  }), nrow = 3, ncol = 1, yAxisLabels = c("yearly", "quarterly", "monthly"), legend = 1) +
  theme(legend.position = "top")
# 420x500

ggmatrix(
  lapply(l_health[c("sim_year.rds", "sim_quarter.rds", "sim_month.rds")], function(i){
    ggplot() +
      geom_line(aes(x = date, y = value, group = sim, col = sim), data = i) + ylim(0, 1) +
      scale_x_continuous(breaks = as.Date(c("1990-01-01",
                                            "2000-01-01",
                                            "2010-01-01",
                                            "2020-01-01")),
                         labels = c(1990, 2000, 2010, 2020)) +
      guides(col = guide_legend(title = "Similarity Measure"))
  }), nrow = 1, ncol = 3, xAxisLabels = c("Yearly", "Quarterly", "Monthly"), legend = 1) 
# 850x200
# 850x166

z_health = rbindlist(
  lapply(l_health[c("sim_quarter.rds", "sim_quarter_first.rds", "sim_quarter_last.rds")],
         function(x) x[sim == "Cosine"]))
z_health[, type := factor(rep(c("Quarterly", "First", "Last"), each = nrow(z_health)/3),
                          levels = c("Quarterly", "First", "Last"))]
z_tech = rbindlist(
  lapply(l_tech[c("sim_quarter.rds", "sim_quarter_first.rds", "sim_quarter_last.rds")],
         function(x) x[sim == "Cosine"]))
z_tech[, type := factor(rep(c("Quarterly", "First", "Last"), each = nrow(z_tech)/3),
                        levels = c("Quarterly", "First", "Last"))]
z_stop = rbindlist(
  lapply(l_stop[c("sim_quarter.rds", "sim_quarter_first.rds", "sim_quarter_last.rds")],
         function(x) x[sim == "Cosine"]))
z_stop[, type := factor(rep(c("Quarterly", "First", "Last"), each = nrow(z_stop)/3),
                        levels = c("Quarterly", "First", "Last"))]


a = plotTopic(obj, lda, names(docs), select = c(76, 44, 46), rel = TRUE, curves = "both", legend = "none", unit = "quarter")
colnames(a) = c("date", "Health", "Technology", "Stopword")
b = melt(a, "date")

plots = lapply(c("Health", "Technology", "Stopword"), function(i)
  ggplot(b[b$variable == i,]) + aes(x = date, y = value) + geom_line() +
    xlab("") + ylab("Topic Share") + ylim(c(0, max(b[b$variable == "Health","value"]))))

plots[4:6] = lapply(list(z_health, z_tech, z_stop), function(i)
  ggplot(i) + aes(x = date, y = value, group = type, col = type) +
    geom_line() + ylim(c(0,1)) +
    scale_x_continuous(breaks = as.Date(c("1990-01-01",
                                          "2000-01-01",
                                          "2010-01-01",
                                          "2020-01-01")),
                       labels = c(1990, 2000, 2010, 2020)) +
    guides(col = guide_legend(title = "Similarity Type")))

ggmatrix(plots, nrow = 2, ncol = 3, legend = 6,
         xAxisLabels = c("Health", "Technology", "Stopword"),
         yAxisLabels = c("Share", "Cosine"),
         yProportions = c(0.4, 0.6))
#850x280
#850x200

getWordCountsPer = function(unit, select, obj, docs, lda){
  floored = floor_date(obj$meta$date[match(names(docs), obj$meta$id)], unit)
  x = sort(unique(floored))
  n_vocab = ncol(getTopics(lda))
  tmp = sapply(x, function(z){
    voc = unlist(lapply(docs[floored == z], function(y) y[1,]))+1
    tmp = tabulate(voc[unlist(getAssignments(lda)[floored == z])+1 == select], nbins = n_vocab)
    tmp
  })
  tmp = matrix(as.integer(t(tmp)), nrow = length(x))
  colnames(tmp) = colnames(getTopics(lda))
  tmp
}
counts = getWordCountsPer("quarter", 76, obj, docs, lda)
tw = topWords(counts, 5)
colnames(tw) = as.character(sort(unique(floor_date(obj$meta$date, "quarter"))))

counts2 = getWordCountsPer("quarter", 44, obj, docs, lda)
tw2 = topWords(counts2, 5)
colnames(tw2) = as.character(sort(unique(floor_date(obj$meta$date, "quarter"))))

xtable(cbind("Overall" = topWords(getTopics(lda), 5)[,76],
             tw[,as.character(l$sim_quarter.rds[sim == "Cosine" & value < 0.9, date])]))

a = plotTopic(obj, lda, names(docs), select = c(76, 44, 46), rel = TRUE, curves = "both", legend = "none", unit = "quarter")
colnames(a) = c("date", "Health", "Technology", "Stopword")
b = melt(a, "date")

ggplot(b) + aes(x = date, y = value, col = variable) + geom_line() + ylim(c(0, max(b$value))) +
  xlab("") + ylab("Relative Topic-Frequency") + guides(col = guide_legend(title = "")) + theme(legend.position = "top")
