library(tosca)
library(ldaPrototype)
library(lubridate)
library(ggplot2)
library(GGally)
library(ggthemes)
library(data.table)
library(xtable)
library(stringr)

cosine = function(a, b){
  a = a/sum(a)
  b = b/sum(b)
  sum(a*b) / sqrt(sum(a^2)) / sqrt(sum(b^2))
}

obj = readRDS("cleannytsample15.rds")

xmonth = sort(unique(floor_date(obj$meta$date, unit = "month")))
xquarter = sort(unique(floor_date(obj$meta$date, unit = "quarter")))
xyear = sort(unique(floor_date(obj$meta$date, unit = "year")))

# determine similar topics top topic 76 from "rolling_quarter_1985-01-01_memory3"
type = "rolling_quarter_1985-01-01_memory3"
tmp = CJ(memory = 1:4, start = c(1981, 1985, 2000))
tmp[, name := paste0("rolling_quarter_", start, "-01-01_memory", memory)]

lda = lapply(tmp$name, function(folder) readRDS(file.path(folder, "ldachunked.rds")))
names(lda) = tmp$name
doc_names = lapply(tmp$name, function(folder) names(readRDS(file.path(folder, "docsall.rds"))))
names(doc_names) = tmp$name

vocab = lapply(tmp$name, function(folder)
  readRDS(file.path(folder, paste0("vocab",
                                   max(as.numeric(str_extract(list.files(folder, pattern = "vocab"), "[0-9]+"))), ".rds"))))
vocab_all = unique(unlist(vocab))

u = lapply(lda, function(x){
  sapply(1:80, function(y){
    a = getTopics(lda[["rolling_quarter_1985-01-01_memory3"]])[76,]
    a = a[match(vocab_all, names(a))]
    a[is.na(a)] = 0
    b = getTopics(x)[y,]
    b = b[match(vocab_all, names(b))]
    b[is.na(b)] = 0
    cosine(a, b)
  })
})
topicnr = sapply(u, which.max)
types = c("Yearly" = "sim_year.rds",
          "Quarterly" = "sim_quarter.rds",
          "Quarterly to First" = "sim_quarter_first.rds",
          "Quarterly to Last" = "sim_quarter_last.rds")

l = rbindlist(lapply(1:12, function(i){
  rbindlist(
    lapply(1:4, function(j){
      val = readRDS(file.path(tmp$name[i], types[j]))$cosine[,topicnr[i]]
      data.table(type = names(types)[j],
                 parameter = tmp$name[i],
                 date = get(switch(as.character(length(val)),
                                   "41" = "xyear",
                                   "163" = "xquarter",
                                   "487" = "xmonth")),
                 value = val)
    })
  )
}))

l[, start := as.Date(str_extract(parameter, "[0-9]{4}-01-01"))]

freq = lapply(1:12, function(i){
  h = plotTopic(obj, lda[[tmp$name[i]]], doc_names[[tmp$name[i]]], topicnr[i], unit = "quarter", rel = T)
  h$type = "bla"
  colnames(h) = c("date", "value", "type")
  h
})
names(freq) = tmp$name

ggplot(l) + aes(x = date, y = value, group = type, col = type) + geom_line() + 
  facet_wrap(~ parameter)

plots = lapply(tmp$name, function(x){
  ggplot(l[x == parameter]) + aes(x = date, y = value, group = type, col = type) +
    geom_vline(xintercept = l[x == parameter, start][1], col = "red", linetype = "dashed") +
    geom_line() + 
    geom_line(data = freq[[x]], aes(x = date, y = value*7), color = "black") +
    scale_x_continuous(breaks = as.Date(c("1990-01-01",
                                          "2000-01-01",
                                          "2010-01-01",
                                          "2020-01-01")),
                       labels = c(1990, 2000, 2010, 2020)) +
    guides(col = guide_legend(title = "Similarity Type")) + ylim(c(0,1)) +
    theme(legend.position = "top")
})

ggmatrix(plots, ncol = 4, nrow = 3,
         yAxisLabels = c(1981, 1985, 2000),
         xAxisLabels = 1:4, legend = 1, byrow = FALSE) + theme(legend.position = "top")
#850x400

changing = rev(c("rolling_year_1985-01-01_memory3",
                 "rolling_year_1985-01-01_memory3_changing",
                 "rolling_quarter_1985-01-01_memory3_changing",
                 "rolling_quarter_1985-01-01_memory3"))

lda_c = lapply(changing, function(folder) readRDS(file.path(folder, "ldachunked.rds")))
names(lda_c) = changing
doc_names_c = lapply(changing, function(folder) names(readRDS(file.path(folder, "docsall.rds"))))
names(doc_names_c) = changing

l_c = rbindlist(lapply(1:4, function(i){
  rbindlist(
    lapply(1:4, function(j){
      val = readRDS(file.path(changing[i], types[j]))$cosine[,76]
      data.table(type = names(types)[j],
                 parameter = changing[i],
                 date = get(switch(as.character(length(val)),
                                   "41" = "xyear",
                                   "163" = "xquarter",
                                   "487" = "xmonth")),
                 value = val)
    })
  )
}))

freq_c = lapply(1:4, function(i){
  h = plotTopic(obj, lda_c[[changing[i]]], doc_names_c[[changing[i]]], 76, unit = "quarter", rel = T)
  h$type = "bla"
  colnames(h) = c("date", "value", "type")
  h
})
names(freq_c) = changing

ggplot(l_c) + aes(x = date, y = value, group = type, col = type) + geom_line() + 
  facet_wrap(~ parameter)

plots_c = lapply(changing, function(x){
  ggplot(l_c[x == parameter]) + aes(x = date, y = value, group = type, col = type) +
    geom_line() + 
    geom_line(data = freq_c[[x]], aes(x = date, y = value*7), color = "black") +
    scale_x_continuous(breaks = as.Date(c("1990-01-01",
                                          "2000-01-01",
                                          "2010-01-01",
                                          "2020-01-01")),
                       labels = c(1990, 2000, 2010, 2020)) +
    guides(col = guide_legend(title = "Similarity Type")) + ylim(c(0,1))
})
ggmatrix(plots_c, ncol = 4, nrow = 1,
         xAxisLabels = rev(c("Year", "Year - Changing", "Quarter - Changing", "Quarter")))
#850x140
