library(tosca)
library(ldaPrototype)
library(lubridate)
library(ggplot2)
library(GGally)
library(ggthemes)
library(data.table)
library(xtable)
library(ggpubr)

obj = readRDS("cleannytsample15.rds")

xmonth = sort(unique(floor_date(obj$meta$date, unit = "month")))
xquarter = sort(unique(floor_date(obj$meta$date, unit = "quarter")))
xyear = sort(unique(floor_date(obj$meta$date, unit = "year")))

docs = readRDS("rolling_quarter_1985-01-01_memory3/docsall.rds")
lda = readRDS("rolling_quarter_1985-01-01_memory3/ldachunked.rds")
tw = paste0("T", 1:80, ": ", apply(topWords(getTopics(lda), 3), 2, paste, collapse = ", "))

funs = c("cosine", "jacc", "manhattan", "chi", "hellinger", "js")

tab = plotTopic(obj, lda, names(docs), unit = "quarter", rel = TRUE)

sim_quarter = readRDS("rolling_quarter_1985-01-01_memory3/sim_quarter.rds")
sim_quarter_first = readRDS("rolling_quarter_1985-01-01_memory3/sim_quarter_first.rds")
sim_quarter_last = readRDS("rolling_quarter_1985-01-01_memory3/sim_quarter_last.rds")

fun = "cosine"

pdf(file.path("figures" ,"example_quarter_85_3.pdf"), width = 4, height = 10)

for(i in 1:80){
  mytab = rbind(data.table(x = xquarter, y = tab[,i+1], type = NA, measure = "share"),
                rbindlist(
                  lapply(funs, function(x){
                    rbind(data.table(x = xquarter, y = sim_quarter[[x]][,i], type = "unit-to-unit", measure = x),
                          data.table(x = xquarter, y = sim_quarter_first[[x]][,i], type = "unit-to-first", measure = x),
                          data.table(x = xquarter, y = sim_quarter_last[[x]][,i], type = "unit-to-last", measure = x))
                  })))
  
  myplots = list()
  myplots[[1]] = ggplot(mytab[measure == "share"]) + aes(x = x, y = y) + geom_line() + xlab("") + ylab("") + ylim(c(0,0.1))
  myplots[2:(length(funs)+1)] = lapply(funs, function(meas){
    ggplot(mytab[measure == meas]) + aes(x = x, y = y, group = type, col = type) + geom_line() + xlab("") + ylab("") + ylim(c(0,1))
  })
  print(ggmatrix(myplots, nrow = length(myplots), ncol = 1, yAxisLabels = c("share", funs), xAxisLabels = tw[i]))
}

dev.off()

fwrite(rbind(paste0("Topic ", 1:80), topWords(getTopics(lda), 50)),
       "topwords_quarter_85_3.csv", col.names = FALSE)
