library(tosca)
library(lubridate)

setwd("//media/TextMining/DoCMA/data/TheNewYorkTimes")

source = c("New York Times", "NEW YORK TIMES", "The New York Times")

obj = readRDS("nyt-clean.rds")

obj = filterID(obj, obj$meta$id[obj$meta$resource %in% source])
obj = filterDate(obj,
                 s.date = as.Date(paste0("1980-01-01")),
                 e.date = as.Date("2020-12-31"))

obj$text = lapply(obj$text, paste, collapse = " ")

obj$text = removeXML(obj$text)
obj$text = lapply(obj$text, function(x) gsub("&[^;]*;", " ", x))
obj$text = lapply(obj$text, function(x) gsub("\u00AD", "", x))

obj$meta = obj$meta[!is.na(obj$meta$id),]
obj$meta = obj$meta[!is.na(obj$meta$date),]
obj$text = obj$text[names(obj$text) %in% obj$meta$id & !is.na(names(obj$text))]

obj$meta = obj$meta[order(obj$meta$date),]
obj$text = obj$text[match(obj$meta$id, names(obj$text))]
obj$text = obj$text[!duplicated(obj$text)]
obj$meta = obj$meta[obj$meta$id %in% names(obj$text),]

obj = cleanTexts(obj, sw = "en")

saveRDS(obj, "cleannyt.rds")

l = split(obj$meta$id, floor_date(obj$meta$date, "weeks"))
set.seed(20201231)
ids = unlist(mapply(function(x, y) sample(x, y), l, ceiling(lengths(l)*0.15)))

obj = filterID(obj, ids)
saveRDS(obj, "cleannytsample15.rds")
