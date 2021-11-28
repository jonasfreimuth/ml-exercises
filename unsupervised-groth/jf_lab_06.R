d <- read.table("data/DW-Data/survey-2021-05.tab", stringsAsFactors = TRUE)

summary(d) # number of NAs etc
pairs(d) # overview of correlation structure

# detect complete datasets
incomplete=apply(d,1,function(x) {TRUE %in% is.na(x)})
d=d[which(incomplete==FALSE),]


# a simple regression model, height as a function of weight
m=lm(cm~kg,data=d)
summary(m)
