#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# this script looks at the aggregate data per fold (from `grep "###" *raw`).
# it compares f2 values by data-set over all p-m values, and f2 with AD values 
#
# run with
#   rq3-stats.R weights.csv

options(digits=3, width=150)

library(agricolae)

printf <- function(...) cat(sprintf(...))

ttest <- function(d, name=NULL)
{
  tt <- t.test(d$predicted, d$AD, paired=TRUE)
  m <- ifelse(is.null(name), as.character(d$mf[[1]]), name)
  printf("%-12.12s     %5.3f  %5.3f   %s   %d\n", m, mean(d$predicted), mean(d$AD), pp(tt[["p.value"]]), nrow(d))
}


xttest <- function(d, name=NULL)
{
  cat("\n")
  tt <- t.test(d$predicted ~ d$program, paired=TRUE)
  m <- ifelse(is.null(name), as.character(d$pm[[1]]), name)
  printf("%-12.12s     %s   %d\n", m,  pp(tt[["p.value"]]), nrow(d))
  d <- rbind(d,d)
  tt <- t.test(d$predicted ~ d$program, paired=TRUE)
  printf("%-12.12s     %s   %d\n", m,  pp(tt[["p.value"]]), nrow(d))
}


overall_p <- function (model) {     # overall p-value for lm or aov model
  c <- class(model)[[1]] 
  if (c == "lm") {
    f <- summary(model)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
  } else if ("aov" %in% c) {
    p <- summary(model)[[1]][["Pr(>F)"]][[1]]
  } else {
    stop("Not an object of class 'lm' or 'aov'")
  }

  attributes(p) <- NULL
  return(p)
}


pp <- function(pval)
{
  return ( ifelse(pval < 0.0001, "< 0.0001", sprintf("  %6.4f", pval)))
}


ext <- function(p) {
  # up through the initial submission some b100k data was in weights.csv.  
  # to use it replace xwe with vdisc. and comment out line XX above
  return (ifelse(p %in% c("loo"), "-loo", ifelse(grepl("VDISC", p), "-vdisc",
                 ifelse(grepl("b100", p), "-xwe", "-win"))))
}


args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1)  stop("usage: s.R <AD summary output cvs file>")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")


cat("*** removing 1% data sets (b10000.v*) ***\n")
d <- d[!grepl("b10000.v", d$program), ]
cat("*** removing 1.6% data set (b100000.v1) ***\n")
d <- d[!grepl("b100000.v1", d$program), ]
cat("*** removing minimum score 1.0 b100000.v1) ***\n")
d <- d[d$filter != "1", ]
cat("*** removing all (remaining) b100K and b10k data ***\n")
d <- d[!grepl("b10000", d$program), ]   # line XX (see below)

printf("\npost filtering step-1 data sizes -- all %d\n\n", nrow(d))

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set

d$filter[d$filter == "-999"] <- "all"
d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0

cat("*** removing m-all ***\n")
d <- d[!(d$model == "m" & d$filter == "all"), ]

d$model <- as.factor(paste(d$model, ext(d$program), sep=""))
d$filter <- as.factor(d$filter)

d$program <- as.character(d$program)
d$fold <- as.character(d$fold)
a <- d$program %in% c("loo")    # swap program and fold when program=loo 
x <- ifelse(a, d$program, d$fold)
y <- ifelse(a, d$fold, d$program)
d$program <- as.factor(y)
d$fold <- as.factor(x)
d$dwc <- round(d$dwc, digits=0)
d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$pm <- as.factor(paste0(d$plus,"-",d$minus))


d$mf <- as.factor(paste0(d$model,"-",d$filter))

cat("*** removing extremes: 1-1000, 1-100, 100-1, 1000-1 ***\n")
d <- d[!d$pm %in% c("1-1000", "1-100", "100-1", "1000-1"), ]


loo <- d[d$model %in% c("c-loo","m-loo"),  ]
win <- d[d$model %in% c("c-win","m-win"),  ]
vdisc <- d[d$model %in% c("c-vdisc","m-vdisc"),  ]
printf("\npost filtering data sizes -- all %d   loo %d   win %d   vdisc %d\n\n", 
       nrow(d), nrow(loo), nrow(win), nrow(vdisc))


a <- aggregate(list(predicted=d$predicted,AD=d$AD, dwc=d$dwc), by=list(d$mf), FUN=mean)
colnames(a) <- c("mf", "f2", "all_dangerous", "dwc")
a$dwc <- round(a$dwc, digits=1)
a$f2 <- round(a$f2, digits=3)
a$all_dangerous <- round(a$all_dangerous, digits=3)
# print(summary(a))
# cat("\nmean f2 by model and filter, sorted on f2\n")
# print(a[order(-a$f2),])

aa <- aggregate(list(predicted=d$predicted,AD=d$AD, dwc=d$dwc), by=list(d$program, d$pm, d$mf), FUN=mean)
colnames(aa) <- c("program", "pm", "mf", "f2", "all_dangerous", "dwc")
aa$dwc <- round(aa$dwc, digits=1)
aa$f2 <- round(aa$f2, digits=3)
aa$all_dangerous <- round(aa$all_dangerous, digits=3)


d <- within(d, mf <- relevel(mf, ref = 9))   # 9 is 'm-all' FIX ME pick worst performing?

m <- aov(d$predicted ~ d$mf)
printf("\n\nall data  aov(f2 ~ mf)  p-value %s  R^2 = %5.3f\n\n", pp(overall_p(m)), summary(m)$r.squared)
# print(summary(m)$coefficients)
# print(HSD.test(m, "d$mf", group=TRUE)$groups)
t <- HSD.test(m, "d$mf", group=TRUE)$groups
t <- merge(t, a[, c("mf", "dwc")], by.x="row.names", by.y="mf")   # add in dwc
colnames(t) <- c("mf", "f2", "group", "dwc")
print(t[order(-t$f2), ])
cat("\n")


cat("\n*** Warning run on weights.csv these stats are over the range of p-m values.  see rq3b.R\n\n")
m <- aov(win$predicted ~ win$mf)
printf("\n\nwin : aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
print(HSD.test(m, "win$mf", group=TRUE)$groups)

## pm is statistically uninteresting
## m <- aov(win$predicted ~ win$mf + win$pm)
## printf("\n\nwin : aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
## print(HSD.test(m, "win$mf", group=TRUE)$groups)
## print(HSD.test(m, "win$pm", group=TRUE)$groups)

## w72 <- win[ win$plus %in% c("7", "x") & win$minus %in% c("2", "x"), ]
## m <- aov(w72$predicted ~ w72$mf)
## printf("\nwin 7-2 : aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
## print(HSD.test(m, "w72$mf", group=TRUE)$groups)

## m <- aov(w72$predicted ~ w72$mf + w72$pm)
## printf("\n\nw72 : aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
## print(HSD.test(m, "w72$mf", group=TRUE)$groups)
## print(HSD.test(m, "w72$pm", group=TRUE)$groups)


m <- aov(loo$predicted ~ loo$mf)
printf("\n\nloo: aov(f2 ~ mf)  p-value %s\n\n", pp(overall_p(m)))
print(HSD.test(m, "loo$mf", group=TRUE)$groups)

## l21 <- loo[loo$plus %in% c("2", "x") & loo$minus %in% c("1", "x"), ]
## m <- aov(l21$predicted ~ l21$mf)
## printf("\nloo 2-1 : aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
## print(HSD.test(m, "l21$mf", group=TRUE)$groups)


m <- aov(vdisc$predicted ~ vdisc$mf)
printf("\n\nvdisc: aov(f2 ~ mf)  p-value %s\n\n",  pp(overall_p(m)))
print(HSD.test(m, "vdisc$mf", group=TRUE)$groups)


cat("\n\nf2 - AD comparison by mf, sorted by diff\n\n")
a <- aggregate(list(d$predicted,d$AD), by=list(d$mf), FUN=mean)
colnames(a) <- c("mf", "predicted", "AD")
a$diff = a$predicted - a$AD
print(a[order(-a$diff),])


cat("\n\np-values comparing f2 with `all dangerous' ***: \n\n")
cat("data subset        f2     AD     p-value    n\n")
ttest(d, "all-data")
ttest(loo, "loo")
ttest(win, "win")
ttest(vdisc, "vdisc")
x <- by(d, d$mf, FUN=ttest)    # run t-test foreach value of mf 
