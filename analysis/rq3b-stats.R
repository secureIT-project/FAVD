#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# this script looks at the aggregate data per fold (from `grep "###" *raw`).
# expects post 13 june data where 'plus' and 'minus' are computed.
# (though they are only meaningful for the '-c' data)
# see the extract script
#
# compares 50fold and 10-fold data and then 
# from the 10-fold data it compares c-0 with m* and then c-all with m*
#
# run with -- see readme

options(digits=3, width=128) 

library(agricolae)
library(ggplot2)
library(reshape2)
library(xtable)


printf <- function(...) cat(sprintf(...))

ttest <- function(d, name=NULL)
{
  tt <- t.test(d$f2, d$AD, paired=TRUE)
  m <- ifelse(is.null(name), as.character(d$mf[[1]]), name)
  printf("%-12.12s     %5.3f  %5.3f   %s   %d\n", m, mean(d$f2), mean(d$AD), pp(tt[["p.value"]]), nrow(d))
}


xttest <- function(d, name=NULL)
{
  prog = d$it[[1]]
  if (length(unique(d$f)) != 2) {
    printf("%s fails to have exactly two 'f' values :(\n", prog)
    # print(unique(d$f))
  } else {
    m <- ifelse(is.null(name), "foo",  name)
    tt <- t.test(d$f2 ~ d$f) # , paired=TRUE)
    printf("%-12.12s  %-6.6s     %s   %d\n", prog, m,  pp(tt[["p.value"]]), nrow(d))
  } 
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


cmp.cm <- function(d, exclude, prog = NA)
{
  if (is.na(prog)) prog <- d$program[[1]]
  d <- d[d$mf != exclude , ]   

  if (sum(d$f2) == 0) {
    printf("\n%s aov(f2 ~ mf)  oops all f2 values are zero\n", prog)
    return (0)
  }

  if (length(unique(d$mf)) < 2)
    return (0)

  m <- aov(d$f2 ~ d$mf) 
  printf("\n%s aov(f2 ~ mf)  p-value %s\n", prog, pp(overall_p(m)))
  # print(HSD.test(m, "d$mf", group=TRUE)$groups)

  a <- aggregate(list(f2=d$f2,AD=d$AD, dwc=d$dwc), by=list(d$mf), FUN=mean)
  colnames(a) <- c("mf", "f2", "all_dangerous", "dwc")
  a$dwc <- round(a$dwc, digits=0)
  a$f2 <- round(a$f2, digits=3)
  t <- HSD.test(m, "d$mf", group=TRUE)$groups
  t <- merge(t, a[, c("mf", "dwc")], by.x="row.names", by.y="mf")   # add in dwc
  colnames(t) <- c("mf", "f2", "group", "dwc")
  print(t[order(-t$f2), ],  row.names = FALSE)
  cat("\n")
}


by.dataset <- function(d)
{
  loo <- d[d$mm %in% c("c-loo","m-loo"),  ]
  win <- d[d$mm %in% c("c-win","m-win"),  ]
  vdisc <- d[d$mm %in% c("c-vdisc","m-vdisc"),  ]
  printf("\npost filtering data sizes -- all %d   loo %d   win %d   vdisc %d\n\n", 
         nrow(d), nrow(loo), nrow(win), nrow(vdisc))

  cmp.cm(win, "xyzzy", prog="win:")
  cmp.cm(loo, "xyzzy", prog="loo:")
  cmp.cm(vdisc, "xyzzy", prog="vdisc:")
}



args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1)  stop("usage: rq3b.R slow.csv")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set

# d$predicted <- d$best.test
# cat("*** best test data ....\n")

names(d)[names(d) == "predicted"] <- "f2"
d <- d[!grepl("b10000.v", d$program), ]  # omit 1% samples b10000.v160 and b10000.v720 
d <- d[!grepl("v16", d$program), ]  # omit 1% samples b10000.v160 and b10000.v720 
d <- d[!grepl("b100", d$program), ]  # omit rest of b100k and b10k data


d$filter[d$filter == "-999"] <- "all"
d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0
d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$filter <- as.factor(d$filter)
d$model <- as.factor(d$model)

d$mm <- as.factor(paste(d$model, ifelse(grepl("loo", d$program), "-loo", ifelse(grepl("b100", d$program), "-b100k", ifelse(d$program == "VDISC", "-vdisc", "-win"))), sep=""))
d$program <- factor(d$program, levels = c("Asterisk", "FFmpeg", "LibPNG", "LibTIFF", "sPidgin", "Pidgin", "VLC", "loo", "b100000.v7200", "VDISC"))

cat("\n*** m-1 sucks and warps averages ... omit it! ***\n")
d <- d[!d$mf == "m-1",]
cat("\n*** omit m-all (it is not one of the top three) ***\n")
d <- d[!d$mf == "m-all",]

# if added to slow remove it :)
# d$pm <- factor(paste0(d$plus,"-",d$minus), levels = l)
# cat("*** removing extremes: 1-1000, 1-100, 100-1, 1000-1 ***\n")
# d <- d[!d$pm %in% c("1-1000", "1-100", "100-1", "1000-1"), ]

# changes all the p-values ....
# cat("\n*** using m-0.99 only as it is the best of the m's ***\n")
# d <- d[d$mf %in% c("c-all", "m-0.99", "c-0"), ]


cat("\nhead-to-head compare of each c-all and separately each c-0 for each data commented out\n")
cat("\n\ncompare each program (data set) separately for c-all (without c-0)\n")
x <- by(d, d$program, FUN=function(x) cmp.cm(x, "c-0"))     # comparisons for c-all (omitting c-0)

cat("\n\ncompare each program (data set) separately for c-0 (without c-all)\n")
x <- by(d, d$program, FUN=function(x) cmp.cm(x, "c-all"))   # comparisons for c-0 (omitting c-all)

 
prog <- "all"
printf("\n\ncompare the performance of each data set using data from %s\n", args[1])
printf("this data mirrors Table III, but is presently unused\n")
by.dataset(d)


cat("\n\n")
prog <- "all"
d.0 <- d[d$mf != "c-all", ]   # compare these two separately with the m's
d.all <- d[d$mf != "c-0", ]

m <- aov(d.all$f2 ~ d.all$mf)
printf("\n%s c-all aov(f2 ~ mf)  p-value %s\n", prog, pp(overall_p(m)))
print(HSD.test(m, "d.all$mf", group=TRUE)$groups)

cat("for 'overall' data in Table {tab:ds-comparison}\n")
m <- aov(d.0$f2 ~ d.0$mf)
printf("\n%s c-0 aov(f2 ~ mf)  p-value %s\n", prog, pp(overall_p(m)))
print(HSD.test(m, "d.0$mf", group=TRUE)$groups)




## focus provides no diff ?
dd <- d[d$mf %in% c("c-all", "c-0"),]
m <- aov(dd$f2 ~ dd$mf) 
printf("\njust c-all vs c-0 aov(f2 ~ mf)  p-value %s\n", pp(overall_p(m)))
print(HSD.test(m, "dd$mf", group=TRUE)$groups)

## focus provides no diff ?
## dd <- d.all[d.all$mf %in% c("c-all", "m-0.9"),]
## m <- aov(dd$f2 ~ dd$mf) 
## printf("\n%s c-all vs m-0.90 aov(f2 ~ mf)  p-value %s\n", "foo", pp(overall_p(m)))
## print(HSD.test(m, "dd$mf", group=TRUE)$groups)


cat("\n\nLeftovers\n1) all m's the same consider using 'best' only")
x <- d[grepl("m-", d$mf), ]
m <- aov(x$f2 ~ x$mf)
printf("\n%s c-all aov(f2 ~ mf)  p-value %s\n", prog, pp(overall_p(m)))
print(HSD.test(m, "x$mf", group=TRUE)$groups)


cat("\n\n2) focus on just c-* still shows no difference\n")
dd <- d[d$mf %in% c("c-all", "c-0"),]
m <- aov(dd$f2 ~ dd$mf)
printf("\n%s c-all vs c-0 aov(f2 ~ mf)  p-value %s\n", prog, pp(overall_p(m)))
print(HSD.test(m, "dd$mf", group=TRUE)$groups)

d <- d[grepl("c-", d$mf), ]
x <- by(d, d$program, FUN=function(x) cmp.cm(x, "xyzzy"))  

cat("\ndwc table (IST Table 7\n\n")

a <- aggregate(list(f2=d$f2,dwc=d$dwc), by=list(d$program, d$mf), FUN=mean)
colnames(a) <- c("program", "mf", "f2", "dwc")

t <- merge(a[a$mf == "c-0", ], a[a$mf != "c-0", ], by="program")[, c(1,3,6,4,7)]
colnames(t) <- c("program", "f2-0", "f2-all", "dwc-0", "dwc-all")

t$percent = t$"dwc-0" / t$"dwc-all" *100
print(t)

foo <- xtable(t, type="latex")
digits(foo) <- c(0,0, 3, 3, 0, 0, 2)
caption(foo) <- "Dangerous Word Count Comparison"
label(foo) <- "tab:dwcc"
print.xtable(foo, file="rq3b.tex", include.rownames=FALSE)   # IST Table 7

# note that loo varies a lot (compared to the m- data) impacted its ttest 
cat("\n\np-values comparing f2 with `all dangerous' ***: \n\n")
cat("data subset        f2     AD     p-value    n\n")
ttest(d, "all-data")
ttest(d[d$mm %in% c("c-win","m-win"),  ], "win")
ttest(d[d$mm %in% c("c-loo","m-loo"),  ], "loo")
ttest(d[d$mm %in% c("c-vdisc","m-vdisc"),  ], "vdisc")
x <- by(d, d$mf, FUN=ttest)    # run t-test foreach value of mf
print(unique(d$mf))

cat("\n")
