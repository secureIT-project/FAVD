#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# this script looks at the aggregate data per fold (grep "###" *raw).

# runs Tukey and draw graphs comparing the f2 values for the data sets loo, 
# within-program, and VDISC.  graphs include selected top performing -m (model) 
# value as reference.
#
# FIX ME include the all-dangerous reference lines for each data set as well???
# NOTE that c-loo:all == m-loo:all  
#
# run with
#   extract data/*
#   rq3.R .data.csv
#   stats.R all.csv

# extract *.raw
#   echo "program,model,filter,plus,minus,AD,AD train,AD test,best train,predicted,best test,fold,dwc" > ! .data.csv
#   grep -H "###" $argv | grep -v AD | sed -e 's/^[^\/]*\///' -e 's/\.[^.]*:###/,/' -e 's/\.-\(.\)./,\1,/' -e 's/ //g' >> .data.csv 
#   stats.R .data.csv

options(digits=3, width=128) 

library(ggplot2)

printf <- function(...) cat(sprintf(...))

# program,model,filter,plus,minus,AD,AD train,AD test,best train,predicted,best test,fold,dwc


expand_row <- function(r, model)
{
  xx <- data.frame(do.call("rbind", replicate(nrow(model), r, simplify = FALSE)))
  names(xx)[names(xx) == "predicted"] <- "f2"
  names(xx)[names(xx) == "AD"] <- "all_dangerous"
  xx$f2 <- as.double(as.character(xx$f2))
  xx$all_dangerous <- as.double(as.character(xx$all_dangerous))
  xx$dwc <- as.double(as.character(xx$dwc))
  xx$plus <- model$plus   # copy over x-axis values
  xx$minus <- model$minus
  xx$pm <- model$pm
  return(xx)
}


ext <- function(p) {
  # up through the initial submission some b100k data was in weights.csv.  
  # to use it replace xwe with vdisc. and comment out line XX below
  return (ifelse(p %in% c("loo"), "-loo", ifelse(grepl("VDISC", p), "-vdisc", ifelse(grepl("b100", p), "-xwe", "-win"))))
}



args <- commandArgs(trailingOnly = TRUE)
# if (length(args) < 1)  stop("usage: s.R <AD summary output cvs file>")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set

cat("*** removing 1% data sets (b10000.v*) ***\n")
d <- d[!grepl("b10000.v", d$program), ]
cat("*** removing 1.6% data set (b100000.v1) ***\n")
d <- d[!grepl("b100000.v1", d$program), ]
cat("*** removing minimum score 1.0 ***\n")
d <- d[d$filter != "1", ]
cat("*** removing all (remaining) b100K and b10k data ***\n")
d <- d[!grepl("b10000", d$program), ]   # line XX (see below)

printf("\npost first filtering step %d rows\n", nrow(d))

if ("m" %in% levels(d$model)) {
  d[d$model == "m",]$plus <- "x"   # subsequent code assumes -m data has plus == 'x'
  d[d$model == "m",]$minus <- "x"  # just to be sure :)
  d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0
}
d$filter[d$filter == "-999"] <- "all"

d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$dataset <- factor(ifelse(d$program == "loo", "leave one out", ifelse(grepl("b100", d$program), "b100k", ifelse(d$program=="VDISC", "VDISC", "within program"))))
d$model <- as.factor(paste(d$model, ext(d$program), sep=""))
d$filter <- as.factor(d$filter)

d$program <- as.character(d$program)
d$fold <- as.character(d$fold)
a <- d$program %in% c("loo")    # swap program and fold when program=loo 
x <- ifelse(a, d$program, d$fold)
y <- ifelse(a, d$fold, d$program)
d$program <- as.factor(y)
d$fold <- as.factor(x)

Ms <- d[d$plus == "x",]   # -m output omits plus and minus
if (nrow(Ms) > 0) {
  Ms <- aggregate(list(predicted=Ms$predicted, AD=Ms$AD, dwc=Ms$dwc), by=list(Ms$dataset, Ms$model,Ms$filter,Ms$plus, Ms$minus), FUN=mean)
  colnames(Ms) <- c("dataset", "model", "filter", "plus", "minus", "f2", "all_dangerous", "dwc")
}


d <- d[! d$plus == "x",] 
d$plus <- as.double(as.character(d$plus))
d$minus <- as.double(as.character(d$minus))
d <- d[order(d$plus/d$minus),]
x <- paste0(d$plus,"-",d$minus)
l <- x[!duplicated(x)]
# cat("\nlevels = ")
# print(l)

d$pm <- factor(paste0(d$plus,"-",d$minus), levels = l)
d$plus <- as.factor(d$plus)
d$minus <- as.factor(d$minus)

printf("post separating -c data %d rows\n", nrow(d))

cat("\n*** removing intermediate m data ***\n")
d <- d[!d$mf %in% c("m-0.25", "m-0.5", "m-0.75", "m-0.8", "m-0.98", "m-1", "m-loo-0.25", "m-loo-0.5", "m-loo-0.75", "m-loo-0.8", "m-loo-0.98", "m-loo-1"), ]
cat("*** removing extremes: 1-1000, 1-100, 100-1, 1000-1 ***\n")
d <- d[!d$pm %in% c("1-1000", "1-100", "100-1", "1000-1"), ]

printf("\npost separating -c and final filters, %d rows\n", nrow(d))


a <- aggregate(list(predicted=d$predicted, AD=d$AD, dwc=d$dwc), by=list(d$dataset, d$model, d$filter, d$plus, d$minus), FUN=mean)
colnames(a) <- c("dataset", "model", "filter", "plus", "minus", "f2", "all_dangerous", "dwc")
# print(a[order(-a$f2),])
# print(summary(a))

# print(Ms[Ms$dataset == "VDISC" ,])
# print(a[a$dataset == "VDISC" & a$filter == "all", ])
# stop('a')

# replicate 'm' data for all plus,minus levels
mx <- head(a$model, 1)
fx <- head(a$filter, 1)
model <- a[a$model == mx & a$filter == fx , ]   # used for range of plus, minus, pm values

printf("model rows %d\n", nrow(model))

if (nrow(Ms) > 0) {
  adds <- do.call("rbind", apply(Ms, 1, function(x) expand_row(x,model)))
  a <- rbind(a, adds)
}
a$pm <- factor(paste0(a$plus,"-",a$minus), levels = l)
a$mf <- factor(paste0(a$model,"-",a$filter))

# library("scales") 
# n1 <- 8
# hex_codes1 <- hue_pal()(n1)                             # Identify hex codes
# print(hex_codes1                  )




pdf("graphs.pdf", 10, 5) 

theme_set(theme_gray(base_size = 16))
lab1 <- c("F-VDISC-0",
          "F-VDISC-all",
          "F-loo-0",
          "F-loo-all",
          "F-within-0",
          "F-within-all",
          "L-VDISC-0.05",
          "L-loo-all",
          "L-within-0.99",
          "L-loo-0.90")
colors <- c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC", "#000000", "#C77CFF")


# retain only best and worst of the m's for the first chart
# cat("\nscan to find  min/max mf for all m techniques\n")
# do.call(rbind, lapply(split(a,as.factor(a$mf)), function(x) {return(x[which.min(x$f2),])}))
# answer is   max m-vdisc-0.05 at 0.254  and min m-loo-all at 0.127

aa <- a[a$mf %in% c("c-vdisc-0", "c-loo-0", "c-win-0", "c-vdisc-all", "c-loo-all", "c-win-all", "m-vdisc-0.05", "m-loo-all"), ]

aa <- droplevels(aa)
# print(summary(aa$mf))
# print(summary(lab1))

g <- ggplot(data = aa) + aes(y = f2, x = pm, color=mf)  +
     ylab("f2") +
     xlab("weight") + 
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     geom_point(aes(color = mf, shape=mf, group=mf) ) +
     geom_line(aes(color = mf, group=mf) ) +
     guides(color=guide_legend(title="tag")) +
     guides(shape=guide_legend(title="tag")) +
     scale_color_manual(values=colors, labels=lab1) + 
     scale_shape_manual(values=1:nlevels(aa$mf), labels=lab1) +
     ggtitle("")
     # ggtitle("All three datasets")
print(g)


win <- a[a$model %in% c("c-win","m-win"),  ]
loo <- a[a$model %in% c("c-loo","m-loo"),  ]
vdisc <- a[a$model %in% c("c-vdisc","m-vdisc"),  ]

# best m's win 0.99   loo 0.90   vdisc  0.05

win <- win[win$mf %in% c("c-win-0", "c-win-all", "m-win-0.99"), ]
loo <- loo[loo$mf %in% c("c-loo-0", "c-loo-all", "m-loo-0.9"), ]
vdisc <- vdisc[vdisc$mf %in% c("c-vdisc-0", "c-vdisc-all", "m-vdisc-0.05"), ]



# print(unique(a$dwc))
# print(summary(a$dwc))
# x <- loo[, c("dwc", "mf", "pm", "filter")]    # verify that dwc does actually change ....
# print(x[order(x$filter, x$pm),])


## d$pm <- factor(paste0(d$plus,"-",d$minus), levels = l)
## dd <- aggregate(list(predicted=d$predicted, AD=d$AD, dwc=d$dwc), by=list(d$program, d$model, d$filter, d$plus, d$minus), FUN=mean)
## colnames(dd) <- c("program", "model", "filter", "plus", "minus", "f2", "all_dangerous", "dwc")
## # dd <- a # d[d$fold %in% c("0", "LibPNG"), ]
## dd <- dd[!grepl("c-", dd$model), ]
## dd$ppf = interaction(dd$program, dd$model, dd$filter)
## dd$pm <- factor(paste0(dd$plus,"-",dd$minus), levels = l)
## # dd <- dd[dd$program == "FFmpeg" ,] # & dd$filter=="0", ]
## # print(summary(dd))
## # print(unique(dd$program))
## # print(head(dd, 20))
## 
## print(head(d[d$mf=="c-0" & d$program=="FFmpeg", ], 20))
## 
## g <- ggplot(data = dd) + aes(y = f2, x = pm, color=ppf)  +
##      geom_point(aes(color = ppf, shape=ppf) ) +
##      geom_line(aes(group=ppf, color = ppf) ) +
##      xlab("weight") + 
##      ylab("f2") +
##      ggtitle("Leave One Out by program")
## print(g)


g <- ggplot(data = loo) + aes(y = f2, x = pm, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     xlab("weight") + 
     ylab("f2") +
     guides(color=guide_legend(title="tag")) +
     guides(shape=guide_legend(title="tag")) +
     scale_color_manual(values=colors[c(3,4,9)], labels=lab1[c(3,4,10)]) +
     scale_shape_manual(values=c(3,4,9), labels=lab1[c(3,4,10)]) +
     ggtitle("Leave One Out")
print(g)

# loo <- loo[!loo$mf == "m-loo-all", ]   ## m-loo-all == c-loo-all
g <- ggplot(data = loo) + aes(y = dwc, x = pm, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group = mf )) +
     scale_y_continuous(trans='log2') +
     xlab("weight") + 
     ylab("Dangerous Words (log)") +
     guides(fill=guide_legend(title="Data Set")) +
     ggtitle("Leave One Out Dangerous Words Count (m-loo-all == c-loo-all)")
print(g)


g <- ggplot(data = win) + aes(y = f2, x = pm, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     xlab("weight") + 
     ylab("f2") +
     guides(color=guide_legend(title="tag")) +
     guides(shape=guide_legend(title="tag")) +
     scale_color_manual(values=colors[c(5,6,9)], labels=lab1[c(5,6,9)]) +
     scale_shape_manual(values=c(5,6,9), labels=lab1[c(5,6,9)]) +
     ggtitle("Within Project")
print(g)

g <- ggplot(data = win) + aes(y = dwc, x = pm, color=mf)  +
     geom_point(aes(group=mf)) +
     geom_line(aes(group = mf )) +
     scale_y_continuous(trans='log2') +
     xlab("weight") + 
     ylab("Dangerous Words (log)") +
     guides(fill=guide_legend(title="Data Set")) +
     ggtitle("Within Project Dangerous Words Count  (m-all == c-all)")
print(g)

x <- d[d$model %in% c("c-win","m-win"),  ]
x <- x[ x$plus %in% c("7", "x") & x$minus %in% c("2", "x"), ] # best case
x <- aggregate(list(f2=x$predicted), by=list( x$program, x$mf ), FUN=mean)
colnames(x) <- c("program", "mf", "f2")
g <- ggplot(data = x) + aes(y = f2, x = program, color=mf)  +
     geom_point(aes(group=mf)) +
     geom_line(aes(group = mf )) +
     xlab("weight") + 
     ylab("Dangerous Words (log)") +
     guides(fill=guide_legend(title="Data Set")) +
     ggtitle("c-7-2 data by program ... looks like only Pidgin is really different ... come back to this")
print(g)

g <- ggplot(data = vdisc) + aes(y = f2, x = pm, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     xlab("weight") + 
     ylab("f2") +
     guides(color=guide_legend(title="tag")) +
     guides(shape=guide_legend(title="tag")) +
     scale_color_manual(values=colors[c(1,2,9)], labels=lab1[c(1,2,7)]) +
     scale_shape_manual(values=c(1,2,9), labels=lab1[c(1,2,7)]) +
     ggtitle("VDISC")
print(g)

g <- ggplot(data = vdisc) + aes(y = dwc, x = pm, color=mf)  +
     geom_point(aes(group=mf)) +
     geom_line(aes(group = mf )) +
     scale_y_continuous(trans='log2') +
     xlab("weight") + 
     ylab("Dangerous Words (log)") +
     guides(fill=guide_legend(title="Data Set")) +
     ggtitle("VDISC Dangerous Words Count  (m-vdisc-all == c-vdisc-all)")
print(g)


## bvd <- d[grep("b100", d$program), ]
## bva <- aggregate(list(predicted=bvd$predicted, AD=bvd$AD, dwc=bvd$dwc), by=list(bvd$program, bvd$dataset, bvd$model, bvd$filter, bvd$plus, bvd$minus), FUN=mean)
## colnames(bva) <- c("program", "dataset", "model", "filter", "plus", "minus", "f2", "all_dangerous", "dwc")
## bva$pm <- factor(paste0(bva$plus,"-",bva$minus), levels = l)
## bva$mf <- factor(paste0(bva$program,"-",bva$filter))

## # both on the same graph (w/o the 10K data this is viable)
## g <- ggplot(data = bva) + aes(y = f2, x = pm, color=mf)  +
##      geom_point(aes(color = mf, shape=mf) ) +
##      geom_line(aes(group=mf, color = mf) ) +
##      coord_cartesian(ylim = c(0.0, 0.25)) +
##      ylab("f2") +
##      ggtitle("b100k.v* datasets (compares 7.2% and 1.6% data sets)")
## print(g)

## g <- ggplot(data = bva[bva$filter == "0", ]) + aes(y = f2, x = pm, color=mf)  +
##      geom_point(aes(color = mf, shape=mf) ) +
##      geom_line(aes(group=mf, color = mf) ) +
##      coord_cartesian(ylim = c(0.0, 0.25)) +
##      ggtitle("b10*.v10* [0.00] datasets")
## print(g)
## 
## g <- ggplot(data = bva[bva$filter == "all", ]) + aes(y = f2, x = pm, color=mf)  +
##      geom_point(aes(color = mf, shape=mf) ) +
##      geom_line(aes(group=mf, color = mf) ) +
##      coord_cartesian(ylim = c(0.0, 0.25)) +
##      ggtitle("b10*.v10* [all] datasets")
## print(g)
 
 

a$plus <- as.double(as.character(a$plus))
a$minus <- as.double(as.character(a$minus))
a$it <- a$plus/a$minus
a <- a[a$it < 6, ]
# g <- ggplot(data = a[grep("loo", a$mf, fixed=TRUE, invert=TRUE), ]) + aes(y = f2, x = it, color=mf)  +
g <- ggplot(data = a) + aes(y = f2, x = it, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("f2") +
     ggtitle("numeric scale for P/M")
print(g)


just1.1 <- a[a$pm == "1-1", ]
g <- ggplot(data = just1.1) + aes(y = f2, x = mf)  +
     geom_point() +
     geom_line(aes(group=1)) +
     ggtitle("1-1 data only")
print(g)


x <- dev.off()

