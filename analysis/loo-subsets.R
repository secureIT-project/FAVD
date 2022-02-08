#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# graph the aggregate performance of the various loo data set proposed at the 25 june zoom chat
#
# run with
#   extract data/loo*
#   ds-cmp-graph.R .data.csv

options(digits=3, width=128) 

library(agricolae)
library(ggplot2)


printf <- function(...) cat(sprintf(...))


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


args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1)  stop("usage: t.R extracted loo files [optional second file]")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")
if (length(args) > 1)  {
d2 <- read.csv(args[2], header=TRUE, row.names=NULL, comment="#")
d <- rbind(d, d2)
}

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set
d[d$model == "m",]$plus <- "x"   # subsequent code assumes -m data has plus == 'x'
d[d$model == "m",]$minus <- "x"  # just to be sure :)

names(d)[names(d) == "predicted"] <- "f2"
# d$f2 <- d$best.test   # generate data for best column

# keep only best for each 'm' and all 'c' data
d <- d[((d$filter %in% c(0.05, 0.90, 0.99) & d$model == "m") | d$model == "c"), ] 


d$filter[d$filter == "-999"] <- "all"
d$program <- factor(d$program)
d$filter <- as.factor(d$filter)
d$model <- as.factor(d$model)
d$dwc <- round(d$dwc, digits=0)
d$mf <- as.factor(paste0(d$model,"-",d$filter))


## aggregate over folds taking the mean of all folds for each 'program + model-filter'
apmf <- aggregate(list(f2=d$f2, AD=d$AD, dwc=d$dwc), by=list(d$program, d$mf), FUN=mean)
colnames(apmf) <- c("program", "mf", "f2", "AD", "dwc")

apm <- aggregate(list(f2=d$f2, AD=d$AD, dwc=d$dwc), by=list(d$program, d$model), FUN=mean)
colnames(apm) <- c("program", "model", "f2", "AD", "dwc")

npmf <- apmf[grepl("m-", apmf$mf), ]
npm <- apm[apm$model == "m", ]
cat("\nnpmf:\n")
print(npmf)
cat("\nnpm:\n")
print(npm)

foo <- function(d)
{
  p <- d["program"]
  mf2 <- npm[npm$program == p, ]$f2
  return (as.double(d["f2"]) / mf2)
}

bar <- function(d, ref)
{
  p <- d["program"]
  mf2 <- ref[ref$program == p, ]$f2
  return (as.double(d["f2"]) / mf2)
}

apmf$scaled_mean <- apply(apmf, FUN = foo, MARGIN=1)
apmf$scaled.005 <- apply(apmf, FUN = function(x) bar(x,  apmf[apmf$mf == "m-0.05", ]), MARGIN=1)
apmf$scaled.090 <- apply(apmf, FUN = function(x) bar(x,  apmf[apmf$mf == "m-0.9", ]), MARGIN=1)
apmf$scaled.099 <- apply(apmf, FUN = function(x) bar(x,  apmf[apmf$mf == "m-0.99", ]), MARGIN=1)

apmf$norm <- apmf$f2 / apmf$AD

cat("\napmf:\n")
print(apmf)


# cat("\n\n0.05\n")
# print(  apmf[apmf$mf == "m-0.05", ])
# cat("\n\n0.90\n")
# print(  apmf[apmf$mf == "m-0.9", ])
# cat("\n\n0.99\n")
# print(  apmf[apmf$mf == "m-0.99", ])




production <- TRUE
if (production) cat("\n*** omitting non-production graphs ***\n")
if (!production) cat("\n*** including non-production graphs ***\n")

pdf("graphs.pdf", 10, 5)

if (!production)
{
g <- ggplot(data = apmf) + aes(y = f2, x = program, color=mf, fill=mf)  +
     geom_bar(stat = "identity", position=position_dodge()) +
     ylab("F2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("Model Comparison")
print(g)

g <- ggplot(data = apm) + aes(y = f2, x = program, color=model, fill=model)  +
     geom_bar(stat = "identity", position=position_dodge()) +
     ylab("F2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("All datasets")
print(g)
}

g <- ggplot(data = apmf) + aes(y = f2, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("All datasets")
print(g)

g <- ggplot(data = apmf) + aes(y = scaled_mean, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("scaled mean")
print(g)


g <- ggplot(data = apmf[apmf$mf %in% c("c-0", "c-all", "m-0.05"), ]) + aes(y = scaled.005, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("scaled mean of 0.05")
print(g)

g <- ggplot(data = apmf[apmf$mf %in% c("c-0", "c-all", "m-0.9"), ]) + aes(y = scaled.090, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("scaled mean of 0.90")
print(g)

g <- ggplot(data = apmf[apmf$mf %in% c("c-0", "c-all", "m-0.99"), ]) + aes(y = scaled.099, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("scaled mean of 0.99")
print(g)

g <- ggplot(data = apmf) + aes(y = norm, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("Normalized to AD")
print(g)

if (FALSE && !production)
{
g <- ggplot(data = apm) + aes(y = f2, x = program, color=model)  +
     geom_point(aes(color = model, shape=model) ) +
     geom_line(aes(group=model, color = model) ) +
     ylab("F2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("All three datasets")
print(g)
}

x <- dev.off()

