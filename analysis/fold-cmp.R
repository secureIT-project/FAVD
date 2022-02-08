#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# this script looks at the aggregate data per fold.
# it compares two data sets.
# initially it was used to compare the 5-fold and 10-fold data.
# because there is no difference, the rest of the analysis uses 5 folds.
#
# run with
#   fold-cmp.R 5-fold.csv 10-fold.csv

options(digits=3, width=128) 

printf <- function(...) cat(sprintf(...))

pp <- function(pval)
{
  return ( ifelse(pval < 0.0001, "< 0.0001", sprintf("  %6.4f", pval)))
}

xttest <- function(d, name=NULL)
{
  prog = d$it[[1]]
  if (length(unique(d$f)) != 2) {
    printf("%s fails to have exactly two 'f' values :( (found %s)\n", prog, unique(d$f))
  } else {
    tt <- t.test(d$f2 ~ d$f) # , paired=TRUE)
    printf("%-16.16s   %10.10s   %d\n", prog, pp(tt[["p.value"]]), nrow(d))
  } 
}


args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2)  stop("usage: fold-cmp.R 5-fold.csv 10-fold.csv")
d5 <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#", stringsAsFactors=FALSE)
d10 <- read.csv(args[2], header=TRUE, row.names=NULL, comment="#", stringsAsFactors=FALSE)

d5$f <- "file1"
d10$f <- "file2"
d <- rbind(d5, d10)

names(d)[names(d) == "predicted"] <- "f2"
d <- d[!grepl("b100", d$program), ]  # omit b100k and b10k data
d$program <- factor(d$program, levels = c("Asterisk", "FFmpeg", "LibPNG",
                               "LibTIFF", "Pidgin", "VLC", "loo", "VDISC"))

d$f <- as.factor(d$f)
d$filter[d$filter == "-999"] <- "all"
d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0
d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$filter <- as.factor(d$filter)
d$model <- as.factor(d$model)
d$it <- as.factor(paste0(d$program, "-", d$model, "-", d$filter))

cat("\n*** until the data is complete this script ignores some bits (see script)  ***\n\n")
d <- d[!d$filter == 1.00,]
d <- d[!d$filter == 0.05,]
d <- d[!(d$model == "m" & d$filter == "all"), ]  # silently omitting  m-all :)
d <- d[d$it != "VDISC-c-all", ]

# foreach model
#   foreach program 
#     foreach filter
#       compare 5-fold with 10-fold
 
# should be 15 points for all but loo where their are 12 and vdisc which has 10
c.only <- d[d$model == "c", ]
c.only <- d
cat("program               p-value   n\n")
x <- by(c.only, c.only$it, FUN=xttest)    # run t-test foreach value of mf 


