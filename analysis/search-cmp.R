#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# compare "faster" binary search with slow search
#
# run with
#   search-cmp.R fast.csv slow.csv

first <- "fast"
second <- "slow"


options(digits=3, width=128) 

library(agricolae)
library(ggplot2)

printf <- function(...) cat(sprintf(...))


args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2)  stop("usage: s.R <AD summary output cvs file>")
f <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")
s <- read.csv(args[2], header=TRUE, row.names=NULL, comment="#")


f$ds <- first
s$ds <- second
d <- rbind(f, s)

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set

names(d)[names(d) == "predicted"] <- "f2"

d$filter[d$filter == "-999"] <- "all"
d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0

d <- d[d$filter != 1.00, ]

d <- d[!grepl("b1000", d$program), ]  # omit VDISC proxy
d$program <- factor(d$program, levels = c("Asterisk", "FFmpeg", "LibPNG", "LibTIFF", "Pidgin", "VLC", "loo", "VDISC"))


d$ds <- as.factor(d$ds)
d$fold <- as.factor(d$fold)
d$dwc <- round(d$dwc, digits=0)
# d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$model <- as.factor(d$model)
d$filter <- as.factor(d$filter)

d$pm <- factor(paste0(d$plus,"-",d$minus))
d$mf <- as.factor(paste0(d$model,"-",d$filter))
d$dmf <- as.factor(paste0(d$ds, "-", d$model,"-",d$filter))

a <- aggregate(list(f2=d$f2, best=d$best.test), by=list(d$ds, d$dmf, d$program), FUN=mean)
colnames(a) <- c("ds", "dmf", "program", "f2", "best")

a$it <- as.factor(paste0(a$program, "-", a$dmf, sep=""))


pp <- function(pval)
{
  return ( ifelse(pval < 0.0001, "< 0.0001", sprintf("  %6.4f", pval)))
}



foo <- function(x) {
  n <- nrow(x)
  n <- length(unique(x$ds))
  name <- x$program[[1]]
  mf <- x$mf[[1]]
  if (n <= 1) {
    # printf("%s only in one dataset\n", name)
  } else {
    m1 <- mean(x$f2[x$ds == first])
    m2 <- mean(x$f2[x$ds == second])
    p <- 100*(m2-m1)/m2

    tt <- t.test(x$f2 ~ x$dmf , paired=FALSE)
    printf("%-10.10s  %-8.8s %5.3f %5.3f  %5.2f%%  %8.8s   %d ", name, mf, m1, m2, p, pp(tt[["p.value"]]), nrow(x))

    p <- unique(x$program[x$ds == first ])
    xx <- x[x$program %in% p, ]
    tt <- t.test(xx$f2 ~ xx$dmf, paired=TRUE)
    printf(" %10.10s %d\n", pp(tt[["p.value"]]), nrow(xx))
  }
}

bar <- function(d) {
  x <- by(d, d$mf, foo)
}

# print(summary(d))
printf("\nprogram     mf     %6.6s%6.6s   diff     p-value  obs   paired p-value \n", first, second)
x <- by(d, d$program, bar)

# quick check :)
# d <- d[d$program %in% c("FFmpeg", "LibPNG", "Pidgin", "loo"), ]
cat("All data (ignore program; only meaningful when datasets include the same programs)\n")
x <- by(d, d$mf, foo)   # do all the data regardless of program



## aggregate over folds taking the mean of all folds for each 'program + model-filter'
a <- aggregate(list(f2=d$f2, AD=d$AD, dwc=d$dwc), by=list(d$program, d$dmf), FUN=mean)
colnames(a) <- c("program", "dmf", "f2", "all_dangerous", "dwc")

pdf("graphs.pdf", 10, 5)

g <- ggplot(data = a) + aes(y = f2, x = program, color=dmf)  +
     geom_point(aes(color = dmf, shape=dmf) ) +
     geom_line(aes(group=dmf, color = dmf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("All three datasets")
print(g)

aa <- a[grepl("all", a$dmf), ]  
g <- ggplot(data = aa) + aes(y = f2, x = program, color=dmf)  +
     geom_point(aes(color = dmf, shape=dmf) ) +
     geom_line(aes(group=dmf, color = dmf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("-all data only")
print(g)

ac <- a[grepl("-c-", a$dmf), ]
g <- ggplot(data = ac) + aes(y = f2, x = program, color=dmf)  +
     geom_point(aes(color = dmf, shape=dmf) ) +
     geom_line(aes(group=dmf, color = dmf) ) +
     ylab("F2") +
     xlab("Data Set") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="Model")) +
     guides(shape=guide_legend(title="Model")) +
     ggtitle("-c data only ")
print(g)
 

x <- dev.off()

