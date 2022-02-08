#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# graph the aggregate performance of each data set


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
if (length(args) < 1)  stop("usage: rq3b.R 5-fold.csv 10-fold.csv")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#")
d$program <- as.character(d$program)
d$program <- as.factor(d$program)

d$AD <- d$AD.test  # AD.test is prediction for each fold rather than entire data set
d[d$model == "m",]$plus <- "x"   # subsequent code assumes -m data has plus == 'x'
d[d$model == "m",]$minus <- "x"  # just to be sure :)

names(d)[names(d) == "predicted"] <- "f2"
# d$f2 <- d$best.test   # draw for best column

d <- d[!grepl("b10000.v", d$program), ]  # omit 1% samples b10000.v160 and b10000.v720 
d <- d[!grepl("v16", d$program), ]  # omit 1% samples b10000.v160 and b10000.v720 
d <- d[!d$filter == 1.00,]    # cat("\n*** m-1 sucks and warps averages ... omit it! ***\n")
d <- d[!(d$model == "m" & d$filter == 0.00),]    # cat("\n*** omit -all as VDISC liked 0.05 better\n")


cat("\nomitting b100* now the VDISC data is available\n");
d <- d[!grepl("b100", d$program), ]  # omit 1% samples b10000.v160 and b10000.v720 

d$filter[d$filter == "-999"] <- "all"
d$filter[d$filter == 0.00 & d$model == "m"] <- "all"      #  m-*-0 -> m-all-0


d$program <- factor(d$program, levels = c("Asterisk", "FFmpeg", "LibPNG", "LibTIFF", "sPidgin", "Pidgin", "VLC", "loo", "b100000.v7200", "VDISC"))
d$filter <- as.factor(d$filter)
d$model <- as.factor(d$model)
d$dwc <- round(d$dwc, digits=0)
d$mf <- as.factor(paste0(d$model,"-",d$filter))

x <- d[! d$plus == "x",] 
x$plus <- as.double(as.character(x$plus))
x$minus <- as.double(as.character(x$minus))
x <- d[order(x$plus/x$minus),]
x <- paste0(x$plus,"-",x$minus)
l <- x[!duplicated(x)]

d$pm <- factor(paste0(d$plus,"-",d$minus), levels = l)




## aggregate over folds taking the mean of all folds for each 'program + model-filter'
a <- aggregate(list(f2=d$f2, AD=d$AD, dwc=d$dwc), by=list(d$program, d$mf), FUN=mean)
colnames(a) <- c("program", "mf", "f2", "all_dangerous", "dwc")

aa <- aggregate(list(f2=d$f2, AD=d$AD, dwc=d$dwc), by=list(d$program, d$model), FUN=mean)
colnames(aa) <- c("program", "model", "f2", "all_dangerous", "dwc")

production <- FALSE
if (production) cat("\n*** omitting non-production graphs ***\n")
if (!production) cat("\n*** including non-production graphs ***\n")


lab1 <- c("F-0",
          "F-all",
          "L-0.05",
          "L-0.90",
          "L-0.99")

colors <- c("#F8766D", "#CD9600", "#7CAE00", "#00BE67", "#00BFC4", "#00A9FF", "#C77CFF", "#FF61CC")


pdf("graphs.pdf", 10, 5)
theme_set(theme_gray(base_size = 16))

if (!production)
{
g <- ggplot(data = a) + aes(y = f2, x = program, color=mf, fill=mf)  +
     geom_bar(stat = "identity", position=position_dodge()) +
     ylab("f2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("Model Comparison")
print(g)

g <- ggplot(data = aa) + aes(y = f2, x = program, color=model, fill=model)  +
     geom_bar(stat = "identity", position=position_dodge()) +
     ylab("f2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("All three datasets")
     ggtitle("")
print(g)
}

g <- ggplot(data = a) + aes(y = f2, x = program, color=mf)  +
     geom_point(aes(color = mf, shape=mf) ) +
     geom_line(aes(group=mf, color = mf) ) +
     ylab("f2") +
     xlab("dataset") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     guides(color=guide_legend(title="model")) +
     guides(shape=guide_legend(title="model")) +
     scale_color_manual(values=colors, labels=lab1) +
     scale_shape_manual(values=1:5, labels=lab1) +

     ggtitle("")
     # ggtitle("All three datasets")
print(g)

if (!production)
{
g <- ggplot(data = aa) + aes(y = f2, x = program, color=model)  +
     geom_point(aes(color = model, shape=model) ) +
     geom_line(aes(group=model, color = model) ) +
     ylab("f2") +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     ggtitle("All three datasets")
print(g)
}

x <- dev.off()

