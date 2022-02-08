#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# for the -m (DNN model) data only (i.e., ignore the plus/minus columns used
# with the -c (counting) data.
#
# runs Tukey and draw graphs comparing the f2 values for the data sets loo,
# within-program, and VDISC.  graphs include the all-dangerous reference lines
# for each data set
#
# run with
#   extract data/*-m*
#   rq2-m-only.R .data.csv


options(digits=3, width=128) 


library(ggplot2)
library(agricolae)

## library(scales)
## print( scales::hue_pal()(6))
## "#F8766D" "#B79F00" "#00BA38" "#00BFC4" "#619CFF" "#F564E3"


printf <- function(...) cat(sprintf(...))

pp <- function(pval)
{
  return ( ifelse(pval < 0.0001, "< 0.0001", sprintf("  %6.4f", pval)))
}

ttest <- function(d, name=NULL)
{
  tt <- t.test(d$f2, d$AD, paired=TRUE)
  m <- ifelse(is.null(name), as.character(d$dataset[[1]]), name)
  printf("%-12.12s     %5.3f  %5.3f   %s   %d\n", m, mean(d$f2), mean(d$AD), pp(tt[["p.value"]]), nrow(d))
}



# program,model,filter,plus,minus,AD,AD train,AD test,best train,predicted,best test,fold,dwc

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1)  stop("usage: s.R <AD summary output cvs file>")
d <- read.csv(args[1], header=TRUE, row.names=NULL, comment="#") # , stringsAsFactors = FALSE)

# x <- read.csv(args[2], header=TRUE, row.names=NULL, comment="#", stringsAsFactors = FALSE)
# d <- rbind(d, x)
# d$program <- as.factor(d$program)
# d$model <- as.factor(d$model)
# d$plus <- as.factor(d$plus)
# d$minus <- as.factor(d$minus)
# d$fold <- as.factor(d$fold)

names(d)[names(d) == "predicted"] <- "f2"

n <- nrow(d[d$model != "m", ])
if (n > 0) printf("\n** Warning dropping %d non-m rows\n\n", n)
d <- d[d$model == "m", ]

d$AD <- d$AD.test

cat("*** as per preliminary comparisons, omitting 1.6% data set***\n\n")
d <- d[!grepl("v16", d$program), ]
cat("*** as VDISC data exists omit b10k and b100k ***\n\n")
d <- d[!grepl("b1000", d$program), ]

## unused post 18 june data
## hack! adjust VDISC's dwc when using the 1% data to approximation the 10% data
## each 10% data data file takes a loooong time to generate :)
d$dwc <- ifelse(grepl("b10000.v", d$program), d$dwc*10, d$dwc)

d$filter <- as.numeric(d$filter)

# excluding these two lines includes details from between 0.90 and 1.00
d <- d[!d$filter %in% c("0.985", "0.92", "0.94", "0.96", "0.995", "0.999", "0.9995"), ]
d$filter <- sprintf("%4.2f", d$filter)

d$dataset <- factor(
  ifelse(d$program == "loo", 
         "loo", 
          ifelse(d$program == "VDISC", 
		         "VDISC", 
                 ifelse(grepl("b100", d$program), "b100k", "within"))))
d$filter <- as.factor(d$filter)

ax <- aggregate(list(f2=d$f2,dwc=d$dwc, AD=d$AD), by=list(d$program,d$filter), FUN=mean)
colnames(ax) <- c("program", "filter", "f2", "dwc", "AD")
xx <- ax[ax$filter == "0.00", ]$f2
yy <- ax[ax$filter == "0.05", ]$f2
tt <- t.test(xx, yy, paired=TRUE)
printf("in future research questions, no need to include 0.00 and 0.05:\n")
printf("  mean filter == 0.00 -> %5.3f mean filter == 0.05 -> %5.3f,  p = %5.4f\n\n", mean(xx), mean(yy), tt$p.value)


## highest of 0.05 is is came group as 0.00 so stay with three levels rather then adding 0.05
## z <- d[grepl("b100", d$program),]
## z <- z[!z$filter == "1.00", ]
## x <- aov(z$f2 ~ z$filter )
## print(summary(x))
## print(HSD.test(x, "z$filter", group=TRUE)$groups)
## stop('a')


x <- aov(d$f2 ~ d$dataset+d$filter )
print(summary(x))
print(HSD.test(x, "d$dataset", group=TRUE)$groups)
print(HSD.test(x, "d$filter", group=TRUE)$groups)

a <- aggregate(list(f2=d$f2,dwc=d$dwc, AD=d$AD), by=list(d$filter,d$dataset), FUN=mean)
colnames(a) <- c("filter", "dataset", "f2", "dwc", "AD")
# a$filter <- as.numeric(as.character(a$filter))   # without x-axis is categoric

aa <- a[!a$filter == "1.00", ]

cat("\nmins\n")
do.call(rbind, lapply(split(aa,as.factor(aa$dataset)), function(x) {return(x[which.min(x$f2),])}))

cat("\nmaxes\n")
do.call(rbind, lapply(split(aa,as.factor(aa$dataset)), function(x) {return(x[which.max(x$f2),])}))

cat("\n\n p-values comparing f2 with `all dangerous' ***: \n\n")
cat("data subset        f2     AD     p-value    n\n")
x <- by(a, a$dataset, FUN=ttest)    # run t-test foreach value of mf
cat("\n\nwithout x=1.00, p-values comparing f2 with `all dangerous' ***: \n\n")
cat("data subset        f2     AD     p-value    n\n")
x <- by(aa, aa$dataset, FUN=ttest)    # run t-test foreach value of mf


theme_set(theme_gray(base_size = 18))

pdf("graphs.pdf", 10, 5) 

g <- ggplot(data = a, aes(y = f2, x = filter, color=dataset))  +
     geom_point() +
     geom_line(aes(group = dataset )) +
     # geom_line(data = a, aes(x = filter, y = AD, shape = dataset, group=1), color="red") +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     xlab("minimum score") + 
     ylab("f2") +
     guides(fill=guide_legend(title="dataset")) +
     ggtitle("DNN Model Prediction of Dangerous Words")
print(g)

library(reshape2)
x <- melt(data = a, id.vars = c("filter", "dataset"), measure.vars = c("f2", "AD"))
print(summary(x))
x$variable <- as.character(x$variable)  # likely no need given lab1....
x$variable[x$variable == "f2"] <- "M_L"
x$variable <- as.factor(x$variable)
lv <- interaction(x$dataset, x$variable) 
lab1 <- c(expression('VDISC.all-vuln'),
          expression('loo.all-vuln'),
          expression('within.all-vuln'),
          expression(VDISC.R['L']),
          expression(loo.R['L']),
          expression(wthin.R['L']))
# title <- c(expression(R['L']), "Dangerous Words Performance")
g <- ggplot(data = x, aes(y = value, x = filter, color=lv, group=lv)) +
     geom_point(aes(shape=lv)) +
     geom_line() +
     coord_cartesian(ylim = c(0.0, 0.30)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     xlab("minimum score") + 
     ylab("f2") +
 scale_color_manual(values= c(scales::hue_pal()(3), scales::hue_pal()(3)), labels=lab1) +
  scale_shape_discrete(labels = lab1) +
  scale_linetype_discrete(labels = lab1) +
 # scale_color_manual(values= c("#F8766D","#B79F00","#00BA38","#F8766D","#B79F00","#00BA38" )) +
     guides(color=guide_legend(title="dataset . model")) +
     guides(shape=guide_legend(title="dataset . model")) +
     # ggtitle(expression(M['L'] Dangerous Words Performance)) #  (AD = all dangerous)")
     ggtitle("Dangerous Words Performance")
     # ggtitle("DNN Model Prediction of Dangerous Words (Av = all dangerous)")
print(g)

g <- ggplot(data = a, aes(y = dwc, x = filter, color=dataset))  +
     geom_point() +
     geom_line(aes(group = dataset )) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
     xlab("minimum score") + 
     scale_y_continuous(trans='log2') +
     ylab("# dangerous words (log)") +
     guides(color=guide_legend(title="dataset")) +
     ggtitle("Dangerous Words Count")
     # ggtitle("DNN Model Prediction of Dangerous Words")
print(g)

g <- ggplot(data = a, aes(y = f2, x = dwc, color=dataset))  +
     geom_point() +
     geom_line(aes(group = dataset )) +
     # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
     xlab("dangerous word count") + 
     ylab("f2") +
     guides(fill=guide_legend(title="dataset")) +
     ggtitle("DNN Model Prediction of Dangerous Words")
print(g)

x <- dev.off()

