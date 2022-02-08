#!/usr/bin/env Rscript
# SPDX-Package: FAVD
# SPDX-PackageHomePage: https://github.com/secureIT-project/FAVD
# SPDX-License-Identifier: MIT
# SPDX-FileCopyrightText: 2021-2022 David Binkley, Leon Moonen, Sibren Isaacman

# analysis of the detailed data from each program/data-set (csv file)
# most important graphs are the ROC and precision-recall graphs
#
# using raw output from CrossValidation.py or LeaveOneOut.py ./separate does
#  grep graph $argv[1] | sed -e 's/graph//' -e 's/##//' -e 's/ //g' > ! $argv[1]:r.csv
#  grep -v graph $argv[1] > !  $argv[1]:r.stats
# individual.R <separated-file.csv>


library(ggplot2)
# library(DescTools)
# library(agricolae)
# library("scatterplot3d") 
# library(plotly)

options(digits=3, width=128) 

printf <- function(...) cat(sprintf(...))


graphit <- function (name, d, id)
{
  # name <- ""   # for production
  n <- if (grepl("-999", name, fixed = TRUE)) "F-all" else "F-0"
  # alas this does not work for binkley :(   n <- gsub("-", "\U2013", n)
  name <- gsub("\\..*", "", gsub(".*/", "", name))
  # scatterplot3d(x=d$threshold, z=d$f2, y=d$cutoff)
  name <- paste(name, n)

  # fig <- plot_ly(x=d$threshold, z=d$f2, y=d$cutoff)
  # fig <- fig %>% add_surface()
  # print(fig)

  # print(summary(d$cutoff))
  # print(summary(d$threshold))
  a <- aggregate(list(f2=d$f2, p=d$p, r=d$r, tpr=d$tpr, fpr=d$fpr), by=list(d$threshold, d$cutoff), FUN=mean)
  colnames(a) <- c("threshold", "cutoff", "f2", "p", "r", "tpr", "fpr")

  # for each cutoff select the threshold(s) that maximize f2
  m <- do.call("rbind",lapply(split(d, d$cutoff), function(x) x[which(x$f2 == max(x$f2)), ]))
  mmin <- do.call("rbind",lapply(split(m, d$cutoff+d$f2), function(x) x[which(x$threshold == min(x$threshold)), ]))

  mt <- do.call("rbind",lapply(split(d, d$threshold), function(x) x[which(x$f2 == max(x$f2)), ]))

  options(scipen=10000)  # This works by setting a higher penalty for deciding to use scientific notation. 
  
if (FALSE) 
{
  g <- ggplot(data = d) + aes(y = f2, x = threshold, color=cutoff)  +
       geom_point(aes(color = cutoff, shape=fold) ) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       geom_vline(xintercept=0.10) +     # min an max thresholds of interest
       geom_vline(xintercept=0.40) +
       ggtitle(paste(name, "Performance by fold"))
  print(g)
  
  g <- ggplot(data = a) + aes(y = f2, x = threshold, color=cutoff)  +
       geom_point() +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       geom_vline(xintercept=0.10) +     # min an max thresholds of interest
       geom_vline(xintercept=0.40) +
       ggtitle(paste(name, "Mean performance over all folds"))
  print(g)

  g <- ggplot(data = m) + aes(y = f2, x = threshold, color=cutoff)  +
       geom_point(aes(color = cutoff, shape=fold) ) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Threshold maximizing each cutoff"))
  print(g)

  g <- ggplot(data = mt) + aes(y = f2, x = cutoff, color=threshold)  +
       geom_point(aes(color = threshold)) + # , shape=fold) ) +
       # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "cutoff maximizing each threshold then max(f2)"))
  print(g)

  g <- ggplot(data = m) + aes(y = cutoff, x = threshold, color=f2)  +
       geom_point(aes(color = f2, shape=fold) ) +
       # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       coord_cartesian(xlim = c(0.0, 1.0)) +
       geom_vline(xintercept=0.10) +     # min an max thresholds of interest
       geom_vline(xintercept=0.35) +
       ggtitle(paste(name, "Threshold maximizing each cutoff"))
  print(g)
  
  g <- ggplot(data = mmin) + aes(y = f2, x = threshold, color=cutoff)  +
       geom_point(aes(color = cutoff, shape=fold) ) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Threshold maximizing each cutoff"))
  print(g)

  g <- ggplot(data = mmin) + aes(y = cutoff, x = threshold, color=f2)  +
       geom_point(aes(color = f2, shape=fold) ) +
       # coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       coord_cartesian(xlim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Threshold maximizing each cutoff"))
  print(g)

  g <- ggplot(data = d) + aes(y = f2, x = cutoff, color=threshold)  +
       geom_point(aes(color = threshold, shape=fold) ) +
       coord_cartesian( ylim = c(0.0, 0.30)) +
       xlab("cutoff") +
       ylab("F2") +
       ggtitle(paste(name, "threshold maximizing each cutoff"))
  print(g)
}
if (FALSE) 
{

  g <- ggplot(data = a) + aes(y = p, x = cutoff, color=threshold)  +
       # geom_point(aes(color = threshold, shape=fold) ) +  # for d$
       geom_point(aes(color = threshold) ) +
       # coord_cartesian( ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Precision - threshold"))
  print(g)

  g <- ggplot(data = a) + aes(y = p, x = threshold, color=cutoff)  +
       # geom_point(aes(color = cutoff, shape=fold) ) +  # for d$
       geom_point(aes(color = cutoff) ) +
       guides(color=guide_legend(title="cutoff")) +
       # coord_cartesian( ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Precision - cutoff"))
  print(g)
  
  
  g <- ggplot(data = a) + aes(y = r, x = cutoff, color=threshold)  +
       # geom_point(aes(color = threshold, shape=fold) ) +
       geom_point(aes(color = threshold) ) +
       # coord_cartesian( ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Recall "))
  print(g)

  g <- ggplot(data = a) + aes(y = r, x = threshold, color=cutoff)  +
       # geom_point(aes(color = cutoff, shape=fold) ) +
       geom_point(aes(color = cutoff) ) +
       guides(color=guide_legend(title="cutoff")) +
       # coord_cartesian( ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "Recall by threshold"))
  print(g)
  
}
#   g <- ggplot(data = a) + aes(y = r, x = cutoff, color=threshold)  +
#        # geom_point(aes(color = threshold, shape=fold) ) +
#        geom_point(aes(color = threshold) ) +
#        # coord_cartesian( ylim = c(0.0, 1.0)) +
#        geom_point(y=a$p, x=a$cutoff,colour='red',aes(color = a$threshold, shape=a$fold) ) +
#        ggtitle(paste(name, "threshold maximizing each cutoff"))
#   print(g)

if (TRUE)  
{
#  g <- ggplot(data = d) + aes(y = tpr, x = fpr, color=threshold)  +
#        # geom_point(aes(color = threshold) ) +
#        geom_point(aes(color = threshold, shape=fold) ) +
#        geom_abline(intercept = 0, slope = 1) +
#        ggtitle(paste(name, "ROC Curves"))
#   print(g)

  g <- ggplot(data = a) + aes(y = tpr, x = fpr, color=cutoff)  +
       # geom_point(aes(color = cutoff) ) +
       geom_point(aes(color = cutoff) ) +
       geom_abline(intercept = 0, slope = 1) +
       ggtitle(paste(name, "ROC Curves"))
  print(g)

samples <- 9
Ks <- unique(a$cutoff)
step <- length(Ks)/samples
indexes <- c(1, as.integer(1+1:(samples-1)*step), length(Ks))
# indexes <- indexes[c(2)]  
# print(Ks[indexes])
sample <- a[a$cutoff %in% Ks[indexes], ]
sample$cutoff <- as.factor(sample$cutoff)
# sample$tpr <- as.integer(sample$tpr*1000)
# print(sample)

# sample <- sample[sample$threshold < 0.10, ]
  g <- ggplot(data = sample) + aes(y = tpr, x = fpr, color=cutoff)  +
       ## theme(aspect.ratio=1) +            # force 45 degree line to be 45 degrees
       geom_point(aes(color = cutoff, shape=cutoff)) + # ,show.legend = FALSE) +
       geom_line(aes(color = cutoff)) + # ,show.legend = FALSE) +
       geom_abline(intercept = 0, slope = 1) +
       # scale_x_continuous(trans='log2') +  # to zoom in on small values
       xlab("false-positive rate") +
       ylab("true-positive rate") +
       scale_shape_manual(values=1:length(indexes)) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste(id, name, "ROC Curves (10 sample cutoffs)"))
  print(g)

a$cutoff <- as.factor(a$cutoff)
  g <- ggplot(data = a) + aes(y = tpr, x = fpr, color=cutoff)  +
       geom_point(aes(color = cutoff)) + # for sabbatical , show.legend = FALSE) +
       geom_line(aes(color = cutoff)) + # for sabbatical , show.legend = FALSE) +
       geom_abline(intercept = 0, slope = 1) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       xlab("false-positive rate") +
       ylab("true-positive rate") +
       # labs(fill = "length (words)") + 
       # guides(color=guide_legend(title="length (words)")) +
       guides(color=guide_legend(title="cutoff")) +
       # ggtitle(paste("(a) ", name, "ROC Curves")) ## for sabbatical proposal
       ggtitle(paste(name, "ROC Curves "))
  print(g)

a$threshold = as.factor(a$threshold)
  g <- ggplot(data = a) + aes(y = tpr, x = fpr, color=threshold)  +
       geom_point(aes(color = threshold), show.legend = FALSE) +
       geom_line(aes(color = threshold))+ #, show.legend = FALSE) +
       geom_abline(intercept = 0, slope = 1) +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste(name, "ROC Curves"))
  print(g)

# print(unique(a$threshold))
  aa <- a[a$threshold %in% c("0", "0.25", "0.5", "0.75", "0.99"), ]
  g <- ggplot(data = aa) + aes(y = tpr, x = fpr, color=threshold)  +
       geom_point(aes(color = threshold), show.legend = FALSE) +
       geom_line(aes(color = threshold))+ #, show.legend = FALSE) +
       geom_abline(intercept = 0, slope = 1) +
       xlab("false-positive rate") +
       ylab("true-positive rate") +
       coord_cartesian(xlim = c(0.0, 1.0), ylim = c(0.0, 1.0)) +
       ggtitle(paste("(b) ", name, "ROC Curves"))
  print(g)


}
if (FALSE)  
{
  g <- ggplot(data = d) + aes(y = p, x = r, color=threshold)  +
       geom_point(aes(color = cutoff, shape=fold) ) +
       geom_point(colour='red', aes(color = threshold, shape=fold) ) +
       ggtitle(paste(name, "Precision Recall "))
  print(g)
  

sample$threshold = as.factor(sample$threshold)
  g <- ggplot(data = sample) + aes(y = p, x = r, color=threshold)  +
       geom_point(aes(color = cutoff, shape=cutoff) ) +
       # geom_point(aes(color = threshold, shape=cutoff) ) +
       ggtitle(paste(name, "Precision Recall"))
  print(g)
}
  
  g <- ggplot(data = a) + aes(y = p, x = r, color=cutoff)  +
       geom_point(aes(color = cutoff)) + # , show.legend = FALSE) +
       xlab("Recall") +
       ylab("Precision") +
       ggtitle(paste("(d) ", name, "Precision Recall (by cutoff)"))
  print(g)
  
  
  # shows a bump up for the unbelieving
  # x <- d[d$cutoff == 118, ]
  # g <- ggplot(data = x) + aes(y = f2, x = threshold)  +
  #      geom_point() +
  #      ggtitle(name)
  # print(g)
  
}
  

doit <- function(name, aggregate, id)
{
  printf("graphing %s\n", name)
  d <- read.csv(name, header=TRUE, row.names=NULL, comment="#")
  d <- d[d$f2 !=  0, ]
  if (nrow(d) == 0) {
    cat("\noops ... no non-zero f2 scores :(\n\n")
    return (aggregate)
  }

  names(d)[names(d) == 'K'] <- 'cutoff'

  ## for the post 14 june data that gets presented shift this back
  ## d$threshold = d$threshold + 0.01   # account for <= in python code outputting 0.00 and -0.01
  d$acc <- (d$tp+d$tn)/(d$tp+d$fp+d$fn+d$tn)
  d$p <- d$tp / (d$tp + d$fp)
  d$r <- d$tp / (d$tp + d$fn)
  d$tpr <- d$tp / (d$tp + d$fn)
  d$fpr <- d$fp / (d$fp + d$tn)

  cat("marker for code to suppress lines to 1,1\n")
  d <- d[!(d$tpr == 1.0 & d$fpr == 1.0), ]

  # cat(name)
  # cat(" - f2\n")
  # print(summary(d$f2))

  ## aa <- aggregate(d$f2, by=list(d$cutoff), FUN=mean)
  ## colnames(aa) <- c("cutoff", "f2")
  ## aa <- aa[order(-aa$f2),]

  d$fold <- as.factor(d$fold)
  graphit(name, d, id)
  return (if (is.null(aggregate))  d else rbind(aggregate, d))
}


args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1)  stop("usage: plot.R <Dangerous_words output cvs files (see script)>")

pdf("graphs.pdf", 10, 5) 
theme_set(theme_gray(base_size = 16))

id <- ""
if (nchar(args[1]) == 1) {
  id <- paste0("(", args[1], ")")
  args <- args[-1]
}

# when comp
aggregate <- NULL
for (filename in args) 
  aggregate <- doit(filename, aggregate, id)

# if (length(args) > 1) graphit("aggregate", aggregate, id)
cat("warning: aggregate graph suppressed\n")

x <- dev.off()

# warnings()
