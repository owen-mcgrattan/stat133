# Title: Hw 04
# Description:
#   Clean the rawscores data for hw04
#
# Inputs:
#   rawscores.csv
# Outputs:
#   cleanscores.csv

# set proper working directory
setwd("~/stat133/stat133-hws-fall17/hw04/code")

# load in functions
source("../code/functions.R")
library(stringr)
# read in rawscores data
raw <- read.csv("../data/rawdata/rawscores.csv")

# sink structure of the dataframe
sink('../output/summary-rawscore.txt')
str(raw)

# summary stats
summary_stats(raw)

# print stats
print_stats(summary_stats(raw))
sink()


# data cleaning

# replace all NA values with 0
for(i in 1:nrow(raw)) {
  for(j in 1:ncol(raw)){
    if(is.na(raw[i,j])) {
      raw[i,j] <- 0
    }
  }
}

# rescale QZ1
raw$QZ1 <- rescale100(raw$QZ1, xmin = 0, xmax = 12)

# rescale QZ2
raw$QZ2 <- rescale100(raw$QZ2, xmin = 0, xmax = 18)

# rescale QZ3
raw$QZ3 <- rescale100(raw$QZ3, xmin = 0, xmax = 20)

# rescale QZ4
raw$QZ4 <- rescale100(raw$QZ4, xmin = 0, xmax = 20)

# rescale test1
raw$test1 <- rescale100(raw$EX1, xmin = 0, xmax = 80)

# rescale test2
raw$test2 <- rescale100(raw$EX2, xmin = 0, xmax = 90)

# add homework variable
for(i in 1:nrow(raw)){
  raw$homework[i] <- score_homework(raw[i,1:9], drop = TRUE)
}

# add in quiz variable
for(i in 1:nrow(raw)){
  raw$quiz[i] <- score_quiz(raw[i, 11:14], drop = TRUE)
}

# create overall score
raw$overall <- (raw$quiz * 0.1) + (raw$homework * 0.3) + 
  (raw$quiz * 0.15) + (raw$test1 * 0.2) + (raw$test2 * 0.25)

# calculate letter grade
for(i in 1:nrow(raw)){ 
  if (raw$overall[i] >= 95 & raw$overall[i] <= 100){
    raw$grade[i] <- "A+"
  } else if(raw$overall[i] < 95 & raw$overall[i] >= 90) {
    raw$grade[i] <- "A"
  } else if (raw$overall[i] < 90 & raw$overall[i] >= 88){
    raw$grade[i] <- "A-"
  } else if (raw$overall[i] < 88 & raw$overall[i] >= 86) {
    raw$grade[i] <- "B+"
  } else if (raw$overall[i] < 86 & raw$overall[i] >= 82) {
    raw$grade[i] <- "B"
  } else if (raw$overall[i] < 82 & raw$overall[i] >= 79.5) {
    raw$grade[i] <- "B-"
  } else if (raw$overall[i] < 79.5 & raw$overall[i] >= 77.5) {
    raw$grade[i] <- "C+"
  } else if (raw$overall[i] < 77.5 & raw$overall[i] >= 70) {
    raw$grade[i] <- "C"
  } else if (raw$overall[i] < 70 & raw$overall[i] >= 60) {
    raw$grade[i] <- "C-"
  } else if (raw$overal[i] < 60 & raw$overall[i] >= 50) {
    raw$grade[i] <- "D"
  } else {
    raw$grade[i] <- "F"
  }
}

# make grades factor
raw$grade <- factor(raw$grade,
                    levels = c("A+", "A", "A-", "B+", "B",
                               "B-", "C+", "C", "C-", "D", "F"))

# set names to be extracted
name <- names(raw)[17:21]
# for loop to extract summary stats

for (i in 17:21) {
  sink(paste("../output/", "-stats.txt", sep = colnames(raw)[i]))
  print(summary_stats(raw[,i]))
  print(print_stats(summary_stats(raw[,i])))
  sink()
}

# sink() structure of data frame
sink("../output/summary-cleanscores.txt")
str(raw)
sink()

# write cleandata.csv
write.csv(x = raw, file = "../data/cleandata/cleandata.csv")
