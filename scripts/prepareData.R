library(dplyr)
library(lubridate)
library(data.table)

accidentData <- read.csv("data/raw/Accident_Information.csv", stringsAsFactors = FALSE)

print(dim(accidentData))

print((str(accidentData)))

print((summary(accidentData)))

print((colnames(accidentData)))

head(accidentData)