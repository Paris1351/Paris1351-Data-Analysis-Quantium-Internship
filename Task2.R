library(data.table)
library(ggplot2)
library(tidyr)
library(ggmosaic)
library(readr)
#### Creating local dataset
filePath <- "D:/Parisa/Internship/Quantium/"
data <- fread(paste0(filePath,"QVI_s=data.csv"))