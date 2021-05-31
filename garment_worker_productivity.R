rm(list=ls())
library(tidyverse)


url <-"https://archive.ics.uci.edu/ml/machine-learning-databases/00597/garments_worker_productivity.csv"
garment <-read_csv(url)