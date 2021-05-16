
#UCI Statlog (German Credit Data) Data Set
#https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data
#https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric

read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric")
read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

german_data <-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data-numeric")
german_data_original <-read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/statlog/german/german.data")

class(german_data)
summary(german_data)
str(german_data)