# Mostapha A
# CST8233 
# Assignment 1 Part 1

# Choose a file to import, separate columns with ";" symbol, print first 6 rows
myCarsList <- read.csv(file.choose(), sep = ';', stringsAsFactors = FALSE)
myCarsList[1:6,]

# delete first row, print the number of rows and columns
myCarsList <- myCarsList[-c(1),]
print(paste("The number of rows is", nrow(myCarsList), "and the number of columns is", ncol(myCarsList)))

# check the column types, change each one then check again
sapply(myCarsList, class)
myCarsList$MPG <- as.numeric(myCarsList$MPG)
myCarsList$Cylinders <- as.numeric(myCarsList$Cylinders)
myCarsList$Displacement <- as.numeric(myCarsList$Displacement)
myCarsList$Horsepower <- as.numeric(myCarsList$Horsepower)
myCarsList$Weight <- as.numeric(myCarsList$Weight)
myCarsList$Acceleration <- as.numeric(myCarsList$Acceleration)
myCarsList$Model <- as.numeric(myCarsList$Model)
myCarsList$Origin <- as.factor(myCarsList$Origin)
sapply(myCarsList, class)

# find cars with specific attributes, print MPG and export horsepower and acceleration
print(paste("The car with the maximum MPG is", myCarsList[which(myCarsList$MPG == max(myCarsList$MPG, na.rm=TRUE)),1]))
write.csv(myCarsList[which(myCarsList$Horsepower > 100),], "Horsepower.csv")
write.csv(myCarsList[which(myCarsList$Acceleration < 15),], "Acceleration.csv")

# create a histogram
hist(myCarsList[myCarsList$MPG != 0,2], main = "MPG Histogram",xlab = "MPG",xlim= c(0,50),breaks = 10,col = "red")
