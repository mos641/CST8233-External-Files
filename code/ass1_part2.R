# Mostapha A
# CST8233
# Assignment 1 Part 2

# create 2 vectors of random numbers
set.seed(75)
aVec <- sample.int(500, 50)
bVec <- sample.int(500, 50)

# subtract aVec frombVec and store
cVec <- c(aVec[1:49] - bVec[2:50])
cVec
cMat <- matrix(cVec, nrow = 7, ncol = 7, byrow = FALSE)
cMat

# create a vector that shows the result of cos(bVec[n]) / sin(aVec[n+1]) where n is 1 to 49
dVec <- vector("numeric", length = 49)
# loop through each value
i <- 1
while(i < 50){
  dVec[i] <- (cos(bVec[i])/sin(aVec[i+1]))
  i <- i + 1
}
# print
dVec

# calculate the sigma of (eulers constant to the power of -aVec[n]+1) / (aVec[n] + 5) where n is 1 to 49
abVecResult <- 0
i <- 1
while(i < 50){
  abVecResult <- abVecResult + ((exp(-aVec[i+1]))/(aVec[i]+5))
  i <- i + 1
}
#display
abVecResult