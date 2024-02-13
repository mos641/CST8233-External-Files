# Mostapha A
# CST8233 
# Assignment 1 Part 3

# create a function that converts a decimal number to IEEE-754 binary representation
decToIEEE <- function(number){
  # define variables
  ogNum <- number
  sign <- 0
  mantissa <- ""
  exponent <- 0
  exponentBin <- ""
  decimal <- 0
  
  # change the sign if negative
  if (number < 0){
    sign <- 1
    number <- number * -1
  }
  
  # split the number into whole number portion and decimal portion
  if ((number %% 2) != 1 || (number %% 2) != 0){
    decimal <- number %% 2
    if (decimal > 1){
      decimal <- decimal - 1
    }
    # remove decimal from number
    number <- number - decimal
  }
  
  # loop to convert decimal to binary - first find the whole number portion
  remainder <- 0
  result <- number
  while(result != 0){
    # divide the number by 2 and store remainders to find binary number
    remainder <- result %% 2
    result <- (result - remainder) / 2
    
    # add to the mantissa if it is 1 or 0
    if (remainder == 1){
      mantissa <- paste("1", mantissa)
    } else {
      mantissa <- paste("0", mantissa)
    }
    
    # exponent (small e) will be how many times we iterated through the loop
    if (result != 0){
      exponent <- exponent + 1
    }
  }
  
  # remove first digit
  mantissa <- gsub(" ", "", mantissa)
  if (exponent > 0){
    mantissa <- sub('.', '', mantissa)
  }
  
  # loop to convert decimal to binary - find decimal portion
  result <- decimal
  while(result != 0){
    # multiply exponent by 2
    result <- result * 2
    
    # add to the mantissa if it is 1 or 0
    if (result >= 1){
      # if it is one or greater add 1 to mantissa and subtract from result
      mantissa <- paste(mantissa, "1")
      result <- result - 1
    } else {
      # otherwise add 0 to mantissa
      mantissa <- paste(mantissa, "0")
    }
    
    # if exponent is less than 1, exponent (small e) will be how many times we iterated through the loop but negative
    if (exponent < 1 && result != 0){
      exponent <- exponent - 1
    }
  }
 
  # remove first digit if num was less than 1
  mantissa <- gsub(" ", "", mantissa)
  if (exponent < 0){
    mantissa <- sub('.', '', mantissa)
  }
  
  # remove spaces on mantissa, add 0s to fill in bits
  mantissa <- gsub(" ", "", mantissa)
  i <- nchar(mantissa)
  while (i < 23){
    mantissa <- paste(mantissa, "0")
    i <- (i + 1)
  }
  mantissa <- gsub(" ", "", mantissa)
  
  # convert exponent (small e) to IEEE representation (big E)
  remainder <- 0
  exponentBin <- ""
  result <- (exponent + 127)
  while(result != 0){
    # divide the number by 2 and store remainders to find binary number
    remainder <- result %% 2
    result <- (result - remainder) / 2
    # add to the exponent (big E) if it is 1 or 0
    if (remainder == 1){
      exponentBin <- paste("1", exponentBin)
    } else {
      exponentBin <- paste("0", exponentBin)
    }
    
  }
  
  # remove spaces on Exponent, add 0s to fill in bits if necessary
  exponentBin <- gsub(" ", "", exponentBin)
  i <- nchar(exponentBin)
  print(nchar(exponentBin))
  while (i < 8){
    exponentBin <- paste("0", exponentBin)
    i <- (i + 1)
  }
  exponentBin <- gsub(" ", "", exponentBin)
  
  # print information
  print(paste("Input:", ogNum,"IEEE-754, 32-bit floating point number representation:"), quote = FALSE)
  print(paste("Sign =", sign), quote = FALSE)
  print(paste("Mantissa (23 bits) =", mantissa), quote = FALSE)
  print(paste("Exponent (8 bits) =", exponentBin), quote = FALSE)
}

decToIEEE(-103.5)
