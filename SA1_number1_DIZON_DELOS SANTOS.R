calculate_defective_probability <- function() {
  

  X1 <- as.numeric(readline(prompt="Enter the production rate of factory 1 (X1): "))
  X2 <- as.numeric(readline(prompt="Enter the production rate of factory 2 (X2): "))
  X3 <- as.numeric(readline(prompt="Enter the production rate of factory 3 (X3): "))
  
 
  if (X1 < 0.10 || X1 > 0.40 || X2 < 0.10 || X2 > 0.40 || X3 < 0.10 || X3 > 0.40 || X1 + X2 + X3 != 1) {
    cat("Invalid input for X values. Production rates should be between 10% and 40% and sum should be 1.\n")
    return(NULL)
  }
 
  Y1 <- as.numeric(readline(prompt="Enter the defective rate of factory 1 (Y1): "))
  Y2 <- as.numeric(readline(prompt="Enter the defective rate of factory 2 (Y2): "))
  Y3 <- as.numeric(readline(prompt="Enter the defective rate of factory 3 (Y3): "))
  
 
  if (Y1 < 0.01 || Y1 > 0.05 || Y2 < 0.01 || Y2 > 0.05 || Y3 < 0.01 || Y3 > 0.05 || Y1 + Y2 + Y3 != 0.12) {
    cat("Invalid input for Y values. Defective rates should be between 1% and 5% and sum should be 0.12.\n")
    return(NULL)
  }
  
 
  probability_defective <- X1 * Y1 + X2 * Y2 + X3 * Y3
  cat("The probability that a randomly selected product is defective is:", probability_defective, "\n")
}


calculate_defective_probability()
