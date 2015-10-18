####################################################
#Script to get the information about Malaria cases #
##############################################

getPalu <- function(x){
  #Creating Reporting values
  paluPart <- c()
  TotalTDR1 <- 0
  TotalTDR0 <- 0
  TotalTDR <- 0
  
  
  #Dummy value for the selection of age. 0 for < 5 years old; 1 for >= 5 years old
  a <- 0 
  
  #Dummy value for the loop length (length of the age category without NA)
  NbCategory <- unique(ageCategory5[!is.na(ageCategory5)])
  
  #calculing number of TDR+/- per category
  for (i in seq_along(NbCategory)) { 
    
    ageGroup <- x[which(x$ageCategory5 == a),]
    
    nbTDR1 <- sum(ageGroup$TDR == 1)
    paluPart <- c(paluPart, nbTDR1)
    
    nbTDR0 <-  sum(ageGroup$TDR == 0)
    paluPart <- c(paluPart, nbTDR0)
    
    nbTotalTDR <- nbTDR0 + nbTDR1
    TotalTDR1 <- TotalTDR1 + nbTDR1
    TotalTDR0 <- TotalTDR0 + nbTDR0
    TotalTDR <- TotalTDR + nbTotalTDR
    
    a <- a +1
  }
  
  #Total of TDR
  paluPart <- c(paluPart, TotalTDR1, TotalTDR0, TotalTDR)

  ## Confirming symptoms of severe malaria and TDR +
  totalPaluSevere <- sum(x$paluSevere)
  
  paluPart <- c(paluPart, totalPaluSevere)
  
  #Sending the Malaria information to the report line
  lineReport <<- c(lineReport, paluPart)
}

namesPalu <- c("Total TDR + pour < 5 ans", "Total TDR - pour < 5 ans", "Total TDR + pour > 5 ans", "Total TDR - pour > 5 ans", "Total TDR +", "Total TDR -", "Total TDR", "Total de patients présentant signes de paludisme sévère")
