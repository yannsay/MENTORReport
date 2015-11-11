#####################################################
#Script to get the information about Diarrhea cases #
#####################################################

getDiarrhea <- function(x){

  #Creating Reporting values
  diarrheaPart <- c()
  totalDiarrhea <- 0
  
  #Dummy value for the selection of age. 0 for < 5 years old; 1 for >= 5 years old
  a <- 0 
  
  #Dummy value for the loop length (length of the age category without NA)
  NbCategory <- unique(categorieAge5[!is.na(categorieAge5)]) + 1
  
  #calculing number of diarrhea per category
  for (i in seq_along(NbCategory)) { 
    
    ageGroup <- x[which(x$categorieAge5 == a),]
    
    nbDiarrhea <- sum(ageGroup$diarrhee == 1, na.rm = TRUE)
    diarrheaPart <- c(diarrheaPart, nbDiarrhea)
    
    totalDiarrhea <- totalDiarrhea + nbDiarrhea
    
    a <- a +1
  }
  
  #Total of Diarrhea cases
  diarrheaPart <- c(diarrheaPart, totalDiarrhea)
  
  #Total of SRO distributed
  totalSRO <- 0
  
  dataSROA <- x[which(x$SRO < 77),] #1. Summing the SRO that have been written by number
  totalSROA <- sum(dataSROA$SRO, na.rm = TRUE) 
  
  dataSROB <- x[which(x$SRO == 77),]
  totalSROB <- nrow(dataSROB) * 2 #2. Looking for the 77s (SRO given but number given is unknown)
  
  totalSRO <- sum(totalSROA, totalSROB, na.rm = TRUE)
  diarrheaPart <- c(diarrheaPart, totalSRO)
  
  #Total Patients given SRO 
  totalPatientSRO <- sum(x$SRO > 0 & x$SRO < 88, na.rm = TRUE) #77 means SRO given but the number is unknown, 88 is the stockout code
  diarrheaPart <- c(diarrheaPart, totalPatientSRO)
  
  #Total Correct Diarrhea Treated
  totalDiarrheeTraiteCorrectement <- sum(x$diarrheeTraiteCorrectement, na.rm = TRUE)
  diarrheaPart <- c(diarrheaPart, totalDiarrheeTraiteCorrectement)
  
  #Percentage of patient with Diarrhea treated correctly
  percentageCorrectTreatedDiarrheaPatient <- totalDiarrheeTraiteCorrectement / totalDiarrhea * 100
  diarrheaPart <- c(diarrheaPart, percentageCorrectTreatedDiarrheaPatient)
  
  #Sending the Diarrhea information to the report line
  lineReport <<- c(lineReport, diarrheaPart)
  
}

namesDiarrhea <- c("Total Diarrhées pour < 5 ans", "Total Diarrhées pour > 5 ans", "Total Diarrhées", "Total SRO distribués", "Total Patients ayant reçu SRO", "Nb de patients avec diarrhée ayant reçu SRO", "% de patients avec diarrhée traités correctement")
