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
  NbCategory <- unique(ageCategory5[!is.na(ageCategory5)]) + 1
  
  #calculing number of TDR+/- per category
  for (i in seq_along(NbCategory)) { 
    
    ageGroup <- x[which(x$ageCategory5 == a),]
    
    nbDiarrhea <- sum(ageGroup$Diarrhee == 1)
    diarrheaPart <- c(diarrheaPart, nbDiarrhea)
    
    totalDiarrhea <- totalDiarrhea + nbDiarrhea
    
    a <- a +1
  }
  
  #Total of Diarrhea cases
  diarrheaPart <- c(diarrheaPart, totalDiarrhea)
  
  #Total of SRO distributed
  totalSRO <- 0
  
  dataSROA <- x[which(x$SRO < 77),] #1. Summing the Artenether that have been written by number
  totalSROA <- sum(dataSROA$SRO) 
  
  dataSROB <- x[which(x$SRO == 77),]
  totalSROB <- nrow(dataSROB) * 3 #2. Looking for the 77s (Artenether given but number given is unknown)
  
  totalSRO <- sum(totalSROA, totalSROB)
  diarrheaPart <- c(diarrheaPart, totalSRO)
  
  #Total Patients given SRO 
  totalPatientSRO <- sum(x$SRO > 0 & x$SRO < 88) #77 means SRO given but the number is unknown, 88 is the stockout code
  diarrheaPart <- c(diarrheaPart, totalPatientSRO)
  
  #Total Correct Diarrhea Treated
  totalCorrectDiarrheaTreated <- sum(x$correctDiarrheaTreated)
  diarrheaPart <- c(diarrheaPart, totalCorrectDiarrheaTreated)
  
  #Percentage of patient with Diarrhea treated correctly
  percentageCorrectTreatedDiarrheaPatient <- totalCorrectDiarrheaTreated / totalDiarrhea * 100
  diarrheaPart <- c(diarrheaPart, percentageCorrectTreatedDiarrheaPatient)
  
  #Sending the Diarrhea information to the report line
  lineReport <<- c(lineReport, diarrheaPart)
  
}

namesDiarrhea <- c("Total Diarrhées pour < 5 ans", "Total Diarrhées pour > 5 ans", "Total Diarrhées", "Total ORS distribués", "Total Patients ayant reÃ§u ORS", "Nb de patients avec diarrhée ayant reçu ORS", "% de patients avec diarrhée traités correctement")
