#########################################################
#Script to get the information about Malaria Treatments #
#########################################################

getPaluTreatment <- function(x){
  #Creating Reporting values
  treatmentPart <- c()
  TotalPatientRecevingTreatment <- 0
  
  
  #Total Blister of Coartem 05 - 15 given
  totalCoartem515 <- sum(x$coartem0515 == 1, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalCoartem515)
  
  #Total Blister of Coartem 15 - 25 given
  totalCoartem1525 <- sum(x$coartem1525 == 1, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalCoartem1525)
  
  #Total Blister of Coartem 25 - 35 given
  totalCoartem2535 <- sum(x$coartem2535 == 1, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalCoartem2535)
  
  #Total Blister of Coartem 35 + given
  totalCoartem35 <- sum(x$coartem35 == 1, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalCoartem35)
  
  #Total Blister of Coartem given
  totalCoartemGiven <- sum(totalCoartem515, totalCoartem1525, totalCoartem2535, totalCoartem35, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalCoartemGiven)
  
  #Total Artenether given
  totalArtenether <- 0
  dataArtenetherA <- x[which(x$Artenether < 77),] #1. Summing the Artenether that have been written by number
  totalArtenetherA <- sum(dataArtenetherA$Artenether) 
  
  dataArtenetherB <- x[which(x$Artenether == 77),]
  totalArtenetherB <- nrow(dataArtenetherB) * 2 #2. Looking for the 77s (Artenether given but number given is unknown)
  
  totalArtenether <- sum(totalArtenetherA, totalArtenetherB, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalArtenether)
  
  #Total Patients treated with Artenether
  totalPatientArtenether <- sum(x$Artenether > 0 & x$Artenether < 88, na.rm = TRUE) #77 is Artenther given but the number is unknown, 88 is the stockout code
  treatmentPart <- c(treatmentPart, totalPatientArtenether)
  
  #Total patients receiving treatment (i.e. only counting 1 patient if the patient received Coartem and Artenether)
  TotalPatientRecevingTreatment <- sum(x$traitementPaluRecu, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, TotalPatientRecevingTreatment)

  #Percentage of patient with simple malaria treated correclty
  ##Total Patient with simple malaria 
  totalPaluSimple <- sum(x$paluSimple, na.rm = TRUE)

  ##Patient with Simple Malaria receiving Coartem treatment
  totalPaluSimpleTraiteCorrectement <- sum(x$paluSimpleTraiteCorrectement, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalPaluSimpleTraiteCorrectement)
  
  ##Percentage of patient with simple malaria treated correclty
  pourcentagePaluSimpleTraiteCorrectement <- totalPaluSimpleTraiteCorrectement/ totalPaluSimple * 100
  treatmentPart <- c(treatmentPart, pourcentagePaluSimpleTraiteCorrectement)
  
  
  #Dummy value for the selection of age. 0 for < 5 years old; 1 for >= 5 years old
  a <- 0 
  
  #Dummy value for the loop length (length of the age category without NA)
  NbCategory <- unique(categorieAge5[!is.na(categorieAge5)])
  ##Simple malaria treatment per age category
  for (i in seq_along(NbCategory)) { 
    ageGroup <- x[which(x$categorieAge5 == a),]
    
    subTotalPaluSimple <- sum(ageGroup$paluSimple, na.rm = TRUE)
    
    subTotalPaluSimpleTraiteCorrectement <- sum(ageGroup$paluSimpleTraiteCorrectement, na.rm = TRUE)
    treatmentPart <- c(treatmentPart, subTotalPaluSimpleTraiteCorrectement)
    
    pourcentagePaluSimpleTraiteCorrectement <- subTotalPaluSimpleTraiteCorrectement/ subTotalPaluSimple * 100
    treatmentPart <- c(treatmentPart, pourcentagePaluSimpleTraiteCorrectement)

    a <- a +1
  }

  #Percentage of patient with severe malaria treated correclty
  ##Total Patient with severe malaria
  totalPaluSevere <- sum(x$paluSevere, na.rm = TRUE)
  
  ##Patient with severe Malaria receiving Artenether and Reference treatment
  totalPaluSevereTraiteCorrectement <- sum(x$paluSevereTraiteCorrectement, na.rm = TRUE)
  treatmentPart <- c(treatmentPart, totalPaluSevereTraiteCorrectement)

  ##Percentage of patient with severe malaria treated correctly
  pourcentagePaluSevereTraiteCorrectement<- totalPaluSevereTraiteCorrectement/ totalPaluSevere * 100
  treatmentPart <- c(treatmentPart, pourcentagePaluSevereTraiteCorrectement)
  
  #Percentage of patient with malaria treated correctly
  percentageCorrectTreatedPatient <- (totalPaluSevereTraiteCorrectement + totalPaluSimpleTraiteCorrectement) / sum(x$TDR == 1, na.rm = TRUE) * 100
  treatmentPart <- c(treatmentPart, percentageCorrectTreatedPatient)
  
  ##Severe malaria treatment per age category
  a <- 0 
  for (i in seq_along(NbCategory)) { 
    ageGroup <- x[which(x$categorieAge5 == a),]
    
    subTotalPaluSevere <- sum(ageGroup$paluSevere, na.rm = TRUE)
    
    subTotalPaluSevereTraiteCorrectement <- sum(ageGroup$paluSevereTraiteCorrectement, na.rm = TRUE)
    treatmentPart <- c(treatmentPart, subTotalPaluSevereTraiteCorrectement)
    
    pourcentagePaluSevereTraiteCorrectement <- subTotalPaluSevereTraiteCorrectement/ subTotalPaluSevere * 100
    treatmentPart <- c(treatmentPart, pourcentagePaluSevereTraiteCorrectement)
    
    a <- a +1
  }
  
  lineReport <<- c(lineReport, treatmentPart)
}

namesPaluTreatment <- c("Total Coartem 05 - 15 distribués", "Total Coartem 15 - 25 distribués", "Total Cortem 25 - 35 distribués", "Total Coartem 35 + distribués", "Total Coartem distribués", "Total Artenether distribués", "Total patients ayant reçu artenether", "Total patient ayant reçu traitement", "Total Patients Palu Simple traités avec Coartem", "% de Palu simple traité correctement", "Total de Palu simple traité correctement (< 5ans)", "% de Palu simple traité correctement (< 5 ans)", "Total de Palu simple traité correctement (5ans +)", "% de Palu simple traité correctement (5 ans +)","Total Patients Palu sévère traités correctement", "% de Palu Sévère traité correctement", "% de patients traités correctement", "Total de Palu sevère traité correctement (< 5ans)", "% de Palu sevère traité correctement (< 5 ans)", "Total de Palu sevère traité correctement (5ans +)", "% de Palu sevère traité correctement (5 ans +)")



