#########################################################
#Script to get the information about Malaria Treatments #
#########################################################

getPaluTreatment <- function(x){
  #Creating Reporting values
  treatmentPart <- c()
  TotalPatientRecevingTreatment <- 0
  
  
  #Total Blister of Coartem 05 - 15 given
  totalCoartem515 <- sum(x$coartem0515 == 1)
  treatmentPart <- c(treatmentPart, totalCoartem515)
  
  #Total Blister of Coartem 15 - 25 given
  totalCoartem1525 <- sum(x$coartem1525 == 1)
  treatmentPart <- c(treatmentPart, totalCoartem1525)
  
  #Total Blister of Coartem 25 - 35 given
  totalCoartem2535 <- sum(x$coartem2535 == 1)
  treatmentPart <- c(treatmentPart, totalCoartem2535)
  
  #Total Blister of Coartem 35 + given
  totalCoartem35 <- sum(x$coartem35 == 1)
  treatmentPart <- c(treatmentPart, totalCoartem35)
  
  #Total Blister of Coartem given
  totalCoartemGiven <- sum(totalCoartem515, totalCoartem1525, totalCoartem2535, totalCoartem35)
  treatmentPart <- c(treatmentPart, totalCoartemGiven)
  
  #Total Artenether given
  totalArtenether <- 0
  dataArtenetherA <- x[which(x$Artenether < 77),] #1. Summing the Artenether that have been written by number
  totalArtenetherA <- sum(dataArtenetherA$Artenether) 
  
  dataArtenetherB <- x[which(x$Artenether == 77),]
  totalArtenetherB <- nrow(dataArtenetherB) * 2 #2. Looking for the 77s (Artenether given but number given is unknown)
  
  totalArtenether <- sum(totalArtenetherA, totalArtenetherB)
  treatmentPart <- c(treatmentPart, totalArtenether)
  
  #Total Patients treated with Artenether
  totalPatientArtenether <- sum(x$Artenether > 0 & x$Artenether < 88) #77 is Artenther given but the number is unknown, 88 is the stockout code
  treatmentPart <- c(treatmentPart, totalPatientArtenether)
  
  #Total patients receiving treatment (i.e. only counting 1 patient if the patient received Coartem and Artenether)
  TotalPatientRecevingTreatment <- sum(x$traitementPaluRecu)
  treatmentPart <- c(treatmentPart, TotalPatientRecevingTreatment)
  
#   #Total Patients with TDR + and receiving treatment
#   TDR1AndTreatment <- sum(x$TDR1AndTreatment)
#   treatmentPart <- c(treatmentPart, TDR1AndTreatment)
  
#   #Percentage of patients treated 
#   percentageTreatedPatients <- TDR1AndTreatment/ sum(x$TDR == 1)
#   treatmentPart <- c(treatmentPart, percentageTreatedPatients)
  
  #Percentage of patient with simple malaria treated correclty
  ##Total Patient with simple malaria 
  totalSimpleMalaria <- sum(x$simpleMalaria)
#   treatmentPart <- c(treatmentPart, totalSimpleMalaria)
  
  ##Patient with Simple Malaria receiving Coartem treatment
  totalCorrectSimpleMalariaTreated <- sum(x$correctSimpleMalariaTreated)
  treatmentPart <- c(treatmentPart, totalCorrectSimpleMalariaTreated)
  
  ##Percentage of patient with simple malaria treated correclty
  percentageCorrectSimpleMalariaTreated <- totalCorrectSimpleMalariaTreated/ totalSimpleMalaria * 100
  treatmentPart <- c(treatmentPart, percentageCorrectSimpleMalariaTreated)
  
  #Percentage of patient with severe malaria treated correclty
  ##Total Patient with severe malaria
  totalPaluSevere <- sum(x$paluSevere)
  
  ##Patient with severe Malaria receiving Artenether and Reference treatment
  totalCorrectSevereMalariaTreated <- sum(x$CorrectSevereMalariaTreated)
  treatmentPart <- c(treatmentPart, totalCorrectSevereMalariaTreated)

  ##Percentage of patient with severe malaria treated correctly
  percentageCorrectSevereMalariaTreated<- totalCorrectSevereMalariaTreated/ totalPaluSevere * 100
  treatmentPart <- c(treatmentPart, percentageCorrectSevereMalariaTreated)
  
  #Percentage of patient with malaria treated correctly
  percentageCorrectTreatedPatient <- (totalCorrectSevereMalariaTreated + totalCorrectSimpleMalariaTreated) / sum(x$TDR == 1) * 100
  treatmentPart <- c(treatmentPart, percentageCorrectTreatedPatient)
  
  lineReport <<- c(lineReport, treatmentPart)
}

namesPaluTreatment <- c("Total Coartem 05 - 15 distribués", "Total Coartem 15 - 25 distribués", "Total Cortem 25 - 35 distribués", "Total Coartem 35 + distribués", "Total Coartem distribués", "Total Artenether distribués", "Total patients ayant reçu artenether", "Total patient ayant reçu traitement", "Total Patients Palu Simple traités avec Coartem", "% de Palu simple traité correctement", "Total Patients Palu sévère traités correctement", "% de Palu Sévère traité correctement", "% de patients traités correctement")