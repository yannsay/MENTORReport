#########################################################
#Script to get the information about Malnutrition cases #
#########################################################

getMalnutrition <- function(x){
  
  #Creating Reporting values
  malnutritionPart <- c()

  #Selection of the patients < 5 years old
  ageGroup <- x[which(x$ageCategory5 == 0),]
  
  #Total MUAC Vert
  totalMUACVert <- sum(ageGroup$MUAC == "Vert")
  malnutritionPart <- c(malnutritionPart, totalMUACVert)
  
  #Total MUAC Jaune
  totalMUACJaune <- sum(ageGroup$MUAC == "Jaune")
  malnutritionPart <- c(malnutritionPart, totalMUACJaune)
  
  #Total MUAC Orange
  totalMUACOrange <- sum(ageGroup$MUAC == "Orange")
  malnutritionPart <- c(malnutritionPart, totalMUACOrange)
  
  #Total of MUAC Rouge
  totalMUACRouge <- sum(ageGroup$MUAC == "Rouge")
  malnutritionPart <- c(malnutritionPart, totalMUACRouge)
  
  #Total of Oedeme
  totalOedeme <- sum(ageGroup$Oedme == 1)
  malnutritionPart <- c(malnutritionPart, totalOedeme)
  
  #Total of Severe Malnutrition
  totalSevereMalnutrition <- sum(ageGroup$MUAC == 4)
  malnutritionPart <- c(malnutritionPart, totalSevereMalnutrition)
  
  #Total Correct Malnutrition Treated
  totalCorrectMalnutritionTreated <- sum(ageGroup$correctMalnutritionTreated)
  malnutritionPart <- c(malnutritionPart, totalCorrectMalnutritionTreated)
    
  #Percentage of patient with Severe Malnutrition treated correctly
  percentageCorrectTreatedMalnutritionPatient <- totalCorrectMalnutritionTreated / totalMUACRouge * 100
  malnutritionPart <- c(malnutritionPart, percentageCorrectTreatedMalnutritionPatient)
  
  #Sending the Diarrhea information to the report line
  lineReport <<- c(lineReport, malnutritionPart)
  
}

namesMalnutrition <- c("MUAC Vert", "MUAC Jaune", "MUAC Orage", "MUAC Rouge", "Total Oedème", "Total Malnutrion sévère", "Nb de patients malnutris ayant reçu référence", "% de patients malnutris traités correctement")