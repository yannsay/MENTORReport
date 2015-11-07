########################################
#Script to get the data and clean them #
########################################


### Preparing the data
## Checking if the file is in the correct folder
   ############ A Ã©crire

data <- read.csv2("../data/test1.csv") #########(1) change the name of csv
# csv2 is used because French annotation uses ";" as separator because "," is used for decimals (instead of ".").
                                
## data cleanning
# checking if new villages or ASC are entered

if (sum(!is.na(data$nomASC2)) == 0){
    data$nomASC2 <- NULL
  
} else {
##Adding value to NomASC1
##Delete column NomASC2  
  
}

if (sum(!is.na(data$villageASC2)) == 0){
    data$villageASC2 <- NULL
  
} else {
##Adding value to VillageASC2
##Delete column VillageASC2  
}

## Changing NAs to 0
data[is.na(data)] <- 0

## Setting 99s to NA
data[, 2:ncol(data)][data[, 2:ncol(data)] == 99] <- NA

## Setting factors
data$sexe <- as.factor(data$sexe)
data$MUAC <- as.factor(data$MUAC)
data$transfert <- as.factor(data$transfert)
data$dateVisite <- as.Date(data$dateVisite, "%d/%m/%Y") #Changer le nom après modification de Epi Info

#Adding value for the month selection
##Creating variables: month category
mois <- format(data$dateVisite, "%m-%Y")
data <- cbind(data, mois)

#Adding value for the getDemography
##Creating variable: age category
ageRangeDemo <- c(5,18,60)
ageCategory <- findInterval(data$age, ageRangeDemo)
data <- cbind(data, ageCategory)

#Adding values for the getPalu, getDiarrhea
##Creating variable: Age Category below/above 5 years old
ageRange5 <- c(5)
ageCategory5 <- findInterval(data$age, ageRange5)
data <- cbind(data, ageCategory5)

## Creating variable: fever > 39.5 
fievre395 <- data$temperature > 39.5
data <- cbind(data, fievre395)

## Creating variable: severe malaria symptoms 
signesPaluSevere <- data$fievre395 == 1 | data$paleur == 1 | data$convultion == 1 | data$eveil == 0
data <- cbind(data, signesPaluSevere)

## Creating variable: symptoms of severe malaria and TDR +
paluSevere <- data$signesPaluSevere == TRUE & data$TDR == 1
data <- cbind(data, paluSevere)

#Adding values for the getPaluTreatment
## Creating variable: patient receiving malaria treatment (i.e. only counting 1 patient if the patient received Coartem and Artenether)
traitementPaluRecu <- data$coartem0515 == 1 | data$coartem1525 == 1 | data$coartem2535 == 1 | data$coartem35 == 1 | (data$artenether > 0 & data$artenether < 88)
data <- cbind(data, traitementPaluRecu)

# ## Creating variable: patient with TDR + and receiving malaria treatment
# TDR1AndTreatment <- data$TDR == 1 & data$traitementPaluRecu == 1
# data <- cbind(data, TDR1AndTreatment)

## Creating variable: patient receiving only Coartem 
coartemGiven <- data$coartem0515 == 1 | data$coartem1525 == 1 | data$coartem2535 == 1 | data$coartem35 == 1
data <- cbind(data, coartemGiven)

## Creating variable: Patient with Simple Malaria
simpleMalaria <-  data$signesPaluSevere == FALSE & data$TDR == 1
data <- cbind(data, simpleMalaria)

## Creating variable: Patient with Simple Malaria receiving correct treatment for simple malaria (i.e Coartem treatment )
correctSimpleMalariaTreated <- data$simpleMalaria == TRUE & data$coartemGiven == TRUE
data <- cbind(data, correctSimpleMalariaTreated)

## Creating variable: Transfert for Malaria
transfertDemande <- data$transfert != 0
data <- cbind(data, transfertDemande)

## Creating variable: Patient with Severe Malaria receiving correct treatment (i.e Coartem and Artenether and Transfert)
CorrectSevereMalariaTreated <- data$paluSevere == TRUE  & data$transfertDemande == TRUE
#ajouter conditions artenether
data <- cbind(data, CorrectSevereMalariaTreated)

#Adding values for the getDiarrhea
## Creating variable: SRO Given
SROGiven <- (data$SRO > 0 & data$SRO < 88) | (data$SRO == 77)
data <- cbind(data, SROGiven)

## Creating variable: Patient with diarrhea and received SRO (i.e correct treatment )
correctDiarrheaTreated <- data$diarrhee == 1 & data$SROGiven == TRUE
data <- cbind(data, correctDiarrheaTreated)

#Adding values for the getMalnutrition
## Creating variable: Patient with Severe Malnutrition and received Transfert for Malnutrition (i.e correct treatment )
correctMalnutritionTreated <- data$MUAC == 4 & data$transfertDemande == TRUE
data <- cbind(data, correctMalnutritionTreated)  