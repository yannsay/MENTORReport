########################################
#Script to get the data and clean them #
########################################


### Preparing the data
## Checking if the file is in the correct folder
   ############ A écrire

data <- read.csv2("../data/data2mois.csv") #########(1) change the name of csv
# csv2 is used because French annotation uses ";" as separator because "," is used for decimals (instead of ".").
                                
## data cleanning
# checking if new villages or ASC are entered

if (sum(!is.na(data$VillageASC2)) == 0){
    data$NomASC2 <- NULL
  
} else {
##Adding value to NomASC1
##Delete column NomASC2  
  
}

if (sum(!is.na(data$VillageASC2)) == 0){
    data$VillageASC2 <- NULL
  
} else {
##Adding value to VillageASC2
##Delete column VillageASC2  
}

## Changing NAs to 0
data[is.na(data)] <- 0

## Setting 99s to NA
data[, 2:ncol(data)][data[, 2:ncol(data)] == 99] <- NA

## Setting factors
data$Sexe <- as.factor(data$Sexe)
data$MUAC <- as.factor(data$MUAC)
data$Transfert <- as.factor(data$Transfert)
data$DateViste <- as.Date(data$DateViste, "%d/%m/%Y") #Changer le nom après modification de Epi Info

#Adding value for the month selection
##Creating variables: month and year categories
monthCategory <- format(data$DateViste, "%m-%Y")
# YearCategory <- format(data$DateViste, "%Y")
data <- cbind(data, monthCategory)

#Adding value for the getDemography
##Creating variable: age category
ageRangeDemo <- c(5,18,60)
ageCategory <- findInterval(data$Age, ageRangeDemo)
data <- cbind(data, ageCategory)

#Adding values for the getPalu, getDiarrhea
##Creating variable: Age Category below/above 5 years old
ageRange5 <- c(5)
ageCategory5 <- findInterval(data$Age, ageRange5)
data <- cbind(data, ageCategory5)

## Creating variable: fever > 39.5 
fievre395 <- data$Temprature > 39.5
data <- cbind(data, fievre395)

## Creating variable: severe malaria symptoms 
signesPaluSevere <- data$fievre395 == 1 | data$Paleur == 1 | data$Convultion == 1 | data$Somnolence == 1
data <- cbind(data, signesPaluSevere)

## Creating variable: symptoms of severe malaria and TDR +
paluSevere <- data$signesPaluSevere == TRUE & data$TDR == 1
data <- cbind(data, paluSevere)

#Adding values for the getPaluTreatment
## Creating variable: patient receiving malaria treatment (i.e. only counting 1 patient if the patient received Coartem and Artenether)
patientReceivedMalariaTreatment <- data$Coartem0515 == 1 | data$Coartem1525 == 1 | data$Coartem2535 == 1 | data$Coartem35 == 1 | (data$Artenether > 0 & data$Artenether < 88)
data <- cbind(data, patientReceivedMalariaTreatment)

# ## Creating variable: patient with TDR + and receiving malaria treatment
# TDR1AndTreatment <- data$TDR == 1 & data$patientReceivedMalariaTreatment == 1
# data <- cbind(data, TDR1AndTreatment)

## Creating variable: patient receiving only Coartem 
coartemGiven <- data$Coartem0515 == 1 | data$Coartem1525 == 1 | data$Coartem2535 == 1 | data$Coartem35 == 1
data <- cbind(data, coartemGiven)

## Creating variable: Patient with Simple Malaria
simpleMalaria <-  data$signesPaluSevere == FALSE & data$TDR == 1
data <- cbind(data, simpleMalaria)

## Creating variable: Patient with Simple Malaria receiving correct treatment for simple malaria (i.e Coartem treatment )
correctSimpleMalariaTreated <- data$simpleMalaria == TRUE & data$coartemGiven == TRUE
data <- cbind(data, correctSimpleMalariaTreated)

## Creating variable: Transfert for Malaria
malariaTransfert <- data$Transfert == 1 | data$Transfert == 3 | data$Transfert == 77
data <- cbind(data, malariaTransfert)

## Creating variable: Patient with Severe Malaria receiving correct treatment (i.e Coartem and Artenether and Transfert)
CorrectSevereMalariaTreated <- data$paluSevere == TRUE & data$Artenether == TRUE  & data$malariaTransfert == TRUE
data <- cbind(data, CorrectSevereMalariaTreated)

#Adding values for the getDiarrhea
## Creating variable: SRO Given
SROGiven <- (data$SRO > 0 & data$SRO < 88) | (data$SRO == 77)
data <- cbind(data, SROGiven)

## Creating variable: Patient with diarrhea and received SRO (i.e correct treatment )
correctDiarrheaTreated <- data$Diarrhee == 1 & data$SROGiven == TRUE
data <- cbind(data, correctDiarrheaTreated)

#Adding values for the getMalnutrition
## Creating variable: Transfert for Malnutrition
malnutritionTransfert <- data$Transfert == 2 | data$Transfert == 3 | data$Transfert == 77 
data <- cbind(data, malnutritionTransfert)

## Creating variable: Patient with Severe Malnutrition and received Transfert for Malnutrition (i.e correct treatment )
correctMalnutritionTreated <- data$MUAC == 4 & data$malnutritionTransfert == TRUE
data <- cbind(data, correctMalnutritionTreated)  