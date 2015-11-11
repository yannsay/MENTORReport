############################################
#Script to get the demographic information #
############################################

getDemography <- function(x) {
  #Creating reporting values
  demographyPart <- c()
  totalPatients <- 0
  totalFemale <- 0
  totalMale <- 0
  
  #Dummy value for the selection
  nbCategorie <- length(ageRangeDemo) + 1  # + 1 because there are just 3 groups, it needs 4 groups, 0-4; 5-17; 18-59; 60+
  a <- 0
  
  #calculing number of female/male per category
  for (i in 1:nbCategorie) { 

    ageGroup <- x[which(x$categorieAge == a),]
    
    nbAgeGroupFemale <- sum(ageGroup$sexe == 1, na.rm = TRUE)
    demographyPart <- c(demographyPart, nbAgeGroupFemale)

    nbAgeGroupMale <-  sum(ageGroup$sexe == 2, na.rm = TRUE)
    demographyPart <- c(demographyPart, nbAgeGroupMale)

    nbAgeGroupTotal <- nbAgeGroupMale + nbAgeGroupFemale
    demographyPart <- c(demographyPart, nbAgeGroupTotal)

    totalPatients <- totalPatients +nbAgeGroupTotal
    totalFemale <- totalFemale + nbAgeGroupFemale
    totalMale <- totalMale + nbAgeGroupMale
    a <- a +1
  }
  
  #Total of Patients
  demographyPart <- c(demographyPart, totalFemale, totalMale, totalPatients)

  #Sending the demographic information to the report line
  lineReport <<- c(lineReport, demographyPart)
}

namesDemography <- c("< 5ans Femmes", "< 5ans Hommes", "< 5ans Total", ">= 5 et < 18 ans Femmes", ">= 5 et < 18 ans Hommes", ">= 5 et < 18 ans Total", ">= 18 et < 60 ans Femmes", ">= 18 et < 60 ans Hommes", ">= 18 et < 60 ans Total", "> 60 ans Femmes", "> 60 ans Hommes", "> 60 ans Total", "Total Femmes", "Total Hommes", "Total Patients")
