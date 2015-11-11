######################################################
#Script pour avoir les informations sur les ruptures #
######################################################
#Fonction pour savoir s'il y a rupture
rupture <- function(x, medicament){
  #Variables pour le comptage
  totalRupture <<- 0
    
  comptageRupture <- function(x2, medicament2){
    #Variables pour le comptage
    stockout <<- 0
    compteurRupture <- 0
    dateRupture <- 0
  
    #Loop
    for (i in seq_len(nrow(x2))){
      if(!is.na(x2[i, medicament2])){
        #Conditions pour les ruptures
        ## Première rupture
        if(x2[i,medicament2] == 88 & compteurRupture == 0) {
          dateRupture <- x2[i, "DateViste"]
          compteurRupture <- compteurRupture + 1
        }
        
        ## Deuxième rupture date différente
        if(x2[i,medicament2] == "88" & compteurRupture > 0 & x2[i, "DateViste"] > dateRupture) {
          incrementationRupture <- as.integer(x2[i, "DateViste"] - dateRupture)
          compteurRupture <- compteurRupture + incrementationRupture
          dateRupture <- x2[i, "DateViste"]
        }
        
        ## rupture > 7 jours
        if(compteurRupture >= 7 & stockout == 0) {
          stockout <<- stockout + 1
        }
        
        ## ravitaillement
        if(x2[i,medicament2] != "88" & x2[i,medicament2] != 0 & compteurRupture > 0) {
          stockout <- 0
          compteurRupture <- 0
          dateRupture <- 0
        } 
      }
    }
    totalRupture <<- totalRupture + stockout
  }
  #Ranger par ordre date
  x[order(x$DateViste),]
  
  #Séparation par villages/mois
  interaction(x$monthCategory, x$VillageASC)
  monthByVillage<- split(x, list(x$monthCategory, x$VillageASC))
  
  #Loop a travers les villages/mois
  lapply(monthByVillage,comptageRupture, medicament2 = medicament) 
}


#Fonction pour vérifier les ruptures pour chaque médicaments
##Variables pour le rapport
rupturePart <- c()
listeMedicament <- c("Coartem0515", "Coartem1525", "Coartem2535", "Coartem35", "Artenether", "SRO", "Albendazole", "Vitamines100000UI", "Vitamines200000UI", "Paractamol", "MILD", "Kit")

##Calculs des ruptures
getRupture <- function(x) {
  for (i in seq_along(listeMedicament)){
    rupture(x, listeMedicament[i])
    rupturePart <- c(rupturePart, totalRupture)
  }
  
  #Sending the demographic information to the report line
  lineReport <<- c(lineReport, rupturePart)
}

namesRupture <- c()
for (i in seq_along(listeMedicament)) {
  namesRupture <- c(namesRupture, paste("Rupture", listeMedicament[i]))
}