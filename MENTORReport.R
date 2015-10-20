source("getData.R")
source("getDemography.R")
source("getPalu.R")
source("getPaluTreatment.R")
source("getDiarrhea.R")
source("getMalnutrition.R")
source("getRupture.R")

createLine <- function(x){
  lineReport <<- c()
  
  getDemography(x)
  getPalu(x)
  getPaluTreatment(x)
  getDiarrhea(x)
  getMalnutrition(x)
  getRupture(x)
  
  MENTORReport <<- rbind(MENTORReport, lineReport)
  
}

# data[which(data$DateViste >= as.Date("1/07/2015", "%d/%m/%Y") & data$DateViste <= as.Date("31/07/2015", "%d/%m/%Y")) ,]

#Report Total
##MENTOR Report Total
MENTORReport <- data.frame()
createLine(data)

##MENTOR Report by Village
byVillage <- split(data, data$VillageASC)
lapply(byVillage, createLine)

#Monthly MENTOR 
## Monthly Total Report
monthGroup <- split(data, data$monthCategory)
lapply(monthGroup, createLine)

## Monthly Report by Village
interaction(data$monthCategory, data$VillageASC)
monthByVillage<- split(data, list(data$monthCategory, data$VillageASC))
lapply(monthByVillage, createLine)

columnsNames <- c(namesDemography, namesPalu, namesPaluTreatment, namesDiarrhea, namesMalnutrition, namesRupture)
names(MENTORReport) <- columnsNames

rowTitle <- c("total", names(byVillage), names(monthGroup), names(monthByVillage))
MENTORReport <- cbind(MENTORReport, rowTitle)

MENTORReport <-MENTORReport[, c(ncol(MENTORReport),seq_along(columnsNames))]

print(MENTORReport)
write.csv2(MENTORReport, "../data/MENTORReporttest.csv")
write.csv2(data, "../data/data for analysis.csv")
cat("Rapport fini")