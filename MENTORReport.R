source("getData.R")
source("getDemography.R")
source("getPalu.R")
source("getPaluTreatment.R")
source("getDiarrhea.R")
source("getMalnutrition.R")

createLine <- function(x){
  lineReport <<- c()
  
  getDemography(x)
  getPalu(x)
  getPaluTreatment(x)
  getDiarrhea(x)
  getMalnutrition(x)
  
  MENTORReport <<- rbind(MENTORReport, lineReport)
  
}

# data <- data[which(data$DateViste > "01/07/2015" & data$DateViste < "15/07/2015"),]

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

columnsNames <- c(namesDemography, namesPalu, namesPaluTreatment, namesDiarrhea, namesMalnutrition)
names(MENTORReport) <- columnsNames

rowTitle <- c("total", names(byVillage), names(monthGroup), names(monthByVillage))
MENTORReport <- cbind(MENTORReport, rowTitle)

MENTORReport <-MENTORReport[, c(ncol(MENTORReport),seq_along(columnsNames))]

print(MENTORReport)
write.csv2(MENTORReport, "../data/MENTORReporttest.csv")
write.csv2(data, "../data/data for analysis.csv")
cat("Rapport fini")

