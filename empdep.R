# Téléchargement des données Pôle Emploi

#ISO CODES des anciennes régions

##install.packages("ISOcodes") # au début de 2017
library(ISOcodes)
data("ISO_3166_2")
ISO_3166_2 <- ISO_3166_2[ISO_3166_2$Country == 'FR',]
dep <- ISO_3166_2[ISO_3166_2$Type == 'Metropolitan department',]
row.names(dep) <- 1:nrow(dep)
reg <- ISO_3166_2[ISO_3166_2$Type == 'Metropolitan region',]
row.names(reg) <- 1:nrow(reg)
dep$Parent <- paste0("FR-", dep$Parent)
reg$Parent <- reg$Code
reg <- reg[, !(colnames(reg) %in% c("Code"))]
regcodes <- merge(dep[c("Code", "Name", "Parent")], reg[c("Name", "Parent")], by="Parent")
names(regcodes) <- c("CodeReg", "CodeDep", "Dep", "Reg")

# Téléchargement

emploi <- readHTMLTable("http://statistiques.pole-emploi.org/stmt/defm?ff=A,B,C&fh=1&lj=0&pp=200901-201702&ss=1", stringsAsFactors = FALSE)
empdep <- data.frame(emploi$DataTable, stringsAsFactors = FALSE)
empdep <- empdep[, !(colnames(empdep) %in% c("Total"))]
##blanks/numbers
empdep[,2:ncol(empdep)] <- apply(empdep[,2:ncol(empdep)], 2, function(y) as.numeric(gsub("[[:blank:]]", "", y)))
##set date to last day of month
empdep <- data.frame("Date"=rev(seq(as.Date("2009-02-01"), length=nrow(empdep), by="month")-1), empdep)

# Regroupement

##install.packages("reshape")
library(reshape)
empdep <- reshape(empdep, varying = c(names(empdep[,3:ncol(empdep)])), v.names = "DEFM", timevar = "Dep", times = c(names(empdep[,3:ncol(empdep)])), direction = "long")
row.names(empdep) <- 1:nrow(empdep)
##format
empdep$Dep <- gsub('[[:alpha:]]+[[:punct:]]', '', empdep$Dep)
empdep$Dep <- paste0("FR-", empdep$Dep)
colnames(empdep)[colnames(empdep) == 'Dep'] <- 'CodeDep'
##merge
empdep <- merge(empdep[c("Date", "Mois", "DEFM", "CodeDep")], regcodes[c("Dep", "Reg", "CodeDep", "CodeReg")], by="CodeDep")
##aggregate
empREG <- aggregate(x = empdep[c("DEFM")], by=list(Date=empdep$Date, Reg=empdep$Reg), FUN=sum)

##revision
###sum(empdep$DEFM[empdep$Reg == 'Auvergne' & empdep$Date == '2009-08-31']) == 
  ###empREG$DEFM[empREG$Reg == 'Auvergne' & empREG$Date == '2009-08-31']
###[1] TRUE
