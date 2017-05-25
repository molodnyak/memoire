#SARIMA(X) Prévision pour chaque de 12 dernières périodes

##Read 22 csv for each region and each period

DF <- data.frame("CodeReg"='', "Date"='', "Month"='', "Model"='', "x"='', "nobs"='', "aicSARIMA"='', "lowerSARIMA"='', "fSARIMA"='', 
                 "upperSARIMA"='', "aicSARIMAX"='', "lowerSARIMAX"='', "fSARIMAX"='', "upperSARIMAX"='', "DEFM"='', 
                 stringsAsFactors = F)
DF <- DF[-1,]

library(forecast)

#Boucle

for (i in month.abb){
  
  file_names <- paste0("C:/Users/Evgenii Molodniak/Documents/Master 2/Memoire/Google/Trends/", i, "/", unique(empreg$Reg), '.csv')
  ###encoding="WINDOWS-1252"
  gtperiod <- do.call(cbind, lapply(file_names, read.csv, stringsAsFactors = F, encoding="UTF-8"))
  ##formatting
  names(gtperiod) <- gsub(".*\\((.*)\\).*", "\\1", t(gtperiod[1,]))
  gtperiod <- gtperiod[-1,]
  
  gtperiod <- data.frame(lapply(gtperiod, as.numeric), stringsAsFactors = F)
  gtperiod <- data.frame("Date"=seq(as.Date("2009-02-01"), length=nrow(gtperiod), by="month")-1, gtperiod, stringsAsFactors = F)
  ##reshape
  library(reshape)
  gtperiod <- reshape(gtperiod, varying = c(names(gtperiod[,-1])), v.names = "GT", timevar = "Reg", times = c(names(gtperiod[,-1])), direction = "long")
  gtperiod <- gtperiod[, !(colnames(gtperiod) %in% c("id"))]
  row.names(gtperiod) <- 1:nrow(gtperiod)
  
  ##region names correction
  gtperiod$CodeReg <- NA
  
  ###unique(gtperiod[c('CodeReg', 'Reg')]) unique(gtperiod$Reg)
  gtperiod$CodeReg[gtperiod$Reg == "Alsace"] <- "FR-A"
  gtperiod$CodeReg[gtperiod$Reg == "Aquitaine"] <- "FR-B"
  gtperiod$CodeReg[gtperiod$Reg == "Auvergne"] <- "FR-C"
  gtperiod$CodeReg[gtperiod$Reg == "Basse.Normandie"] <- "FR-P"
  gtperiod$CodeReg[gtperiod$Reg == "Bourgogne"] <- "FR-D"
  gtperiod$CodeReg[gtperiod$Reg == "Bretagne"] <- "FR-E"
  gtperiod$CodeReg[gtperiod$Reg == "Centre.Val.de.Loire"] <- "FR-F"
  gtperiod$CodeReg[gtperiod$Reg == "Champagne.Ardenne"] <- "FR-G"
  gtperiod$CodeReg[gtperiod$Reg == "Corse"] <- "FR-H"
  gtperiod$CodeReg[gtperiod$Reg == "Franche.Comte"] <- "FR-I"
  gtperiod$CodeReg[gtperiod$Reg == "Haute.Normandie"] <- "FR-Q"
  gtperiod$CodeReg[gtperiod$Reg == "Ile.de.France"] <- "FR-J"
  gtperiod$CodeReg[gtperiod$Reg == "Languedoc.Roussillon"] <- "FR-K"
  gtperiod$CodeReg[gtperiod$Reg == "Limousin"] <- "FR-L"
  gtperiod$CodeReg[gtperiod$Reg == "Lorraine"] <- "FR-M"
  gtperiod$CodeReg[gtperiod$Reg == "Midi.Pyrenees"] <- "FR-N"
  gtperiod$CodeReg[gtperiod$Reg == "Nord.Pas.de.Calais"] <- "FR-O"
  gtperiod$CodeReg[gtperiod$Reg == "Pays.de.la.Loire"] <- "FR-R"
  gtperiod$CodeReg[gtperiod$Reg == "Picardie"] <- "FR-S"
  gtperiod$CodeReg[gtperiod$Reg == "Poitou.Charentes"] <- "FR-T"
  gtperiod$CodeReg[gtperiod$Reg == "Provence.Alpes.Cote.d.Azur"] <- "FR-U"
  gtperiod$CodeReg[gtperiod$Reg == "Rhone.Alpes"] <- "FR-V"
  
  ## Merge GT and DEFM
  gtperiod <- merge(gtperiod[c("Date", "GT",'CodeReg')], empreg[c("Reg","Date","DEFM","CodeReg")], 
                    by=c('Date', 'CodeReg'), all.y=TRUE)
  ##Forecast Date
  fDATE <- gtperiod$Date[length(gtperiod$Date[!is.na(gtperiod$GT)])]
  
  for(j in unique(empreg$CodeReg)){
  tryCatch({
    #SARIMA
    x <- ts(gtperiod$DEFM[gtperiod$CodeReg == j & gtperiod$Date < fDATE], start=c(2009, 1), frequency=12)
    sarima <- auto.arima(x)
    fsarima <- forecast(sarima, h=1)
    ##orders from auto.arima(x)
    orderx <- as.numeric(unlist(strsplit(regmatches(fsarima$method, gregexpr("(?<=\\().*?(?=\\))", fsarima$method, perl=T))[[1]][1], ",")))
    seasonalx <- as.numeric(unlist(strsplit(regmatches(fsarima$method, gregexpr("(?<=\\().*?(?=\\))", fsarima$method, perl=T))[[1]][2], ",")))
    library(stringr)
    periodx <- as.numeric(str_extract_all(fsarima$method, regex('(?<=\\[|-)[0-9]+(?=\\]|-?)'))[[1]])
    #SARIMAX
    xx <- ts(gtperiod$GT[gtperiod$CodeReg == j & gtperiod$Date < fDATE], start=c(2009, 1), frequency=12)
    ##xreg - external regressor
    sarimax <- Arima(x, xreg = xx, order = orderx, seasonal = list(order=seasonalx, period=periodx), 
                     include.drift = grepl("drift", fsarima$method), method="ML")
    ##xreg - future value for external regressor
    fsarimax <- forecast(sarimax, xreg = gtperiod$GT[gtperiod$CodeReg == j & gtperiod$Date == fDATE])
    ##bind dataframe
    DF <- rbind(DF, data.frame("CodeReg"=j, "Date"=as.Date(fDATE), "Month"=i,
                               "Model"=fsarima$method, "x"=length(sarima$x), "nobs"=sarima$nobs, 
                               "aicSARIMA"=sarima$aic, "lowerSARIMA"=fsarima$lower[2], "fSARIMA"=fsarima$mean[1], 
                               "upperSARIMA"=fsarima$upper[2], 
                               "aicSARIMAX"=sarimax$aic, "lowerSARIMAX"=fsarimax$lower[2], "fSARIMAX"=fsarimax$mean[1], 
                               "upperSARIMAX"=fsarimax$upper[2],
                               "DEFM"=gtperiod$DEFM[gtperiod$CodeReg == j & gtperiod$Date == fDATE], 
                               stringsAsFactors = F))

    message("Auto Arima modelling for region ",  j, " for month ", i)})
  }
  
}

DF <- merge(DF, reg[c("Reg", "CodeReg")], by="CodeReg")
DF <- DF[order(DF$Date),]
rownames(DF) <- 1:nrow(DF)

#PLOTS

##------------------------ 22 graphiques --------------------------

for(i in unique(empreg$CodeReg)){
  plot(DF$Date[DF$CodeReg == i], DF$fSARIMA[DF$CodeReg == i], 
       col="#dc3912", pch=20, lwd=5, main=unique(DF$Reg[DF$CodeReg == i]), ylab="DEFM", xlab="", xaxt = "n")
  
  lines(DF$Date[DF$CodeReg == i], DF$lowerSARIMA[DF$CodeReg == i], col="#dc3912", lwd=1, lty=2)
  lines(DF$Date[DF$CodeReg == i], DF$upperSARIMA[DF$CodeReg == i], col="#dc3912", lwd=1, lty=2)
  
  lines(DF$Date[DF$CodeReg == i], DF$fSARIMAX[DF$CodeReg == i], col="#3366cc", type = "p", pch=20, lwd=5)
  lines(DF$Date[DF$CodeReg == i], DF$lowerSARIMAX[DF$CodeReg == i], col="#3366cc", lwd=1, lty=2)
  lines(DF$Date[DF$CodeReg == i], DF$upperSARIMAX[DF$CodeReg == i], col="#3366cc", lwd=1, lty=2)
  
  lines(DF$Date[DF$CodeReg == i], DF$DEFM[DF$CodeReg == i], col="#109618", lwd=3)
  abline(v = as.Date("2017-01-15"))
  legend("topleft", legend=c("SARIMA","SARIMAX","DEFM"), fill=c("#dc3912","#3366cc","#109618"))
  axis(1, at=unique(DF$Date), labels=unique(DF$Month))
}

##------------------------ 22 graphiques --------------------------

#ACCURACY

##------------------------ MAPE --------------------------

library(forecast)

eDF <- data.frame("CodeReg"="", "MAPESARIMA"="", "MAPESARIMAX"="")
eDF <- eDF[-1,]

for(i in unique(empreg$CodeReg)){
  
  eDF <- rbind(eDF, data.frame("CodeReg" = i, 
                               "MAPESARIMA" = accuracy(DF$fSARIMA[DF$CodeReg == i], DF$DEFM[DF$CodeReg == i])[5],
                               "MAPESARIMAX" = accuracy(DF$fSARIMAX[DF$CodeReg == i], DF$DEFM[DF$CodeReg == i])[5]))

}

eDF$diffMAPE <- eDF$MAPESARIMA - eDF$MAPESARIMAX
eDF$GoogleTrends <- ifelse(eDF$MAPESARIMAX < eDF$MAPESARIMA, "+ (Oui)", "- (No)")
eDF$GTmap <- ifelse(eDF$MAPESARIMAX < eDF$MAPESARIMA, 1, 0)
eDF <- merge(eDF, correlate[c("Region", "Blason", "Correlation", "DEFM", "Chomage", "CodeReg")], by="CodeReg")
eDF <- eDF[order(-eDF$Chomage),]
rownames(eDF) <- 1:nrow(eDF)

##------------------------ MAPE --------------------------

###MAPE

##------------------------ Tableu --------------------------

x <- eDF[c("CodeReg", "Region", "MAPESARIMA", "MAPESARIMAX",  "diffMAPE", "GoogleTrends", "Chomage")]
x <- x[order(-x$Chomage),]
View(x)

##------------------------ Tableu --------------------------
