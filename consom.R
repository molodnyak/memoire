#Indice des prix а la consommation - Base 2015 - Ensemble des ménages
#France métropolitaine - Nomenclature Coicop : 
#03.1.2 - Vêtements
#Identifier 001763962

#------------------------ Données ------------------------------------------------

library(htmltab)
inseeurl <- "http://www.bdm.insee.fr/bdm2/affichageSeries?"
inseeperiod <- paste("periodeDebut=1", "anneeDebut=2009", "periodeFin=3", "anneeFin=2017", sep = "&")
idbank <- "001763962"
consommation <- htmltab(paste0(inseeurl, inseeperiod, "&recherche=criteres&codeGroupe=1744&idbank=", idbank), 1)
names(consommation) <- c("Year", "Month", "Vetements")
consommation$Vetements <- as.numeric(consommation$Vetements)
consommation$Date <- rev(seq(as.Date("2009-02-01"), length=nrow(consommation), by="month")-1)
consommation <- data.frame("Year"=consommation$Year, "Month"=consommation$Month, "Date"=consommation$Date, "Vetements"=consommation$Vetements, stringsAsFactors = FALSE)

#------------------------ Données ------------------------------------------------

#SARIMA(X) Prévision pour chaque de 12 dernières périodes

DFc <- data.frame("Date"='', "Month"='', "Model"='', "x"='', "nobs"='', "aicSARIMA"='', "lowerSARIMA"='', "fSARIMA"='', 
                 "upperSARIMA"='', "aicSARIMAX"='', "lowerSARIMAX"='', "fSARIMAX"='', "upperSARIMAX"='', "Vetements"='', 
                 stringsAsFactors = F)
DFc <- DFc[-1,]
library(forecast)

for (i in month.abb){
  
  file_names <- paste0("C:/Users/Evgenii Molodniak/Documents/Master 2/Memoire/Google/Soldes/", i, '.csv')
  ###encoding="WINDOWS-1252"
  gtperiod <- do.call(cbind, lapply(file_names, read.csv, stringsAsFactors = F, encoding="UTF-8"))
  ##formatting
  gtperiod <- gtperiod[-1,]
  gtperiod <- data.frame(as.numeric(gtperiod), stringsAsFactors = F)
  names(gtperiod) <- "GT"
  gtperiod <- data.frame("Date"=seq(as.Date("2009-02-01"), length=nrow(gtperiod), by="month")-1, gtperiod, stringsAsFactors = F)

  ## Merge GT Soldes and CONSOMMATION Vetements
  gtperiod <- merge(gtperiod[c("Date", "GT")], consommation[c("Date","Vetements")], 
                    by=c('Date'), all.y=TRUE)

  ##Forecast Date
  fDATE <- gtperiod$Date[length(gtperiod$Date[!is.na(gtperiod$GT)])]
  
    tryCatch({
      #SARIMA
      x <- ts(gtperiod$Vetements[gtperiod$Date < fDATE], start=c(2009, 1), frequency=12)
      sarima <- auto.arima(x)
      fsarima <- forecast(sarima, h=1)
      ##orders from auto.arima(x)
      orderx <- as.numeric(unlist(strsplit(regmatches(fsarima$method, gregexpr("(?<=\\().*?(?=\\))", fsarima$method, perl=T))[[1]][1], ",")))
      seasonalx <- as.numeric(unlist(strsplit(regmatches(fsarima$method, gregexpr("(?<=\\().*?(?=\\))", fsarima$method, perl=T))[[1]][2], ",")))
      #SARIMAX
      xx <- ts(gtperiod$GT[gtperiod$Date < fDATE], start=c(2009, 1), frequency=12)
      ##xreg - external regressor
      sarimax <- Arima(x, xreg = xx, order = orderx, seasonal = seasonalx, 
                       include.drift = grepl("drift", fsarima$method), method="ML")
      ##xreg - future value for external regressor
      fsarimax <- forecast(sarimax, xreg = gtperiod$GT[gtperiod$Date == fDATE])
      ##bind dataframe
      DFc <- rbind(DFc, data.frame("Date"=as.Date(fDATE), "Month"=i,
                                 "Model"=fsarima$method, "x"=length(sarima$x), "nobs"=sarima$nobs, 
                                 "aicSARIMA"=sarima$aic, "lowerSARIMA"=fsarima$lower[2], "fSARIMA"=fsarima$mean[1], 
                                 "upperSARIMA"=fsarima$upper[2], 
                                 "aicSARIMAX"=sarimax$aic, "lowerSARIMAX"=fsarimax$lower[2], "fSARIMAX"=fsarimax$mean[1], 
                                 "upperSARIMAX"=fsarimax$upper[2],
                                 "Vetements"=gtperiod$Vetements[gtperiod$Date == fDATE], 
                                 stringsAsFactors = F))
      
      message("Auto Arima modelling for month ", i)})
}

rownames(DFc) <- 1:nrow(DFc)
DFc <- DFc[order(DFc$Date),]


#PLOTS

##------------------------ Graphique --------------------------

  plot(DFc$Date, DFc$fSARIMA, 
       col="#dc3912", pch=1, lwd=3, main="Vêtements", ylab="Indice de prix", xlab="", xaxt = "n")
  
  lines(DFc$Date, DFc$lowerSARIMA, col="#dc3912", lwd=1, lty=2)
  lines(DFc$Date, DFc$upperSARIMA, col="#dc3912", lwd=1, lty=2)
  
  lines(DFc$Date, DFc$fSARIMAX, col="#3366cc", type = "p", pch=1, lwd=3)
  lines(DFc$Date, DFc$lowerSARIMAX, col="#3366cc", lwd=1, lty=2)
  lines(DFc$Date, DFc$upperSARIMAX, col="#3366cc", lwd=1, lty=2)
  
  lines(DFc$Date, DFc$Vetements, col="#109618", lwd=3)
  abline(v = as.Date("2017-01-15"))
  legend("bottomleft", legend=c("SARIMA","SARIMAX","Consommation"), fill=c("#dc3912","#3366cc","#109618"))

  axis(1, at = DFc$Date, labels = DFc$Month)

##------------------------ Graphique --------------------------
