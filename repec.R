library(jsonlite)
library(curl)

#GOOGLE.Sheets.API
##Downloading prepared list of founded publications
article <- fromJSON("https://sheets.googleapis.com/v4/spreadsheets/1ig5g_HT2ZDksA87NMmtTxSQ6s2akLewqed9bCrV6V6A?ranges=JSON-bib!P2:P46&fields=sheets/data/rowData/values/effectiveValue/stringValue&key=AIzaSyBDkgQUbXmlP5nyM64SDo93DqO_aRjeTCs")
article <- data.frame("Handle" = unlist(article$sheets$data[[1]][[1]]), stringsAsFactors = F)

#IDEAS REPEC API
##https://ideas.repec.org/api.html
##https://ideas.repec.org/cgi-bin/getapifunctions.cgi?code=ArG7qVxm
baseurlrepec <- "https://api.repec.org/call.cgi?"
coderepec <- "ArG7qVxm" #the code is now unavailable

#Creating empty dataframes 
##REFERENCES and CITATIONS

repecref <- data.frame("Handle"="", "Year"="", "Title"="", "Author"="", "Source"="", "Keywords"="", "Abstract"="", "Link"="", 
                       "refHandle"="", "refType"="", "refYear"="", "refTitle"="", "refAuthor"="", "refSource"="", "refLink"="",
                       stringsAsFactors = F)
repecref <- repecref[-1,]
repeccit <- data.frame("Handle"="", "Year"="", "Title"="", "Author"="", "Source"="", "Keywords"="", "Abstract"="", "Link"="", 
                       "citHandle"="", "citType"="", "citYear"="", "citTitle"="", "citAuthor"="", "citSource"="", "citLink"="",
                       stringsAsFactors = F)
repeccit <- repeccit[-1,]

##ARTICLES

repec <- data.frame("Handle"="", "Year"="", "Title"="", "Author"="", "Source"="", "Keywords"="", "Abstract"="", "Link"="", 
                     stringsAsFactors = F)
repec <- repec[-1,]

#LOOP ARTICLES INFO
for (i in 1:nrow(article)){
  tryCatch({

    repec0 <- fromJSON(paste0(baseurlrepec, "code=", coderepec, "&getref=", article$Handle[i], "&repecservice=ideas"))      #Provides all variables to constitute a bibliographic reference
    repec1 <- fromJSON(paste0(baseurlrepec, "code=", coderepec, "&getitemreferences=", article$Handle[i], "&repecservice=ideas"))      #Obtain all references
    repec2 <- fromJSON(paste0(baseurlrepec, "code=", coderepec, "&getitemcitations=", article$Handle[i], "&repecservice=ideas"))      #Obtain all citations
        
    if(length(repec0)+length(repec1)+length(repec2) == "0" ) {stop("ERROR: RePeC Page is unavailable ! - ", article$Handle[i])}
    else { 
      
      DF <- data.frame("Handle"=article$Handle[i], 
                       "Year"=gsub('NA', '', paste0(repec0$bibliographic$year, repec0$bibliographic$creation_date)), 
                       "Title"=gsub('NA', '', paste0(repec0$bibliographic$name, repec0$bibliographic$title)), 
                       "Author"=repec0$author, "Source"=repec0$series$name, "Keywords"=repec0$keywords, "Abstract"=repec0$abstract, "Link"=repec0$link, 
                       "refHandle"=repec1$handle, "refType"=gsub('redif-', '', repec1$seriestype), 
                       "refYear"=gsub('NA', '', paste0(repec1$creationdate, repec1$year)), 
                       "refTitle"=gsub('NA', '', paste0(repec1$title, repec1$name)), 
                       "refAuthor"=repec1$author, "refSource"=repec1$series$name, "refLink"=repec1$link,
                       stringsAsFactors = F)
      DF <- DF[!is.na(DF$refHandle),]
      row.names(DF) <- 1:nrow(DF)
      repecref <- rbind(repecref, DF)
      
      DF <- data.frame("Handle"=article$Handle[i], 
                       "Year"=gsub('NA', '', paste0(repec0$bibliographic$year, repec0$bibliographic$creation_date)), 
                       "Title"=gsub('NA', '', paste0(repec0$bibliographic$name, repec0$bibliographic$title)), 
                       "Author"=repec0$author, "Source"=repec0$series$name, "Keywords"=repec0$keywords, "Abstract"=repec0$abstract, "Link"=repec0$link, 
                       "citHandle"=repec2$handle, "citType"=gsub('redif-', '', repec2$seriestype), 
                       "citYear"=gsub('NA', '', paste0(repec2$creationdate, repec2$year)), 
                       "citTitle"=gsub('NA', '', paste0(repec2$title, repec2$name)), 
                       "citAuthor"=repec2$author, "citSource"=repec2$series$name, "citLink"=repec2$link,
                       stringsAsFactors = F)
      DF <- DF[!is.na(DF$citHandle),]
      row.names(DF) <- 1:nrow(DF)
      repeccit <- rbind(repeccit, DF)
      
      rm(DF)
      
      }
  message("Connecting RePeC API ", paste(gsub('NA', '', paste0(repec0$bibliographic$name, repec0$bibliographic$title)), i, "from", nrow(article), sep = " "))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

###YEARS

repecref$Year <- gsub('\"undated\"', '', repecref$Year)
repecref$Year <- as.numeric(repecref$Year)
repecref$refYear <- gsub('\"undated\"', '', repecref$refYear)
repecref$refYear <- as.numeric(repecref$refYear)

repeccit$Year <- gsub('\"undated\"', '', repeccit$Year)
repeccit$Year <- as.numeric(repeccit$Year)
repeccit$citYear <- gsub('\"undated\"', '', repeccit$citYear)
repeccit$citYear <- as.numeric(repeccit$citYear)

#NODES for Graphe

repecgraph <- data.frame("Node1"="", "Node2"="", stringsAsFactors = F)
repecgraph <- repecgraph[-1,]

DF <- data.frame("Node1"=repeccit$citTitle, "Node2"=repeccit$Title, stringsAsFactors = F)
repecgraph <- rbind(repecgraph, DF)
DF <- data.frame("Node1"=repecref$Title[repecref$refYear >= "2004"], "Node2"=repecref$refTitle[repecref$refYear >= "2004"], stringsAsFactors = F)
repecgraph <- rbind(repecgraph, DF)
rm(DF)

###ADD REF and CIT
repecgraph <- rbind(repecgraph, data.frame("Node1"=refadd1$Title, "Node2"=refadd2$refTitle, stringsAsFactors = F))
repecgraph <- rbind(repecgraph, data.frame("Node1"=citadd2$citTitle, "Node2"=citadd1$Title, stringsAsFactors = F))


###Case Sensativity

repecgraph$Node1 <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", repecgraph$Node1, perl=TRUE)
repecgraph$Node2 <- gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", repecgraph$Node2, perl=TRUE)

###Text Errors

repecgraph$Node1 <- gsub('&quot;Google It!&quot;', '"Google It!"', repecgraph$Node1)
repecgraph$Node1 <- gsub('“Google It!”', '"Google It!" ', repecgraph$Node1)
repecgraph$Node1 <- gsub('\'Google It!\'', '"Google It!"', repecgraph$Node1)
repecgraph$Node1 <- gsub('Putcome', 'Outcome', repecgraph$Node1)
repecgraph$Node1 <- gsub('‐', '-', repecgraph$Node1)
repecgraph$Node1 <- gsub(' :', ':', repecgraph$Node1)
repecgraph$Node1 <- gsub('?:', '?', repecgraph$Node1)
repecgraph$Node1 <- gsub('??', '?', repecgraph$Node1, fixed = TRUE)
repecgraph$Node1 <- gsub('Modelling', 'Modeling', repecgraph$Node1)
repecgraph$Node1 <- gsub(', And', ' And', repecgraph$Node1)
repecgraph$Node1 <- gsub('? – On', '? On', repecgraph$Node1)
repecgraph$Node1 <- gsub('-', '', repecgraph$Node1)
repecgraph$Node1 <- gsub('&quot;', '', repecgraph$Node1)
-------------------------------------------------------------------------------------------------
repecgraph$Node2 <- gsub('&quot;Google It!&quot;', '"Google It!"', repecgraph$Node2)
repecgraph$Node2 <- gsub('“Google It!”', '"Google It!" ', repecgraph$Node2)
repecgraph$Node2 <- gsub('\'Google It!\'', '"Google It!"', repecgraph$Node2)
repecgraph$Node2 <- gsub('Putcome', 'Outcome', repecgraph$Node2)
repecgraph$Node2 <- gsub('‐', '-', repecgraph$Node2)
repecgraph$Node2 <- gsub(' :', ':', repecgraph$Node2)
repecgraph$Node2 <- gsub('?:', '?', repecgraph$Node2)
repecgraph$Node2 <- gsub('??', '?', repecgraph$Node2, fixed = TRUE)
repecgraph$Node2 <- gsub('Modelling', 'Modeling', repecgraph$Node2)
repecgraph$Node2 <- gsub(', And', ' And', repecgraph$Node2)
repecgraph$Node2 <- gsub('? – On', '? On', repecgraph$Node2)
repecgraph$Node2 <- gsub('-', '', repecgraph$Node2)
repecgraph$Node2 <- gsub('&quot;', '', repecgraph$Node2)

###UNIQUE

repecgraph <- unique(repecgraph[c("Node1", "Node2")])
repecgraph <- na.omit(repecgraph)
row.names(repecgraph) <- 1:nrow(repecgraph)

###CSV

write.csv(repecgraph, file="repecgraph.csv", row.names=FALSE)
