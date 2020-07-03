##Skript for cleaning Data. It solves the Problem that are multiple features in multiple cells in a dataset. Also filter a big data set##

# Funktion zum Überprüfen von Bibliotheken. EInfach die Bibliotheken bei X angeben und alles wird automatisch installiert oder eingeladen

x <- c("dplyr", "vroom")
for (i in 1:length(x)){
  if(x[i] %in% rownames(installed.packages()) == FALSE) {install.packages(x[i])}
}
lapply(x, require, character.only = TRUE)

### Es gibt deutlich schnellere Wege CSV UND TEXT Dateien einzuladen. DIe deutlich schnellste ist vroom: 4 sekunden

data <- vroom(file = 'C:/Users/mmues/Desktop/LocSigDB.csv', delim = ",")

# Daten Filtern um den Datensatz zu verkleinern und Datenformat an Code anpassen
df_filter <- dplyr::filter(data, grepl('Homo sapiens', Organism))
df_filter$Organism <- gsub(';', ',', df_filter$Organism)
df_filter$Organism <- strsplit(df_filter$Organism, ",")
df_filter$Uniprot_IDs <- gsub(';', ',' ,df_filter$Uniprot_IDs)
df_filter$Uniprot_IDs <- strsplit(df_filter$Uniprot_IDs,",")

# Mehrfachattribute zählen
times <- sapply(df_filter$Organism,length)

single <- data.frame()
multi <- data.frame()
#Fortschritt Statusbalken erstellen
pb <- txtProgressBar(1, nrow(df_filter), style = 3)
for (i in 1:nrow(df_filter)){
  setTxtProgressBar(pb, i) # Fortschritt anzeigen lassen
  # Alle Einzel genannten Attribute werden in einen data frame gepackt 
  if (times[i] == 1 ){
    temp1 <- data.frame(df_filter$Signal[i],df_filter$Proteins[i],df_filter$Localization[i],
                       df_filter$Pubmed_IDs[i],df_filter$Uniprot_IDs[i], df_filter$Organism[i])
    names(temp1) <- c("Signal", "Proteins", "Localization", "Pubmed_IDs", "Uniprot_IDs", "Organism")
    single <- rbind(single, temp1)
    names(single) <- c("Signal", "Proteins", "Localization", "Pubmed_IDs", "Uniprot_IDs", "Organism")
}
  else if (times[i] > 1){
    # Mehrfachnennungen werden nach Anzahl der Nennungen aufgesplittet und dem single Dataframe ebenfalls angefügt
    for (j in 1:times[i]){
      if (df_filter$Organism[[i]][j] == "Homo sapiens"){
      temp2 <- data.frame(df_filter$Signal[i],df_filter$Proteins[i],df_filter$Localization[i],
                         df_filter$Pubmed_IDs[i],df_filter$Uniprot_IDs[[i]][j], df_filter$Organism[[i]][j])
      names(temp2) <- c("Signal", "Proteins", "Localization", "Pubmed_IDs", "Uniprot_IDs", "Organism")
      multi <- rbind(multi, temp2)
      names(multi) <- c("Signal", "Proteins", "Localization", "Pubmed_IDs", "Uniprot_IDs", "Organism")
      }
    }
  }
  } 
# alle EInzel homo sapiens in einen data frame packen und die anderen EInträge verwerfen
df_final <- rbind(single, multi)
write.csv(df_final, file = 'C:/Users/mmues/Desktop/data.csv')
