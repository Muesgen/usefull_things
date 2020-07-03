### Skript zur Extraction von Multiband Rastern ###

#Benoetigte Bibliotheken laden und ggbfls. installieren, einfach ausfuehren :)
x <- c("raster", "ncdf", "ncdf4")
for (i in 1:length(x)){
  if(x[i] %in% rownames(installed.packages()) == FALSE) {install.packages(x[i])}
}
lapply(x, require, character.only = TRUE)
# Je nach R Version die du installiert hast, geht ncdf oder ncdf4 nicht. Hauptsache 1 von beiden ist nach dem Check drauf

# Daten einladen
#Aufpassen, dass die / richtig rum sind. Weiß nicht was du benutzt, aber bei WIndows müssen die in / geändert werden.
Rasterfilenames <- c("T_2M_AV_daymax") # hier nur die Namen der Raster ohne Datenformat angeben, also ohne ".nc"
data_directory <- ("C:/Users/mmues/Desktop/MIMI/Data/") # hier den Pfad angeben wo deine Daten im Ordner liegen
results_directory <- ("C:/Users/mmues/Desktop/MIMI/Results/") # hier den Pfad angeben wo die fertigen CSV dateien gespeichert werden sollen

#FUnktion zum extrahieren. Wenn die fertig ist sollten die CSV Datein im Results ordner sein
suppressWarnings({for (l in 1:seq(length(Rasterfilenames))){

raster_read <- raster::stack(paste0(data_directory, Rasterfilenames[l], ".nc"))
data_extract <- as.data.frame(raster_read)
write.csv2(data_extract, file = paste0(results_directory, Rasterfilenames[l],"_extract.csv"), sep = ".")
}
})
# extrahiert wird von links nach rechts. also Nr 30 in der csv ist dann das 30te pixel von links in de ersten reihe beim test raster
# nummer 31 ist dann das erte von links in Reihe 2


# kleiner Kommentar: normaler weise müssen Multiband Raster aufwaendiger extracted werden aber da du .nc daten hast greift 
# das Paket Raster automatisch auf ncdf oder ncdf4, je nach R version zu, und vollzieht die Umwandlung von Arrays automatisch
# so lässt sich ganz einfach ein dataframe erstellen. Falls du Koorodinaten oder sonst irgendwas dabei brauchst, sag mal bescheid, 
# dann muesste man manuel ueber ncdf4 eingreifen.

