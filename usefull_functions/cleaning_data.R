### cleaning function for Data with more than one feature in a cell
x=df
clean <- function(x) {
  require(tidyr)
  
  x <- separate(x, col=3, into = c("Ort", "Administration", "Zusatz"), sep=", ")
  
  for (i in seq(nrow(x))) {
    if(!is.na(x[i,"Zusatz"])) {
      x[i, ] <- x[i, c(1:3,5,4,6:ncol(x))]
    }
  }
  
  for (r in seq(nrow(x))) {
    if(is.na(x$Administration[r]) &
       grepl("Kreis", tolower(x$Ort[r]))){
      x$Administration[r] <-"Landkreis"
    }
  }
  
  x$Administration [is.na(x$Administration) & nchar(as.character(x$ID) == 2)] <- "Bundesland"
  x$Administration [x$ID == "DG"] <- "Land"
  
  assign("area_clean", x,envir = .GlobalEnv)
}

clean(df)