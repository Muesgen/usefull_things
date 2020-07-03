#installing and loading packages
pck <- c("raster, ggplot2")
for (i in 1:length(pck)){
  if(pck[i] %in% rownames(installed.packages()) == FALSE) {install.packages(pck[i])}
}
lapply(pck, require, character.only = TRUE)