weighted.mean(tab[tab$Aufschluss=="333" & tab$Variable=="X",6], tab[tab$Aufschluss=="333" & tab$Variable=="X",5])

wmeans <- as.data.frame(do.call(rbind,
                                lapply(unique(tab$Variable), function(i){
                                  temptab <- tab[tab$Variable==i,]
                                  vartab <- as.data.frame(do.call(rbind, 
                                                                  lapply(unique(temptab$Aufschluss[temptab$Variable==i]), function(l){
                                                                    atab <- temptab[temptab$Aufschluss==l,]
                                                                    atab$wmean <- weighted.mean(temptab[temptab$Aufschluss==l, 6], temptab[temptab$Aufschluss==l, 5])
                                                                    return(atab)
                                                                  })))
                                  vartab <- vartab[, c(1, 7:8)]
                                  vartab <- vartab[!duplicated(vartab),]
                                  return(vartab)
                                })))
wmeans <- wmeans[order(wmeans$Aufschluss),]
