#' Combine two phyDat objects saved as RData
#' @param phyDat1 First phyDat object saved as .RData
#' @param phyDat2 Second phyDat object saved as .RData
#' @param outfile Prefix for output files

combinePhyDat<-function(phyDat1,phyDat2,outfile){
  
  require(phangorn)
  require(stringr)
  
  #load(phyDat1) 
  load("dna_phyDat.Lin2_1.RData")
  phydat1<-dna_phydat
  #load(phyDat2)
  load("dna_phyDat.Lin2_2.RData")
  phydat2<-dna_phydat
  rm(dna_phydat)
  index1<-attr(phydat1,"index")
  index2<-attr(phydat2,"index")
  
  joined_index<-paste0(index1,":",index2)
  finalhaplo<-unique(joined_index)
  newindex<-match(joined_index, unique(joined_index))
  newweight <- as.integer(table(joined_index))
  newnames<-c(names(phydat1),names(phydat2))
  
  first_element<-as.integer(vapply(strsplit(finalhaplo,":"), `[`, 1, FUN.VALUE=character(1)))
  for (i in 1:length(phydat1)){
    newhaplo<-c(phydat1[[i]][first_element[1:length(first_element)]])
    phydat1[[i]]<-newhaplo
  }
  
  second_element<-as.integer(vapply(strsplit(finalhaplo,":"), `[`, 2, FUN.VALUE=character(1)))
  for (i in 1:length(phydat2)){
    newhaplo<-c(phydat2[[i]][second_element[1:length(second_element)]])
    phydat1[[names(phydat2)[i]]]<-newhaplo
  }
  
  attr(phydat1,"weight")<-newweight
  attr(phydat1,"index")<-newindex
  attr(phydat1,"nr")<-length(phydat1[[1]])
  save(phydat1, file =paste0(outfile,'.RData'))
}
