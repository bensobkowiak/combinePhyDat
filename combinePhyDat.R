#' Combine two phyDat objects saved as RData
#' @param phyDat1 First phyDat object saved as .RData
#' @param phyDat2 Second phyDat object saved as .RData
#' @param outfile Prefix for output files

combinePhyDat<-function(phyDat1,phyDat2,outfile){
  
  require(phangorn)
  
  load(phyDat1) 
  phydat1<-dna_phydat
  load(phyDat2)
  phydat2<-dna_phydat
  
  index1<-attr(phydat1,"index")
  index2<-attr(phydat2,"index")
  
  phydat1_list<-paste0(as.character(lapply(phydat1, `[[`, 1)),collapse = ",")
  for (i in 2:max(index1)){
    phydat1_list<-c(phydat1_list,paste0(as.character(lapply(phydat1, `[[`, i)),collapse = ","))
  }
  phydat2_list<-paste0(as.character(lapply(phydat2, `[[`, 1)),collapse = ",")
  for (i in 2:max(index2)){
    phydat2_list<-c(phydat2_list,paste0(as.character(lapply(phydat2, `[[`, i)),collapse = ","))
  }
  newlist<-list()
  for (i in 1:length(phydat2_list)){
    newlist<-c(newlist,paste0(phydat1_list[index1[i]],",",phydat2_list[index2[i]]))
  }
  finallist<-unique(newlist)
  newindex<-rep(0,length(index1))
  for (i in 1:length(finallist)){
    newindex[which(grepl(finallist[i],newlist)==T)]<-i
  }
  new_weights<-rep(0,length(finallist))
  for (i in 1:length(new_weights)){
    new_weights[i]<-length(which(grepl(finallist[i],newlist)==T))
  }
  newnames<-c(names(phydat1),names(phydat2))
  finallist<-t(matrix(unlist(lapply(finallist,function(x) (strsplit(x, ",")))),
                      ncol=length(newnames),byrow = T))
  row.names(finallist)<-newnames
  new.list <- setNames(split(as.integer(finallist), seq(nrow(finallist))), rownames(finallist))
  for (i in 1:length(phydat1)){
    phydat1[[i]]<-new.list[[i]]
  }
  for (i in length(phydat1)+1:length(phydat2)){
    phydat1[[newnames[i]]]<-new.list[[i]]
  }
  attr(phydat1,"weight")<-new_weights
  attr(phydat1,"index")<-newindex
  attr(phydat1,"nr")<-length(phydat1[[1]])
  save(phydat1, file =paste0(outfile,'.RData'))
}
