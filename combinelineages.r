#To use this script:
#Rscript combinelineages.r --offset=<n> TnTn+1.csv Tn+1Tn+2.csv ... Tn+k-1Tn+k.csv
#Where <n> is the offset of the first time-point (0 by default), and TiTi+1.csv are the pair-wise lineages between successive timepoints
#Pairwise lineages between all time-points are output at T_i_j.csv

args = commandArgs(trailingOnly = TRUE)
#On by default, turn off to include incomplete lineages
remove_incomplete_lineages <- TRUE



offset <- 0
parent_maps <- list()
for(i in 1:length(args)){
#  print(args)
  if(grepl(".csv",args[[i]],fixed=TRUE))
    parent_maps[[length(parent_maps)+1]] <- read.csv(args[i],sep=",",header=T)
  if(grepl("--offset=",args[[i]],fixed=TRUE)){
      offset <- as.numeric(strsplit(args[[i]],"=")[[1]][2])
#      print(strsplit(args[[i]],"=")[[1]][2])
  }
}
print(length(parent_maps))
print(offset)
if(length(parent_maps)<2)
  stop("At least two consecutive parent maps must be provided")

while(length(parent_maps)>1){

#parent_maps <- list(T2T3,T3T4,T4T5,T5T6,T6T7)
lineage_maps <- list()
lineage_maps[1] <- parent_maps[1]

for(i in 2:length(parent_maps))
{
  lineage_maps[i] <-parent_maps[i]
  for(j in 1:length(parent_maps[[i]]$Label)){
    plabel <- parent_maps[[i]][j,]$Parent.Label
    llabel <- lineage_maps[[i-1]][lineage_maps[[i-1]]$Label == plabel, ]$Parent.Label
    if(length(llabel)>0)
      lineage_maps[[i]][j,]$Parent.Label <- llabel
    else
      lineage_maps[[i]][j,]$Parent.Label <- NA
  }    
  
}

incomplete_lineages <- list()
lin_to_remove <-list()
if(remove_incomplete_lineages){
  for(i in 2:length(lineage_maps))
  {
    for(j in 1:length(lineage_maps[[1]]$Parent.Label)){
      plabel <- lineage_maps[[1]][j,]$Parent.Label
      previous_clones <- lineage_maps[[i-1]][!is.na(lineage_maps[[i-1]]$Parent.Label) & lineage_maps[[i-1]]$Parent.Label == plabel,]$Label
      previous_clones <- unique(unlist(previous_clones))
      clone_matching <- match(previous_clones,parent_maps[[i]]$Parent.Label)
      if(anyNA(clone_matching)){
        incomplete_lineages <- list(unlist(incomplete_lineages),plabel)
#        print(unique(unlist(incomplete_lineages)))
      }
    }       
    lin_to_remove[[i-1]] <- unique(unlist(incomplete_lineages))
#    print(length(unique(lineage_maps[[1]]$Parent.Label) )-length(unique(incomplete_lineages)))
#    print(length(unique(lineage_maps[[i]]$Parent.Label)))
  } 
}


length(lineage_maps[[1]]$Parent.Label )
incomplete_lineages <- unique(incomplete_lineages[[1]])

for(i in 1:length(lineage_maps)){
  lineage_maps[[i]] <- lineage_maps[[i]][!is.na(lineage_maps[[i]]$Parent.Label), ]
}

for(i in 2:length(lineage_maps)){
  lineage_maps[[i]] <- lineage_maps[[i]][!is.na(lineage_maps[[i]]$Parent.Label), ]
  if(remove_incomplete_lineages){
    lineage_maps[[i]] <- lineage_maps[[i]][!(lineage_maps[[i]]$Parent.Label %in% lin_to_remove[[i-1]]),]
  }
}

#print(lin_to_remove)

for(i in 1:length(lineage_maps))
    write.csv(lineage_maps[[i]], paste("T",toString(0+offset),"_",toString(i+offset),".csv", sep=""), quote = FALSE, row.names = FALSE)

offset <- offset+1
parent_maps <- parent_maps[2:length(parent_maps)]
}
write.csv(parent_maps[1], paste("T",toString(0+offset),"_",toString(0+offset+1),".csv",sep=""), quote = FALSE, row.names = FALSE)
