library("data.table")
library("seqinr")

check_file <- function(infile){
  if(!file.exists(infile)){
    stop(cat(infile, "does not exist.\n"))
  }
}

read_fasta <- function(infile){
  check_file(infile)
  fasta.named.list <- read.fasta(infile) # find better name for variable
  element.size <- -1
  
  for(element in fasta.named.list){
    if(element.size == -1){
      element.size = length(element)
    }
    else{
      if(element.size != length(element)){
        stop("The length of input sequences in the fasta file is different.")
      }
    }
  }
  
  return(fasta.named.list)
}

read_aa <- function(infile){
  check_file(infile)
  dt <- read.csv(file = infile, sep="\t")
  setDT(dt)
  
  return(dt)
}
