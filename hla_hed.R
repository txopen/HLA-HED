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
      element.size <- length(element)
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

calculate_distance <- function(hla1, hla2, sequence, distance){
  seq_hla1 <- sequence[hla1]
  seq_hla2 <- sequence[hla2]

  if(is.null(seq_hla1[[1]])){
    stop(cat(hla1, "does not exist in sequence."))
  }

  if(is.null(seq_hla2[[1]])){
    stop(cat(hla2, "does not exist in sequence."))
  }

  seq_len = length(seq_hla1[[1]])
  dis <- 0
  
  for(i in 1:seq_len){
    aa1 = seq_hla1[[1]][[i]]
    aa2 = seq_hla2[[1]][[i]]
    dis = dis + distance[X == toupper(aa1), c(toupper(aa2)), with = FALSE][[1]]
  }

  dis = dis / seq_len
  return(dis)
}
