install.packages('ape')
library(ape)

seq = ape::read.dna("./sequence.fasta", format="fasta")
nucs = c("A", "T", "C", "G")

maxSeq <- ""
minSeq <- ""
maxOcc <- 0
minOcc <- length(seq)

fn = function(cw) {
  if(nchar(cw) == 6) {
    co()
    return;
  }
  
  for(i in 1:4) {
    cw = paste0(cw, nucs[i])
    fn(cw)
    cw = substr(cw, 1, nchar(cw) - 1)
  }
}

co = function(st, let) {
  m = gregexpr(let, st)
  return(sum(unlist(m) != -1))
}

