install.packages('ape')
library(ape)

seq = ape::read.dna("C:/dev/hse-adbm-r/seminar_4/sequence.fasta", format="fasta")
s = paste(as.character(seq)[1, ], collapse = "")[1]
rm(seq)
gc()
nchar(s)

nucs = c("a", "t", "c", "g")
mx = 0
mn = 5132068
mxseq = ""
mnseq = ""

fn("")

fn = function(cw) {
  if(nchar(cw) == 6) {
    t = co(s, cw)
    if(t > mx) {
      mx = t
      mxseq = cw
    }
    if(t < mn) {
      mn = t
      mnseq = cw
    }
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
