install.packages('ape')
library(ape)

s = paste(as.character(
  ape::read.dna("./sequence.fasta", format="fasta")
)[1, ], collapse = "")[1]

sp = 1:(nchar(s) - 5)

# Возьмем вектор стартов и сделаем из него вектор подстрок 
ss = sapply(sp, function(sp) {
  substring(s, sp, sp + 5)
})

# Двы самых редких
print(sort(table(ss))[1:2])

# Два самых частых
print(sort(table(ss), decreasing = TRUE)[1:2])

