set.seed(24)

n = 10000

###
  
m_normal = rbind(
  n1=rnorm(n, mean = 0, sd = 1),
  n2=rnorm(n, mean = 0, sd = 1),
  n3=rnorm(n, mean = 0, sd = 1),
  n4=rnorm(n, mean = 0, sd = 1),
  n5=rnorm(n, mean = 0, sd = 1)
)

###

m_cancer = rbind(
  o1=rnorm(n, mean = 0, sd = 1),
  o2=rnorm(n, mean = 0, sd = 1),
  o3=rnorm(n, mean = 0, sd = 1),
  o4=rnorm(n, mean = 0, sd = 1),
  o5=rnorm(n, mean = 0, sd = 1)
) 

m_cancer[, 1:500] = m_cancer[, 1:500] + 3

###

m = cbind(t(m_normal), t(m_cancer))

col = c(rep('green', 5), rep('orange', 5))

pca = prcomp(t(m))
plot(pca$x[,1], pca$x[,2], pch=16, xlim=range(pca$x),
     ylim=range(pca$x), col=col)

cr = cor(m, use='pair', method='p')
heatmap(1-cr, symm=T)

mds = cmdscale(1-cr, k=2)
plot(mds, pch=19, col=col)

###

p.values = apply(m, 1, function(x) {
  r = t.test(x[1:5], x[6:10])
  return(r$p.value)
})

fdr = function(d, a) {
  tp = length(d[1:500][d[1:500] < a])
  fp = length(d[501:10000][d[501:10000] < a])
  return(fp / (tp + fp))
}

length(p.values[p.values < 0.05]) # Кол-во: 948
print(fdr(p.values, 0.05)) # FDR = 0.48

###

p.adj.bh = p.adjust(p.values[order(p.values)], method='BH')
n.rej.bh = sum(p.adj.bh < 0.05)
a.adj.bh = 0.05 * n.rej.bh / 10000 

sum(p.values < a.adj.bh) #КОл-во: 158
print(fdr(p.values, a.adj.bh)) #0.03

###

smpl = m[p.values < a.adj.bh, ]
eff = apply(smpl, 1, function(x) {
  mean(x[1:5]) - mean(x[6:10])
})

hist(eff, col='orange', breaks=25)

###

x = which(p.values < a.adj.bh) 

set.seed(25)
  
m_normal_new = rbind(
  n1=rnorm(n, mean = 0, sd = 1),
  n2=rnorm(n, mean = 0, sd = 1),
  n3=rnorm(n, mean = 0, sd = 1),
  n4=rnorm(n, mean = 0, sd = 1),
  n5=rnorm(n, mean = 0, sd = 1)
)
m_cancer_new = rbind(
  o1=rnorm(n, mean = 0, sd = 1),
  o2=rnorm(n, mean = 0, sd = 1),
  o3=rnorm(n, mean = 0, sd = 1),
  o4=rnorm(n, mean = 0, sd = 1),
  o5=rnorm(n, mean = 0, sd = 1)
) 
m_cancer_new[, 1:500] = m_cancer_new[, 1:500] + 3

m2 = cbind(t(m_normal_new), t(m_cancer_new))
p.values2 = apply(m2, 1, function(x) {
  r = t.test(x[1:5], x[6:10])
  return(r$p.value)
})

p.adj.bh2 = p.adjust(p.values2[order(p.values2)], method='BH')
n.rej.bh2 = sum(p.adj.bh2 < 0.05)
a.adj.bh2 = 0.05 * n.rej.bh2 / 10000 

y = which(p.values2 < 0.05)

print(length(
  intersect(x, y)
) / length(x)) #0.94
