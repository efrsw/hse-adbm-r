my.barplot = function(d, t, e, ylim=c(0, max(d+e)), ...) {
  b = barplot(d, ylim=ylim, ...)
  segments(b, d, b, d+e)
  text(b, d, t, adj=c(0.5, 1))
}

set.seed(123)
my.barplot(1:5, t=LETTERS[1:5], e=runif(10, 1, 10), 
           xlab='xlab', ylab='ylab', col='red')

imageWithText = function(d, t=round(d, 2), ... ) {
  x = 1:nrow(d)
  y = 1:ncol(d)
  image(x, y, d, xaxt='n', yaxt='n', ...)
  axis(1, x, rownames(d))
  axis(2, y, colnames(d))
  text(rep(x, times=length(y)),
       rep(y, each=length(x)), t)
}

d <- matrix(1:8, ncol=2)
colnames(d) = c('col1', 'col2')
rownames(d) = paste0("row", 1:4)

par(mfrow=c(1,2))
image(d, col=terrain.colors(100))
imageWithText(d, t=paste0("x=", d), xlab="x", ylab="y", col=rainbow(length(d)))

######

x = rnorm(100)
y = rnorm(length(x), x)
m = rbind(gene1=x, gene2=y)
m

pca = prcomp(t(m))
names(pca)

plot(x, y, pch=19, xlim=range(x,y), ylim=range(x,y))
points(pca$center[1], pca$center[2], cex=2, pch=19, col='red')

pca$rotation

b1 = pca$rotation[2, 1] / pca$rotation[1, 1]
abline(a=pca$center[2]-b1*pca$center[1], b=b1,
       col='red', lwd=2)

b2 = pca$rotation[2, 2] / pca$rotation[1, 2]
abline(a=pca$center[2]-b2*pca$center[1], b=b2,
       col='blue', lwd=2)

plot(pca$x[,1], pca$x[,2], pch=16, xlim=range(pca$x),
     ylim=range(pca$x))


######

n = 5000
k = 10
a = rnorm(n, s=5)
x = rnorm(n, a, sd=0.6)
y = rnorm(n, a, sd=0.6)

d = matrix(rnorm(n*k*2, c(x,y)), ncol=k*2)
d
dim(d)
colnames(d) = paste0(rep(c('c', 't'), times=k),
                     rep(1:k, each=2))
d

pca = prcomp(t(d))

plot(d[1, ], d[2,], pch=16)
plot(pca$x[, 1:2], pch=16)

col = rep(c('orange', 'green'), times=k)
plot(d[1, ], d[2,], pch=16, col=col)
plot(pca$x[, 1:2], pch=16, col=col)

pca$sdev

barplot(pca$sdev)
barplot(pca$sdev / sum(pca$sdev)*100)

########

d2 = exp(d)

hist(d2, ylim=c(0, 50))
hist(log(d2))

pca2 = prcomp(t(d2))
plot(pca2$x[,1:2], pch=16, col=col)

pca2 = prcomp(t(log(d2)))
plot(pca2$x[,1:2], pch=16, col=col)

pca3 = prcomp(t(d2), scale. = T)
plot(pca3$x[,1:2], pch=16, col=col)

#######

cr = cor(d, use='pair', method='p')
cr

image(cr)
heatmap(1-cr, symm=T)

mds = cmdscale(1-cr, k=2)
plot(mds, pch=19, col=col)

cr = cor(d2, use='pair', method='sp')
mds = cmdscale(1-cr, k=2)
plot(mds, pch=19, col=col)

plot(d[,1:2])
plot(d[,1:2], log='xy')
plot(rank(d2[,1]), rank(d2[,2]))

x = c(rnorm(100), 1:10)
y = c(rnorm(100), 1:10)
plot(x,y)
cor(x,y, m='p')
cor(x,y, m='sp')

x = c(rnorm(100), 10)
y = c(rnorm(100), 10)
plot(x,y)
cor(x,y, m='p')
cor(x,y, m='sp')

#######

cancer = 1
healthy = rnorm(15, 0)

hist(healthy)
abline(v=cancer, col='red', lwd=2)

table(healthy >= cancer)
c = sum(healthy >= cancer)
t = length(healthy)

pbinom(c, t, prob = 0.5)

binom.test(c,t, 0.5)
c/t

bt = binom.test(c, t, 0.5, alternative = 'l')
bt
typeof(bt)
names(bt)
bt$p.value
bt$estimate
bt$conf.int

#########

cancer = rnorm(15, 1)
healthy = rnorm(15, 0)

ustat = function(x, y) {
  sum(sapply(x, function(z) {sum(z > y)}))
} 

ustat(cancer, healthy)

wilcox.test(cancer)
wt = wilcox.test(cancer, healthy)
names(wt)
wt$p.value
wt$statistic

wilcox.test(cancer, healthy, alternative = 'g')

##########

wilcox.test(cancer, healthy,
            alternative = 'g', paired=T)

x = rnorm(150)
y = rnorm(150, x+0.3)
wilcox.test(x, y)
wilcox.test(x, y, paired = T)

#############

cancer = rnorm(15, 1)
healthy = rnorm(15, 0)

t.test(cancer, healthy)

tt = t.test(cancer, healthy)#, alternative = 'g', paired = T)
names(tt)
tt$statistic

mean(cancer)/mean(healthy)
tt$estimate[1] / tt$estimate[2]

log2(mean(cancer)/mean(healthy))
log2(tt$estimate[1] / tt$estimate[2])

tt$estimate[1] - tt$estimate[2]

t.test(log2(cancer), log2(healthy))

########

pv = sapply(1:10000, function(i) {t.test(rnorm(10), rnorm(10))$p.value})
table(pv < 0.05)
hist(pv)
table(pv < 0.001)

# (1 - (1-alpha)^N) ~ N*alpha
# 0.05 = N*alpha
# alpha = 0.05/N

sum(pv < 0.05/length(pv))

pv.adj = pmin(1, pv*length(pv))
sum(pv.adj < 0.05)

d = data.frame(indx=1:length(pv), pv=pv)
d
d[1:2, ]
d = d[order(d$pv), ]
d[1:2,]

d$fp = d$pv * length(pv)
d$p = 1:nrow(d)
d[1:5,]
d$fdr = d$fp / d$p

plot(d$pv, d$fdr, t='l')

d$fdr = pmin(1, rev(cummin(rev(d$fdr))))
plot(d$pv, d$fdr, t='l')

d = d[order(d$indx),]
d[1:5,]

table(p.adjust(pv, method = 'bonf') == pv.adj)
table(p.adjust(pv, method = 'fdr') == d$fdr)
plot(p.adjust(pv, method = 'fdr'), d$fdr)
abline(a=0, b=1, col='red', lwd=2)

hist(p.adjust(pv, method = 'fdr') - d$fdr)