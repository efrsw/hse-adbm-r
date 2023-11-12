my.barplot = function(d, t, e, col, border) {
  b = barplot(d, col=col, border=border, ylim=c(0, max(d) + max(e) + 0.1))
  text(b, d - 0.2, t)
  for(i in 1:length(d)) {
    lines(c(b[i], b[i]), c(d[i], d[i] + e[i]))
  }
}

my.barplot(rnorm(5, 10, 1), c('A', 'B', 'C', 'D', 'E'), runif(5, max = 3), 'red', 'black')

###

d = matrix(1:12, ncol=4)
colnames(d) = c('c1', 'col2', 'c3', 'col4')
rownames(d) = paste0('row', 1:3)

my.image = function(m, t, xlab, ylab, col) {
  image(m, col=col, axes=F, xlab=xlab, ylab=ylab)
  
  xx = seq(0, 1, length.out=length(rownames(m)))
  yy = seq(0, 1, length.out=length(colnames(m)))
  
  tc = expand.grid(xx, yy)
  
  text(x = tc[, 1], y = tc[, 2], labels = t)
  
  axis(1, at=xx, labels=rownames(m))
  axis(2, at=yy, labels=colnames(m))
}

par(mfrow=c(1,2))
image(d, col=terrain.colors(100))
my.image(d, paste0('x=', 12:23), xlab='Cols', ylab='rows', col=terrain.colors(100))
