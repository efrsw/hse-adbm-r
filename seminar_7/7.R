library(plyr)

d = lapply(1:4, function(x) rnorm(100,x))
d

boxplot(d)
boxplot(baseball$year ~ baseball$team)

d = split(baseball$year, baseball$team)
d
boxplot(d[order(sapply(d, mean))])

x = runif(5, 10, 30)
sort(x)
order(x)

d = lapply(1:4, function(x) rnorm(100,x))
boxplot(d[[2]], d[[1]], rnorm(100, 10))
d[[2]][10] = 300
boxplot(d, col=rainbow(4))
boxplot(d, col=rainbow(4), outline=F, notch=T,
        horizontal=T)

d1 = lapply(1:4, function(x) rnorm(100, x+5))
boxplot(d, col=rainbow(4), outline=F, notch=T,
        horizontal=F, ylim=range(unlist(d,d1)))
boxplot(d, col=rainbow(4), outline=F, notch=T,
        horizontal=F, ylim=c(0,10))
boxplot(d1, add=T, col=rainbow(4), outline=F,
        notch=T)

############################

x = c(rnorm(1000, 0), rnorm(1000, 3))
hist(x)
hist(x, breaks=100)

h = hist(x, breaks=100)
h$breaks

hist(x, seq(-4, 6.5, by=0.2))
hist(x, seq(-2, 2, by=0.2))
h = hist(x, seq(min(x), max(x), length.out=100))
h$breaks

x = rnorm(1000, 0)
y = rnorm(1000, 3)

hist(x)
hist(y, add=T)

range(x, y)
breaks = seq(-3.5, 6.5, by=0.1)
hist(x, breaks, col='#FF000080',
     border=NA, freq=F)
hist(y, breaks, add=T, col='#00FF0080',
     border='black', freq=F)

h1 = hist(x, breaks, col='#FF000080',
          border=NA, freq=F, plot=F)
h2 = hist(y, breaks, add=T, col='#00FF0080',
            border='black', freq=F, plot=F)

hist(x, breaks, col='#FF000080',
     border=NA, freq=F,
     ylim=range(0, h1$density, h2$density))
hist(y, breaks, add=T, col='#00FF0080',
     border='black', freq=F)

h1$counts
h1$density

############################

barplot(1:3)

x = c(rnorm(1000, 0), rnorm(1000, 3))
t = table(rep(c('g1', 'g2'), each=1000), round(x))
t

barplot(t)
barplot(t, beside=T)

barplot(sweep(t, 2, apply(t, 2, sum), '/')*100)

b = barplot(t, beside=T, col=c('red', 'blue'),
        legend.text=c('groupA', 'groupB'),
        args.legend=list(title='Group', x='topleft'))
text(b, t, t)
#куда писать, координата на графике, сам текст

barplot(t(t), beside=F)

pie(t[1,])

##############

x = 0:10
y = exp(x) - 1

plot(x, y)
plot(x, y, log='y')

plot(x, y+1, log='y')

f = function(x) {log(x+1)}

plot(x, f(y), yaxt='n')
at = c(0, 10, 100, 1000, 1e4)
axis(2, at=f(at), labels=at)
axis(4, at=f(at), labels=at)

#################

x = rnorm(1e4, 0)
y = round(rnorm(1e4, x)*2, 0)
x = round(x*2)

t = table(x, y)
t

image(t)
image(t, col=heat.colors(100))
image(t, breaks=c(0, 10, 50, 1000),
      col=c('red', 'green', 'blue'))
image(t, zlim=c(0, 1000))
image(t, zlim=range(t))

xx = 1:nrow(t)
yy = 1:ncol(t)
image(xx, yy, t, xaxt='n', yaxt='n')
axis(1, xx, rownames(t))
axis(2, yy, colnames(t))

#######################

c = cor(mtcars)
image(c)
cl = c[1,] > 0

h = heatmap(c, symm=T,
            distfun=function(x) as.dist(1-c),
            RowSideColors=ifelse(cl,'red','blue'))
h

heatmap(c[h$rowInd, h$colInd], symm=T,
        distfun=function(x) as.dist(1-c),
        RowSideColors=ifelse(cl[h$rowInd], 'red', 'blue'),
        Rowc=NA, Colv=NA)

##############

x = seq(from=0, 5, length.out=100)
y = 1 - exp(-x)
y = rnorm(length(y), y, 0.1)

plot(x, y, pch=16)
lines(x, y, col='grey', lwd=3)
lines(x, smooth(y), col='black', lwd=3)

m = smooth.spline(x, y, df=2)
m
p = predict(m)
lines(p, col='red', lwd=3)
lines(predict(smooth.spline(x, y, df=7)),
      col='green', lwd=3)

x2 = x^2
m = lm(y ~ x + x2)
lines(x, predict(m), col='blue', lwd=3)

#####################

plot(1, 1, xlim=c(0, 1))
text(0, 1, 'A', adj=c(0,1))
x1 = grconvertX(0, 'npc', 'user')
y1 = grconvertY(1, 'npc', 'user')
text(x1, y1, 'A', adj=c(0,1), col='red', cex=7)

x2 = grconvertX(0, 'nfc', 'user')
y2 = grconvertY(1, 'nfc', 'user')
text(x2, y2, 'B', adj=c(0,1), col='red', cex=7,
     xpd=T)