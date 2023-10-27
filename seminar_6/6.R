x = seq(0,5,length.out=20)
y = 1-exp(-x)
y = rnorm(length(y),y,0.03) # шум

plot(y)
plot(x,y)

# parameters
par(mfrow=c(2,3))

# отключить
par(mfrow=c(1,1))
dev.off()

par(mfrow=c(2,3))
?plot
for(t in c('p','l','b','o','h','s')) # S - ступенька через верх
  plot(x,y,type = t,main=t)

plot(x,y,type='n') # пустой график. для заполнения потом

plot(x,y,log='x')
plot(x,y,log='y')
plot(x,y,log='xy') #  значения не имеет

par(mfrow=c(1,1))
?par # lty
plot(1,type='n',xlim=c(0,1),ylim=c(1,6)) # 1 так как надо хоть что-то передать
for(i in 1:6)
  lines(0:1,c(i,i),
        lty=i, 
        lwd=i)
plot(x,y,pch=9)

x = rep(1:5,times=5)
y = rep(5:1,each=5)
plot(x,y,pch=0:24,cex=seq(1,3,length.out=25),xlim=c(1,5.5),ylim=c(1,5.5))
#text(x+0.15,y+0.15,labels = 0:24) при скалиролвании съезжает. если тянуть график
text(x,y,labels = 0:24,adj = c(-0.5, -0.5)) # сдвиг привязан к размеру текста

########

colors()

col = colors()
plot.new()
legend("topleft", fill = col, legend = col, ncol=5)

# hex rgb rgba
plot(1:10,col='tomato3',pch=19)
plot(1:10,col="#FF000050",pch=19,cex=15)

# цвета из дефолтной палитры. Повторения!
plot(1:10,col=1:10,pch=19,cex=15)

RColorBrewer::display.brewer.all()
cols = RColorBrewer::brewer.pal(8,'Set1')
plot(1:8,col=cols[1:8],pch=19,cex=15)
plot(1:8,col=1:8,pch=19,cex=15)

rgb(red=1,green=0.6,blue=0,alpha = 0.5)

col2rgb(cols)

par(mfrow=c(3,3))
x = seq(0,5,length.out=20)
y = 1-exp(-x)
y = rnorm(length(y),y,0.03)
plot(x,y,pch=19,col=c('blue','red'))

cols = rep('blue',length(x))
cols[10] = 'red'
plot(x,y,pch=19,col=cols)
cols = rgb(y^2,0,max(y^2)-y^2,maxColorValue = max(y^2))
plot(x,y,pch=19,col=cols)
plot(x,y,pch=19,col=heat.colors(20))
plot(x,y,pch=19,col=terrain.colors(20))
plot(x,y,pch=19,col=topo.colors(20))
plot(x,y,pch=19,col=rainbow(20))

dev.off()
RColorBrewer::display.brewer.all()
RColorBrewer::brewer.pal.info
cols = RColorBrewer::brewer.pal(8,'Set1')

#####

bgr = colorRamp(c('blue','gray', 'red'))
#bgr = colorRamp(c('blue','green','gray','orange','red'))

bgr(1)
bgr(0.5)
bgr(0)

y
(y-min(y))/(max(y)-min(y))
bgr((y-min(y))/(max(y)-min(y)))

cols = apply(bgr((y-min(y))/(max(y)-min(y))),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))
plot(x,y,pch=19,col=cols)


#####
plot(1,t='n',xlim=c(1,100),ylim=c(1,100))
lines(1:100,90+sin(1:100/10)*10) #type='p'
x = 0:10*10
y = 90+cos(0:10)*10
points(x,y,pch=16) # type='l'

segments(x,y-3,x,y+3)
# OR #
arrows(x,y-3,x,y+3,angle = 90,code=3,length = 0.1)

text(x,y,labels = paste0('x=',x),adj=c(-0.15,0))

polygon(c(0,10,20),c(0,20,0),col='blue',border = 'red')
polygon(c(0,10,20),c(0,20,0),col='orange',border = 'red',density = 10,angle = 30)
polygon(c(0,10,20),c(0,20,0),col='green',border = 'red',density = 20,angle = -30)

a = seq(0,2*pi,length.out=100)
polygon(30+10*sin(a),30+10*cos(a))
rect(40,40,60,60,col='green') # указываем координаты углов

abline(a=0,b=1,lty=3,col='magenta')

abline(v=0:5*20,lty=3,col='gray')
abline(h=0:5*20,lty=3,col='gray')
grid()

#####
plot(1)
par(mfrow=c(2,2),
    tcl=-0.2,
    mgp=c(1.5,0.4,0),
    mar=c(2.5,2.5,1,0),
    oma=c(0,0,1,2),
    las = 0,
    bty = 'n'
    )
plot(1)
mtext("Header",3,line = 0,outer = T)
par(las=2)
plot(1)
par(las=3)
plot(1)
par(xpd=NA)
lines(0:2,0:2)
par(xpd=F)
legend(1.3,1.4,fill=c('red','blue'),legend = c('ttttt','tttttt'),xpd=NA)
par('xpd')

######
dev.off()
par(mfrow=c(2,3),cex=5)
layout.show(6)
par(mfcol=c(2,3),cex=5)
layout.show(6)

m = matrix(c(1,1,2,1,1,2,3,4,4),ncol=3)
m
layout(m)
par(cex=10)
layout.show(4)

layout(m,widths = c(2,2,1))
plot(mtcars$mpg,mtcars$disp)
hist(mtcars$mpg)
barplot(table(mtcars$cyl))
image(cor(mtcars))

x = rnorm(100)
y = rnorm(100,x)
par(fig=c(0,0.8,0,0.8),oma=c(0,0,0,0),mar=c(2,2,0,0))
plot(x,y,pch=19)
par(fig=c(0,0.8,0.8,1),new=TRUE)
boxplot(x,horizontal = T)

par(fig=c(0.8,1,0,0.8),new=TRUE)
boxplot(y,horizontal = F)

par(fig=c(0.4,0.6,0.1,0.3),new=TRUE,bty='n')
hist(x)

######## практика ##########

library(plyr)
baseball[1:2,]
?baseball

r = lapply(split(baseball,baseball$team),function(x){
  data.frame(miny = min(x$year),
             maxy = max(x$year),
             n = nrow(x)
  )
})

r = do.call(rbind,r)
dim(r)
r = r[order(r$miny,r$maxy),]
r[1:10,]
dev.off()

hist(log(r$n),10)


r$n = log(r$n)
plot(1,t='n',xlim=range(r$miny,r$maxy),ylim=c(0,nrow(r)),xlab='Year',ylab='Team',bty='n')
cex = (r$n-min(r$n))/(max(r$n)-min(r$n))
bgr = colorRamp(c('blue','grey','red'))
#col = rgb(cex,0,1-cex)
col=apply(bgr(cex),1,function(x)rgb(x[1],x[2],x[3],maxColorValue = 255))

cex = 0.7 + cex*3
segments(r$miny,1:nrow(r),r$maxy,1:nrow(r),lwd=cex,col=col)


rownames(r)[which(r$miny == min(r$miny[r$maxy >= 2005]) & r$maxy >= 2005)]
r[which(r$miny == min(r$miny[r$maxy >= 2005]) & r$maxy >= 2005),]
