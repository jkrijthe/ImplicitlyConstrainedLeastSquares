library(calibrate)

pdf(file = "ConstrainedProblem-1D.pdf",width=7,height=4)
par(mgp=c(1,0,0))
plot(seq(-1.5,1,0.01),(seq(-1.5,1,0.01)*2+1)^2+1,'l',cex.lab=2,axes=FALSE,ylab="Loss",xlab=expression(beta),lwd=3,asp=1,xlim=c(-0.5,1),ylim=c(0.0,2))
axis(1, pos=0,col=24,labels=FALSE,lwd=1)
axis(2, pos=-1.5,col=24,labels=FALSE,lwd=1)
lines(c(0.0,1.5), c(0,0),'l',lwd=5)

points(-0.5,0,cex=2,pch=16)
textxy(-0.5, 0,expression(hat(beta)[sup]),cex=2)
       
points(1,0,cex=2,pch=16)
textxy(1,0,expression(beta["*"]),cex=2)  

points(0.0,0,cex=2,pch=16)
textxy(0.0,0,expression(hat(beta)[semi]),cex=2) 

lines(c(-0.5,-0.5),c(0,1),lty=2,lwd=3)
lines(c(-0.0,-0.0),c(0,(0*2+1)^2+1),lty=2,lwd=3)

dev.off()


d<-1
r<-0.9
library(ggplot2)
plot(1:100,sapply(1:100, function(d){1^d-r^d}),'l',xlab="Dimensionality",ylab="Proportion of volume in outer layer")

plot(1:100,sapply(1:100, function(r){(pi*r^2)/(2*pi*r)}),'l')
lines(1:100,sapply(1:100, function(r){((4/3)*pi*r^3)/(4*pi*r^2)}),'l')