source("setup.R")
source("einevariable-functions.R")

odd <- c(1,2,4,4.5,5)
even <- c(1,3.2,4.2,5)

png(file.path(img_folder, "median.png"), height=480)
op <- par(mfrow=c(2,1))
mediandots(odd, titlestring="n ungerade")
mediandots(even, titlestring="n gerade")
par(op)
dev.off()

int <- c(1,2.3,4,4.5,5)
frac <- c(1,2.3,2.7,3.4,4.2,5)

png(file.path(img_folder, "quantile.png"), height=480)
op <- par(mfrow=c(2,1))
quantiledots(int, titlestring="n*p ganzzahlig", q=0.2)
quantiledots(frac, titlestring="n*p nicht ganzzahlig", q=0.2)
par(op)
dev.off()

png(file.path(img_folder, "spannweite.png"), height=240)
set.seed(2)
x <- rnorm(10)
spannweite(x)
dev.off()

png(file.path(img_folder, "interquartilsabstand.png"), height=240)
iqr(x)
dev.off()

png(file.path(img_folder, "lorenzkurve.png"))
  plotlorenz(EK, main="Lorenzkurve")
  lines(c(0.8,0.8), c(-1, LC$L[5]), lty=3) # c(fromX, toX), c(fromY, toY)
  lines(c(-1, 0.8), c(LC$L[5], LC$L[5]), lty=3)
dev.off()

png(file.path(img_folder, "vierlorenzkurven.png"))
op <- par(mfrow=c(2,2))
plotlorenz(EK, main="(a) eine normale Lorenzkurve")
EK2 <- c(1,2,2,30,59)
plotlorenz(EK2, main="(b) etwas unfairer")
EK3 <- c(0,0,0,0,10)
plotlorenz(EK3, main="(c) eine Person besitzt alles")
EK4 <- c(2,2,2,2,2)
plotlorenz(EK4, main="(d) perfekte faire Gleichverteilung")
par(op)
dev.off()

png(file.path(img_folder, "aufgabe-lorenzkurve.png"))
EKa <- c(20, 100, 160, 220, 250, 310, 330)
plotlorenz(EKa, xlab="Anteil der Autounternehmen", ylab="Anteil am Umsatz")
dev.off()

png(file.path(img_folder, "gini-veranschaulichung.png"))
op <- par(mfrow=c(2,2))
plotlorenz(EK, main="(a) eine normale Lorenzkurve", area="over")
EK2 <- c(1,2,2,30,59)
plotlorenz(EK2, main="(b) etwas unfairer", area="over")
EK3 <- c(0,0,0,0,10)
plotlorenz(EK3, main="(c) eine Person besitzt alles", area="over")
EK4 <- c(2,2,2,2,2)
plotlorenz(EK4, main="(d) perfekte faire Gleichverteilung", area="over")
par(op)
dev.off()

png(file.path(img_folder, "gini-schritte.png"), width=800, height=800)
op <- par(mfrow=c(2,2))
LC <- plotlorenz(EK)
for(i in 1:length(EK)){
  polygon(c(LC$p[i], LC$p[i+1], LC$p[i+1], LC$p[i]),
          c(0, 0, LC$L[i+1], LC$L[i]), col=i)
}
##
plot.new()
plot.window(xlim=c(0,100), ylim=c(10,80))
title(expression(A==frac(h[1]+h[2], 2) %.% b)) # see ?plotmath
polygon(c(15,15,35,35), c(20,50,70,20), col=1)
lines(c(15, 15), c(50, 60), lty=2)
lines(c(15, 35), c(60, 60), lty=2)
text(10, 35, expression(h[1]))
text(40, 35, expression(h[2]))
text(25, 15, expression(b))
text(50, 45, "=", cex=4)
polygon(c(65,65,85,85), c(20,60,60,20), col=4)
text(60, 35, expression(frac(h[1]+h[2], 2)))
text(90, 35, expression(frac(h[1]+h[2], 2)))
text(75, 15, expression(b))
##
plot(c(0,1), c(0,1), type="n", 
     xlab="Anteil der Bevölkerung", ylab="Anteil des Einkommens")
lines(c(0,1), c(0,1), lty=2)
polygon(c(0,1,1), c(0,1,0), col=3)
text(0.2, 0.8, expression(A==frac(1,2)), cex=1.5)
##
plot(c(0,1), c(0,1), type="n", 
     xlab="Anteil der Bevölkerung", ylab="Anteil des Einkommens")
lines(c(0,1), c(0,1), lty=2)
polygon(c(0,0.8,1), c(0,0,1), col=3)
text(0.2, 0.8, expression(A==frac(n-1,2*n)), cex=1.5)
par(op)
dev.off()


png(file.path(img_folder, "konzentrationsflaeche.png"), height=260, width=720)
cuts <- c(0.3, 0.35, 0.65, 0.7)
smx <- matrix(c(0, cuts[1], 0, 1,
                cuts[1], cuts[2], 0, 1,
                cuts[2], cuts[3], 0, 1,
                cuts[3], cuts[4], 0, 1,
                cuts[4], 1, 0, 1
                ), byrow=T, ncol=4)
split.screen(smx)
screen(2)
text(0.5, 0.5, "-", cex=10)
png(file.path(img_folder, "histogramm-fix.png"))
  set.seed(280989)
  x <- round(rnorm(20, 170, 10))
  hist(x, main="Histogramm der Körpergröße von ")
dev.off()
screen(4)
text(0.5, 0.5, "=", cex=10)

screen(1)
plotlorenz(1, xlab="", ylab="", area="under", points=FALSE)

screen(3)
plotlorenz(EK, area="under", xlab="", ylab="", points=FALSE)

screen(5)
plotlorenz(EK, area="over", xlab="", ylab="", points=FALSE)

close.screen(all.screens=TRUE)
dev.off()


png(file.path(img_folder, "balkendiagramm.png"), height=960)
op <- par(mfrow=c(2,1))
s <- c(rep(1,20),rep(2,4),rep(3,13),rep(4,9),rep(5,21),rep(6,5),rep(7,8))
ts <- table(s)
fs <- ts/80
es <- ecdf(s)
Es <- es(1:7) * 80
barplot(ts, main="absolute Häufigkeiten der Semester", xlab="Semester", col=1)
barplot(fs, main="relative Häufigkeiten der Semester", xlab="Semester", col=1)
par(op)
dev.off()

png(file.path(img_folder, "verteilungsfunktion.png"))
plot(es, verticals=TRUE, main="Verteilungsfunktion des Merkmals \"Semester\"")
dev.off()

png(file.path(img_folder, "verteilungsfunktion-beispiel.png"))
plot(ecdf(c(1,2,3,5,7)), verticals=TRUE, main="")
lines(c(-2,1), c(.2,.2), lty=3)
dev.off()

png(file.path(img_folder, "boxplot1.png"))
  ## n=20 fix! Schiefe und parts der Values auch!
  d <- 10-c(9,6,7,7,3,9,10,1,8,7,9,9,8,10,5,10,10,9,10,8)
  boxplot(d)
dev.off()

png(file.path(img_folder, "histogramm-fix.png"), width=960)
  set.seed(280989)
  x <- round(rnorm(20, 170, 10))
  par(mfrow=c(1,2))
  hist(x, right=FALSE, breaks=c(150,160,170,180,190,200), freq=FALSE, main="Histogramm der Körpergröße von 20 Personen", col=1, ylab="Dichte", xlab="Körpergröße in cm")
  hist(x, right=FALSE, breaks=c(150,160,170,180,190,200), main="Histogramm der Körpergröße von 20 Personen", col=2, ylab="Anzahl Personen", xlab="Körpergröße in cm")
dev.off()

png(file.path(img_folder, "histogramm-variabel.png"))
  hist(x, right=FALSE, breaks=c(140, 160, 165, 190, 200), main="Histogramm der Körpergröße von 20 Personen", col=3, ylab="Dichte", xlab="Körpergröße in cm")
dev.off()

set.seed(20141111)
cm <- round(rnorm(5, 172, 9))
m <- cm/100
mean(m)
var(m)
sd(m)
sd(m)/mean(m)
png(file.path(img_folder, "variationskoeffizient.png"), width=480)
par(mfrow=c(1,2))
boxplot(m, main="Körpergrösse in Meter", ylab="Körpergrösse (m)", col=1)
boxplot(cm, main="Körpergrösse in Zentimetern", ylab="Körpergrösse (cm)", col=2)
dev.off()
