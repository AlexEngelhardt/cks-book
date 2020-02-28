source("setup.R")

################################################################
#### Diskrete Gleichverteilung

png(file.path(img_folder, "verteilungen-diskrete-gleichverteilung-dichte.png"))
plot(1:6, rep(1/6, 6), xlim=c(0, 7), ylim=c(0,0.5), col=1, pch=19, main="Dichte der Zufallsvariablen\nX = Ergebnis eines Würfelwurfs", xlab="x: Ergebnis", ylab="f(x)")
for(xx in 1:6){
    lines(c(xx,xx), c(0, 1/6), col=1, lwd=2)
}
dev.off()

x <- seq(-1, 8, by=0.01)
F <- function(x){
    Fx <- rep(NA, times=length(x))
    Fx <- floor(x)/6
    Fx[x<1] <- 0
    Fx[x>=6] <- 1
    return(Fx)
}

png(file.path(img_folder, "verteilungen-diskrete-gleichverteilung-verteilungsfunktion.png"))
plot(x, F(x), type="l", main="Verteilungsfunktion der Zufallsvariable\nX = Ergebnis eines Würfelwurfs", xlab="x: Ergebnis", ylab="F(x)", lwd=2, col=2)  
points(x=c(1:6), y=c((1:6)/6), pch=19, col=2)
dev.off()


################################################################
#### Bernoulliverteilung

p <- 0.2

png(file.path(img_folder, "verteilungen-bernoulliverteilung-dichte.png"))
plot(0:1, c(1-p, p), xlim=c(-0.5, 1.5), ylim=c(0,1), pch=19, col=1, main="Dichte f(x) der Bernoulliverteilung mit p=0.2", ylab="f(x)", xlab="x")
lines(c(0,0), c(0, 1-p), lwd=2, col=1)
lines(c(1,1), c(0, p), lwd=2, col=1)
dev.off()

png(file.path(img_folder, "verteilungen-bernoulliverteilung-verteilungsfunktion.png"))
plot(c(-0.5,1.5), c(-0.1,1.1), type="n", main="Verteilungsfunktion F(x) der Bernoulliverteilung mit p=0.2", ylab="F(x)", xlab="x")
lines(c(-0.5,0), c(0,0), col=2)
lines(c(0,1), c(1-p,1-p), col=2)
lines(c(1,1.5), c(1,1), col=2)
points(c(0,1), c(1-p, 1), pch=19, col=2)
points(c(0,1), c(0, 1-p), col=2)
dev.off()

################################################################
#### Binomialverteilung

p <- 0.2
n <- 6

x <- 0:n
png(file.path(img_folder, "verteilungen-binomialverteilung-dichte.png"))
plot(x, dbinom(x, size=n, prob=p), pch=19, col=1, main=paste0("Dichte der Binomialverteilung mit n=", n, " und p=", p), xlab="x", ylab="f(x)")
for(xi in x){
    lines(c(xi, xi), c(0, dbinom(xi, size=n, prob=p)), lwd=2, col=1)
}
dev.off()

rbind(x, round(dbinom(x,n,p), 4))

png(file.path(img_folder, "verteilungen-binomialverteilung-verteilungsfunktion.png"))
plot(x, pbinom(x, size=n, prob=p), pch=19, col=2, main=paste0("Verteilungsfunktion der Binomialverteilung mit n=", n, " und p=", p), xlab="x", ylab="F(x)", ylim=c(0,1))
for(xi in x){
    lines(c(xi, xi+1), c(pbinom(xi,n,p), pbinom(xi,n,p)), lwd=2, col=2)
    points(xi+1, pbinom(xi,n,p), col=2)
}
dev.off()

################################################################
#### Hypergeometrische Verteilung

N <- 15  # Anzahl Kugeln
M <- 5   # Anzahl "Treffer"
miss <- N-M  # dhyper() braucht nicht (N,M) sondern (M,miss)
n <- 4   # Anzahl gezogene Kugeln

x <- 0:n

png(file.path(img_folder, "verteilungen-hypergeometrische-verteilung-dichte.png"))
plot(x, dhyper(x, M, miss, n), pch=19, col=1, main=paste0("Dichte der hypergeometrischen Verteilung\nmit N=",N,", M=",M," und n=", n), xlab="x", ylab="f(x)", xlim=c(-1,5))
for(xi in x){
    lines(c(xi, xi), c(0, dhyper(xi, M, miss, n)), lwd=2, col=1)
}
dev.off()

png(file.path(img_folder, "verteilungen-hypergeometrische-verteilung-verteilungsfunktion.png"))
plot(x, phyper(x, M, miss, n), pch=19, col=2, main=paste0("Verteilungsfunktion der hypergeometrischen Verteilung\nmit N=",N,", M=",M," und n=", n), xlab="x", ylab="F(x)", xlim=c(-1,5))
for(xi in x){
    lines(c(xi, xi+1), c(phyper(xi,M,miss,n), phyper(xi,M,miss,n)), lwd=2, col=2)
    points(xi+1, phyper(xi,M,miss,n), col=2)
}
dev.off()

################################################################
#### Geometrische Verteilung

p <- 1/8

x <- 0:40

png(file.path(img_folder, "verteilungen-geometrische-verteilung-dichte.png"))
plot(x, dgeom(x-1, p), pch=19, col=1, main=paste0("Dichte der geometrischen Verteilung\nmit p=",p), xlab="x", ylab="f(x)")
for(xi in x){
    lines(c(xi, xi), c(0, dgeom(xi-1, p)), lwd=2, col=1)
}
dev.off()

png(file.path(img_folder, "verteilungen-geometrische-verteilung-verteilungsfunktion.png"))
plot(x, pgeom(x-1, p), pch=19, col=2, main=paste0("Verteilungsfunktion der geometrischen Verteilung\nmit p=",p), xlab="x", ylab="F(x)", ylim=c(0,1))
## for(xi in x){
##     lines(c(xi, xi+1), c(pgeom(xi,p), pgeom(xi,p)), lwd=2, col=2)
##     points(xi+1, pgeom(xi,p), col=2)
## }
dev.off()

################################################################
#### Poissonverteilung

lambda <- 1.2

x <- 0:7

png(file.path(img_folder, "verteilungen-poissonverteilung-dichte.png"))
plot(x, dpois(x, lambda), pch=19, col=1, main=paste0("Dichte der Poissonverteilung mit lambda=", lambda), xlab="x", ylab="f(x)")
for(xi in x){
    lines(c(xi, xi), c(0, dpois(xi, lambda)), lwd=2, col=1)
}
dev.off()

png(file.path(img_folder, "verteilungen-poissonverteilung-verteilungsfunktion.png"))
plot(x, ppois(x, lambda), pch=19, col=2, main=paste0("Verteilungsfunktion der Poissonerteilung\nmit lambda=",lambda), xlab="x", ylab="F(x)", ylim=c(0,1))
for(xi in x){
    lines(c(xi, xi+1), c(ppois(xi,lambda), ppois(xi,lambda)), lwd=2, col=2)
    points(xi+1, ppois(xi,lambda), col=2)
}
dev.off()


################################################################
#### Stetige Gleichverteilung

a <- 0
b <- 10

x <- seq(a-2, b+2, by=0.01)

png(file.path(img_folder, "verteilungen-stetige-gleichverteilung-dichte.png"))
plot(x, dunif(x,a,b), ylim=c(0, 0.12), type="l", xlab="x", ylab="f(x)", col=1, lwd=2,
     main=paste0("Dichte der stetigen Gleichverteilung\nmit a=",a," und b=",b))
dev.off()

png(file.path(img_folder, "verteilungen-stetige-gleichverteilung-verteilungsfunktion.png"))
plot(x, punif(x,a,b), type="l", ylim=c(0,1.2), xlab="x", ylab="F(x)", col=2, lwd=2,
     main=paste0("Verteilungsfunktion der stetigen Gleichverteilung\nmit a=",a," und b=",b))
dev.off()

################################################################
#### Exponentialverteilung

lambda <- 0.5

x <- seq(-1, 8, by=0.01)

png(file.path(img_folder, "verteilungen-exponentialverteilung-dichte.png"))
plot(x, dexp(x, lambda), type="l", xlab="x", ylab="f(x)", col=1, lwd=2, main=paste0("Dichte der Exponentialverteilung\nmit lambda=", lambda))
dev.off()

png(file.path(img_folder, "verteilungen-exponentialverteilung-verteilungsfunktion.png"))
plot(x, pexp(x, lambda), type="l", xlab="x", ylab="F(x)", ylim=c(0,1.05), col=2, lwd=2, main=paste0("Verteilungsfunktion der Exponentialverteilung\nmit lambda=", lambda))
dev.off()

################################################################
#### Normalverteilung

x <- seq(-10, 10, by=0.01)

png(file.path(img_folder, "verteilungen-normalverteilung-mu-sigma.png"), width=2*480)
par(mfrow=c(1,2))
plot(x, dnorm(x, -3, 2), type="l", ylab="f(x)", col=1, lwd=2,
     main=paste0("Drei Normalverteilungen mit verschiedenen Mittelwerten\n und gleicher Varianz"))
lines(x, dnorm(x, 0, 2), col=2, lty=2, lwd=2)
lines(x, dnorm(x, 5, 2), col=3, lty=3, lwd=2)
legend("topleft", legend=c(expression(mu==-3),expression(mu==0),expression(mu==5)), lty=1:3, col=1:3)


plot(x, dnorm(x, 0, 1), type="l", ylab="f(x)", col=1, lwd=2,
     main=paste0("Drei Normalverteilungen mit verschiedenen Varianzen\n und gleichem Mittelwert"))
lines(x, dnorm(x, 0, 2), col=2, lty=2, lwd=2)
lines(x, dnorm(x, 0, 4), col=3, lty=3, lwd=2)
legend("topleft", legend=c(expression(sigma==1),expression(sigma==2),expression(sigma==4)), lty=1:3, col=1:3)
dev.off()

#### Sigma-Intervalle

mu <- 0
var <- 1
sd <- sqrt(var)
##
x <- seq(mu-4*sd, mu+4*sd, by=0.01)

png(file.path(img_folder, "verteilungen-normalverteilung-skizze.png"))
plot(x, dnorm(x, mean=mu, sd=sd), type="l", ylab="f(x)", main="")
##
x_1 <- mu-sd
x_2 <- mu+sd
y_1 <- dnorm(x_1, mean=mu, sd=sd)
lines(x=c(x_1,x_2), y=c(y_1,y_1), lty=2)
text(0, y_1+0.01, expression(mu %+-% 1*sigma))
lines(x=c(x_1,x_1), y=c(y_1-0.01, y_1+0.01), lty=2)
lines(x=c(x_2,x_2), y=c(y_1-0.01, y_1+0.01), lty=2)
##
x_1 <- mu-2*sd
x_2 <- mu+2*sd
y_1 <- dnorm(x_1, mean=mu, sd=sd)
lines(x=c(x_1,x_2), y=c(y_1,y_1), lty=2)
text(0, y_1+0.01, expression(mu %+-% 2*sigma))
lines(x=c(x_1,x_1), y=c(y_1-0.01, y_1+0.01), lty=2)
lines(x=c(x_2,x_2), y=c(y_1-0.01, y_1+0.01), lty=2)
##
x_1 <- mu-3*sd
x_2 <- mu+3*sd
y_1 <- dnorm(x_1, mean=mu, sd=sd)
lines(x=c(x_1,x_2), y=c(y_1,y_1), lty=2)
text(0, y_1+0.01, expression(mu %+-% 3*sigma))
lines(x=c(x_1,x_1), y=c(y_1-0.01, y_1+0.01), lty=2)
lines(x=c(x_2,x_2), y=c(y_1-0.01, y_1+0.01), lty=2)
dev.off()

#### Standardisierung

z <- seq(-6, 6, by=0.01)
x <- seq(-6, 6, by=0.01)
mu <- 4

png(file.path(img_folder, "verteilungen-normalverteilung-standardisierung.png"), width=2*480, height=0.5*480)
par(mfrow=c(1,2))
plot(x, dnorm(x, mu, 1), type="l", xlim=c(-6,6), ylab="f(x)", main="P(X <= 3)")
legend("topleft", legend="X~N(4,1)")
idx <- x<3
polygon(x=c(x[idx], x[sum(idx)]), y=c(dnorm(x[idx], mu, sd),0), col=1)
##
plot(z, dnorm(z, 0, 1), type="l", xlim=c(-6,6), ylab="f(z)", main="P(Z <= -1)")
legend("topleft", legend="Z~N(0,1)")
idx <- z < -1
polygon(x=c(z[idx], z[sum(idx)]), y=c(dnorm(z[idx], 0, sd),0), col=1)
##
dev.off()

#### Verteilungsfunktion

png(file.path(img_folder, "verteilungen-normalverteilung-verteilungsfunktion.png"))
x <- seq(-4,4, by=0.01)
plot(x, pnorm(x), type="l", ylab="F(x)", main="Verteilungsfunktion der Normalverteilung", col=1, lwd=2)
dev.off()

#### Verteilungstabelle

png(file.path(img_folder, "verteilungen-normalverteilung-verteilungstabelle-1.png"), height=0.5*480)
x <- seq(-3,3, by=0.01)
plot(x, pnorm(x), type="l", xlab="z", ylab=expression(Phi(z)), main="Verteilungsfunktion der Normalverteilung", lty=2, col=1)
idx <- x>0
lines(x[idx], pnorm(x[idx]), lwd=2, col=1)
##abline(v=0)
dev.off()

png(file.path(img_folder, "verteilungen-normalverteilung-verteilungstabelle-2.png"), height=0.5*480)
plot(x, pnorm(x), type="l", ylab=expression(Phi(z)), xlab="z", main=expression(Phi(-1.5) == 1-Phi(1.5)), col=1, lwd=2)
lines(c(1.5,1.5), c(0, pnorm(1.5)))
lines(c(-1.5,-1.5), c(pnorm(-1.5), 1))
tickwidth <- 0.1
lines(c(1.5-tickwidth,1.5+tickwidth), c(0, 0))
lines(c(1.5-tickwidth,1.5+tickwidth), c(pnorm(1.5), pnorm(1.5)))
lines(c(-1.5-tickwidth,-1.5+tickwidth), c(1,1))
lines(c(-1.5-tickwidth,-1.5+tickwidth), c(pnorm(-1.5), pnorm(-1.5)))
dev.off()

#### Aufgabe ~N
mu <- 175
var <- 100
sd <- sqrt(var)
##
x <- seq(mu-4*sd, mu+4*sd, by=0.01)

## a)
png(file.path(img_folder, "verteilungen-normalverteilung-aufgabe-a.png"), height=0.5*480)
plot(x, dnorm(x, mean=mu, sd=sd), type="l", ylab="f(x)", main="", col=1, lwd=2)
dev.off()

## b)
png(file.path(img_folder, "verteilungen-normalverteilung-aufgabe-b.png"), height=0.5*480)
plot(x, dnorm(x, mean=mu, sd=sd), type="l", ylab="f(x)", main="", col=1, lwd=2)
idx <- x<mu
polygon(c(x[idx], mu), c(dnorm(x[idx], mu, sd), 0), col=1)
dev.off()

## c)
png(file.path(img_folder, "verteilungen-normalverteilung-aufgabe-c.png"), height=0.5*480)
plot(x, dnorm(x, mean=mu, sd=sd), type="l", ylab="f(x)", main="", col=1, lwd=2)
x <- seq(180, 190, length.out=100)
poly_x <- c(180, x, 190)
polygon(poly_x, c(0, dnorm(x, mu, sd), 0), col=1)
dev.off()

################################################################
#### t-Verteilung

x <- seq(-4, 4, by=0.01)
dfs <- c(2, 5, 10, 20, 30)

png(file.path(img_folder, "verteilungen-t-verteilung-normalverteilung.png"))
plot(x, dnorm(x), ylab="f(x)", type="l", lwd=2, main="Verschiedene t-Verteilungen und die Normalverteilung")
for(i in seq_along(dfs)){
    df <- dfs[i]
    lines(x, dt(x, df), col=i)
}
legend("topleft",
       legend=c(
           "N(0,1)",
           "t(2)",
           "t(5)",
           "t(10)",
           "t(20)",
           "t(30)"
       ),
       col=c("black", palette()[1:5]),
       lwd=c(2,1,1,1,1,1)
       )
dev.off()


dfs <- c(2, 5, 10)
png(file.path(img_folder, "verteilungen-t-verteilung-normalverteilung-verteilungsfunktion.png"), height=0.5*480)
plot(x, pnorm(x), ylab="F(x)", type="l", lwd=2, main="Verschiedene t-Verteilungen und die Normalverteilung")
for(i in seq_along(dfs)){
    df <- dfs[i]
    lines(x, pt(x, df), col=i)
}
legend("topleft",
       legend=c(
           "N(0,1)",
           "t(2)",
           "t(5)",
           "t(10)"
       ),
       col=c("black", palette()[1:5]),
       lwd=c(2,1,1,1,1,1)
       )
dev.off()

#### Verteilungstabelle

xlim <- 6
x <- seq(-xlim, xlim, by=0.01)
df <- 4

png(file.path(img_folder, "verteilungen-t-verteilung-verteilungstabelle.png"))
par(mfrow=c(2,1))
plot(x, dt(x, df), type="l", ylab="f(x)", main=paste("Dichtefunktion der t-Verteilung\nmit", df, "Freiheitsgraden"))
Q <- qt(0.975, df)
poly_x <- c(Q, Q, x[x>Q], xlim)
polygon(poly_x, c(0, dt(Q, df), dt(x[x>Q], df), 0), col=1)
polygon(-poly_x, c(0, dt(Q, df), dt(x[x>Q], df), 0), col=1)
poly_x <- c(-Q, -Q, x[x > -Q & x < Q], Q, Q)
polygon(poly_x, c(0, dt(Q, df), dt(x[x > -Q & x < Q], df), dt(Q, df), 0), col=2)
text(0, 0.10, "95%", cex=1)
text(Q, 0.03, "2.5%", cex=1, pos=4)
text(-Q, 0.03, "2.5%", cex=1, pos=2)
plot(x, pt(x, df), type="l", ylab="F(x)", main=paste("Verteilungsfunktion der t-Verteilung\nmit", df, "Freiheitsgraden"))
lines(x=c(Q, Q, -xlim), y=c(0, 0.975, 0.975), lty=2)
lines(x=c(-Q, -Q, -xlim), y=c(0, 0.025, 0.025), lty=2)
text(Q, 0, paste0("x=",round(Q, 3)), pos=4)
text(-Q, 0, paste0("x=",-round(Q, 3)), pos=4)
text(-xlim, 0.85, "F(x)=0.975", pos=4)
text(-xlim, 0.1, "F(x)=0.025", pos=4)
dev.off()

################################################################
#### Chi-Quadrat-Verteilung

#### Chisq-Tabelle
qs <- c(0.05, 0.10, 0.20, 0.3, 0.5, 0.7, 0.8, 0.9, 0.95, 0.99, 0.999)
for(df in 1:30){
    cat(
paste(c('<tr><td style="text-align: center;"><strong>', df, '</strong></td>',
       paste0(           '<td>', sprintf("%.3f", round(qchisq(qs, df), 3)), '</td>'       ),
       '</tr>'        ), collapse="")        )
}

#### Dichte und Verteilung

png(file.path(img_folder, "verteilungen-chisq-verteilung-dichte-und-verteilungsfunktion.png"))
par(mfrow=c(2,1))

x <- seq(0.01, 10, by=0.01)
dfs <- c(2, 3, 4, 5)
plot(x, dchisq(x, 1), type="l", col=1, main="Dichtefunktionen der Chi-Quadrat-Verteilung", ylab="f(x)", ylim=c(0,1), lwd=2)
for(i in seq_along(dfs)){
    col <- i+1
    df <- dfs[i]
    lines(x, dchisq(x, df), col=col, lwd=2)
}
legend("topright", lty=1, legend=paste(c(1, dfs), c("Freiheitsgrad", rep("Freiheitsgrade", length(dfs)))), col=1:(1+length(dfs)), lwd=2)

x <- seq(0.01, 10, by=0.01)
dfs <- c(2, 3, 4, 5)
plot(x, pchisq(x, 1), type="l", col=1, main="Verteilungsfunktionen der Chi-Quadrat-Verteilung", ylab="F(x)", ylim=c(0,1), lwd=2)
for(i in seq_along(dfs)){
    col <- i+1
    df <- dfs[i]
    lines(x, pchisq(x, df), col=col, lwd=2)
}
legend("bottomright", lty=1, legend=paste(c(1, dfs), c("Freiheitsgrad", rep("Freiheitsgrade", length(dfs)))), col=1:(1+length(dfs)), lwd=2)

dev.off()

#### Ablesehilfe Verteilungstabelle

png(file.path(img_folder, "verteilungen-chisq-verteilung-verteilungstabelle.png"))
par(mfrow=c(2,1))
df <- 5
Q <- qchisq(0.95, df)
xlim=30
x <- seq(0, xlim, by=0.01)
plot(x, dchisq(x, df), type="l", ylab="f(x)", main=paste0("95%-Quantil der Chi-Quadrat-Verteilung\nmit ", df, " Freiheitsgraden: ", round(Q, 3)))
poly_x <- c(Q, Q, x[x>Q], xlim)
polygon(poly_x, c(0, dchisq(Q, df), dchisq(x[x>Q], df), 0), col=1)
text(Q+2, 0.015, "5% Fläche", pos=4)

plot(x, pchisq(x, df), type="l", ylab="F(x)", main=paste0("Verteilungsfunktion\nF(",round(Q,3),") = 0.95"))
lines(c(Q, Q, 0), c(0, 0.95, 0.95), lty=2)

dev.off()
