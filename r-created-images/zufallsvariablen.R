source("setup.R")
source("zufallsvariablen-functions.R")
set.seed(20140814)

lambda <- 3

x <- 0:10

################################################################
#### Diskrete ZV

#### Nebeneinander: Dichte und zugehörige Verteilungsfunktion
png(file.path(img_folder, "zufallsvariablen-dichte-verteilung.png"), width=2*480)
op <- par(mfrow=c(1,2))
plot_pois_dens(x, lambda, E=TRUE, main="Ausschnitt einer Dichte")
plot_pois_dist(x, lambda, q=0.3, main="Ausschnitt der zugehörigen Verteilungsfunktion")
par(op)
dev.off()

#### Eine Dichte mit hoher, eine mit niedriger Varianz
lambda1 <- 2
lambda2 <- 6
x <- 0:15

png(file.path(img_folder, "zufallsvariablen-varianz.png"))
op <- par(mfrow=c(2,1))
plot_pois_dens(x, lambda1, E=TRUE, main="Dichte einer Zufallsvariablen mit niedriger Varianz", ylim=c(0, 0.3))
plot_pois_dens(x, lambda2, E=TRUE, main="Dichte einer Zufallsvariablen mit hoher Varianz", ylim=c(0, 0.3))
par(op)
dev.off()

################################################################
#### Stetige ZV

x <- seq(0, 4, by=.01)
shape <- 2
rate <- 3

png(file.path(img_folder, "zufallsvariablen-stetig-dichte-verteilung.png"), width=2*480)
op <- par(mfrow=c(1,2))
plot_gamma_dens(x, shape, rate, E=TRUE, main="Dichte einer stetigen Zufallsvariablen")
plot_gamma_dist(x, shape, rate, q=0.3, main="Die zugehörige Verteilungsfunktion")
par(op)
dev.off()

png(file.path(img_folder, "zufallsvariablen-stetig-integral.png"))
plot_gamma_dens(x, shape, rate, area=c(1,2))
dev.off()

png(file.path(img_folder, "zufallsvariablen-dichte-zu-integral-1.png"), width=2*480)
par(mfrow=c(1,2))
plot_gamma_dens(x,shape,rate,area=c(0,1.5), main=expression({plain(P)(X <= 1.5) == plain(F)(1.5)} == 0.939))
plot_gamma_dist(x, shape, rate, q=0.939)
dev.off()

png(file.path(img_folder, "zufallsvariablen-dichte-zu-integral-2.png"), width=2*480)
par(mfrow=c(1,2))
plot_gamma_dens(x,shape,rate,area=c(0,0.5), main=expression({plain(P)(X <= 0.5) == plain(F)(0.5)} == 0.442))
plot_gamma_dist(x, shape, rate, q=0.442)
dev.off()

png(file.path(img_folder, "zufallsvariablen-dichte-zu-integral-3.png"), width=2*480)
par(mfrow=c(1,2))
plot_gamma_dens(x,shape,rate,area=c(0.5,1.5),
              main=expression({plain(P)({0.5 <= X} <= 1.5) == plain(F)(1.5)-plain(F)(0.5)} == 0.497))
plot_gamma_dist(x, shape, rate, q=c(0.442, 0.939))
text(0, 0.69, "}", cex=13)
text(0.5, 0.69, "0.497", cex=1.5)
dev.off()

################################################################
#### Stetige Dichten, Bespielaufgabe

f <- function(x) 2*x * (x>=0) * (x<=1)
x <- seq(-0.5, 1.5, by=0.01)
fx <- f(x)
png(file.path(img_folder, "zufallsvariablen-stetige-dichte.png"))
plot(x, fx, type="l", ylab="f(x)", main="Dichtefunktion von X")
dev.off()

Fx <- 0*x * (x<=0) +
    x^2 * (x>0) * (x<=1) +
    1 * (x>1)
png(file.path(img_folder, "zufallsvariablen-stetige-verteilungsfunktion.png"))
plot(x, Fx, type="l", ylab="F(x)", main="Verteilungsfunktion von X")
dev.off()

x2 <- x[x<=1 & x>=0]
Qx <- sqrt(x2)
png(file.path(img_folder, "zufallsvariablen-stetige-quantilssfunktion.png"))
plot(x2, Qx, type="l", ylab="Q(x)", main="Quantilsfunktion von X")
dev.off()

png(file.path(img_folder, "zufallsvariablen-stetige-wahrscheinlichkeit.png"), width=2*480)
op <- par(mfrow=c(1,2))
xx <- seq(0.5, 0.6, by=.01)
plot(x, fx, type="l", ylab="f(x)", main="Dichtefunktion von X")
polygon(x=c(xx[1], xx, xx[length(xx)]),
        y=c(0, f(xx), 0),
        col=1)
text(0.7, 0.5, "0.11")

plot(x, Fx, type="l", ylab="F(x)", main="Verteilungsfunktion von X")
q <- 0.6^2
lines(x=c(-0.5, sqrt(q)),
      y=c(q, q), lty=2)
lines(x=c(sqrt(q), sqrt(q)),
      y=c(q, 0), lty=2)
q <- 0.5^2
lines(x=c(-0.5, sqrt(q)),
      y=c(q, q), lty=2)
lines(x=c(sqrt(q), sqrt(q)),
      y=c(q, 0), lty=2)
text(x=-0.5, y=mean(c(0.5^2, 0.6^2)), "}", cex=3)
text(x=-0.4, y=mean(c(0.5^2, 0.6^2)), "0.11")
par(op)
dev.off()

################################################################
#### Unabhängigkeit zweier ZV

set.seed(20141026)

n <- 100

png(file.path(img_folder, "zufallsvariablen-unabhaengige-ZV.png"))
op <- par(mfrow=c(2,2))
x <- runif(n, 0, 10)
y <- rpois(n, 4)
plot(x, y, main="(a)")
x <- rpois(n, 2)
y <- rpois(n, 4)
plot(x, y, main="(b)")
x <- runif(n, 0, 10)
y <- runif(n,0,10)
plot(x, y, main="(c)")
x <- rnorm(n, 0, 10)
y <- rnorm(n,0,10)
plot(x, y, main="(d)")
par(op)
dev.off()

png(file.path(img_folder, "zufallsvariablen-abhaengige-ZV.png"))
x <- runif(n, -2, 2)
op <- par(mfrow=c(2,2))
y <- 3*x + rnorm(n, sd=2)
plot(x, y, main="(a)")
y <- -(x-0.5)^2 + rnorm(n, sd=1)
plot(x, y, main="(b)")
y <- (x > 0 & x < 1) + rnorm(n, sd=.1)
plot(x, y, main="(c)")
y <- rpois(n, abs(x))
plot(x, y, main="(d)")
par(op)
dev.off()

