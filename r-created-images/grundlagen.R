source("setup.R")

png(file.path(img_folder, "summenzeichen-kuchen.png"))
  op <- par(mar=c(0,0,0,0), oma=c(0,0,0,0))
  pie(c(4,2,1,1), labels=c(1,2,3,NA))
dev.off()

################################################################
#### Exponential- und Logarithmusfunktion

png(file.path(img_folder, "grundlagen-explog-polynom.png"))
pow <- c(1,2,3,4,5)
x <- seq(-2,3, by=0.01)
plot(x, x^pow[1], type="l", lty=2, xlab="x", ylab="f(x)", main="Einige Polynome", lwd=2)
abline(h=0)
abline(v=0)
for(i in 2:length(pow)){
    p <- pow[i]
    LTY <- i+1
    lines(x, x^pow[i], lty=LTY, lwd=2)
}
legendtext <- c(
    expression(f(x)==x^1),
    expression(f(x)==x^2),
    expression(f(x)==x^3),
    expression(f(x)==x^4),
    expression(f(x)==x^5)
)
legend("bottomright", lty=2:5, legend=legendtext, lwd=2)
dev.off()


png(file.path(img_folder, "grundlagen-explog-exponential.png"))
base <- c(1, 2, exp(1), 3)
x <- seq(-2, 3, by=0.01)
plot(x, base[1]^x, type="l", lty=2, xlab="x", ylab="f(x)", main="Einige Exponentialfunktionen", ylim=c(-1, 8), lwd=2)
abline(h=0)
abline(v=0)
for(i in 2:length(base)){
    lines(x, base[i]^x, lty=i+1, lwd=2)
}
legendtext <- c(
    expression(f(x)==1^x),
    expression(f(x)==2^x),
    expression(f(x)==e^x),
    expression(f(x)==3^x)
)
legend("topleft", lty=2:6, legend=legendtext, lwd=2)
dev.off()

png(file.path(img_folder, "grundlagen-explog-log.png"), width=2*480)
x <- seq(-2,4,by=0.01)
op <- par(mfrow=c(1,2))
plot(x, exp(x), type="l", lwd=2, main="f(x) = exp(x)")
abline(h=0);abline(v=0)
plot(exp(x), log(exp(x)), type="l", lwd=2, xlab="x", ylab="log(x)", main="f(x) = log(x)")
abline(h=0);abline(v=0)
par(op)
dev.off()


ppl <- read.csv("data/Statista2_UTF8.csv", header=TRUE)

png(file.path(img_folder, "grundlagen-explog-einwohner.png"), width=2*480)
op <- par(mfrow=c(2,1))
y <- runif(nrow(ppl))
plot(ppl$X2013, y, xlab="Einwohnerzahl", ylab="", yaxt="n", ylim=c(-.25, 1.25), main=iconv("Deutschlands Großstädte (Stand 2014)", from="UTF8", to="LATIN1"))
plot(log10(ppl$X2013), y, xlab="log10(Einwohnerzahl)", ylab="", yaxt="n", ylim=c(-.25, 1.25))
par(op)
dev.off()
