# setwd("/home/alexx/Dropbox/Buch_CrashkursStatistik/blog_R")
source("setup.R")
source("regression-functions.R")

################################################################
#### Einfache lineare Regression

set.seed(20160511)
n <- 10
x <- runif(n,155,180)
y <- 0+0.3*x+rnorm(n,sd=.8)
mod <- lm(y~x)

x <- round(x,1)
y <- round(y,1)

png(file.path(img_folder, "regression-motivation1.png"))
plot(x,y, xlab="Körpergröße (x)", ylab="Ringgröße (y)")
dev.off()

png(file.path(img_folder, "regression-motivation2.png"))
plot(x,y, xlab="Körpergröße (x)", ylab="Ringgröße (y)", main="Welche Gerade ist die beste?")

abline(mod)
abline(coef(mod)[1] - 17,
       coef(mod)[2] + 0.1,
       lty=2)
abline(coef(mod)[1] - 1,
       coef(mod)[2],
       lty=2)
dev.off()

#### manuelle koeffizienten
(xmx <- x-mean(x))
(ymy <- y-mean(y))
(xmx2 <- xmx^2)

(z <- sum(xmx*ymy))
(n <- sum(xmx2))

(b <- z/n)

(a <- mean(y) - b*mean(x))

coef(mod)

#### vorhersage

x_1 <- 160
x_2 <- 170

y_1 <- a + b * x_1
y_2 <- a + b * x_2
## coef(mod)[1] + coef(mod)[2] * x_1  # wahre (ungerundete) werte
## coef(mod)[1] + coef(mod)[2] * x_2

png(file.path(img_folder, "regression-gerade.png"))
plot(x,y, xlab="Körpergröße (x)", ylab="Ringgröße (y)", main="Regressionsgerade y = a + b*x")
points(c(x_1, x_2), c(y_1, y_2), col=2, pch=19)
abline(a=a, b=b, col=2, lwd=2)
dev.off()

#### Klausuraufgabe

set.seed(20190815)
n <- 6
alpha <- 11400
beta <- 200  # 1 PS mehr -> 200€ mehr
epsilon <- rnorm(n, sd=5000)
x <- round(runif(n, 48, 220))  # Leistung in PS
y <- round(alpha + beta * x + epsilon, -2)  # Listenpreis €

mod <- lm(y~x)
a <- round(coef(mod)[1], 2)
b <- round(coef(mod)[2], 2)

gleichung <- paste("y =", a, "+", b, "* x")

## a)

mean(x)
mean(y)

x - mean(x)
y - mean(y)

(x - mean(x)) * (y - mean(y))
(x - mean(x))^2

sum((x - mean(x)) * (y - mean(y)))
sum((x - mean(x))^2)

sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)

## b)

y_hat_120 = predict(mod, newdata=data.frame(x=120))
y_hat_100 = predict(mod, newdata=data.frame(x=100))

## c)

png(file.path(img_folder, "regression-auto.png"))
plot(x, y, main=paste0("Neupreis vs. PS\nGleichung: ", gleichung), xlab="Leistung (PS)", ylab="Neupreis (Euro)")
points(100, y_hat_100, pch=4, cex=2)
points(120, y_hat_120, pch=4, cex=2)
abline(mod)
dev.off()


################################################################
#### Multiple lineare Regression

x_1 <- x
x_2 <- c(62, 52, 83, 69, 74, 52, 77, 65, 79, 51)
x_3 <- c(24, 34, 26, 51, 43, 33, 22, 21, 19, 34)

mod_mult <- lm(y~x_1+x_2+x_3)
summary(mod_mult)

################################################################
#### Modellannahmen lineare Regression

n <- 100
x <- runif(n, min=10, max=160)
y <- x^2 / 200 + rnorm(n)
png(file.path(img_folder, "regression-annahmen-1.png"), width=2*480)
layout(matrix(1:2, nrow=1))
plot(x, y, xlab="Geschwindigkeit (km/h)", ylab="Bremsweg (m)", main="Streudiagramm für 100 Autos")
plot(x, y, xlab="Geschwindigkeit (km/h)", ylab="Bremsweg (m)", main="Mit Regressionsgerade")
abline(lm(y~x), lwd=2, col=2)
dev.off()

## Annahme linearer Zusammenhang:

y1 <- x * 3 + 2 + rnorm(n, sd=40)
y2 <- sin(x/10) + rnorm(n, sd=0.1)
y3 <- exp(x/15) + rnorm(n, sd=1000)
png(file.path(img_folder, "regression-annahmen-2.png"), width=1.5*480, height=0.5*480)
layout(matrix(1:3, nrow=1))
plot(x, y1, main="Linearer Zusammenhang")
abline(lm(y1~x), lwd=2, col=2)
plot(x, y2, main="Saisonaler Trend")
abline(lm(y2~x), lwd=2, col=2)
plot(x, y3, main="Exponentieller Zusammenhang")
abline(lm(y3~x), lwd=2, col=2)
dev.off()

## Annahme normalverteilter Residuen

x <- runif(n, min=1, max=5)
y1 <- x * 3 + 2 + rnorm(n, sd=4)
y2 <- x * 3 + 2 + rt(n, df=1) * 5
y3 <- x * 1.0 + 2 + rpois(n, lambda=4)
png(file.path(img_folder, "regression-annahmen-3.png"), width=1.5*480, height=0.5*480)
layout(matrix(1:3, nrow=1))
plot(x, y1, main="Residuen normalverteilt")
abline(lm(y1~x), lwd=2, col=2)
plot(x, y2, main="Zu grosse Ausreisser")
abline(lm(y2~x), lwd=2, col=2)
plot(x, y3, main="Residuen ganzzahlig")
abline(lm(y3~x), lwd=2, col=2)
dev.off()

## Annahme Homoskedastizität

x <- sort(runif(n, min=1, max=20))
y1 <- x * 1.2 + 2 + rnorm(n, sd=4)
y2 <- x * 1.2 + 2 + rnorm(n, sd=x)
png(file.path(img_folder, "regression-annahmen-4.png"), width=2.0*480, height=1.0*480)
layout(matrix(1:2, nrow=1))
plot(x, y1, main="Gleichbleibende Varianz")
abline(lm(y1~x), lwd=2, col=2)
plot(x, y2, main="Steigende Varianz")
abline(lm(y2~x), lwd=2, col=2)
dev.off()

## Annahme Unabhängigkeit

x <- sort(runif(n, min=1, max=20))
y <- 10*sin(x/16) - exp(x/10) + rnorm(n, sd=0.2)
png(file.path(img_folder, "regression-annahmen-5.png"))
plot(x, y)
abline(lm(y~x), lwd=2, col=2)
dev.off()

################################################################
## Konfidenzintervall für Parameter

set.seed(20190727)
# x <- runif(20, 2, 8)
# y <- 3 * x + 2.4 + rnorm(length(x), sd=3)
# mod <- lm(y~x)
# a <- coef(mod)[1]
# b <- coef(mod)[2]
# 
# plot(x, y)
# abline(mod)
# summary(mod)
# 
# a_lo <- confint(mod)[1,1]
# a_hi <- confint(mod)[1,2]
# b_lo <- confint(mod)[2,1]
# b_hi <- confint(mod)[2,2]
# 
# abline(a=a_lo, b=b_lo, lty=2)
# abline(a=a_lo, b=b_hi, lty=2)
# abline(a=a_hi, b=b_lo, lty=2)
# abline(a=a_hi, b=b_hi, lty=2)

set.seed(20190727)
png(file.path(img_folder, "regression-ki-1.png"))
x <- runif(6, 155, 200)
y <- 0.9 * x - 60 + rnorm(length(x), sd=10)
mod <- lm(y~x)
plot(x, y, xlim=c(154, 201), ylim=c(60, 120),
     main=paste0("Stichprobe 1\ny = ", round(coef(mod)[1], 2), " + ", round(coef(mod)[2], 2), " * x"))
abline(mod)
dev.off()

set.seed(20190727)
png(file.path(img_folder, "regression-ki-2.png"), width=1.5*480)
layout(matrix(1:6, nrow=2, byrow=TRUE))
for(i in 1:6){
    x <- runif(6, 155, 200)
    y <- 0.9 * x - 60 + rnorm(length(x), sd=10)
    mod <- lm(y~x)
    plot(x, y, xlim=c(154, 201), ylim=c(60, 120),
         main=paste0("Stichprobe ", i, "\ny = ", round(coef(mod)[1], 2), " + ", round(coef(mod)[2], 2), " * x"))
    abline(mod)
}
dev.off()

set.seed(20190727)
png(file.path(img_folder, "regression-ki-3.png"))
plot(x, y, xlim=c(154, 201), ylim=c(60, 120), type="n",
     main="1000 simulierte Stichproben (nicht gezeigt)\nund deren Regressionsgeraden")
for(i in 1:1000){
    x <- runif(10, 155, 200)
    y <- 0.9 * x - 60 + rnorm(length(x), sd=10)
    mod <- lm(y~x)
    abline(mod, col="#00000033")
}
dev.off()

################################################################
## Testen von Regressionsparametern

set.seed(20190804)

n <- 20
groesse_cm <- runif(n, 150, 195)
hausnr <- round(exp(runif(n, 0, 5)))
gewicht <- -41 + 0.7 * groesse_cm + rnorm(n, sd=10)

png(file.path(img_folder, "regression-testen-1.png"), width=2*480)
layout(matrix(1:2, nrow=1))
plot(groesse_cm, gewicht, xlab="Körpergröße (cm)", ylab="Gewicht (kg)", main="Gewicht vs. Körpergröße")
plot(hausnr, gewicht, xlab="Hausnummer", ylab="Gewicht (kg)", main="Gewicht vs. Hausnummer")
dev.off()

png(file.path(img_folder, "regression-testen-2.png"), width=2*480)
layout(matrix(1:2, nrow=1))
plot(groesse_cm, gewicht, xlab="Körpergröße (cm)", ylab="Gewicht (kg)", main="Gewicht vs. Körpergröße")
abline(lm(gewicht ~ groesse_cm))
plot(hausnr, gewicht, xlab="Hausnummer", ylab="Gewicht (kg)", main="Gewicht vs. Hausnummer")
abline(lm(gewicht ~ hausnr))
dev.off()

mod <- lm(gewicht ~ groesse_cm + hausnr)
summary(mod)


set.seed(20190806)
n <- 15
cm <- runif(15, 150, 200)
m <- cm/100
kg <- -41 + 0.7 * cm + rnorm(n, sd=10)

png(file.path(img_folder, 'regression-testen-3.png', width=2*480)
layout(matrix(1:2, nrow=1))
mod_cm <- lm(kg~cm)
gerade_cm <- paste(")Regressionsgerade: y =", round(coef(mod_cm)[1], 2), "+", round(coef(mod_cm)[2], 4), "* cm")
plot(cm, kg, main=paste("Gewicht (kg) vs. Größe (in Zentimetern)", gerade_cm, sep="\n"), xlab="Größe (cm)", ylab="Gewicht (kg)")
abline(mod_cm)

mod_m <- lm(kg~m)
gerade_m <- paste("Regressionsgerade: y =", round(coef(mod_m)[1], 2), "+", round(coef(mod_m)[2], 2), "* m")
# https://stackoverflow.com/questions/49708371/how-to-show-more-decimal-digits-on-an-axis
plot(m, kg, xaxt='n', main=paste("Gewicht (kg) vs. Größe (in Metern)", gerade_m, sep="\n"), xlab="Größe (m)", ylab="Gewicht (kg)")
abline(mod_m)
p <- pretty(par('usr')[1:2])
l <- formatC(p, format="f", digits=2)
axis(1, at=p, labels=l)
dev.off()

################################################################
#### Kategorien als Einflussgröße

set.seed(20190815)

n <- 20
x1 <- sample(c(10, 50, 100, 200), n, replace=TRUE)  # dosis (mg)
x2 <- factor(sample(c("gesund", "leicht", "schwer"), n, replace=TRUE))

alpha <- 350
beta_1 <- .8
beta_leicht <- 15
beta_schwer <- 100

y <- round(alpha + beta_1 * x1 + beta_leicht * (x2 == 'leicht') + beta_schwer * (x2 == 'schwer') + rnorm(n, sd = 75))

mod <- lm(y~x1+x2)
summary(mod)
plot(x1, y)

################################################################
#### Was sind Residuen?

set.seed(20191001)
n <- 10
x <- sort(runif(n, min=1, max=10))
y <- 5 * x + 3 + rnorm(n, sd=6)
mod <- lm(y~x)

i <- 4
x[i]
c(y[i], predict(mod)[i])

y_i <- round(y[i])
yhat_i <- round(predict(mod)[i])
epsilon_i <- y_i - yhat_i

png(file.path(img_folder, "regression-residuen-1.png"))
plot(x, y, main=bquote(y[.(i)] == .(y_i) ~ ", " ~ hat(y)[.(i)] == .(yhat_i) ~ ", " ~ hat(epsilon[.(i)]) == .(epsilon_i)))
abline(mod)
lines(x=c(x[i], x[i]), y=c(y[i], predict(mod)[i]), col=1, lwd=3)
dev.off()

################################################################
#### Modelldiagnose

# 1.) Ist der Zusammenhang linear?

set.seed(20190925)
n <- 100
x <- runif(n, min=10, max=70)
y <- abs(x^2 / 200 + rnorm(n))
mod <- lm(y~x)
png(file.path(img_folder, "regression-modelldiagnose-1.png"), height=360, width=3*360)
par(mfrow=c(1,3))
plot(x, y, xlab="Geschwindigkeit (km/h)", ylab="Bremsweg (m)", main="(a) Streudiagramm Bremsweg vs.\nGeschwindigkeit für 100 Autos")
abline(mod, lwd=2, col=2)
plot(predict(mod, data.frame(x=x)), y, xlab="Vorhergesagte y-Werte", ylab="Beobachtete y-Werte", main="(b) Beobachtete vs. vorhergesagte Werte\nder Zielgröße")
abline(1,1)
plot(predict(mod, data.frame(x=x)), residuals(mod), xlab="Vorhergesagte y-Werte", ylab="Residuen", main="(c) Residuen vs. vorhergesagte Werte")
abline(0,0)
dev.off()

png(file.path(img_folder, "regression-modelldiagnose-2.png"), height=360, width=2*360)
par(mfrow=c(1, 2), oma=c(0, 0, 3, 0))
mod_alt1 <- lm(sqrt(y)~x)
plot(predict(mod_alt1, data.frame(x=x)), sqrt(y), xlab="Vorhergesagte y-Werte", ylab="Beobachtete y-Werte", main="(a) Beobachtete vs. vorhergesagte Werte\nder Zielgröße")
plot(predict(mod_alt1, data.frame(x=x)), residuals(mod_alt1), xlab="Vorhergesagte y-Werte", ylab="Residuen", main="(b) Residuen vs. vorhergesagte Werte")
title(bquote(atop("Alternative 1: Zielgröße transformieren", sqrt(y) == a + b * x)), outer=TRUE)
dev.off()

png(file.path(img_folder, "regression-modelldiagnose-3.png"), height=360, width=2*360)
par(mfrow=c(1, 2), oma=c(0, 0, 3, 0))
mod_alt2 <- lm(y ~ x + I(x^2))
plot(predict(mod_alt2, data.frame(x=x)), y, xlab="Vorhergesagte y-Werte", ylab="Beobachtete y-Werte", main="(a) Beobachtete vs. vorhergesagte Werte\nder Zielgröße")
plot(predict(mod_alt2, data.frame(x=x)), residuals(mod_alt2), xlab="Vorhergesagte y-Werte", ylab="Residuen", main="(b) Residuen vs. vorhergesagte Werte")
title(bquote(atop("Alternative 2: Neue Einflussgröße aufnehmen", y == a + b[1] * x + b[2] * x^2)), outer=TRUE)
dev.off()

# 2.) Sind die Residuen normalverteilt?

set.seed(20190927)
n <- 40
x <- runif(n, 1, 5)
y <- exp(-1 + 2*x + rnorm(n))
plot(x, y)
mod <- lm(y~x)
abline(mod)

plot(mod, which=2)
standardized_residuals <- MASS::stdres(mod)
theoretical_quantiles <- todo
qqnorm(resid(mod))
qqnorm(MASS::stdres(mod))

# Kolmogorov-Smirnov Test

ks.test(resid(mod), "pnorm")
ks.test(MASS::stdres(mod), "pnorm")

# Testing ks.test on actually good data

set.seed(20190927)
n <- 20
x <- runif(n, 1, 3)
y <- 3 + 12 * x + rnorm(n, sd=10)
plot(x, y)
mod <- lm(y~x)
ks.test(resid(mod), "pnorm")  # highly significant, i.e. ks.test tests on the N(0,1) distribution specifically, not *any* Gaussian
ks.test(MASS::stdres(mod), "pnorm")  # non-significant, which means the *standardized* resids (to N(0,1)) fit well here!

# 3.) Ist die Varianz der Residuen gleichbleibend?

set.seed(20191222)
n <- 100
x <- round(runif(n, 10, 20))
y <- -10 + 2*x
resids <- rnorm(n) * (x-min(x)+0.01)
y <- y + resids

mod <- lm(y~x)

png(file.path(img_folder, "regression-modelldiagnose-heteroskedastie.png"))
plot(x, y, xlab="Alter des Küken (Tage)", ylab="Gewicht (g)")
abline(mod)
dev.off()

png(file.path(img_folder, "regression-modelldiagnose-heteroskedastie-diagnose.png"))
plot(predict(mod), resid(mod), xlab="Vorhergesagte y-Werte", ylab="Residuen")
dev.off()
