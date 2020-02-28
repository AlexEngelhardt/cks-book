mediandots <- function(x, titlestring=NULL){
  plot.new()
  plot.window(xlim=range(x), ylim=c(1,1.1))
  title(titlestring)
  plot.xy(xy.coords(x=x, y=rep(1.03, times=length(x))), type="p", pch=19, cex=2, col=3)
  lines(c(median(x), median(x)-.7), c(1.03,1.08))
  points(median(x), 1.03, pch=4)
  text(median(x)-.7, 1.09, "Median")
  axis(1)
  #axis(2)
  #box()
}

quantiledots <- function(x, titlestring=NULL, q=0.2){
qx <- quantile(x, probs=q)

plot.new()
plot.window(xlim=range(x), ylim=c(1,1.1))
title(titlestring)
plot.xy(xy.coords(x=x, y=rep(1.03, times=length(x))), type="p", pch=19, cex=2, col=3)
lines(c(qx, qx+.7), c(1.03,1.08), lty=2)
points(qx, 1.03, pch=4)

## if(!(length(x)*q %% 1)){ ## is.integer failt -.-
if(all.equal(length(x)*q, round(length(x)*q))==TRUE){
  text(qx+.7, 1.09, paste0("ein mögliches ", q*100, "%-Quantil: ", qx))
} else {
  text(qx+.7, 1.09, paste0(q*100, "%-Quantil: ", qx))
}
axis(1)
#axis(2)
#box()
}

## Spannweite UND Interquartilsabstand

spannweite <- function(x, titlestring=NULL){
  plot.new()
  plot.window(xlim=range(x), ylim=c(1,1.1))
  title(titlestring)
  plot.xy(xy.coords(x=x, y=rep(1.03, times=length(x))), type="p", pch=19, cex=2, col=3)
  lines(c(min(x), min(x)), c(1.03,1.06), cex=2)
  lines(c(max(x), max(x)), c(1.03,1.06), cex=2)
  lines(c(min(x), max(x)), c(1.06, 1.06), cex=2)
  lines(rep(mean(c(min(x), max(x))), 2), c(1.06, 1.08), cex=2)
  text(xy.coords(mean(c(min(x), max(x))), 1.09), "Spannweite")
  axis(1)
}

iqr <- function(x, titlestring=NULL){
  plot.new()
  plot.window(xlim=range(x), ylim=c(1,1.1))
  title(titlestring)
  plot.xy(xy.coords(x=x, y=rep(1.03, times=length(x))), type="p", pch=19, cex=2, col=3)
  low <- quantile(x, 0.25, type=1)
  high <- quantile(x, 0.75, type=1)
  lines(c(low, low), c(1.03,1.06), cex=2)
  lines(c(high, high), c(1.03,1.06), cex=2)
  lines(c(low, high), c(1.06, 1.06), cex=2)
  lines(rep(mean(c(low, high)), 2), c(1.06, 1.08), cex=2)
  text(xy.coords(mean(c(low, high)), 1.09), "Interquartilsabstand")
  axis(1)
}

## fürs Histogramm:
set.seed(2)
male <- rnorm(20, mean=180, sd=7)
female <- rnorm(20, mean=170, sd=7)
male_ind <- sample(c(0,1), 20, replace=TRUE)
heights <- ifelse(male_ind==1, male, female)
heights <- round(heights, digits=1)
  

## Der Teil gehört hierhin, weil lorenz UND gini auf die Daten zugreifen,
##   und ich sie nur einmal gespeichert haben will.
## (Die .Rnw erwartet nen Vektor mit Länge 5!)
EK <- c(3, 4, 5, 5, 18)
EK <- sort(EK) # Einkommen_i
sEK <- cumsum(EK) # summiertes EK_i
EKsum <- sum(EK) # Merkmalssumme
tEK <- round(sEK/EKsum, digits=3) # Anteil summiert (y-Achse)

library(ineq)
LC <- Lc(EK)

## <aufgabe>
aEK <- c(100, 310, 20, 220, 160, 330, 250)
aEK <- sort(aEK)
asEK <- cumsum(aEK)
aEKsum <- sum(aEK)
atEK <- round(asEK/aEKsum, digits=3)
## </aufgabe>

plotlorenz <- function(x,
                       xlab="Anteil der Bevölkerung",
                       ylab="Anteil des Einkommens",
                       points=TRUE,
                       eqline=TRUE,
                       area=c("none", "over", "under", "all"),
                       ...){
  area <- match.arg(area)
  library(ineq)
  LC <- Lc(x)

  plot(Lc(x)$p, Lc(x)$L, type="l", xlab=xlab, ylab=ylab, ...)

  if(points)
    points(LC$p, LC$L, pch=19)
  
  if(eqline)
    lines(c(0,1), c(0,1), lty=2)

  if(area=="over"){
    x <- LC$p
    y <- LC$L
    polygon(x, y, lty=0, col=1)
  } else if(area=="under"){
    x <- c(LC$p, 1)
    y <- c(LC$L, 0)
    polygon(x, y, lty=0, col=1)
  } else if(area=="all"){
    x <- c(0,1,1)
    y <- c(0,0,1)
    polygon(x, y, lty=0, col=1)
  }
  
  return(LC)
}
