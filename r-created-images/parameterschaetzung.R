source("setup.R")
set.seed(20190421)

xlim <- c(900, 1100)
x <- seq(xlim[1], xlim[2], by=0.01)

derp <- function(x){
    0.1 * dnorm(x, 950, 10) + 
    0.45 * dnorm(x, 975, 20) +
        0.45 * dnorm(x, 1000, 10)
}

png(file.path(img_folder, "parameterschaetzung-derp-dichte.png"), width=480, height=240)
plot(x, derp(x), type="l", xlab="x (in Milliliter)", ylab="f(x)", main=paste("Mögliche Verteilung des Inhalts von Maßkrügen"))
dev.off()

png(file.path(img_folder, "parameterschaetzung-norm-dichte.png"), width=480, height=240)
plot(x, dnorm(x, 985, 15), type="l", xlab="x (in Milliliter)", ylab="f(x)", main=paste("Normalverteilter Inhalt\n(Einfacher im Umgang)"))
dev.off()



