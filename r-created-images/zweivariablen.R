source("setup.R")
source("zweivariablen-functions.R")

set.seed(20140605)
n <- 20

x <- list()
y <- list()
titel_x <- titel_y <- list()

e <- function(n=20, sd=.1) rnorm(n=n, sd=sd)

titel_x[[1]] <- "Aussentemperatur"
titel_y[[1]] <- "Eisumsatz"
x[[1]] <- runif(n, 0, 40)
y[[1]] <- 2*(x[[1]] + 5 ) + e(sd=6)

titel_x[[2]] <- "Kinopreis"
titel_y[[2]] <- "Kinobesucher"
x[[2]] <- runif(n, 5, 20)
y[[2]] <- -15*x[[2]] + 1500 +e(sd=15)

titel_x[[3]] <- "Körpergrösse"
titel_y[[3]] <- "Nettoeinkommen"
x[[3]] <- runif(n, 150, 200)
y[[3]] <- 2000 + runif(n, -500,500)

titel_x[[4]] <- "Nettoeinkommen"
titel_y[[4]] <- "gekaufte Busfahrkarten pro Monat"
x[[4]] <- runif(n, 0, 4000)
y[[4]] <- -1/1e6*(x[[4]]-2000)^2 + 15 +e(sd=.5)

ylims <- list(NULL, c(1000,2000), NULL, NULL)

png(file.path(img_folder, "korrelationen.png"), height=1.5*480, width=1.5*480)
op <- par(mfrow=c(2,2))
for(i in seq(y)){
    this_y <- y[[i]]
    titel <- paste0(titel_y[[i]], " vs. ", titel_x[[i]])
    plot(x[[i]], this_y,
         ylab=titel_y[[i]],
         xlab=titel_x[[i]],
         ylim=ylims[[i]],
         main=bquote( atop(.(LETTERS[i]) * plain(")") ~ .(titel), r==.(round(cor(x[[i]],this_y),digits=2))) )
         )
    abline(lm(this_y~x[[i]]))
}
par(op)
dev.off()

################################################################
#### Korrelation != Kausalität

set.seed(20140921)

n <- 30

geschlecht <- sample(0:1, n, replace=TRUE)
male <- geschlecht==1
female <- geschlecht==0

shoesize<- runif(n, min=36, max=43) + geschlecht*5
income <- rnorm(n, mean=1300, sd=100) + geschlecht*286  # 22% mehr

png(file.path(img_folder, "zweivariablen-kausalitaet.png"))
plot(shoesize, income, pch=19, col=1, xlab="Schuhgrösse", ylab="Einkommen", main="Einkommen vs. Schuhgrösse")
abline(lm(income~shoesize), col=1)
dev.off()

png(file.path(img_folder, "zweivariablen-kausalitaet-getrennt.png"))
plot(shoesize, income, col=geschlecht+1, pch=19, main="Einkommen vs. Schuhgrösse, nach Geschlecht getrennt", xlab="Schuhgrösse", ylab="Einkommen")
legend("topleft", legend=c("Männer", "Frauen"), col=c(2,1), pch=19)
abline(lm(income[male] ~ shoesize[male]), col=2)
abline(lm(income[female] ~ shoesize[female]), col=1)
dev.off()

################################################################
#### Streudiagramme
library(dplyr)

png(file.path(img_folder, "zweivariablen-streudiagramm-bsp.png"))
cars[3,2] <- 110
cars %>%
    filter(speed<17 | speed>20) %>%
        transform(speed=speed*6) %>%
        plot(main="Bremsweg vs. Geschwindigkeit eines Autos", xlab="Geschwindigkeit", ylab="Bremsweg")
dev.off()

png(file.path(img_folder, "zweivariablen-streudiagramme-2d.png"))
x <- c(3, 5, 3, 6)
y <- c(22, 26, 23, 25)
plot(x,y, xlab="Alter", ylab="Schuhgröße", xlim=c(1, 8), ylim=c(20, 30), main="Schuhgröße vs. Alter eines Kindes")
dev.off()

library(scatterplot3d)
png(file.path(img_folder, "zweivariablen-streudiagramme-3d.png"))
scatterplot3d(mtcars$wt,mtcars$disp,mtcars$mpg, main="3D Scatterplot", xlab="Gewicht in tausend Pfund (lbs.)", ylab="Hubraum (Kubikzoll)", zlab="Benzinverbrauch (Meilen pro US-Gallone)")
dev.off()

#### Spearman-Korrelation

spearmanplot <- function(a, b, main_1="", xlab_1="x", ylab_1="y", main_2="", xlab_2="x", ylab_2="y"){
    layout(matrix(1:2, nrow=1))
    plot(a, b, main=main_1, xlab=xlab_1, ylab=ylab_1)
    text(min(a), max(b), paste0("Pearson-Korrelation: ", round(cor(a,b),2)), adj=0)
    plot(rank(a), rank(b), main=main_2, xlab=xlab_2, ylab=ylab_2)
    text(1,length(a), paste0("Spearman-Korrelation: ", round(cor(a,b,method="spearman"),2)), adj=0)
}

set.seed(20150418)
n <- 6
Alter <- round(runif(n, 18, 80))
Zeit <- round(rnorm(n, mean=9+0.1*alter), 2)
png(file.path(img_folder, "zweivariablen-spearman-bsp.png"), width=2*480)
spearmanplot(Alter, Zeit,
             "Alter vs. Zeit", "Alter (Jahre)", "Zeit (Sekunden)",
             "Rang des Alters vs. Platzierung", "Rang des Alters", "Platzierung (Rang der Zeit)"
             )
dev.off()


x <- runif(10,0,5)
y <- exp(x)
png(file.path(img_folder, "zweivariablen-spearman-r1.png"), width=2*480)
spearmanplot(x,y, main_1="Originaldaten", main_2="Ränge der Daten")
dev.off()
