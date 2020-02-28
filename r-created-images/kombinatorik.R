source("setup.R")

png(file.path(img_folder, "permutationen.png"), height=240)
plot.new()
plot.window(xlim=c(0.7,7.3),ylim=c(0.8,1.2))
plot.xy(xy.coords(1:7, rep(1,7)), type="p", pch=c(18,15,15,17,16,16,16), cex=8, col=c(1, 2, 2, 3, 4, 4, 4))
dev.off()
