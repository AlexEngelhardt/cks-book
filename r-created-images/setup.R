library(colorspace)
n <- 7
palette(
  rainbow_hcl(n, c = 50, l = 70, start = 0, end = 360*(n-1)/n,
              gamma = NULL, fixup = TRUE)
  )
dev.off()

img_folder = "../book/img"
