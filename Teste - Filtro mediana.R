library(jpeg)



imageStackFilter <- function(folder, filename) {
  setwd(folder)
  img <- lapply(list.files(), readJPEG)
  
  medianPixel <- function(layer, image, nr, nc) {
    img1 <- lapply(image, function(x) as.vector(x[, , layer]))
    img1 <- apply(do.call(cbind, img1), 1, median)
    matrix(img1, nr, nc)
  }
  
  nr <- nrow(img[[1]])
  nc <- ncol(img[[1]])
  nl <- dim(img[[1]])[3]
  
  img1 <- lapply(1:nl, medianPixel, img, nr, nc)
  img2 <- array(unlist(img1), dim = c(nr, nc, nl))
  
  writeJPEG(img2, filename)
}


base <- "C:/Users/Rubem/Pictures/Dell WebCam Central/teste/"
imageStackFilter(base, "teste.jpg")