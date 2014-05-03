library(biOps)



boxing <- function(x, pix) {
  n <- pix * x %/% pix
  remove <- (x - n) %/% 2
  i <- c(1 + remove, n + remove)
  lapply(seq(i[1], i[2], pix), function(k) seq(k, k + pix - 1))
}


imgDens <- function(img, rowpix, colpix) {
  y <- boxing(nrow(img), rowpix)
  x <- boxing(ncol(img), colpix)

  newimg <- matrix(0.0, length(y), length(x))
  for(i in 1:length(y)) {
    for(j in 1:length(x)) {
      newimg[i, j] <- mean(img[y[[i]], x[[j]]])
    }
  }
  
  imagedata(newimg, type = 'grey')
}


imgAscii <- function(outfile, rowpix = 7, colpix = 7) {
  img <- readJpeg(choose.files())
  if(attr(img, 'type') == 'rgb')
    img <- imgRGB2Grey(img)
  img.dens <- imgDens(img, rowpix, colpix)
  
  asciis <- c(' ', '.', ',', ':', ';', 'i', 'r', 'X', 'A', 's', '2',
              '5', '3', 'h', 'M', 'H', 'G', 'S', '#', '9', 'B', '&', '@')
  asciis <- rev(asciis)

  img.interval <- cut(as.vector(img.dens), 23, FALSE)
  img.char <- asciis[img.interval]
  
  img.char <- matrix(img.char, nrow(img.dens), ncol(img.dens))

  cat('', file = outfile)
  for(i in 1:nrow(img.char)) {
    linetext <- paste0(img.char[i, ], collapse = '')
    cat(sprintf('%s\n', linetext), file = outfile, append = T)
  }
}



imgAscii('testeAscii1.txt', 10, 7)