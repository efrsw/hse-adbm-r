n_max = function(vec, n) {
  return(unique(sort(vec))[n])
}

####

mb = function(X, Y, dimX, dimY, n) {
  
  x0 = matrix(seq(X[1], X[2], length.out=dimX), ncol = dimX, nrow = dimX, byrow = TRUE)
  y0 = matrix(seq(Y[1], Y[2], length.out=dimY), ncol = dimY, nrow = dimY, byrow = FALSE)
  
  x = x0
  y = y0
  
  for(i in 1:n) {
    xt = x
    yt = y
    
    x=xt^2-yt^2+x0
    y=2*xt*yt+y0
  }
  
  z = t(abs(x^2 + y^2))
  z[!is.na(z)] = rank(z[!is.na(z)])
  image(z^3, col=rev(terrain.colors(dimX)))
}

mb(c(-2, 2), c(-3, 3), 1500, 1500, 15)
####