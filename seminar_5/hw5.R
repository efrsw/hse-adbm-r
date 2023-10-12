n_max = function(vec, n) {
  return(unique(sort(vec))[n])
}

####

x0 = matrix(seq(-1, 2, length.out=1000), ncol = 1000, nrow = 1000, byrow = TRUE)
y0 = matrix(seq(-1, 1, length.out=1000), ncol = 1000, nrow = 1000, byrow = FALSE)

x = x0
y = y0

for(i in 1:20) {
  xt = x
  yt = y
  
  x=xt^2-yt^2+x0
  y=2*xt*yt+y0
}

z = t(abs(x^2 + y^2))
z[!is.na(z)] = rank(z[!is.na(z)])
image(z^3, col=rev(terrain.colors(1000)))

####