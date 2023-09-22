estimate_pi = function(num_samples) {
  x = runif(num_samples, -1, 1)
  y = runif(num_samples, -1, 1)
  
  d <- data.frame(x=x, y=y)
  
  s = d[(x^2 + y^2) <= 1, ]
  prob = length(s[,1]) / num_samples 
  
  # Площадь квадрата = 4*r^2, площадь круга = p*r^2.
  # Вероятность точки = площадь круга / площадь квадрата
  # p = 4 * вероятность точки
  
  return(prob * 4)
}

estimate_pi(10000) # наша оценка на 10000

n_sims = seq(100, 15000, 100)
estims = Map(estimate_pi, n_sims)

plot(n_sims, estims)
lines(x=1:15000, y=rep(pi, 15000),
      col = "red",
      lwd = 2)
