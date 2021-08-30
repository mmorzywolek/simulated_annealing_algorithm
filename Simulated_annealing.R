# Parametry:
# x - initial solution
# f - objective function 
# t - initial temperature
# alpha -temperature update param
# radius of N(x) - promień sąsiedztwa
# maxIt - # of iteration

f = function(x)
{
  sum(x^2)
}

SA = function(x, f, t, alpha, delta, maxIt)
{
  n = length(x) #ilość elementów wektora x, inaczej w ilo wymiarowej przestrzeni się znajdujemy
  out = list()
  out$f_hist = rep(NA, maxIt+1)
  out$f_hist[1] = f(x)
  out$t_hist = rep(NA, maxIt+1)
  out$t_hist[1] = t
  
  for (i in 1 : maxIt + 1)
  {
    # 1. losowanie 'candidate solution'
    x_c = x + runif(n, min = -delta, delta)
    
    # 2. symulacja przejścia do candidate solution (f-cja aktywacji)
    A = min(1, exp(-(f(x_c) - f(x))/t))
    u = runif(1)
    # Spr. u < A
    if (u < A)
    {
      x = x_c
    }
    t = alpha * t
    
    out$f_hist[i+1] = f(x)
    out$t_hist[i+1] = t
    # 3. aktualizacja wartości parametru temperatury (annealian scedule)
  }
  out$x_opt = x
  return(out)
}

x = c(10, 10)
t = 1
alpha = 0.995
delta = 0.1
maxIt = 2000

resu = SA(x, f, t, alpha, delta, maxIt)
plot(resu$f_hist, type="l")
lines(resu$f_hist * 2, type = "l", col = "red")
