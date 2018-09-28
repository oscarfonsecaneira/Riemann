CalculoArea = function(ar,ba){
  return  (ar*ba)}

Distr = function(s, u, x){
  val <- (1/(s * sqrt(2*pi))) * exp((-0.5*((x-u)/s)**2))
  return (val)}

reinman = function( sup, inf, x, sigma, u){
  ac <- 0
  while (sup > inf){
    ac <- ac + CalculoArea(x,Distr(inf + (x/2), sigma, u))
    inf <- inf + x}
  return (ac)}

print(reinman(0,-15,1,1,0))
