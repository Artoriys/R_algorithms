newton <- function(y,x0,e){
#newton can find solve of the equation using the known interval
  k <- 0;
  dy <- function(x) (y(e+x0)-y(x0))/e;
  dy2 <- function(x0) (dy(e+x0)-dy(x0))/e;
  if (y(x0) * dy2(x0) < 0){
    
    error('Wrong x0')
    
  }
  xi <- x0;
  xn <- xi-y(xi)/dy(xi);
  while (abs(xn-xi)>e){
    xi <- xn;
    xn <- xi-y(xi)/dy(xi);
    k <- k+1;
  }
  returnlist <- list("xn" = xn, "k" = k)
  return(returnlist);
}