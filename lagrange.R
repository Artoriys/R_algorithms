lagrange <- function(a,b,y,n,p) {
  k=0;
  xx <- seq(a,b,length=n);
  x <- seq(a,b,length=p);
  Y <- matrix(0,1,p);

  for (i in 1:p) {
    k <- k+1;
    Y[k] <- y(x[i]);
  }
  w <- length(x);
  L <- matrix(0,1, length(xx));
  
  for (k in 1:w) {
    H <- matrix(1,1,length(xx));
      for (j in 1:w) {
        if (k!=j) {
          H=H*(xx-x[j])/(x[k]-x[j]);
          }
      }
      L = L + Y[k]*H;
  }
  plot(xx,L, ylim=c(-1,1));
  plot(y,xlim = c(a,b),ylim=c(-1,1), add = T)
  grid()
  returnlist <- list("Y" = Y, "x" = x, "L" = L)
}
