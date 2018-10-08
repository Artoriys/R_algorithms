seidel <- function(A,b,e){
  S <- dim(A);
  n <- ncol(b);
  L <- A;
  R <- A;
  L[upper.tri(L)] <- 0;
  R[lower.tri(R)] <- 0;
  D <- A*(diag(1,n));
  A2 <- -((L+D)^(-1))*R;
  q <- norm(A2);

  k <- 0;
  c = matrix(, nrow = n, ncol = n)
  d = vector(mode="numeric", length=n)
for (i in 1:n) {
    for (j in 1:n){
      if (i==j){
        c[i,j] <- 0;
      } else {
       c[i,j] <- -A[i,j]/A[i,i];
     }
}
  d[i] <- b[i]/A[i,i];
}
  X0 <- b;
  for (i in 2:n){
    for (j in 1:(i-1)) {
      X0[i] <- X0[i]+b[j]*X0[j];
      }
  }

Xk <- X0; 
Xk1 <- X0;

  e1=e*(1-q)/q;
while (norm((Xk1-Xk)) > e1){
  Xk=Xk1;
    for (i in 1:n) {
      S1=0;
      S2=0;
      for (j in 1:(i-1)) {
        S1=S1+c[i,j]*Xk1[j];
      }
      for (j in (i+1):n) {
        S2=S2+c[i,j]*Xk[j];
      }
      Xk1[i]=S1+S2+d[i];
    }
  k=k+1;
}



for (i in 1:n) {
if (abs(Xk1(i))==Inf)
  error('the solution couldn`t be given in numerical form')  
}
  X <- Xk1;
  return(X)
}  

