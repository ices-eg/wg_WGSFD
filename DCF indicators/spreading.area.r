spreading.area <- function (z, w, plot = F)
{
  # extract data
  nb<-length(z)
  
  # sort data in increasing order
  zi <- sort(z,index.return=TRUE)
  z<-zi$x
  w<-w[zi$ix]
  
  # computation of the spreading area 
  Q <- sum(z*w)
  QT <- c(0,cumsum(z*w))
  SA <- sum((QT[1:nb]+QT[2:(nb+1)])*w)/Q
  
  # computation of (Q-Q(T))/Q as a function of T
  T <- c(0,cumsum(w))
  T <- T[nb+1] - T
  T <- rev(T)
  QT <- QT[nb+1] - QT
  QT <- rev(QT)
  
  # display
  if(plot)
    plot(T, (Q-QT)/Q, main="Curve (Q-Q(T))/Q", type="o", pch="+")
  
  # outputs
  SA
}
