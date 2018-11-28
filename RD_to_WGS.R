rd_to_wgs84 <- function(x, y) {
  if (!is.numeric(x)) stop("x needs to be numeric.")
  if (nargs() == 1  && length(x) == 2) {
    y <- x[2]
    x <- x[1]
  } 
  if (!is.numeric(y)) stop("y needs to be numeric.")
  if (length(x) != length(y)) stop("x and y need to have same length.")
  
  x0 <- 155000.00
  y0 <- 463000.00
  phi0 <- 52.15517440
  lambda0 <- 5.38720621
  
  k <- list( 
    list(p=0, q=1, k=3235.65389), 
    list(p=2, q=0, k= -32.58297), 
    list(p=0, q=2, k=  -0.24750), 
    list(p=2, q=1, k=  -0.84978), 
    list(p=0, q=3, k=  -0.06550), 
    list(p=2, q=2, k=  -0.01709), 
    list(p=1, q=0, k=  -0.00738), 
    list(p=4, q=0, k=   0.00530), 
    list(p=2, q=3, k=  -0.00039), 
    list(p=4, q=1, k=   0.00033), 
    list(p=1, q=1, k=  -0.00012))
  l <- list(
    list(p=1, q=0, l=5260.52916), 
    list(p=1, q=1, l= 105.94684), 
    list(p=1, q=2, l=   2.45656), 
    list(p=3, q=0, l=  -0.81885), 
    list(p=1, q=3, l=   0.05594), 
    list(p=3, q=1, l=  -0.05607), 
    list(p=0, q=1, l=   0.01199), 
    list(p=3, q=2, l=  -0.00256), 
    list(p=1, q=4, l=   0.00128))
  dx <- (x - x0)/1E5
  dy <- (y - y0)/1E5
  phi <- rep(phi0, length(x))
  lambda <- rep(lambda0, length(x))
  
  for (i in seq_along(k)) {
    p <- k[[i]][["p"]]
    q <- k[[i]][["q"]]
    ki <- k[[i]][["k"]]
    phi <- phi + (ki * (dx^p) * (dy^q))/3600
  }
  for (i in seq_along(l)) {
    p <- l[[i]][["p"]]
    q <- l[[i]][["q"]]
    li <- l[[i]][["l"]]
    lambda <- lambda + (li * (dx^p) * (dy^q))/3600
  }
  if (nargs() == 1) {
    c(lambda[1], phi[1])
  } else list(lambda=lambda, phi=phi)
}
