ccf2_myfunc=function (x, y, max.lag = NULL, main = NULL, ylab = "CCF", na.action = na.pass) 
{
  par = graphics::par
  plot = graphics::plot
  grid = graphics::grid
  abline = graphics::abline
  lines = graphics::lines
  frequency = stats::frequency
  ts.intersect = stats::ts.intersect
  lag.max = max.lag
  if (is.matrix(x) || is.matrix(y)) {
    stop("univariate time series only")
  }
  X <- ts.intersect(as.ts(x), as.ts(y))
  num = nrow(X)
  if (num > 49 & is.null(lag.max)) 
    lag.max = max(ceiling(10 + sqrt(num)), 3 * frequency(X))
  if (num < 50 & is.null(lag.max)) 
    lag.max = floor(5 * log10(num))
  if (lag.max > (num - 1)) 
    stop("Number of lags exceeds number of observations")
  colnames(X) <- c(deparse(substitute(x))[1L], deparse(substitute(y))[1L])
  acf.out <- acf(X, lag.max = lag.max, plot = FALSE, type = "correlation", 
                 na.action = na.action)
  lag <- c(rev(acf.out$lag[-1, 2, 1]), acf.out$lag[, 1, 2])
  y <- c(rev(acf.out$acf[-1, 2, 1]), acf.out$acf[, 1, 2])
  acf.out$CCF <- array(y, dim = c(length(y), 1L, 1L))
  acf.out$LAG <- array(lag, dim = c(length(y), 1L, 1L))
  acf.out$snames <- paste(acf.out$snames, collapse = " & ")
  if (is.null(main)) {
    main = acf.out$snames
  }
  U = 2/sqrt(num)
  par(mar = c(2.5, 2.5, 1.5, 0.5), mgp = c(1.5, 0.6, 0), cex.main = 1)
  plot(acf.out$LAG, acf.out$CCF, type = "n", ylab = ylab, xlab = "LAG", 
       main = main)
  grid(lty = 1, col = gray(0.9))
  box()
  abline(h = c(0, -U, U), lty = c(1, 2, 2), col = c(1, 4, 4))
  abline(v = 0, lty = 2, col = gray(0.5, alpha = 0.5))
  lines(acf.out$LAG, acf.out$CCF, type = "h")
  return(acf.out)
}

cc_test=ccf2_myfunc(x=api4_ts[,10],y=api4_ts[,1],max.lag = 100)
max.lag=100
seq_gen=(-1*max.lag):max.lag
for(i in 1:(2*max.lag+1))
{
  cc_test$LAG[i,,]=seq_gen[i]
}




