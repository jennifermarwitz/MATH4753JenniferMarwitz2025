#' Maximum Likelihood Estimation for Normal Distribution
#'
#' Computes the log-likelihood for a normal distribution across a grid of means (`mu`) and standard deviations (`sig`),
#' then finds and returns the maximum likelihood estimates. The function also generates a contour plot of the likelihood surface.
#'
#' @param x A numeric vector representing the sample data.
#' @param mu A numeric vector of candidate values for the population mean.
#' @param sig A numeric vector of candidate values for the population standard deviation.
#' @param ... Additional graphical parameters passed to the \code{contour} plotting function.
#'
#' @return A list containing:
#' \describe{
#'   \item{x}{The input sample data.}
#'   \item{coord}{A matrix of row and column indices indicating the position in the likelihood matrix where the maximum likelihood occurs.}
#'   \item{maxl}{The maximum likelihood value (not log-likelihood).}
#' }
#'
#' @details
#' The function evaluates the log-likelihood function of the normal distribution over a grid of candidate values for \code{mu} and \code{sig}.
#' It plots a contour map of the likelihood surface and indicates both the sample estimates and the MLEs on the plot.
#'
#' @examples
#' x <- c(10, 12, 13, 15, 12, 11, 10)
#' mu_vals <- seq(10, 15, length = 1000)
#' sig_vals <- seq(0.1, 4, length = 1000)
#' mymlnorm(x = x, mu = mu_vals, sig = sig_vals, lwd = 2, labcex = 1)
#'
#' @importFrom stats dnorm sd
#' @importFrom graphics contour
#' @export

mymlnorm=function(x,mu,sig,...){  #x sample vector
  nmu=length(mu) # number of values in mu
  nsig=length(sig)
  n=length(x) # sample size
  zz=c()    ## initialize a new vector
  lfun=function(x,m,p) log(dnorm(x,mean=m,sd=p))   # log lik for normal
  for(j in 1:nsig){
    z=outer(x,mu,lfun,p=sig[j]) # z a matrix
    # col 1 of z contains lfun evaluated at each x with first value of mu,
    # col2 each x with 2nd value of m
    # all with sig=sig[j]
    y=apply(z,2,sum)
    # y is a vector filled with log lik values,
    # each with a difft mu and all with the same sig[j]
    zz=cbind(zz,y)
    ## zz is the matrix with each column containing log L values, rows difft mu, cols difft sigmas
  }
  maxl=max(exp(zz))
  coord=which(exp(zz)==maxl,arr.ind=TRUE)
  maxlsig=apply(zz,1,max)
  contour(mu,sig,exp(zz),las=3,xlab=expression(mu),ylab=expression(sigma),axes=TRUE,
          main=expression(paste("L(",mu,",",sigma,")",sep="")),...)
  mlx=round(mean(x),2)  # theoretical
  mly=round(sqrt((n-1)/n)*sd(x),2)
  #axis(1,at=c(0:20,mlx),labels=sort(c(0:20,mlx)))
  #axis(2,at=c(0:20,mly),labels=TRUE)
  abline(v=mean(x),lwd=2,col="Green")
  abline(h=sqrt((n-1)/n)*sd(x),lwd=2,col="Red")

  # Now find the estimates from the co-ords
  muest=mu[coord[1]]
  sigest=sig[coord[2]]

  abline(v=muest, h=sigest)
  return(list(x=x,coord=coord,maxl=maxl))
}

mymlnorm(x=c(10,12,13,15,12,11,10),mu=seq(10,15,length=1000),sig=seq(0.1,4,length=1000),lwd=2,labcex=1)
