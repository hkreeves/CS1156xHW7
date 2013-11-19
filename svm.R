##
## EdX - Machine Learning
## HW7
##
## Author: Kai He
##

## Q8-Q10 SVM

library(LowRankQP)
source("../CS1156xHW1/PLA.R")

svm <- function(X, Y)
{
	# solve the quadratic programing problem with SVM
	# in the dual form min(-d^T b + 1/2 b^T D b)
	# with the constraints A^T b >= b_0
	# Our problem has
	# constrants 0 =< alpha_n( <= inf), and Y^T alpha = 0
	
	Dmat <- (Y %*% t(Y)) * (X %*% t(X))
	dvec <- rep(-1, nrow(X))
	Amat <- t(Y)
	bvec <- 0
	uvec <- rep(1e6, nrow(X)) # C is set 1000, should be theoretically inf

	res <- invisible(LowRankQP(Dmat, dvec, Amat, bvec, uvec, method="CHOL"))
	alphas <- res$alpha
	#print(res$alpha)
	sup.vec <- which(alphas > 1e-8) 
	w <- t(X) %*% (alphas * Y)
	#print(w)
	b <- Y[sup.vec] - X[sup.vec, ] %*% w
	#print(b)
	#print(alphas[sup.vec,])
	return(list(sup.vec=sup.vec, g=c(mean(b), w)))
}

testSVM <- function(N=10, plot=F)
{
	X <- matrix(runif(2*N, -1, 1), N, 2)
	f <- getLine()
	Y <- getClass(X, f)

	res.svm <- svm(X, Y)
	svec <- res.svm$sup.vec
	g <- res.svm$g
	if(plot)
	{
		par(mfrow=c(1,2))
		plotSim(X, Y, g)
		points(X[svec,1], X[svec, 2], pch=1, cex=2)	
	}
	eval(g, f, plot, 1000)
}

iterTest <- function(N=10, iter=1000)
{
	errSet <- vector()
	num.svec <- vector()
	#names(errSet) <- c("pla", "svm")
	for(i in 1:iter)
	{
	X <- matrix(runif(2*N, -1, 1), N, 2)
	f <- getLine()
	Y <- getClass(X, f)
	
	res.pla <- pla(X, Y)
	res.svm <- svm(X, Y)

	err.pla <- eval(res.pla$g, f)
	err.svm <- eval(res.svm$g, f)
	num.svec <- c(num.svec, length(res.svm$sup.vec))
	errSet <- rbind(errSet, c(err.pla, err.svm))
	}
	print(mean(num.svec, na.rm=T))
	return(errSet)
}
	
	
	
