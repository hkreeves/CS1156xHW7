##
## EdX - Machine Learning
## HW7
##
## Author: Kai He
##

## Q1-Q5 Validation

## load data
din <- read.table("http://work.caltech.edu/data/in.dta", 
			col.names=c("x1","x2","y"))
dout <- read.table("http://work.caltech.edu/data/out.dta",
			col.names=c("x1","x2","y"))
#Xtrain <- din[1:25,1:2]
#Ytrain <- din[1:25, 3]
#Xval <- din[26:35, 1:2]
#Yval <- din[26:35, 3]
## reverse training and val. samples
Xval <- din[1:25,1:2]
Yval <- din[1:25, 3]
Xtrain <- din[26:35, 1:2]
Ytrain <- din[26:35, 3]
Xout <- dout[,1:2]
Yout <- dout[,3]

# transform from (x1, x2) space to z space
# z is a subset of (1, x1, x2, x1^2, x2^2, x1*x2, |x1-x2|, |x1+x2|) 
# cut off by "degree" variable (degree = 0,..,7)
nonlinTrans <- function(X, degree=7)
{
	Z <- apply(X, 1, function(x) c(1, x[1], x[2], x[1]^2, x[2]^2, x[1]*x[2], abs(x[1]-x[2]),
		abs(x[1]+x[2])))
	t(Z)[, 1:(degree+1)]
}

## Traditional linear regression
linReg <- function(X, Y)
{
	w <- solve(t(X) %*% X) %*% t(X) %*% Y
	w
}

## validation, reporting binary error rate
validate <- function(X, Y, w)
{
	Yp <- sign(X %*% w)
	mean(Yp != Y)
}

testVal <- function(degrees=0:7, is.in=T)
{
	if(max(degrees)>7 | min(degrees)<0) stop("Wrong range of degrees.")
	
	errors <- vector()
	for(d in degrees)
	{
		Ztrain <- nonlinTrans(Xtrain, d)
		Zval <- nonlinTrans(Xval, d)
		g <- linReg(Ztrain, Ytrain)
		#print(g)
		if(is.in)
		{
			e.val <- validate(Zval, Yval, g)		
			errors <- c(errors, e.val)
		}
		else
		{	
			e.out <- validate(nonlinTrans(Xout, d), Yout, g)
			errors <- c(errors, e.out)
		}
		names(errors)[length(errors)] <- d
	}
	errors
}
		
	
	
	


