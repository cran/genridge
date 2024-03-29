
#' Ridge Regression Estimates
#' 
#' @name ridge
#' @aliases ridge ridge.default ridge.formula coef.ridge print.ridge vcov.ridge
#'
#' @description  
#' The function \code{ridge} fits linear models by ridge regression, returning
#' an object of class \code{ridge} designed to be used with the plotting
#' methods in this package.
#' 
#' Ridge regression shrinkage can be parameterized in several ways. If a vector
#' of \code{lambda} values is supplied, these are used directly in the ridge
#' regression computations. Otherwise, if a vector \code{df} is supplied the
#' equivalent values of \code{lambda}.  In either case, both \code{lambda} and
#' \code{df} are returned in the \code{ridge} object, but the rownames of the
#' coefficients are given in terms of \code{lambda}.
#' 
#' @param y A numeric vector containing the response variable. NAs not allowed.
#' @param X A matrix of predictor variables. NA's not allowed. Should not
#'        include a column of 1's for the intercept.
#' @param formula For the \code{formula} method, a two-sided formula.
#' @param data For the \code{formula} method, data frame within which to
#'        evaluate the formula.
#' @param lambda A scalar or vector of ridge constants. A value of 0
#'        corresponds to ordinary least squares.
#' @param df A scalar or vector of effective degrees of freedom corresponding
#'        to \code{lambda}
#' @param svd If \code{TRUE} the SVD of the centered and scaled \code{X} matrix
#'        is returned in the \code{ridge} object.
#' @param x,object An object of class \code{ridge}
#' @param \dots Other arguments, passed down to methods
#' @param digits For the \code{print} method, the number of digits to print.
#' 
#' @return A list with the following components: 
#' \item{lambda}{The vector of ridge constants} 
#' \item{df}{The vector of effective degrees of freedom corresponding to \code{lambda}} 
#' \item{coef}{The matrix of estimated ridge regression coefficients} 
#' \item{scales}{scalings used on the X matrix} 
#' \item{kHKB}{HKB estimate of the ridge constant}
#' \item{kLW}{L-W estimate of the ridge constant} 
#' \item{GCV}{vector of GCV values} 
#' \item{kGCV}{value of \code{lambda} with the minimum GCV}
#' 
#' If \code{svd==TRUE}, the following are also included:
#' 
#' \item{svd.D}{Singular values of the \code{svd} of the scaled X matrix}
#' \item{svd.U}{Left singular vectors of the \code{svd} of the scaled X matrix.
#'       Rows correspond to observations and columns to dimensions.}
#' \item{svd.V}{Right singular vectors of the \code{svd} of the scaled X
#'       matrix. Rows correspond to variables and columns to dimensions.} %% ...
#' @author Michael Friendly
#' @importFrom stats coef coefficients contrasts lm.fit model.matrix model.offset model.response uniroot vcov
#' @export
#' @seealso \code{\link[MASS]{lm.ridge}} for other implementations of ridge
#' regression
#' 
#' \code{\link{traceplot}}, \code{\link{plot.ridge}},
#' \code{\link{pairs.ridge}}, \code{\link{plot3d.ridge}}, for 1D, 2D, 3D plotting methods
#' 
#' \code{\link{pca.ridge}}, \code{\link{biplot.ridge}},
#' \code{\link{biplot.pcaridge}} for views in PCA/SVD space
#' 
#' \code{\link{precision.ridge}} for measures of shrinkage and precision
#' 
#' @references Hoerl, A. E., Kennard, R. W., and Baldwin, K. F. (1975), "Ridge
#' Regression: Some Simulations," \emph{Communications in Statistics}, 4,
#' 105-123.
#' 
#' Lawless, J.F., and Wang, P. (1976), "A Simulation Study of Ridge and Other
#' Regression Estimators," \emph{Communications in Statistics}, 5, 307-323.
#' @keywords models regression
#' @examples
#' 
#' 
#' #\donttest{
#' # Longley data, using number Employed as response
#' longley.y <- longley[, "Employed"]
#' longley.X <- data.matrix(longley[, c(2:6,1)])
#' 
#' lambda <- c(0, 0.005, 0.01, 0.02, 0.04, 0.08)
#' lridge <- ridge(longley.y, longley.X, lambda=lambda)
#' 
#' # same, using formula interface
#' lridge <- ridge(Employed ~ GNP + Unemployed + Armed.Forces + Population + Year + GNP.deflator, 
#' 		data=longley, lambda=lambda)
#' 
#' 
#' coef(lridge)
#' 
#' # standard trace plot
#' traceplot(lridge)
#' # plot vs. equivalent df
#' traceplot(lridge, X="df")
#' pairs(lridge, radius=0.5)
#' #}
#' 
#' \donttest{
#' data(prostate)
#' py <- prostate[, "lpsa"]
#' pX <- data.matrix(prostate[, 1:8])
#' pridge <- ridge(py, pX, df=8:1)
#' pridge
#' 
#' plot(pridge)
#' pairs(pridge)
#' traceplot(pridge)
#' traceplot(pridge, X="df")
#' }
#' 
#' # Hospital manpower data from Table 3.8 of Myers (1990) 
#' data(Manpower)
#' str(Manpower)
#' 
#' mmod <- lm(Hours ~ ., data=Manpower)
#' vif(mmod)
#' # ridge regression models, specified in terms of equivalent df
#' mridge <- ridge(Hours ~ ., data=Manpower, df=seq(5, 3.75, -.25))
#' vif(mridge)
#' 
#' # univariate ridge trace plots
#' traceplot(mridge)
#' traceplot(mridge, X="df")
#' 
#' \donttest{
#' # bivariate ridge trace plots
#' plot(mridge, radius=0.25, labels=mridge$df)
#' pairs(mridge, radius=0.25)
#' 
#' # 3D views
#' # ellipsoids for Load, Xray & BedDays are nearly 2D
#' plot3d(mridge, radius=0.2, labels=mridge$df)
#' # variables in model selected by AIC & BIC
#' plot3d(mridge, variables=c(2,3,5), radius=0.2, labels=mridge$df)
#' 
#' # plots in PCA/SVD space
#' mpridge <- pca(mridge)
#' traceplot(mpridge, X="df")
#' biplot(mpridge, radius=0.25)
#' }
#' 
#' 
#' @export
ridge <- function(y, ...) {
	UseMethod("ridge")
}

#' @rdname ridge
#' @exportS3Method 
ridge.formula <-
		function(formula, data, lambda=0, df, svd=TRUE, ...){
	
	#code from MASS:::lm.ridge
	m <- match.call(expand.dots = FALSE)
	m$model <- m$x <- m$y <- m$contrasts <- m$... <- m$lambda <-m$df <- NULL
	m[[1L]] <- as.name("model.frame")
	m <- eval.parent(m)
	Terms <- attr(m, "terms")
	Y <- model.response(m)
	X <- model.matrix(Terms, m, contrasts)
	n <- nrow(X)
	p <- ncol(X)
	offset <- model.offset(m)
	if (!is.null(offset)) 
		Y <- Y - offset
	if (Inter <- attr(Terms, "intercept")) {
		Xm <- colMeans(X[, -Inter])
		Ym <- mean(Y)
		p <- p - 1
		X <- X[, -Inter] - rep(Xm, rep(n, p))
		Y <- Y - Ym
	}
	ridge.default(Y, X, lambda=lambda, df=df, svd=svd)
}


#' @rdname ridge
#' @exportS3Method  
ridge.default <-
		function(y, X, lambda=0, df, svd=TRUE, ...){
	#dimensions	
	n <- nrow(X)
	p <- ncol(X)
	#center X and y
	Xm <- colMeans(X)
	ym <- mean(y)
	X <- X - rep(Xm, rep(n, p))
	y <- y - ym
	#scale X, as in MASS::lm.ridge 
	Xscale <- drop(rep(1/n, n) %*% X^2)^0.5
	X <- as.matrix(X/rep(Xscale, rep(n, p)))
	
	XPX <- crossprod(X)
	XPy <- crossprod(X,y)
	I <- diag(p)
	lmfit <- lm.fit(X, y)
	MSE <- sum(lmfit$residuals^2) / (n-p-1)
	HKB <- (p - 2) * MSE/sum(lmfit$coefficients^2)
	LW <- (p - 2) * MSE * n/sum(lmfit$fitted.values^2)
	
	# from ElemStatLearn:::simple.ridge
	svd.x <- svd(X, nu = p, nv = p)
	dd <- svd.x$d
	u <- svd.x$u
	v <- svd.x$v
	if (missing(df)) {
		df <- sapply(lambda, function(x) sum(dd^2/(dd^2 + x)))
	}
	else {
		fun <- function(df, lambda) df - sum(dd^2/(dd^2 + lambda))
		lambda <- sapply(df, FUN = function(df) uniroot(f = function(lambda) fun(df, 
										lambda), lower = 0, upper = 1000, maxiter = 10000)$root)
	}
	
	# prepare output    
	coef <- matrix(0, length(lambda), p)
	cov <- as.list(rep(0, length(lambda)))
	mse <- rep(0, length(lambda))
	
	# loop over lambdas
	for(i in seq(length(lambda))) {
		lam <- lambda[i]
		XPXr <- XPX + lam * I
		XPXI <- solve(XPXr)
		coef[i,] <- XPXI %*% XPy
		cov[[i]] <- MSE * XPXI %*% XPX %*% XPXI
		res <- y - X %*% coef[i,]
		mse[i] <- sum(res^2) / (n-p) 
		dimnames(cov[[i]]) <- list(colnames(X), colnames(X))
	}
	dimnames(coef) <- list(format(lambda), colnames(X))
	
	# calculate GCV, from MASS::lm.ridge
	dn <- length(dd)
	nl <- length(lambda)
	div <- dd^2 + rep(lambda, rep(dn, nl))
	GCV <- colSums((y - X %*% t(coef))^2)/(n - colSums(matrix(dd^2/div, dn)))^2
	k <- seq_along(GCV)[GCV == min(GCV)]
	kGCV <- lambda[k]
	
	result <- list(lambda=lambda, df=df, coef=coef, cov=cov, mse=mse, scales=Xscale, kHKB=HKB, kLW=LW,
			GCV=GCV, kGCV=kGCV)
	if (svd) {
		rownames(u) <- rownames(X)
		colnames(u) <- colnames(v) <- paste("dim", 1:p, sep="")
		rownames(v) <- colnames(X)
		result <- c(result, list(svd.D=dd, svd.U=u, svd.V=v))
	}
	class(result) <- "ridge"
	result
}



#' @rdname ridge
#' @exportS3Method coef ridge
coef.ridge <-
function(object, ...) {
	object$coef
}

#' @rdname ridge
#' @exportS3Method print ridge
print.ridge <-
function(x, digits = max(5, getOption("digits") - 5),...) {
  if (length(coef(x))) {
      cat("Ridge Coefficients:\n")
      print.default(format(coef(x), digits = digits), print.gap = 2, 
          quote = FALSE)
  }
  invisible(x)
}

#' @rdname ridge
#' @exportS3Method vcov ridge
vcov.ridge <- function(object,  ...) {
	object$cov
}
