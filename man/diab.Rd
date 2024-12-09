% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{diab}
\alias{diab}
\title{Diabetes Progression}
\format{
A data frame with 442 observations on the following 11 variables.
 \describe{
   \item{\code{prog}}{disease progression, a numeric vector}
   \item{\code{age}}{age, a numeric vector}
   \item{\code{sex}}{integer, a numeric vector}
   \item{\code{bmi}}{body mass index, a numeric vector}
   \item{\code{map}}{mean arterial blood pressure, a numeric vector}
   \item{\code{tc}}{blood serum TC, a numeric vector}
   \item{\code{ldl}}{blood serum low-density lipoprotein ("bad cholersterol"), a numeric vector}
   \item{\code{hdl}}{blood serum high-density lipoprotein ("good cholersterol"), a numeric vector}
   \item{\code{tch}}{blood serum TCH, a numeric vector}
   \item{\code{ltg}}{blood serum lamotrigine, a numeric vector}
   \item{\code{glu}}{blood serum glucose, a numeric vector}
 }
}
\source{
The dataset was taken from the web site for Efron & Hastie (2021), \emph{Computer Age Statistical Inference},
\url{https://hastie.su.domains/CASI_files/DATA/diabetes.csv}.
}
\usage{
data("diab")
}
\description{
These data consist of observations on 442 patients, with the response of interest being 
a quantitative measure of disease progression one year after baseline.

There are ten baseline variables: age, sex, body-mass index (\code{bmi}), average blood pressure (\code{map})
and six blood serum measurements.
}
\details{
Efron & Hastie describe their analysis using the centered predictor variables standardized to unit L2 norm.
\code{\link{ridge}} does not (yet) provide this scaling.
}
\examples{
data(diab)
## maybe str(diab) ; plot(diab) ...

}
\references{
Efron, B., Hastie, T., Johnstone, I., & Tibshirani, R. (2004). Least Angle Regression. 
\emph{The Annals of Statistics}, \bold{32}(2), 407-499. 
\doi{https://doi.org/10.1214/009053604000000067}

Efron, B., & Hastie, T. (2021). \emph{Computer Age Statistical Inference, Student Edition: Algorithms, Evidence, and Data Science},
Cambridge University Press. \doi{https://doi.org/10.1017/9781108914062}
}
\concept{ridge}
\keyword{datasets}
