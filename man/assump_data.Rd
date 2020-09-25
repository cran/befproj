\name{assump_data}
\alias{assump_data}
\docType{data}
\title{assumptions
}
\description{
This is a Data Frame with assumptions about migrations rates, deaths and births.
}
\usage{data("assump_data")}
\format{
  A data frame with 1111 observations on the following 14 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{category}}{a factor with levels \code{asdr_men} \code{asdr_women} \code{asfr} \code{inmig.rates.men} \code{inmig.rates.women} \code{intermig.net.men} \code{intermig.net.women} \code{natpop.men} \code{natpop.women} \code{outmig.rates.men} \code{outmig.rates.women}}
    \item{\code{ar_1}}{a numeric vector}
    \item{\code{ar_2}}{a numeric vector}
    \item{\code{ar_3}}{a numeric vector}
    \item{\code{ar_4}}{a numeric vector}
    \item{\code{ar_5}}{a numeric vector}
    \item{\code{ar_6}}{a numeric vector}
    \item{\code{ar_7}}{a numeric vector}
    \item{\code{ar_8}}{a numeric vector}
    \item{\code{ar_9}}{a numeric vector}
    \item{\code{ar_10}}{a numeric vector}
    \item{\code{ar_11}}{a numeric vector}
    \item{\code{ar_12}}{a numeric vector}
  }
}
\details{
This is a Data Frame that consists of assumptions and input to the population
model. The Data Frame has 14 different variables under category:
age specific death rates (asdr) for men and women, age specific fertility rates
for women (asfr), domestic in migration and out migration rates for men and
women, international in and out net migration for men and women, and the age
specific national population.
}
\source{
Umea kommun
}

\examples{
data(assump_data)
str(assump_data)
}
\keyword{datasets}
