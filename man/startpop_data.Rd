\name{startpop_data}
\alias{startpop_data}
\docType{data}
\title{Startpopulation
}
\description{
This is a Data Frame with a startpopulation. The ages reaches from 0 to 100.
    The start year is from 2019.
}
\usage{data("startpop_data")}
\format{
  A data frame with 101 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{a numeric vector}
    \item{\code{women}}{a numeric vector}
    \item{\code{men}}{a numeric vector}
  }
}

\source{Statistiska centralbyran, SCB, Swedish statistics
}

\examples{
data(startpop_data)
str(startpop_data)
}
\keyword{datasets}
