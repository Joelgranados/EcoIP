\name{eip.smooth}
\alias{eip.smooth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Smoothing functionality for EcoIP.
}
\description{
Generate smoothed versions of signals resulting from genSignal.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eip.smooth(signal, stype = "MA", ma_coeffs = 7, lo_span=2/3, lo_iter=3 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{
String, Vector.
If it is a string, it should point to a table in the filesystem. When
generating from file, we assume data is on the 2nd col. When a vector it
should be one dimensional.
}
  \item{stype}{
String [MA|LO]
MA -> Moving Average
LO -> Lowess
Type of smoothing process. Default is MA
}
  \item{ma_coeffs}{
Numeric.
Coefficients for MA. if =# then coeffs of that size is created. if =c(#...#)
then coeffs is used directly (sum(ma_coeffs)=1). Default is 7.
}
  \item{lo_span}{
Numeric.
Proportion of points that influences the smoothing.
}
  \item{lo_iter}{
Numeric.
Number of iterations. More iterations means more robustness.
}
}
%%\details{}
\value{
Returns the new smoothed out signal.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{@EIP_AUTHOR@}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{eip.showModel}}
\code{\link{eip.genOutput}}
\code{\link{eip.histcmp}}
}

\examples{
    signal = sin(seq(1,100)/10)+runif(100)
    plot(eip.smooth(signal, stype="MA", ma_coeffs=21))
}
\keyword{ EcoIP }
