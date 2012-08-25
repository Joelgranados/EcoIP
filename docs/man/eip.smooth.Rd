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
eip.smooth( signal, output = NULL, stype = "MA",
            ma_coeffs = 7, lo_span=2/3, lo_iter=3,
            gc_sigma=1, gc_size=5, ma2_k=3, ms_w=3,
            ignore_missing=TRUE  )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{
String, Vector.
If it is a string, it should point to a table in the filesystem. When
generating from file, we assume data is on the 2nd col. When a vector it
is what eip.get_table returns.
}
  \item{stype}{
String [MA|LO|GC]
MA -> Moving Average
MA2 -> Moving Average 2 (It handles NA)
LO -> Lowess
GC -> Gauss Convolution
MS -> Markov Smoothing
Type of smoothing process. Default is MA2
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
  \item{output}{
String.
Output file for when you want to put the smoothed signal in a file.
}
  \item{gc_sigma}{
Nueric
Value of sigma for Gauss filter in GC. Default 1.
}
  \item{gc_size}{
Numeric
Size of the 1-D Gauss filter. Default 5. Is forced to uneven.
}
  \item{ignore_missing}{
Boolean
When true we don't create the missing data. Default is TRUE.
}
  \item{ma2_k}{
Numeric
The range of the moving averate is 2*ma2_k+1. Default is 3.
}
  \item{ms_w}{
Numeric
Markov smoothing window size, Defaults to 3.
}
}
%%\details{}
\value{
Returns the new smoothed signal. It changes the second column of the input
signal.
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
\code{\link{eip.plot}}
\code{\link{eip.smooth}}
}

\examples{
    signal = cbind ( seq(1,100), sin(seq(1,100)/10)+runif(100) )
    plot(eip.smooth(signal, stype="MA", ma_coeffs=21)[,2])
}
\keyword{ EcoIP }
