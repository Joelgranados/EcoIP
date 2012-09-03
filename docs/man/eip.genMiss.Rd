\name{eip.genMiss}
\alias{eip.genMiss}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Generate missing dates }
\description{
    It adds dates that are not present in the signal and interpolates the
    missing values with a linear function
}
\usage{
eip.genMiss(signal)
}
\arguments{
  \item{signal}{
Vector or String
When String it is the path to the signal.
When vector it is whatever eip.get_table returns
}
}
%%\details{}
\value{
Returns a list:
    $signal is the original signal with the interpolated data and dates added.
    $missing is a boolean vector that is TRUE when values are missing and
    FALSE otherwise.
}
%%\references{}
\author{@EIP_AUTHOR@}
%%\note{}

\seealso{
\code{\link{eip.showModel}}
\code{\link{eip.genOutput}}
\code{\link{eip.histcmp}}
\code{\link{eip.plot}}
\code{\link{eip.smooth}}
\code{\link{eip.sigmoid}}
\code{\link{eip.nbm}}
}
%%\examples{}
\keyword{ EcoIP }
