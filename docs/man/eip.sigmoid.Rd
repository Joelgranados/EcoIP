\name{eip.sigmoid}
\alias{eip.sigmoid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create sigmoid equivalent signals }
\description{
    From a smoothing object, create a sigmoid equivalent. The smoothing object
    needs to contain the smoothed signal and the turning points.
}
\usage{
eip.sigmoid(sm_obj, silent=T)
}
\arguments{
  \item{sm_obj}{
list
Whatever eip.smooth returns.
}
  \item{silent}{
Boolean
When false we output the error messages that caused the warnings. When
true we ignore the error messages that caused the warnings. Default is
True.
}
}
%%\details{}
\value{
    It return a list containing the sigmoid signal and the inflection
    points.
    retVal$sigmoid is the signal.
    retVal$ip are the points.
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
%\examples{} FIXME: create examples.
\keyword{ EcoIP }
