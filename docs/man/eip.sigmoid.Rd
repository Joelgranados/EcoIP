\name{eip.sigmoid}
\alias{eip.sigmoid}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create sigmoid equivalent signals }
\description{
    The smoothing object is used to find the turning points in the signal.
    Then it tries to fit the original signal to a sigmoid function based on
    the turning points.
    If a fit is not possible, it smoothes the signal and tries again. This
    happens until maxSmoothingSize is reached. A moving average is used for
    as a smoothing operation.
    This is done per sigmoid subsection of the signal. It analyzes the growing
    and decreasing sections separately.
}
\usage{
eip.sigmoid(sm_obj, sig_obj, maxSmoothSize = 30, silent=T)
}
\arguments{
  \item{sm_obj}{
list
Whatever eip.smooth returns.
}
  \item{sig_obj}{
Whatever eip.genMiss return.
}
  \item{maxSmoothSize}{
The maximum size of the smoothing filter. Defaults to 30.
The smoothing begins at 0 and moves towards 30 when not sigmoid fits are
found.
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
