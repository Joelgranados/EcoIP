\name{eip.show_sig}
\alias{eip.show_sig}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Plots various signal stages }
\description{
    Can plot raw signasl, smoothed signals, turning points, sigmoid signals
    and inflection points.
}
\usage{
eip.show_sig(signal = NULL, smoothed = NULL, tpoints = NULL, sigmoid = NULL, ipoints = NULL, xlim = NULL, ylim = NULL)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{
String or vector
The original signal. Default is NULL.
}
  \item{smoothed}{
vector
The smoothed version of the signal. eip.smooth$ss. Default is NULL.
}
  \item{tpoints}{
list
List of points marking the valleys and peaks in the smoothed
signal. eip.smooth$tp. Default is NULL.
}
  \item{sigmoid}{
matrix
The sigmoid signal. eip.sigmoid$sigmoid. Defaults to NULL.
}
  \item{ipoints}{
vector
The inflection points for the sigmoid signal. eip.sigmoid$ip.
Defaults to NULL
}
  \item{xlim}{
vector
Two value vector with the range of the x axis of the plot.
Automatically calculated.
}
  \item{ylim}{
vector
Two value vector with the range of the y axis of the plot.
Automatically calculated.
}
}
%%\details{}
\value{ It plots different signals according to the argument list.}
%%\references{}
\author{@EIP_AUTHOR@}
%%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{eip.showModel}}
\code{\link{eip.genOutput}}
\code{\link{eip.histcmp}}
\code{\link{eip.plot}}
\code{\link{eip.smooth}}
}
%\examples{} FIXME: create an examples.
\keyword{ EcoIP }
