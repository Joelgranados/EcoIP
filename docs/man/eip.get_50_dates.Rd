\name{eip.get_50_dates}
\alias{eip.get_50_dates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get 50 \% onset date estimations
}
\description{
This function generates 2 dates: The first is when 50 \% of the onset has
ocurred. The second is when 50 \% of the onset has gone senescent.
}
\usage{
eip.get_50_dates(sig_obj)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sig_obj}{
list
It is the signal object returned by eip.genMiss().
}
}
%%\details{}
\value{
Returns a list of two characters containing the 50 \% dates.
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
\examples{
# Create a signal
ss=list()
ss$signal = cbind ( seq(-10,50), sin(seq(-10,50)/10)+runif(61) )
dates_50 = eip.get_50_dates(ss)
print(dates_50)

}
\keyword{ EcoIP }
