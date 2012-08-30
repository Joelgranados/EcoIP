\name{eip.histcmp}
\alias{eip.histcmp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Histogram Comparison
}
\description{
This function creates plots for every available color space in such a way that
the separability of each color space is seen.
}
\usage{
eip.histcmp(trdir, bins = 100, pct = 0.05, output = NULL,
            fglabl = "foreground", bglabl = "background")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trdir}{
String.
Path to training directory
}
  \item{bins}{
Integer.
Number of bins per channel, Default is 100.
}
  \item{pct}{
Double.
This is the percent of the total collected data that is used to
create the histogram comparison. Default is 0.05.
}
  \item{output}{
String.
Where you want the resulting histogram to be saved.
}
  \item{fglabl}{
String.
Name of foreground. Defaults to 'foreground'
}
  \item{bglabl}{
String.
Name of background. Defaults to 'background'
}
}
%%\details{}
\value{
It outputs a plot of a histogram comparison.
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
# Create the histogram comparison
train_dir = file.path(system.file("extdata", package="EcoIP"), "images/treetr")
eip.histcmp(trdir=train_dir, bins=200, output="~/histcmp.svg")

# Clean up
unlink(c("~/histcmp.svg"))
}
\keyword{ EcoIP }
