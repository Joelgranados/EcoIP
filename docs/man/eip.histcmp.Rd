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
eip.histcmp(trdir, bins = 100, pct = 0.05, output = "histcmp.svg",
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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
%%\references{}
\author{@EIP_AUTHOR@}
%%\note{}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (trdir, bins = 100, pct = 0.05, output = "histcmp.svg", 
    fglabl = "foreground", bglabl = "background") 
{
    lablList = list(fg = fglabl, bg = bglabl)
    if (!file.exists(trdir)) 
        stop("Training dir does not exist")
    dnbm = new.DiscNaiveBayesianModel(trdir, getwd(), nbins = bins, 
        nfolds = -1, transform = "rgb", labls = lablList)
    CH = common.getColorHists(dnbm, pct)
    common.plotColorHists(CH, plotName = output)
    cat("\nThe new histogram analysis was created at", output, 
        "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
