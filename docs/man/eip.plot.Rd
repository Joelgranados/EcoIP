\name{eip.plot}
\alias{eip.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Naive Bayesian Model Plot Generator
}
\description{
It generates plots based on eip.genOutput text signals.
}
\usage{
eip.plot(tfile, ignore_missing = FALSE, output = "plot.pdf", width = 10,
         height = 5, lwidth = 0.25, xlabl = "Time", ylabl = "Value",
         type = "l", lcolor = "red", ptitle = "Title", minimum_show = -1,
         missing_color = "#F0FFFFAA",
         mark_training = NULL, color_training="#FFF0FFAA")
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tfile}{
String. File where the table is kept. Has no default.
}
  \item{ignore_missing}{
Boolean. When TRUE it ignores missing dates. Default is FALSE.
}
  \item{output}{
String. Name of the output file. Defaults to plot.svg. Default is plot.eps.
}
  \item{width}{
Integer. Width of the resulting figure. Defaults to 10.
}
  \item{height}{
Integer. Height of the resulting figure. Defaults to 5.
}
  \item{lwidth}{
Double. Like width. Default is 0.25.
}
  \item{xlabl}{
String. X axis label, Defaults to Time.
}
  \item{ylabl}{
String. Y axis label, Defaults to Value.
}
  \item{type}{
String. The type of plot point. Defaults to l.
}
  \item{lcolor}{
String. Line color. Defaults to red.
}
  \item{ptitle}{
String. Title of the plot, Defaults to Title.
}
  \item{minimum_show}{
Double. All values over this will be given a label tick.
}
  \item{missing_color}{
String. The color given to the missing dates. Defaults to transparent azure.
}
  \item{mark_training}{
String. Date range where the training set is. FROMDATE,TODATE. Default is NULL.
}
  \item{color_training}{
String. Default color of the training rectangle, Default is "#FFF0FFAA" redish.
}
}
%%\details{}
\value{
It creates an EPS formated figure of the data.
}
%%\references{}
\author{@EIP_AUTHOR@}
%%\note{}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{eip.showModel}}
\code{\link{eip.genOutput}}
\code{\link{eip.histcmp}}
}
\examples{
train_dir = file.path(system.file("extdata", package="EcoIP"), "images/treetr")
test_dir = file.path(system.file("extdata", package="EcoIP"), "images/treete")
M = eip.nbm(trdir=train_dir, tedir=test_dir, bins=200, folds=3)

# Show M
eip.showModel(M)

# Create a text signal with the mask process
eip.genOutput( encoding="signal", process="mask", tedir=test_dir,
               output="~/maskSig.txt", model=M )

eip.plot("~/maskSig.txt", output="~/maskSig.pdf", ptitle="Test")

# Clean up
unlink(c("~/maskSig.txt", "~/maskSig.pdf"))
}

\keyword{ EcoIP }
