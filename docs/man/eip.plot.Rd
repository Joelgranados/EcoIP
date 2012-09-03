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
eip.plot( signal=NULL, smoothed=NULL, sigmoid=NULL, tp=NULL, ip=NULL,
           xlabl="Time", ylabl="Value", xlim=NULL, ylim=NULL,
           width=10, height=5, ptitle="Title", minimum_show=-1,
           output=NULL, lwidth=0.25, type="l", CEX=0.5,
           miss=NULL, missing_color="#F0FFFFAA",
           mark_training=NULL, color_training="#FFF0FFAA",
           si_col="red", sm_col="blue", sig_col="black",
           si_lty="dotted", sm_lty="dashed", sig_lty="solid" )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{signal}{
String or vector.
If string file where the table is kept. If vector, it represents the
signal. Defaults to NULL.
}
  \item{smoothed}{
matrix.
2 column matrix. 1st col are dates, 2nd are values of the smoothed signal
}
  \item{sigmoid}{
matrix
2 column matrix. 1st col are dates, 2nd are values of the sigmoid signal
}
  \item{tp}{
list
tp$peaks are the dates where the smoothed signal peaks.
tp$valleys are the dates where the smoothed signal valleys.
}
  \item{ip}{
vector
The dates where the sigmoid signal changes concavity.
}
  \item{xlabl}{
String. X axis label, Defaults to Time.
}
  \item{ylabl}{
String. Y axis label, Defaults to Value.
}
  \item{xlim}{
vector
Two element vector c(from, to) that controls the x axis range.
Default is NULL.
}
  \item{ylim}{
vector
Two element vector c(from, to) that controls the y axix range.
Default is NULL.
}
  \item{width}{
Integer. Width of the resulting figure. Defaults to 10.
}
  \item{height}{
Integer. Height of the resulting figure. Defaults to 5.
}
  \item{ptitle}{
String. Title of the plot, Defaults to Title.
}
  \item{minimum_show}{
Double. All values over this will be given a label tick.
}
  \item{output}{
String. Name of the output file. Defaults to NULL.
}
  \item{lwidth}{
Double. Like width. Default is 0.25.
}
  \item{type}{
String. The type of plot point. Defaults to l.
}
  \item{CEX}{
Double
Proportion of text size.
}
  \item{miss}{
Boolean Vector.
Vector that is TRUE when the offset is NA and FALSE otherwise. Dfault is NULL
}
  \item{missing_color}{
String. The color given to the missing dates. Defaults to transparent azure.
}
  \item{mark_training}{
String.
Date range where the training set is. FROMDATE,TODATE. Default is NULL.
}
  \item{color_training}{
String.
Default color of the training rectangle, Default is "#FFF0FFAA" redish.
}
  \item{si_col}{
String
Color of the signal signal.
}
  \item{sm_col}{
String
Color of the smoothed signal
}
  \item{sig_col}{
String
Color of the sigmoid signal
}
  \item{si_lty}{
String
Type of line for the signal
}
  \item{sm_lty}{
String
Type of line for the smoothed signal
}
  \item{sig_lty}{
String
Type of line for the sigmoid signal
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
\code{\link{eip.plot}}
\code{\link{eip.smooth}}
\code{\link{eip.sigmoid}}
\code{\link{eip.nbm}}
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
