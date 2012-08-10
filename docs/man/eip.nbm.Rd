\name{eip.nbm}
\alias{eip.nbm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Naive Bayesian Model
}
\description{
This function genereates a Naive Bayesian Model.
}
\usage{
eip.nbm(trdir, tedir, bins = 100, folds = -1, color_space = "CIELAB",
        msgf_size = 5, msgf_sigma = 4, priors = NULL, nbmsave = FALSE,
        fglabl = "foreground", bglabl = "background")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{trdir}{
String.
Path to training images.
}
  \item{tedir}{
String.
Path to data images.
}
  \item{bins}{
Integer.
Number of bins to use for the color signal. Default 100.
}
  \item{folds}{
Integer
Number of folds for S-fold error calculation, Default -1.
}
  \item{color_space}{
String [rgb|hsv|CIEXYZ|CIELAB|CIELUV|yCbCr|ExG].
Color space in which the calculations are to take place. Default CIELAB.
}
  \item{msgf_size}{
Integer.
Size of gauss smoothing filter (in pixels). Used in model
calculation. Default is 5 pix 0 means no gaussian smoothing.
}
  \item{msgf_sigma}{
Double
Standard deviation used to create gaussiand smoothing filter. It
is only used in the model calculation. Default is 4.
}
  \item{priors}{
String [foregroundPrior,backgroundPrior]
Specifies the value of the prior in the Naive Bayesian Model
creation. Should idealy add 1. Default is autocalculated. Used in
naive bayesian model creation
}
  \item{nbmsave}{
Boolean.
Saves the created model when TRUE.
}
  \item{fglabl}{
String.
The name of the foreground label. Defaults to 'foreground'.
}
  \item{bglabl}{
String.
The name of the background label. Defaults to 'background'.
}
}
%%\details{}
\value{
It returns an environment that represents the model.
}
%%\references{}
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
# Create the Naive Bayesian Model.
train_dir = file.path(system.file("extdata", package="EcoIP"), "images/treetr")
test_dir = file.path(system.file("extdata", package="EcoIP"), "images/treete")
M = eip.nbm(trdir=train_dir, tedir=test_dir, bins=200, folds=3)

# Show M
eip.showModel(M)
}

\keyword{ EcoIP }
