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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It returns an environment that represents the model.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{@EIP_AUTHOR@}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (trdir, tedir, bins = 100, folds = -1, color_space = "CIELAB", 
    msgf_size = 5, msgf_sigma = 4, priors = NULL, nbmsave = FALSE, 
    fglabl = "foreground", bglabl = "background") 
{
    G = NULL
    if (msgf_size > 0) 
        G = makeBrush(size = msgf_size, sigma = msgf_sigma, shape = "gaussian")
    lablList = list(fg = fglabl, bg = bglabl)
    if (is.null(priors)) 
        priors = list(fg = NULL, bg = NULL)
    else {
        ptmp = strsplit(priors, ",")[[1]]
        if (length(ptmp) != 2) 
            stop("You must define 2 prior values for the --priors argument")
        pfg = as.numeric(ptmp[1])
        pbg = as.numeric(ptmp[2])
        if (is.na(pfg) || is.na(pbg)) 
            stop("Both the foreground and background values must be ints")
        priors = list(fg = pfg, bg = pbg)
    }
    dnbm = new.DiscNaiveBayesianModel(trdir, tedir, nbins = bins, 
        nfolds = folds, transform = color_space, labls = lablList, 
        G = G, priors = priors)
    dnbm$m.generate(dnbm)
    if (nbmsave && !file.exists(dnbm$v.outfile)) {
        dnbm$m.save(dnbm)
        cat("\nThe new model was saved at", dnbm$v.outfile, "\n")
    }
    return(dnbm)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
