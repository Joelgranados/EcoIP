\name{eip.showModel}
\alias{eip.showModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Show NBM model
}
\description{
Extracts important information from a model object or file.
}
\usage{
eip.showModel(model = NULL, mfile = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{
Environment.
Its the model object as returned by eip.nbm.
}
  \item{mfile}{
String.
The file where the model was saved.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
It prints out the model information.
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
function (model = NULL, mfile = NULL) 
{
    if (!is.null(model)) {
        model$m.print(model)
    }
    else if (!is.null(mfile)) {
        if (!file.exists(mfile)) 
            stop("mfile does not exist")
        load(mfile)
        self$m.print(self)
    }
    else stop("Please define either a EcoIP object or a path to it.")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
