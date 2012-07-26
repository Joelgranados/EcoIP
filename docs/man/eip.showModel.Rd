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
%%\details{}
\value{
It prints out the model information.
}
%%\references{}
\author{@EIP_AUTHOR@}
%%\note{}

\seealso{
\code{\link{eip.showModel}}
\code{\link{eip.genOutput}}
\code{\link{eip.histcmp}}
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
