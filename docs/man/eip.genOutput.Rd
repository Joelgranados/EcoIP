\name{eip.genOutput}
\alias{eip.genOutput}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate Output
}
\description{
This function generates signal output. It can either be a video or a table. Its
arguments are a way of tweeking the resulting signal.
}
\usage{
eip.genOutput(encoding, process, model, tedir, morphs = "",
                vid_sbys = FALSE, remove_too_many = FALSE,
                remove_too_big = FALSE, output = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{encoding}{
String.
It can be either 'signal' or 'video'. When 'signal' is passed,
the result is a table. When 'video' is passed, a video is created. The 'output'
variable will point to the place where the output will be created.
}
  \item{process}{
String.
It can be either 'mask' or 'blobs'. When 'mask' is passed, the
signal is created with a simple (no morphological operators) mask. When 'blobs'
is passed, the signal is created using morphological operators.
}
  \item{model}{
String or an environment.
When a String is passed, it represents the
path were the model is stored. If a environment is passed, it represents the
model object (whatever is returned by eip.nbm).
}
  \item{tedir}{
String.
Path to data images.
}
  \item{morphs}{
String [shape,size,action[;shape,size,action]...]
Specify morphological actions. Relevant only in video.
    shape = [box|disc|diamond]
    action = [dilate|erode|open|close]
    size = Size of the structuring element.
}
  \item{vid_sbys}{
Boolean.
This option controls the type of video generated. When present a
video of the mask side by side with the original is created.
Default is to create only masked videos.
}
  \item{remove_too_many}{
Boolean.
Remove images that contain 'too many' blobs. Decision is based on
standard deviation and mean from trained blobs. Default is FALSE.
}
  \item{remove_too_big}{
Boolean.
Remove images that are have 'too big' blobs. Decision is based on
standard deviation and mean of trained blob size. Default is FALSE.
}
  \item{output}{
String.
Stuff gets output to this file path. Default depends on generate
}
}
%%\details{}
\value{
This function does not actually return anything. It will tell you where the
signal was saved.
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

# Create a text signal with the mask process
eip.genOutput( encoding="signal", process="mask", tedir=test_dir,
               output="~/maskSig.txt", model=M )

# Create a signal with the blobs process
eip.genOutput( encoding="signal", process="blobs", tedir=test_dir,
               output="~/blobsSig.txt", model=M )

# Create a video with the mask process. Include original image.
# Remember that for the video you need to have ffmpeg installed
eip.genOutput( encoding="video", process="mask", tedir=test_dir,
               output="~/vidMask.mp4", model=M, vid_sbys=TRUE )

# Create a video with the blobs process. (Requires ffmpeg)
eip.genOutput( encoding="video", process="blobs", tedir=test_dir,
               output="~/vidBlobs.mp4", model=M )

# Clean up all the created files
unlink(c("~/maskSig.txt", "~/blobsSig.txt", "~/vidMask.mp4", "~/vidBlobs.mp4"))

}
\keyword{ EcoIP }
