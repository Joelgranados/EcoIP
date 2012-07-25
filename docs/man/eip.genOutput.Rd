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
\references{
%% ~put references to the literature/web site here ~
}
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
function (encoding, process, model, tedir, morphs = "", vid_sbys = FALSE, 
    remove_too_many = FALSE, remove_too_big = FALSE, output = NULL) 
{
    if (is.null(encoding) || encoding != "signal" || encoding != 
        "video") 
        stop("The encoding needs to be either 'signal' or 'video'.")
    if (is.null(process) || process != "mask" || process != "blobs") 
        stop("The process needs to be either 'mask' or 'blobs'.")
    if (!file.exists(tedir)) 
        stop("=== THE ", tedir, " DIRECTORY DOES NOT EXIST ===\n")
    else model$v.testDir = tedir
    if (class(model) == "character" && file.exists(model)) {
        load(model)
        model = self
        rm(self)
    }
    else if (is.null(model$v.type) || model$v.type != "dnbm") 
        stop("You need to pass a model as returned by eip.nbm")
    G = NULL
    if (!is.null(model$v.G)) 
        G = model$v.G
    if (is.null(output)) {
        if (encoding == "signal") {
            output = file.path(getwd(), "signal.txt")
        }
        else if (encoding == "video") {
            output = file.path(getwd(), "video.mp4")
        }
        else output = "output.txt"
    }
    if (file.exists(output)) 
        stop("=== THE ", output, " FILE EXISTS. ERASE IT ===\n")
    morphsList = list()
    if (nchar(morphs) > 0) {
        mstmp = strsplit(morphs, ";")[[1]]
        for (i in 1:length(mstmp)) {
            mtmp = strsplit(mstmp[i], ",")[[1]]
            if (length(mtmp) != 3) 
                stop("=== morphs MUST HAVE shape, size AND action ===\n")
            if (!mtmp[1] \%in\% morphShapes) 
                stop("=== ", mtmp[1], " INVALID SHAPE IN morphs ===\n")
            ss = as.integer(mtmp[2])
            if (is.na(ss)) 
                stop("=== ", mtmp[2], " IS NOT AN INTEGER in morphs ===\n")
            if (!mtmp[3] \%in\% names(morphFuncs)) 
                stop("=== ", mtmp[3], " INVALID ACTION IN morphs ===\n")
            morphsList[[i]] = common.getStructElem(ss, act = mtmp[3], 
                type = mtmp[1])
        }
    }
    it = new.ImageTransformer(model$v.testDir, model)
    it$m.append(it, list(transfunc = it$m.calcMask, transargs = list(G = G)))
    if (length(morphsList) <= 0 && process == "blobs") {
        warning("Adding a morphological filter", immediate. = T)
        mlsize = model$m.getMeanPS(model, model$v.labels$fg)
        morphsList[[1]] = common.getStructElem(mlsize)
    }
    if (length(morphsList) > 0) 
        it$m.append(it, list(transfunc = it$m.calcMorph, transargs = list(morphs = morphsList)))
    if (process == "mask") {
        if (encoding == "signal") {
            it$m.append(it, list(transfunc = it$m.accumMean, 
                transargs = list()))
        }
        else if (encoding == "video") {
            if (vid_sbys) 
                it$m.append(it, list(transfunc = it$m.combine, 
                  transargs = list()))
            it$m.append(it, list(transfunc = it$m.saveMask, transargs = list()))
        }
        else stop("Undefined error")
    }
    else if (process == "blobs") {
        if (remove_too_many) 
            it$m.append(it, list(transfunc = it$m.remTooManyBlob, 
                transargs = list()))
        if (remove_too_big) 
            it$m.append(it, list(transfunc = it$m.remTooBigBlob, 
                transargs = list()))
        if (encoding == "signal") {
            it$m.append(it, list(transfunc = it$m.accumBlobCount, 
                transargs = list()))
        }
        else if (encoding == "video") {
            it$m.append(it, list(transfunc = it$m.paintImgBlobs, 
                transargs = list()))
            it$m.append(it, list(transfunc = it$m.saveMask, transargs = list()))
        }
        else stop("Undefined error")
    }
    else {
        stop("Undefined error")
    }
    if (encoding == "signal") 
        it$m.append(it, list(transfunc = it$m.saveTable, transargs = list(tablename = output)), 
            indTrans = F)
    else if (encoding == "video") 
        it$m.append(it, list(transfunc = it$m.genVid, transargs = list(videoname = output)), 
            indTrans = F)
    res = it$m.trans(it)
    cat("\nThe new output was created at", output, "\n")
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
