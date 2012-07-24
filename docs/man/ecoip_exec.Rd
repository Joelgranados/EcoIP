\name{ecoip_exec}
\alias{ecoip_exec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ecoip_exec}
\description{description}
\usage{
ecoip_exec(arguments = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{arguments}{ arg1 }
}
\details{
Details
}
\value{
    The value that is return
}
\references{
    Refs
}
\author{@EIP_AUTHOR@}

\note{
    Notes
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
function (arguments = "") 
{
    optMat = matrix(data = c("help", "h", 0, "logical", "\tPrints help information\n", 
        "aid", "H", 0, "logical", "\tPrints the totality of the help information\n", 
        "examples", "e", 0, "logical", "\tPrints example commands to get you started\n", 
        "version", "v", 0, "logical", "\tPrints version information\n", 
        "generate", "G", 1, "character", paste("\t[DNBM|modInfo|ma_vid|bc_vid|ma_sig|bc_sig]\n", 
            "\tDNBM -> Discreate Naive Bayesian Model.\n", "\tmodInfo -> Prints the models info.\n", 
            "\tma_vid -> A video of the masks. Depends on ffmpeg\n", 
            "\tbc_vid -> A video that counts blobs. Depends on ffmpeg\n", 
            "\tma_sig -> A signal of masks means.\n", "\tbc_sig -> A signal of blob counts.\n", 
            "\thistcmp -> Histogram comparison.\n", "\tupdate -> Update model with current code.\n", 
            "\tThis argument is necessary\n"), "trdir", "T", 
        2, "character", "\tPath to training images and csv files. Required with DNBM\n", 
        "tedir", "d", 2, "character", paste("\tPath to data images. Required with DNBM\n", 
            "\tIf undefined for signal or video, the saved dir is used\n"), 
        "bins", "b", 2, "integer", "\tNumber of bins to use for the color signal. Default 100.\n", 
        "folds", "f", 2, "integer", "\tNumber of folds for S-fold error calculation, Default -1.\n", 
        "color_space", "c", 2, "character", paste("\tColor space in which the calculations are to take place\n", 
            "\t[rgb|hsv|CIEXYZ|CIELAB|CIELUV|yCbCr|ExG]. Default CIELAB.\n", 
            "\tHas effect only with DNBM\n"), "morphs", "M", 
        2, "character", paste("\t[shape,size,action[;shape,size,action]...]\n", 
            "\tSpecify morphological actions. Relevant only in video.\n", 
            "\tshape = [box|disc|diamond]\n", "\taction = [dilate|erode|open|close]\n", 
            "\tsize = Size of the structuring element.\n"), "rinstall", 
        "I", 0, "logical", paste("\tInstalls needed packages. It will not install imageMagick\n", 
            "\tnor GTK+. Need to have admin rights.\n"), "fglabl", 
        "F", 2, "character", "\tString used in csv files for foreground. Default 'foreground'\n", 
        "bglabl", "B", 2, "character", "\tString used in csv files for background. Default 'background'\n", 
        "mfile", "m", 2, "character", paste("\tPath were required model is stored.", 
            "\tRequired with modInfo, video & signal\n"), "sig_rdata", 
        "s", 0, "logical", "\tWhen used the format of the signal output file is R binary\n", 
        "output", "O", 2, "character", "\tStuff gets output to this file path. Default depends on generate\n", 
        "sig_overwrite", "1", 0, "logical", "\tWeather to overwrite the signal file or not. Default is NO.\n", 
        "vid_sbys", "i", 0, "logical", paste("\tThis option controls the type of video generated\n", 
            "\tWhen present a video of the mask side by side with the original\n", 
            "\tis created. Default is to create only masked videos\n"), 
        "vid_overwrite", "o", 0, "logical", "\tWeather to overwrite the video file or not. Default is NO.\n", 
        "msgf_sigma", "W", 2, "double", paste("\tStandard deviation used to create gaussiand smoothing filter.\n", 
            "\tIt is only used in the model calculation. Default is 4\n"), 
        "msgf_size", "w", 2, "integer", paste("\tSize of gauss smoothing filter (in pixels).\n", 
            "\tIt is only used in the model calculation. Default is 5 pix\n", 
            "\t0 means no gaussian smoothing.\n"), "hc_pct", 
        "P", 2, "double", paste("\tThis is the percent of the total collected data that is\n", 
            "\tused to create the histogram comparison. Valid only with\n", 
            "\tthe --generate=histcmp option. Default is 0.05\n"), 
        "priors", "p", 2, "character", paste("\tforegroundPrior,backgroudPrior\n", 
            "\tSpecifies the value of the prior in the Naive Bayesian Model\n", 
            "\tcreation. Should idealy add 1. Default is autocalculated\n", 
            "\tOnly used in naive bayesian model creation\n"), 
        "remove_too_many", "R", 2, "logical", paste("\tRemove images that contain 'too many' blobs. Decision\n", 
            "\tis based on standard deviation and mean from trained blobs\n", 
            "\tDefault is FALSE\n"), "remove_too_big", "r", 2, 
        "logical", paste("\tRemove images that are have 'too big' blobs. Decision is\n", 
            "\tbased on standard deviation and mean of trained blob size.\n", 
            "\tDefault is FALSE\n"), "adj_mod", "z", 2, "character", 
        "\tPath to the Adjacent model.\n", "debug", "D", 0, "logical", 
        "\tPrints debug information\n"), ncol = 5, byrow = T)
    cmdArgs = strsplit(arguments, " ")[[1]]
    opts = getopt(optMat, opt = cmdArgs)
    if (!is.null(opts$help)) 
        return(usage(optMat))
    if (!is.null(opts$aid)) 
        return(usage(optMat, long = TRUE))
    if (!is.null(opts$rinstall)) {
        if (ecoip_install("fields") == 1 || ecoip_install("digest") == 
            1 || ecoip_install("EBImage") == 1) 
            stop("Automatic package install failed")
        return(0)
    }
    if (!is.null(opts$version)) {
        version()
        return(0)
    }
    if (!is.null(opts$examples)) 
        return(examples())
    if (is.null(opts$bins)) {
        opts$bins = 100
    }
    if (is.null(opts$folds)) {
        opts$folds = -1
    }
    if (is.null(opts$color_space)) {
        opts$color_space = "CIELAB"
    }
    if (is.null(opts$fglabl)) {
        opts$fglabl = "foreground"
    }
    if (is.null(opts$bglabl)) {
        opts$bglabl = "background"
    }
    if (is.null(opts$msgf_sigma)) {
        opts$msgf_sigma = 4
    }
    if (is.null(opts$msgf_size)) {
        opts$msgf_size = 5
    }
    if (is.null(opts$vid_sbys)) {
        opts$vid_sbys = FALSE
    }
    if (is.null(opts$vid_overwrite)) {
        opts$vid_overwrite = FALSE
    }
    if (is.null(opts$sig_rdata)) {
        opts$sig_rdata = FALSE
    }
    if (is.null(opts$sig_overwrite)) {
        opts$sig_overwrite = FALSE
    }
    if (is.null(opts$morphs)) {
        opts$morphs = ""
    }
    opts$morphsList = list()
    if (is.null(opts$hc_pct)) {
        opts$hc_pct = 0.05
    }
    if (is.null(opts$output) && !is.null(opts$generate)) {
        if (opts$generate == "histcmp") {
            opts$output = file.path(getwd(), "histcmp.svg")
        }
        else if (opts$generate == "bc_sig" || opts$generate == 
            "ma_sig") {
            if (opts$sig_rdata == FALSE) 
                opts$output = file.path(getwd(), "signal.txt")
            else opts$output = file.path(getwd(), "signal.Rdata")
        }
        else if (opts$generate == "ma_vid" || opts$generate == 
            "bc_vid") {
            opts$output = file.path(getwd(), "video.mp4")
        }
        else opts$output = "output.txt"
    }
    if (is.null(opts$priors)) 
        opts$priors = list(fg = NULL, bg = NULL)
    else {
        ptmp = strsplit(opts$priors, ",")[[1]]
        if (length(ptmp) != 2) 
            stop("You must define 2 prior values for the --priors argument")
        pfg = as.numeric(ptmp[1])
        pbg = as.numeric(ptmp[2])
        if (is.na(pfg) || is.na(pbg)) 
            stop("Both the foreground and background values must be ints")
        opts$priors = list(fg = pfg, bg = pbg)
    }
    if (is.null(opts$remove_too_many)) {
        opts$remove_too_many = FALSE
    }
    if (is.null(opts$remove_too_big)) {
        opts$remove_too_big = FALSE
    }
    if (is.null(opts$adj_mod)) {
        opts$adj_mod = NULL
    }
    if (length(cmdArgs) == 0) 
        return(usage(optMat, st = 1))
    if (is.null(opts$generate)) 
        stop("=== PLEASE DEFINE THE --generate OPTION ===\n")
    if (opts$generate == "DNBM" && (is.null(opts$trdir) || is.null(opts$tedir))) 
        stop("=== tedir AND trdir MUST BE DEFINED ===\n")
    if ((opts$generate == "modInfo" || opts$generate == "update" || 
        opts$generate == "ma_vid" || opts$generate == "bc_vid" || 
        opts$generate == "ma_sig" || opts$generate == "bc_sig") && 
        is.null(opts$mfile)) 
        stop("=== MUST DEFINE --mfile_WHEN USING", opts$generate, 
            "  ===\n")
    if (opts$generate == "histcmp" && is.null(opts$trdir)) 
        stop("=== trdir MUST BE DEFINED WITH histcmp OPTION ===\n")
    if (opts$generate == "ma_vid" || opts$generate == "bc_vid") {
        res = system("ffmpeg -version", ignore.stderr = T, ignore.stdout = T)
        if (res != 0) 
            stop("=== THE ffmpeg COMMAND MUST BE INSTALLED ===\n")
    }
    if (!is.null(opts$trdir) && !file.exists(opts$trdir)) 
        stop("=== THE ", opts$trdir, " DIRECTORY DOES NOT EXIST ===\n")
    if (!is.null(opts$tedir) && !file.exists(opts$tedir)) 
        stop("=== THE ", opts$tedir, " DIRECTORY DOES NOT EXIST ===\n")
    if (file.exists(opts$output)) 
        stop("=== THE ", opts$output, " FILE EXISTS. ERASE IT ===\n")
    if (!is.null(opts$mfile) && !file.exists(opts$mfile)) 
        stop("=== THE ", opts$mfile, " FILE DOES NOT EXIST ===\n")
    if (!is.null(opts$adj_mod) && !file.exists(opts$adj_mod)) 
        stop("=== THE ", opts$adj_mod, " FILE DOES NOT EXIST ===\n")
    if (nchar(opts$morphs) > 0) {
        mstmp = strsplit(opts$morphs, ";")[[1]]
        opts$morphsList = list()
        for (i in 1:length(mstmp)) {
            mtmp = strsplit(mstmp[i], ",")[[1]]
            if (length(mtmp) != 3) 
                stop("=== --morphs MUST HAVE shape, size AND action ===\n")
            if (!mtmp[1] \%in\% morphShapes) 
                stop("=== ", mtmp[1], " INVALID SHAPE IN --morphs ===\n")
            ss = as.integer(mtmp[2])
            if (is.na(ss)) 
                stop("=== ", mtmp[2], " IS NOT AN INTEGER in --morphs ===\n")
            if (!mtmp[3] \%in\% names(morphFuncs)) 
                stop("=== ", mtmp[3], " INVALID ACTION IN --morphs ===\n")
            opts$morphsList[[i]] = common.getStructElem(ss, act = mtmp[3], 
                type = mtmp[1])
        }
    }
    if (!is.null(opts$debug)) 
        printopts(opts)
    for (i in 1:length(ecoip_packages)) suppressMessages(library(ecoip_packages[i], 
        character.only = TRUE))
    if (class(try(source(file.path(ecoip_wd, "R/common.R")))) == 
        "try-error" || class(try(source(file.path(ecoip_wd, "naiveBayes.R")))) == 
        "try-error" || class(try(source(file.path(ecoip_wd, "colorTrans.R")))) == 
        "try-error" || class(try(source(file.path(ecoip_wd, "imageTrans.R")))) == 
        "try-error") 
        stop("Make sure you call source with chdir=TURE\n")
    if (opts$generate == "DNBM") {
        generate.DNBM(opts)
    }
    else if (opts$generate == "ma_sig" || opts$generate == "bc_sig") {
        generate.signal(opts)
    }
    else if (opts$generate == "bc_vid" || opts$generate == "ma_vid") {
        generate.video(opts)
    }
    else if (opts$generate == "histcmp") {
        generate.histcmp(opts)
    }
    else if (opts$generate == "modInfo") {
        generate.modelInformation(opts)
    }
    else if (opts$generate == "update") {
        common.update(opts$mfile)
    }
    else {
        stop("=== THE ", opts$generate, " OPTION IS NOT DEFINED ===\n")
    }
    return(0)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
