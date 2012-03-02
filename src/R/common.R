getCSV <- function(filename)
{
    if ( !file.exists(filename) )
    {
        print ( paste("File ", filename, "not found.") )
        return (FALSE)
    }

    # Valid for files created by annotation
    input = read.csv(filename, skip=4, header=FALSE)

    retL = list()

    for ( i in c(1:length(input[,1])) )
    {
        #col16...-> polygon
        ptemp = input[i,16:length(input[i,])]
        # Remove the NAs from the list.
        ptemp = ptemp[sapply(ptemp, function(x) !any(is.na(x)))]

        # col1->filename, col3->label
        retL[[i]] = list(
            name=as.character(input[i,1]),
            label=as.character(input[i,3]),
            polygon=ptemp )
    }

    # Note: Matlab indexes as (column, row). R indexes as (row, column).
    # We adjust this by using rev.
    for ( i in c(1:length(retL)) )
    {
        numCoor = length(retL[[i]]$polygon)/2 # Should be multiple of 2
        retL[[i]]$polygon = matrix( rev(retL[[i]]$polygon),
                                    nrow=numCoor,
                                    ncol=2,
                                    byrow=TRUE)
    }

    return (retL)
}

getRGBMat <- function(filename)
{
    if ( require(adimpro) == FALSE )
    {
        print ("Package admipro not found. Please install.")
        return (FALSE)
    }

    if ( !file.exists(filename) )
    {
        print ( paste("File ", filename, "not found.") )
        return (FALSE)
    }

    retImg = read.image(filename, compress=FALSE)
    retImg = rotate.image(retImg, angle = 270, compress=NULL)
    retImg = extract.image(retImg)

    # Image values should be 0-1.
    retImg = retImg/65535

    return (retImg)
}

getInPolyPixels <- function(img, poligono)
{
    if ( require(fields) == FALSE )
    {
        print ("Package fields not found. Please install it.")
        return (FALSE)
    }

    # Dimensions are: rows, columns and ColorSpace.
    if ( length(dim(img)) != 3 )
    {
        print ("The image must have three dimensions.")
        return (FALSE)
    }

    # Get numcolumns and numrows
    nRows = dim(img)[1]
    nCols = dim(img)[2]

    # Create [1,2,...nRows,1,2,...nRows...1,2...nRows]. repeated nCols times
    a = rep(c(1:nRows),nCols)
    # Create [1,...1,2,....2,...nCols,nCols,...nCols]. #s are repeated nRows
    b = c(matrix(rep(c(1:nCols),nRows), nrow=nRows, ncol=nCols, byrow=TRUE))

    # Mat has all coordinates. dim(Mat) = [nRows*nCols,2]
    ab = cbind(a,b)

    # Create binary mat. True when pixel is in poly, false otherwise.
    inMat = matrix(in.poly(ab, poligono),ncol=nCols,nrow=nRows)

    # Save memory...
    rm (a,b,ab)

    # To visualize the masked image:
    # > img[,,{1,2,3}] = img[,,{1,2,3}]*inMat
    # > show.image(make.image(IMG))
    pixels = img[,,1][inMat]
    if (dim(img)[3] > 1) # Avoid for(i in 2:1)
        for (i in 2:dim(img)[3])
            pixels = cbind(pixels, img[,,i][inMat])

    return (pixels)
}

# Construct a list of (csvFile, imgFile) pairs.
getImgCsv <- function(directory)
{
    filePairs = list()

    # Valid image extensions: .jpg, .tiff, .png
    imageFiles = list.files(path=directory, pattern=".jpg|.tiff|.png",
                            full.names=TRUE, ignore.case=TRUE)

    for ( ifOffset in 1:length(imageFiles) )
    {
        imgFile = imageFiles[ifOffset]
        csvFile = paste(imageFiles[ifOffset], ".csv", sep="")

        if ( !file.exists(csvFile) )
            next

        # If csvFile and imgFile exist append to filePairs
        appToCount = length(filePairs)+1
        filePairs[[appToCount]] = list(csv=csvFile, img=imgFile)
    }

    return (filePairs)
}

# List of all pixels of all images inside directory.
# For every image there is a csv file.
getPixels <- function(directory, label)
{
    if ( !file.exists(directory) )
    {
        print ( paste("Directory ", filename, "not found.") )
        return (FALSE)
    }

    # Accumulator of pixel values
    pixAccum = NULL

    filePairs = getImgCsv(directory)

    # Check all csv files
    for ( i in 1:length(filePairs) )
    {
        img = getRGBMat(filePairs[[i]]$img)
        # Here is where we would introduce the color space transform
        csv = getCSV(filePairs[[i]]$csv)

        # Check all annotations in csv file
        for (j in 1:length(csv))
        {
            if (csv[[j]]$label!=label)
                next

            pixAccum = rbind(pixAccum, getInPolyPixels(img,csv[[j]]$polygon))
        }
    }

    if (is.null(pixAccum))
    {
        print ("Failed to accumulate any pixels.")
        return (FALSE)
    }

    return (pixAccum)
}

# Calc the Naive Bayesian element. In P(a|b) = (prod(P(b|a))*p(a))/p(b) we are
# calculating P(b|a).
# FIXME: We are concerned about R's pass by value. The link found at
#   http://cran.r-project.org/doc/manuals/R-lang.html#Argument-evaluation
#   suggests that the colMat matrix will not be recopied.
calcNaiveBayesElem <- function(colMat, bins)
{
    histlist = list()
    colMat = as.matrix(colMat)

    for (i in 1:dim(colMat)[2])
        histlist[[i]] = hist(colMat[,i], bins, plot=FALSE)

    if ( length(histlist) == 0 )
    {
        print("Could not histlist")
        return (FALSE)
    }

    return (histlist)
}

create.NaiveBayesianModel <- function(classes, dataPoints, numBins)
{
    if ( !is.matrix(dataPoints) )
    {
        print ( "The dataPoints argument must be a matrix" )
        return (FALSE)
    }

    if ( !is.vector(classes) )
    {
        print ( "The classes argument must be a boolean vector" )
        return (FALSE)
    }

    if ( length(classes) != dim(dataPoints)[1] )
    {
        print("Length of classes must be equal to first dimension dataPoints")
        return (FALSE)
    }

    if ( sum(classes) == 0 || sum(!classes) == 0 )
    {
        print ("Must include data for two classes")
        return (FALSE)
    }

    NBM = list() #Naive Bayesian Model (NBM)
    NBM$bins = seq(0,1,1/numBins)
    NBM$cls1Hists = calcNaiveBayesElem(dataPoints[classes,],NBM$bins)
    NBM$cls0Hists = calcNaiveBayesElem(dataPoints[!classes,],NBM$bins)

    NBM$freq1 = sum(classes)/length(classes)
    NBM$freq0 = sum(!classes)/length(classes)

    NBM$dimension = dim(dataPoints)[2]

    return (NBM)
}

classify.NaiveBayesianModel <- function(NBM, dataInput)
{
    nbmNames = names(NBM)
    if ( is.null(nbmNames) || !"cls1Hists" %in% nbmNames
         || !"cls0Hists" %in% nbmNames || !"freq1" %in% nbmNames
         || !"freq0" %in% nbmNames || !"dimension" %in% nbmNames )
    {
        print ( "The NBM object is not a Naive Bayesian Model Object" )
        return (FALSE)
    }

    if ( dim(dataInput)[2] != NBM$dimension )
    {
        print ("The dimensions of data and model should be the same")
        return (FALSE)
    }

    # Fit the raw data into the bins.
    for (i in 1:dim(dataInput)[2])
        data[,i] = findInterval(data[,i] , NBM$bins)

    # OneZero[,1] -> One probabilities | OneZero[,2] -> Zero Probabilities.
    OneZero = matrix( rep(1,dim(dataInput)[1]*2),
                      ncol=2, nrow=dim(dataInput)[1] )

    # Calculate the One probabilities.
    for (i in 1:dim(dataInput)[2])
        OneZero[,1] = OneZero[,1] * NBM$cls1Hists[[i]]$density[dataInput[,i]]
    OneZero[,1] = OneZero[,1] * NBM$freq1

    # Calculate the Zero probabilities.
    for (i in 1:dim(dataInput)[2])
        OneZero[,2] = OneZero[,2] * NBM$cls0Hists[[i]]$density[dataInput[,i]]
    OneZero[,2] = OneZero[,2] * NBM$freq0

    # Return the classification.
    return(OneZero[,1] > OneZero[,2])
}
