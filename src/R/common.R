getCSV <- function(filename)
{
    if ( !file.exists(filename) )
    {
        sprintf ("File %s not found.", filename)
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

getImgMat <- function(filename)
{
    if ( require(adimpro) == FALSE )
    {
        sprintf ("Package admipro not found. Please install.")
        return (FALSE)
    }

    if ( !file.exists(filename) )
    {
        sprintf ("File %s not found.", filename)
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
        sprintf ("Package fields not found. Please install it.")
        return (FALSE)
    }

    if ( dim(img)[3] != 3 )
    {
        sprintf ("The image must have three dimensions. (RGB).")
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
    pixels = cbind(img[,,1][inMat], img[,,2][inMat], img[,,3][inMat])
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
        sprintf ("Directory %s not found.", filename)
        return (FALSE)
    }

    # Accumulator of pixel values
    pixAccum = c(NA,NA, NA) #FIXME more than 3 dims????

    filePairs = getImgCsv(directory)

    # Check all csv files
    for ( i in 1:length(filePairs) )
    {
        img = getImgMat(filePairs[[i]]$img)
        csv = getCSV(filePairs[[i]]$csv)

        # Check all annotations in csv file
        for (j in 1:length(csv))
        {
            if (csv[[j]]$label!=label)
                next

            pixAccum = rbind (pixAccum, getInPolyPixels(img,csv[[j]]$polygon))
        }
    }

    if (length(pixAccum) == 3) #FIXME more than 3 dims???
    {
        sprintf ("Failed to accumulate any pixels.")
        return (FALSE)
    }

    # Remove first row
    pixAccum = pixAccum[-1,]

    return (pixAccum)
}
