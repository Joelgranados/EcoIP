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
    pixAccum = "" # FIXME: There has to be a better way to initialize.

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

            if (pixAccum == "")
                pixAccum = getInPolyPixels(img,csv[[j]]$polygon)
            else
                pixAccum = rbind(pixAccum, getInPolyPixels(img,csv[[j]]$polygon))
        }
    }

    if (pixAccum == "")
    {
        print ("Failed to accumulate any pixels.")
        return (FALSE)
    }

    # Remove first row
    pixAccum = pixAccum[-1,]

    return (pixAccum)
}
