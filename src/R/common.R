getCSV <- function(filename)
{
    if ( !file.exists(filename) )
        return (FALSE)

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

    # Note: Matlab's coordinate system has (0,0) in top left and indexes
    # as (column, row). R also has (0,0) in top left but indexes as
    # (row, column). We adjust this by using rev.
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
        return (FALSE)

    if ( !file.exists(filename) )
        return (FALSE)

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
        return (FALSE)

    if ( dim(img)[3] != 3 )
        return (FALSE)

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
