getPoly <- function(filename)
{
    if ( !file.exists(filename) )
        return (FALSE)

    # Valid for files created by annotation
    input = read.csv(filename, skip=4, header=FALSE)

    retL = list()

    for ( i in c( 1:length(input[,1]) ) ) {
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
    return (retL)
}

getImgMat <- function(filename)
{
    if ( require(adimpro) == FALSE )
        return (FALSE)

    if ( !file.exists(filename) )
        return (FALSE)

    retImg = read.image(filename, compress=FALSE)
    #FIXME: This might be 270 dpending on the direction.
    retImg = rotate.image(retImg, angle = 90, compress=NULL)
    retImg = extract.image(retImg)

    return (retImg)
}

getInPolyPixels <- function(img, poligono)
{
    if ( require(fields) == FALSE )
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

    # return Mat defining what coordinate falls into the poligon.
    return ( matrix(in.poly(ab, poligono),ncol=nCols,nrow=nRows) )
}
