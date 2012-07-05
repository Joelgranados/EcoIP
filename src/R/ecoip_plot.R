# Copyright (C) 2012 Joel Granados <joel.granados@gmail.com>
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

library(getopt)

usage <- function( optMat, st=0 )
{
    cat ( "Usage:\n" )
    cat ( cmdCmd, " OPTIONS\n", sep="" )
    cat ( "\nOPTIONS\n" )

    for ( i in 1:dim(optMat)[1] )
        cat ( "  [--",optMat[i,1],"|-",optMat[i,2],"]\n",optMat[i,5], sep="")
    cat ("\n")

    flush.console()
    return (st)
}

version <- function()
{
    cat ( "\tName: @EIP_NAME@\n" )
    cat ( "\tVersion: @EIP_VER_MAJOR@.@EIP_VER_MINOR@.@EIP_VER_DATE@\n" )
    return (0)
}

generate_missing_dates <- function ( plotTable )
{
    ncols = dim(plotTable)[2]

    # Init return matrix
    allDates = matrix(0, nrow=0, ncol=ncols)

    # Init Date
    dateCount = as.Date(plotTable[1,1])

    for ( i in 1:dim(plotTable)[1] )
    {
        if ( dateCount > as.Date(plotTable[i,1]) ) {
            stop("Lost count of dates when including missing dates...")
        } else if ( dateCount < as.Date(plotTable[i,1]) ) {
            while ( dateCount < as.Date(plotTable[i,1]) )
            {
                allDates = rbind ( allDates,
                                   c(as.character(dateCount),rep(NA,ncols-1)) )
                dateCount = dateCount + 1
            }
        }

        allDates = rbind ( allDates, c(plotTable[i,1], plotTable[i,2]) )
        dateCount = dateCount + 1
    }

    allDates = data.frame(allDates, stringsAsFactors=FALSE)
    allDates[,2] = as.double(allDates[,2])
    colnames (allDates) <- names(plotTable)
    return (allDates)
}

ecoip_plot_generate <- function( opts )
{
    # Get the data
    table = try(read.table(opts$tfile), silent=TRUE)
    if ( class(table) == "try-error" )
    {
        if ( class(try(load(opts$tfile), silent=TRUE)) == "try-error" )
            stop ( "The ", table, " file does not contain data" )
    }

    # Make sure our data is ordered
    table = table[order(table$V1),]

    # Extract the dates
    SUBFROM = 1
    SUBTO = 10
    table[,1] = substr(basename(as.character(table[,1])),SUBFROM, SUBTO)

    # Make sure there are no repeated dates.
    if ( sum(duplicated(table[,1])) > 0 )
        stop ( paste("There are duplicated dates in", opts$tfile) )

    # Introduce Missing dates
    if ( ! opts$ignore_missing )
        table = generate_missing_dates ( table )

    # Output to EPS.
    postscript(file=opts$output, width=opts$width,height=opts$height)

    # FIXME: Give control to the user
    LWD = opts$lwidth # Linewidth
    CEX = .5 # Fontsize

    # Init plot
    plot(table[,2], pch=21, xlab=opts$xlab, ylab=opts$ylab,
         type=opts$type, col=opts$color, main=opts$title, axes=F, lwd=LWD)

    # Calc Indices
    if ( opts$minimum_show < 0 )
        opts$minimum_show = ( min(table[,2], na.rm=TRUE)
                              + abs(min(table[,2], na.rm=TRUE)
                                  + max(table[,2], na.rm=TRUE)*0.25) )

    tmpInd = table[,2]>opts$minimum_show
    tmpInd[is.na(tmpInd)] = FALSE

    # create draw rectangles
    rectPos = matrix(0, ncol=2, nrow=0)
    YFrom = par("usr")[3]
    YTo = par("usr")[4]

    for ( i in 1:dim(table)[1] )
    {
        if ( ! is.na(table[i,2]) )
            next

        ofset = i
        while ( is.na(table[i,2]) )
            i = i + 1

        rectPos = rbind(rectPos, c(ofset-1, i-ofset+1))
    }

    if ( dim(rectPos)[1] > 0 )
        for ( i in 1:dim(rectPos)[1] )
            rect( rectPos[i,1], YFrom, rectPos[i,1]+rectPos[i,2], YTo,
                  col=opts$space_color, border=NA )


    # Calc the tick strings and points where to draw a ticks.
    if ( sum(tmpInd) > 0 )
    {
        labls = table[tmpInd,1]
        AT = which(tmpInd)
    } else {
        labls = table[,1]
        AT = seq(1, length(table[,1]))
    }

    # This is painful. To avoid label overlap only include one tick per "clumped
    # label group". Remove overlapping labels from min distance. This min
    # distance is a function of the plot size and the font size.
    MD = ( (par("cin")[1]*CEX)
           / ( par("fin")[1]/abs(abs(par("usr")[1])-abs(par("usr")[2])) ) )

    ATtmp = c(AT[1])
    Ltmp  = c(labls[1])
    for ( i in 1:(length(AT)-1) )
    {
        DIST = abs(ATtmp[length(ATtmp)] - AT[i+1])
        if ( DIST > MD )
        {
            ATtmp = c(ATtmp, AT[i+1])
            Ltmp = c(Ltmp, labls[i+1])
        }
    }
    AT = ATtmp
    labls = Ltmp

    # Calculate the relative down. Used to place X tick labels
    RD = par("usr")[3]-(abs(par("usr")[3]-par("usr")[4])*0.05)

    # draw axis
    axis(2)
    axis(1, AT, labels=FALSE, lwd=LWD, lwd.ticks=LWD)
    text(AT, RD, srt = 90, adj = 1, labels = labls, xpd = TRUE, cex=CEX)
    box()

    dev.off()
}

ecoip_plot_exec <- function ( arguments = "" )
{
    optMat = matrix ( data=c(
    "help",     "h",    0, "logical",
        "\tPrints help information\n",
    "examples", "e",   0, "logical",
        "\tPrints example commands to get you started\n",
    "version",  "v",    0, "logical",
        "\tPrints version information\n",

    "tfile",     "a",    2, "character",
        "\tFile where the table is kept. Has no default.\n",

    "type",     "t",    2, "character",
        "\tType of plot: l: line, p: points. Defaults to l.\n",

    "color",    "c",    2, "character",
        "\tColor of lines: [red|blue|green|yellow...]Defaults to red.\n",

    "xlab",     "x",    2, "character",
        "\tName of X axis. Defaults to Time\n",
    "ylab",     "y",   2, "character",
        "\tName of Y axis. Defaults to Value.\n",

    "width",    "w",  2, "integer",
        "\tWidth of the resulting figure. Defaults to 1024.\n",
    "height",   "H",    2, "integer",
        "\tHeight of the resulting figure. Defaults to 768.\n",

    "lwidth",    "l",  2, "double",
        "\tLine width.\n",

    "output",   "o",    2, "character",
        "\tName of the output file. Defaults to plot.svg.\n",

    "minimum_show",   "m",    2, "double",
        "\tAll values over this will be given a label tick.\n",

    "show_all",   "s",    2, "logical",
        "\tWhen true we try to put as many labels in X as possible.\n",

    "ignore_missing",   "i",    2, "logical",
        "\tDon't plot the missing dates.\n",

    "space_color",   "S",    2, "character",
        "\tThe color of the missing date spaces. Defaults to \"azure\"\n",

    "title",    "T",   2, "character",
        "\tPlot title. Defaults to Phenology Plot.\n" ),
    ncol=5, byrow=T )
    cmdArgs = strsplit(arguments, " ")[[1]]
    opts = getopt ( optMat, opt=cmdArgs )

    # Take care of simple user commands.
    if ( !is.null(opts$help) )
        return (usage(optMat))
    if ( !is.null(opts$version) )
        version()
    if ( is.null(opts$tfile) )
        return (usage(optMat, st=1))
    if ( length(cmdArgs) == 0 )
        return (usage(optMat, st=1))

    # Set the defaults
    if (is.null(opts$type)) {opts$type = "l"}
    if (is.null(opts$color)) {opts$color = "red"}
    if (is.null(opts$xlab)) {opts$xlab = "Time"}
    if (is.null(opts$ylab)) {opts$ylab = "Value"}
    if (is.null(opts$width)) {opts$width = 10}
    if (is.null(opts$height)) {opts$height = 5}
    if (is.null(opts$output)) {opts$output = "plot.eps"}
    if (is.null(opts$title)) {opts$title = "Title"}
    if (is.null(opts$show_all)) {opts$show_all = FALSE}
    if (opts$show_all) {opts$minimum_show = 0}
    if (is.null(opts$minimum_show)) {opts$minimum_show = -1}
    if (is.null(opts$ignore_missing)) {opts$ignore_missing = FALSE}
    if (is.null(opts$lwidth)) {opts$lwidth = 0.25}
    if (is.null(opts$space_color)) {opts$space_color = "azure"}

    ecoip_plot_generate( opts )
}

