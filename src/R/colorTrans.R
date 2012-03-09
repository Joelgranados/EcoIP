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

# Calc the Naive Bayesian element. In P(a|b) = (prod(P(b|a))*p(a))/p(b) we are
# calculating P(b|a).
# FIXME: We are concerned about R's pass by value. The link found at
#   http://cran.r-project.org/doc/manuals/R-lang.html#Argument-evaluation
#   suggests that the colMat matrix will not be recopied.

passByRefMessage <- function(mess)
{
    return (
        paste("To control memory usage we have implemented pass by reference ",
            "by using R's environments. When calling one of these functions, ",
            "first put all of the expected arguments in a new environment ",
            "called refArgs. Note that at the end of the function those ",
            "arguments will be removed. If they are still referenced in other ",
            "environments, they will not be garbage collected. ?new.env, ",
            "?assign, ?rm, ?get for more information.",
            "Error: ", mess)
        )
}

rgb2rgb <-function()
{
    #FIXME: We should check if environment is valid.
    if ( ! "RGB" %in% ls(envir=as.environment(refArgs)) )
        stop (passByRefMessage("The rgb2rgb method needs RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    rm("RGB", envir=as.environment(refArgs))
    return (RGB)
}

rgb2r <- function()
{
    #FIXME: We should check if environment is valid.
    if ( ! "RGB" %in% ls(envir=as.environment(refArgs)) )
        stop (passByRefMessage("The rgb2r method needs RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    rm("RGB", envir=as.environment(refArgs))
    return (RGB[,1])
}

rgb2g <- function()
{
    #FIXME: We should check if environment is valid.
    if ( ! "RGB" %in% ls(envir=as.environment(refArgs)) )
        stop (passByRefMessage("The rgb2g method needs RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    rm("RGB", envir=as.environment(refArgs))
    return (RGB[,2])
}

rgb2b <- function()
{
    #FIXME: We should check if environment is valid.
    if ( ! "RGB" %in% ls(envir=as.environment(refArgs)) )
        stop (passByRefMessage("The rgb2b method needs RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    rm("RGB", envir=as.environment(refArgs))
    return (RGB[,3])
}

# We base our calculations on opencv's equation.
# http://opencv.itseez.com/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
rgb2hsv <- function()
{
    #FIXME: We should check if environment is valid.
    if ( ! "RGB" %in% ls(envir=as.environment(refArgs)) )
        stop (passByRefMessage("The rgb2hsv method needs RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    V = apply(RGB, 1, max) # V is MAX
    maxMinDelta = V - apply(RGB, 1, min) # V - MIN

    S = maxMinDelta / V
    S[ is.infinite(S) ] = 0 # Inf is the result of dividing by 0
    S[ is.nan(S) ] = 0 # Nan is the result of dividing by 0

    Coef = (V == RGB) # Results in Nx3 boolean matrix

    # In R FALSE*num = 0 :)
    H = 60 * ( Coef[,1]*((RGB[,2]-RGB[,3])/maxMinDelta)
               + Coef[,2]*(((RGB[,3]-RGB[,1])/maxMinDelta)+2)
               + Coef[,3]*(((RGB[,1]-RGB[,2])/maxMinDelta)+4) )
    H = (361^(H<0) - 1) + H # add 360 to negative values.
    #FIXME : Make sure we clean NANs and INFs.

    rm(maxMinDelta, Coef) # Keep memory usage down.
    rm("RGB", envir=as.environment(refArgs))

    return (cbind(H,S,V))
}

colorSpaceFuns = c ( "rgb"=rgb2rgb, "r"=rgb2r, "g"=rgb2g, "b"=rgb2b )
