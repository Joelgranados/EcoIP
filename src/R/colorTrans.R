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

rgb2rgb <-function( rgb )
{
    if ( dim(rgb)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    return (rgb)
}

rgb2r <- function( rgb )
{
    if ( dim(rgb)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    return (rgb[,1])
}

rgb2g <- function( rgb )
{
    if ( dim(rgb)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    return (rgb[,2])
}

rgb2b <- function( rgb )
{
    if ( dim(rgb)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    return (rgb[,3])
}

# We assume dim(rgb)=c(N,3). rgb[1]->R, rgb[2]->g, rgb[3]->b
# We base our calculations on opencv's equation.
# http://opencv.itseez.com/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
rgb2hsv <- function( rgb )
{
    maxRGB = apply(rgb, 1, max)
    minRGB = apply(rgb, 1, min) #1 is for function apply on row.
    maxMinDelta = maxRGB-minRGB

    V = maxRGB

    S = maxMinDelta / maxRGB
    S[ is.infinite(S) ] = 0 # Inf is the result of dividing by 0

    Coef = (maxRGB == rgb) # Results in Nx3 boolean matrix

    H = 60 * ( Coef[,1]*((rgb[2]-rgb[3])/maxMinDelta)
               + Coef[,2]*(((rgb[3]-rgb[1])+2)/maxMinDelta)
               + Coef[,3]*(((rgb[1]-rgb[2])+4)/maxMinDelta) )

    return (cbind(H,S,V))
}

colorSpaceFuns = c ( "rgb"=rgb2rgb, "r"=rgb2r, "g"=rgb2g, "b"=rgb2b )
