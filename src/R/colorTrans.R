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

source("common.R")

# This transformation is based on http://www.poynton.com/ColorFAQ.html. This
# FIXME: function expects rgb values that are Rec.709. I'm unsure how to check
#FIXME: we still need to validate this.
rgb2CIEXYZ <-function()
{
    in.refArgs(c("RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    # Matrix defined by CIE
    XYZTrans = matrix(data=c(0.412453, 0.35758 , 0.180423,
                             0.212671, 0.71516 , 0.072169,
                             0.019334, 0.119193, 0.950227),
                      ncol=3, nrow=3, byrow=TRUE)

    #FIXME: the Z value might exceed 1. Not sure about this.
    # Transpose the trans matrix because I use column vectors
    XYZ = RGB %*% t(XYZTrans)
    rm("RGB", envir=as.environment(refArgs))

    return (XYZ)
}

#FIXME: we still need to validate this.
rgb2CIELUV <- function()
{
    in.refArgs(c("RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    # Environment already setup.
    XYZ = rgb2CIEXYZ()

    # Numbers defined in opencv's cvtColor function doc.
    LCoef = (XYZ[,2] > 0.008856)
    L = (LCoef * (XYZ[,2]^(1/3)) * 116) + (!LCoef * XYZ[,2] * 903.3)

    rm (LCoef) # Save memory.
    gc()

    # In R + precedes *.
    U = L * 13 * ( ( 4*XYZ[,1] / (XYZ[,1] + 15*XYZ[,2] + 3*XYZ[,3]) )
                   - 0.19793943 )

    V = L * 13 * ( ( 9*XYZ[,2] / (XYZ[,1] + 15*XYZ[,2] + 3*XYZ[,3]) )
                   - 0.46831096 )

    rm (XYZ)
    gc()

    # From cvtColor doc: 0≤L≤100, −134≤u≤220, −140≤v≤122
    return (cbind(L,U,V))
}

#FIXME: we still need to validate this.
rgb2yCbCr <-function()
{
    in.refArgs(c("RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    # Matrix defined by CIE
    YCbCrTrans = matrix(data=c(65.481 , 128.553, 24.966,
                               -37.797, -74.203, 112,
                               112.   , -93.786, -18.214),
                        ncol=3, nrow=3, byrow=TRUE)

    # Transpose the trans matrix because I use column vectors
    YCbCr = RGB %*% t(YCbCrTrans)
    # Add [16,128,128]
    YCbCr = YCbCr + matrix(data=c( rep(16,dim(YCbCr)[1]),
                                   rep(128,dim(YCbCr)[1]),
                                   rep(128,dim(YCbCr)[1]) ),
                           ncol=3, nrow=dim(YCbCr)[1] )

    rm("RGB", envir=as.environment(refArgs))

    return (YCbCr)
}

# We base our calculations on opencv's equation.
# http://opencv.itseez.com/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
rgb2hsv <- function()
{
    in.refArgs(c("RGB"))
    RGB = get("RGB", envir=as.environment(refArgs))

    if ( dim(RGB)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    V = apply(RGB, 1, max) # V is MAX
    maxMinDelta = V - apply(RGB, 1, min) # V - MIN

    S = maxMinDelta / V
    S[ is.infinite(S) ] = 0 # Inf might result from dividing by 0
    S[ is.nan(S) ] = 0 # Nan might result from dividing by 0

    Coef = (V == RGB) # Results in Nx3 boolean matrix

    # In R FALSE*num = 0 :)
    H = 60 * ( Coef[,1]*((RGB[,2]-RGB[,3])/maxMinDelta)
               + Coef[,2]*(((RGB[,3]-RGB[,1])/maxMinDelta)+2)
               + Coef[,3]*(((RGB[,1]-RGB[,2])/maxMinDelta)+4) )
    H = H + (361^(H<0) - 1) # add 360 to negative values.
    H[ is.infinite(H) ] = 0
    H[ is.nan(H) ] = 0
    H[ is.na(H) ] = 0

    rm(maxMinDelta, Coef) # Keep memory usage down.
    rm("RGB", envir=as.environment(refArgs))
    gc()

    return (cbind(H,S,V))
}

colorSpaceFuns = c ( "rgb"=rgb2rgb, "hsv"=rgb2hsv,
                     "CIEXYZ"=rgb2CIEXYZ, "CIELUV"=rgb2CIELUV )
