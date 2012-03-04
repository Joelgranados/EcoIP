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

source("common.R")

sigDigi = 10 #Significant digits.

# This transformation is based on http://www.poynton.com/ColorFAQ.html. This
# FIXME: function expects rgb values that are Rec.709. I'm unsure how to check
rgb2CIEXYZ <-function( env )
{
    isParamInEnv(c("img"), env)

    # Matrix defined by CIE
    XYZTrans = matrix(data=c(0.412453, 0.35758 , 0.180423,
                             0.212671, 0.71516 , 0.072169,
                             0.019334, 0.119193, 0.950227),
                      ncol=3, nrow=3, byrow=TRUE)

    #FIXME: the Z value might exceed 1. Not sure about this.
    # Transpose the trans matrix because I use column vectors
    env$img = env$img %*% t(XYZTrans)
    env$img = signif(env$img, sigDigi)
}

# Numbers in method defined in opencv's cvtColor function doc.
rgb2CIELUV <- function( env )
{
    isParamInEnv(c("img"), env)

    # Environment already setup.
    rgb2CIEXYZ( env )

    # Implements If(>0.0088){116Y^.33} else {Y*903.3}
    LCoef = (env$img[,2] > 0.008856)*1
    L = (LCoef * (env$img[,2]^(1/3)) * 116) + ((!LCoef) * env$img[,2] * 903.3)

    rm (LCoef) # Save memory.
    gc()

    # In R + precedes *.
    U = L * 13 * ( ( 4*env$img[,1]
                     / (env$img[,1] + 15*env$img[,2] + 3*env$img[,3]) )
                   - 0.19793943 )
    U[ is.nan(U) ] = 0

    V = L * 13 * ( ( 9*env$img[,2]
                     / (env$img[,1] + 15*env$img[,2] + 3*env$img[,3]) )
                   - 0.46831096 )
    V[ is.nan(V) ] = 0

    # From cvtColor doc: 0≤L≤100, −134≤u≤220, −140≤v≤122
    env$img = cbind(L,U,V)
    env$img = signif(env$img, sigDigi)
}

# Numbers in method defined in opencv's cvtColor function doc.
# FIXME: we still need to validate this.
rgb2CIELAB <- function( env )
{
    isParamInEnv(c("img"), env)

    # Environment already setup.
    rgb2CIEXYZ( env )

    env$img[,1] = env$img[,1]/0.950456
    env$img[,3] = env$img[,3]/1.088754

    # A 3*N matrix conditions. We use this to implement the 'if'
    XYZCoef = (env$img > 0.008856)

    # Implements If(>0.0088){116Y^.33} else {Y*903.3}
    L = (XYZCoef[,2] * (116 * (env$img[,2]^(1/3))-16))
        + ((!XYZCoef[,2]) * env$img[,2] * 903.3)

    # Implements 500(f(X) − f(Y))
    # f(x) = ((Coef(x)*(XYZ(x)^.33)) + (!Coef(x)*(7.787*XYZ(x)+0.137))
    A = 500 * ( ( (XYZCoef[,1] * (env$img[,1]^(1/3)))
                  + ((!XYZCoef[,1]) * (7.787*env$img[,1] + 0.137931)) )
                - ( (XYZCoef[,2] * (env$img[,2]^(1/3)))
                    + ((!XYZCoef[,2]) * (7.787*env$img[,2] + 0.137931)) ) )

    # Implements 200(f(Y) − f(Z))
    # f(x) = same as before.
    B = 200 * ( ( (XYZCoef[,2] * (env$img[,2]^(1/3)))
                  + ((!XYZCoef[,2]) * (7.787*env$img[,2] + 0.137931)) )
                - ( (XYZCoef[,3] * (env$img[,3]^(1/3)))
                    + ((!XYZCoef[,3]) * (7.787*env$img[,3] + 0.137931)) ) )

    rm(XYZCoef)
    gc()

    # From cvtColor doc: 0≤L≤100, −127≤a≤127, −127≤b≤127
    env$img = cbind(L,A,B)
    env$img = signif(env$img, sigDigi)
}

# FIXME: we still need to validate this.
rgb2yCbCr <-function( env )
{
    isParamInEnv(c("img"), env)

    # Matrix defined by CIE
    YCbCrTrans = matrix(data=c(65.481 , 128.553, 24.966,
                               -37.797, -74.203, 112,
                               112.   , -93.786, -18.214),
                        ncol=3, nrow=3, byrow=TRUE)

    # Transpose the trans matrix because I use column vectors
    env$img = env$img %*% t(YCbCrTrans)
    # Add [16,128,128]
    env$img[,1] = env$img[,1]+16
    env$img[,2:3] = env$img[,2:3]+128
    env$img = signif(env$img, sigDigi)
}

# We base our calculations on opencv's equation.
# http://opencv.itseez.com/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
rgb2hsv <- function( env )
{
    isParamInEnv(c("img"), env)

    if ( dim(env$img)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    V = apply(env$img, 1, max) # V is MAX
    maxMinDelta = V - apply(env$img, 1, min) # V - MIN

    S = maxMinDelta / V
    S[ is.infinite(S) ] = 0 # Inf might result from dividing by 0
    S[ is.nan(S) ] = 0 # Nan might result from dividing by 0

    Coef = (V == env$img) # Results in Nx3 boolean matrix

    # Calculate H in 4 steps and then clean it up.
    # If V == R, If V == G, If V == B, H*60. See cvtColor from opencv
    H = (Coef[,1])*((env$img[,2]-env$img[,3])/maxMinDelta)
    H = H + ((H==0)*(Coef[,2]))*(((env$img[,3]-env$img[,1])/maxMinDelta)+2)
    H = H + ((H==0)*(Coef[,3]))*(((env$img[,1]-env$img[,2])/maxMinDelta)+4)
    H = H*60

    # Clean H.
    H = H + (361^(H<0) - 1) # add 360 to negative values.
    H[ is.infinite(H) ] = 0
    H[ is.nan(H) ] = 0
    H[ is.na(H) ] = 0

    rm(maxMinDelta, Coef) # Keep memory usage down.
    gc()

    #FIXME: remember to erase H,S,V. also in other funcs.
    env$img = cbind(H,S,V)
    env$img = signif(env$img, sigDigi)
}

rgb2rgb <-function( env ) {}

colorSpaceFuns = c ( "rgb"=rgb2rgb, "-"=rgb2rgb, "hsv"=rgb2hsv,
                     "CIEXYZ"=rgb2CIEXYZ, "CIELUV"=rgb2CIELUV )
