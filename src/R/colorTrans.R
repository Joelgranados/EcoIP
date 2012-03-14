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

if ( !exists ("isParamInEnv") )
    source("common.R")

sigDigi = 10 #Significant digits.

# This transformation is based on http://www.poynton.com/ColorFAQ.html.
# Note: Z sometimes exceedes 1. We adjust in get.CIEXYZBins
# FIXME: function expects rgb values that are Rec.709. I'm unsure how to check
rgb2CIEXYZ <-function( env )
{
    isParamInEnv(c("data"), env)

    # Matrix defined by CIE
    XYZTrans = matrix(data=c(0.412453, 0.35758 , 0.180423,
                             0.212671, 0.71516 , 0.072169,
                             0.019334, 0.119193, 0.950227),
                      ncol=3, nrow=3, byrow=TRUE)

    # Transpose the trans matrix because I use column vectors
    env$data = env$data %*% t(XYZTrans)
    env$data = signif(env$data, sigDigi)
}
get.CIEXYZBins <- function( nbins )
{
    # -1 & 2 to account for Z
    s = seq(0,1,1/nbins)
    s[0] = -1
    s[length(s)] = 2
    return ( cbind(s,s,s) )
}

# Numbers in method defined in opencv's cvtColor function doc.
rgb2CIELUV <- function( env )
{
    isParamInEnv(c("data"), env)

    # Environment already setup.
    rgb2CIEXYZ( env )

    # Implements If(>0.0088){116Y^.33 - 16} else {Y*903.3}
    LCoef = (env$data[,2] > 0.008856)*1
    L = (LCoef * (116 * env$data[,2]^(1/3)) - 16)
        + ((!LCoef) * env$data[,2] * 903.3)

    rm (LCoef); gc() # Save memory.

    # In R + precedes *.
    U = L * 13 * ( ( 4*env$data[,1]
                     / (env$data[,1] + 15*env$data[,2] + 3*env$data[,3]) )
                   - 0.19793943 )
    U[ is.nan(U) ] = 0

    V = L * 13 * ( ( 9*env$data[,2]
                     / (env$data[,1] + 15*env$data[,2] + 3*env$data[,3]) )
                   - 0.46831096 )
    V[ is.nan(V) ] = 0

    # From cvtColor doc: 0≤L≤100, −134≤u≤220, −140≤v≤122
    env$data = cbind(L,U,V)
    env$data = signif(env$data, sigDigi)
}
get.CIELUVBins <- function( nbins )
{
    # From cvtColor doc: 0≤L≤100, −134≤u≤220, −140≤v≤122
    return ( cbind( seq(0,100,100/nbins),
                    seq(-134,220,354/nbins),
                    seq(-140,122,262/nbins) ) )
}

# Numbers in method defined in opencv's cvtColor function doc.
rgb2CIELAB <- function( env )
{
    isParamInEnv(c("data"), env)

    # Environment already setup.
    rgb2CIEXYZ( env )

    env$data[,1] = env$data[,1]/0.950456
    env$data[,3] = env$data[,3]/1.088754

    # A 3*N matrix conditions. We use this to implement the 'if'
    XYZCoef = (env$data > 0.008856)

    # Implements If(>0.0088){116Y^.33} else {Y*903.3}
    L = (XYZCoef[,2] * (116 * (env$data[,2]^(1/3))-16))
        + ((!XYZCoef[,2]) * env$data[,2] * 903.3)

    # Implements 500(f(X) − f(Y))
    # f(x) = ((Coef(x)*(XYZ(x)^.33)) + (!Coef(x)*(7.787*XYZ(x)+0.137))
    A = 500 * ( ( (XYZCoef[,1] * (env$data[,1]^(1/3)))
                  + ((!XYZCoef[,1]) * (7.787*env$data[,1] + 0.137931)) )
                - ( (XYZCoef[,2] * (env$data[,2]^(1/3)))
                    + ((!XYZCoef[,2]) * (7.787*env$data[,2] + 0.137931)) ) )

    # Implements 200(f(Y) − f(Z))
    # f(x) = same as before.
    B = 200 * ( ( (XYZCoef[,2] * (env$data[,2]^(1/3)))
                  + ((!XYZCoef[,2]) * (7.787*env$data[,2] + 0.137931)) )
                - ( (XYZCoef[,3] * (env$data[,3]^(1/3)))
                    + ((!XYZCoef[,3]) * (7.787*env$data[,3] + 0.137931)) ) )

    rm(XYZCoef); gc()

    # From cvtColor doc: 0≤L≤100, −127≤a≤127, −127≤b≤127
    env$data = cbind(L,A,B)
    env$data = signif(env$data, sigDigi)
}
get.CIELABBins <- function( nbins )
{
    # From cvtColor doc: 0≤L≤100, −127≤a≤127, −127≤b≤127
    return ( cbind( seq(0,100,100/nbins),
                    seq(-127,127,254/nbins),
                    seq(-127,127,254/nbins) ) )
}

rgb2yCbCr <-function( env )
{
    isParamInEnv(c("data"), env)

    # Matrix defined by CIE
    YCbCrTrans = matrix(data=c(65.481 , 128.553, 24.966,
                               -37.797, -74.203, 112,
                               112    , -93.786, -18.214),
                        ncol=3, nrow=3, byrow=TRUE)

    # Transpose the trans matrix because I use column vectors
    env$data = env$data %*% t(YCbCrTrans)
    # Add [16,128,128]
    env$data[,1] = env$data[,1]+16
    env$data[,2:3] = env$data[,2:3]+128
    env$data = signif(env$data, sigDigi)
}
get.YCbCrBins <-function( nbins )
{
    # Go to http://www.poynton.com/notes/colour_and_gamma/ColorFAQ.html#RTFToC29
    return ( cbind(seq(16,235,219/nbins), seq(16,240,224/nbins), seq(16,240,224/nbins)) )
}

# We base our calculations on opencv's equation.
# http://opencv.itseez.com/modules/imgproc/doc/miscellaneous_transformations.html#cvtcolor
rgb2hsv <- function( env )
{
    isParamInEnv(c("data"), env)

    if ( dim(env$data)[2] != 3 )
        stop ("The rgb var must have 3 dimensions")

    V = apply(env$data, 1, max) # V is MAX
    maxMinDelta = V - apply(env$data, 1, min) # V - MIN

    S = maxMinDelta / V
    S[ is.infinite(S) ] = 0 # Inf might result from dividing by 0
    S[ is.nan(S) ] = 0 # Nan might result from dividing by 0

    Coef = (V == env$data) # Results in Nx3 boolean matrix

    # Calculate H in 4 steps and then clean it up.
    # If V == R, If V == G, If V == B, H*60. See cvtColor from opencv
    H = (Coef[,1])*((env$data[,2]-env$data[,3])/maxMinDelta)
    H = H + ((H==0)*(Coef[,2]))*(((env$data[,3]-env$data[,1])/maxMinDelta)+2)
    H = H + ((H==0)*(Coef[,3]))*(((env$data[,1]-env$data[,2])/maxMinDelta)+4)
    H = H*60

    # Clean H.
    H = H + (361^(H<0) - 1) # add 360 to negative values.
    H[ is.infinite(H) ] = 0
    H[ is.nan(H) ] = 0
    H[ is.na(H) ] = 0

    rm(maxMinDelta, Coef); gc() # Keep memory usage down.

    #FIXME: remember to erase H,S,V. also in other funcs.
    env$data = cbind(H,S,V)
    env$data = signif(env$data, sigDigi)
}
get.HSVBins <-function( nbins )
{
    return ( cbind(seq(0,360,360/nbins), seq(0,1,1/nbins), seq(0,1,1/nbins)) )
}

rgb2rgb <-function( env ) {}
get.RGBBins <-function( nbins )
{
    return ( cbind(seq(0,1,1/nbins), seq(0,1,1/nbins), seq(0,1,1/nbins)) )
}

colorSpaceFuns = c ( "rgb"=rgb2rgb, "-"=rgb2rgb, "hsv"=rgb2hsv,
                     "CIEXYZ"=rgb2CIEXYZ, "CIELUV"=rgb2CIELUV,
                     "CIELAB"=rgb2CIELAB, "yCbCr"=rgb2yCbCr )
binGetFuns = c ( "rgb"=get.RGBBins, "-"=get.RGBBins, "hsv"=get.HSVBins,
                     "CIEXYZ"=get.CIEXYZBins, "CIELUV"=get.CIELUVBins,
                     "CIELAB"=get.CIELABBins, "yCbCr"=get.YCbCrBins )
