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

require(RUnit)
source("colorTrans.R")

initRGB <- function()
{
    return ( matrix(data=c( 1 ,0 ,0,
                            0 ,1 ,0,
                            0 ,0 ,1,
                            1 ,1 ,0,
                            0 ,1 ,1,
                            1 ,0 ,1,
                            1 ,1 ,1,
                            0 ,0 ,0,
                            .1,.5,.9,
                            .9,.1,.5,
                            .5,.9,.1 ),
                   ncol=3, nrow=11, byrow=TRUE) )
}

test.rgb2CIEXYZ <- function()
{
    ciexyz = matrix(data=c(0.412453, 0.212671, 0.019334,
                           0.35758 , 0.71516 , 0.119193,
                           0.180423, 0.072169, 0.950227,
                           0.770033, 0.927831, 0.138527,
                           0.538003, 0.787329, 1.069420,
                           0.592876, 0.284840, 0.969561,
                           0.950456, 1.000000, 1.088754,
                           0       , 0       ,0        ,
                           0.3824160, 0.4437992, 0.9167342,
                           0.4971772, 0.2990044, 0.5044334,
                           0.5460908, 0.7571964, 0.2119634),
                          ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$img = initRGB()
    rgb2CIEXYZ ( env )
    checkEquals( sum(env$img != ciexyz), 0 )
}

test.rgb2CIELUV <- function()
{
    cieluv = matrix(data=c( 69.24058794,    227.5206211000, 49.12556917000,
                            103.73509950,   -98.3629126300, 127.01928890000,
                            48.29567257,    -14.1268629500, -194.89302810000,
                            113.13950700,   8.8267004570,   124.41340680000,
                            107.11330140,   -82.9934952500, -17.82881688000,
                            76.32350653,    106.2752970000, -137.47699300000,
                            116.00000000,   -0.1508420296,  0.04711110498,
                            0.00000000,     0.00000000,     0.00000000,
                            88.48170735,    -47.9493140200, -69.36950373000,
                            77.56822418,    109.1333658000, -54.47388559000,
                            105.72901090,   -32.6393969800, 103.27078470000),
                    ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$img = initRGB()
    rgb2CIELUV ( env )
    checkEquals( sum(env$img != cieluv), 0 )
}

test.rgb2CIELAB <- function()
{
    cielab = matrix(data=c( 53.24058794, 80.09416683,  67.20153700,
                            87.73509949, -86.18125751, 83.17747707,
                            32.29567257, 79.18700180,  -107.86174730,
                            97.13950704, -21.55239244, 94.47578178,
                            91.11330144, -48.08856548, -14.13095553,
                            60.32350653, 98.23515144,  -60.82549225,
                            100,         0,            0,
                            0,           0,            0,
                            72.48170735, -12.26346271, -36.30296953,
                            61.56822418, 68.52247913,  -21.02024459,
                            89.72901090, -40.05951235,  66.37620398),
                    ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$img = initRGB()
    rgb2CIELAB ( env )
    checkEquals( sum(env$img != cielab), 0 )
}

test.rgb2hsv <- function()
{
    hsvpix = matrix(data=c(0,   1, 1,
                           120, 1, 1,
                           240, 1, 1,
                           60,  1, 1,
                           180, 1, 1,
                           300, 1, 1,
                           0,   0, 1,
                           0,   0, 0,
                           210, 0.8888888889, 0.9,
                           330, 0.8888888889, 0.9,
                           90,  0.8888888889, 0.9),
                    ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$img = initRGB()
    rgb2hsv ( env )
    checkEquals( sum(env$img != hsvpix), 0 )
}

test.YCbCr <- function()
{
    ycbcrpix = matrix(data=c(   81.481,     90.203,     240,
                                144.553,    53.797,     34.214,
                                40.966,     240,        109.786,
                                210.034,    16,         146.214,
                                169.519,    165.797,    16,
                                106.447,    202.203,    221.786,
                                235,        128,        128,
                                16,         128,        128,
                                109.294,    187.9188,   75.9144,
                                100.2712,   142.5624,   210.3144,
                                166.9348,   53.5188,    97.7712),
                    ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$img = initRGB()
    rgb2yCbCr ( env )
    checkEquals( sum(env$img != ycbcrpix), 0 )
}
