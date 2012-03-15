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
    env$data = initRGB()
    rgb2CIEXYZ ( env )
    checkEquals( sum(env$data != ciexyz), 0 )
}

test.rgb2CIELUV <- function()
{
    cieluv = matrix(data=c( 53.24058794, 174.9455341000,   37.77371428000,
                            87.73509949, -83.1915134600,  107.42795840000,
                            32.29567257,  -9.4467374790, -130.32640580000,
                            97.13950704,   7.5784432300,  106.81907070000,
                            91.11330144, -70.5963801800,  -15.16564558000,
                            60.32350653,  83.9963841000, -108.65714460000,
                            100.00000000,  -0.1300362324,    0.04061302153,
                            -16.00000000,   0.0000000000,    0.00000000000,
                            72.48170735, -39.2787193000,  -56.82553173000,
                            61.56822418,  86.6224230500,  -43.23755553000,
                            89.72901090, -27.7000681500,   87.64278872000),
                    ncol=3, nrow=11, byrow=TRUE)
    env = new.env(parent=emptyenv())
    env$data = initRGB()
    rgb2CIELUV ( env )
    checkEquals( sum(env$data != cieluv), 0 )
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
    env$data = initRGB()
    rgb2CIELAB ( env )
    checkEquals( sum(env$data != cielab), 0 )
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
    env$data = initRGB()
    rgb2hsv ( env )
    checkEquals( sum(env$data != hsvpix), 0 )
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
    env$data = initRGB()
    rgb2yCbCr ( env )
    checkEquals( sum(env$data != ycbcrpix), 0 )
}

if ( exists("RUN_TEST") && RUN_TEST == T )
{
    rm(RUN_TEST)
    res = runTestFile('test_color.R')
    if (res$test_color$nFail != 0 || res$test_color$nErr != 0)
        q(sa='no',st=1)
    else
        q(sa='no',st=0)
}
