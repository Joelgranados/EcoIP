# -*- coding: cp1252 -*-
# Data Generator Scripts.
# Copyright (C) 2012 Eric Graham <egraham@cens.ucla.edu>
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

import math

def rgb_to_HSL(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH
#  requires inputs of rgb in range of 0 to 1

    maxV = max(r, g, b)
    minV = min(r, g, b)
    delMax = maxV - minV

    L2 = (maxV + minV) * 0.5  #  luminance

    if delMax == 0:  #  no chroma
        S2 = 0
        H2 = 0

    else:
        if (L2 < 0.5):  #  HSL stuff
            S2 = delMax / (maxV + minV)
        else:
            S2 = delMax / (2 - maxV - minV)

        del_R = (((maxV - r) * 0.166666666667) + (delMax * 0.5)) / delMax
        del_G = (((maxV - g) * 0.166666666667) + (delMax * 0.5)) / delMax
        del_B = (((maxV - b) * 0.166666666667) + (delMax * 0.5)) / delMax

        if (r == maxV):
            H2 = del_B - del_G
        elif (g == maxV):
            H2 = (0.3333333333) + del_R - del_B
        elif (b == maxV):
            H2 = (0.66666666667) + del_G - del_R

        if (H2 < 0): H2 = H2 + 1
        if (H2 > 1): H2 = H2 - 1

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max H = 0.999346405229, Max S = 1.0, Max L =1.0
        #  Min H = 0.0, Min S = 0.0, Min L = 0.0
        H2 = int(round(H2 * 255.0))
        S2 = int(round(S2 * 255.0))
        L2 = int(round(L2 * 255.0))

    return H2, S2, L2


def rgb_to_HSV(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH
#  requires inputs of rgb in range of 0 to 1

    maxV = max(r, g, b)
    minV = min(r, g, b)
    delMax = maxV - minV

    V = maxV

    if delMax == 0:  #  no chroma
        H2 = 0
        S = 0
    else:

        S = delMax / maxV

        del_R = (((maxV - r) * 0.166666666667) + (delMax * 0.5)) / delMax
        del_G = (((maxV - g) * 0.166666666667) + (delMax * 0.5)) / delMax
        del_B = (((maxV - b) * 0.166666666667) + (delMax * 0.5)) / delMax

        if (r == maxV):
            H2 = del_B - del_G
        elif (g == maxV):
            H2 = (0.3333333333) + del_R - del_B
        elif (b == maxV):
            H2 = (0.66666666667) + del_G - del_R

        if (H2 < 0): H2 = H2 + 1
        if (H2 > 1): H2 = H2 - 1

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max H = 0.999346405229, Max S = 1.0, Max V = 1.0
        #  Min H = 0.0, Min S = 0.0, Min V = 0.0
        H2 = int(round(H2 * 255.0))
        S = int(round(S * 255.0))
        V = int(round(V * 255.0))

    return H2, S, V


def rgb_to_NRGB(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH

    if ((r + g + b) <> 0):  #  make normalized RGB values
        NR = float(r) / (r + g + b)
        NG = float(g) / (r + g + b)
        NB = float(b) / (r + g + b)
    else:
        NR = 0
        NG = 0
        NB = 0

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max = 1, Min = 0
        NR = int(round(NR * 255.0))
        NG = int(round(NG * 255.0))
        NB = int(round(NB * 255.0))

    return NR, NG, NB


def rbg_to_NRGB_2D(r, g, b, c256 = False):  #  Normalized RGB is a 2D plane, based on PCA of 16 million color space.

    NR, NG, NB = rgb_to_NRGB(r, g, b, False)

    #  PCA on 16 M color cube values:
    #  Explained variance:  0.9999769
    #  see ExRGB

    XX = NR - 0.5*NG - 0.5*NB
    YY = NB - NG

    if c256:  #  if desired to convert to range 0 - 255
        #  Range XX = -1 to 1
        #  Range YY = -1 to 1
        #
        XX = int(round((XX + 1.0)/2.0 * 255))
        YY = int(round((YY + 1.0)/2.0 * 255))

    return XX, YY


def rgb_to_XYZ(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH
#  requires inputs of rgb in range of 0 to 1
    R = r
    G = g
    B = b

    if R > 0.04045:
        R = ((R + 0.055) / 1.055)**2.4
    else:
        R = R / 12.92
    if G > 0.04045:
        G = ((G + 0.055) / 1.055)**2.4
    else:
        G = G / 12.92
    if B > 0.04045:
        B = ((B + 0.055) / 1.055)**2.4
    else:
        B = B / 12.92

    R = R * 100.0
    G = G * 100.0
    B = B * 100.0

    X = R * 0.4124 + G * 0.3576 + B * 0.1805  #  Observer. = 2 degree, Illuminant = D65 (daylight)
    Y = R * 0.2126 + G * 0.7152 + B * 0.0722
    Z = R * 0.0193 + G * 0.1192 + B * 0.9505

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        # Max X = 95.05, Max Y = 100.0, Max Z = 108.9
        # Min X = 0.0, Min Y = 0.0, Min Z = 0.0
        X = int(round(X * 2.68279852709))
        Y = int(round(Y * 2.55))
        Z = int(round(Z * 2.34159779614))

    return X, Y, Z


def rgb_to_Yxy(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH
#  requires inputs of rgb in range of 0 to 1

    X, Y, Z = rgb_to_XYZ(r, g, b)

    if (X + Y + Z) <> 0:
        x = X / (X + Y + Z)
        y = Y / (X + Y + Z)
    else:
        x = 0
        y = 0

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max Y = 100.0, Max x = 0.640074499457, Max y = 0.6
        #  Min Y = 0.0, Min x =  0.0, Min y = 0.0
        #  discontinuity for x and y at zero (see Yxy.png)
        Y = int(round(Y * 2.55))
        if x <> 0:
            x = int(round((x-0.15001662234)/(0.640074499457 - 0.15001662234) * 255))
        if y <> 0:
            y = int(round((y-0.0600066489362)/(0.6 - 0.0600066489362) * 255))

    return Y, x, y


def rgb_to_Lab(r, g, b, c256 = False):  #  http://www.easyrgb.com/index.php?X=MATH
#  requires inputs of rgb in range of 0 to 1

    X, Y, Z = rgb_to_XYZ(r, g, b)

    varX = X * 0.0105211106  #  Observer= 2 degree, Illuminant= D65
    varY = Y * 0.01
    varZ = Z * 0.0091841701

    if varX > 0.008856:
        varX = varX**0.3333333333
    else:
        varX = (7.787 * varX) + 0.1379310344

    if varY > 0.008856:
        varY = varY**0.3333333333
    else:
        varY = (7.787 * varY) + 0.1379310344

    if varZ > 0.008856:
        varZ = varZ**0.3333333333
    else:
        varZ = (7.787 * varZ) + 0.1379310344

    L = (116.0 * varY) - 16
    a = 500.0 * (varX - varY)
    b2 = 200.0 * (varY - varZ)

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max L = 100.0, Max a = 98.2542185649, Max b = 94.4824856748
        #  Min L = -9.59999901795e-09, Min a = -86.1846365873, Min b = -107.863680592
        L = int(round(L * 2.55))
        a = int(round((a + 86.1846365873) * 1.38257201710))
        b2 = int(round((b2 + 107.863680592) * 1.26021661149))

    return L, a, b2


def rgb_to_ATD(r, g, b, c256 = False):  #  Guth 1995, ATD color space as described by: T. Jetsu et al. 2009.  Comparison of Color Vision Models Based on Spectral Color Representation. Color Research & Application Volume 34, Number 5, 341 - 350
                                        #  also: Further applications of the ATD model for color vision.  Guth, S. Lee.  Proc. SPIE Vol. 2414, p. 12-26, Device-Independent Color Imaging II, Eric Walowit; Ed.
#  requires inputs of rgb in range of 0 to 1

##    X, Y, Z = rgb_to_XYZ(r, g, b, False)
##
##    if (0.2435 * X + 0.8524 * Y - 0.0516 * Z) < 0:
##        L = 0
##    else:
##        L = (0.66*(0.2435 * X + 0.8524 * Y - 0.0516 * Z)) ** 0.7 + 0.024  #  ATD Long-wavelength cone response
##
##    if (-3.954 * X + 1.1642 * Y + 0.837 * Z) < 0:
##        M = 0
##    else:
##        M = (-3.954 * X + 1.1642 * Y + 0.837 * Z) ** 0.7 + 0.024 #  ATD Medium
##
##    if (0.04 * Y + 0.06225 * Z) < 0:
##        S = 0
##    else:
##        S = (0.43*(0.04 * Y + 0.06225 * Z)) ** 0.7 + 0.31  #  ATD Short
##
##    Lg = L * (300 / (300 + L))  #  gain control
##    Mg = M * (300 / (300 + M))
##    Sg = S * (300 / (300 + S))
##
##    A1 = 3.57 * Lg + 2.64 * Mg
##    T1 = 7.18 * Lg - 6.21 * Mg
##    D1 = -0.7 * Lg + 0.085 * Mg + Sg
##
##    A2 = 0.09 * A1
##    T2 = 0.43 * T1 + 0.76 * D1
##    D2 = D1
##
##    A1 = A1 / (200 + abs(A1))
##    T1 = T1 / (200 + abs(T1))
##    D1 = D1 / (200 + abs(D1))
##
##    A2 = A2 / (200 + abs(A2))
##    T2 = T2 / (200 + abs(T2))
##    D2 = D2 / (200 + abs(D2))

##    SmallColorDifference = (delta(A1)**2 + delta(T1)**2 + delta(D1)**2)**0.5
##    LargeColorDifference = (delta(A2)**2 + delta(T2)**2 + delta(D2)**2)**0.5

##    if D2 == 0:
##        Hue = 0
##    else:
##        Hue = int(round(T2 / D2))
##
##    if A2 == 0:
##        Chroma = 0
##    else:
##        Chroma = (T2**2 + D2**2)**0.5 / A2

#  Computationally streamlined via Granger 1995. Gamut mapping for hard copy using the ATD color space.  Proc. SPIE, Vol. 2414, 27 (1995); doi:10.1117/12.206550

    A1 = (r + 3*g) / 4
    T1 = r - g
    D1 = (r + g -2*b) / 2
    if (r + 1.5 * g + b) <> 0:
        t1 = (r - g) / (r + 1.5 * g + b)
    else:
        t1 = 0
    if (2*r + 3*g + 2*b) <> 0:
        d1 = (r + g -2*b) / (2*r + 3*g + 2*b)
    else:
        d1 = 0

#  Also, M. S. Mill´an and M. Corbal´an. 1995. Optical pattern recognition based on color vision models. OPTICS LETTERS / Vol. 20, No. 16 / August 15, 1995

    A2 = 0.5967*r + 0.3654*g
    T2 = 0.9553*r - 1.2836*g
    D2 = -0.0248*r + 0.0483*b

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max A1 = 1.0 Max T1 =  1.0 Max D1 = 1.0
        #  Min A1 = 0.0 Min T1 =  -1.0 Min D1 = -1.0
        #  Max t1 = 1.0 Max d1 =  0.5
        #  Min t1 = -0.666666666667 Min d1 =  -1.0
        #  Max A2 = 0.9621 Max T2 =  0.9553 Max D2 = 0.0483
        #  Min A2 = 0.0 Min T2 =  -1.2836 Min D2 = -0.0248

        A1 = int(round(A1 * 255.0))
        T1 = int(round((T1 + 1) * 127.5))
        D1 = int(round((D1 + 1) * 127.5))
        t1 = int(round((t1 + 0.666666666667) * 153))
        d1 = int(round((d1 + 1) * 170))
        A2 = int(round(A2 * 265.0452))
        T2 = int(round((T2 + 1.2836) * 113.8952164))
        D2 = int(round((D2 + 0.0248) * 3488.372093))

    return A1, T1, D1, t1, d1, A2, T2, D2


def rgb_to_Ingling(r, g, b, c256 = False):  #  Ingling CR, Jr. Tsou BHP. Orthogonal combination of the three visual channels. Vis Res 1977;17:1075–1082.
                                            #  Jetsu et al. 2008.  Comparison of Color Vision Models Based on Spectral Color Representation.  COLOR research and application Volume 34, Number 5, October 2009
    X, Y, Z = rgb_to_XYZ(r, g, b, False)

    if (0.2435 * X + 0.8524 * Y - 0.0516 * Z) < 0:
        L = 0
    else:
        L = (0.2435 * X + 0.8524 * Y - 0.0516 * Z)  #  Long-wavelength cone response

    if (-3.954 * X + 1.1642 * Y + 0.837 * Z) < 0:
        M = 0
    else:
        M = (-3.954 * X + 1.1642 * Y + 0.837 * Z)  #  Medium

    if (0.04 * Y + 0.06225 * Z) < 0:
        S = 0
    else:
        S = (0.04 * Y + 0.06225 * Z)  #  Short

    r_g = 1.2*L - 1.6*M + 0.4*S
    b_y = 0.24*L - 0.105*M - 0.7*S
    V = 0.6*L + 0.4 *M

    if c256:  #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max r_g = 127.630132 Max b_y =  20.10756913 Max V = 61.659261
        #  Min r_g = -17.2921058 Min b_y =  -4.731413 Min V = 0.0
        r_g = int(round((r_g + 17.2921058) * 1.759564328))
        b_y = int(round((b_y + 4.731413) * 10.26612116))
        V = int(round(V * 4.135631792))

    return r_g, b_y, V


def rgb_to_ExRGB(r, g, b, c256 = False):  #  Excess Red, Green, Blue
    ExR1 = 1.4*r - g - b
    ExR2 = 2*r - g - b
    ExG = 2*g - r - b
    ExB = 2*b - r - g

    if c256:
        #  Max X1 =,1.4,Max X2 = ,2.0,Max X3 =,2.0
        #  Min X1 =,-2.0,Min X2 = ,-2.0,Min X3 =,-2.0
        #  Tested with rgb color cube:
        ExR1 = int(round((ExR1 + 2) * 75.0))
        ExR2 = int(round((ExR2 + 2) * 63.75))
        ExG = int(round((ExG + 2) * 63.75))
        ExB = int(round((ExB + 2) * 63.75))

    return ExR1, ExR2, ExG, ExB


def rgb_to_ExRGB_2D(r, g, b, c256 = False):  #  ExRGB is a 2D plane, based on PCA of 16 million color space.
    #  using ExR = 2*r - g- b
    #  Comp 1  0.8164966*ExR - 0.4082483*ExG - 0.4082483*ExB
    #  Comp 2  -0.7071068*ExG + 0.7071068*ExB
    #  XX = (math.sqrt(2)/2) * (ExB - ExG)
    #  YY = (math.sqrt(2)/math.sqrt(3)) * (ExR - 0.5 * ExG -  0.5 * ExB)
    #  same as XX = B - G, YY = 2R - g - B, just scaled by 2 * sqrt(2) * sqrt(3)

    XX = b - g
    YY = 2*r - g - b

    if c256:  #  if desired to convert to range 0 - 255
        #  Range XX = -1 to 1
        #  Range YY = -2 to 2
        XX = int(round((XX + 1.0) * 127.5))
        YY = int(round((YY + 2.0) * 63.75))

    return XX, YY


def rgb_to_NDI123(r, g, b, c256 = False):  #  Perez et al 2000. Colour and shape analysis techniques for weed detection in cereal fields.  Computers and Electronics in Agriculture 25 (2000) 197–212
#  requires inputs of rgb in range of 0 to 1

    if (g + r) <> 0:
        N = (g - r)/float(g + r)
    else:
        N = 0

    if (g + b) <> 0:
        D = (g - b)/float(g + b)
    else:
        D = 0

    if (b + r) <> 0:
        I = (r - b)/float(r + b)
    else:
        I = 0

    if c256:  #  if desired to convert to range 0 - 255
    #  Max X1 =,1.0,Max X2 = ,1.0,Max X3 =,1.0
    #  Min X1 =,-1.0,Min X2 = ,-1.0,Min X3 =,-1.0
    #  Tested with rgb color cube:
        N = int(round((N + 1) * 127.5))
        D = int(round((D + 1) * 127.5))
        I = int(round((I + 1) * 127.5))

    return N, D, I


def rgb_to_CIVE(r, g, b, c256 = False):  #  PCA-based method.  Kataoka et al., 2003.  Crop Growth Estimation System Using Machine Vision.  Proceedings of the 2003 IEEE/ASME International Conference on Advanced Intelligent Mechatronics (AIM 2003)

    CIVE = 0.441*r - 0.811*g + 0.385*b + 18.78745

    if c256:
        #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  Max = 19.61345
        #  Min = 17.97645
        CIVE = int(round((CIVE - 17.97645) * 155.77275503971))

    return CIVE


def rgb_to_shadow(r, g, b, c256 = False):  #  Marchant JA, Onyango CM.  2002.  Spectral invariance under daylight illumination changes.  JOURNAL OF THE OPTICAL SOCIETY OF AMERICA A-OPTICS IMAGE SCIENCE AND VISION   Volume: 19   Issue: 5   Pages: 840-848
                                            # John A. Marchant and Christine M. Onyango 2000.  Shadow-invariant classification for scenes illuminated by daylight.  J. Opt. Soc. Am. A/ Vol. 17, No. 11. 1952 - 1961
#  requires inputs of rgb in range of 0 to 1

    a= 0.6666667  # T. Hague N. D. Tillett H. Wheeler.  2006.  Automated crop and weed monitoring in widely spaced cereals.  Precision Agric (2006) 7:21–32.  DOI 10.1007/s11119-005-6787-1

    if (r**a) * (b**(1-a)) == 0:
        shadow = 0
    else:
        shadow = g / ((r**a) * (b**(1-a)))

    if c256:
        #  if desired to convert to range 0 - 255
        #  Tested with rgb color cube:
        #  normalize using reciprical (see shadow.png)
        #  Max = 1
        #  Min = 0
        shadow = int(round(1/(shadow+1)*256.0)) - 1

    return shadow


def rgb_to_i1i2i3(r,g,b, c256 = False):  #  Isabelle Philipp, Thomas Rath.  2002.  Improving plant discrimination in image processing by use of different colour space transformations.  Computers and Electronics in Agriculture 35 (2002) 1–15.
#  requires inputs of rgb in range of 0 to 1

    i1 = r * 0.34 + g * 0.33 + b * 0.33
    i2 = r * 0.07 + g * 0.39 - b * 0.54
    i3 = -r * 0.35 + g * 0.51 - b * 0.14  #  channel to use for plant vs. soil

    if c256:  #  if desired to convert to range 0 - 255
    #  Explained variance:,0.854807223843
    #  Projection matrix: [[-0.04920657  0.73944976  0.67141103] [ 0.99143879 -0.04523945  0.12248474]]
    #  Max i1 = 1.0, Max i2 = 0.46, Max i3 = 0.51
    #  Min i1 = 0.0, Min i2 = -0.54, Min i3 = -0.49
    #  Tested with rgb color cube:
        i1 = int(round(i1 * 255.0))
        i2 = int(round((i2 + 0.54) * 255.0))
        i3 = int(round((i3 + 0.49) * 255.0))

    return i1, i2, i3
