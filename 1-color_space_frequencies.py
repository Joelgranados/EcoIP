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

import os
import sys
import csv
import Image
import ImageChops
#from ImageTk import *
#import pickle
from numpy import *
import ColorConverter

#  ****************************************************************************************************
#  Turn images into various color spaces
#  Convert to range 0 - 255
#  Convert to derived color spaces (e.g. [Red + Green] / Blue) and then convert to 0 - 255
#  Accumulate in a master 2D histogram
#  Normalize the histogram and use Bayes rule for creating probabilities, per subdirectory
#  ***************************************************************************************************

def getMainDir():
    if sys.platform == "win32":
        from browse_files import *
        return fileBrowser("folder', 'Select directory for photos...")
    elif sys.platform == "linux2":
        picdir = raw_input("Please type the name of the main picture folder: ")
        return os.path.realpath(picdir)


def main():

    ##################  Change these flags to modify the program #################

    write256 = True  #  output of color conversion should be scaled, each component, to 0-255

    R_G_B_bool = False  #  what color spaces to run
    RGBfract_bool = False
    RGB3D_bool = False
    HSL_bool = True
    Yxy_bool = True
    NRGB_bool = True
    Lab_bool = True
    ExRGB_bool = True
    ATD_bool = True
    NDI123_bool = True
    CIVE_bool = True
    shadow_bool = True
    i123_bool = True
    Ingling_bool = True

    ##############################################################################

    #  Open dialog to find the photo subdirectory.
    #  Assumed: \masks\ holds more folders.
    #  Each subfolder holds mask files that have the same name as each photo, but are bmp's.
    rootDirName = getMainDir()
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return
    photoPath = rootDirName
    maskPath = os.path.join ( rootDirName, "masks" )
    if not os.path.exists(maskPath):
        print "No subdirectory called 'masks' found.  Program aborted."
        return

    maskDirs = [ name for name in os.listdir(maskPath) \
            if os.path.isdir(os.path.join(maskPath, name)) ]

    if maskDirs == []:
        print "No subdirectories within \masks\ exists.  program aborted."
        return
    else:
        print "Success in finding subdirectories..."

    ###  Start main loop ###

    for maskDirLocation in maskDirs:
        print 'Working on', maskDirLocation
        count = 0.0

        #  get the mask file names in each subdirectory
        maskFileList = os.listdir( os.path.join(maskPath, maskDirLocation) )

        outFileList = []
        outDictList = []

        if RGB3D_bool:
            outFileList = outFileList + ['RGB3D']
            RGB3D = {}
            outDictList = outDictList + [RGB3D]
        if R_G_B_bool:
            outFileList = outFileList + ['R', 'G', 'B']
            R = {}
            G = {}
            B = {}
            outDictList = outDictList + [R, G, B]
        if RGBfract_bool:
            outFileList = outFileList + ['GR', 'BR', 'GRBR']
            GR = {}
            BR = {}
            GRBR = {}
            outDictList = outDictList + [GR, BR, GRBR]
        if HSL_bool:
            outFileList = outFileList + ['HSL_H','HSL_S','HSL_L','HSL_HS']
            HSL_H = {}
            HSL_S = {}
            HSL_L = {}
            HSL_HS = {}
            #HSL_HSL = {}
            outDictList = outDictList + [HSL_H,HSL_S,HSL_L,HSL_HS]
        if Yxy_bool:
            outFileList = outFileList + ['Yxy_Y','Yxy_x','Yxy_y2','Yxy_xy']
            Yxy_Y = {}
            Yxy_x = {}
            Yxy_y2 = {}
            Yxy_xy = {}
            #Yxy_Yxy = {}
            outDictList = outDictList + [Yxy_Y,Yxy_x,Yxy_y2,Yxy_xy]
        if NRGB_bool:
            outFileList = outFileList + ['NRGB_NR','NRGB_NG','NRGB_NB',
                                         'NRGB1','NRGB2','NRGB12']
            NRGB_NR = {}
            NRGB_NG = {}
            NRGB_NB = {}
            NRGB1 = {}
            NRGB2 = {}
            NRGB12 = {}
            outDictList = outDictList + [NRGB_NR,NRGB_NG,NRGB_NB,
                                         NRGB1,NRGB2,NRGB12]
        if Lab_bool:
            outFileList = outFileList + ['Lab_L','Lab_a','Lab_b','Lab_ab']
            Lab_L = {}
            Lab_a = {}
            Lab_b = {}
            Lab_ab = {}
            #Lab_Lab = {}
            outDictList = outDictList + [Lab_L,Lab_a,Lab_b,Lab_ab]
        if Ingling_bool:
            outFileList = outFileList + ['Ingling_r_g','Ingling_b_y',
                                         'Ingling_rgby','Ingling_V']
            Ingling_r_g = {}
            Ingling_b_y = {}
            Ingling_rgby = {}
            Ingling_V = {}
            outDictList = outDictList + [Ingling_r_g, Ingling_b_y,
                                         Ingling_rgby, Ingling_V]
        if ExRGB_bool:
            outFileList = outFileList + ['ExRGB_R14','ExRGB_R20',
                                         'ExRGB_G','ExRGB_B',
                                         'ExRGB1','ExRGB2','ExRGB12']
            ExRGB_R14 = {}
            ExRGB_R20 = {}
            ExRGB_G = {}
            ExRGB_B = {}
            ExRGB1 = {}
            ExRGB2 = {}
            ExRGB12 = {}
            outDictList = outDictList + [ExRGB_R14,ExRGB_R20,
                                         ExRGB_G,ExRGB_B,ExRGB1,
                                         ExRGB2,ExRGB12]
        if ATD_bool:
            outFileList = outFileList + ['ATD_A1', 'ATD_T1', 'ATD_D1',
                                         'ATD_TD1', 'ATD_t', 'ATD_d',
                                         'ATD_td', 'ATD_A2', 'ATD_T2',
                                         'ATD_D2', 'ATD_TD2']
            ATD_A1 = {}
            ATD_T1 = {}
            ATD_D1 = {}
            ATD_TD1 = {}
            ATD_t = {}
            ATD_d = {}
            ATD_td = {}
            #ATD_ATD1 = {}
            ATD_A2 = {}
            ATD_T2 = {}
            ATD_D2 = {}
            ATD_TD2 = {}
            #ATD_ATD2 = {}
            outDictList = outDictList + [ATD_A1, ATD_T1, ATD_D1,
                                         ATD_TD1, ATD_t, ATD_d,
                                         ATD_td, ATD_A2, ATD_T2,
                                         ATD_D2,ATD_TD2]
        if NDI123_bool:
            outFileList = outFileList + ['NDI123_1', 'NDI123_2', 'NDI123_3',
                                         'NDI123_NDI12', 'NDI123_NDI23',
                                         'NDI123_NDI13']
            NDI123_1 = {}
            NDI123_2 = {}
            NDI123_3 = {}
            NDI123_NDI12 = {}
            NDI123_NDI23 = {}
            NDI123_NDI13 = {}
            outDictList = outDictList + [NDI123_1, NDI123_2, NDI123_3,
                                    NDI123_NDI12, NDI123_NDI23, NDI123_NDI13]
        if i123_bool:
            outFileList = outFileList + ['i123_1', 'i123_2', 'i123_3',
                                         'i123_i12', 'i123_i23', 'i123_i13']
            i123_1 = {}
            i123_2 = {}
            i123_3 = {}
            i123_i12 = {}
            i123_i23 = {}
            i123_i13 = {}
            outDictList = outDictList + [i123_1, i123_2, i123_3,
                                         i123_i12, i123_i23, i123_i13]
        if CIVE_bool:
            outFileList = outFileList + ['CIVE']
            CIVE_val = {}
            outDictList = outDictList + [CIVE_val]
        if shadow_bool:
            outFileList = outFileList + ['shadow']
            shadow_val = {}
            outDictList = outDictList + [shadow_val]

        for maskFile in maskFileList:
            # could be other types of files in the subdirectory
            if os.path.splitext( os.path.join(maskPath, maskFile))[1] == '.bmp':
                maskPath = os.path.join ( maskPath, maskDirLocation, maskFile )
                imageFile = \
                        os.path.join( photoPath,
                                      os.path.splitext(maskFile)[0] + '.jpg' )
                print 'Mask: ', maskPath
                print 'Image:', imageFile
                print

                #  open the mask image with foreground areas in white, background in black
                maskImage = Image.open(maskPath).convert("L")
                image = Image.open(imageFile)  #  open the image file, no mask
                image.load()
                #  paste the image onto the jpg, need to invert, masking everything but what is wanted
                image.paste(0, (0,0), ImageChops.invert(maskImage))
                #image.save(maskPath + maskDirLocation + '\\' + maskFile + '_masked.jpg')

                imageSource = image.split()  #  split image into RGB
                foreR = list(imageSource[0].getdata())  #  get pixel values for every pixel in image
                foreG = list(imageSource[1].getdata())
                foreB = list(imageSource[2].getdata())

                #  convert RGBs to different color spaces with the output range 0 - 255
                # for each color component (see subroutines)
                #  send pixel data to subroutines to calculate colors
                for i in range(0, len(foreR)):
                    #  process only if not black, the mask
                    if not ((foreR[i] == 0) and ((foreG[i] == 0) and (foreB[i] == 0))):
                        sys.stdout.write('')
                        r = float(foreR[i])/255.0
                        g = float(foreG[i])/255.0
                        b = float(foreB[i])/255.0
                        count = count + 1.0

                        # *********************  Accumulate  *********************************
##                        if RGB_bool:
##                            RG[(foreR[i],foreG[i])] = RG.get((foreR[i],foreG[i]), 0) + 1
##                            RB[(foreR[i],foreB[i])] = RB.get((foreR[i],foreB[i]), 0) + 1
##                            GB[(foreG[i],foreB[i])] = GB.get((foreG[i],foreB[i]), 0) + 1
                        if R_G_B_bool:
                            R[foreR[i]] = R.get(foreR[i], 0) + 1
                            G[foreG[i]] = G.get(foreG[i], 0) + 1
                            B[foreB[i]] = B.get(foreB[i], 0) + 1

                        if RGBfract_bool:
                            if foreR[i] <> 0:
                                GR[float(foreG[i])/foreR[i]] = GR.get(float(foreG[i])/foreR[i], 0) + 1
                                BR[float(foreB[i])/foreR[i]] = BR.get(float(foreB[i])/foreR[i], 0) + 1
                                GRBR[(float(foreG[i])/foreR[i], float(foreB[i])/foreR[i])] = GRBR.get((float(foreG[i])/foreR[i], float(foreB[i])/foreR[i]), 0) + 1
                            else:
                                GR[0.0] = GR.get(0.0, 0) + 1
                                BR[0.0] = BR.get(0.0, 0) + 1
                                GRBR[(0.0, 0.0)] = GRBR.get((0.0, 0.0), 0) + 1

                        if RGB3D_bool:
                            RGB3D[(foreR[i],foreG[i],foreB[i])] = RGB3D.get((foreR[i],foreG[i],foreB[i]), 0) + 1

                        if HSL_bool:
                            H, S, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
                            HSL_H[H] = HSL_H.get(H, 0) + 1
                            HSL_S[S] = HSL_S.get(S, 0) + 1
                            HSL_L[L] = HSL_L.get(L, 0) + 1
                            HSL_HS[(H,S)] = HSL_HS.get((H,S), 0) + 1

                        if Yxy_bool:
                            Y, x, y2 = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                            Yxy_Y[Y] = Yxy_Y.get(Y, 0) + 1
                            Yxy_x[x] = Yxy_x.get(x, 0) + 1
                            Yxy_y2[y2] = Yxy_y2.get(y2, 0) + 1
                            Yxy_xy[(x,y2)] = Yxy_xy.get((x,y2), 0) + 1

                        if NRGB_bool:
                            NR, NG, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
                            NRGB_NR[NR] = NRGB_NR.get(NR, 0) + 1
                            NRGB_NG[NG] = NRGB_NG.get(NG, 0) + 1
                            NRGB_NB[NB] = NRGB_NB.get(NB, 0) + 1
                            NRGB_1, NRGB_2 = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                            NRGB1[NRGB_1] = NRGB1.get(NRGB_1, 0) + 1
                            NRGB2[NRGB_2] = NRGB2.get(NRGB_2, 0) + 1
                            NRGB12[(NRGB_1, NRGB_2)] = NRGB12.get((NRGB_1, NRGB_2), 0) + 1

                        if Lab_bool:
                            L2, a, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
                            Lab_L[L2] = Lab_L.get(L2, 0) + 1
                            Lab_a[a] = Lab_a.get(a, 0) + 1
                            Lab_b[b2] = Lab_b.get(b2, 0) + 1
                            Lab_ab[(a,b2)] = Lab_ab.get((a,b2), 0) + 1

                        if Ingling_bool:
                            r_g, b_y, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                            Ingling_r_g[r_g] = Ingling_r_g.get(r_g, 0) + 1
                            Ingling_b_y[b_y] = Ingling_b_y.get(b_y, 0) + 1
                            Ingling_rgby[(r_g,b_y)] = Ingling_rgby.get((r_g,b_y), 0) + 1
                            Ingling_V[V3] = Ingling_V.get(V3, 0) + 1

                        if ExRGB_bool:
                            ExR14, ExR20, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                            ExRGB_R14[ExR14] = ExRGB_R14.get(ExR14, 0) + 1
                            ExRGB_R20[ExR20] = ExRGB_R20.get(ExR20, 0) + 1
                            ExRGB_G[ExG] = ExRGB_G.get(ExG, 0) + 1
                            ExRGB_B[ExB] = ExRGB_B.get(ExB, 0) + 1
                            ExRGB_1, ExRGB_2 = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
                            ExRGB1[ExRGB_1] = ExRGB1.get(ExRGB_1, 0) + 1
                            ExRGB2[ExRGB_2] = ExRGB2.get(ExRGB_2, 0) + 1
                            ExRGB12[(ExRGB_1, ExRGB_2)] = ExRGB12.get((ExRGB_1, ExRGB_2), 0) + 1

                        if ATD_bool:
                            A1, T1, D1, t1, d1, A2, T2, D2 = ColorConverter.rgb_to_ATD(r, g, b, write256)
                            ATD_A1[A1] = ATD_A1.get(A1, 0) + 1
                            ATD_T1[T1] = ATD_T1.get(T1, 0) + 1
                            ATD_D1[D1] = ATD_D1.get(D1, 0) + 1
                            ATD_TD1[(T1,D1)] = ATD_TD1.get((T1,D1), 0) + 1
                            ATD_t[t1] = ATD_t.get(t1, 0) + 1
                            ATD_d[d1] = ATD_d.get(d1, 0) + 1
                            ATD_td[(t1,d1)] = ATD_td.get((t1,d1), 0) + 1
                            ATD_A2[A2] = ATD_A2.get(A2, 0) + 1
                            ATD_T2[T2] = ATD_T2.get(T2, 0) + 1
                            ATD_D2[D2] = ATD_D2.get(D2, 0) + 1
                            ATD_TD2[(T2,D2)] = ATD_TD2.get((T2,D2), 0) + 1

                        if NDI123_bool:
                            NDI1, NDI2, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                            NDI123_1[NDI1] = NDI123_1.get(NDI1, 0) + 1
                            NDI123_2[NDI2] = NDI123_2.get(NDI2, 0) + 1
                            NDI123_3[NDI3] = NDI123_3.get(NDI3, 0) + 1
                            NDI123_NDI12[(NDI1, NDI2)] = NDI123_NDI12.get((NDI1, NDI2),0) + 1
                            NDI123_NDI23[(NDI2, NDI3)] = NDI123_NDI23.get((NDI2, NDI3),0) + 1
                            NDI123_NDI13[(NDI1, NDI3)] = NDI123_NDI13.get((NDI1, NDI3),0) + 1

                        if i123_bool:
                            i1, i2, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                            i123_1[i1] = i123_1.get(i1, 0) + 1
                            i123_2[i2] = i123_2.get(i2, 0) + 1
                            i123_3[i3] = i123_3.get(i3, 0) + 1
                            i123_i12[(i1, i2)] = i123_i12.get((i1, i2),0) + 1
                            i123_i23[(i2, i3)] = i123_i23.get((i2, i3),0) + 1
                            i123_i13[(i1, i3)] = i123_i13.get((i1, i3),0) + 1

                        if CIVE_bool:
                            CIVE_value = ColorConverter.rgb_to_CIVE(r, g, b, write256)
                            CIVE_val[CIVE_value] = CIVE_val.get(CIVE_value, 0) + 1

                        if shadow_bool:
                            shadow_value = ColorConverter.rgb_to_shadow(r, g, b, write256)
                            shadow_val[shadow_value] = shadow_val.get(shadow_value, 0) + 1

        #  ***********************************************************************************************
        #  write the output files
        #  ***********************************************************************************************

        print 'Writing output files'
        print

        for dictionary in range(0,len(outDictList)):
            if write256:
                outputFileName = maskPath + maskDirLocation + '\\' + outFileList[dictionary] + '_256.csv'
            else:
                outputFileName = maskPath + maskDirLocation + '\\' + outFileList[dictionary] + '_native.csv'

            writerFile = open(outputFileName, 'wb')  #  open the output file and prep
            writer = csv.writer(writerFile, delimiter = ',',quoting=csv.QUOTE_NONE)

            newList = []
            for i in outDictList[dictionary].iteritems():  #  write header to file
                newList = []
                itemCount = 1
                if hasattr(i[0], '__iter__'):
                    for ii in i[0]:
                        newList.append('X' + str(itemCount))
                        itemCount = itemCount + 1
                else:
                    newList.append(outFileList[dictionary])
            newList = newList + ['count', 'frequency']
            writer.writerow(newList)

            for i in outDictList[dictionary].iteritems():  #  write items to file
                newList = []
                if hasattr(i[0], '__iter__'):
                    for ii in i[0]:
                        newList.append(ii)
                else:
                    newList.append(i[0])
                newList.append(i[1])
                newList.append(i[1]/count)
                writer.writerow(newList)
            writerFile.close()

    print 'Done!'
    image = []

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
