import os
import sys
import csv
import Image
import ImageChops
#from ImageTk import *
#import pickle
from numpy import *
import ColorConverter
from browse_files import *

#  ****************************************************************************************************
#  Turn images into various color spaces
#  Convert to range 0 - 255
#  Convert to derived color spaces (e.g. [Red + Green] / Blue) and then convert to 0 - 255
#  Accumulate in a master 2D histogram
#  Normalize the histogram and use Bayes rule for creating probabilities, per subdirectory
#  ***************************************************************************************************

def main():

    ##################  Change these flags to modify the program #################

    write256 = True  #  output of color conversion should be scaled, each component, to 0-255

    RGB = False  #  what color spaces to run
    HSL = False
    Yxy = False
    NRGB = False
    Lab = False
    ExRGB = False
    ATD = False
    NDI123 = False
    CIVE = False
    shadow = False
    i123 = False
    Ingling = True

    ##############################################################################

    outFileList = []
    outDictList = []
    if RGB:
        outFileList = outFileList + ['RG', 'RB', 'GB']
        RG = {}
        RB = {}
        GB = {}
        outDictList = outDictList + [RG, RB, GB]
    if HSL:
        outFileList = outFileList + ['HSL_H','HSL_S','HSL_L','HSL_HS']
        HSL_H = {}
        HSL_S = {}
        HSL_L = {}
        HSL_HS = {}
        #HSL_HSL = {}
        outDictList = outDictList + [HSL_H,HSL_S,HSL_L,HSL_HS]
    if Yxy:
        outFileList = outFileList + ['Yxy_Y','Yxy_x','Yxy_y','Yxy_xy']
        Yxy_Y = {}
        Yxy_x = {}
        Yxy_y = {}
        Yxy_xy = {}
        #Yxy_Yxy = {}
        outDictList = outDictList + [Yxy_Y,Yxy_x,Yxy_y,Yxy_xy]
    if NRGB:
        outFileList = outFileList + ['NRGB_NR','NRGB_NG','NRGB_NB','NRGB1','NRGB2','NRGB12']
        NRGB_NR = {}
        NRGB_NG = {}
        NRGB_NB = {}
        NRGB1 = {}
        NRGB2 = {}
        NRGB12 = {}
        outDictList = outDictList + [NRGB_NR,NRGB_NG,NRGB_NB,NRGB1,NRGB2,NRGB12]
    if Lab:
        outFileList = outFileList + ['Lab_L','Lab_a','Lab_b','Lab_ab']
        Lab_L = {}
        Lab_a = {}
        Lab_b = {}
        Lab_ab = {}
        #Lab_Lab = {}
        outDictList = outDictList + [Lab_L,Lab_a,Lab_b,Lab_ab]
    if Ingling:
        outFileList = outFileList + ['Ingling_r_g','Ingling_b_y','Ingling_rgby','Ingling_V']
        Ingling_r_g = {}
        Ingling_b_y = {}
        Ingling_rgby = {}
        Ingling_V = {}
        outDictList = outDictList + [Ingling_r_g,Ingling_b_y,Ingling_rgby,Ingling_V]
    if ExRGB:
        outFileList = outFileList + ['ExRGB_R14','ExRGB_R20','ExRGB_G','ExRGB_B','ExRGB1','ExRGB2','ExRGB12']
        ExRGB_R14 = {}
        ExRGB_R20 = {}
        ExRGB_G = {}
        ExRGB_B = {}
        ExRGB1 = {}
        ExRGB2 = {}
        ExRGB12 = {}
        outDictList = outDictList + [ExRGB_R14,ExRGB_R20,ExRGB_G,ExRGB_B,ExRGB1,ExRGB2,ExRGB12]
    if ATD:
        outFileList = outFileList + ['ATD_A1','ATD_T1','ATD_D1','ATD_TD1','ATD_t1','ATD_d1','ATD_td1','ATD_A2','ATD_T2','ATD_D2','ATD_TD2']
        ATD_A1 = {}
        ATD_T1 = {}
        ATD_D1 = {}
        ATD_TD1 = {}
        ATD_t1 = {}
        ATD_d1 = {}
        ATD_td1 = {}
        #ATD_ATD1 = {}
        ATD_A2 = {}
        ATD_T2 = {}
        ATD_D2 = {}
        ATD_TD2 = {}
        #ATD_ATD2 = {}
        outDictList = outDictList + [ATD_A1,ATD_T1,ATD_D1,ATD_TD1,ATD_t1,ATD_d1,ATD_td1,ATD_A2,ATD_T2,ATD_D2,ATD_TD2]
    if NDI123:
        outFileList = outFileList + ['NDI123_1','NDI123_2','NDI123_3','NDI123_NDI12','NDI123_ND23','NDI123_NDI13']
        NDI123_1 = {}
        NDI123_2 = {}
        NDI123_3 = {}
        NDI123_NDI12 = {}
        NDI123_NDI23 = {}
        NDI123_NDI13 = {}
        outDictList = outDictList + [NDI123_1,NDI123_2,NDI123_3,NDI123_NDI12,NDI123_NDI23,NDI123_NDI13]
    if i123:
        outFileList = outFileList + ['i123_1','i123_2','i123_3','i123_i12','i123_i23','i123_i13']
        i123_1 = {}
        i123_2 = {}
        i123_3 = {}
        i123_i12 = {}
        i123_i23 = {}
        i123_i13 = {}
        outDictList = outDictList + [i123_1,i123_2,i123_3,i123_i12,i123_i23,i123_i13]
    if CIVE:
        outFileList = outFileList + ['CIVE_dict']
        CIVE_dict = {}
        outDictList = outDictList + [CIVE_dict]
    if shadow:
        outFileList = outFileList + ['shadow_dict']
        shadow_dict = {}
        outDictList = outDictList + [shadow_dict]

    #  Open dialog to find the photo subdirectory.
    #  Assumed: \masks\ holds more folders.
    #  Each subfolder holds mask files that have the same name as each photo, but are bmp's.
    rootDirName = fileBrowser()
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return
    
    photoPath = rootDirName
    maskPath = rootDirName + '\\masks\\'
    if not os.path.exists(maskPath):
        print "No subdirectory called 'masks' found.  Program aborted."
        return

    maskDirs = [ name for name in os.listdir(maskPath) if os.path.isdir(os.path.join(maskPath, name)) ]

    if maskDirs == []:
        print "No subdirectories within \masks\ exists.  program aborted."
        return
    else:
        print "Success in finding subdirectories..."

    for maskDirLocation in maskDirs:
        print 'Working on', maskDirLocation
        maskFileList = os.listdir(maskPath + maskDirLocation)  #  get the mask file names in each subdirectory
        for maskFile in maskFileList:
            if os.path.splitext(maskPath + maskFile)[1] == '.bmp':  #  could be other types of files in the subdirectory
                imageFile = photoPath + '\\' + os.path.splitext(maskFile)[0] + '.jpg'
                print 'Processing: ', maskFile
                maskImage = Image.open(maskPath + maskDirLocation + '\\' + maskFile).convert("L")  #  open the mask image with foreground areas in white, background in black
                image = Image.open(imageFile)  #  open the image file, no mask
                image.load()
                image.paste(0, (0,0), ImageChops.invert(maskImage))  #  paste the image onto the jpg, need to invert, masking everything but what is wanted
                #image.show()
                        
                imageSource = image.split()  #  split image into RGB
                foreR = list(imageSource[0].getdata())  #  get pixel values for every pixel in image
                foreG = list(imageSource[1].getdata())
                foreB = list(imageSource[2].getdata())

                #  make blank lists to accumulate the pixel data

                for colorDictionary in outDictList:
                    colorDictionary = {}
                    
                count = 0

                #  convert RGBs to different color spaces with the output range 0 - 255 for each color component (see subroutines)
                for i in range(0, len(foreR)):  #  send pixel data to subroutines to calculate colors
                    if (foreR[i] > 0) and ((foreG[i] > 0) and (foreB[i] > 0)):  #  process only if not black, the mask
                        sys.stdout.write('')
                        r = float(foreR[i])/255.0
                        g = float(foreG[i])/255.0
                        b = float(foreB[i])/255.0
                        count += 1

                        # *********************  Accumulate  *********************************
                        if RGB:
                            RG[(foreR[i],foreG[i])] = RG.get((foreR[i],foreG[i]), 0) + 1
                            RB[(foreR[i],foreB[i])] = RB.get((foreR[i],foreB[i]), 0) + 1
                            GB[(foreG[i],foreB[i])] = GB.get((foreG[i],foreB[i]), 0) + 1
                        
                        if HSL:
                            H, S, L = ColorConverter.rgb_to_HSL(r, g, b, write256)
                            HSL_H[H] = HSL_H.get(H, 0) + 1
                            HSL_S[S] = HSL_S.get(S, 0) + 1
                            HSL_L[L] = HSL_L.get(L, 0) + 1
                            HSL_HS[(H,S)] = HSL_HS.get((H,S), 0) + 1
                            #HSL_HSL[(H,S,L)] = HSL_HSL.get((H,S,L), 0) + 1
                        
                        if Yxy:
                            Y, x, y = ColorConverter.rgb_to_Yxy(r, g, b, write256)
                            Yxy_Y[Y] = Yxy_Y.get(Y, 0) + 1
                            Yxy_x[x] = Yxy_x.get(x, 0) + 1
                            Yxy_y[y] = Yxy_y.get(y, 0) + 1
                            Yxy_xy[(x,y)] = Yxy_xy.get((x,y), 0) + 1
                            #Yxy_Yxy[(Y,x,y)] = Yxy_Yxy.get((Y,x,y), 0) + 1
                
                        if NRGB:
                            NR, NG, NB = ColorConverter.rgb_to_NRGB(r, g, b, write256)
                            NRGB_NR[NR] = NRGB_NR.get(NR, 0) + 1
                            NRGB_NG[NG] = NRGB_NG.get(NG, 0) + 1
                            NRGB_NB[NB] = NRGB_NB.get(NB, 0) + 1
                            NRGB_1, NRGB_2 = ColorConverter.rbg_to_NRGB_2D(r, g, b, write256)
                            NRGB1[NRGB_1] = NRGB1.get(NRGB_1, 0) + 1
                            NRGB2[NRGB_2] = NRGB2.get(NRGB_2, 0) + 1
                            NRGB12[(NRGB_1, NRGB_2)] = NRGB12.get((NRGB_1, NRGB_2), 0) + 1
                       
                        if Lab:
                            L2, a, b2 = ColorConverter.rgb_to_Lab(r, g, b, write256)
                            Lab_L[L2] = Lab_L.get(L2, 0) + 1
                            Lab_a[a] = Lab_a.get(a, 0) + 1
                            Lab_b[b2] = Lab_b.get(b2, 0) + 1
                            Lab_ab[(a,b2)] = Lab_ab.get((a,b2), 0) + 1
                            #Lab_Lab[(L,a,b2)] = Lab_Lab.get((La,b2), 0) + 1

                        if Ingling:
                            r_g, b_y, V3 = ColorConverter.rgb_to_Ingling(r, g, b, write256)
                            Ingling_r_g[r_g] = Ingling_r_g.get(r_g, 0) + 1
                            Ingling_b_y[b_y] = Ingling_b_y.get(b_y, 0) + 1
                            Ingling_rgby[(r_g,b_y)] = Ingling_rgby.get((r_g,b_y), 0) + 1
                            Ingling_V[V3] = Ingling_V.get(V3, 0) + 1                  
                        
                        if ExRGB:
                            ExR14, ExR20, ExG, ExB = ColorConverter.rgb_to_ExRGB(r, g, b, write256)
                            ExRGB_R14[ExR14] = ExRGB_R14.get(ExR14, 0) + 1
                            ExRGB_R20[ExR20] = ExRGB_R20.get(ExR20, 0) + 1
                            ExRGB_G[ExG] = ExRGB_G.get(ExG, 0) + 1
                            ExRGB_B[ExB] = ExRGB_B.get(ExB, 0) + 1
                            ExRGB_1, ExRGB_2 = ColorConverter.rgb_to_ExRGB_2D(r, g, b, write256)
                            ExRGB1[ExRGB_1] = ExRGB1.get(ExRGB_1, 0) + 1
                            ExRGB2[ExRGB_2] = ExRGB2.get(ExRGB_2, 0) + 1
                            ExRGB12[(ExRGB_1, ExRGB_2)] = ExRGB12.get((ExRGB_1, ExRGB_2), 0) + 1

                        if ATD:
                            A1, T1, D1, t1, d1, A2, T2, D2 = ColorConverter.rgb_to_ATD(r, g, b, write256)
                            ATD_A1[A1] = ATD_A1.get(A1, 0) + 1
                            ATD_T1[T1] = ATD_T1.get(T1, 0) + 1
                            ATD_D1[D1] = ATD_D1.get(D1, 0) + 1
                            ATD_TD1[(T1,D1)] = ATD_TD1.get((T1,D1), 0) + 1
                            ATD_t1[t1] = ATD_t1.get(t1, 0) + 1
                            ATD_d1[d1] = ATD_d1.get(d1, 0) + 1
                            ATD_td1[(t1,d1)] = ATD_td1.get((t1,d1), 0) + 1
                            #ATD_ATD1[(A1,T1,D1)] = ATD_ATD1.get((A1,T1,D1), 0) + 1
                            ATD_A2[A2] = ATD_A2.get(A2, 0) + 1
                            ATD_T2[T2] = ATD_T2.get(T2, 0) + 1
                            ATD_D2[D2] = ATD_D2.get(D2, 0) + 1
                            ATD_TD2[(T2,D2)] = ATD_TD2.get((T2,D2), 0) + 1
                            #ATD_ATD2[(A2,T2,D2)] = ATD_ATD2.get((A2,T2,D2), 0) + 1
                        
                        if NDI123:
                            NDI1, NDI2, NDI3 = ColorConverter.rgb_to_NDI123(r, g, b, write256)
                            NDI123_1[NDI1] = NDI123_1.get(NDI1, 0) + 1
                            NDI123_2[NDI2] = NDI123_2.get(NDI2, 0) + 1
                            NDI123_3[NDI3] = NDI123_3.get(NDI3, 0) + 1
                            NDI123_NDI12[(NDI1, NDI2)] = NDI123_NDI12.get((NDI1, NDI2),0) + 1
                            NDI123_NDI23[(NDI2, NDI3)] = NDI123_NDI23.get((NDI2, NDI3),0) + 1
                            NDI123_NDI13[(NDI1, NDI3)] = NDI123_NDI13.get((NDI1, NDI3),0) + 1
                            #NDI123_NDI123[(NDI1, NDI2, NDI3)] = NDI123_NDI123.get((NDI1, NDI2, NDI3), 0) + 1

                        if i123:
                            i1, i2, i3 = ColorConverter.rgb_to_i1i2i3(r, g, b, write256)
                            i123_1[i1] = i123_1.get(i1, 0) + 1
                            i123_2[i2] = i123_2.get(i2, 0) + 1
                            i123_3[i3] = i123_3.get(i3, 0) + 1
                            i123_i12[(i1, i2)] = i123_i12.get((i1, i2),0) + 1
                            i123_i23[(i2, i3)] = i123_i23.get((i2, i3),0) + 1
                            i123_i13[(i1, i3)] = i123_i13.get((i1, i3),0) + 1
                            #i123_i123[(i1, i2, i3)] = i123_i123.get((i1, i2, i3), 0) + 1

                        if CIVE:
                            CIVE_value = ColorConverter.rgb_to_CIVE(r, g, b, write256)
                            CIVE_dict[CIVE_value] = CIVE_dict.get(CIVE_value, 0) + 1
                        
                        if shadow:
                            shadow_value = ColorConverter.rgb_to_shadow(r, g, b, write256)
                            shadow_dict[shadow_value] = shadow_dict.get(shadow_value, 0) + 1

        #  ***********************************************************************************************
        #  write the output files
        #  ***********************************************************************************************
        
        count = float(count)

        print 'Writing output files'
        print

        for dictionary in range(0,len(outDictList)):
            if write256:
                outputFileName = maskPath + maskDirLocation + '\\' + str(maskDirLocation) + '_' + outFileList[dictionary] + '_256.csv'
            else:
                outputFileName = maskPath + maskDirLocation + '\\' + str(maskDirLocation) + '_' + outFileList[dictionary] + '_native.csv'
            writerFile = open(outputFileName, 'wb')  #  open the output file and prep
            writer = csv.writer(writerFile, delimiter = ',',quoting=csv.QUOTE_NONE)
            
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
