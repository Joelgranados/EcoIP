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
#import Image
#import ImageChops
#from ImageTk import *
#import pickle
from numpy import *
#import ColorConverter
from browse_files import *

#  ****************************************************************************************************
#  Pull stats from various csv files
#  ***************************************************************************************************


def main():

    ##################  Change these flags to modify the program #################

    write256 = True  #  output of color conversion should be scaled, each component, to 0-255

    RGB_bool = True  #  what color spaces to run
    RGBfract_bool = True
    RGBfractRGB_bool = True
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

    #  Open dialog to find the _probabilities subdirectory.

    rootDirName = fileBrowser('folder', 'Select directory where probabilies are kept...')
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return
    else:
        print
        print "'" + rootDirName + "'", 'selected.'
        print
    
    csvFileDirectory = rootDirName
        
    ###  make list of files that meets the descriptions ###

    csvFileList = []
    if RGBfractRGB_bool:
        csvFileList = csvFileList + ['R_256', 'G_256', 'B_256', 'GR_256', 'BR_256']
    elif RGB_bool:
        csvFileList = csvFileList + ['R_256', 'G_256', 'B_256']
    elif RGBfract_bool:
        csvFileList = csvFileList + ['GR_256', 'BR_256']
        
    if HSL_bool:
        csvFileList = csvFileList + ['HSL_H_256','HSL_S_256','HSL_L_256','HSL_HS_256']
    if Yxy_bool:
        csvFileList = csvFileList + ['Yxy_Y_256','Yxy_x_256','Yxy_y2_256','Yxy_xy_256']
    if NRGB_bool:
        csvFileList = csvFileList + ['NRGB_NR_256','NRGB_NG_256','NRGB_NB_256','NRGB1_256','NRGB2_256','NRGB12_256']
    if Lab_bool:
        csvFileList = csvFileList + ['Lab_L_256','Lab_a_256','Lab_b_256','Lab_ab_256']
    if Ingling_bool:
        csvFileList = csvFileList + ['Ingling_r_g_256','Ingling_b_y_256','Ingling_rgby_256','Ingling_V_256']
    if ExRGB_bool:
        csvFileList = csvFileList + ['ExRGB_R14_256','ExRGB_R20_256','ExRGB_G_256','ExRGB_B_256','ExRGB1_256','ExRGB2_256','ExRGB12_256']
    if ATD_bool:
        csvFileList = csvFileList + ['ATD_A1_256','ATD_T1_256','ATD_D1_256','ATD_TD1_256','ATD_t_256','ATD_d_256','ATD_td_256','ATD_A2_256','ATD_T2_256','ATD_D2_256','ATD_TD2_256']
    if NDI123_bool:
        csvFileList = csvFileList + ['NDI123_1_256','NDI123_2_256','NDI123_3_256','NDI123_NDI12_256','NDI123_NDI23_256','NDI123_NDI13_256']
    if i123_bool:
        csvFileList = csvFileList + ['i123_1_256','i123_2_256','i123_3_256','i123_i12_256','i123_i23_256','i123_i13_256']
    if CIVE_bool:
        csvFileList = csvFileList + ['CIVE_256']
    if shadow_bool:
        csvFileList = csvFileList + ['shadow_256']

    #columnList = ['Path and filename','number pixels examined','number of black pixels','foremask pixels','correct foreground','incorrect foreground','correct background','segments before clean','segments after clean','foreBigNans','QsegFore','QsegBack']
    #looksLike = 'Yxy_Y_256.csv_probabilities.csv_probability_segmented.csv'

    writerFile = open(csvFileDirectory + '\\summary.csv', 'wb')
    writer = csv.writer(writerFile, delimiter = ',')
    writer.writerow(['Color space','QsegFore','QsegForeStdv','QsegBack','QsegBackStdv'])
    
    for inFile in csvFileList:
        csvFile = csvFileDirectory + '\\' + inFile + '.csv_probability_segmented.csv'
        if os.path.isfile(csvFile):
            print 'Opening:', inFile
            readerFile = open(csvFile, 'rb')
            reader = csv.reader(readerFile, delimiter = ',')

            QsegForeAvg = array([])
            QsegBackAvg = array([])

            headerFlag = True
            for dataLine in reader:
                if headerFlag:
                    headerFlag = False
                else:
                    QsegForeAvg = append(QsegForeAvg, float(dataLine[10]))
                    QsegBackAvg = append(QsegBackAvg, float(dataLine[11]))

            readerFile.close()
            
            writer.writerow([inFile,mean(QsegForeAvg),std(QsegForeAvg),mean(QsegBackAvg),std(QsegBackAvg)])

    writerFile.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
