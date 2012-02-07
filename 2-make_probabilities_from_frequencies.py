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
from numpy import *
from scipy import *
from scipy import ndimage
import math
import ColorConverter
from browse_files import *

def main():

    # This program will take all the csv files in one directory and divide
    # them by their equivalent in another, matching up the color value in
    # the first column, keeping the frequencyas counts of the first file,
    # and dividing the frequencies listed in the third column of the first
    # file by the sum of those and the frequencies of the second.  Thus,
    # if the second file  has no matching value, then the probability is
    # 1.0 and if they are equal, the probability is 0.5.

    # *****************************************************************
    # Open dialog to find the first and second subdirectories.
    # *****************************************************************

    rootDirName = getMainDir(message='Select the foreground directory: ')
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return

    firstPath = rootDirName + '/'

    rootDirName = getMainDir(message='Select the background directory: ')
    if rootDirName == '':
        print
        print "No directory selected, program aborted."
        print
        return

    secondPath = rootDirName + '/'

    # *****************************************************************
    # Load the 1_d and 2-D color spaces probabilities into a dictionary
    # with the color space tuples (XX, YY) as keys
    # *****************************************************************

    # get the mask file names in each subdirectory
    firstFileList = [name for name in os.listdir(firstPath) \
            if name.endswith(".csv") ]
    # get the mask file names in each subdirectory
    secondFileList = os.listdir(secondPath)

    # for each color space file...
    for csvFile in firstFileList:
        firstFileListName = (firstPath + csvFile)
        secondFileListName = (secondPath + csvFile)
        print 'File name:', csvFile
        readerFirstFile = open(firstFileListName, 'rb')

        #  open the first data file
        firstReader = csv.reader(readerFirstFile, delimiter = ',')

        # load up the first list with color value and frequency
        firstDictionary = {}
        firstCountDict = {}

        # skip header row
        firstReader.next()

        # File format is: Xcol, Ycol (if it exists), Count, Frequency
        for dataItem in firstReader:
            if len(dataItem) == 3:
                tmpkey = float(dataItem[0])
                # count of pixels
                firstCountDict[tmpkey] = int(float(dataItem[1]))
                #  frequency of pixels
                firstDictionary[tmpkey] = float(dataItem[2])

            elif len(dataItem) == 4:
                tmpkey = (float(dataItem[0]),float(dataItem[1]))
                firstCountDict[tmpkey] = float(dataItem[2])
                firstDictionary[tmpkey] = float(dataItem[3])

            elif len(dataItem) == 5:
                tmpkey = ( float(dataItem[0]),
                           float(dataItem[1]),
                           float(dataItem[2]) )
                firstCountDict[tmpkey] = float(dataItem[3])
                firstDictionary[tmpkey] = float(dataItem[4])

        readerFirstFile.close()

        # over-write the original file!!!
        writerFirstFile = open(firstPath + csvFile, 'wb')
        writer = csv.writer(writerFirstFile, delimiter = ',',quoting=csv.QUOTE_NONE)

        readerSecondFile = open(secondFileListName, 'rb')
        # open the second data file
        secondReader = csv.reader(readerSecondFile, delimiter = ',')

        headerFlag = 1
        secondDirName = os.path.split(secondPath)[0]
        secondDirName = secondDirName.split('/')[len(secondDirName.split('/'))-2]

        # Loop through the second file and match it with the items in
        # the first file, creating a probability [0:1]
        for secondDataItem in secondReader:
            if headerFlag == 1:
                if len(secondDataItem) == 3:
                    newList = [ secondDataItem[0],
                                'Frequency',
                                'Probs_vs_' + secondDirName,
                                'Count' ]
                elif len(secondDataItem) == 4:
                    newList = [ secondDataItem[0],
                                secondDataItem[1],
                                'Frequency',
                                'Probs_vs_' + secondDirName,
                                'Count' ]
                elif len(secondDataItem) == 5:
                    newList = [ secondDataItem[0],
                                secondDataItem[1],
                                secondDataItem[2],
                                'Frequency',
                                'Probs_vs_' + secondDirName,
                                'Count' ]
                writer.writerow(newList)
                headerFlag = 0
            else:
                # if the color space is a single vector
                if len(secondDataItem) == 3:
                    index = float(secondDataItem[0])
                    # If the background item is in the first dictionary,
                    # then make the probability
                    if index in firstDictionary:
                        newList = [index]  #  write the color vector value

                        # write the frequency of the value
                        # (number of pixels / total count)
                        newList.append(firstDictionary[index])

                        # write frequency/frequency of background for that color
                        newList.append( firstDictionary[index]
                                        / ( float(secondDataItem[2])
                                            + firstDictionary[index] ) )

                        # write original count
                        newList.append(firstCountDict[index])
                        writer.writerow(newList)
                        # remove the item from the first dictionary
                        # to keep track of things
                        del firstDictionary[index]
                    else:
                        # if the item is not in the first dictionary,
                        # then write the probability of zero
                        writer.writerow([index, float(secondDataItem[2]), 0.0, 0])

                # if the color space is a double
                elif len(dataItem) == 4:
                    index = (float(secondDataItem[0]), float(secondDataItem[1]))
                    if index in firstDictionary:
                        #  write the color vector values
                        newList = [index[0], index[1]]
                        newList.append(firstDictionary[index])
                        newList.append( firstDictionary[index]
                                        / ( float(secondDataItem[3])
                                            + firstDictionary[index] ) )
                        newList.append(firstCountDict[index])
                        writer.writerow(newList)
                        del firstDictionary[index]

                    else:
                        writer.writerow([float(secondDataItem[0]),
                                         float(secondDataItem[1]),
                                         float(secondDataItem[3]),
                                         0.0,
                                         0])

                # if the color space is a triple
                elif len(dataItem) == 5:
                    index = ( float(secondDataItem[0]),
                              float(secondDataItem[1]),
                              float(secondDataItem[2]) )
                    if index in firstDictionary:
                        # write the color vector values
                        newList = [index[0], index[1], index[2]]
                        newList.append(firstDictionary[index])
                        newList.append(firstDictionary[index]
                                       / ( float(secondDataItem[4])
                                           + firstDictionary[index] ) )
                        newList.append(firstCountDict[index])
                        writer.writerow(newList)
                        del firstDictionary[index]
                    else:
                        writer.writerow([float(secondDataItem[0]),
                                         float(secondDataItem[1]),
                                         float(secondDataItem[2]),
                                         float(secondDataItem[3]),
                                         0.0,
                                         0])

        # for the remaining items that were in the first
        # list but did not occur in the second, write them
        # to the file wiht a probablity of 1
        for items in firstDictionary.iteritems():
            newList = []
            # if the first item in the dictionary is a tuple
            # (it is "iterable"), then start with that tuple
            if hasattr(items[0], '__iter__'):
                for ii in items[0]:
                    newList.append(ii)
            else:
                # otherwise, just take the first item
                newList.append(items[0])
            newList.append(items[1])  # append the frequency
            newList.append(1.0)  #  append '1'
            # append the count of the foreground
            newList.append(firstCountDict[items[0]])
            writer.writerow(newList)

        writerFirstFile.close()
        readerSecondFile.close()

if __name__ == "__main__":
    main()
    x = raw_input('Done!  Enter any key to quit.')
