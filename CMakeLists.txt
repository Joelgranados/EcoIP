# ILAC: Image labeling and Classifying
# Copyright (C) 2011 Joel Granados <joel.granados@gmail.com>
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

cmake_minimum_required (VERSION 2.8)

project (ecoacq)

# Set the version number
set (EA_NAME "\"Ecological Data Acquisition\"")

# Move files for the test
file(COPY "${PROJECT_SOURCE_DIR}/test" DESTINATION "${PROJECT_BINARY_DIR}")
file(GLOB SRC_FILES "${PROJECT_SOURCE_DIR}/src/R/*.R")
file(COPY ${SRC_FILES} DESTINATION "${PROJECT_BINARY_DIR}/test")

enable_testing()
add_test(NAME color
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_color )

add_test(NAME nbm
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_nbm )

add_test(NAME mask
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_mask )
