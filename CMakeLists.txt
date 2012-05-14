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

CMAKE_MINIMUM_REQUIRED (VERSION 2.8)

PROJECT (EcoIP)

# Set the version number
SET (EIP_NAME "Ecological Image Processing")
SET (EIP_VER_MAJOR 0)
SET (EIP_VER_MINOR 1)
EXECUTE_PROCESS(COMMAND git show -s --format=%h
                COMMAND tr -d "\r\n"
                OUTPUT_VARIABLE EIP_VER_GIT)
EXECUTE_PROCESS(COMMAND git show -s --format=%ci
                COMMAND cut -c 1-10
                COMMAND tr -d "\r\n-"
                OUTPUT_VARIABLE EIP_VER_DATE)


CONFIGURE_FILE( "${PROJECT_SOURCE_DIR}/src/R/ecoip.R"
                "${PROJECT_BINARY_DIR}" )

INSTALL ( FILES "src/R/common.R" "src/R/colorTrans.R" "src/R/naiveBayes.R"
    "src/R/imageTrans.R" "src/R/ecoip" "COPYING" DESTINATION ".")
INSTALL ( FILES "${PROJECT_BINARY_DIR}/ecoip.R" DESTINATION "." )
INSTALL ( DIRECTORY "samples" DESTINATION "." PATTERN "*.Rdata" EXCLUDE )


# Testing stuff
FILE(COPY "${PROJECT_SOURCE_DIR}/test" DESTINATION "${PROJECT_BINARY_DIR}")
FILE(GLOB SRC_FILES "${PROJECT_SOURCE_DIR}/src/R/*.R")
FILE(COPY ${SRC_FILES} DESTINATION "${PROJECT_BINARY_DIR}/test")
CONFIGURE_FILE("${PROJECT_SOURCE_DIR}/src/R/ecoip.R"
               "${PROJECT_BINARY_DIR}/test/ecoip.R")

ENABLE_TESTING()
ADD_TEST(NAME color
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_color )

ADD_TEST(NAME nbm
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_nbm )

ADD_TEST(NAME mask
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_mask )

ADD_TEST(NAME cmd
    WORKING_DIRECTORY "${PROJECT_BINARY_DIR}/test"
    COMMAND ./run_test.R test_cmd )

# Packaging stuff
SET(CPACK_PACKAGE_NAME "EcoIP")
SET(CPACK_PACKAGE_VERSION_MAJOR ${EIP_VER_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${EIP_VER_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${EIP_VER_DATE})
SET(CPACK_GENERATOR "ZIP;TGZ")
INCLUDE(CPack)
