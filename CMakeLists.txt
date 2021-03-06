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
SET (EIP_AUTHOR "Joel Andres Granados <joel.granados@gmail.com>")
SET (EIP_DESCRIPTION "EcoIP detects phenological phases based on img series.")
SET (EIP_NAME "Ecological Image Processing")
SET (EIP_NAME_SHORT "EcoIP")
SET (EIP_LICENSE "GPL-3")
SET (EIP_VER_MAJOR 0)
SET (EIP_VER_MINOR 1)
EXECUTE_PROCESS(COMMAND git show -s --format=%h
                COMMAND tr -d "\r\n"
                OUTPUT_VARIABLE EIP_VER_GIT)
EXECUTE_PROCESS(COMMAND git show -s --format=%ci
                COMMAND cut -c 1-10
                COMMAND tr -d "\r\n-"
                OUTPUT_VARIABLE EIP_VER_DATE)
SET (EIP_VER_NUM ${EIP_VER_MAJOR}.${EIP_VER_MINOR}-${EIP_VER_DATE})
SET (EIP_RPKG "${EIP_NAME_SHORT}_${EIP_VER_NUM}.tar.gz" )


# Testing stuff
FILE(COPY "${PROJECT_SOURCE_DIR}/test" DESTINATION "${PROJECT_BINARY_DIR}")
FILE(GLOB SRC_FILES "${PROJECT_SOURCE_DIR}/src/R/*.R"
                    "${PROJECT_SOURCE_DIR}/src/R/ecoip"
                    "${PROJECT_SOURCE_DIR}/src/R/ecoip_plot")
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
CONFIGURE_FILE( "${PROJECT_SOURCE_DIR}/src/R/ecoip.R"
                "${PROJECT_BINARY_DIR}" )
INSTALL ( FILES  "src/R/common.R" "src/R/colorTrans.R"
    "src/R/naiveBayes.R" "src/R/imageTrans.R" "src/R/ecoip"
    "src/R/ecoip_plot"
    "src/R/ecoip_consolidate" "COPYING"
    DESTINATION "." )
INSTALL ( FILES "${PROJECT_BINARY_DIR}/ecoip.R" DESTINATION "." )
INSTALL ( DIRECTORY "samples" DESTINATION "." PATTERN "*.Rdata" EXCLUDE )

SET(CPACK_PACKAGE_NAME ${EIP_NAME_SHORT})
SET(CPACK_PACKAGE_VERSION_MAJOR ${EIP_VER_MAJOR})
SET(CPACK_PACKAGE_VERSION_MINOR ${EIP_VER_MINOR})
SET(CPACK_PACKAGE_VERSION_PATCH ${EIP_VER_DATE})
SET(CPACK_GENERATOR "ZIP;TGZ")
INCLUDE(CPack)

# R Packaging stuff
FILE ( MAKE_DIRECTORY "${PROJECT_BINARY_DIR}/RPackage/EcoIP/man"
                      "${PROJECT_BINARY_DIR}/RPackage/EcoIP/R"
                      "${PROJECT_BINARY_DIR}/RPackage/EcoIP/inst/extdata")

FILE ( WRITE "${PROJECT_BINARY_DIR}/RPackage/EcoIP/DESCRIPTION"
    "Package: ${EIP_NAME_SHORT}\n"
    "Type: Package\n"
    "Title: ${EIP_NAME}\n"
    "Version: ${EIP_VER_NUM}\n"
    "Date: ${EIP_VER_DATE}\n"
    "Author: ${EIP_AUTHOR}\n"
    "Maintainer: ${EIP_AUTHOR}\n"
    "Description: ${EIP_DESCRIPTION}\n"
    "License: ${EIP_LICENSE}\n"
    "Depends: EBImage,digest,fields,RSVGTipsDevice\n" )

FILE ( WRITE "${PROJECT_BINARY_DIR}/RPackage/EcoIP/NAMESPACE"
    "export(eip.genOutput)\n"
    "export(eip.nbm)\n"
    "export(eip.histcmp)\n"
    "export(eip.showModel)\n"
    "export(eip.plot)\n"
    "export(eip.version)\n"
    "export(eip.genMiss)\n"
    "export(eip.sigmoid)\n"
    "export(eip.smooth)\n"
    "export(eip.get_50_dates)\n")

FOREACH ( EIP_DOC_NAME "EcoIP-package.Rd" "eip.plot.Rd"
                       "eip.genOutput.Rd" "eip.histcmp.Rd"
                       "eip.nbm.Rd" "eip.showModel.Rd"
                       "eip.smooth.Rd" "eip.version.Rd"
                       "eip.sigmoid.Rd" "eip.genMiss.Rd"
                       "eip.get_50_dates.Rd")
    CONFIGURE_FILE ( "${PROJECT_SOURCE_DIR}/docs/man/${EIP_DOC_NAME}"
        "${PROJECT_BINARY_DIR}/RPackage/EcoIP/man/${EIP_DOC_NAME}" )
ENDFOREACH ( EIP_DOC_NAME )

CONFIGURE_FILE("${PROJECT_SOURCE_DIR}/src/R/ecoip.R"
    "${PROJECT_BINARY_DIR}/RPackage/EcoIP/R/ecoip.R")
CONFIGURE_FILE("${PROJECT_SOURCE_DIR}/src/R/ecoip_install.R"
    "${PROJECT_BINARY_DIR}/ecoip_install.R")

FILE ( COPY "${PROJECT_SOURCE_DIR}/src/R"
       DESTINATION "${PROJECT_BINARY_DIR}/RPackage/EcoIP"
       PATTERN "*ecoip.R" EXCLUDE
       PATTERN "*ecoip_consolidate" EXCLUDE
       PATTERN "*ecoip" EXCLUDE
       PATTERN "*ecoip_install.R" EXCLUDE
       PATTERN "*ecoip_plot" EXCLUDE )

FILE ( COPY "${PROJECT_SOURCE_DIR}/samples/images"
       DESTINATION "${PROJECT_BINARY_DIR}/RPackage/EcoIP/inst/extdata"
       PATTERN "*.txt" EXCLUDE
       PATTERN "*.svg" EXCLUDE
       PATTERN "*.Rdata" EXCLUDE )

ADD_CUSTOM_TARGET(rpackage R CMD build "${PROJECT_BINARY_DIR}/RPackage/EcoIP"
    COMMAND R CMD check "${PROJECT_BINARY_DIR}/${EIP_RPKG}" )
