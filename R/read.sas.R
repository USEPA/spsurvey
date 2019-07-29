################################################################################
# Function: read.sas
# Programmer: Tom Kincaid
# Date: May 9, 2007
# Revised: March 9, 2010
# Revised: April 18, 2016
#'
#' Read a SAS dataset or a SAS XPORT File
#'
#' This function reads either a SAS dataset or a SAS XPORT (transport) file and
#' creates a data frame.
#'
#' @param filename If xport equals TRUE, a character string giving the full
#'   path to the SAS XPORT file, which must include the file extension.  If
#'   xport equals FALSE, either a character string giving the the name of a
#'   dataset in the SAS library or a vector of character strings giving the
#'   names of datasets in the SAS library, where the dataset names cannot exceed
#'   eight characters in length and do not include the file extension.
#'
#' @param libname Character string defining the SAS library, which is usually a
#'   directory reference.   If xport equals FALSE and the dataset(s) named in
#'   argument filename do not reside in the working directory, then this
#'   argument is required.  The default value is NULL.
#'
#' @param xport Logical value indicating whether the input file is a SAS XPORT
#'   file.  The default value is FALSE.
#'
#' @param sascmd Character string giving the full path to SAS executable.  This
#'   argument is required only when xport equals FALSE.  The default value is
#'   "C:/Program Files/SAS/SAS 9.1/sas.exe".
#'
#' @return Either a single data frame or a list of data frames.
#'
#' @section Other Functions Required:
#'   \describe{
#'     \item{\code{\link{read.ssd}}}{function in the foreign package that reads
#'       a SAS dataset and creates a data frame}
#'     \item{\code{\link{read.xport}}}{function in the foreign package that
#'       reads a SAS XPORT file and creates a data frame}
#'   }
#'
#' @author Tom Kincaid \email{Kincaid.Tom@epa.gov}
#'
#' @examples
#' \dontrun{
#' MySasFile <- read.sas("mysasfil", "C:/Documents and Settings/auser/My Project")
#' }
#'
#' @export
################################################################################

read.sas <- function(filename, libname = NULL, xport = FALSE,
   sascmd = "C:/Program Files/SASHome/SASFoundation/9.4/sas.exe") {

# Read the SAS XPORT file

   if(xport) {
      df <- read.xport(filename)

# Read the SAS dataset(s)

   } else if(is.null(libname)) {
      df <- read.ssd(getwd(), filename, sascmd=sascmd)

   } else {
      df <- read.ssd(libname, filename, sascmd=sascmd)
   }

# Return the data frame(s)

   return(df)
}
