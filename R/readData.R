#' Read Data File
#'
#' A generic function to read data files.
#' Methods are provided for `.gdx` and `.imf` file types.
#'
#' @param .path The file path to the data file (string).
#' @param ... Additional arguments passed to methods.
#' @return An object of class "magpie", or NULL if an error occurs.
#' @examples
#' data <- readData("inst/extdata/blabla.gdx", "iSet")
#' @importFrom gdx readGDX

#' @export
readData <- function(.path, ...) {
  UseMethod("readData")
}

#' @export
readData.gdx <- function(.path, ...) {
  tryCatch(
    {
      data <- readGDX(.path, ...)
      return (data)
    },
    error = function(e) {
      message("Error reading GDX file: ", .path)
      message(e$message)
      return(NULL)
    }
  )
}

#' @export
readData.imf <- function(.path, ...) { # PLACEHOLDER
  return (TRUE)
}

# Helpers ---------------------------------------------------
#' @export
readData.character <- function(.path, ...) {
  # Handles string file input by delegating
  # to the appropriate reader method based on file type.

  extension <- tools::file_ext(.path)
  switch(
    extension,
    gdx = readData.gdx(.path, ...),
    imf = readData.imf(.path, ...),
    stop("Unsupported file type: ",
         extension,
         ". Supported types are 'gdx' and 'imf'.")
  )
}
#' @export
readData.list <- function(.path, ...) { # PLACEHOLDER
  return (TRUE)
}
