#' Process and Aggregate Primary Energy Data
#'
#' A function to process and aggregate primary energy data from a GDX file.
#' @param path The file path to the GDX data file.
#' @param regions A character vector of region names to filter data.
#' @return A magpie object containing processed and aggregated primary energy data.
#'
#' @examples
#' # Example usage:
#' result <- reportPE(system.file("extdata", "blabla.gdx", package = "openprom"), c("MEA"))
#' @export
#source("R/readData.R")
reportPE <- function(path, regions) {
  vars = c("VProdPrimary", "BALEF2EFS")
  values <- readData(path, vars, field = 'l')

  VProdPrimary <- values$VProdPrimary[regions, , ]
  sets <- values$BALEF2EFS
  print(sets)
  names(sets) <- c("BAL", "EF")

  replacements <- c(
    "Gas fuels" = "Gases",
    "Solids" = "Coal",
    "Crude oil and Feedstocks" = "Oil",
    "Nuclear heat" = "Nuclear",
    "Solar energy" = "Solar",
    "Geothermal heat" = "Geothermal",
    "Steam" = "Heat"
  )
  sets$BAL <- Reduce(function(x, pattern) {
    gsub(pattern, replacements[pattern], x)
  }, names(replacements), init = sets$BAL)

    VProdPrimary <- madrat::toolAggregate(
    VProdPrimary[ , , unique(sets$EF)],
    dim = 3,
    rel = sets,
    from = "EF",
    to = "BAL"
  )

  # Update item names
  magclass::getItems(VProdPrimary, 3) <- paste0("Primary Energy|", magclass::getItems(VProdPrimary, 3))
  magclass::getNames(VProdPrimary)[magclass::getNames(VProdPrimary) == "Primary Energy|Total"] <- "Primary Energy"

  # Add dimensions and bind to magpie object
  VProdPrimary <- magclass::add_dimension(VProdPrimary, dim = 3.2, add = "unit", nm = "Mtoe")
  magpie_object <- magclass::mbind(NULL, VProdPrimary)
  return(magpie_object)
}
