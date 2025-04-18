#' Generate a Plot from a MAgPIE Object
#'
#' This function generates a plot for a MAgPIE object using specified settings. It supports both bar and area plot types and handles color mappings for variables.
#'
#' @param magpie_obj A MAgPIE object containing the data to be plotted.
#' @param plot_type A string specifying the type of plot. Options are `"bar"` or `"area"`. Defaults to `"bar"`.
#' @param label An optional string to label the plot legend. Defaults to `NULL`.
#' @param save_name An optional string specifying the file name to save the plot. If `NULL`, the plot is not saved. Defaults to `NULL`.
#'
#' @return A ggplot object representing the plot.
#' @details
#' - The function extracts variable names from the `magpie_obj` and matches them with color mappings.
#' - A warning is issued if any variables do not have corresponding color mappings.
#' - If `save_name` is provided, the plot is saved to the specified file in high resolution.
#'
#' @examples
#' \dontrun{
#' plotReport(magpie_obj = example_magpie, plot_type = "bar", save_name = "example_plot.png")
#' }
#' @importFrom ggplot2 ggplot aes geom_area geom_bar facet_wrap scale_fill_manual
#' @importFrom ggplot2 scale_color_manual labs theme_bw theme ggsave element_text
#' @importFrom rlang enquo as_label
#' @importFrom dplyr filter arrange %>%
#' @importFrom quitte as.quitte
#' @importFrom magclass getSets
#' @export
plotReport <- function(magpie_obj, plot_type = "bar",
                       label = NULL, save_name = NULL) {
  variable_name <- getSets(magpie_obj)["d3.1"]
  vars <- getNames(magpie_obj, 3.1)[[variable_name]]

  colors_vars <- getColorMappings() %>%
    filter(X %in% vars)

  # Throw warning if missing color mappings are found
  missing_colors <- setdiff(vars, colors_vars$X)
  if (length(missing_colors) > 0) {
    warning(sprintf(
      "No color available for the following variables: %s",
      paste(missing_colors, collapse = ", ")
    )
    )
  }

  data <- as.quitte(magpie_obj)
  plot <- plotTool(data, colors_vars, variable_name, plot_type, label)

  if (!is.null(save_name)) {
    print(paste0("Saving plot to ", save_name))
    ggsave(save_name, plot, units = "in", width = 5.5, height = 4, dpi = 1200)
  }
  return(plot)
}
# Helpers -------------------------------------------------------------
plotTool <- function(data, colors_vars, variable, plot_type, label) {
  if (is.null(label)) {
    label <- rlang::as_label(rlang::enquo(variable))
  }

  plot <- ggplot(data, aes(y = value, x = period, color = .data[[variable]])) +
    scale_fill_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    scale_color_manual(
      values = as.character(colors_vars[, 3]),
      limits = as.character(colors_vars[, 1])
    ) +
    switch(plot_type,
      "area" = geom_area(stat = "identity", aes(fill = .data[[variable]])),
      "bar" = geom_bar(stat = "identity", aes(fill = .data[[variable]])),
      stop("Invalid plot_type.")
    ) +
    facet_wrap("region", scales = "free_y") +
    labs(
      x = "period",
      y = paste0("Capacity|Electricity", " ", unique(data[["unit"]])),
      color = label,
      fill = label
    ) +
    theme_bw() +
    theme(
      text = element_text(size = 4),
      strip.text.x = element_text(margin = margin(0.05, 0, 0.05, 0, "cm")),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
      aspect.ratio = 1.5 / 2,
      plot.title = element_text(size = 4),
      legend.key.size = unit(0.5, "cm"),
      legend.key.width = unit(0.5, "cm")
    )
  return(plot)
}

getColorMappings <- function(new_mappings = NULL) {
  extra_mappings <- read.csv(system.file(package = "openprom", file.path("extdata", "plotstyle.csv")))

  if (!is.null(new_mappings)) {
    extra_mappings <- new_mappings %>%
      filter(!(X %in% extra_mappings$X)) %>%
      bind_rows(extra_mappings)
  }

  color_mappings <- read.csv(system.file(package = "mip", file.path("extdata", "plotstyle.csv")), sep = ";") %>%
    select(c("X", "legend", "color")) %>%
    distinct() %>%
    bind_rows(extra_mappings)
  return(color_mappings)
}
