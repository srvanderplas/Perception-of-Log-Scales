#' Function to save a lineup
#'
#' Takes plot and location/filename information, and saves the plot as a
#' png, pdf, and svg in subfolders of the specified path.
#' @param plot ggplot object
#' @param filename filename to use to save the plot
#' @param path vector of paths to save lineup files at
#' @param ... other arguments to be passed to ggsave or gridsvg
#' @return plot object and list of paths
#' @examples
#' library(nullabor)
#' library(dplyr)
#' library(ggplot2)
#'
#' data(aud)
#' p <- lineup(null_permute("rate"), aud) %>%
#'        ggplot(aes(x = date, y = rate)) +
#'        geom_point() +
#'        facet_wrap(~.sample)
#' save_lineup(p, "test", width = 8, height = 8, dpi = 600)
save_lineup <- function(plot, filename, path = "plots", script = "action.js", ...) {
  ext <- c("png", "pdf", "svg")
  filenames <- paste0(filename, ".", ext)

  # create folders to store images
  dirs <- file.path(path, ext)
  purrr::walk(dirs, ~if(!dir.exists(.)) dir.create(., recursive = T))

  filepaths <- file.path(dirs, filenames)

  ggsave(filename = filepaths[1], plot = plot, device = ext[1], ...)
  ggsave(filename = filepaths[2], plot = plot, device = ext[2], ...)

  print(plot)
  grobs <- grid::grid.ls(grid::grid.force())

  toggle <- "toggle"

  idx <- grep("panel-", grobs$name)
  for (i in idx) {
    grid.garnish(grobs$name[i],
                 onmouseover = paste("frame('",grobs$name[i + 2], ".1')", sep = ""),
                 onmouseout = paste("deframe('",grobs$name[i + 2], ".1')", sep = ""),
                 onmousedown = paste(sprintf("%shigh(evt, '", toggle),grobs$name[i + 2], ".1')", sep = ""))
  }

  # Include script with each SVG
  # This may cause problems with shiny (not sure)
  jsfile <- paste(readLines(script), collapse = "\n")
  grid.script(jsfile, type = "text/javascript")

  # Include link to the script
  # (but file paths have to be correct for this to work with Shiny)
  # grid.script(filename = script, type = "text/javascript")
  grid.export(name = filepaths[3],
              uniqueNames = FALSE,
              exportJS = "inline",
              exportCoords = "inline",
              exportMappings = "inline")

  # return list of stuff invisibly
  invisible(list(plot = plot, files = filepaths, script_link = script))
}
