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

library(grid)
library(gridSVG)
library(stringr)

save_lineup <- function(plot, filename, path = "plots", script = "www/js/action.js", ...) {
  ext <- c("png", "pdf", "svg")
  filenames <- paste0(filename, ".", ext)

  # create folders to store images
  dirs <- file.path(path, ext)
  purrr::walk(dirs, ~if(!dir.exists(.)) dir.create(., recursive = T))

  filepaths <- file.path(dirs, filenames)

  ggsave(filename = filepaths[1], plot = plot, device = ext[1], ...)
  ggsave(filename = filepaths[2], plot = plot, device = ext[2], ...)

  print(plot)

  # This only works for 5 cols 4 rows
  name_corresp <- tribble(
    ~panelname, ~replacement,
    "panel-1-1.8-5-8-5", "1", "panel-5-1.8-9-8-9", "2", "panel-4-2.8-13-8-13", "3", "panel-3-3.8-17-8-17", "4", "panel-2-4.8-21-8-21", "5",
    "panel-2-1.13-5-13-5", "6", "panel-1-2.13-9-13-9", "7", "panel-5-2.13-13-13-13", "8", "panel-4-3.13-17-13-17", "9", "panel-3-4.13-21-13-21", "10",
    "panel-3-1.18-5-18-5", "11", "panel-2-2.18-9-18-9", "12", "panel-1-3.18-13-18-13", "13", "panel-5-3.18-17-18-17", "14", "panel-4-4.18-21-18-21", "15",
    "panel-4-1.23-5-23-5", "16", "panel-3-2.23-9-23-9", "17", "panel-2-3.23-13-23-13", "18", "panel-1-4.23-17-23-17", "19", "panel-5-4.23-21-23-21", "20")
  namerepl <- c(name_corresp$replacement) %>% magrittr::set_names(name_corresp$panelname)
  grobs <- grid::grid.ls(grid::grid.force())



  toggle <- "toggle"

  idx <- grep("panel-", grobs$name)
  for (i in idx) {
    grid.garnish(grobs$name[i],
                 id = str_replace_all(grobs$name[i], namerepl),
                 onmouseover = paste("frame('",grobs$name[i + 2], ".1')", sep = ""),
                 onmouseout = paste("deframe('",grobs$name[i + 2], ".1')", sep = ""),
                 onmousedown = paste(sprintf("%shigh(evt, '", toggle),grobs$name[i + 2], ".1')", sep = ""))
  }

  # Include script with each SVG
  # This may cause problems with shiny (not sure)
  # jsfile <- paste(readLines(script), collapse = "\n")
  # grid.script(jsfile, type = "text/javascript")

  # Include link to the script
  # (but file paths have to be correct for this to work with Shiny)
  grid.script(filename = script, type = "text/javascript")
  grid.export(name = filepaths[3],
              uniqueNames = FALSE,
              exportJS = "inline",
              exportCoords = "inline",
              exportMappings = "inline")

  # return list of stuff invisibly
  invisible(list(plot = plot, files = filepaths, script_link = script))
}
