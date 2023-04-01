#' Call TISEAN routines.
#'
#' A verbose method before I figure out how to do this in rcpp!
#' @param tisean_path A string indicating the path to the Tisean directory
#' @param time_series A matrix or data frame containing the time series data.
#' @param routine A string indicating the Tisean routine to use.
#' @param remove_header A logical value indicating whether to remove the header from the output. Default is FALSE.
#' @param show_output_on_console A logical value indicating whether to show the output on console. Default is FALSE.
#' @param ... Additional parameters to be passed to the Tisean routine.
#'
#' @return A numeric vector or matrix.
#' @import dplyr
#' @examples
#' library(dissthispkgr)
#' library(dplyr)
#' henon_m <- tisean_diss(
#'   routine = "henon",
#'   remove_header = FALSE,
#'   show.output.on.console = FALSE, l = 10000)
#' plot(henon_m)
#' @export
tisean_diss <- function(
    tisean_path = "/mnt/working_volume/tisean/TISEAN_3.0.1_unix/Tisean_3.0.1",
    time_series = NULL, routine = NULL, remove_header = FALSE,
    show_output_on_console = FALSE, ...) {

  if (is.null(routine)) {
    stop("routine argument should not be NULL.")
  }

  # check system; OS x not tested.
  if (!.Platform$OS.type == "unix") paste0(routine, ".exe")

  dots <- list(...)
  param_string <- paste0("-", paste0(names(dots), unlist(dots)), collapse = " ")

  binary_location <- paste0(tisean_path, "/bin/", routine)

  # create output file
  output_file <- tempfile()
  output_file_name <- gsub("(^.*\\\\)(.*$)", "\\2", output_file)

  # create input file if time series is provided
  if (!is.null(time_series)) {
    # create a temp file name
    input_file <- tempfile()
    # dump data to the temp file
    write.table(time_series, file = input_file, row.names = F)
    shell_command <- paste(
      binary_location,
      input_file, param_string, "-o", output_file
    )
  } else {
    shell_command <- paste(
      binary_location,
      param_string, "-o", output_file
    )
  }

  # print command for troubleshooting
  print(shell_command)

  # execute command
  system(shell_command, show.output.on.console = show_output_on_console)

  # select the latest temp files from the directory.
  # multi-file output corner case is not handles; are there any ?
  output_file <- file.info(dir(tempdir(), full.names = TRUE)) %>%
    arrange(desc(ctime)) %>%
    filter(grepl(output_file_name, rownames(.))) %>%
    slice(1)

  # get data and remove temp file(s)
  if (!is.null(time_series)) unlink(input_file)

  det <- read.table(rownames(output_file), header = remove_header)
  unlink(output_file)
  return(det)
}
