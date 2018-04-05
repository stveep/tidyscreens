#' This function reads in a set of files, adds a column with the filename (with extension stripped) and combines them into one big tibble.
#'
#' @param pattern input character Pattern to match filenames (e.g. "*.xls").
#' @param path input character Path to directory to search for files.
#' @param replacements input character vector These patterns will be deleted from filenames to form the sample name.
#' @param type input character "tsv" or "excel" to specify whether \link{read_tsv} or \link{read_excel} is used to import data.
#' @return A tibble combining all input files with a new column giving the sample (derived from stripping the 
#' replacements from the filename). If the files have different numbers of columns, a list of tibbles is returned instead.
#' @seealso \code{\link{list.files}, \link{gsub}} which this function wraps.
#' @export
#' @examples
#' \dontrun{ filenamer("*.txt",path="~/data/", replacements=c(".txt"), type="tsv") }  
filenamer <- function(pattern, path="./", replacements = c(".xls",".txt",".tsv",".csv",".xlsx"), type="tsv") {
       	flist <- list.files(pattern, path=path)
	fpaths <- paste0(path,flist)
	tibs <- switch(type,
	       tsv = { lapply(fpaths, readr::read_tsv) },
	       excel = { lapply(fpaths, readxl::read_excel) })
	# Make replacements on filenames (remove extensions etc) to tidy up names
	if (length(replacements) > 0) {
		for (i in replacements) {
			flist <- gsub(i,"",flist)
		}
	}
	names(tibs) <- flist
	tibs <- lapply(names(tibs), function(x) mutate(tibs[[x]], sample = x))
	if (length(unique(sapply(tibs, ncol))) == 1) { # If all tibbles have same number of columns
		return(dplyr::bind_rows(tibs)) # Return a single dataframe
	} else {
		message(paste("Numbers of rows differ:", do.call(paste,lapply(tibs, ncol))))
		return(tibs) # If inconsistent column numbers, just return the list
	}

}

	
