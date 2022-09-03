.onLoad <- function(libname, pkgname) {
  op <- options()
  op.hathiTools <- list(
    hathiTools.hathifile.dir = "./raw-hathifiles",
    hathiTools.metadata.dir = "./metadata",
    hathiTools.ef.dir = "./hathi-ef",
    hathiTools.cacheformat = "csv.gz"
  )
  toset <- !(names(op.hathiTools) %in% names(op))
  if(any(toset)) options(op.hathiTools[toset])

  invisible()

}

.onAttach <- function(libname, pkgname) {
  fields <- NULL
  if(is.null(getOption("hathiTools.bookworm.fields"))) {
    if(curl::has_internet()) {
      msg <- .makeMessage("Unable to load available fields for bookworm db - ",
                          "internet connection may not be available. ",
                          "options(\"hathiTools.bookworm.fields\") should be NULL.")
      tryCatch(fields <- suppressMessages(query_bookworm(method = "returnPossibleFields")),
               error = function(e) packageStartupMessage(msg))
    }
    if(!is.null(fields)) {
      options(hathiTools.bookworm.fields = fields$name)
      packageStartupMessage("Available fields for bookworm queries in the Bookworm2021 db:")
      packageStartupMessage(paste(fields$name, collapse = ", "))
      packageStartupMessage("Retrieve options via getOption(\"hathiTools.bookworm.fields\")")
    }
  }
  packageStartupMessage("Currently caching Hathi Trust Extracted Features files to ",
                        getOption("hathiTools.ef.dir"))
  packageStartupMessage("Change default caching directory by setting ",
                        "options(hathiTools.ef.dir = $DIR)")
  packageStartupMessage("Default cache format ",
                        getOption("hathiTools.cacheformat"))
  packageStartupMessage("Change default cache format by setting ",
                        "options(hathiTools.cacheformat = 'new_cache_format')")
}
