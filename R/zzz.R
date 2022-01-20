.onLoad <- function(libname, pkgname) {
  op <- options()
  op.hathiTools <- list(
    hathiTools.hathifile.dir = "./raw-hathifiles",
    hathiTools.metadata.dir = "./metadata",
    hathiTools.ef.dir = "./hathi-ef",
    hathiTools.cachetype = "csv.gz"
  )
  toset <- !(names(op.hathiTools) %in% names(op))
  if(any(toset)) options(op.hathiTools[toset])

  invisible()
}
