# hathiTools 0.3.0

* New function `htids_to_dfm()` builds a page-level [quanteda](https://quanteda.io) document-feature matrix directly from the JSON Extracted Features files of a set of Hathi Trust IDs (optionally rsyncing any missing files first), with filtering by part of speech, token pattern, token length, page language, and sentence count.
* Added internal helpers for stable random projections of Extracted Features files (`srp()` and friends in `R/srp.R`).
* Migrated the pkgdown articles from R Markdown (`.Rmd`) to Quarto (`.qmd`); the `.qmd` sources are now the single source of truth and their `.Rmd` twins have been removed.
* Modernised the GitHub Actions workflows to `r-lib/actions` v2 (R-CMD-check matrix and pkgdown deploy), and added a Quarto setup step plus `Config/Needs/website: quarto` so the `.qmd` articles render on the pkgdown runner.
* Switched the live-API `@examples` (e.g. `query_bookworm()`, `workset_builder()`, `get_workset_meta()`, `get_hathi_counts()`, `get_hathi_meta()`, `get_hathi_page_meta()`, `cache_htids()`, `rsync_from_hathi()`) from `\donttest{}` to `\dontrun{}` so `R CMD check --run-donttest` no longer fails on network access.

# hathiTools 0.2.1
* Fixed the remote HTRC path encoding used by `rsync_from_hathi()` / `htid_to_rsync()` (via `stubby_url_to_rsync()`) and the HTTP fallback in `get_hathi_counts()` (via `download_http()`). The remote Extracted Features tree uses full pairtree encoding (`.` -> `,`, `:` -> `+`, `/` -> `=`) in both the stub directory and the filename's local id, but the previous code used `id_clean()`, which omits the `.` -> `,` substitution. As a result, every htid whose local id contains a period (e.g. Michigan ids such as `miun.*` and `miua.*`) resolved to a non-existent remote path and failed to download. Remote paths now use `id_encode()`-style encoding while the local cache layout is unchanged, so `find_cached_htids()` still locates downloaded files.

# hathiTools 0.2.0
* More consistent API for caching functions (includes now by default caching directory, cache format, and cache type)
* Faster caching, especially of page metadata
* Caching to parquet and feather formats
* Changes to `query_bookworm()` to work with Bookworm2021 version of the bookworm database
* `query_bookworm()` now accepts a literal query string
* Changes to `workset_builder()` to work with the latest version of the EF workset builder (2.0)
* New vignettes to show example workflows, tokenization, etc.

# hathiTools 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Added a `get_hathi_page_meta()` function to retrieve the page-level metadata of any given volume
* Fixes to messages in `cache_htids()`.
* Fixes to documentation and addition of descriptions of metadata variables.

# hathiTools 0.0.0.9001

* Initial release

