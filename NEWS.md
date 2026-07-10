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

