# Regression tests for the HTRC remote-path encoding fix.
#
# The remote rsync/http tree uses full pairtree encoding (`.` -> `,`,
# `:` -> `+`, `/` -> `=`) in both the stub directory and the filename's local
# id, while the local cache keeps the dot-encoded (`id_clean()`) convention.
# Before the fix, `stubby_url_to_rsync()` used `id_clean()` and so dropped the
# `.` -> `,` substitution, 404ing every htid with a dotted local id.

test_that("stubby_url_to_rsync leaves plain ids unchanged", {
  # No special characters in the local id: comma/plus/equals encoding is a
  # no-op, so output must match the historical (pre-fix) path exactly.
  expect_equal(
    stubby_url_to_rsync("mdp.39015001796443"),
    "mdp/31094/mdp.39015001796443.json.bz2"
  )
})

test_that("stubby_url_to_rsync comma-encodes dotted local ids", {
  # Live-verified against the HTRC tree 2026-07-07. The namespace-separating
  # period is preserved; every dot inside the local id becomes a comma, in both
  # the stub directory and the filename.
  expect_equal(
    stubby_url_to_rsync("miun.abj7655.0004.001"),
    "miun/a750,1/miun.abj7655,0004,001.json.bz2"
  )
  expect_equal(
    stubby_url_to_rsync("miua.4925052.0001.001"),
    "miua/4520,1/miua.4925052,0001,001.json.bz2"
  )
})

test_that("stubby_url_to_rsync encodes ark ids with ':' and '/'", {
  # ':' -> '+' and '/' -> '=' in the local id (no dots here, so unchanged by
  # the fix relative to the historical behavior).
  expect_equal(
    stubby_url_to_rsync("nc01.ark:/13960/t2v41mn4r"),
    "nc01/a+30214/nc01.ark+=13960=t2v41mn4r.json.bz2"
  )
  expect_equal(
    stubby_url_to_rsync("loc.ark:/13960/t0000000"),
    "loc/a+30000/loc.ark+=13960=t0000000.json.bz2"
  )
})

test_that("id_encode_local encodes only the local id, preserving the namespace period", {
  expect_equal(id_encode_local("miun.abj7655.0004.001"), "miun.abj7655,0004,001")
  expect_equal(id_encode_local("mdp.39015001796443"), "mdp.39015001796443")
  expect_equal(id_encode_local("nc01.ark:/13960/t2v41mn4r"), "nc01.ark+=13960=t2v41mn4r")
})

test_that("local cache path keeps the dot-encoded convention (unchanged by the fix)", {
  # The fix must NOT alter where files land locally, or find_cached_htids() would
  # stop finding them. local_loc() still uses id_clean() (dots preserved).
  expect_equal(
    as.character(local_loc("miun.abj7655.0004.001", suffix = "json.bz2", dir = "cache")),
    "cache/miun/a750.1/miun.abj7655.0004.001.json.bz2"
  )
})
