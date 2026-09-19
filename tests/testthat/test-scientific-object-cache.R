test_that("new_scientific_cache_key works with its own documented default (no dependency fingerprints)", {
  # order(NULL) is a hard error -- names(character(0)) is NULL, so the
  # documented default dependency_fingerprints = character() previously
  # crashed this function unconditionally, before it could ever be called
  # without explicitly supplying a non-empty dependency_fingerprints.
  expect_no_error(
    key <- new_scientific_cache_key(
      object_fingerprints = c(vcf = "abc123"),
      schema_id = "schema-a", schema_version = "1.0",
      module_id = "diversity", module_version = "1.0"
    )
  )
  expect_identical(key$dependency_fingerprints, character())
})

test_that("cache key fingerprints are deterministic and distinguish different content", {
  key1 <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc123"),
    schema_id = "schema-a", schema_version = "1.0", module_id = "diversity",
    module_version = "1.0", parameters = list(window = 100L)
  )
  key2 <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc123"),
    schema_id = "schema-a", schema_version = "1.0", module_id = "diversity",
    module_version = "1.0", parameters = list(window = 100L)
  )
  key3 <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc123"),
    schema_id = "schema-a", schema_version = "1.0", module_id = "diversity",
    module_version = "1.0", parameters = list(window = 200L)
  )

  expect_identical(key1$fingerprint, key2$fingerprint)
  expect_false(identical(key1$fingerprint, key3$fingerprint))
})

test_that("the cache contract fingerprint is a compact, fixed-size digest, not a full re-encoding of the value", {
  # Previously hex-encoded the entire serialized byte stream one byte at a
  # time -- not a compact fingerprint at all, but a literal, invertible
  # re-encoding that doubled in size with the object being fingerprinted.
  # A real digest (this package's own established convention elsewhere,
  # e.g. hash_file(), R/utils.R) stays a fixed size regardless of input size.
  small <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc"),
    schema_id = "s", schema_version = "1", module_id = "m", module_version = "1"
  )
  large <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc"),
    schema_id = "s", schema_version = "1", module_id = "m", module_version = "1",
    parameters = list(big = strrep("x", 100000L))
  )

  expect_identical(nchar(small$fingerprint), nchar(large$fingerprint))
  expect_true(nchar(small$fingerprint) < 100L)
})

test_that("a cache manifest round-trips through validation with a matching key and payload checksum", {
  key <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc123"),
    schema_id = "schema-a", schema_version = "1.0", module_id = "diversity",
    module_version = "1.0"
  )
  manifest <- new_scientific_cache_manifest(
    key = key, payload_checksum = "deadbeef", payload_format = "rds",
    provenance_fingerprint = "prov-1", created_by = "test"
  )

  report <- validate_scientific_cache_manifest(
    manifest, expected_key = key, payload_checksum = "deadbeef"
  )
  expect_true(report$valid)
  expect_identical(report$decision, "accept")
})

test_that("a tampered manifest fingerprint is rejected", {
  key <- new_scientific_cache_key(
    object_fingerprints = c(vcf = "abc123"),
    schema_id = "schema-a", schema_version = "1.0", module_id = "diversity",
    module_version = "1.0"
  )
  manifest <- new_scientific_cache_manifest(
    key = key, payload_checksum = "deadbeef", payload_format = "rds",
    provenance_fingerprint = "prov-1", created_by = "test"
  )
  manifest$manifest_fingerprint <- "tampered"

  report <- validate_scientific_cache_manifest(manifest)
  expect_false(report$valid)
  expect_true("manifest_fingerprint_mismatch" %in% report$errors)
})
