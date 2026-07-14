.snprate_component <- function(result, candidates, label, required = TRUE) {
  hit <- candidates[candidates %in% names(result)]
  if (length(hit)) return(result[[hit[[1L]]]])
  if (!required) return(NULL)
  stop(
    sprintf(
      "SNPRelate::snpgdsSNPRateFreq did not return %s. Available fields: %s",
      label,
      paste(names(result), collapse = ", ")
    ),
    call. = FALSE
  )
}

normalize_snpratefreq <- function(result) {
  snp_id <- .snprate_component(result, c("snp.id", "snp_id", "SNP.ID"), "SNP identifiers")
  allele_frequency <- .snprate_component(
    result,
    c("AlleleFreq", "allele.freq", "allele_frequency"),
    "allele frequencies"
  )
  minor_frequency <- .snprate_component(
    result,
    c("MinorFreq", "minor.freq", "maf", "MAF"),
    "minor-allele frequencies"
  )

  missing_rate <- .snprate_component(
    result,
    c("MissingRate", "missing.rate", "missing_rate"),
    "missing rates",
    required = FALSE
  )
  call_rate <- .snprate_component(
    result,
    c("CallRate", "call.rate", "call_rate"),
    "call rates",
    required = FALSE
  )

  if (is.null(missing_rate) && is.null(call_rate)) {
    stop(
      paste0(
        "SNPRelate::snpgdsSNPRateFreq returned neither missing-rate nor ",
        "call-rate values. Available fields: ", paste(names(result), collapse = ", ")
      ),
      call. = FALSE
    )
  }
  if (is.null(missing_rate)) missing_rate <- 1 - call_rate
  if (is.null(call_rate)) call_rate <- 1 - missing_rate

  lengths <- c(
    snp_id = length(snp_id),
    allele_frequency = length(allele_frequency),
    minor_frequency = length(minor_frequency),
    missing_rate = length(missing_rate),
    call_rate = length(call_rate)
  )
  if (length(unique(lengths)) != 1L) {
    stop(
      sprintf(
        "Inconsistent SNPRelate rate/frequency result lengths: %s",
        paste(sprintf("%s=%d", names(lengths), lengths), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  list(
    snp_id = snp_id,
    allele_frequency = allele_frequency,
    minor_frequency = minor_frequency,
    missing_rate = missing_rate,
    call_rate = call_rate
  )
}

variant_qc <- function(gds, sample_ids, ids, maf_threshold, max_missing = 0.2) {
  raw <- call_supported(
    SNPRelate::snpgdsSNPRateFreq,
    list(gds, sample.id = sample_ids, with.id = TRUE, verbose = FALSE),
    "SNPRelate::snpgdsSNPRateFreq"
  )
  st <- normalize_snpratefreq(raw)
  dt <- data.table::data.table(
    snp_id = st$snp_id,
    chromosome = ids$chromosome[match(st$snp_id, ids$snp)],
    position = ids$position[match(st$snp_id, ids$snp)],
    allele = ids$allele[match(st$snp_id, ids$snp)],
    maf = st$minor_frequency,
    alternate_allele_frequency = st$allele_frequency,
    call_rate = st$call_rate,
    missing_rate = st$missing_rate
  )
  dt[, pass_maf := is.finite(maf) & maf >= maf_threshold]
  dt[, pass_missing := is.finite(missing_rate) & missing_rate <= max_missing]
  dt[, pass_combined := pass_maf & pass_missing]
  dt
}

ld_prune_exact <- function(gds, sample_ids, maf_threshold, threads, seed) {
  safe_threads <- max(1L, min(as.integer(threads), 4L))
  set.seed(seed)

  # Some SNPRelate releases coerce slide.max.bp to a 32-bit integer. Passing
  # Inf then becomes NA and disables the intended comparison window. The
  # largest positive integer is the API-safe representation of an unbounded
  # genomic window for VCF coordinates while preserving slide.max.bp = Inf
  # semantics from the public popgenVCF configuration.
  effective_slide_max_bp <- .Machine$integer.max

  z <- SNPRelate::snpgdsLDpruning(
    gds,
    sample.id = sample_ids,
    maf = maf_threshold,
    missing.rate = 0.2,
    method = "corr",
    ld.threshold = sqrt(0.2),
    slide.max.bp = effective_slide_max_bp,
    slide.max.n = 50L,
    start.pos = "first",
    autosome.only = FALSE,
    num.thread = safe_threads,
    verbose = FALSE
  )
  out <- unique(unlist(z, use.names = FALSE))
  if (!length(out)) stop("LD pruning retained no SNPs", call. = FALSE)
  # SNPRelate requires snp.id to be a plain atomic vector. Do not attach
  # custom attributes here: is.vector() becomes FALSE when non-name
  # attributes are present, and downstream SNPRelate calls reject the IDs.
  as.vector(out)
}

qc_reports <- function(vq, final_snps) {
  vq[, retained_ld := snp_id %in% final_snps]
  bad <- vq[retained_ld & !pass_combined]
  if (nrow(bad)) stop("SNPRelate LD set disagrees with independent MAF/missingness audit", call. = FALSE)
  independent <- data.table::data.table(
    criterion = c("Input biallelic", "Pass MAF", "Pass missingness", "Pass both", "Final LD-pruned"),
    variants = c(nrow(vq), sum(vq$pass_maf), sum(vq$pass_missing), sum(vq$pass_combined), length(final_snps))
  )
  independent[, retained_percent := 100 * variants / variants[1]]
  sequential <- data.table::data.table(
    step = c("Input biallelic", "After MAF", "After missingness", "After LD pruning"),
    variants = c(nrow(vq), sum(vq$pass_maf), sum(vq$pass_combined), length(final_snps))
  )
  sequential[, `:=`(removed_at_step = c(0L, utils::head(variants, -1) - utils::tail(variants, -1)),
                    retained_percent = 100 * variants / variants[1])]
  list(variant = vq, independent = independent, sequential = sequential)
}

plot_qc_reports <- function(reports, sample_qc, cfg, dirs) {
  fmts <- cfg$output$figure_formats; dpi <- cfg$output$dpi
  p1 <- ggplot2::ggplot(reports$variant, ggplot2::aes(maf)) + ggplot2::geom_histogram(bins = 50) +
    ggplot2::geom_vline(xintercept = cfg$qc$maf, linetype = 2) +
    ggplot2::labs(title = "Minor allele frequency", x = "MAF", y = "Variants") + theme_publication()
  save_plot(p1, "01_MAF", dirs, fmts, 7, 5, dpi)
  p2 <- ggplot2::ggplot(reports$variant, ggplot2::aes(missing_rate)) + ggplot2::geom_histogram(bins = 50) +
    ggplot2::geom_vline(xintercept = 0.2, linetype = 2) +
    ggplot2::labs(title = "Variant missingness", x = "Missing rate", y = "Variants") + theme_publication()
  save_plot(p2, "02_variant_missingness", dirs, fmts, 7, 5, dpi)
  p3 <- ggplot2::ggplot(sample_qc, ggplot2::aes(stats::reorder(sample, missing_rate), missing_rate, fill = population)) +
    ggplot2::geom_col() + ggplot2::coord_flip() + ggplot2::geom_hline(yintercept = cfg$qc$max_sample_missing, linetype = 2) +
    ggplot2::scale_fill_manual(values = population_palette(sample_qc$population)) +
    ggplot2::labs(title = "Per-sample missingness", x = NULL, y = "Missing rate") + theme_publication()
  save_plot(p3, "03_sample_missingness", dirs, fmts, 8, max(5, nrow(sample_qc) * 0.12), dpi)
  p4 <- ggplot2::ggplot(reports$sequential, ggplot2::aes(step, variants)) + ggplot2::geom_col() +
    ggplot2::geom_text(ggplot2::aes(label = scales::comma(variants)), vjust = -0.4) +
    ggplot2::labs(title = "Sequential SNP retention", x = NULL, y = "Variants") + theme_publication() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 25, hjust = 1))
  save_plot(p4, "04_SNP_retention", dirs, fmts, 8, 5, dpi)
}
