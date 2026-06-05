################################################################
# analyze_3words_F0.R
#
# Acoustic analysis of the first three words (article, noun, verb)
# for a Spanish stress study: PRESENT (a) vs PAST (b).
#   a = present tense, penult stress  (e.g. BUS-ca)
#   b = past tense,    final stress   (e.g. bus-CO)
#
# Measures:
#   * F0        -> contour GAMMs over normalized utterance time
#   * Intensity -> contour GAMMs over normalized utterance time
#   * Duration  -> per-syllable summaries (by position + whole word)
# Plus per-syllable summary tables of all three measures, reported
# BOTH by syllable position (penult/final/...) and by whole word.
#
# Input: the tab-separated file produced by extract_f0_3words.praat
#        (18 columns; header on the SECOND row, a title on row 1 if
#        exported from Excel -- the loader handles both).
#
# This is the F0-only adaptation of the older combined F0+EEG script:
# all EEG/PsychoPy loading, EEG GAMMs, combined F0+EEG figures, and
# the L1-vs-L2 comparison have been removed (this study has no EEG).
#
# Run: open in RStudio and source, or `Rscript analyze_3words_F0.R`.
# When sourced interactively it will ask you to pick the input file.
################################################################

## ----------------------------- Packages -----------------------------
library(tidyverse)
library(readxl)
library(mgcv)        # bam() GAMMs
library(itsadug)     # start_event(), acf_resid(), AR(1) helpers
library(patchwork)   # plot composition
library(viridis)

## ------------------------- 0. Load the data -------------------------
# Accepts either the Praat .txt (tab-separated) or an .xlsx where the
# real header is on row 2 (row 1 is a title like "output").

read_praat_output <- function(path) {
  # The 18 columns are fixed and known, so we assign them explicitly rather
  # than trusting header parsing (UTF-16 BOMs from Praat can corrupt the
  # header line and silently promote the first data row to column names).
  praat_cols <- c("file_stem","condition","item_num","word_num","word_type",
                  "word_label","syllable_num","syllable_pos","syll_label",
                  "syll_dur_ms","syll_start_ms","syll_end_ms","point",
                  "time_ms","time_norm","f0_hz","intensity_db","is_voiced")

  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx", "xls")) {
    raw <- readxl::read_excel(path, col_names = FALSE)
    hdr_row <- which(apply(raw, 1, function(r) any(r == "file_stem", na.rm = TRUE)))[1]
    if (is.na(hdr_row)) hdr_row <- 1
    dat <- raw[(hdr_row + 1):nrow(raw), , drop = FALSE]
  } else {
    # Detect encoding from the byte-order mark. Praat on Mac usually writes
    # UTF-16; the BOM tells us LE vs BE. We then open a CONNECTION with that
    # encoding so R actually transcodes the bytes to its native encoding
    # (readLines(path, encoding=) only *labels* bytes, it does not convert,
    # which is what caused the "unable to translate" error).
    con <- file(path, "rb"); bom <- readBin(con, "raw", 4); close(con)
    file_enc <- "UTF-8"
    if (length(bom) >= 2) {
      if (bom[1] == as.raw(0xff) && bom[2] == as.raw(0xfe)) {
        file_enc <- "UTF-16LE"
      } else if (bom[1] == as.raw(0xfe) && bom[2] == as.raw(0xff)) {
        file_enc <- "UTF-16BE"
      }
    }
    con <- file(path, "r", encoding = file_enc)
    all_lines <- readLines(con, warn = FALSE)
    close(con)
    if (length(all_lines) == 0) stop("The Praat output file appears to be empty.")
    # Drop any leading BOM that survived transcoding, plus other invisibles,
    # from the first line. enc2utf8 first so the regex operates on valid UTF-8.
    all_lines[1] <- enc2utf8(all_lines[1])
    all_lines[1] <- gsub("\ufeff", "", all_lines[1], useBytes = FALSE)
    all_lines[1] <- gsub("[^[:print:]\t]", "", all_lines[1])
    # Find the real header line (containing 'file_stem') and drop everything
    # up to and including it; also removes a stray title line above it.
    hdr_idx <- which(grepl("file_stem", all_lines, fixed = TRUE))[1]
    data_lines <- if (is.na(hdr_idx)) all_lines else all_lines[(hdr_idx + 1):length(all_lines)]
    data_lines <- data_lines[nzchar(data_lines)]
    if (length(data_lines) == 0) stop("No data rows found after the header.")
    dat <- readr::read_tsv(I(data_lines), col_names = FALSE,
                           show_col_types = FALSE, na = c("", "NA"))
  }

  # Assign the known names; guard against a column-count mismatch.
  if (ncol(dat) != length(praat_cols)) {
    stop(sprintf("Expected %d columns but found %d. Check the Praat output file.",
                 length(praat_cols), ncol(dat)))
  }
  colnames(dat) <- praat_cols
  dat
}

## ===================================================================
## This script is organized in TWO parts:
##   PART 1 (COMPUTE): load, tidy, filter to 2-syllable nouns, fit all
##                     models, build every plot object and table.
##                     Nothing is printed or saved here.
##   PART 2 (RENDER) : at the very end, print all tables and all plots
##                     and save them to plots/ , tables/ , models/.
## Conditions: a = present (paroxytone, purple), b = past (oxytone, green).
## Only 2-syllable-noun items are analyzed.
## Plot style matches the project website: F0 in semitones, absolute time
## in ms, dashed vertical syllable-boundary lines with labels
## (Art, N-s1, N-s2, V-s1, V-s2), contour panel stacked over a
## difference panel (paroxytone - oxytone) with red significant dots.
## ===================================================================

# Interactive pick if not provided:
if (!exists("input_path")) {
  input_path <- file.choose()
}

## ========================== PART 1: COMPUTE =========================

# ---- 1.1 load + tidy ----
praat_raw <- read_praat_output(input_path)

praat_clean <- praat_raw %>%
  mutate(
    item_num      = as.integer(item_num),
    word_num      = as.integer(word_num),
    syllable_num  = as.integer(syllable_num),
    syll_dur_ms   = as.numeric(syll_dur_ms),
    syll_start_ms = as.numeric(syll_start_ms),
    syll_end_ms   = as.numeric(syll_end_ms),
    point         = as.integer(point),
    time_ms       = as.numeric(time_ms),
    time_norm     = as.numeric(time_norm),
    f0_hz         = suppressWarnings(as.numeric(na_if(as.character(f0_hz), "NA"))),
    intensity_db  = suppressWarnings(as.numeric(na_if(as.character(intensity_db), "NA"))),
    is_voiced     = as.integer(is_voiced),
    condition  = factor(condition, levels = c("a", "b")),
    word_type  = factor(word_type, levels = c("article", "noun", "verb")),
    syllable_pos = factor(syllable_pos,
                          levels = c("only", "antepenult", "penult", "final", "pre3"))
  ) %>%
  mutate(
    f0_st = ifelse(!is.na(f0_hz) & f0_hz > 0, 12 * log2(f0_hz / 100), NA_real_),
    item_id     = item_num,
    participant = sub("_.*$", "", file_stem),   # "P01_L1_01a" -> "P01"
    stimulus_id = paste0(item_num, "_", condition),
    syll_tag = dplyr::case_when(
      word_type == "article" ~ "Art",
      word_type == "noun"    ~ paste0("N-s", syllable_num),
      word_type == "verb"    ~ paste0("V-s", syllable_num),
      TRUE ~ NA_character_
    )
  ) %>%
  mutate(
    # ---- PER-SYLLABLE CONTINUOUS TIME INDEX (0-5) ----
    # Replaces absolute ms so that syllable boundaries align ACROSS tokens
    # regardless of duration. Each syllable occupies one unit:
    #   Art = [0,1), N-s1 = [1,2), N-s2 = [2,3), V-s1 = [3,4), V-s2 = [4,5].
    # syll_pos_idx is the ordinal slot; within_syll is 0->1 progress through
    # the current syllable (from its start ms to its end ms); time_idx is the
    # sum. A point at 2.5 = "halfway through N-s2" in EVERY token.
    syll_pos_idx = dplyr::case_when(
      syll_tag == "Art"  ~ 0L,
      syll_tag == "N-s1" ~ 1L,
      syll_tag == "N-s2" ~ 2L,
      syll_tag == "V-s1" ~ 3L,
      syll_tag == "V-s2" ~ 4L,
      TRUE ~ NA_integer_
    ),
    within_syll = dplyr::case_when(
      is.na(syll_start_ms) | is.na(syll_end_ms) ~ NA_real_,
      (syll_end_ms - syll_start_ms) <= 0        ~ 0.5,   # degenerate guard
      TRUE ~ pmin(pmax((time_ms - syll_start_ms) /
                       (syll_end_ms - syll_start_ms), 0), 1)
    ),
    time_idx = syll_pos_idx + within_syll
  )

# ---- 1.2 restrict to 2-SYLLABLE-NOUN items only ----
noun_syll_count <- praat_clean %>%
  filter(word_type == "noun") %>%
  group_by(item_id) %>%
  summarise(n_noun_sylls = n_distinct(syllable_num), .groups = "drop")

items_2syll <- noun_syll_count %>% filter(n_noun_sylls == 2) %>% pull(item_id)

n_items_total <- n_distinct(praat_clean$item_id)
praat_clean <- praat_clean %>% filter(item_id %in% items_2syll)
n_items_kept <- n_distinct(praat_clean$item_id)

# voiced-only subset for F0
praat_voiced <- praat_clean %>% filter(!is.na(f0_hz) & f0_hz > 0)

# ---- 1.3 output dirs + aesthetics ----
dir.create("plots",  showWarnings = FALSE)
dir.create("tables", showWarnings = FALSE)
dir.create("models", showWarnings = FALSE)

cond_cols <- c("a" = "#882255", "b" = "#117733")   # purple / green
cond_labs <- c("Paroxytone", "Oxytone")            # present / past

# ---- 1.4 syllable boundaries on the PER-SYLLABLE INDEX scale ----
# On the 0-5 index, boundaries are exact integers (no token-to-token averaging
# needed): Art spans [0,1], N-s1 [1,2], ... V-s2 [4,5]. Labels sit at the
# midpoints (0.5, 1.5, ...). This is the x-axis for all contour/difference plots.
syll_levels_idx <- c("Art", "N-s1", "N-s2", "V-s1", "V-s2")
syll_bounds <- tibble::tibble(
  syll_tag = factor(syll_levels_idx, levels = syll_levels_idx),
  start    = 0:4,
  end      = 1:5,
  mid      = (0:4) + 0.5)

# vertical divider positions = each integer syllable boundary 0..5
divider_x <- 0:5

# ---- 1.5 helper: bin the per-syllable index for the GAMM ----
# Models run over the continuous per-syllable index (time_idx, range 0-5), NOT
# absolute ms. We lightly bin it (default 0.02 units ~ 50 bins/syllable) so the
# discrete=TRUE fitter has a modest number of unique covariate values. The
# binned column is still called time_bin so the model/plot code is unchanged.
bin_ms <- function(df, col = "time_idx", width = 0.02) {
  df[["time_bin"]] <- round(df[[col]] / width) * width
  df
}

# ---- 1.6 fit a contour GAMM in absolute ms and return tidy predictions ----
# Random-effects structure follows Casillas's "keep it maximal" guidance and
# the Coretta & Casillas (2025) GAMM tutorial for bilingualism research.
# The fixed-effect / autocorrelation / difference-smooth choices below follow
# Wieling (2018), "Analyzing dynamic phonetic data using GAMM": a tutorial.
#
# Two kinds of grouping-level term are used, deliberately:
#   * ITEM: random intercept s(item_id, bs="re") + random condition slope
#     s(item_id, condition, bs="re"). These let each item shift up/down and
#     tilt by condition. (~90 items -> plenty of levels to estimate.)
#   * PARTICIPANT: a FACTOR SMOOTH s(time_bin, participant, bs="fs", m=1).
#     Unlike a bs="re" term (which only adjusts a curve's height/rotation), a
#     factor smooth fits a DIFFERENT TRAJECTORY SHAPE to each participant --
#     the appropriate choice for dynamic/contour data, where speakers differ
#     in the *shape* of their F0/intensity contour, not just overall level.
#     m=1 applies a stronger smoothing penalty as a safeguard against
#     over-fitting (both per the tutorial).
#
# The participant factor smooth needs enough speakers to be meaningful, so it
# is added automatically only once n_part >= 5 (you're heading to 15). Until
# then it is skipped to avoid an unstable/over-parameterised fit.
#
# FOUR additions over the original version, all from the Wieling tutorial:
#   (1) ORDERED-FACTOR DIFFERENCE SMOOTH (Sec 4.5.3). Instead of fitting one
#       smooth per condition and hand-subtracting them, we fit a reference
#       smooth s(time_bin) plus a centred difference smooth s(time_bin,
#       by=conditionO). That difference smooth carries its OWN p-value telling
#       us directly whether the paroxytone/oxytone contour differs in SHAPE,
#       and conditionO as a parametric term tests the CONSTANT offset. This is
#       both the proper significance test and the correct way to get the
#       difference curve + CI (the model knows the covariance between the two
#       conditions; hand-subtracting two independent predictions does not).
#   (2) AR(1) AUTOCORRELATION CORRECTION (Sec 4.8). Densely sampled contours
#       have residuals that are ~0.9 correlated at lag 1; ignoring this makes
#       CIs far too narrow and "significant" windows spuriously wide. We fit
#       once, read the lag-1 residual autocorrelation, then refit with that as
#       rho and AR.start marking each trajectory's first sample.
#   (3) gam.check()-style k diagnostic (Sec 4.3): we report k-index/edf so you
#       can tell whether k needs raising (re-run with a larger k if flagged).
#   (4) Optional heavy-tailed family (Sec 4.6): F0 residuals are often
#       heavy-tailed (octave/halving tracking errors); set family="scat" to
#       use a scaled-t model. Note scat is slower and (like all non-fREML
#       comparisons) cannot be compared across fixed effects via compareML.
#
# `event_cols` identifies one contiguous time series (one token): the data are
# ordered by time within each so AR.start / acf_resid are meaningful.
# --- helper: find the POPULATION-LEVEL difference-smooth label in a fitted ---
# model, i.e. the s(time_bin):conditionO... term that does NOT involve a
# grouping factor. Derived from the model so it matches summary() exactly.
fit_diff_label <- function(m) {
  labs <- vapply(m$smooth, function(s) s$label, character(1))
  cand <- labs[grepl("conditionO", labs) & !grepl("participant|item_id", labs)]
  if (length(cand) >= 1) cand[1] else NA_character_
}

# --- helper: fit ONE GAMM with a chosen factor-smooth basis (tp or cr) -------
# Implements the random-effects structure recommended by Soskuthy (2021) for
# WITHIN-ITEM effects: random REFERENCE smooths + random DIFFERENCE smooths.
# Because our treatment (condition: present/paroxytone vs past/oxytone) varies
# WITHIN each participant, Soskuthy's Set 3 simulations show that:
#   * a single by-participant factor smooth (item-by-effect / item x effect) is
#     overly CONSERVATIVE (type I ~ 0, power ~ 0.06): the random smooth soaks up
#     variance that belongs to the fixed difference; and
#   * a reference + difference factor-smooth pair restores power (~0.57) while
#     keeping type I nominal (~0.05), because the by-participant CONTRAST is
#     explicitly penalised (shrunk), like (1 + condition | participant) in lme4.
# So the participant structure is:
#     s(time_bin, participant, bs="fs", m=1)                # reference smooths
#   + s(time_bin, participant, by=conditionO, bs="fs", m=1) # difference smooths
# We keep a by-ITEM random intercept (s(item_id, bs="re")) for word-level height
# variation, but DROP the old by-item condition slope: the participant
# difference smooth now carries the within-participant contrast variability.
.fit_one_basis <- function(df, yvar, k, fam, rho, basis = "tp",
                           ar_start = NULL, use_participant = TRUE) {
  # xt passes the spline basis used INSIDE the factor smooth (tp or cr);
  # Soskuthy (2021) Tables 1-2 show the best choice is data-dependent, so we
  # fit both and compare by AIC rather than assuming the default tp is best.
  # When use_participant is FALSE (pilot mode: too few speakers to estimate
  # by-participant curves), the participant smooths are dropped and only the
  # by-item random intercept is retained. Results are then DESCRIPTIVE only.
  rand_terms <- "s(item_id, bs = 're')"
  if (use_participant) {
    fs_ref  <- sprintf("s(time_bin, participant, bs = 'fs', m = 1, xt = '%s', k = %d)",
                       basis, k)
    fs_diff <- sprintf("s(time_bin, participant, by = conditionO, bs = 'fs', m = 1, xt = '%s', k = %d)",
                       basis, k)
    rand_terms <- paste(rand_terms, fs_ref, fs_diff, sep = " + ")
  }

  f <- as.formula(paste0(
        yvar, " ~ conditionO",
        " + s(time_bin, k = ", k, ")",
        " + s(time_bin, by = conditionO, k = ", k, ")",
        " + ", rand_terms))

  if (is.null(ar_start)) {
    bam(f, data = df, method = "fREML", discrete = TRUE, family = fam)
  } else {
    bam(f, data = df, method = "fREML", discrete = TRUE, family = fam,
        rho = rho, AR.start = ar_start)
  }
}

# --- helper: binary-smooth companion model (single combined difference test) -
# Soskuthy (2021) Set 4: a BINARY difference smooth folds the height + shape
# difference into ONE smooth, giving a single significance test with the highest
# power and nominal type I error, WITHOUT needing slow ML model comparison. We
# fit it alongside the ordered-factor model so the output can report both the
# combined test (binary) and the split test (ordered factor: height vs shape).
.fit_binary <- function(df, yvar, k, fam, rho, basis, ar_start,
                        use_participant = TRUE) {
  rand_terms <- "s(item_id, bs = 're')"
  if (use_participant) {
    fs_ref  <- sprintf("s(time_bin, participant, bs = 'fs', m = 1, xt = '%s', k = %d)",
                       basis, k)
    fs_diff <- sprintf("s(time_bin, participant, by = conditionO, bs = 'fs', m = 1, xt = '%s', k = %d)",
                       basis, k)
    rand_terms <- paste(rand_terms, fs_ref, fs_diff, sep = " + ")
  }
  # IsB is the 0/1 binary indicator for condition "b"; s(time_bin, by=IsB) is
  # then a non-centred binary difference smooth (combines height + shape).
  f <- as.formula(paste0(
        yvar, " ~ s(time_bin, k = ", k, ")",
        " + s(time_bin, by = IsB, k = ", k, ")",
        " + ", rand_terms))
  bam(f, data = df, method = "fREML", discrete = TRUE, family = fam,
      rho = rho, AR.start = ar_start)
}

fit_contour <- function(df, yvar, k = 25,
                        family = "gaussian",
                        event_cols = c("participant", "stimulus_id"),
                        rho = NULL, verbose = TRUE,
                        compare_basis = TRUE,
                        min_participants = 5) {
  df$item_id     <- droplevels(factor(df$item_id))
  df$participant <- droplevels(factor(df$participant))
  n_part <- nlevels(df$participant)

  # PILOT GATE: by-participant reference/difference factor smooths need enough
  # speakers to be estimable and meaningful. With fewer than min_participants
  # the random difference smooth cannot be separated from the fixed effect, so
  # we DROP the by-participant smooths and keep only the by-item intercept. In
  # that case the model is DESCRIPTIVE (it summarises the speakers you have);
  # its p-values do NOT generalise to the population and must not be reported as
  # inferential. Re-run with the full sample (>= min_participants) for inference.
  use_participant <- (n_part >= min_participants)
  if (verbose && !use_participant) {
    cat(sprintf(paste0(
      "\n*** PILOT MODE [%s]: only %d participant(s) (< %d). ***\n",
      "*** By-participant random smooths DROPPED. Results are DESCRIPTIVE ***\n",
      "*** ONLY -- do not interpret p-values as generalisable inference.   ***\n\n"),
      yvar, n_part, min_participants))
  }

  # Ordered-factor version of condition for the difference smooth (Wieling Sec
  # 4.5.3; Soskuthy 1.2.4). contr.treatment makes the ordered factor behave like
  # a 0/1 dummy: level "a" (present/paroxytone) is the reference, "b" the contrast.
  df$conditionO <- as.ordered(df$condition)
  contrasts(df$conditionO) <- "contr.treatment"
  # Binary 0/1 indicator for the companion binary-smooth model.
  df$IsB <- as.numeric(df$condition == "b")

  # Order rows by time within each token and add a start.event column marking
  # the first sample of every trajectory (needed for the AR(1) model). Requires
  # a "time" column for start_event(); we point it at time_bin.
  # IMPORTANT: coerce to a base data.frame first. start_event() (and order()
  # inside it) can fail with "cannot xtfrm data frames" when handed a tibble,
  # because tibble single-bracket indexing returns a 1-column tibble rather than
  # a vector. as.data.frame() restores base-R column extraction.
  df <- as.data.frame(df)
  df$time <- df$time_bin
  df <- start_event(df, column = "time", event = event_cols, label.event = "Event")

  fam <- if (identical(family, "scat")) mgcv::scat() else family

  # --- Pass 1: fit (no AR) to estimate the lag-1 residual autocorrelation.
  # Done with the default tp basis; rho is then reused for all later fits.
  m0 <- .fit_one_basis(df, yvar, k, fam, rho = NULL, basis = "tp", ar_start = NULL,
                       use_participant = use_participant)
  if (is.null(rho)) {
    # Robust lag-1 rho. Try itsadug's start_value_rho(), then acf_resid(), then
    # a plain base-R ACF of the residuals. Each result is coerced to a single
    # finite number; anything else (NULL, data frame, vector, NA) is treated as
    # a failure and we move to the next method. This avoids the xtfrm/order
    # error some itsadug paths can raise on certain model/data shapes.
    as_scalar <- function(x) {
      x <- suppressWarnings(tryCatch(as.numeric(unlist(x))[1],
                                     error = function(e) NA_real_))
      if (length(x) == 1 && is.finite(x)) x else NA_real_
    }
    rho <- as_scalar(tryCatch(start_value_rho(m0), error = function(e) NA_real_))
    if (!is.finite(rho))
      rho <- as_scalar(tryCatch(acf_resid(m0, plot = FALSE)[2],
                                error = function(e) NA_real_))
    if (!is.finite(rho)) {
      r <- as.numeric(residuals(m0)); r <- r[is.finite(r)]
      rho <- as_scalar(tryCatch(stats::acf(r, lag.max = 1, plot = FALSE)$acf[2],
                                error = function(e) NA_real_))
    }
  }
  if (!is.finite(rho)) rho <- 0     # last-resort guard
  if (verbose) cat(sprintf("[fit_contour:%s] rho (lag-1 acf) = %.3f\n", yvar, rho))

  # --- Pass 2: refit with AR(1) for each basis, then pick the better by AIC.
  # Soskuthy (2021): the optimal factor-smooth basis (tp vs cr) is data-
  # dependent (his F2 favoured cr, his pitch favoured tp), so we fit both and
  # let conditional AIC decide rather than hard-coding one.
  # In pilot mode there are no factor smooths, so tp and cr are identical;
  # skip the redundant second fit.
  bases <- if (compare_basis && use_participant) c("tp", "cr") else "tp"
  fits  <- list()
  for (b in bases) {
    if (verbose) cat(sprintf("[fit_contour:%s] fitting basis '%s'...\n", yvar, b))
    fits[[b]] <- .fit_one_basis(df, yvar, k, fam, rho = rho, basis = b,
                                ar_start = df$start.event,
                                use_participant = use_participant)
  }
  # AIC() can return a 1-row data frame for some model classes; force a plain
  # numeric scalar per fit so order()/min() work on a numeric vector.
  aic_vals <- vapply(fits, function(mm) {
    a <- tryCatch(AIC(mm), error = function(e) NA_real_)
    as.numeric(a)[1]
  }, numeric(1))
  ord <- order(aic_vals)
  aic_tab <- data.frame(
    basis = names(fits)[ord],
    AIC   = unname(aic_vals[ord]),
    row.names = NULL,
    stringsAsFactors = FALSE)
  aic_tab$dAIC <- aic_tab$AIC - min(aic_tab$AIC, na.rm = TRUE)
  best_basis <- aic_tab$basis[1]
  m <- fits[[best_basis]]
  if (verbose) {
    cat(sprintf("[fit_contour:%s] basis comparison (lower AIC = better):\n", yvar))
    print(aic_tab, row.names = FALSE)
    cat(sprintf("[fit_contour:%s] selected basis: '%s'\n", yvar, best_basis))
  }

  # --- Companion binary-smooth model (same basis), for the combined test.
  m_bin <- tryCatch(
    .fit_binary(df, yvar, k, fam, rho, best_basis, df$start.event,
                use_participant = use_participant),
    error = function(e) { if (verbose) cat("  binary-smooth fit failed:",
                                           conditionMessage(e), "\n"); NULL })

  # --- k diagnostic (Wieling Sec 4.3; Soskuthy 4): the gam.check p-value is
  # unreliable for choosing k (esp. pitch), so we report EDF vs k' directly and
  # flag any smooth whose EDF is within 5% of k' (raise k and refit). k.check()
  # returns a matrix with columns k', edf, k-index, p-value (the backtick names
  # get mangled by as.data.frame, so we read columns 1-2 positionally).
  kdiag <- tryCatch({
    kc <- mgcv::k.check(m)              # matrix: cols = k', edf, k-index, p-value
    kp   <- kc[, 1]                     # k'
    kedf <- kc[, 2]                     # edf
    data.frame(
      term        = rownames(kc),
      k_prime     = kp,
      edf         = kedf,
      k_index     = kc[, 3],
      p_value     = kc[, 4],
      flag_raise_k = (kedf / kp) > 0.95,
      row.names   = NULL,
      stringsAsFactors = FALSE)
  }, error = function(e) NULL)
  if (verbose && !is.null(kdiag)) {
    cat(sprintf("[fit_contour:%s] k diagnostics (watch edf vs k_prime):\n", yvar))
    print(kdiag, row.names = FALSE)
    if (any(kdiag$flag_raise_k, na.rm = TRUE))
      cat("  NOTE: a smooth has EDF close to k' -- consider raising k and refitting.\n")
  }

  # --- Population-level predictions per condition (for the contour panel).
  # We use itsadug::get_predictions(rm.ranef=TRUE) rather than a raw
  # predict(exclude=, newdata.guaranteed=TRUE) call: the latter can fail with
  # "NA/NaN argument" (an internal dk$nr indexing error) when excluding bs="re"
  # / "fs" terms, whereas get_predictions() cancels random effects via the same
  # robust path get_difference() uses (already working in diff_ms()).
  rng    <- range(df$time_bin, na.rm = TRUE)
  grid_t <- seq(rng[1], rng[2], length.out = 200)
  all_labels <- vapply(m$smooth, function(s) s$label, character(1))
  excl <- all_labels[grepl("participant|item_id", all_labels)]

  pred_one <- function(lev) {
    pp <- get_predictions(
      m,
      cond = list(time_bin = grid_t,
                  conditionO = factor(lev, levels = levels(df$conditionO),
                                      ordered = TRUE)),
      rm.ranef = TRUE, se = TRUE, print.summary = FALSE)
    data.frame(
      time_bin  = pp$time_bin,
      condition = lev,
      fit       = pp$fit,
      se        = pp$CI / 1.96,            # itsadug returns CI half-width (1.96*SE)
      lower     = pp$fit - pp$CI,
      upper     = pp$fit + pp$CI,
      stringsAsFactors = FALSE)
  }
  nd <- rbind(pred_one("a"), pred_one("b"))
  nd$condition <- factor(nd$condition, levels = c("a", "b"))

  list(model = m, pred = nd, rho = rho, family = family,
       basis = best_basis, basis_aic = aic_tab,
       model_binary = m_bin, kdiag = kdiag,
       diff_smooth = fit_diff_label(m),
       n_participants = n_part, pilot = !use_participant,
       excl = excl)
}

# Difference (a - b) over time. Uses get_difference() from itsadug, which reads
# the difference straight out of the fitted model -- correctly accounting for
# the covariance between the two conditions -- rather than subtracting two
# independent predictions and adding their SEs in quadrature (which assumes
# independence and is what the tutorial warns against in Sec 4.5 / 4.10).
# `comp` is the present-vs-past contrast; grouping smooths are excluded so the
# difference is the population-level (excl. random) contrast.
diff_ms <- function(fit) {
  m   <- fit$model
  rng <- range(m$model$time_bin, na.rm = TRUE)
  grid_t <- seq(rng[1], rng[2], length.out = 200)

  # The contrast now lives entirely in the ordered factor conditionO (carrying
  # the difference smooth + the parametric height offset). The old plain-factor
  # `condition` random slope was dropped in favour of the participant difference
  # smooth, so we vary conditionO alone. rm.ranef=TRUE drops all grouping-level
  # terms, giving the population-level (excl. random) present-vs-past difference.
  d <- get_difference(m,
        comp = list(conditionO = c("a", "b")),
        cond = list(time_bin = grid_t),
        rm.ranef = TRUE,
        print.summary = FALSE)

  # itsadug returns columns: difference, CI (half-width), time_bin, ...
  tibble(time_bin = d$time_bin,
         diff     = d$difference,
         se_diff  = d$CI / 1.96,
         lower    = d$difference - d$CI,
         upper    = d$difference + d$CI) %>%
    arrange(time_bin) %>%
    mutate(significant = (lower > 0) | (upper < 0))
}

## ---- 1.6b APA-style extraction + formatting helpers -------------------
# Pull the parametric (height) and smooth (shape) difference tests out of an
# ordered-factor GAMM, and the combined test out of the binary-smooth companion.
# Returns a tidy data frame of term-level statistics plus pre-formatted APA
# strings, so the render section can both tabulate and narrate the results.

fmt_p <- function(p) {
  # APA p-value formatting: "< .001" or "= .034" (no leading zero).
  if (is.na(p)) return("= NA")
  if (p < .001) return("< .001")
  paste0("= ", sub("^0", "", formatC(p, format = "f", digits = 3)))
}

# Round-trip a number to an APA-style string with a fixed number of decimals.
fmt_n <- function(x, d = 2) formatC(x, format = "f", digits = d)

# Extract term-level stats from one fitted contour model (ordered-factor form).
apa_terms <- function(fit, measure_label, unit_label) {
  m  <- fit$model
  sm <- summary(m)

  # Parametric height difference: the conditionO coefficient (b - a offset).
  # Row name in p.table is "conditionO.L" or "conditionOb" depending on coding.
  pcoef <- sm$p.table
  hrow  <- grep("conditionO", rownames(pcoef))
  height <- if (length(hrow) == 1) {
    data.frame(
      measure = measure_label,
      term    = "Height difference (parametric: past - present)",
      estimate = pcoef[hrow, "Estimate"],
      se       = pcoef[hrow, "Std. Error"],
      stat     = pcoef[hrow, 3],          # t (gaussian) or z-like
      stat_name = colnames(pcoef)[3],
      edf      = NA_real_, ref_df = NA_real_,
      p        = pcoef[hrow, 4],
      stringsAsFactors = FALSE)
  } else NULL

  # Smooth (shape) difference: the s(time_bin):conditionO... row in s.table.
  stab  <- sm$s.table
  srow  <- grep("conditionO", rownames(stab))
  srow  <- srow[!grepl("participant", rownames(stab)[srow])]  # exclude random
  shape <- if (length(srow) >= 1) {
    data.frame(
      measure = measure_label,
      term    = "Shape difference (smooth: present vs past contour)",
      estimate = NA_real_, se = NA_real_,
      stat     = stab[srow[1], "F"],
      stat_name = "F",
      edf      = stab[srow[1], "edf"],
      ref_df   = stab[srow[1], "Ref.df"],
      p        = stab[srow[1], "p-value"],
      stringsAsFactors = FALSE)
  } else NULL

  out <- do.call(rbind, list(height, shape))

  # Combined (binary-smooth) test, if the companion model fitted.
  if (!is.null(fit$model_binary)) {
    sb <- summary(fit$model_binary)$s.table
    brow <- grep("IsB", rownames(sb))
    if (length(brow) == 1) {
      combined <- data.frame(
        measure = measure_label,
        term    = "Combined difference (binary smooth: any present-vs-past diff)",
        estimate = NA_real_, se = NA_real_,
        stat     = sb[brow, "F"],
        stat_name = "F",
        edf      = sb[brow, "edf"],
        ref_df   = sb[brow, "Ref.df"],
        p        = sb[brow, "p-value"],
        stringsAsFactors = FALSE)
      out <- rbind(out, combined)
    }
  }
  attr(out, "unit") <- unit_label
  out
}

# Turn one row of apa_terms() into an APA-style inline result string.
apa_sentence <- function(row) {
  if (!is.na(row$edf)) {
    # smooth term: F(edf, ref_df) = ..., p ...
    sprintf("%s: F(%s, %s) = %s, p %s",
            row$term,
            fmt_n(row$edf, 2), fmt_n(row$ref_df, 2),
            fmt_n(row$stat, 2), fmt_p(row$p))
  } else {
    # parametric term: b = ..., SE = ..., t = ..., p ...
    sprintf("%s: b = %s, SE = %s, %s = %s, p %s",
            row$term,
            fmt_n(row$estimate, 2), fmt_n(row$se, 2),
            row$stat_name, fmt_n(row$stat, 2), fmt_p(row$p))
  }
}

# ---- 1.7 website-style plot builders ----------------------------------
# layer of dashed syllable dividers + top labels, shared by all panels.
# On the per-syllable index, dividers fall at the integers 0..5 and labels sit
# at the midpoints; we also set integer x-breaks so the axis reads as syllables.
add_syll_dividers <- function(p, label_y) {
  p +
    geom_vline(xintercept = divider_x, linetype = "dashed",
               color = "gray55", linewidth = 0.4) +
    annotate("text", x = syll_bounds$mid, y = label_y,
             label = syll_bounds$syll_tag, fontface = "bold", size = 3) +
    scale_x_continuous(breaks = 0:5,
                       labels = c("0", "1", "2", "3", "4", "5"),
                       expand = expansion(mult = 0.01))
}

# CONTOUR panel (purple/green smooths + ribbons, syllable dividers)
contour_panel <- function(pred, ylab, title) {
  ytop <- max(pred$upper, na.rm = TRUE)
  p <- ggplot(pred, aes(time_bin, fit, color = condition, fill = condition)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
    scale_fill_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
    labs(title = title, x = "Syllable position (Art=0-1, N-s1=1-2, N-s2=2-3, V-s1=3-4, V-s2=4-5)", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  add_syll_dividers(p, label_y = ytop + 0.06 * abs(ytop) + 0.5)
}

# DIFFERENCE panel (black line, gray CI, red significant dots)
difference_panel <- function(dd, ylab, title) {
  ytop <- max(dd$upper, na.rm = TRUE)
  p <- ggplot(dd, aes(time_bin, diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "gray60") +
    geom_line(linewidth = 1) +
    geom_point(data = dd %>% filter(significant),
               color = "red", size = 1.4) +
    labs(title = title, x = "Syllable position (Art=0-1, N-s1=1-2, N-s2=2-3, V-s1=3-4, V-s2=4-5)", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  add_syll_dividers(p, label_y = ytop + 0.10 * abs(ytop) + 0.3)
}

# ---- 1.8 F0 contour + difference (semitones) --------------------------
# F0 residuals are prone to heavy tails (octave/halving tracking errors), so
# we fit the scaled-t family here (Sec 4.6). Switch to "gaussian" if gam.check
# shows the residuals are already well-behaved and you want the faster fit.
f0_dat  <- bin_ms(praat_voiced %>% filter(!is.na(f0_st)))
f0_fit  <- fit_contour(f0_dat, "f0_st", k = 25, family = "scat")
f0_diff <- diff_ms(f0_fit)
p_f0_contour <- contour_panel(f0_fit$pred,
                              ylab = "F0 (semitones re 100 Hz)",
                              title = "2-Syllable Nouns: F0 Contour (GAMM)")
p_f0_diff <- difference_panel(f0_diff,
                              ylab = expression(Delta~"F0 (semitones)"),
                              title = "F0 Difference (Paroxytone - Oxytone)")
fig_f0 <- p_f0_contour / p_f0_diff +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    tag_levels = "A",
    title = "GAMM Smoothed: F0",
    subtitle = "F0 in semitones | Red dots = significant differences (95% CI)",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "gray40")))

# ---- 1.9 Intensity contour + difference (dB) --------------------------
int_dat  <- bin_ms(praat_clean %>% filter(!is.na(intensity_db)))
int_fit  <- fit_contour(int_dat, "intensity_db", k = 25)
int_diff <- diff_ms(int_fit)
p_int_contour <- contour_panel(int_fit$pred,
                               ylab = "Intensity (dB)",
                               title = "2-Syllable Nouns: Intensity Contour (GAMM)")
p_int_diff <- difference_panel(int_diff,
                               ylab = expression(Delta~"Intensity (dB)"),
                               title = "Intensity Difference (Paroxytone - Oxytone)")
fig_int <- p_int_contour / p_int_diff +
  plot_layout(heights = c(2, 1)) +
  plot_annotation(
    tag_levels = "A",
    title = "GAMM Smoothed: Intensity",
    subtitle = "Intensity in dB | Red dots = significant differences (95% CI)",
    theme = theme(plot.title = element_text(face = "bold", size = 14),
                  plot.subtitle = element_text(color = "gray40")))

# ---- 1.10 per-syllable tables (duration / F0 / intensity) -------------
syll_level <- praat_clean %>%
  group_by(file_stem, condition, item_id, stimulus_id,
           word_num, word_type, syll_tag, syllable_num, syllable_pos,
           syll_dur_ms) %>%
  summarise(mean_f0_hz     = mean(f0_hz, na.rm = TRUE),
            mean_f0_st     = mean(f0_st, na.rm = TRUE),
            mean_intensity = mean(intensity_db, na.rm = TRUE),
            .groups = "drop")

# order the syllable tag as a factor following the utterance
syll_order <- as.character(syll_bounds$syll_tag)
syll_level <- syll_level %>%
  mutate(syll_tag = factor(syll_tag, levels = syll_order))

by_syllable <- syll_level %>%
  group_by(syll_tag, condition) %>%
  summarise(n = n(),
            dur_ms    = mean(syll_dur_ms, na.rm = TRUE),
            dur_se    = sd(syll_dur_ms, na.rm = TRUE) / sqrt(n()),
            f0_st     = mean(mean_f0_st, na.rm = TRUE),
            intensity = mean(mean_intensity, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(syll_tag, condition)

by_syllable_diff <- by_syllable %>%
  select(syll_tag, condition, dur_ms, f0_st, intensity) %>%
  pivot_wider(names_from = condition,
              values_from = c(dur_ms, f0_st, intensity)) %>%
  mutate(dur_diff_ms   = dur_ms_a - dur_ms_b,
         f0_diff_st     = f0_st_a - f0_st_b,
         intensity_diff = intensity_a - intensity_b)

by_word <- syll_level %>%
  group_by(word_type, condition) %>%
  summarise(n_syll = n(),
            dur_ms    = mean(syll_dur_ms, na.rm = TRUE),
            f0_st     = mean(mean_f0_st, na.rm = TRUE),
            intensity = mean(mean_intensity, na.rm = TRUE),
            .groups = "drop") %>%
  arrange(word_type, condition)

# ---- 1.11 DURATION per-syllable plot (matching style) -----------------
p_duration <- ggplot(by_syllable,
                     aes(syll_tag, dur_ms, fill = condition)) +
  geom_col(position = position_dodge(0.7), width = 0.62) +
  geom_errorbar(aes(ymin = dur_ms - dur_se, ymax = dur_ms + dur_se),
                position = position_dodge(0.7), width = 0.2) +
  scale_fill_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
  labs(title = "Duration by Syllable (2-Syllable Nouns)",
       subtitle = "Mean +/- SE",
       x = "Syllable", y = "Duration (ms)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"))

# ---- 1.12 individual-sentence F0 plots (semitones, syllable dividers) -
items_both <- praat_voiced %>% distinct(item_id, condition) %>%
  count(item_id) %>% filter(n == 2) %>% pull(item_id)
set.seed(42)
sample_items <- sample(items_both, min(3, length(items_both)))

plot_item <- function(it) {
  d <- praat_voiced %>% filter(item_id == it, !is.na(f0_st))
  noun_lab <- d %>% filter(word_type == "noun") %>% pull(word_label) %>%
    unique() %>% .[1]
  ytop <- max(d$f0_st, na.rm = TRUE)
  # per-item syllable boundaries (averaged over its two conditions)
  ib <- d %>% distinct(condition, syll_tag, syll_start_ms, syll_end_ms) %>%
    group_by(syll_tag) %>%
    summarise(start = mean(syll_start_ms), end = mean(syll_end_ms),
              .groups = "drop") %>% arrange(start) %>%
    mutate(mid = (start + end) / 2)
  ggplot(d, aes(time_ms, f0_st, color = condition)) +
    geom_vline(xintercept = c(ib$start, max(ib$end)),
               linetype = "dashed", color = "gray60", linewidth = 0.4) +
    annotate("text", x = ib$mid, y = ytop + 0.6,
             label = ib$syll_tag, size = 2.6, fontface = "bold") +
    geom_line(linewidth = 1) +
    scale_color_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
    labs(title = paste0("Item ", it, ": ", noun_lab),
         x = "Time (ms)", y = "F0 (semitones)") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"))
}
fig_items <- if (length(sample_items) > 0) {
  wrap_plots(lapply(sample_items, plot_item), ncol = 1) +
    plot_annotation(
      title = "Individual Sentences: 2-Syllable Nouns",
      subtitle = "Purple = Paroxytone (present), Green = Oxytone (past)",
      theme = theme(plot.title = element_text(face = "bold", size = 14),
                    plot.subtitle = element_text(color = "gray40")))
} else NULL

# ---- 1.13 semitone / perceptibility checks (verb & noun regions) ------
st_diff <- function(df) {
  m <- df %>% group_by(condition) %>%
    summarise(f0 = mean(f0_hz, na.rm = TRUE), .groups = "drop")
  a <- m$f0[m$condition == "a"]; b <- m$f0[m$condition == "b"]
  c(present_hz = a, past_hz = b, diff_st = 12 * log2(a / b))
}
verb_st <- st_diff(praat_voiced %>% filter(word_type == "verb"))
noun_st <- st_diff(praat_voiced %>% filter(word_type == "noun"))
perceptibility <- tibble(
  region     = c("verb", "noun"),
  present_hz = c(verb_st["present_hz"], noun_st["present_hz"]),
  past_hz    = c(verb_st["past_hz"],    noun_st["past_hz"]),
  diff_st    = c(verb_st["diff_st"],    noun_st["diff_st"]),
  audible    = abs(c(verb_st["diff_st"], noun_st["diff_st"])) > 1)

# significant-window summaries (on the per-syllable index, 0-5)
sig_window <- function(d) {
  s <- d %>% filter(significant)
  if (nrow(s) == 0) return(tibble(start_idx = NA, end_idx = NA, mean_diff = NA))
  tibble(start_idx = min(s$time_bin), end_idx = max(s$time_bin),
         mean_diff = round(mean(s$diff), 2))
}
f0_sig  <- sig_window(f0_diff)
int_sig <- sig_window(int_diff)


## ========================== PART 2: RENDER ==========================
## Everything is printed and saved here, at the very end.

cat("\n\n################# RESULTS #################\n")
cat("Input file:", input_path, "\n")
cat(sprintf("Items kept (2-syllable nouns): %d of %d\n", n_items_kept, n_items_total))
cat(sprintf("Rows: %d total, %d voiced (%.1f%%)\n",
            nrow(praat_clean), nrow(praat_voiced),
            100 * nrow(praat_voiced) / nrow(praat_clean)))

cat("\n=== Mean syllable boundaries (ms) ===\n");           print(syll_bounds)
cat("\n=== Per-syllable measures (dur / F0 st / intensity) ===\n"); print(by_syllable, n = Inf)
cat("\n=== Per-syllable a-vs-b differences ===\n");          print(by_syllable_diff, n = Inf, width = Inf)
cat("\n=== Per-word (whole-word) measures ===\n");           print(by_word, n = Inf)
cat("\n=== Perceptibility (JND ~ 1 st) ===\n");              print(perceptibility)
cat("\n=== Significant F0 difference window (ms) ===\n");    print(f0_sig)
cat("\n=== Significant intensity difference window (ms) ===\n"); print(int_sig)

cat("\n=== F0 GAMM summary ===\n")
cat(sprintf("(family = %s, factor-smooth basis = %s, AR(1) rho = %.3f)\n",
            f0_fit$family, f0_fit$basis, f0_fit$rho))
print(summary(f0_fit$model))
cat("\n=== Intensity GAMM summary ===\n")
cat(sprintf("(family = %s, factor-smooth basis = %s, AR(1) rho = %.3f)\n",
            int_fit$family, int_fit$basis, int_fit$rho))
print(summary(int_fit$model))

# Model criticism (Sec 4.6): residual QQ/histogram + heteroscedasticity, plus
# the k-index table. NB gam.check residuals are UNcorrected (they ignore rho),
# so the scatter plots will still look "spaghetti-like" -- that's expected.
# What matters is whether the QQ plot / histogram look approximately normal
# (Gaussian) or t-distributed (scat). If F0's QQ shows heavy tails under
# gaussian, keep family="scat" (already the default for F0 above).
png("plots/gamcheck_F0.png", width = 9, height = 8, units = "in", res = 150)
par(mfrow = c(2, 2)); gam.check(f0_fit$model); dev.off()
png("plots/gamcheck_Intensity.png", width = 9, height = 8, units = "in", res = 150)
par(mfrow = c(2, 2)); gam.check(int_fit$model); dev.off()
cat("\nModel-criticism plots saved to plots/gamcheck_F0.png and plots/gamcheck_Intensity.png\n")

## ================= APA-STYLE RESULTS (for write-up) =================
## Side-by-side reporting of (a) the ordered-factor split tests -- height
## (parametric) and shape (smooth) -- and (b) the combined binary-smooth test.
## Soskuthy (2021) Set 4: reading the two split p-values as a single "any
## difference?" test inflates type I error, so we also print a Bonferroni
## threshold (.025) for the split tests and recommend the binary smooth (or
## Bonferroni) when the hypothesis is simply "do the conditions differ at all".

f0_terms  <- apa_terms(f0_fit,  "F0 (semitones)",  "st")
int_terms <- apa_terms(int_fit, "Intensity (dB)",  "dB")
apa_all   <- rbind(f0_terms, int_terms)

# significant-window text from the difference curves (visual/illustrative).
# Translates a 0-5 per-syllable index value into a readable "syllable (pct%)".
idx_to_syll <- function(x) {
  labs <- c("Art", "N-s1", "N-s2", "V-s1", "V-s2")
  slot <- pmin(floor(x), 4)                 # 0..4
  pct  <- round((x - slot) * 100)
  sprintf("%s (%d%%)", labs[slot + 1], pct)
}
win_txt <- function(sig, unit) {
  if (is.na(sig$start_idx)) return("no interval where the 95% CI excluded zero")
  sprintf("%s to %s on the syllable axis (mean difference %.2f %s)",
          idx_to_syll(sig$start_idx), idx_to_syll(sig$end_idx),
          sig$mean_diff, unit)
}

apa_path <- "tables/APA_results.txt"
con <- file(apa_path, open = "wt")
emit <- function(...) { cat(..., "\n", sep = ""); cat(..., "\n", sep = "", file = con) }

emit("================ APA-STYLE GAMM RESULTS ================")
if (isTRUE(f0_fit$pilot) || isTRUE(int_fit$pilot)) {
  emit("")
  emit("!!! PILOT MODE: fewer participants than the threshold for inference. !!!")
  emit("!!! By-participant random smooths were dropped, so these results are !!!")
  emit("!!! DESCRIPTIVE only. The p-values below summarise the speakers in   !!!")
  emit("!!! hand and do NOT generalise. Re-run with the full sample before   !!!")
  emit("!!! reporting any inferential claim.                                 !!!")
  emit(sprintf("!!! N participants: F0 = %d, Intensity = %d.",
               f0_fit$n_participants, int_fit$n_participants))
  emit("")
}
emit("Study: Spanish stress/tense contrast (present/paroxytone 'a' vs ",
     "past/oxytone 'b')")
emit("Analysis: GAMM contour models over a per-syllable time index (0-5;")
  emit("  Art=0-1, N-s1=1-2, N-s2=2-3, V-s1=3-4, V-s2=4-5), 2-syllable nouns.")
emit("N participants = ", nlevels(factor(praat_clean$participant)),
     "; N items kept = ", n_items_kept, " of ", n_items_total, ".")
emit("")
emit("--- Model specification (both measures) ---")
emit("Fitted with mgcv::bam(), fREML, discretised covariates. Each model: an")
emit("ordered-factor parametric term (condition) + a reference smooth s(time) +")
emit("an ordered-factor difference smooth s(time, by=conditionO), plus by-item")
emit("random intercepts and by-participant random REFERENCE and DIFFERENCE")
emit("factor smooths (Soskuthy 2021, within-item recommendation). An AR(1)")
emit("error model corrected residual autocorrelation.")
emit("  F0 model:        family = ", f0_fit$family,
     ", factor-smooth basis = ", f0_fit$basis,
     ", AR(1) rho = ", fmt_n(f0_fit$rho, 3))
emit("  Intensity model: family = ", int_fit$family,
     ", factor-smooth basis = ", int_fit$basis,
     ", AR(1) rho = ", fmt_n(int_fit$rho, 3))
emit("")
emit("--- Basis selection (lower AIC = better) ---")
emit("F0:")
emit(paste(utils::capture.output(print(f0_fit$basis_aic, row.names = FALSE)),
           collapse = "\n"))
emit("Intensity:")
emit(paste(utils::capture.output(print(int_fit$basis_aic, row.names = FALSE)),
           collapse = "\n"))
emit("")

emit("--- Results, side by side (Soskuthy split vs combined test) ---")
for (meas in unique(apa_all$measure)) {
  emit("")
  emit("### ", meas)
  rows <- apa_all[apa_all$measure == meas, ]
  for (i in seq_len(nrow(rows))) emit("  - ", apa_sentence(rows[i, ]))
  # interpretive note: Bonferroni threshold for the split tests
  split_ps <- rows$p[grepl("Height|Shape", rows$term)]
  any_split_sig_bonf <- any(split_ps < .025, na.rm = TRUE)
  comb_p <- rows$p[grepl("Combined", rows$term)]
  emit("  Note: for a single 'any difference?' claim, use the combined binary-")
  emit("  smooth test", if (length(comb_p)) paste0(" (p ", fmt_p(comb_p), ")") else "",
       " or Bonferroni-correct the two split tests at alpha = .025")
  emit("  (split tests ", if (any_split_sig_bonf) "remain" else "do NOT remain",
       " significant under Bonferroni).")
}
emit("")
emit("--- Illustrative difference windows (visual; NOT a standalone test) ---")
emit("Soskuthy (2021) Set 4: pointwise 'CI excludes zero' regions are for")
emit("illustration only; significance claims rest on the tests above.")
emit("  F0 difference curve excluded zero across: ", win_txt(f0_sig, "st"))
emit("  Intensity difference curve excluded zero across: ", win_txt(int_sig, "dB"))
emit("")
emit("--- Ready-to-adapt METHODS paragraph ---")
emit("F0 and intensity contours were analysed with generalized additive mixed")
emit("models (GAMMs) using the mgcv package (Wood, 2017) in R, following the")
emit("strategies evaluated by Soskuthy (2021) and Wieling (2018). Each contour")
emit("measure was modelled over a per-syllable normalised time index (each")
  emit("syllable mapped to one unit so boundaries align across tokens) with a")
  emit("reference smooth and an")
emit("ordered-factor difference smooth contrasting the present/paroxytone and")
emit("past/oxytone conditions. The random-effects structure comprised by-item")
emit("random intercepts and by-participant random reference and difference")
emit("factor smooths (penalised thin-plate/cubic-regression bases, m = 1),")
emit("which capture participant-specific contour shapes and participant-specific")
emit("realisations of the condition contrast. Residual autocorrelation was")
emit("addressed with a first-order autoregressive (AR1) error model, with rho")
emit("set to the lag-1 residual autocorrelation of an otherwise identical model.")
emit("Models were estimated with fast restricted maximum likelihood (fREML) and")
emit("discretised covariates. The factor-smooth basis (thin-plate vs cubic")
emit("regression) was selected by AIC. Heavy-tailed F0 residuals were")
emit("accommodated with a scaled-t family. The presence of a condition")
emit("difference was tested both via a combined binary difference smooth and via")
emit("separate tests of the parametric (height) and smooth (shape) difference")
emit("terms; for the latter, a Bonferroni-corrected alpha of .025 was used.")
emit("")
emit("================ END ================")
close(con)

# machine-readable term table for tables/
apa_out <- apa_all
apa_out$p_apa <- vapply(apa_out$p, fmt_p, character(1))
write_csv(apa_out, "tables/apa_gamm_terms.csv")
cat("\nAPA results written to ", apa_path,
    " and tables/apa_gamm_terms.csv\n", sep = "")


# write tables
write_csv(by_syllable,      "tables/measures_by_syllable.csv")
write_csv(by_syllable_diff, "tables/measures_by_syllable_diff.csv")
write_csv(by_word,          "tables/measures_by_word.csv")
write_csv(perceptibility,   "tables/perceptibility_semitones.csv")

# show plots in the Plots pane (use the arrows to flip through) ...
print(fig_f0)
print(fig_int)
print(p_duration)
if (!is.null(fig_items)) print(fig_items)

# ... and save them
ggsave("plots/F0_contour_difference.png",        fig_f0,    width = 11, height = 8,  dpi = 300, bg = "white")
ggsave("plots/Intensity_contour_difference.png", fig_int,   width = 11, height = 8,  dpi = 300, bg = "white")
ggsave("plots/Duration_by_syllable.png",         p_duration, width = 9,  height = 5,  dpi = 300, bg = "white")
if (!is.null(fig_items))
  ggsave("plots/Individual_sentences.png", fig_items, width = 9,
         height = 3 * length(sample_items), dpi = 300, bg = "white")

# save models
save(f0_fit, int_fit, f0_diff, int_diff,
     by_syllable, by_syllable_diff, by_word, perceptibility,
     syll_bounds, apa_all, file = "models/f0_intensity_models.RData")

cat("\nDone. Plots shown in the Plots pane and saved to plots/.\n")
cat("Tables in tables/ (incl. APA_results.txt + apa_gamm_terms.csv), models in models/.\n")
