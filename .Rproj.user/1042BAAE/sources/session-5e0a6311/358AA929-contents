# Spanish stress/tense acoustic analysis: GAMMs (F0, intensity contours)
# and GLMMs (N-s2 anticipation). a = present/paroxytone, b = past/oxytone.
# List = presentation order (L1=1, L2=2).

library(tidyverse)
library(readxl)
library(mgcv)
library(itsadug)
library(patchwork)
library(lme4)

# Pick the Praat output file if not already set
if (!exists("input_path")) input_path <- file.choose()

# Read the Praat tab-separated output, handling BOM/encoding and a stray title row
read_praat_output <- function(path) {
  praat_cols <- c("file_stem","condition","item_num","word_num","word_type",
                  "word_label","syllable_num","syllable_pos","syll_label",
                  "syll_dur_ms","syll_start_ms","syll_end_ms","point",
                  "time_ms","time_norm","f0_hz","intensity_db","is_voiced")
  ext <- tolower(tools::file_ext(path))
  if (ext %in% c("xlsx","xls")) {
    raw <- readxl::read_excel(path, col_names = FALSE)
    hdr_row <- which(apply(raw, 1, function(r) any(r == "file_stem", na.rm = TRUE)))[1]
    if (is.na(hdr_row)) hdr_row <- 1
    dat <- raw[(hdr_row + 1):nrow(raw), , drop = FALSE]
  } else {
    con <- file(path, "rb"); bom <- readBin(con, "raw", 4); close(con)
    file_enc <- "UTF-8"
    if (length(bom) >= 2) {
      if (bom[1] == as.raw(0xff) && bom[2] == as.raw(0xfe)) file_enc <- "UTF-16LE"
      else if (bom[1] == as.raw(0xfe) && bom[2] == as.raw(0xff)) file_enc <- "UTF-16BE"
    }
    con <- file(path, "r", encoding = file_enc)
    all_lines <- readLines(con, warn = FALSE); close(con)
    if (length(all_lines) == 0) stop("Empty Praat output file.")
    all_lines[1] <- enc2utf8(all_lines[1])
    all_lines[1] <- gsub("\ufeff", "", all_lines[1], useBytes = FALSE)
    all_lines[1] <- gsub("[^[:print:]\t]", "", all_lines[1])
    hdr_idx <- which(grepl("file_stem", all_lines, fixed = TRUE))[1]
    data_lines <- if (is.na(hdr_idx)) all_lines else all_lines[(hdr_idx + 1):length(all_lines)]
    data_lines <- data_lines[nzchar(data_lines)]
    if (length(data_lines) == 0) stop("No data rows after header.")
    dat <- readr::read_tsv(I(data_lines), col_names = FALSE, show_col_types = FALSE, na = c("","NA"))
  }
  if (ncol(dat) != length(praat_cols))
    stop(sprintf("Expected %d columns, found %d.", length(praat_cols), ncol(dat)))
  colnames(dat) <- praat_cols
  dat
}

# Load and tidy, deriving order from the list field (L1/L2) in the filename
praat_clean <- read_praat_output(input_path) %>%
  mutate(
    item_num = as.integer(item_num), word_num = as.integer(word_num),
    syllable_num = as.integer(syllable_num),
    syll_dur_ms = as.numeric(syll_dur_ms),
    syll_start_ms = as.numeric(syll_start_ms), syll_end_ms = as.numeric(syll_end_ms),
    point = as.integer(point), time_ms = as.numeric(time_ms), time_norm = as.numeric(time_norm),
    f0_hz = suppressWarnings(as.numeric(na_if(as.character(f0_hz), "NA"))),
    intensity_db = suppressWarnings(as.numeric(na_if(as.character(intensity_db), "NA"))),
    is_voiced = as.integer(is_voiced),
    condition = factor(condition, levels = c("a","b")),
    word_type = factor(word_type, levels = c("article","noun","verb")),
    syllable_pos = factor(syllable_pos, levels = c("only","antepenult","penult","final","pre3"))
  ) %>%
  mutate(
    f0_st = ifelse(!is.na(f0_hz) & f0_hz > 0, 12 * log2(f0_hz / 100), NA_real_),
    item_id = item_num,
    participant = sub("^(P[0-9]+)_.*$", "\\1", file_stem),
    list = factor(sub("^P[0-9]+_(L[12])_.*$", "\\1", file_stem), levels = c("L1","L2")),
    order = factor(ifelse(list == "L1", "first", "second"), levels = c("first","second")),
    order_c = ifelse(list == "L1", -0.5, 0.5),
    stimulus_id = paste0(item_num, "_", condition),
    syll_tag = case_when(
      word_type == "article" ~ "Art",
      word_type == "noun"    ~ paste0("N-s", syllable_num),
      word_type == "verb"    ~ paste0("V-s", syllable_num), TRUE ~ NA_character_)
  ) %>%
  mutate(
    syll_pos_idx = case_when(
      syll_tag == "Art" ~ 0L, syll_tag == "N-s1" ~ 1L, syll_tag == "N-s2" ~ 2L,
      syll_tag == "V-s1" ~ 3L, syll_tag == "V-s2" ~ 4L, TRUE ~ NA_integer_),
    within_syll = case_when(
      is.na(syll_start_ms) | is.na(syll_end_ms) ~ NA_real_,
      (syll_end_ms - syll_start_ms) <= 0 ~ 0.5,
      TRUE ~ pmin(pmax((time_ms - syll_start_ms) / (syll_end_ms - syll_start_ms), 0), 1)),
    time_idx = syll_pos_idx + within_syll
  )

# Restrict to two-syllable-noun items
noun_syll_count <- praat_clean %>% filter(word_type == "noun") %>%
  group_by(item_id) %>% summarise(n = n_distinct(syllable_num), .groups = "drop")
items_2syll <- noun_syll_count %>% filter(n == 2) %>% pull(item_id)
n_items_total <- n_distinct(praat_clean$item_id)
praat_clean <- praat_clean %>% filter(item_id %in% items_2syll)
n_items_kept <- n_distinct(praat_clean$item_id)
praat_voiced <- praat_clean %>% filter(!is.na(f0_hz) & f0_hz > 0)

# Aesthetics and syllable boundary layout for plots
cond_cols <- c("a" = "#882255", "b" = "#117733")
cond_labs <- c("Paroxytone (present)", "Oxytone (past)")
syll_levels_idx <- c("Art","N-s1","N-s2","V-s1","V-s2")
syll_bounds <- tibble(syll_tag = factor(syll_levels_idx, levels = syll_levels_idx),
                      start = 0:4, end = 1:5, mid = (0:4) + 0.5)
divider_x <- 0:5

# Bin the per-syllable time index for the discrete GAMM fitter
bin_idx <- function(df, col = "time_idx", width = 0.02) {
  df[["time_bin"]] <- round(df[[col]] / width) * width; df
}

# Fit one contour GAMM with a chosen factor-smooth basis; order is a parametric covariate
fit_one_basis <- function(df, yvar, k, fam, rho, basis = "tp", ar_start = NULL, use_participant = TRUE) {
  rand_terms <- "s(item_id, bs = 're')"
  if (use_participant) {
    fs_ref  <- sprintf("s(time_bin, participant, bs = 'fs', m = 1, xt = '%s', k = %d)", basis, k)
    fs_diff <- sprintf("s(time_bin, participant, by = conditionO, bs = 'fs', m = 1, xt = '%s', k = %d)", basis, k)
    rand_terms <- paste(rand_terms, fs_ref, fs_diff, sep = " + ")
  }
  f <- as.formula(paste0(yvar, " ~ conditionO + order_c + s(time_bin, k = ", k,
                         ") + s(time_bin, by = conditionO, k = ", k, ") + ", rand_terms))
  if (is.null(ar_start)) bam(f, data = df, method = "fREML", discrete = TRUE, family = fam)
  else bam(f, data = df, method = "fREML", discrete = TRUE, family = fam, rho = rho, AR.start = ar_start)
}

# Fit the contour model: estimate AR(1) rho, refit per basis, pick lowest AIC
fit_contour <- function(df, yvar, k = 25, family = "gaussian",
                        event_cols = c("participant","stimulus_id"),
                        compare_basis = TRUE, min_participants = 5) {
  df$item_id <- droplevels(factor(df$item_id))
  df$participant <- droplevels(factor(df$participant))
  n_part <- nlevels(df$participant)
  use_participant <- (n_part >= min_participants)
  df$conditionO <- as.ordered(df$condition)
  contrasts(df$conditionO) <- "contr.treatment"
  df <- as.data.frame(df)
  df$time <- df$time_bin
  df <- start_event(df, column = "time", event = event_cols, label.event = "Event")
  fam <- if (identical(family, "scat")) mgcv::scat() else family
  m0 <- fit_one_basis(df, yvar, k, fam, rho = NULL, basis = "tp", ar_start = NULL, use_participant = use_participant)
  as_scalar <- function(x) {
    x <- suppressWarnings(tryCatch(as.numeric(unlist(x))[1], error = function(e) NA_real_))
    if (length(x) == 1 && is.finite(x)) x else NA_real_
  }
  rho <- as_scalar(tryCatch(start_value_rho(m0), error = function(e) NA_real_))
  if (!is.finite(rho)) rho <- as_scalar(tryCatch(acf_resid(m0, plot = FALSE)[2], error = function(e) NA_real_))
  if (!is.finite(rho)) {
    r <- as.numeric(residuals(m0)); r <- r[is.finite(r)]
    rho <- as_scalar(tryCatch(stats::acf(r, lag.max = 1, plot = FALSE)$acf[2], error = function(e) NA_real_))
  }
  if (!is.finite(rho)) rho <- 0
  bases <- if (compare_basis && use_participant) c("tp","cr") else "tp"
  fits <- list()
  for (b in bases)
    fits[[b]] <- fit_one_basis(df, yvar, k, fam, rho = rho, basis = b,
                               ar_start = df$start.event, use_participant = use_participant)
  aic_vals <- vapply(fits, function(mm) as.numeric(tryCatch(AIC(mm), error = function(e) NA_real_))[1], numeric(1))
  best_basis <- names(fits)[order(aic_vals)][1]
  m <- fits[[best_basis]]
  rng <- range(df$time_bin, na.rm = TRUE)
  grid_t <- seq(rng[1], rng[2], length.out = 200)
  pred_one <- function(lev) {
    pp <- get_predictions(m, cond = list(time_bin = grid_t,
            conditionO = factor(lev, levels = levels(df$conditionO), ordered = TRUE)),
            rm.ranef = TRUE, se = TRUE, print.summary = FALSE)
    data.frame(time_bin = pp$time_bin, condition = lev, fit = pp$fit,
               lower = pp$fit - pp$CI, upper = pp$fit + pp$CI, stringsAsFactors = FALSE)
  }
  nd <- rbind(pred_one("a"), pred_one("b"))
  nd$condition <- factor(nd$condition, levels = c("a","b"))
  list(model = m, pred = nd, rho = rho, family = family, basis = best_basis,
       n_participants = n_part, pilot = !use_participant)
}

# Population-level present-vs-past difference curve over time
diff_curve <- function(fit) {
  m <- fit$model
  rng <- range(m$model$time_bin, na.rm = TRUE)
  grid_t <- seq(rng[1], rng[2], length.out = 200)
  d <- get_difference(m, comp = list(conditionO = c("a","b")),
                      cond = list(time_bin = grid_t), rm.ranef = TRUE, print.summary = FALSE)
  tibble(time_bin = d$time_bin, diff = d$difference,
         lower = d$difference - d$CI, upper = d$difference + d$CI) %>%
    arrange(time_bin) %>% mutate(significant = (lower > 0) | (upper < 0))
}

# Shared dashed syllable dividers and labels for contour plots
add_syll_dividers <- function(p, label_y) {
  p + geom_vline(xintercept = divider_x, linetype = "dashed", color = "gray55", linewidth = 0.4) +
    annotate("text", x = syll_bounds$mid, y = label_y, label = syll_bounds$syll_tag,
             fontface = "bold", size = 3) +
    scale_x_continuous(breaks = 0:5, labels = c("0","1","2","3","4","5"), expand = expansion(mult = 0.01))
}

# Contour panel: condition smooths with ribbons
contour_panel <- function(pred, ylab, title) {
  ytop <- max(pred$upper, na.rm = TRUE)
  p <- ggplot(pred, aes(time_bin, fit, color = condition, fill = condition)) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.25, color = NA) +
    geom_line(linewidth = 1.1) +
    scale_color_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
    scale_fill_manual(values = cond_cols, labels = cond_labs, name = "Condition") +
    labs(title = title, x = "Syllable position", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(), plot.title = element_text(face = "bold"),
          axis.title = element_text(face = "bold"))
  add_syll_dividers(p, label_y = ytop + 0.06 * abs(ytop) + 0.5)
}

# Difference panel: black line, gray CI, red significant points
difference_panel <- function(dd, ylab, title) {
  ytop <- max(dd$upper, na.rm = TRUE)
  p <- ggplot(dd, aes(time_bin, diff)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3, fill = "gray60") +
    geom_line(linewidth = 1) +
    geom_point(data = dd %>% filter(significant), color = "red", size = 1.4) +
    labs(title = title, x = "Syllable position", y = ylab) +
    theme_minimal(base_size = 12) +
    theme(panel.grid.minor = element_blank(), panel.grid.major.x = element_blank(),
          plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"))
  add_syll_dividers(p, label_y = ytop + 0.10 * abs(ytop) + 0.3)
}

# Fit F0 contour GAMM and build its figure
f0_fit  <- fit_contour(bin_idx(praat_voiced %>% filter(!is.na(f0_st))), "f0_st", k = 25, family = "scat")
f0_diff <- diff_curve(f0_fit)
fig_f0 <- contour_panel(f0_fit$pred, "F0 (semitones re 100 Hz)", "2-Syllable Nouns: F0 Contour (GAMM)") /
  difference_panel(f0_diff, expression(Delta~"F0 (st)"), "F0 Difference (Paroxytone - Oxytone)") +
  plot_layout(heights = c(2,1)) +
  plot_annotation(tag_levels = "A", title = "GAMM Smoothed: F0",
                  subtitle = "Red dots = significant (95% CI)",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

# Fit intensity contour GAMM and build its figure
int_fit  <- fit_contour(bin_idx(praat_clean %>% filter(!is.na(intensity_db))), "intensity_db", k = 25)
int_diff <- diff_curve(int_fit)
fig_int <- contour_panel(int_fit$pred, "Intensity (dB)", "2-Syllable Nouns: Intensity Contour (GAMM)") /
  difference_panel(int_diff, expression(Delta~"Intensity (dB)"), "Intensity Difference (Paroxytone - Oxytone)") +
  plot_layout(heights = c(2,1)) +
  plot_annotation(tag_levels = "A", title = "GAMM Smoothed: Intensity",
                  subtitle = "Red dots = significant (95% CI)",
                  theme = theme(plot.title = element_text(face = "bold", size = 14)))

# One row per N-s2 token with order parsed from list
ns2_tok <- praat_clean %>%
  filter(syll_tag == "N-s2") %>%
  group_by(file_stem, participant, item_id, condition, order, order_c) %>%
  summarise(dur_ms = first(syll_dur_ms),
            intensity = mean(intensity_db, na.rm = TRUE),
            f0_st = mean(f0_st, na.rm = TRUE), .groups = "drop") %>%
  mutate(f0_st = ifelse(is.nan(f0_st), NA, f0_st),
         condition = factor(condition, levels = c("a","b")),
         participant = factor(participant), item_id = factor(item_id))

# Fit inclusive and restricted GLMMs with maximal-to-intercepts fallback; order is a fixed covariate
ranef_ladder <- c("(1 + condition | participant) + (1 + condition | item_id)",
                  "(1 + condition | participant) + (1 | item_id)",
                  "(1 | participant) + (1 + condition | item_id)",
                  "(1 | participant) + (1 | item_id)")
fit_pair <- function(dv, dat) {
  d <- dat[!is.na(dat[[dv]]), ]
  ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 3e5))
  chosen <- NA; incl <- NULL
  for (rr in ranef_ladder) {
    m <- suppressWarnings(suppressMessages(try(
      lmer(as.formula(paste0(dv, " ~ condition + order_c + ", rr)), data = d,
           REML = FALSE, control = ctrl), silent = TRUE)))
    if (!inherits(m, "try-error") && !lme4::isSingular(m, tol = 1e-4)) { chosen <- rr; incl <- m; break }
  }
  if (is.null(incl)) {
    chosen <- ranef_ladder[length(ranef_ladder)]
    incl <- suppressWarnings(suppressMessages(
      lmer(as.formula(paste0(dv, " ~ condition + order_c + ", chosen)), data = d, REML = FALSE, control = ctrl)))
  }
  restr <- suppressWarnings(suppressMessages(
    lmer(as.formula(paste0(dv, " ~ order_c + ", chosen)), data = d, REML = FALSE, control = ctrl)))
  list(inclusive = incl, restricted = restr, structure = chosen)
}

# Fit the three N-s2 GLMMs
dur_pair <- fit_pair("dur_ms", ns2_tok)
int_pair <- fit_pair("intensity", ns2_tok)
f0_pair  <- fit_pair("f0_st", ns2_tok)

# Fit the N-s2 F0 contour-shape GAMM over within-syllable time
ns2_pts <- praat_voiced %>%
  filter(syll_tag == "N-s2", !is.na(f0_st)) %>%
  mutate(participant = factor(participant), item_id = factor(item_id),
         conditionO = ordered(condition, levels = c("a","b")), t = within_syll) %>%
  as.data.frame()
contrasts(ns2_pts$conditionO) <- "contr.treatment"
use_part <- nlevels(ns2_pts$participant) >= 5
rand <- "s(item_id, bs='re')"
if (use_part) rand <- paste(rand, "s(t, participant, bs='fs', m=1, k=8)",
                            "s(t, participant, by=conditionO, bs='fs', m=1, k=8)", sep = " + ")
m_f0_shape <- bam(as.formula(paste0("f0_st ~ conditionO + order_c + s(t, k=8) + s(t, by=conditionO, k=8) + ", rand)),
                  data = ns2_pts, method = "fREML", discrete = TRUE)

# N-s2 duration partial-pooling plot: per-participant means plus pooled estimate
dur_incl <- dur_pair$inclusive
raw_pp <- ns2_tok %>% group_by(participant, condition) %>%
  summarise(m = mean(dur_ms, na.rm = TRUE), .groups = "drop")
nd <- expand.grid(condition = factor(c("a","b"), levels = c("a","b")), order_c = 0)
nd$fit <- predict(dur_incl, newdata = nd, re.form = NA)
p_dur <- ggplot() +
  geom_line(data = raw_pp, aes(condition, m, group = participant), color = "gray70", linewidth = 0.5) +
  geom_point(data = raw_pp, aes(condition, m, color = condition), alpha = 0.5, size = 1.8) +
  geom_line(data = nd, aes(as.numeric(condition), fit), color = "black", linewidth = 1.1) +
  geom_point(data = nd, aes(condition, fit), color = "black", size = 3) +
  scale_color_manual(values = cond_cols, guide = "none") +
  scale_x_discrete(labels = cond_labs) +
  labs(title = "N-s2 Duration: per-participant means + pooled estimate",
       x = NULL, y = "N-s2 duration (ms)") +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), axis.title = element_text(face = "bold"),
        panel.grid.major.x = element_blank(), panel.grid.minor = element_blank())

# N-s2 F0 contour-shape plot over within-syllable time
rng <- range(ns2_pts$t, na.rm = TRUE); gt <- seq(rng[1], rng[2], length.out = 100)
pred_shape <- function(lev) {
  pp <- get_predictions(m_f0_shape, cond = list(t = gt,
          conditionO = factor(lev, levels = levels(ns2_pts$conditionO), ordered = TRUE)),
          rm.ranef = TRUE, se = TRUE, print.summary = FALSE)
  data.frame(t = pp$t, condition = lev, fit = pp$fit, lower = pp$fit - pp$CI, upper = pp$fit + pp$CI)
}
f0shape_pred <- rbind(pred_shape("a"), pred_shape("b"))
f0shape_pred$condition <- factor(f0shape_pred$condition, levels = c("a","b"))
p_f0shape <- ggplot(f0shape_pred, aes(t, fit, color = condition, fill = condition)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.20, color = NA) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = cond_cols, labels = cond_labs, name = "Upcoming verb") +
  scale_fill_manual(values = cond_cols, labels = cond_labs, name = "Upcoming verb") +
  scale_x_continuous(breaks = c(0,.5,1), labels = c("start","mid","end")) +
  labs(title = "N-s2 F0 Contour Shape by Upcoming Verb Tense",
       x = "Within-syllable position (N-s2)", y = "F0 (semitones re 100 Hz)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold"),
        axis.title = element_text(face = "bold"), panel.grid.minor = element_blank())

# Print run info
cat("\n================= RESULTS =================\n")
cat("Input:", input_path, "\n")
cat(sprintf("Participants: %d | Items kept (2-syll nouns): %d of %d | N-s2 tokens: %d\n",
            nlevels(factor(praat_clean$participant)), n_items_kept, n_items_total, nrow(ns2_tok)))
cat(sprintf("Order coding: L1 = first (-0.5), L2 = second (+0.5)\n"))

# Print F0 contour GAMM summary
cat("\n========== F0 CONTOUR GAMM ==========\n")
cat(sprintf("family=%s basis=%s rho=%.3f pilot=%s\n", f0_fit$family, f0_fit$basis, f0_fit$rho, f0_fit$pilot))
print(summary(f0_fit$model))

# Print intensity contour GAMM summary
cat("\n========== INTENSITY CONTOUR GAMM ==========\n")
cat(sprintf("family=%s basis=%s rho=%.3f pilot=%s\n", int_fit$family, int_fit$basis, int_fit$rho, int_fit$pilot))
print(summary(int_fit$model))

# Print N-s2 duration GLMM: nested model comparison then full summary
cat("\n========== N-s2 DURATION GLMM ==========\n")
cat("structure:", dur_pair$structure, "\n")
print(anova(dur_pair$restricted, dur_pair$inclusive))
print(summary(dur_pair$inclusive))

# Print N-s2 intensity GLMM
cat("\n========== N-s2 INTENSITY GLMM ==========\n")
cat("structure:", int_pair$structure, "\n")
print(anova(int_pair$restricted, int_pair$inclusive))
print(summary(int_pair$inclusive))

# Print N-s2 F0-mean GLMM
cat("\n========== N-s2 F0 MEAN GLMM ==========\n")
cat("structure:", f0_pair$structure, "\n")
print(anova(f0_pair$restricted, f0_pair$inclusive))
print(summary(f0_pair$inclusive))

# Print N-s2 F0 contour-shape GAMM
cat("\n========== N-s2 F0 SHAPE GAMM ==========\n")
print(summary(m_f0_shape))

# Print all plots to the pane
print(fig_f0)
print(fig_int)
print(p_dur)
print(p_f0shape)

save(praat_clean, ns2_tok, f0_fit, int_fit, dur_pair, int_pair, f0_pair,
     m_f0_shape, fig_f0, fig_int, p_dur, p_f0shape,
     file = "report_objects.RData")
