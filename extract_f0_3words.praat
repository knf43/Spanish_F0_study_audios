################################################################
# extract_f0_3words.praat
#
# Extracts per-syllable acoustic measures (F0, intensity, duration)
# for the FIRST THREE WORDS (article, noun, verb) of each sentence,
# sampling F0 + intensity at 10 points per syllable.
#
# INPUT  : a folder of MFA-aligned TextGrids (tiers:
#          1=sentence, 2=words, 3=syllables, 4=phones) and the
#          matching mono 16 kHz .wav files (same base name).
# OUTPUT : one tab-separated file (open directly in Excel) with the
#          18 columns the R analysis script expects:
#
#   file_stem, condition, item_num, word_num, word_type,
#   word_label, syllable_num, syllable_pos, syll_label,
#   syll_dur_ms, syll_start_ms, syll_end_ms, point, time_ms,
#   time_norm, f0_hz, intensity_db, is_voiced
#
# Filename convention (REQUIRED): P##_L#_##[ab]...  e.g.
#   P01_L1_01a_syllabified  ->  participant P01, list 1, item 1,
#   variant a (a = present, b = past). condition column = a or b.
#
# HOW TO RUN
#   1. Open Praat.
#   2. Praat menu > Open Praat script... > choose this file.
#   3. In the script window: Run > Run  (Cmd-R).
#   4. Fill in the form (folders + pitch floor/ceiling), click OK.
#   5. When done, open the output .txt in Excel (it is tab-separated).
#
# NOTES
#   * F0 is sampled at 10 equally-spaced time points inside each
#     syllable (point 1..10). Unvoiced points get f0_hz = "NA" and
#     is_voiced = 0, matching the format the R script cleans up.
#   * time_norm is the within-syllable normalized time (point k of 10
#     -> k/10 rounded to 4 dp), as in the reference dataset.
#   * Only the first three words (article/noun/verb) are exported.
#   * Pitch floor/ceiling: defaults 75-400 Hz suit a typical adult.
#     Narrow them to the speaker for cleaner tracks if needed.
################################################################

form Extract F0/intensity/duration per syllable (first 3 words)
    comment Folder with the *_syllabified.TextGrid files:
    text textgrid_folder /Users/kayleefernandez/Spanish_F0_study_audios/fa_work/syllabified
    comment Folder with the matching .wav files:
    text wav_folder /Users/kayleefernandez/Spanish_F0_study_audios/fa_work/standardized
    comment Output file (tab-separated, open in Excel):
    text output_file /Users/kayleefernandez/Spanish_F0_study_audios/praat_output_3_words.txt
    comment Pitch analysis settings:
    positive pitch_floor_Hz 75
    positive pitch_ceiling_Hz 400
    comment Points sampled per syllable:
    positive points_per_syllable 10
    comment TextGrid filename suffix (without .TextGrid):
    text tg_suffix _syllabified
    comment WAV filename suffix (without .wav), often _standardized or empty:
    text wav_suffix
endform

# ---- tier numbers in the MFA TextGrids ----
words_tier = 2
syll_tier  = 3

# ---- write header ----
header$ = "file_stem" + tab$ + "condition" + tab$ + "item_num" + tab$
header$ = header$ + "word_num" + tab$ + "word_type" + tab$ + "word_label" + tab$
header$ = header$ + "syllable_num" + tab$ + "syllable_pos" + tab$ + "syll_label" + tab$
header$ = header$ + "syll_dur_ms" + tab$ + "syll_start_ms" + tab$ + "syll_end_ms" + tab$
header$ = header$ + "point" + tab$ + "time_ms" + tab$ + "time_norm" + tab$
header$ = header$ + "f0_hz" + tab$ + "intensity_db" + tab$ + "is_voiced"
writeFileLine: output_file$, header$

# ---- gather TextGrid files ----
Create Strings as file list: "tglist", textgrid_folder$ + "/*" + tg_suffix$ + ".TextGrid"
n_files = Get number of strings
writeInfoLine: "Found ", n_files, " TextGrid files. Processing..."

n_written = 0
for f from 1 to n_files
    selectObject: "Strings tglist"
    tg_name$ = Get string: f

    # base stem = filename without the _syllabified.TextGrid part
    stem$ = tg_name$ - ".TextGrid"
    base$ = stem$ - tg_suffix$

    # ---- parse condition + item from the base name (P##_L#_##[ab]) ----
    # find the variant letter: the [ab] right after the item digits.
    # We locate the pattern _<digits><letter> using index_regex.
    variant$ = ""
    cond$ = ""
    item_num = 0
    # extract the item+variant chunk after the last underscore-digit group
    # Robust approach: scan for the last occurrence of _NN a/b
    # Use a regex to capture digits then a or b.
    startPos = index_regex(base$, "_[0-9]+[ab]")
    if startPos > 0
        rest$ = mid$(base$, startPos + 1, length(base$))
        # rest$ now starts with the digits then the letter (maybe more after)
        # pull leading digits
        digits$ = ""
        i = 1
        ch$ = mid$(rest$, i, 1)
        while index("0123456789", ch$) > 0
            digits$ = digits$ + ch$
            i = i + 1
            ch$ = mid$(rest$, i, 1)
        endwhile
        item_num = number(digits$)
        variant$ = mid$(rest$, i, 1)
    endif
    if variant$ = "a"
        cond$ = "a"
    elsif variant$ = "b"
        cond$ = "b"
    else
        cond$ = variant$
    endif

    # ---- locate and open the matching wav ----
    wav_path$ = wav_folder$ + "/" + base$ + wav_suffix$ + ".wav"
    if not fileReadable(wav_path$)
        # try without suffix
        wav_path$ = wav_folder$ + "/" + base$ + ".wav"
    endif

    if fileReadable(wav_path$)
        Read from file: textgrid_folder$ + "/" + tg_name$
        tg = selected("TextGrid")
        Read from file: wav_path$
        snd = selected("Sound")

        # build Pitch and Intensity once per file
        selectObject: snd
        To Pitch: 0, pitch_floor_Hz, pitch_ceiling_Hz
        pitch = selected("Pitch")
        selectObject: snd
        To Intensity: pitch_floor_Hz, 0, "yes"
        intens = selected("Intensity")

        # ---- iterate the first three NON-EMPTY words ----
        selectObject: tg
        n_word_int = Get number of intervals: words_tier
        word_count = 0
        for wi from 1 to n_word_int
            selectObject: tg
            wlab$ = Get label of interval: words_tier, wi
            if wlab$ <> ""
                word_count = word_count + 1
                if word_count <= 3
                    word_num = word_count
                    if word_num = 1
                        wtype$ = "article"
                    elsif word_num = 2
                        wtype$ = "noun"
                    else
                        wtype$ = "verb"
                    endif

                    w_start = Get start time of interval: words_tier, wi
                    w_end   = Get end time of interval: words_tier, wi

                    # ---- collect syllables whose midpoint falls in this word ----
                    n_syll_int = Get number of intervals: syll_tier
                    # first count this word's syllables (for syllable_pos)
                    n_word_sylls = 0
                    for si from 1 to n_syll_int
                        slab$ = Get label of interval: syll_tier, si
                        if slab$ <> ""
                            s_start = Get start time of interval: syll_tier, si
                            s_end   = Get end time of interval: syll_tier, si
                            s_mid = (s_start + s_end) / 2
                            if s_mid >= w_start and s_mid <= w_end
                                n_word_sylls = n_word_sylls + 1
                            endif
                        endif
                    endfor

                    # second pass: emit each syllable with its position label
                    syll_idx = 0
                    for si from 1 to n_syll_int
                        selectObject: tg
                        slab$ = Get label of interval: syll_tier, si
                        if slab$ <> ""
                            s_start = Get start time of interval: syll_tier, si
                            s_end   = Get end time of interval: syll_tier, si
                            s_mid = (s_start + s_end) / 2
                            if s_mid >= w_start and s_mid <= w_end
                                syll_idx = syll_idx + 1

                                # syllable_pos from position within the word
                                # counting from the END (penult/final logic)
                                from_end = n_word_sylls - syll_idx
                                if n_word_sylls = 1
                                    spos$ = "only"
                                elsif from_end = 0
                                    spos$ = "final"
                                elsif from_end = 1
                                    spos$ = "penult"
                                elsif from_end = 2
                                    spos$ = "antepenult"
                                else
                                    spos$ = "pre3"
                                endif

                                s_dur_ms   = (s_end - s_start) * 1000
                                s_start_ms = s_start * 1000
                                s_end_ms   = s_end * 1000

                                # ---- sample F0 + intensity at N points ----
                                for p from 1 to points_per_syllable
                                    # equally spaced points inside the syllable
                                    frac = (p - 0.5) / points_per_syllable
                                    t = s_start + frac * (s_end - s_start)
                                    t_ms = t * 1000
                                    tnorm = number(fixed$(p / points_per_syllable, 4))

                                    selectObject: pitch
                                    f0 = Get value at time: t, "Hertz", "linear"
                                    selectObject: intens
                                    intdb = Get value at time: t, "cubic"

                                    if f0 = undefined or f0 <= 0
                                        f0$ = "NA"
                                        voiced = 0
                                    else
                                        f0$ = fixed$(f0, 2)
                                        voiced = 1
                                    endif
                                    if intdb = undefined
                                        int$ = "NA"
                                    else
                                        int$ = fixed$(intdb, 2)
                                    endif

                                    row$ = base$ + tab$ + cond$ + tab$ + string$(item_num) + tab$
                                    row$ = row$ + string$(word_num) + tab$ + wtype$ + tab$ + wlab$ + tab$
                                    row$ = row$ + string$(syll_idx) + tab$ + spos$ + tab$ + slab$ + tab$
                                    row$ = row$ + fixed$(s_dur_ms, 2) + tab$ + fixed$(s_start_ms, 2) + tab$
                                    row$ = row$ + fixed$(s_end_ms, 2) + tab$ + string$(p) + tab$
                                    row$ = row$ + fixed$(t_ms, 2) + tab$ + fixed$(tnorm, 4) + tab$
                                    row$ = row$ + f0$ + tab$ + int$ + tab$ + string$(voiced)
                                    appendFileLine: output_file$, row$
                                    n_written = n_written + 1
                                endfor
                            endif
                        endif
                    endfor
                endif
            endif
        endfor

        # cleanup per-file objects
        selectObject: pitch
        plusObject: intens
        plusObject: snd
        plusObject: tg
        Remove
    else
        appendInfoLine: "WAV not found for ", base$, " (looked for ", wav_path$, ") - skipped"
    endif
endfor

selectObject: "Strings tglist"
Remove

appendInfoLine: "Done. Wrote ", n_written, " rows to:"
appendInfoLine: output_file$
