# Running the cut-audio alignment protocol

This is everything you need to align your cut sentence audios, get 4-tier
TextGrids (sentence / words / syllables / phones), and a QC report.

There is **one** script: `align_cut_audios.py`. You do **not** need the old
01-07 pipeline scripts, the `libs/` folder, diarization, or Whisper.

Your setup (confirmed):
- The script and this file live inside
  `/Users/kayleefernandez/Spanish_F0_study_audios/`
- The cut wavs are in the `2_cut_audios` subfolder, named like
  `P01_L1_01a.wav`.

---

## 1. What you need before starting

- The folder of cut `.wav` files (`2_cut_audios`).
- The `aligner` conda environment with Montreal Forced Aligner installed.
- Two small Python packages: `praatio` and `soundfile`.

You do **not** need to create any folders by hand. The script creates its own
working directory (`fa_work/`) and all subfolders automatically.

---

## 2. One-time setup

Open Terminal and activate your environment:

    conda activate aligner

Your prompt should now start with `(aligner)` instead of `(base)`. If it
still says `(base)`, the activate did not take - run it again.

Move into the study folder:

    cd /Users/kayleefernandez/Spanish_F0_study_audios

Install the two helper packages (only needed once):

    pip install praatio soundfile

The standardize step uses ffmpeg if it is installed (recommended). To check:

    ffmpeg -version

If ffmpeg is missing, the script falls back to a built-in converter using
soundfile/numpy, so it still works either way. To install ffmpeg in this env:

    conda install -c conda-forge ffmpeg

Confirm MFA is reachable (should print a version, not an error):

    mfa --version

If `mfa --version` fails, you are not in the right environment - re-run
`conda activate aligner`.

---

## 3. Quick check before the full run

Confirm the wavs are visible from where you are:

    ls 2_cut_audios/*.wav | head

You should see a list like `2_cut_audios/P01_L1_01a.wav`, etc. If you instead
see `no matches found` or `No such file or directory`, stop - the path is
wrong (see the note about hidden spaces in section 8).

---

## 4. Run everything in one go

From inside `Spanish_F0_study_audios`, run:

    python align_cut_audios.py --audio-dir 2_cut_audios --all

That runs all steps in order:

0. **standardize** - converts every wav to mono, 16 kHz (MFA's preferred
   format). Uses ffmpeg if installed, otherwise a built-in fallback.
1. **corpus**    - reads each filename, writes the known sentence as a `.lab`
2. **mfa**       - forced-aligns audio to text with the bundled `spanish_mfa`
3. **syllabify** - builds the 4-tier TextGrids from MFA's phone timings
4. **table**     - writes per-file target-verb syllable boundaries (CSV)
5. **qc**        - checks every file against the approved syllable key

If your audio is already mono 16 kHz, or you want to skip conversion, add
`--no-standardize` to use the originals as-is.

The MFA step is the slow part and may take a while across all your files. The
first time, MFA may also pause to load the Spanish model. Watch for the five
`[STEP n]` lines printing in order.

---

## 5. Where the output goes

Everything lands in a new folder called `fa_work/` inside
`Spanish_F0_study_audios` (next to the script). You can change its location
with `--work-dir /some/path` if you want.

    fa_work/
      standardized/                mono 16 kHz copies (from step 0)
      corpus/                      wav + .lab pairs fed to MFA
      aligned/                     raw MFA TextGrids (words + phones)
      syllabified/                 *** the 4-tier TextGrids you open in Praat ***
      target_verb_syllables.csv    one row per file: verb syllable boundaries
      stress_report.csv            per-file stressed-syllable summary
      qc_report.csv                one row per file, with a "flags" column

The TextGrids you actually work with are in `fa_work/syllabified/`, named like
`P01_L1_01a_syllabified.TextGrid`. Open these in Praat together with the
matching `.wav`.

---

## 6. Reading the QC report (`qc_report.csv`)

Open it in Excel or R. The key column is **`flags`**:

- **Blank** = the file passed the automatic checks (right number of
  syllables, stress on the expected syllable, sensible durations).
- **Not blank** = the file is worth opening in Praat. The flag text tells you
  what looked wrong and which word it is in, e.g.:
  - `verb_syllable_count_mismatch(verb=busco,got 1,exp 2)` - MFA split the
    target verb differently than expected. **These matter most** - they touch
    your stress measurement.
  - `stress_syllable_short(...)` - the stressed syllable is suspiciously
    short, often a sign of a misplaced boundary.
  - `duration_gap(...)` - the aligned span is far from the audio length.
  - `total_syllable_count_mismatch(...)` - the whole sentence came out with a
    different syllable count (often a non-target word like alioli; less
    critical).

Workflow: sort or filter by `flags`, open the flagged files in Praat first,
then spot-check a handful of clean ones to confirm boundaries look right.

---

## 7. Running one step at a time (optional)

If you want to run steps individually instead of `--all`:

    python align_cut_audios.py --audio-dir 2_cut_audios --step standardize
    python align_cut_audios.py --audio-dir 2_cut_audios --step corpus
    python align_cut_audios.py --audio-dir 2_cut_audios --step mfa
    python align_cut_audios.py --audio-dir 2_cut_audios --step syllabify
    python align_cut_audios.py --audio-dir 2_cut_audios --step table
    python align_cut_audios.py --audio-dir 2_cut_audios --step qc

Each step reads the previous step's output from `fa_work/`, so run them in
order the first time. After that you can re-run, say, just `qc` without
re-aligning.

---

## 8. Important things to know

- **The alignment timing comes entirely from MFA (step 2).** Everything else
  groups or checks MFA's output; nothing invents timings.
- **The QC report points you to files; it does not certify correctness.** On
  your first real run, open 3-4 TextGrids in Praat and check that the target
  verb's final vowel boundary (the `a` vs `o`/`ó` contrast) sits where you
  would place it by hand.
- **Filename convention is baked in:** `a` = present, `b` = past, for both
  lists; the target verb is word 3. If any file is named differently, the
  script will skip it and tell you in the step-1 output.
- **alioli (item 5)** is recorded in the key as a-li-o-li (hiatus). If MFA
  treats it as a diphthong, those files get a `total_syllable_count_mismatch`
  flag - expected and harmless, since it's the object noun, not the target.
- **Hidden spaces in folder names break paths.** We hit this: the folder was
  named `Spanish_F0_study_audios ` with a trailing space, which made every
  typed path fail with "No such file or directory" even though Finder showed
  it fine. It is now fixed. If a path ever fails again but the folder clearly
  exists, check for a stray space with:
  `find /Users/kayleefernandez -maxdepth 4 -name "2_cut_audios" -type d`

---

## 9. If something fails

- `mfa: command not found` -> you're not in the `aligner` env. Run
  `conda activate aligner`.
- `praatio not found` / `No module named soundfile` -> run
  `pip install praatio soundfile` inside the `aligner` env.
- `no matches found` / `No such file or directory` on the audio path -> you
  are not inside `Spanish_F0_study_audios`, or the folder name has a hidden
  space. `cd` into the folder first, then run `ls 2_cut_audios/*.wav | head`.
- A few files fail MFA alignment -> MFA prints which ones; they'll be missing
  from `aligned/`. Re-check those clips for clipping or silence.
