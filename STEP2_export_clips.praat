# ============================================================
# STEP 2 — EXPORT LABELED INTERVALS TO WAV
# ============================================================
# WHAT IT DOES:
#   - Writes each non-empty interval of the TextGrid out as its own WAV,
#     named  {participant}_{list}_{label}.wav  e.g. P03_L1_01a.wav
#   - Hard cap of 180 files per run; if more non-empty intervals exist,
#     it stops and warns rather than writing extras.
#
# HOW TO RUN:
#   1. Select BOTH the Sound and the TextGrid in the Objects list
#      (click one, Cmd-click the other).
#   2. Open this script, Run > Run (Cmd-R).
#   3. Pick the same participant and list you used in STEP1.
#
# SET THIS ONCE:
#   - Edit the Output_folder default below to your real 2_cut_audios path.
#   - Keep the trailing slash. Copy the path via Finder:
#     right-click the folder, hold Option, "Copy ... as Pathname".
# ============================================================

form Step 2 - Export labeled intervals
    optionmenu Participant 3
        option P01
        option P02
        option P03
        option P04
        option P05
        option P06
        option P07
        option P08
        option P09
        option P10
        option P11
        option P12
    optionmenu List 1
        option L1
        option L2
    sentence Output_folder /Users/kaylee/Spanish_F0_study_audios/2_cut_audios/
endform

max_clips = 180
prefix$ = participant$ + "_" + list$

sound = selected("Sound")
textgrid = selected("TextGrid")

selectObject: textgrid
n = Get number of intervals: 1

count = 0
for i to n
    selectObject: textgrid
    label$ = Get label of interval: 1, i
    if label$ <> ""
        if count >= max_clips
            appendInfoLine: "WARNING: hit ", max_clips, "-file cap. Interval ", i, " (label ", label$, ") and any after were NOT exported."
            goto DONE
        endif
        start = Get start time of interval: 1, i
        end = Get end time of interval: 1, i
        selectObject: sound
        clip = Extract part: start, end, "rectangular", 1, "no"
        selectObject: clip
        Save as WAV file: output_folder$ + prefix$ + "_" + label$ + ".wav"
        Remove
        count = count + 1
    endif
endfor
label DONE

appendInfoLine: "Exported ", count, " files to ", output_folder$
