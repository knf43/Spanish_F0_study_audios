# ============================================================
# STEP 1 — PRE-SEGMENT AND LABEL
# ============================================================
# WHAT IT DOES:
#   - Run with a single long recording (e.g. P03_L1) selected in the Objects list.
#   - Auto-places boundaries using silence detection and labels each
#     speech interval following the naming protocol.
#   - List 1 order: a then b  ->  01a, 01b, 02a, 02b, ...
#   - List 2 order: b then a  ->  01b, 01a, 02b, 02a, ...   (recording opens with b)
#   - Capped at 90 items / 180 clips per list. Extra detected intervals
#     beyond that are left blank and flagged in the Info window.
#   - Opens the Sound + TextGrid editor so you can review/fix boundaries.
#
# HOW TO RUN:
#   1. Click your Sound object in the Objects list to select it.
#   2. Open this script (Praat > Open Praat script...), then Run > Run (Cmd-R).
#   3. Pick the participant and list in the dialog.
#   4. Review boundaries in the editor that opens.
#   5. If you split/merge/delete intervals, run STEP1b to re-clean the names.
#   6. Then run STEP2 to export.
# ============================================================

form Step 1 - Pre-segment and label
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
    comment === Silence detection settings ===
    real Silence_threshold_dB -25
    real Min_silence_duration 0.3
    real Min_sounding_duration 0.2
    comment === Labeling ===
    integer Start_item_number 1
endform

# Each list = 90 items x (a/b) = 180 clips. Hard cap.
last_item = 90

# List 1: order is a then b. List 2: order is b then a.
if list$ = "L2"
    first_suffix$ = "b"
    second_suffix$ = "a"
else
    first_suffix$ = "a"
    second_suffix$ = "b"
endif

prefix$ = participant$ + "_" + list$

sound = selected("Sound")

selectObject: sound
textgrid = To TextGrid (silences): 100, 0, silence_threshold_dB, min_silence_duration, min_sounding_duration, "", "sounding"
Rename: prefix$

@relabel: textgrid, start_item_number, last_item, first_suffix$, second_suffix$

selectObject: sound, textgrid
View & Edit

procedure relabel: .tg, .startnum, .lastitem, .first$, .second$
    selectObject: .tg
    .n = Get number of intervals: 1
    .item = .startnum
    .ab = 1
    .clips = 0
    .overflow = 0
    for .i to .n
        .label$ = Get label of interval: 1, .i
        if .label$ <> ""
            if .item > .lastitem
                Set interval text: 1, .i, ""
                .overflow = .overflow + 1
            else
                if .item < 10
                    .num$ = "0" + string$(.item)
                else
                    .num$ = string$(.item)
                endif
                if .ab = 1
                    .suffix$ = .first$
                else
                    .suffix$ = .second$
                endif
                Set interval text: 1, .i, .num$ + .suffix$
                .clips = .clips + 1
                if .ab = 1
                    .ab = 2
                else
                    .ab = 1
                    .item = .item + 1
                endif
            endif
        endif
    endfor
    appendInfoLine: "Labeled ", .clips, " clips."
    if .overflow > 0
        appendInfoLine: "WARNING: ", .overflow, " extra interval(s) beyond ", .lastitem, " items were left blank. Check segmentation."
    endif
endproc
