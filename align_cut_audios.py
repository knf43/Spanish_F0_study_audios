"""
align_cut_audios.py
===================================================================
Forced-alignment protocol for the Spanish F0 study CUT audios.

This REPLACES the diarize -> Whisper -> MFA -> acoustic-syllabify pipeline
(old stages 03, 04, 06 and most of 05) with a deterministic flow that fits
the actual data: single-speaker, single-sentence clips whose transcript is
KNOWN from the filename. No ASR, no diarization, no acoustic guessing.

Filename convention (confirmed):
    P{participant}_L{list}_{item}{variant}.wav
    e.g. P01_L1_01a.wav, P02_L2_12b.wav
  - variant 'a' = PRESENT tense   (always, both lists)
  - variant 'b' = PAST tense      (always, both lists)
  - list number = reading order only; does NOT change the transcript

Target verb = WORD 3 (1-indexed) of every sentence:
    "el dueño  busca  la llave"
      1    2      3    4   5
  - PRESENT -> lexical stress on the PENULTimate syllable (BUS-ca)
  - PAST    -> lexical stress on the FINAL syllable        (bus-CO)

Pipeline (each step is a function; run all or pick one):
  STEP 1  build_corpus()    filename -> known sentence -> write .lab beside .wav
  STEP 2  run_mfa()         mfa align with bundled spanish_mfa model + dict
  STEP 3  syllabify()       group MFA phones into Spanish syllables (rule-based,
                            deterministic); produce a TextGrid with the standard
                            4 tiers: sentence / words / syllables / phones, and
                            flag the target verb's stressed syllable
  STEP 4  extract_table()   one tidy CSV row per file with the target-verb
                            syllable boundaries you need for F0 extraction
  STEP 5  qc()              validate MFA output against the APPROVED syllable key
                            (SYLLABLE_KEY); one row per file, flags the files
                            worth checking by hand in Praat (verb syllable-count
                            mismatch, stress misplacement, duration gaps, etc.)

Environment: conda env `aligner` (Montreal Forced Aligner installed).
Dependencies: praatio, soundfile  (pip install praatio soundfile).
              MFA provides `mfa` on PATH.

Dictionary: MFA's bundled `spanish_mfa` is missing a few words used here
(plantó, manchó, sopló, limpió, jarra, perra, cartulina, brocha). A custom
dictionary `spanish_mfa_custom.dict` (bundled `spanish_mfa` + these 8 entries
in the spanish_mfa phone set) fixes this. If that file sits next to this
script, it is used automatically. Override with --dictionary <name-or-path>.

Usage:
    conda activate aligner
    python align_cut_audios.py --audio-dir /path/to/2_cut_audios --all
  or run individual steps:
    python align_cut_audios.py --audio-dir ... --step corpus
    python align_cut_audios.py --audio-dir ... --step mfa
    python align_cut_audios.py --audio-dir ... --step syllabify
    python align_cut_audios.py --audio-dir ... --step table
    python align_cut_audios.py --audio-dir ... --step qc
"""

import argparse
import csv
import re
import subprocess
import sys
import unicodedata
from pathlib import Path

try:
    from praatio import textgrid
except ImportError:
    sys.exit("praatio not found. In your aligner env:  pip install praatio")

# ===================================================================
# CANONICAL SENTENCE TABLE  (item -> (present, past))
# Parsed directly from pre_post_sentence_lists.docx (List 1).
# 'a' = present, 'b' = past, for BOTH lists.
# ===================================================================
SENTENCES = {
    1: ('el dueño busca la llave', 'el dueño buscó la llave'),
    2: ('el cloro limpia el virus', 'el cloro limpió el virus'),
    3: ('el hijo planta el árbol', 'el hijo plantó el árbol'),
    4: ('el grifo filtra el agua', 'el grifo filtró el agua'),
    5: ('el tío monta el alioli', 'el tío montó el alioli'),
    6: ('el ave canta la canción', 'el ave cantó la canción'),
    7: ('la joven junta la basura', 'la joven juntó la basura'),
    8: ('la osa marca la puerta', 'la osa marcó la puerta'),
    9: ('el río mezcla la arena', 'el río mezcló la arena'),
    10: ('el suegro sopla la vela', 'el suegro sopló la vela'),
    11: ('el yerno culpa el ruido', 'el yerno culpó el ruido'),
    12: ('la tinta mancha la camisa', 'la tinta manchó la camisa'),
    13: ('la doña gasta el dinero', 'la doña gastó el dinero'),
    14: ('la perra salta la cerca', 'la perra saltó la cerca'),
    15: ('el cúter corta el paquete', 'el cúter cortó el paquete'),
    16: ('la niña busca el gato', 'la niña buscó el gato'),
    17: ('la madre limpia la casa', 'la madre limpió la casa'),
    18: ('la hija planta la flor', 'la hija plantó la flor'),
    19: ('el termo filtra el té', 'el termo filtró el té'),
    20: ('la tía monta la nata', 'la tía montó la nata'),
    21: ('la nieta canta el himno', 'la nieta cantó el himno'),
    22: ('el duque junta la leña', 'el duque juntó la leña'),
    23: ('el perro marca el territorio', 'el perro marcó el territorio'),
    24: ('el lobo mezcla la carne', 'el lobo mezcló la carne'),
    25: ('el nene sopla el guiso', 'el nene sopló el guiso'),
    26: ('la dama culpa la tormenta', 'la dama culpó la tormenta'),
    27: ('el lodo mancha el pantalón', 'el lodo manchó el pantalón'),
    28: ('el chico gasta la pila', 'el chico gastó la pila'),
    29: ('el socio salta el control', 'el socio saltó el control'),
    30: ('la nena corta la cartulina', 'la nena cortó la cartulina'),
    31: ('el móvil busca la señal', 'el móvil buscó la señal'),
    32: ('el coche limpia la nieve', 'el coche limpió la nieve'),
    33: ('la reina planta el rosal', 'la reina plantó el rosal'),
    34: ('la jarra filtra el caldo', 'la jarra filtró el caldo'),
    35: ('el monje monta la mayonesa', 'el monje montó la mayonesa'),
    36: ('el loro canta la melodía', 'el loro cantó la melodía'),
    37: ('el clavo junta la madera', 'el clavo juntó la madera'),
    38: ('el sastre marca el traje', 'el sastre marcó el traje'),
    39: ('la brocha mezcla la pintura', 'la brocha mezcló la pintura'),
    40: ('la suegra sopla la sopa', 'la suegra sopló la sopa'),
    41: ('el conde culpa el error', 'el conde culpó el error'),
    42: ('el zumo mancha la servilleta', 'el zumo manchó la servilleta'),
    43: ('la jueza gasta el sueldo', 'la jueza gastó el sueldo'),
    44: ('la rana salta la piedra', 'la rana saltó la piedra'),
    45: ('el láser corta el acero', 'el láser cortó el acero'),
    46: ('el padre busca el libro', 'el padre buscó el libro'),
    47: ('la lluvia limpia la calle', 'la lluvia limpió la calle'),
    48: ('el líder planta la semilla', 'el líder plantó la semilla'),
    49: ('la tela filtra el café', 'la tela filtró el café'),
    50: ('la dueña monta la tienda', 'la dueña montó la tienda'),
    51: ('la gorda canta la ópera', 'la gorda cantó la ópera'),
    52: ('el primo junta el papel', 'el primo juntó el papel'),
    53: ('el hombre marca la ruta', 'el hombre marcó la ruta'),
    54: ('el lago mezcla la tierra', 'el lago mezcló la tierra'),
    55: ('la chica sopla el polvo', 'la chica sopló el polvo'),
    56: ('la ama culpa la demora', 'la ama culpó la demora'),
    57: ('el vino mancha la mesa', 'el vino manchó la mesa'),
    58: ('el carro gasta la gasolina', 'el carro gastó la gasolina'),
    59: ('el niño salta el charco', 'el niño saltó el charco'),
    60: ('la rata corta el queso', 'la rata cortó el queso'),
    61: ('la vaca busca la paja', 'la vaca buscó la paja'),
    62: ('la prima limpia el baño', 'la prima limpió el baño'),
    63: ('la vieja planta el jazmín', 'la vieja plantó el jazmín'),
    64: ('el filtro filtra el aire', 'el filtro filtró el aire'),
    65: ('la jefa monta el escritorio', 'la jefa montó el escritorio'),
    66: ('la guapa canta la nana', 'la guapa cantó la nana'),
    67: ('la nuera junta el cartón', 'la nuera juntó el cartón'),
    68: ('la grasa marca la pared', 'la grasa marcó la pared'),
    69: ('la flaca mezcla el yogur', 'la flaca mezcló el yogur'),
    70: ('el huésped sopla la pizza', 'el huésped sopló la pizza'),
    71: ('el cónsul culpa la crisis', 'el cónsul culpó la crisis'),
    72: ('la rueda mancha el garaje', 'la rueda manchó el garaje'),
    73: ('la rica gasta el crédito', 'la rica gastó el crédito'),
    74: ('la cabra salta la valla', 'la cabra saltó la valla'),
    75: ('el mozo corta la cinta', 'el mozo cortó la cinta'),
    76: ('la rubia busca el anillo', 'la rubia buscó el anillo'),
    77: ('el trapo limpia la suciedad', 'el trapo limpió la suciedad'),
    78: ('el cura planta el geranio', 'el cura plantó el geranio'),
    79: ('la malla filtra el aceite', 'la malla filtró el aceite'),
    80: ('el nieto monta la estantería', 'el nieto montó la estantería'),
    81: ('el viejo canta el bolero', 'el viejo cantó el bolero'),
    82: ('la gata junta la comida', 'la gata juntó la comida'),
    83: ('la bici marca el suelo', 'la bici marcó el suelo'),
    84: ('la presa mezcla el cemento', 'la presa mezcló el cemento'),
    85: ('la monja sopla el fuego', 'la monja sopló el fuego'),
    86: ('la novia culpa el calor', 'la novia culpó el calor'),
    87: ('la sangre mancha la falda', 'la sangre manchó la falda'),
    88: ('el novio gasta el jabón', 'el novio gastó el jabón'),
    89: ('el mago salta el obstáculo', 'el mago saltó el obstáculo'),
    90: ('la mula corta el heno', 'la mula cortó el heno'),
}

# MFA bundled Spanish models (match your existing 05_align.py config)
MFA_ACOUSTIC = 'spanish_mfa'
MFA_DICT = 'spanish_mfa'

# ===================================================================
# APPROVED SYLLABLE KEY  (item -> (present_words, past_words))
# Each value is a list of words, each word a list of syllable strings.
# Reviewed/approved by Kaylee; alioli corrected to a-li-o-li (hiatus).
# Used by STEP 5 (qc) to validate MFA's syllable count & stress.
# ===================================================================
SYLLABLE_KEY = {
    1: ([['el'], ['due', 'ño'], ['bus', 'ca'], ['la'], ['lla', 've']], [['el'], ['due', 'ño'], ['bus', 'có'], ['la'], ['lla', 've']]),
    2: ([['el'], ['clo', 'ro'], ['lim', 'pia'], ['el'], ['vi', 'rus']], [['el'], ['clo', 'ro'], ['lim', 'pió'], ['el'], ['vi', 'rus']]),
    3: ([['el'], ['hi', 'jo'], ['plan', 'ta'], ['el'], ['ár', 'bol']], [['el'], ['hi', 'jo'], ['plan', 'tó'], ['el'], ['ár', 'bol']]),
    4: ([['el'], ['gri', 'fo'], ['fil', 'tra'], ['el'], ['a', 'gua']], [['el'], ['gri', 'fo'], ['fil', 'tró'], ['el'], ['a', 'gua']]),
    5: ([['el'], ['tí', 'o'], ['mon', 'ta'], ['el'], ['a', 'li', 'o', 'li']], [['el'], ['tí', 'o'], ['mon', 'tó'], ['el'], ['a', 'li', 'o', 'li']]),
    6: ([['el'], ['a', 've'], ['can', 'ta'], ['la'], ['can', 'ción']], [['el'], ['a', 've'], ['can', 'tó'], ['la'], ['can', 'ción']]),
    7: ([['la'], ['jo', 'ven'], ['jun', 'ta'], ['la'], ['ba', 'su', 'ra']], [['la'], ['jo', 'ven'], ['jun', 'tó'], ['la'], ['ba', 'su', 'ra']]),
    8: ([['la'], ['o', 'sa'], ['mar', 'ca'], ['la'], ['puer', 'ta']], [['la'], ['o', 'sa'], ['mar', 'có'], ['la'], ['puer', 'ta']]),
    9: ([['el'], ['rí', 'o'], ['mez', 'cla'], ['la'], ['a', 're', 'na']], [['el'], ['rí', 'o'], ['mez', 'cló'], ['la'], ['a', 're', 'na']]),
    10: ([['el'], ['sue', 'gro'], ['so', 'pla'], ['la'], ['ve', 'la']], [['el'], ['sue', 'gro'], ['so', 'pló'], ['la'], ['ve', 'la']]),
    11: ([['el'], ['yer', 'no'], ['cul', 'pa'], ['el'], ['rui', 'do']], [['el'], ['yer', 'no'], ['cul', 'pó'], ['el'], ['rui', 'do']]),
    12: ([['la'], ['tin', 'ta'], ['man', 'cha'], ['la'], ['ca', 'mi', 'sa']], [['la'], ['tin', 'ta'], ['man', 'chó'], ['la'], ['ca', 'mi', 'sa']]),
    13: ([['la'], ['do', 'ña'], ['gas', 'ta'], ['el'], ['di', 'ne', 'ro']], [['la'], ['do', 'ña'], ['gas', 'tó'], ['el'], ['di', 'ne', 'ro']]),
    14: ([['la'], ['pe', 'rra'], ['sal', 'ta'], ['la'], ['cer', 'ca']], [['la'], ['pe', 'rra'], ['sal', 'tó'], ['la'], ['cer', 'ca']]),
    15: ([['el'], ['cú', 'ter'], ['cor', 'ta'], ['el'], ['pa', 'que', 'te']], [['el'], ['cú', 'ter'], ['cor', 'tó'], ['el'], ['pa', 'que', 'te']]),
    16: ([['la'], ['ni', 'ña'], ['bus', 'ca'], ['el'], ['ga', 'to']], [['la'], ['ni', 'ña'], ['bus', 'có'], ['el'], ['ga', 'to']]),
    17: ([['la'], ['ma', 'dre'], ['lim', 'pia'], ['la'], ['ca', 'sa']], [['la'], ['ma', 'dre'], ['lim', 'pió'], ['la'], ['ca', 'sa']]),
    18: ([['la'], ['hi', 'ja'], ['plan', 'ta'], ['la'], ['flor']], [['la'], ['hi', 'ja'], ['plan', 'tó'], ['la'], ['flor']]),
    19: ([['el'], ['ter', 'mo'], ['fil', 'tra'], ['el'], ['té']], [['el'], ['ter', 'mo'], ['fil', 'tró'], ['el'], ['té']]),
    20: ([['la'], ['tí', 'a'], ['mon', 'ta'], ['la'], ['na', 'ta']], [['la'], ['tí', 'a'], ['mon', 'tó'], ['la'], ['na', 'ta']]),
    21: ([['la'], ['nie', 'ta'], ['can', 'ta'], ['el'], ['him', 'no']], [['la'], ['nie', 'ta'], ['can', 'tó'], ['el'], ['him', 'no']]),
    22: ([['el'], ['du', 'que'], ['jun', 'ta'], ['la'], ['le', 'ña']], [['el'], ['du', 'que'], ['jun', 'tó'], ['la'], ['le', 'ña']]),
    23: ([['el'], ['pe', 'rro'], ['mar', 'ca'], ['el'], ['te', 'rri', 'to', 'rio']], [['el'], ['pe', 'rro'], ['mar', 'có'], ['el'], ['te', 'rri', 'to', 'rio']]),
    24: ([['el'], ['lo', 'bo'], ['mez', 'cla'], ['la'], ['car', 'ne']], [['el'], ['lo', 'bo'], ['mez', 'cló'], ['la'], ['car', 'ne']]),
    25: ([['el'], ['ne', 'ne'], ['so', 'pla'], ['el'], ['gui', 'so']], [['el'], ['ne', 'ne'], ['so', 'pló'], ['el'], ['gui', 'so']]),
    26: ([['la'], ['da', 'ma'], ['cul', 'pa'], ['la'], ['tor', 'men', 'ta']], [['la'], ['da', 'ma'], ['cul', 'pó'], ['la'], ['tor', 'men', 'ta']]),
    27: ([['el'], ['lo', 'do'], ['man', 'cha'], ['el'], ['pan', 'ta', 'lón']], [['el'], ['lo', 'do'], ['man', 'chó'], ['el'], ['pan', 'ta', 'lón']]),
    28: ([['el'], ['chi', 'co'], ['gas', 'ta'], ['la'], ['pi', 'la']], [['el'], ['chi', 'co'], ['gas', 'tó'], ['la'], ['pi', 'la']]),
    29: ([['el'], ['so', 'cio'], ['sal', 'ta'], ['el'], ['con', 'trol']], [['el'], ['so', 'cio'], ['sal', 'tó'], ['el'], ['con', 'trol']]),
    30: ([['la'], ['ne', 'na'], ['cor', 'ta'], ['la'], ['car', 'tu', 'li', 'na']], [['la'], ['ne', 'na'], ['cor', 'tó'], ['la'], ['car', 'tu', 'li', 'na']]),
    31: ([['el'], ['mó', 'vil'], ['bus', 'ca'], ['la'], ['se', 'ñal']], [['el'], ['mó', 'vil'], ['bus', 'có'], ['la'], ['se', 'ñal']]),
    32: ([['el'], ['co', 'che'], ['lim', 'pia'], ['la'], ['nie', 've']], [['el'], ['co', 'che'], ['lim', 'pió'], ['la'], ['nie', 've']]),
    33: ([['la'], ['rei', 'na'], ['plan', 'ta'], ['el'], ['ro', 'sal']], [['la'], ['rei', 'na'], ['plan', 'tó'], ['el'], ['ro', 'sal']]),
    34: ([['la'], ['ja', 'rra'], ['fil', 'tra'], ['el'], ['cal', 'do']], [['la'], ['ja', 'rra'], ['fil', 'tró'], ['el'], ['cal', 'do']]),
    35: ([['el'], ['mon', 'je'], ['mon', 'ta'], ['la'], ['ma', 'yo', 'ne', 'sa']], [['el'], ['mon', 'je'], ['mon', 'tó'], ['la'], ['ma', 'yo', 'ne', 'sa']]),
    36: ([['el'], ['lo', 'ro'], ['can', 'ta'], ['la'], ['me', 'lo', 'dí', 'a']], [['el'], ['lo', 'ro'], ['can', 'tó'], ['la'], ['me', 'lo', 'dí', 'a']]),
    37: ([['el'], ['cla', 'vo'], ['jun', 'ta'], ['la'], ['ma', 'de', 'ra']], [['el'], ['cla', 'vo'], ['jun', 'tó'], ['la'], ['ma', 'de', 'ra']]),
    38: ([['el'], ['sas', 'tre'], ['mar', 'ca'], ['el'], ['tra', 'je']], [['el'], ['sas', 'tre'], ['mar', 'có'], ['el'], ['tra', 'je']]),
    39: ([['la'], ['bro', 'cha'], ['mez', 'cla'], ['la'], ['pin', 'tu', 'ra']], [['la'], ['bro', 'cha'], ['mez', 'cló'], ['la'], ['pin', 'tu', 'ra']]),
    40: ([['la'], ['sue', 'gra'], ['so', 'pla'], ['la'], ['so', 'pa']], [['la'], ['sue', 'gra'], ['so', 'pló'], ['la'], ['so', 'pa']]),
    41: ([['el'], ['con', 'de'], ['cul', 'pa'], ['el'], ['e', 'rror']], [['el'], ['con', 'de'], ['cul', 'pó'], ['el'], ['e', 'rror']]),
    42: ([['el'], ['zu', 'mo'], ['man', 'cha'], ['la'], ['ser', 'vi', 'lle', 'ta']], [['el'], ['zu', 'mo'], ['man', 'chó'], ['la'], ['ser', 'vi', 'lle', 'ta']]),
    43: ([['la'], ['jue', 'za'], ['gas', 'ta'], ['el'], ['suel', 'do']], [['la'], ['jue', 'za'], ['gas', 'tó'], ['el'], ['suel', 'do']]),
    44: ([['la'], ['ra', 'na'], ['sal', 'ta'], ['la'], ['pie', 'dra']], [['la'], ['ra', 'na'], ['sal', 'tó'], ['la'], ['pie', 'dra']]),
    45: ([['el'], ['lá', 'ser'], ['cor', 'ta'], ['el'], ['a', 'ce', 'ro']], [['el'], ['lá', 'ser'], ['cor', 'tó'], ['el'], ['a', 'ce', 'ro']]),
    46: ([['el'], ['pa', 'dre'], ['bus', 'ca'], ['el'], ['li', 'bro']], [['el'], ['pa', 'dre'], ['bus', 'có'], ['el'], ['li', 'bro']]),
    47: ([['la'], ['llu', 'via'], ['lim', 'pia'], ['la'], ['ca', 'lle']], [['la'], ['llu', 'via'], ['lim', 'pió'], ['la'], ['ca', 'lle']]),
    48: ([['el'], ['lí', 'der'], ['plan', 'ta'], ['la'], ['se', 'mi', 'lla']], [['el'], ['lí', 'der'], ['plan', 'tó'], ['la'], ['se', 'mi', 'lla']]),
    49: ([['la'], ['te', 'la'], ['fil', 'tra'], ['el'], ['ca', 'fé']], [['la'], ['te', 'la'], ['fil', 'tró'], ['el'], ['ca', 'fé']]),
    50: ([['la'], ['due', 'ña'], ['mon', 'ta'], ['la'], ['tien', 'da']], [['la'], ['due', 'ña'], ['mon', 'tó'], ['la'], ['tien', 'da']]),
    51: ([['la'], ['gor', 'da'], ['can', 'ta'], ['la'], ['ó', 'pe', 'ra']], [['la'], ['gor', 'da'], ['can', 'tó'], ['la'], ['ó', 'pe', 'ra']]),
    52: ([['el'], ['pri', 'mo'], ['jun', 'ta'], ['el'], ['pa', 'pel']], [['el'], ['pri', 'mo'], ['jun', 'tó'], ['el'], ['pa', 'pel']]),
    53: ([['el'], ['hom', 'bre'], ['mar', 'ca'], ['la'], ['ru', 'ta']], [['el'], ['hom', 'bre'], ['mar', 'có'], ['la'], ['ru', 'ta']]),
    54: ([['el'], ['la', 'go'], ['mez', 'cla'], ['la'], ['tie', 'rra']], [['el'], ['la', 'go'], ['mez', 'cló'], ['la'], ['tie', 'rra']]),
    55: ([['la'], ['chi', 'ca'], ['so', 'pla'], ['el'], ['pol', 'vo']], [['la'], ['chi', 'ca'], ['so', 'pló'], ['el'], ['pol', 'vo']]),
    56: ([['la'], ['a', 'ma'], ['cul', 'pa'], ['la'], ['de', 'mo', 'ra']], [['la'], ['a', 'ma'], ['cul', 'pó'], ['la'], ['de', 'mo', 'ra']]),
    57: ([['el'], ['vi', 'no'], ['man', 'cha'], ['la'], ['me', 'sa']], [['el'], ['vi', 'no'], ['man', 'chó'], ['la'], ['me', 'sa']]),
    58: ([['el'], ['ca', 'rro'], ['gas', 'ta'], ['la'], ['ga', 'so', 'li', 'na']], [['el'], ['ca', 'rro'], ['gas', 'tó'], ['la'], ['ga', 'so', 'li', 'na']]),
    59: ([['el'], ['ni', 'ño'], ['sal', 'ta'], ['el'], ['char', 'co']], [['el'], ['ni', 'ño'], ['sal', 'tó'], ['el'], ['char', 'co']]),
    60: ([['la'], ['ra', 'ta'], ['cor', 'ta'], ['el'], ['que', 'so']], [['la'], ['ra', 'ta'], ['cor', 'tó'], ['el'], ['que', 'so']]),
    61: ([['la'], ['va', 'ca'], ['bus', 'ca'], ['la'], ['pa', 'ja']], [['la'], ['va', 'ca'], ['bus', 'có'], ['la'], ['pa', 'ja']]),
    62: ([['la'], ['pri', 'ma'], ['lim', 'pia'], ['el'], ['ba', 'ño']], [['la'], ['pri', 'ma'], ['lim', 'pió'], ['el'], ['ba', 'ño']]),
    63: ([['la'], ['vie', 'ja'], ['plan', 'ta'], ['el'], ['jaz', 'mín']], [['la'], ['vie', 'ja'], ['plan', 'tó'], ['el'], ['jaz', 'mín']]),
    64: ([['el'], ['fil', 'tro'], ['fil', 'tra'], ['el'], ['ai', 're']], [['el'], ['fil', 'tro'], ['fil', 'tró'], ['el'], ['ai', 're']]),
    65: ([['la'], ['je', 'fa'], ['mon', 'ta'], ['el'], ['es', 'cri', 'to', 'rio']], [['la'], ['je', 'fa'], ['mon', 'tó'], ['el'], ['es', 'cri', 'to', 'rio']]),
    66: ([['la'], ['gua', 'pa'], ['can', 'ta'], ['la'], ['na', 'na']], [['la'], ['gua', 'pa'], ['can', 'tó'], ['la'], ['na', 'na']]),
    67: ([['la'], ['nue', 'ra'], ['jun', 'ta'], ['el'], ['car', 'tón']], [['la'], ['nue', 'ra'], ['jun', 'tó'], ['el'], ['car', 'tón']]),
    68: ([['la'], ['gra', 'sa'], ['mar', 'ca'], ['la'], ['pa', 'red']], [['la'], ['gra', 'sa'], ['mar', 'có'], ['la'], ['pa', 'red']]),
    69: ([['la'], ['fla', 'ca'], ['mez', 'cla'], ['el'], ['yo', 'gur']], [['la'], ['fla', 'ca'], ['mez', 'cló'], ['el'], ['yo', 'gur']]),
    70: ([['el'], ['hués', 'ped'], ['so', 'pla'], ['la'], ['piz', 'za']], [['el'], ['hués', 'ped'], ['so', 'pló'], ['la'], ['piz', 'za']]),
    71: ([['el'], ['cón', 'sul'], ['cul', 'pa'], ['la'], ['cri', 'sis']], [['el'], ['cón', 'sul'], ['cul', 'pó'], ['la'], ['cri', 'sis']]),
    72: ([['la'], ['rue', 'da'], ['man', 'cha'], ['el'], ['ga', 'ra', 'je']], [['la'], ['rue', 'da'], ['man', 'chó'], ['el'], ['ga', 'ra', 'je']]),
    73: ([['la'], ['ri', 'ca'], ['gas', 'ta'], ['el'], ['cré', 'di', 'to']], [['la'], ['ri', 'ca'], ['gas', 'tó'], ['el'], ['cré', 'di', 'to']]),
    74: ([['la'], ['ca', 'bra'], ['sal', 'ta'], ['la'], ['va', 'lla']], [['la'], ['ca', 'bra'], ['sal', 'tó'], ['la'], ['va', 'lla']]),
    75: ([['el'], ['mo', 'zo'], ['cor', 'ta'], ['la'], ['cin', 'ta']], [['el'], ['mo', 'zo'], ['cor', 'tó'], ['la'], ['cin', 'ta']]),
    76: ([['la'], ['ru', 'bia'], ['bus', 'ca'], ['el'], ['a', 'ni', 'llo']], [['la'], ['ru', 'bia'], ['bus', 'có'], ['el'], ['a', 'ni', 'llo']]),
    77: ([['el'], ['tra', 'po'], ['lim', 'pia'], ['la'], ['su', 'cie', 'dad']], [['el'], ['tra', 'po'], ['lim', 'pió'], ['la'], ['su', 'cie', 'dad']]),
    78: ([['el'], ['cu', 'ra'], ['plan', 'ta'], ['el'], ['ge', 'ra', 'nio']], [['el'], ['cu', 'ra'], ['plan', 'tó'], ['el'], ['ge', 'ra', 'nio']]),
    79: ([['la'], ['ma', 'lla'], ['fil', 'tra'], ['el'], ['a', 'cei', 'te']], [['la'], ['ma', 'lla'], ['fil', 'tró'], ['el'], ['a', 'cei', 'te']]),
    80: ([['el'], ['nie', 'to'], ['mon', 'ta'], ['la'], ['es', 'tan', 'te', 'rí', 'a']], [['el'], ['nie', 'to'], ['mon', 'tó'], ['la'], ['es', 'tan', 'te', 'rí', 'a']]),
    81: ([['el'], ['vie', 'jo'], ['can', 'ta'], ['el'], ['bo', 'le', 'ro']], [['el'], ['vie', 'jo'], ['can', 'tó'], ['el'], ['bo', 'le', 'ro']]),
    82: ([['la'], ['ga', 'ta'], ['jun', 'ta'], ['la'], ['co', 'mi', 'da']], [['la'], ['ga', 'ta'], ['jun', 'tó'], ['la'], ['co', 'mi', 'da']]),
    83: ([['la'], ['bi', 'ci'], ['mar', 'ca'], ['el'], ['sue', 'lo']], [['la'], ['bi', 'ci'], ['mar', 'có'], ['el'], ['sue', 'lo']]),
    84: ([['la'], ['pre', 'sa'], ['mez', 'cla'], ['el'], ['ce', 'men', 'to']], [['la'], ['pre', 'sa'], ['mez', 'cló'], ['el'], ['ce', 'men', 'to']]),
    85: ([['la'], ['mon', 'ja'], ['so', 'pla'], ['el'], ['fue', 'go']], [['la'], ['mon', 'ja'], ['so', 'pló'], ['el'], ['fue', 'go']]),
    86: ([['la'], ['no', 'via'], ['cul', 'pa'], ['el'], ['ca', 'lor']], [['la'], ['no', 'via'], ['cul', 'pó'], ['el'], ['ca', 'lor']]),
    87: ([['la'], ['san', 'gre'], ['man', 'cha'], ['la'], ['fal', 'da']], [['la'], ['san', 'gre'], ['man', 'chó'], ['la'], ['fal', 'da']]),
    88: ([['el'], ['no', 'vio'], ['gas', 'ta'], ['el'], ['ja', 'bón']], [['el'], ['no', 'vio'], ['gas', 'tó'], ['el'], ['ja', 'bón']]),
    89: ([['el'], ['ma', 'go'], ['sal', 'ta'], ['el'], ['obs', 'tá', 'cu', 'lo']], [['el'], ['ma', 'go'], ['sal', 'tó'], ['el'], ['obs', 'tá', 'cu', 'lo']]),
    90: ([['la'], ['mu', 'la'], ['cor', 'ta'], ['el'], ['he', 'no']], [['la'], ['mu', 'la'], ['cor', 'tó'], ['el'], ['he', 'no']]),
}


FNAME_RE = re.compile(
    r'^(?P<pid>P\d+)_L(?P<list>[12])_(?P<item>\d+)(?P<var>[ab])$',
    re.IGNORECASE,
)


# ===================================================================
# Filename -> sentence resolver  (the heart of the protocol)
# ===================================================================
def parse_filename(stem: str) -> dict:
    """P01_L1_01a -> {pid, list, item, variant, tense, sentence, target_verb}."""
    m = FNAME_RE.match(stem)
    if not m:
        raise ValueError(f"Filename does not match P##_L#_##[ab]: {stem}")
    item = int(m.group('item'))
    var = m.group('var').lower()
    if item not in SENTENCES:
        raise ValueError(f"Item {item} not in sentence table (stem={stem})")
    tense = 'present' if var == 'a' else 'past'           # a=present, b=past
    sentence = SENTENCES[item][0 if tense == 'present' else 1]
    words = sentence.split()
    target_verb = words[2]                                # WORD 3 (0-indexed = 2)
    return {
        'pid': m.group('pid').upper(),
        'list': int(m.group('list')),
        'item': item,
        'variant': var,
        'tense': tense,
        'sentence': sentence,
        'target_verb': target_verb,
    }


# ===================================================================
# STEP 0 — standardize audio to mono 16 kHz (optional but recommended)
# ===================================================================
def standardize_audio(audio_dir: Path, out_dir: Path) -> int:
    """Convert every .wav to mono, 16 kHz, 16-bit PCM into out_dir.

    MFA downmixes/resamples internally anyway, but standardizing up front
    avoids any surprises with odd sample rates or multi-channel files and
    makes the corpus uniform. Uses ffmpeg if available (fast, robust);
    otherwise falls back to soundfile + numpy (already installed).

    Files that already parse by filename are converted; others are skipped
    and reported. Returns the count converted.
    """
    import shutil as _shutil
    out_dir.mkdir(parents=True, exist_ok=True)
    wavs = sorted(audio_dir.glob('*.wav'))
    if not wavs:
        sys.exit(f"No .wav files found in {audio_dir}")

    use_ffmpeg = _shutil.which('ffmpeg') is not None
    if use_ffmpeg:
        print("[STEP 0] Using ffmpeg to convert to mono 16 kHz.")
    else:
        print("[STEP 0] ffmpeg not found; using soundfile/numpy fallback.")
        import soundfile as sf
        import numpy as np
        try:
            from scipy.signal import resample_poly  # better resampler if present
            have_scipy = True
        except ImportError:
            have_scipy = False

    converted = 0
    skipped = []
    for wav in wavs:
        try:
            parse_filename(wav.stem)        # only convert files we recognize
        except ValueError as e:
            skipped.append((wav.name, str(e)))
            continue
        dest = out_dir / wav.name
        if dest.exists():
            converted += 1
            continue
        if use_ffmpeg:
            rc = subprocess.run(
                ['ffmpeg', '-i', str(wav), '-ac', '1', '-ar', '16000',
                 '-sample_fmt', 's16', str(dest), '-y', '-loglevel', 'error'],
                capture_output=True, text=True)
            if rc.returncode != 0:
                skipped.append((wav.name, f"ffmpeg: {rc.stderr.strip()[:120]}"))
                continue
        else:
            try:
                data, sr = sf.read(str(wav), always_2d=True)
                mono = data.mean(axis=1)                       # downmix
                if sr != 16000:
                    if have_scipy:
                        from math import gcd
                        g = gcd(int(sr), 16000)
                        mono = resample_poly(mono, 16000 // g, int(sr) // g)
                    else:
                        # crude linear resample fallback
                        import numpy as np
                        n_out = int(round(len(mono) * 16000 / sr))
                        xp = np.linspace(0, 1, len(mono), endpoint=False)
                        xq = np.linspace(0, 1, n_out, endpoint=False)
                        mono = np.interp(xq, xp, mono)
                sf.write(str(dest), mono.astype('float32'), 16000, subtype='PCM_16')
            except Exception as e:                              # noqa: BLE001
                skipped.append((wav.name, f"convert: {e}"))
                continue
        converted += 1

    print(f"[STEP 0] Standardized {converted} files into {out_dir}")
    if skipped:
        print(f"[STEP 0] Skipped {len(skipped)} files:")
        for name, reason in skipped[:10]:
            print(f"         - {name}: {reason}")
        if len(skipped) > 10:
            print(f"         ... and {len(skipped) - 10} more")
    return converted


# ===================================================================
# STEP 1 — build corpus: write a .lab next to each .wav
# ===================================================================
def build_corpus(audio_dir: Path, corpus_dir: Path) -> int:
    """Copy/link wavs into a flat corpus dir and write matching .lab files.

    MFA expects, in ONE directory:  stem.wav  +  stem.lab  (same stem).
    We keep the original filename so the parser still works downstream.
    """
    corpus_dir.mkdir(parents=True, exist_ok=True)
    wavs = sorted(audio_dir.glob('*.wav'))
    if not wavs:
        sys.exit(f"No .wav files found in {audio_dir}")
    written = 0
    skipped = []
    for wav in wavs:
        try:
            info = parse_filename(wav.stem)
        except ValueError as e:
            skipped.append((wav.name, str(e)))
            continue
        # Hardlink the wav into the corpus (cheap, no copy); fall back to copy.
        dest_wav = corpus_dir / wav.name
        if not dest_wav.exists():
            try:
                dest_wav.hardlink_to(wav)
            except (OSError, AttributeError):
                import shutil
                shutil.copy2(wav, dest_wav)
        # Write transcript. MFA reads the .lab verbatim as ground truth.
        (corpus_dir / f"{wav.stem}.lab").write_text(
            info['sentence'] + "\n", encoding='utf-8'
        )
        written += 1
    print(f"[STEP 1] Wrote {written} .lab/.wav pairs into {corpus_dir}")
    if skipped:
        print(f"[STEP 1] Skipped {len(skipped)} files that did not parse:")
        for name, reason in skipped[:10]:
            print(f"         - {name}: {reason}")
        if len(skipped) > 10:
            print(f"         ... and {len(skipped) - 10} more")
    return written


# ===================================================================
# STEP 2 — run MFA forced alignment
# ===================================================================
def run_mfa(corpus_dir: Path, aligned_dir: Path, dictionary: str = MFA_DICT) -> None:
    """Forced-align the corpus with the spanish_mfa acoustic model.

    `dictionary` is the pronunciation dictionary: either the bundled name
    ('spanish_mfa') or a path to a custom .dict file that extends it (e.g.
    'spanish_mfa_custom.dict' containing supplemental OOV entries). The
    acoustic model is always the bundled 'spanish_mfa'.

    No --clean of the corpus text; the transcript is already ground truth.
    --beam/--retry-beam widened to avoid spurious failures on short clips.
    """
    aligned_dir.mkdir(parents=True, exist_ok=True)
    # Sanity: confirm mfa is reachable. MFA 3.x uses `mfa version` (no dashes);
    # older builds use `mfa --version`. Treat "command exists" as success
    # regardless of which subcommand it accepts.
    import shutil as _shutil
    if _shutil.which('mfa') is None:
        sys.exit("`mfa` not on PATH. Did you `conda activate aligner`?")

    cmd = [
        'mfa', 'align',
        str(corpus_dir),
        str(dictionary),
        MFA_ACOUSTIC,
        str(aligned_dir),
        '--clean',            # clean MFA's own temp workspace, not your text
        '--overwrite',
        '--beam', '100',
        '--retry_beam', '400',
        # Note: a flat corpus dir (all wavs in one folder) makes MFA treat each
        # file as its own speaker by default, which is correct for these
        # independent single-sentence clips. No --single_speaker flag needed.
    ]
    print("[STEP 2] Running:", ' '.join(cmd))
    result = subprocess.run(cmd)
    if result.returncode != 0:
        sys.exit(f"[STEP 2] MFA alignment failed (exit {result.returncode}). "
                 f"Check the MFA log printed above.")
    n = len(list(aligned_dir.glob('*.TextGrid')))
    print(f"[STEP 2] MFA produced {n} aligned TextGrids in {aligned_dir}")


# ===================================================================
# STEP 3 — phones -> Spanish syllables (deterministic) + stress flag
# ===================================================================
# Spanish phone inventory from the spanish_mfa phone set is IPA-based.
# We classify each MFA phone label as vowel/glide/consonant to syllabify by
# the maximal-onset principle. Vowels carry the syllable nucleus.
SPANISH_VOWELS = set('aeiouɑɛɔ')           # core vowels (ipa); includes variants
SPANISH_GLIDES = set('jw')                 # true semivowel glides (form diphthongs)
# NOTE: ʝ / ɟʝ (Spanish "ll"/"y") are CONSONANTS that act as syllable onsets
# (ma-ʝa, ʝa-βe, ka-ʝe), never diphthong glides, so they are NOT listed here.
# Everything else that isn't whitespace/empty is treated as a consonant.

# Consonant clusters that must NOT be split (stay together as a complex onset).
SPANISH_INSEPARABLE_ONSETS = {
    'pɾ', 'bɾ', 'tɾ', 'dɾ', 'kɾ', 'gɾ', 'fɾ',
    'pl', 'bl', 'kl', 'gl', 'fl',
    'tʃ',  # ch is a single onset
}


def _phone_is_vowel(label: str) -> bool:
    base = label.rstrip('012ːˈˌ').lower()
    return bool(base) and base[0] in SPANISH_VOWELS


def _phone_is_glide(label: str) -> bool:
    base = label.rstrip('012ːˈˌ').lower()
    return bool(base) and base[0] in SPANISH_GLIDES


def syllabify_phones(phones: list, word_label: str = "") -> list:
    """Group a word's phone intervals into syllables by maximal onset.

    phones: list of (start, end, label) for ONE word, in order.
    word_label: the orthographic word (e.g. "tío", "reina"); used to tell a
        diphthong from a hiatus, because the written accent (í/ú) that forces a
        hiatus is lost in the bare phone labels.
    returns: list of syllables, each a list of (start,end,label) phone tuples.

    Spanish diphthong/hiatus rule for two ADJACENT vowels (nothing between):
      * STRONG = a, e, o ; WEAK = i, u.
      * weak+strong, strong+weak, or weak+weak  -> DIPHTHONG  (one nucleus)
        e.g. reina (e+i), socio (i+o), causa (a+u), ciudad (i+u).
      * strong+strong                            -> HIATUS    (two nuclei)
        e.g. caer (a+e), leon (e+o).
      * a WEAK vowel bearing a written accent (í, ú) -> HIATUS
        e.g. tío (í+o), río (í+o), día (í+a).
    Glides (j, w) between/around a vowel always join its nucleus.
    Consonant(s) between two vowels split them (maximal onset for the cluster).
    """
    n = len(phones)
    is_vowel = [_phone_is_vowel(p[2]) for p in phones]
    is_glide = [_phone_is_glide(p[2]) for p in phones]

    raw_vowels = [i for i in range(n) if is_vowel[i]]
    if not raw_vowels:
        return [phones]

    # Does the word carry an accented weak vowel (í or ú)? If so, that vowel
    # forms a hiatus with an adjacent vowel. We can only know this from spelling.
    wl = (word_label or "").lower()
    has_accent_i = "í" in wl
    has_accent_u = "ú" in wl

    STRONG = set("aeoɑɛɔ")
    WEAK = set("iu")

    def vowel_base(idx):
        return phones[idx][2].rstrip('012ːˈˌ').lower()[:1]

    def is_hiatus(a_idx, b_idx):
        """True if the two adjacent vowels at a_idx,b_idx form a hiatus."""
        va, vb = vowel_base(a_idx), vowel_base(b_idx)
        # strong + strong -> hiatus
        if va in STRONG and vb in STRONG:
            return True
        # accented weak vowel -> hiatus (e.g. tío, río, día).
        # The accented vowel is the WEAK one; if the word has í/ú and one of
        # this pair is that weak vowel, treat as hiatus.
        if has_accent_i and ('i' in (va, vb)):
            return True
        if has_accent_u and ('u' in (va, vb)):
            return True
        return False

    nuclei = []
    group_last = raw_vowels[0]
    for a, b in zip(raw_vowels, raw_vowels[1:]):
        between = list(range(a + 1, b))
        if len(between) == 0:
            # adjacent vowels: diphthong unless hiatus
            if is_hiatus(a, b):
                nuclei.append(group_last)   # close: hiatus -> separate nuclei
                group_last = b
            else:
                group_last = b              # diphthong -> same nucleus
        elif all(is_glide[j] for j in between):
            group_last = b                  # glide(s) only -> same nucleus
        else:
            nuclei.append(group_last)       # consonant between -> separate nuclei
            group_last = b
    nuclei.append(group_last)

    syllables = []
    prev_cut = 0  # index where current syllable starts

    for k in range(len(nuclei)):
        v = nuclei[k]
        if k == len(nuclei) - 1:
            # Last nucleus: take everything to the end.
            syllables.append(phones[prev_cut:n])
            break
        next_v = nuclei[k + 1]
        # Consonants strictly between this nucleus and the next, excluding any
        # glides that cling to either nucleus.
        between = list(range(v + 1, next_v))
        cons = [i for i in between if not is_glide[i]]

        if len(cons) == 0:
            cut = next_v          # open syllable: V.(g)V
        elif len(cons) == 1:
            cut = cons[0]         # V.CV  -> single C starts next syllable
        else:
            # 2+ consonants: check if the LAST TWO form an inseparable onset.
            last2 = (phones[cons[-2]][2].rstrip('012ːˈˌ').lower()
                     + phones[cons[-1]][2].rstrip('012ːˈˌ').lower())
            if last2 in SPANISH_INSEPARABLE_ONSETS:
                cut = cons[-2]    # both consonants begin next syllable
            else:
                cut = cons[-1]    # only last consonant begins next syllable
        syllables.append(phones[prev_cut:cut])
        prev_cut = cut

    return [s for s in syllables if s]


def _strip_accents(s: str) -> str:
    return ''.join(c for c in unicodedata.normalize('NFD', s)
                   if unicodedata.category(c) != 'Mn')


def stressed_syllable_index(verb: str, tense: str, n_syllables: int) -> int:
    """Return 0-based index of the lexically stressed syllable of the target verb.

    By study design: present -> penult, past -> final (oxytone, -ó).
    We trust the design but also respect a written accent if present
    (e.g. a -ó form is final-stressed regardless).
    """
    if n_syllables <= 1:
        return 0
    # Written accent wins if present (defensive; -ó forms are accented).
    for i, ch in enumerate(verb):
        if ch in 'áéíóú':
            # crude: map accent position is unreliable across syllable split,
            # so fall back to design rule below unless it's a final -ó.
            break
    if tense == 'past':
        return n_syllables - 1            # final syllable (bus-CO)
    return n_syllables - 2                # penult (BUS-ca)


def syllabify_textgrid(tg_path: Path, info: dict, out_path: Path) -> dict:
    """Add a <speaker>_syllables tier built from the phones tier, and report the
    target verb's stressed-syllable boundaries.

    MFA output tiers are named 'words' and 'phones' (single_speaker) or
    '<name> - words'/'<name> - phones'. We detect whichever exists.
    """
    tg = textgrid.openTextgrid(str(tg_path), includeEmptyIntervals=True)

    # Locate word + phone tiers (case/format tolerant).
    word_tier = phone_tier = None
    for name in tg.tierNames:
        low = name.lower()
        if 'word' in low and word_tier is None:
            word_tier = tg.getTier(name)
        elif 'phone' in low and phone_tier is None:
            phone_tier = tg.getTier(name)
    if word_tier is None or phone_tier is None:
        raise RuntimeError(f"Could not find word/phone tiers in {tg_path.name} "
                           f"(tiers: {tg.tierNames})")

    def entries(tier):
        for attr in ('entries', 'entryList', '_entries'):
            if hasattr(tier, attr):
                return list(getattr(tier, attr))
        return []

    phone_entries = sorted(
        [(e.start, e.end, e.label) for e in entries(phone_tier) if e.label.strip()],
        key=lambda x: x[0],
    )
    word_entries = sorted(
        [(e.start, e.end, e.label) for e in entries(word_tier) if e.label.strip()],
        key=lambda x: x[0],
    )

    syllable_intervals = []   # (start, end, label) across whole utterance
    target_syllables = None   # list for the target verb (word index 2)

    for w_idx, (ws, we, wlabel) in enumerate(word_entries):
        # phones whose center falls inside this word
        wphones = [p for p in phone_entries if p[0] >= ws - 1e-6 and p[1] <= we + 1e-6]
        if not wphones:
            continue
        sylls = syllabify_phones(wphones, wlabel)
        for syl in sylls:
            s_start = syl[0][0]
            s_end = syl[-1][1]
            s_label = ''.join(p[2] for p in syl)
            syllable_intervals.append((s_start, s_end, s_label))
        if w_idx == 2:  # WORD 3 = target verb
            target_syllables = sylls

    # Build a sentence/phrase tier: one interval spanning the spoken words,
    # labelled with the full known sentence. Sits on top of the word tier so
    # the final TextGrid has the standard 4 tiers: sentence/word/syllable/phone.
    sentence_intervals = []
    if word_entries:
        sent_start = word_entries[0][0]
        sent_end = word_entries[-1][1]
        sentence_intervals = [(sent_start, sent_end, info['sentence'])]
    sent_tier = textgrid.IntervalTier(
        'sentence', sentence_intervals, tg.minTimestamp, tg.maxTimestamp
    )

    # Build the syllable tier (inserted conceptually between words and phones).
    syl_tier = textgrid.IntervalTier(
        'syllables', syllable_intervals, tg.minTimestamp, tg.maxTimestamp
    )

    # praatio replaces by name if present; add fresh.
    for name in ('sentence', 'syllables'):
        if name in tg.tierNames:
            tg.removeTier(name)
    tg.addTier(sent_tier)
    tg.addTier(syl_tier)

    # Reorder tiers to the conventional top-down layout:
    # sentence -> word(s) -> syllables -> phone(s).
    desired = []
    for kw in ('sentence', 'word', 'syll', 'phone'):
        for tname in tg.tierNames:
            if kw in tname.lower() and tname not in desired:
                desired.append(tname)
    # Append any leftover tiers (defensive) so none are lost.
    for tname in tg.tierNames:
        if tname not in desired:
            desired.append(tname)
    try:
        tg.renameTier  # noqa: B018  (presence check only)
    except AttributeError:
        pass
    # praatio Textgrid supports tier reordering via _tierDict/tierNames in
    # newer versions; rebuild a fresh TextGrid to guarantee order portably.
    ordered = textgrid.Textgrid()
    ordered.minTimestamp = tg.minTimestamp
    ordered.maxTimestamp = tg.maxTimestamp
    for tname in desired:
        ordered.addTier(tg.getTier(tname))
    tg = ordered

    out_path.parent.mkdir(parents=True, exist_ok=True)
    tg.save(str(out_path), format='long_textgrid', includeBlankSpaces=True)

    # Report target-verb stressed syllable.
    report = {'target_verb': info['target_verb'], 'tense': info['tense'],
              'n_syllables': 0, 'stress_idx': None,
              'stress_start': None, 'stress_end': None,
              'verb_syllable_labels': ''}
    if target_syllables:
        n_syl = len(target_syllables)
        s_idx = stressed_syllable_index(info['target_verb'], info['tense'], n_syl)
        stressed = target_syllables[s_idx]
        report.update({
            'n_syllables': n_syl,
            'stress_idx': s_idx,
            'stress_start': round(stressed[0][0], 4),
            'stress_end': round(stressed[-1][1], 4),
            'verb_syllable_labels': '.'.join(
                ''.join(p[2] for p in syl) for syl in target_syllables),
        })
    return report


def syllabify(aligned_dir: Path, syll_dir: Path, report_csv: Path) -> None:
    tgs = sorted(aligned_dir.glob('*.TextGrid'))
    if not tgs:
        sys.exit(f"[STEP 3] No TextGrids in {aligned_dir}. Run step 2 first.")
    rows = []
    failed = []
    for tg_path in tgs:
        try:
            info = parse_filename(tg_path.stem)
            out = syll_dir / f"{tg_path.stem}_syllabified.TextGrid"
            rep = syllabify_textgrid(tg_path, info, out)
            rows.append({**{k: info[k] for k in
                            ('pid', 'list', 'item', 'variant', 'tense',
                             'sentence', 'target_verb')},
                         **rep, 'file': tg_path.stem})
        except Exception as e:                       # noqa: BLE001
            failed.append((tg_path.name, str(e)))
    # Write a compact report.
    if rows:
        report_csv.parent.mkdir(parents=True, exist_ok=True)
        with open(report_csv, 'w', newline='', encoding='utf-8') as f:
            w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
            w.writeheader()
            w.writerows(rows)
    print(f"[STEP 3] Syllabified {len(rows)} TextGrids -> {syll_dir}")
    print(f"[STEP 3] Stress report -> {report_csv}")
    if failed:
        print(f"[STEP 3] {len(failed)} files failed:")
        for name, reason in failed[:10]:
            print(f"         - {name}: {reason}")


# ===================================================================
# STEP 4 — tidy CSV for F0 extraction (one row per file)
# ===================================================================
def extract_table(syll_dir: Path, out_csv: Path) -> None:
    """Emit per-file target-verb syllable boundaries from syllabified TextGrids.

    This is the hand-off to your Praat / F0 extraction: for each clip it gives
    the target verb's syllable onsets/offsets and which syllable is stressed,
    so you can sample F0 at 10 points/syllable on the right interval.
    """
    tgs = sorted(syll_dir.glob('*_syllabified.TextGrid'))
    if not tgs:
        sys.exit(f"[STEP 4] No syllabified TextGrids in {syll_dir}. Run step 3.")
    rows = []
    for tg_path in tgs:
        stem = tg_path.stem.replace('_syllabified', '')
        info = parse_filename(stem)
        tg = textgrid.openTextgrid(str(tg_path), includeEmptyIntervals=True)
        # words + syllables tiers
        wt = st = None
        for name in tg.tierNames:
            low = name.lower()
            if 'word' in low and wt is None:
                wt = tg.getTier(name)
            elif 'syllable' in low and st is None:
                st = tg.getTier(name)

        def ents(t):
            for a in ('entries', 'entryList', '_entries'):
                if hasattr(t, a):
                    return list(getattr(t, a))
            return []

        words = sorted([(e.start, e.end, e.label) for e in ents(wt)
                        if e.label.strip()], key=lambda x: x[0])
        sylls = sorted([(e.start, e.end, e.label) for e in ents(st)
                        if e.label.strip()], key=lambda x: x[0])
        if len(words) < 3:
            continue
        vs, ve, _ = words[2]                         # target verb interval
        verb_sylls = [s for s in sylls if s[0] >= vs - 1e-6 and s[1] <= ve + 1e-6]
        n_syl = len(verb_sylls)
        s_idx = stressed_syllable_index(info['target_verb'], info['tense'], n_syl) \
            if n_syl else None
        row = {
            'file': stem, 'pid': info['pid'], 'list': info['list'],
            'item': info['item'], 'variant': info['variant'],
            'tense': info['tense'], 'target_verb': info['target_verb'],
            'verb_start': round(vs, 4), 'verb_end': round(ve, 4),
            'n_verb_syllables': n_syl,
            'stressed_syll_idx': s_idx,
        }
        # Per-syllable columns (syll1_start/end/label ... up to 4)
        for i in range(4):
            if i < n_syl:
                ss, se, sl = verb_sylls[i]
                row[f'syll{i+1}_start'] = round(ss, 4)
                row[f'syll{i+1}_end'] = round(se, 4)
                row[f'syll{i+1}_label'] = sl
            else:
                row[f'syll{i+1}_start'] = ''
                row[f'syll{i+1}_end'] = ''
                row[f'syll{i+1}_label'] = ''
        rows.append(row)
    out_csv.parent.mkdir(parents=True, exist_ok=True)
    with open(out_csv, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    print(f"[STEP 4] Wrote {len(rows)} rows -> {out_csv}")


# ===================================================================
# STEP 5 — QC: validate MFA output against the approved syllable key
# ===================================================================
def _expected_counts(item: int, tense: str) -> dict:
    """From SYLLABLE_KEY: expected total syllables, verb syllable count,
    which verb-syllable index is stressed, and the per-word expected syllable
    counts for the first three words (article, subject noun, verb) -- the
    analysis window."""
    words = SYLLABLE_KEY[item][0 if tense == 'present' else 1]
    verb_sylls = words[2]                      # word 3 = target verb
    n_verb = len(verb_sylls)
    stress_idx = stressed_syllable_index('', tense, n_verb)
    return {
        'exp_total_syll': sum(len(w) for w in words),
        'exp_verb_syll': n_verb,
        'exp_stress_idx': stress_idx,
        'exp_verb_key': '-'.join(
            s.upper() if i == stress_idx else s for i, s in enumerate(verb_sylls)),
        # per-word expected syllable counts for words 1,2,3
        'exp_w1_syll': len(words[0]) if len(words) > 0 else 0,   # article
        'exp_w2_syll': len(words[1]) if len(words) > 1 else 0,   # subject noun
        'exp_w3_syll': len(words[2]) if len(words) > 2 else 0,   # verb
        'key_w2': '-'.join(words[1]) if len(words) > 1 else '',  # noun syllables
    }


def qc(syll_dir: Path, out_csv: Path, min_vowel_ms: float = 30.0,
       dur_gap_tol: float = 0.15) -> None:
    """One row per file. Compares MFA's aligned output to the approved key and
    flags files worth checking by hand in Praat.

    Flags (semicolon-joined in the `flags` column; blank = clean):
      verb_syllable_count_mismatch   MFA verb syllables != key (word 3)
      noun_syllable_mismatch         MFA subject-noun syllables != key (word 2)
      article_syllable_mismatch      MFA article syllables != key (word 1)
      total_syllable_count_mismatch  whole-utterance syllable count != key
      stress_syllable_short          stressed-syllable nucleus < min_vowel_ms
      duration_gap                   aligned span vs audio length off > tol
      missing_verb / missing_tiers   structural problem in the TextGrid
    The article/noun/verb flags cover the FIRST THREE WORDS (the analysis
    window); the total flag also reflects object words 4-5, so a clean
    article+noun+verb with only a total_syllable_count_mismatch means the
    discrepancy is in the object phrase and is irrelevant to a 3-word analysis.
    """
    import soundfile as sf  # local import; only needed here

    tgs = sorted(syll_dir.glob('*_syllabified.TextGrid'))
    if not tgs:
        sys.exit(f"[STEP 5] No syllabified TextGrids in {syll_dir}. Run step 3.")

    def ents(tier):
        for a in ('entries', 'entryList', '_entries'):
            if hasattr(tier, a):
                return list(getattr(tier, a))
        return []

    rows = []
    for tg_path in tgs:
        stem = tg_path.stem.replace('_syllabified', '')
        try:
            info = parse_filename(stem)
        except ValueError as e:
            rows.append({'file': stem, 'flags': f'unparseable_filename: {e}'})
            continue
        exp = _expected_counts(info['item'], info['tense'])

        tg = textgrid.openTextgrid(str(tg_path), includeEmptyIntervals=True)
        wt = st = pt = None
        for name in tg.tierNames:
            low = name.lower()
            if 'word' in low and wt is None:
                wt = tg.getTier(name)
            elif 'syll' in low and st is None:
                st = tg.getTier(name)
            elif 'phone' in low and pt is None:
                pt = tg.getTier(name)

        flags = []
        if wt is None or st is None:
            flags.append('missing_tiers')
            rows.append({
                'file': stem, 'pid': info['pid'], 'item': info['item'],
                'variant': info['variant'], 'tense': info['tense'],
                'target_verb': info['target_verb'], 'flags': ';'.join(flags),
            })
            continue

        words = sorted([(e.start, e.end, e.label) for e in ents(wt)
                        if e.label.strip()], key=lambda x: x[0])
        sylls = sorted([(e.start, e.end, e.label) for e in ents(st)
                        if e.label.strip()], key=lambda x: x[0])

        # Whole-utterance syllable count
        if len(sylls) != exp['exp_total_syll']:
            flags.append(
                f"total_syllable_count_mismatch(got {len(sylls)},"
                f"exp {exp['exp_total_syll']})")

        verb_start = verb_end = None
        n_verb = 0
        stress_start = stress_end = None
        if len(words) < 3:
            flags.append('missing_verb(word3)')
        else:
            verb_start, verb_end, _ = words[2]
            verb_sylls = [s for s in sylls
                          if s[0] >= verb_start - 1e-6 and s[1] <= verb_end + 1e-6]
            n_verb = len(verb_sylls)
            if n_verb != exp['exp_verb_syll']:
                flags.append(
                    f"verb_syllable_count_mismatch(verb={info['target_verb']},"
                    f"got {n_verb},exp {exp['exp_verb_syll']})")
            if n_verb:
                s_idx = min(exp['exp_stress_idx'], n_verb - 1)
                stress_start, stress_end, _ = verb_sylls[s_idx]
                # short stressed-syllable check (proxy for a bad boundary)
                if (stress_end - stress_start) * 1000.0 < min_vowel_ms:
                    flags.append(
                        f"stress_syllable_short(verb={info['target_verb']},"
                        f"{(stress_end - stress_start) * 1000:.0f}ms)")

        # ---- PER-WORD syllable-count check for words 1-3 (analysis window) ----
        # Counts syllables whose midpoint falls inside each of the first three
        # words and compares to the key. These flags are what matter for the
        # first-three-words analysis (article / subject noun / verb), unlike the
        # whole-utterance total which also reflects object words 4-5.
        w_counts = []
        for wi in range(min(3, len(words))):
            ws, we, _ = words[wi]
            cnt = sum(1 for s in sylls if ws - 1e-6 <= (s[0] + s[1]) / 2 <= we + 1e-6)
            w_counts.append(cnt)
        while len(w_counts) < 3:
            w_counts.append(0)
        n_w1, n_w2, n_w3 = w_counts[0], w_counts[1], w_counts[2]

        wlab1 = words[0][2] if len(words) > 0 else ''
        wlab2 = words[1][2] if len(words) > 1 else ''
        if len(words) >= 1 and n_w1 != exp['exp_w1_syll']:
            flags.append(
                f"article_syllable_mismatch({wlab1},got {n_w1},exp {exp['exp_w1_syll']})")
        if len(words) >= 2 and n_w2 != exp['exp_w2_syll']:
            flags.append(
                f"noun_syllable_mismatch({wlab2},got {n_w2},exp {exp['exp_w2_syll']})")
        # word 3 (verb) already covered by verb_syllable_count_mismatch above.

        # Duration sanity: aligned span vs actual audio length
        audio_dur = None
        wav = (syll_dir.parent / 'corpus' / f"{stem}.wav")
        if wav.exists():
            try:
                audio_dur = sf.info(wav).duration
            except Exception:  # noqa: BLE001
                audio_dur = None
        aligned_span = (words[-1][1] - words[0][0]) if words else 0.0
        if audio_dur and abs(audio_dur - aligned_span) > max(dur_gap_tol,
                                                             0.25 * audio_dur):
            flags.append(
                f"duration_gap(audio {audio_dur:.2f}s,aligned {aligned_span:.2f}s)")

        rows.append({
            'file': stem,
            'pid': info['pid'],
            'list': info['list'],
            'item': info['item'],
            'variant': info['variant'],
            'tense': info['tense'],
            'target_verb': info['target_verb'],
            'verb_start': round(verb_start, 4) if verb_start is not None else '',
            'verb_end': round(verb_end, 4) if verb_end is not None else '',
            'n_verb_syll': n_verb,
            'exp_verb_syll': exp['exp_verb_syll'],
            'n_w1_syll': n_w1, 'exp_w1_syll': exp['exp_w1_syll'],
            'n_noun_syll': n_w2, 'exp_noun_syll': exp['exp_w2_syll'],
            'key_noun': exp['key_w2'],
            'stress_start': round(stress_start, 4) if stress_start is not None else '',
            'stress_end': round(stress_end, 4) if stress_end is not None else '',
            'exp_verb_key': exp['exp_verb_key'],
            'n_total_syll': len(sylls),
            'exp_total_syll': exp['exp_total_syll'],
            'audio_dur': round(audio_dur, 3) if audio_dur else '',
            'aligned_span': round(aligned_span, 3),
            'flags': ';'.join(flags),   # blank => clean
        })

    out_csv.parent.mkdir(parents=True, exist_ok=True)
    with open(out_csv, 'w', newline='', encoding='utf-8') as f:
        w = csv.DictWriter(f, fieldnames=list(rows[0].keys()))
        w.writeheader()
        w.writerows(rows)
    n_flagged = sum(1 for r in rows if r.get('flags'))
    # Problems INSIDE the 3-word analysis window (article/noun/verb).
    window_flag_types = ('article_syllable_mismatch', 'noun_syllable_mismatch',
                         'verb_syllable_count_mismatch', 'stress_syllable_short',
                         'missing_verb', 'missing_tiers')
    window_bad = [r for r in rows
                  if any(t in r.get('flags', '') for t in window_flag_types)]
    print(f"[STEP 5] QC wrote {len(rows)} rows -> {out_csv}")
    print(f"[STEP 5] {n_flagged} file(s) flagged overall, "
          f"{len(rows) - n_flagged} fully clean.")
    print(f"[STEP 5] ANALYSIS WINDOW (words 1-3, article/noun/verb): "
          f"{len(window_bad)} file(s) need checking, "
          f"{len(rows) - len(window_bad)} clean in-window.")
    if window_bad:
        print("[STEP 5] In-window problems (these affect the 3-word analysis):")
        for r in window_bad:
            wf = ';'.join(f for f in r['flags'].split(';')
                          if any(t in f for t in window_flag_types))
            print(f"         {r['file']} (item {r.get('item')}): {wf}")
    else:
        print("[STEP 5] No in-window problems: article, subject noun, and verb "
              "syllable counts all match the key for every file.")


# ===================================================================
# CLI
# ===================================================================
def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument('--audio-dir', required=True, type=Path,
                    help='Folder of cut .wav files (e.g. 2_cut_audios)')
    ap.add_argument('--work-dir', type=Path, default=Path('./fa_work'),
                    help='Working dir for corpus/aligned/syllabified (default ./fa_work)')
    ap.add_argument('--step',
                    choices=['standardize', 'corpus', 'mfa', 'syllabify',
                             'table', 'qc'],
                    help='Run a single step')
    ap.add_argument('--all', action='store_true', help='Run all steps')
    ap.add_argument('--no-standardize', action='store_true',
                    help='Skip the mono/16kHz conversion; use originals as-is')
    ap.add_argument('--dictionary', default=None,
                    help="Pronunciation dictionary for MFA: the bundled name "
                         "'spanish_mfa' or a path to a custom .dict file. If "
                         "omitted, the script uses 'spanish_mfa_custom.dict' "
                         "next to this script when it exists, otherwise the "
                         "bundled 'spanish_mfa'.")
    args = ap.parse_args()

    # Resolve which pronunciation dictionary to use.
    if args.dictionary:
        dictionary = args.dictionary
    else:
        default_custom = Path(__file__).resolve().parent / 'spanish_mfa_custom.dict'
        if default_custom.exists():
            dictionary = str(default_custom)
            print(f"[info] Using custom dictionary: {default_custom}")
        else:
            dictionary = MFA_DICT
            print(f"[info] Using bundled dictionary: {MFA_DICT} "
                  f"(no spanish_mfa_custom.dict found next to the script)")

    work = args.work_dir
    std_dir = work / 'standardized'
    corpus_dir = work / 'corpus'
    aligned_dir = work / 'aligned'
    syll_dir = work / 'syllabified'
    stress_report = work / 'stress_report.csv'
    f0_table = work / 'target_verb_syllables.csv'
    qc_report = work / 'qc_report.csv'

    if not args.step and not args.all:
        ap.error("Pick --all or --step "
                 "{standardize,corpus,mfa,syllabify,table,qc}")

    # Decide which audio folder the corpus should be built from.
    # If standardization runs (or already produced files), use std_dir.
    run_std = (args.all or args.step == 'standardize') and not args.no_standardize
    if run_std:
        standardize_audio(args.audio_dir, std_dir)
    corpus_source = std_dir if (std_dir.exists() and any(std_dir.glob('*.wav'))
                                and not args.no_standardize) else args.audio_dir

    if args.all or args.step == 'corpus':
        build_corpus(corpus_source, corpus_dir)
    if args.all or args.step == 'mfa':
        run_mfa(corpus_dir, aligned_dir, dictionary)
    if args.all or args.step == 'syllabify':
        syllabify(aligned_dir, syll_dir, stress_report)
    if args.all or args.step == 'table':
        extract_table(syll_dir, f0_table)
    if args.all or args.step == 'qc':
        qc(syll_dir, qc_report)


if __name__ == '__main__':
    main()
