#!/bin/bash
alr build

Push-Location source
New-Item -ItemType Directory -Path output -Force
../.obj/arm_form aa-aarm.msm Ada_Lang_IO New-Only 5 5 "D:\dev\ada\ada-lang-io\docs\arm\"
# ../.obj/arm_form aa-aarm.msm Tracer New-Changes 5 5 "D:\dev\ada\ada-lang-io\docs\arm\"
# ../.obj/arm_form aa-aarm.msm Ada_Lang_IO New-Changes 5
Pop-Location

# These counts are consistent between changes in parameters
# run of the program.
#
# ../.obj/arm_form aa-aarm.msm Tracer New-Changes 5
# Category Header :  1443
# Clause Header :  345
# Clause Reference :  3350
# Close :  1
# Create :  1
# End Hand Item :  363
# End Paragraph :  14626
# Hard Space :  98070
# Index Line Break :  3916
# Index Reference :  7654
# Index Target :  6377
# Line Break :  6674
# Local Link :  71381
# Local Link End :  476
# Local Link Start :  476
# Local Target :  278
# New column :  7
# Ordinary Character :  1882815
# Ordinary Text :  16345
# Section :  32
# Separator line :  1
# SetColumns :  20
# Soft hyphen break :  1599
# Soft line break :  8
# Special character :  2690
# Start Paragraph :  14626
# Start table :  4
# TOC Marker :  2
# Tab :  2240
# Table Marker :  152
# Text Format :  54828
