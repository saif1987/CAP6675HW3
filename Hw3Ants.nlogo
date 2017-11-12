patches-own [
  chemical             ;; amount of chemical on this patch
  food-or-larvae?                 ;; amount of food on this patch (0, 1, or 2)
  food-or-larvae-amount
  nest?                ;; true on nest patches, false elsewhere
  forage?
  path?
  nest-entrance?
]

turtles-own [
  previous-steps
  cur-prev-step
  own-brood-forage-points
  own-brood-points
  own-forage-points
  normal-color
  brood-worker?
  foraging-threshold
  brood-bucket
  forage-bucket
]

globals [
  nest-entrance-center-x
  nest-entrance-center-y

  total-forage-buckets
  total-brood-buckets

  brood-points
  forage-points

  Pct-Forager-Count
  Pct-Forage-Point-by-Foragers
  Pct-Brood-Point-by-Foragers

  brood-colors
  forage-colors
  infile
]

;;;;;;;;;;;;;;;;;;;;;;;;
;;; Setup procedures ;;;
;;;;;;;;;;;;;;;;;;;;;;;;
to setup-special

end
to setup
  let temp infile
  clear-all
  set temp infile
  set-default-shape turtles "bug"
  set nest-entrance-center-x 0
  set nest-entrance-center-y (min-pycor + (nest-size))
  set brood-points 1
  set forage-points 1

  set brood-colors (list (gray + 4) (orange + 3) (orange + 2) (orange + 1) (orange))
  set forage-colors (list (gray + 4) (turquoise + 3) (turquoise + 2) (turquoise + 1) (turquoise))

  create-turtles population
  [ set size 2         ;; easier to see
    set normal-color gray + 4
    set color normal-color
    set brood-worker? random-float 100.0 > initial-forage-assignment-rate
    let here-patch patch-here
    reset-previous-steps
    set cur-prev-step 0
    move-to patch-at nest-entrance-center-x nest-entrance-center-y]  ;; red = not carrying food
  setup-patches
  reset-ticks
end

to setup-patches
  ask patches
  [ set path? false
    set food-or-larvae? false
    setup-nest
    recolor-patch ]
end

to setup-nest  ;; patch procedure
  ;; set nest? variable to true inside the nest, false elsewhere
  set nest-entrance? (distancexy nest-entrance-center-x nest-entrance-center-y) < 5
  if (nest-entrance?)
  [set path? true]
  set nest? (pycor <= nest-entrance-center-y) or nest-entrance?
  set forage? not nest? or nest-entrance?
end

to recolor-patch  ;; patch procedure
  ;; give color to nest and food sources

  ifelse nest-entrance?
  [set pcolor violet]
  [ifelse food-or-larvae?
    [ if nest?
      [ set pcolor 25]
      if forage?
      [ set pcolor 5]]
    ;; scale color to show chemical concentration
    [ if path?
      [ if forage? [set pcolor 66]
        if nest? [set pcolor 36]]
        ifelse(chemical > 1)
        [set pcolor scale-color magenta chemical 0.1 5
        if pcolor > 129 [set pcolor pcolor - 2]]
        [ set chemical 0]]
      if not path? and (chemical <= 0 or pcolor < 122)
       [if nest? [set pcolor 33]
        if forage? [set pcolor 52]]]
end

;;;;;;;;;;;;;;;;;;;;;
;;; Go procedures ;;;
;;;;;;;;;;;;;;;;;;;;;

to go  ;; forever button
  set-buckets
  ask turtles
  [ if who >= ticks [ stop ] ;; delay initial departure
    ifelse not (color = black or color = yellow)
    [ look-for-food ]
    [ return-to-nest ]]
  ask patches
  [ set chemical chemical * (100 - evaporation-rate) / 100  ;; slowly evaporate chemical
    recolor-patch ]


   set Pct-Forager-Count 100.0 * (count turtles with [not brood-worker?]) / (count turtles)
    if (sum [own-forage-points] of turtles) > 0
    [set Pct-Forage-Point-by-Foragers 100 *  (sum [own-forage-points] of turtles with [not brood-worker?]) / (sum [own-forage-points] of turtles)]
    if (sum [own-brood-points] of turtles) > 0
    [set Pct-Brood-Point-by-Foragers 100 *  (sum [own-brood-points] of turtles with [not brood-worker?]) / (sum [own-brood-points] of turtles)]

  ifelse ticks <= 5000
  [tick]
  [stop]
end

to set-buckets
  set total-forage-buckets 0
  set total-brood-buckets 0
  let forage-ordered sort-on [own-brood-forage-points] turtles with [own-brood-forage-points >= 0]
  let brood-ordered sort-on [(- own-brood-forage-points)] turtles with [own-brood-forage-points < 0]

  let cur-size-forage 0
  foreach forage-ordered
  [cur ->
    ask cur
    [ set brood-bucket 0
      ifelse own-brood-forage-points = 0
      [ set forage-bucket 0 ]
      [ ifelse own-brood-forage-points > cur-size-forage
       [ set total-forage-buckets total-forage-buckets + 1
         set forage-bucket total-forage-buckets
         set cur-size-forage cur-size-forage + 1]
        [ set forage-bucket total-forage-buckets]]]
  ]
  let cur-size-brood 0
  foreach brood-ordered
  [cur ->
    ask cur
    [ set forage-bucket 0
      ifelse own-brood-forage-points < cur-size-brood
      [ set total-brood-buckets total-brood-buckets + 1
        set brood-bucket total-brood-buckets
        set cur-size-brood cur-size-brood - 1]
      [ set brood-bucket total-brood-buckets]]
  ]
end

to reset-previous-steps
  let center-patch (patch-at nest-entrance-center-x nest-entrance-center-y)
  set previous-steps list center-patch center-patch
end

to return-to-nest  ;; turtle procedure
  ifelse nest-entrance?
  [ ;; drop food and head out again
    reset-previous-steps
    set cur-prev-step 0
    if not (color = black)
    [ifelse brood-worker?
     [ set own-brood-forage-points  own-brood-forage-points - 1
       set own-brood-points own-brood-points + 1
       set brood-points brood-points + 1
       if Threshold-Change-Scheme !=  "No-Change"
       [
          ifelse Threshold-Change-Scheme =  "Flat-Change" ;;we change threshold by 3
          [set foraging-threshold foraging-threshold - 3.0]
          [set foraging-threshold foraging-threshold - (10.0 / (own-brood-points + own-forage-points))]
          if foraging-threshold < 25.0
          [set foraging-threshold 25.0]
          if foraging-threshold > 75.0
          [set foraging-threshold 75.0]
       ]

     ]
     [ set own-brood-forage-points own-brood-forage-points + 1
       set own-forage-points own-forage-points + 1
       set forage-points forage-points + 1
       if (Threshold-Change-Scheme !=  "No-Change")
       [
          ifelse Threshold-Change-Scheme =  "Flat-Change" ;;we change threshold by 3
          [set foraging-threshold foraging-threshold + 3.0]
          [set foraging-threshold foraging-threshold + (10.0 / (own-brood-points + own-forage-points))]
          if foraging-threshold < 25.0
          [set foraging-threshold 25.0]
          if foraging-threshold > 75.0
          [set foraging-threshold 75.0]
       ]

     ]]
    brood-or-forage-worker
    move-to patch-at nest-entrance-center-x nest-entrance-center-y
    set color normal-color ]
  [ let chem-amount 5000
    if not (color = black)
    [set chemical chemical + chem-amount]  ;; drop some chemical
    if (cur-prev-step <= 0)
    [ set cur-prev-step length previous-steps - 1]
    let cont-loop true
    while [cont-loop]
    [ set cur-prev-step cur-prev-step - 1
      if cur-prev-step <= 0
      [ set cur-prev-step 0
        set cont-loop false ]
      let next-patch item cur-prev-step previous-steps
      let ant-color color
      ask patch-here
      [ if not (next-patch = nobody)
        [ if (distance next-patch >= return-speed)
          [set cont-loop false
           if not (ant-color = black)
           [  set chemical chemical + chem-amount
              set chem-amount chem-amount * .50]]]]]
    set heading towards item cur-prev-step previous-steps
  move-to item cur-prev-step previous-steps]
end

to brood-or-forage-worker
  if total-brood-buckets < 8
  [ set total-brood-buckets 8]
  if total-forage-buckets < 8
  [ set total-forage-buckets 8]

  let change-to-brood? false
  let change-to-forage? false

  ;;set brood-worker? ((random-float 100.0) > (100 * (brood-points / (brood-points + forage-points))))
  let colony-need (100 * (brood-points / (brood-points + forage-points))) ;;foraging need
  let brooding-threshold 100.0 - foraging-threshold
  ifelse brood-worker? = true
  [
    if colony-need > foraging-threshold
    [set change-to-forage? true
      if brood-bucket = 0
      [set brood-worker? false ]
    ]
  ]
  [
    if colony-need < brooding-threshold
    [set change-to-brood? true
       if forage-bucket = 0
      [set brood-worker? true ]
    ]
  ]

  if brood-bucket > 0 and change-to-forage? = true
  [ let col-for-bucket total-brood-buckets / 4
    let extra total-brood-buckets mod 4
    let bucket-color-i 0
    let cont true
    while [cont][
      if brood-bucket <= (extra + (bucket-color-i * col-for-bucket))
      [ set normal-color item bucket-color-i brood-colors
        set cont false ]
      set bucket-color-i bucket-color-i + 1
    ]
    let stay-brood (max-resistance / brood-bucket * total-brood-buckets)
    if random-float 100.0 > stay-brood
    [ set brood-worker? false]
  ]


  if forage-bucket > 0 and change-to-brood? = true
  [ let col-for-bucket total-forage-buckets / 4
    let extra total-forage-buckets mod 4
    let bucket-color-i 0
    let cont true
    while [cont] [
      if forage-bucket <= (extra + (bucket-color-i * col-for-bucket))
      [ set normal-color item bucket-color-i forage-colors
        set cont false ]
      set bucket-color-i bucket-color-i + 1
    ]
    let stay-forage (max-resistance / forage-bucket * total-forage-buckets)
    if random-float 100.0 > stay-forage
    [ set brood-worker? true]]
end

to look-for-food  ;; turtle procedure
  if food-or-larvae? = true
  [ set color yellow    ;; pick up food
    set food-or-larvae-amount food-or-larvae-amount - 1        ;; and reduce the food source
    if food-or-larvae-amount <= 0 [set food-or-larvae? false]
    rt 180                   ;; and turn around
    stop ]
  ;; go in the direction where the chemical smell is strongest
  ;;if (chemical >= 0.05) and (chemical < 2)
  uphill-chemical
end

;; sniff left and right, and go where the strongest smell is
to uphill-chemical  ;; turtle procedure
  let scent-list [0 -20 -45 -60 -20 45 60]
  if nest-entrance?
  [set scent-list [0 45 -45 75 -75 90 -90 120 -120 145 -145 180]]

  let largest -1
  let largest-angle 0
  foreach scent-list
  [ x ->
     let chem-num (chemical-scent-at-angle x)
     if chem-num > largest
     [ set largest chem-num
       set largest-angle x]]
  ifelse largest <= 0
  [wiggle true]
  [rt largest-angle
   wiggle false]
end

to wiggle [move?] ;; turtle procedure
  ifelse move?
  [ rt random 40
    lt random 40]
  [ rt random 2
    lt random 2]
  let rotate false
  let is-brood-worker brood-worker?
  ifelse (not can-move? 1)
  [ set rotate true ]
  [ ask patch-ahead 1
    [ ifelse (not path?) and (not (food-or-larvae?))
      [ set rotate true ]
      [ ifelse is-brood-worker
        [ifelse nest?
          [set rotate false]
          [set rotate true]]
        [ifelse forage?
          [set rotate false]
          [set rotate true]] ]]] ;; set rotate false
  ifelse rotate and move?
  [ let rand random 75
    set rand rand * one-of [-1 1]
    rt rand]
  [
    ifelse ( position (patch-ahead 1) previous-steps ) != false
    [
      let endpos (position (patch-ahead 1) previous-steps) + 1
      set previous-steps (sublist previous-steps 0 endpos)
    ]
    [
    set previous-steps lput (patch-ahead 1) previous-steps
    ]
    if (length previous-steps >= max-steps)
    [ set color black]
    fd 1]
end

to-report chemical-scent-at-angle [angle]
  let i 0
  let p patch-right-and-ahead angle 1
  let scent-total 0
  loop [
    ifelse p = nobody or not [path?] of p [report scent-total]
    [if ((brood-worker? and [nest?] of p) or (not brood-worker? and [forage?] of p) and not [nest-entrance?] of p)
    [ifelse [food-or-larvae?] of p
    [ set scent-total scent-total + 9999999]
    [ set scent-total scent-total + [chemical] of p]]]

    set p patch-right-and-ahead angle 1
    set i i + 1
    if (i > smell-range) [report scent-total]
  ]
end

to draw-path
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
        [if (not food-or-larvae?)
        [ set path? true
          set pcolor yellow ]
          ask neighbors
          [ if (not food-or-larvae?)
            [set path? true
             set pcolor yellow]]]
      display ]
end

to draw-food
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [ set path? true
        set food-or-larvae-amount food-amount
        set food-or-larvae? true
        set pcolor pink]
    display]
end

to erase-path
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
        [if (not food-or-larvae?)
        [ set path? false
          set pcolor white ]
          ask neighbors
          [ if (not food-or-larvae?)
            [set path? false
             set pcolor white]]]
      display ]
end

to erase-food
  while [mouse-down?]
    [ ask patch mouse-xcor mouse-ycor
      [ set food-or-larvae? false
        set pcolor white]
    display]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  simulation saving procedures  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; saves patch info, turtle info, and user settings to file
to save-scenario
  let file user-new-file
  if (is-string? file) [
    if (file-exists? file) [
    file-close
    file-delete file
    ]
    file-open file
    write-scenario
    file-close
  ]
end

;; called from save-scenario to write stuff to file
to write-scenario
  ;; write tick number
  file-print ticks

  ;; write view dimensions
  file-print min-pycor
  file-print max-pycor
  file-print min-pxcor
  file-print max-pycor

  ;; write globals
  file-print nest-entrance-center-x
  file-print nest-entrance-center-y
  file-print brood-points
  file-print forage-points
  file-print brood-colors
  file-print forage-colors

  ;; write every patch's info
  let yctr min-pycor
  while [yctr <= max-pycor] [
    let xctr min-pxcor
    while [xctr <= max-pxcor] [
      file-print get-patch-str yctr xctr
      set xctr (xctr + 1)
    ]
    set yctr (yctr + 1)
  ]

  ;; write every turtle's info
  let num-ants count turtles
  file-print num-ants
  let ant-ctr 0
  while [ant-ctr < num-ants] [
    file-print get-ant-str ant-ctr
    set ant-ctr (ant-ctr + 1)
  ]
end

;; reports a string with all attributes of specified patch
to-report get-patch-str [y-num x-num]
  let cur-patch (patch y-num x-num)
  report (word
    ([chemical] of cur-patch) " "
    ([food-or-larvae?] of cur-patch) " "
    ([food-or-larvae-amount] of cur-patch) " "
    ([nest?] of cur-patch) " "
    ([forage?] of cur-patch) " "
    ([path?] of cur-patch) " "
    ([nest-entrance?] of cur-patch)
  )
end

;; reports a string with all attributes of specified turtle
to-report get-ant-str [who-num]
  show who-num
  let cur-ant turtle who-num

  ;; must convert previous-steps list into a string
  let p-ctr 0
  let prev-steps-word ""
  let prev-steps [previous-steps] of cur-ant
  while [p-ctr < length prev-steps] [
    set prev-steps-word (word
      prev-steps-word " "
      [pxcor] of (item p-ctr prev-steps) " "
      [pycor] of (item p-ctr prev-steps)
    )
    set p-ctr (p-ctr + 1)
  ]

  report (word
    [size] of cur-ant " "
    [color] of cur-ant " "
    [xcor] of cur-ant " "
    [ycor] of cur-ant " "
    length prev-steps " "
    prev-steps-word " "
    [cur-prev-step] of cur-ant " "
    [own-brood-forage-points] of cur-ant " "
    [own-brood-points] of cur-ant " "
    [own-forage-points] of cur-ant " "
    [brood-worker?] of cur-ant " "
    [brood-bucket] of cur-ant " "
    [forage-bucket] of cur-ant
  )
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  simulation loading procedures  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; loads patch info, turtle info, and user settings from file
to load-scenario
  set infile user-file
  let temp infile
  if (infile != false) [
    file-open infile
    init-sim-from-file
    file-close
  ]
  set infile temp
end

to reload-scenario
  let temp infile
  if (infile != false) [
    file-open infile
    init-sim-from-file
    file-close
  ]
  set infile temp
end


;; called from load-scenario to initialize simulation from stuff from file
to init-sim-from-file
  clear-all
  set-default-shape turtles "bug"
  check-header
  init-globals
  init-patches-from-file
  Init-Ants
  ;;init-turtles-from-file
end

;; called from load-scenario to check if file info from file matches current sim
to check-header
  reset-ticks
  tick-advance file-read
  let min-y file-read
  let max-y file-read
  let min-x file-read
  let max-x file-read
  if (min-y != min-pycor
    or max-y != max-pycor
    or min-x != min-pxcor
    or max-x != max-pxcor) [
    user-message "View size of file scenario doesn't match current view"
    stop
  ]
end

to init-globals
  set nest-entrance-center-x file-read
  set nest-entrance-center-y file-read
  set brood-points file-read
  set forage-points file-read
  set brood-colors file-read
  set forage-colors file-read
end

;; initialize patches from file
to init-patches-from-file
  let yctr min-pycor
  while [yctr <= max-pycor] [
    let xctr min-pxcor
    while [xctr <= max-pxcor] [
      ;; initialize patch
      ask patch yctr xctr [
        set chemical file-read
        set food-or-larvae? file-read
        set food-or-larvae-amount file-read
        set nest? file-read
        set forage? file-read
        set path? file-read
        set nest-entrance? file-read
      ]
      set xctr (xctr + 1)
    ]
    set yctr (yctr + 1)
  ]
  ask patches [
    recolor-patch
  ]
end

;; initialize turtles from file
to init-turtles-from-file
  let num-ants file-read
  create-turtles num-ants
  let ant-ctr 0
  while [ant-ctr < num-ants] [
    show ant-ctr
    ;; initialize turtle
    ask turtle ant-ctr [
      set size file-read
      set color file-read
      setxy file-read file-read
      ;set previous-steps file-read
      let num-steps file-read
      set previous-steps list (patch file-read file-read) (patch file-read file-read)
      while [num-steps > 2] [
        ;set previous-steps list center-patch center-patch
        ;set previous-steps lput (patch-ahead 1) previous-steps
        set previous-steps lput (patch file-read file-read) previous-steps
        set num-steps (num-steps - 1)
      ]
      set cur-prev-step file-read
      set own-brood-forage-points file-read
      set own-brood-points file-read
      set own-forage-points file-read
      set brood-worker? file-read
      set brood-bucket file-read
      set forage-bucket file-read
    ]
    set ant-ctr (ant-ctr + 1)
  ]
end

;;Init Ants based on Threshold Mode
to Init-Ants
  clear-turtles
  create-turtles population
  [
    set size 2         ;; easier to see
    set normal-color gray + 4
    set color normal-color
    set brood-worker? random-float 100.0 > initial-forage-assignment-rate
    let here-patch patch-here
    reset-previous-steps
    set cur-prev-step 0
    move-to patch-at nest-entrance-center-x nest-entrance-center-y

    ifelse Initial-Foraging-Threshold = "Random"
    [
      set foraging-threshold random-float 100.0
    ]
    [
      ifelse Initial-Foraging-Threshold = "50%"
      [
        set foraging-threshold 50.0
      ]
      [
        set foraging-threshold 75.0
      ]
    ]

  ]
end

; Copyright 1997 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
257
10
762
516
-1
-1
7.0
1
10
1
1
1
0
0
0
1
-35
35
-35
35
1
1
1
ticks
30.0

BUTTON
78
71
158
104
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
31
141
221
174
evaporation-rate
evaporation-rate
0.0
99.0
7.0
1.0
1
NIL
HORIZONTAL

BUTTON
169
70
244
103
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
0

SLIDER
31
36
221
69
population
population
0.0
1000
1000.0
1.0
1
NIL
HORIZONTAL

BUTTON
800
31
890
65
NIL
draw-path
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
776
126
832
186
nest-size
35.0
1
0
Number

BUTTON
916
31
1006
65
NIL
draw-food
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
838
125
898
185
max-steps
500.0
1
0
Number

BUTTON
8
72
72
106
next
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
27
180
227
330
Points
ticks
points
0.0
10.0
0.0
10.0
true
false
"" ""
PENS
"brood" 1.0 0 -6459832 true "" "plot brood-points"
"forage" 1.0 0 -13840069 true "" "plot forage-points"

MONITOR
26
334
110
379
NIL
brood-points
17
1
11

MONITOR
116
335
204
380
NIL
forage-points
17
1
11

BUTTON
802
86
896
120
NIL
erase-path
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
919
84
1013
118
NIL
erase-food
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

INPUTBOX
899
125
973
185
food-amount
1000.0
1
0
Number

INPUTBOX
847
416
922
476
return-speed
1.0
1
0
Number

BUTTON
9
390
114
423
NIL
load-scenario
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
9
430
115
463
NIL
save-scenario
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
770
481
983
514
initial-forage-assignment-rate
initial-forage-assignment-rate
0
100
50.0
1
1
NIL
HORIZONTAL

PLOT
926
361
1186
481
# Ants in Brood vs Forage
ticks
Brood vs  Forage
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"brood" 1.0 0 -955883 true "" "plot count turtles with [brood-worker?]"
"forage" 1.0 0 -13840069 true "" "plot count turtles with [not brood-worker?]"

INPUTBOX
771
418
843
478
smell-range
5.0
1
0
Number

SLIDER
985
481
1157
514
max-resistance
max-resistance
0
100
50.0
1
1
NIL
HORIZONTAL

CHOOSER
1031
13
1168
58
Threshold-Change-Scheme
Threshold-Change-Scheme
"No-Change" "Flat-Change" "Gradual-Change"
2

CHOOSER
1032
63
1198
108
Initial-Foraging-Threshold
Initial-Foraging-Threshold
"75%" "Random" "50%"
2

BUTTON
1032
113
1112
146
Init-Ants
Init-Ants
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
778
195
1184
355
%number of Foragers and %Forage point by Foragers
NIL
NIL
0.0
10.0
0.0
100.0
true
true
"" ""
PENS
"%Forager" 1.0 0 -14439633 true "" "Plot Pct-Forager-Count"
"%FPoints" 1.0 0 -817084 true "" "Plot Pct-Forage-Point-by-Foragers"
"%BPoints" 1.0 0 -13345367 true "" "Plot Pct-Brood-Point-by-Foragers"

MONITOR
1115
264
1182
309
%Foragers
Pct-Forager-Count
1
1
11

MONITOR
1115
312
1181
357
%Fpoints
Pct-Forage-Point-by-Foragers
1
1
11

BUTTON
138
389
204
422
reload
reload-scenario
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

In this project, a colony of ants forages for food. Though each ant follows a set of simple rules, the colony as a whole acts in a sophisticated way.

## HOW IT WORKS

When an ant finds a piece of food, it carries the food back to the nest, dropping a chemical as it moves. When other ants "sniff" the chemical, they follow the chemical toward the food. As more ants carry food to the nest, they reinforce the chemical trail.

## HOW TO USE IT

Click the SETUP button to set up the ant nest (in violet, at center) and three piles of food. Click the GO button to start the simulation. The chemical is shown in a green-to-white gradient.

The EVAPORATION-RATE slider controls the evaporation rate of the chemical. The DIFFUSION-RATE slider controls the diffusion rate of the chemical.

If you want to change the number of ants, move the POPULATION slider before pressing SETUP.

## THINGS TO NOTICE

The ant colony generally exploits the food source in order, starting with the food closest to the nest, and finishing with the food most distant from the nest. It is more difficult for the ants to form a stable trail to the more distant food, since the chemical trail has more time to evaporate and diffuse before being reinforced.

Once the colony finishes collecting the closest food, the chemical trail to that food naturally disappears, freeing up ants to help collect the other food sources. The more distant food sources require a larger "critical number" of ants to form a stable trail.

The consumption of the food is shown in a plot.  The line colors in the plot match the colors of the food piles.

## EXTENDING THE MODEL

Try different placements for the food sources. What happens if two food sources are equidistant from the nest? When that happens in the real world, ant colonies typically exploit one source then the other (not at the same time).

In this project, the ants use a "trick" to find their way back to the nest: they follow the "nest scent." Real ants use a variety of different approaches to find their way back to the nest. Try to implement some alternative strategies.

The ants only respond to chemical levels between 0.05 and 2.  The lower limit is used so the ants aren't infinitely sensitive.  Try removing the upper limit.  What happens?  Why?

In the `uphill-chemical` procedure, the ant "follows the gradient" of the chemical. That is, it "sniffs" in three directions, then turns in the direction where the chemical is strongest. You might want to try variants of the `uphill-chemical` procedure, changing the number and placement of "ant sniffs."

## NETLOGO FEATURES

The built-in `diffuse` primitive lets us diffuse the chemical easily without complicated code.

The primitive `patch-right-and-ahead` is used to make the ants smell in different directions without actually turning.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Wilensky, U. (1997).  NetLogo Ants model.  http://ccl.northwestern.edu/netlogo/models/Ants.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 1997 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

This model was created as part of the project: CONNECTED MATHEMATICS: MAKING SENSE OF COMPLEX PHENOMENA THROUGH BUILDING OBJECT-BASED PARALLEL MODELS (OBPML).  The project gratefully acknowledges the support of the National Science Foundation (Applications of Advanced Technologies Program) -- grant numbers RED #9552950 and REC #9632612.

This model was developed at the MIT Media Lab using CM StarLogo.  See Resnick, M. (1994) "Turtles, Termites and Traffic Jams: Explorations in Massively Parallel Microworlds."  Cambridge, MA: MIT Press.  Adapted to StarLogoT, 1997, as part of the Connected Mathematics Project.

This model was converted to NetLogo as part of the projects: PARTICIPATORY SIMULATIONS: NETWORK-BASED DESIGN FOR SYSTEMS LEARNING IN CLASSROOMS and/or INTEGRATED SIMULATION AND MODELING ENVIRONMENT. The project gratefully acknowledges the support of the National Science Foundation (REPP & ROLE programs) -- grant numbers REC #9814682 and REC-0126227. Converted from StarLogoT to NetLogo, 1998.

<!-- 1997 1998 MIT -->
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
