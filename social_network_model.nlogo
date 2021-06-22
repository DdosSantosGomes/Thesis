extensions [ nw ]

undirected-link-breed [ friendships friendship ]
undirected-link-breed [ acquaintanceships acquaintanceship ]

turtles-own [
  p
  q
  r

  p2
  q2
  r2

  rig-influencer
  flex-influencer
]

globals [
  is-connected

  fdist
  acdist

  components
  comp-layers
  consensus-list
  X
  Y

  rig-inf-list
  flex-inf-list
  rig-inf-pos
  flex-inf-pos

  layers
  next-layer-exists
  agents-left

  contr
  is-polarized
]

to setup
  clear-all
  set-default-shape turtles "circle"

  generate-network
  set-positions
  set-link-colors
  set-dists

  ifelse network-type = "fully connected"
  [ set is-connected true ]
  [ set is-connected false ]


  set is-polarized false
  set contr false
  set components []

  reset-ticks

end

to go
  identify-influencers
  visualize-influencers

  get-inf-lists
  get-inf-positions

  perform-social-influence
  perform-friendship-selection

  perform-balance-measure

  if is-polarized [
    visualize-polarization

    find-components
    check-consensus
  ]

  tick
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SETUP

;; Report the maximum number of links that can be added
;; to a random network, given the specified number
;; of nodes, with 1000 as an arbitrary upper bound
to-report max-links
  report min (list (num-turtles * (num-turtles - 1) / 2) 1000)
end

; Generates chosen network, generates acquaintanceships by default
to generate-network
  (ifelse
    network-type = "ring" [
      nw:generate-ring turtles acquaintanceships 10 [
        set size 2
        set color blue
        layout-circle sort turtles 10
      ]
    ]

    network-type = "star" [
      nw:generate-star turtles acquaintanceships 10 [
        set size 2
        set color blue
        layout-radial turtles links turtle 0
      ]
    ]

    network-type = "double star" [
      nw:generate-star turtles acquaintanceships 5 [
        set size 2
        set color blue
        layout-radial turtles acquaintanceships turtle 0
      ]
      create-turtles 5 [
        set size 2
        set color blue
        foreach [0 6 7 8 9] [
          this-turtle ->
          ask turtle 5 [ create-acquaintanceship-with turtle this-turtle ]
        ]
        layout-radial turtles links turtle 5
      ]
    ]

    network-type = "fully connected" [
      create-turtles 10 [
        set size 2
        set color blue
        create-acquaintanceships-with other turtles
        layout-circle sort turtles 10
      ]
    ]

    network-type = "Erdös-Rényi random network" [
      ;; This is the classic Erdős-Rényi random network.
      ;; It uses WHILE to ensure we get NUM-LINKS links.

      create-turtles num-turtles [
      set shape "circle"
      set size 2
      set color blue
      setxy random-xcor random-ycor
      ]

      if num-links > max-links [ set num-links max-links ]

      while [ count links < num-links ] [

        ask one-of turtles [
          create-acquaintanceship-with one-of other turtles
        ]

      ]
    ])
end

; Assigns positions regarding p, q, r to agents
to set-positions
  ask turtles [
    set p random 2
    set q random 2
    set r random 2
    set label (list p q r)
  ]
end

; Colours links
to set-link-colors
  ask acquaintanceships [ set color red ]
  ask friendships [ set color green ]
end

; Defines distances required for friendship and acquaintanceship
to set-dists
  (ifelse
    theta-values = "configuration 3" [
      set fdist (list 0)
      set acdist (list 1)
    ]

    theta-values = "configuration 2" [
      set fdist (list 0)
      set acdist (list 1 2)
    ]

    theta-values = "configuration 1" [
      set fdist (list 0 1)
      set acdist (list 2)
    ]
  )
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; FRIENDSHIP SELECTION UPDATE

; Returns distance (number of differing propositions) between two agents
to-report dist [ list1 list2 ]
  let similarity (map [ [a b] -> a = b] list1 list2)
  report length (filter [ i -> i = false] similarity)
end

; Kills acquaintanceship between two agents if it exists
to kill-acquaintanceship [ agent1 agent2 ]
  let who1 [who] of agent1
  let who2 [who] of agent2
  if acquaintanceship who1 who2 != nobody [ ask acquaintanceship who1 who2 [ die ] ]
end

; Kills friendship between two agents if it exists
to kill-friendship [ agent1 agent2 ]
  let who1 [who] of agent1
  let who2 [who] of agent2
  if friendship who1 who2 != nobody [ ask friendship who1 who2 [ die ] ]
end

; Updates relation between two agents given distance
to update-relation [ agent1 agent2 ]
  let list1 (list [p] of agent1 [q] of agent1 [r] of agent1)
  let list2 (list [p] of agent2 [q] of agent2 [r] of agent2)
  let d dist list1 list2

  (ifelse
    member? d fdist [ ; Distance small enough for friendship
      ask agent1 [
        create-friendship-with agent2 [ set color green ]
        kill-acquaintanceship self agent2
      ]
    ]

    member? d acdist [ ; Distance small enough for acquaintanceship
      ask agent1 [
        create-acquaintanceship-with agent2 [ set color red ]
        kill-friendship self agent2
      ]
    ]

    [ ; Distance too big for a link
      kill-acquaintanceship agent1 agent2
      kill-friendship agent1 agent2
    ]
  )
end

; Updates all relations in the network (per agent instead of simultaneously)
to perform-friendship-selection
  ask turtles [ ask other turtles [ update-relation myself self ] ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; SOCIAL INFLUENCE UPDATE

; Stores updated position w.r.t. p of agent in p2
to get-p2 [ agent relations ]
  ask agent [
    let total-relations (count relations + 1) ; Count turtle as his own neighbor
    let support count relations with [ p = 1 ] ; Count relations supporting p
    set support support + (ifelse-value (p = 1) [1] [0]) ; Count own support of p (either 1 or 0)

    ; Adopt p if proportion exceeds threshold
    ifelse (support / total-relations) >= influence-threshold
    [ set p2 1 ]
    [ set p2 0 ]
  ]
end

; Stores updated position w.r.t. q of agent in q2
to get-q2 [ agent relations ]
  ask agent [
    let total-relations (count relations + 1) ; Count turtle as his own neighbor
    let support count relations with [ q = 1 ] ; Count relations supporting q
    set support support + (ifelse-value (q = 1) [1] [0]) ; Include own support of q (either 1 or 0)

    ;adopt q if proportion exceeds threshold
    ifelse (support / total-relations) >= influence-threshold
    [ set q2 1 ]
    [ set q2 0 ]
  ]
end

; Stores updated position w.r.t. r of agent in r2
to get-r2 [ agent relations ]
  ask agent [
    let total-relations (count relations + 1) ; Count turtle as his own neighbor
    let support count relations with [ r = 1 ] ; Count relations supporting r
    set support support + (ifelse-value (r = 1) [1] [0]) ; Include own support of r (either 1 or 0)

    ; Adopt r if proportion exceeds threshold
    ifelse (support / total-relations) >= influence-threshold
    [ set r2 1 ]
    [ set r2 0 ]
  ]
end

; Stores simultaneously updated positions of agent in p2, q2, r2 given relations
; Is also used during influencer identification
to get-updated-positions-agent [ agent relations ]
  ask agent [
    let total-relations (count relations + 1)

    ifelse total-relations = 1

    ; If turtle has no other neighbors, do not update positions
    [ set p2 p
      set q2 q
      set r2 r
    ]

    ; Compute new positions
    [ get-p2 self relations
      get-q2 self relations
      get-r2 self relations
    ]

  ]
end

; Stores simultaneously updated positions of every agent given current relations
to get-updated-positions-total
  ask turtles [ get-updated-positions-agent self link-neighbors ]
end

; Updates all positions of the turtles in the network
to perform-social-influence
  get-updated-positions-total
  ask turtles [

    ; Set positions to updated positions
    set p p2
    set q q2
    set r r2
    set label (list p q r) ; Adjust labels
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; IDENTIFYING INFLUENCERS

; Reports true if i has the same value as i2
to-report is-rigid-i [ i i2 ]
  ifelse i = i2
  [ report true ]
  [ report false ]
end

; Checks if agent is rigid or flexible and adapts counts accordingly
to check-inf-type [ agent ]
  ask agent [
    (ifelse
      ((is-rigid-i p p2) and (is-rigid-i q q2)) and (is-rigid-i r r2) [ ; Rigid influencer
        set rig-influencer rig-influencer + 1
        set flex-influencer flex-influencer + 1
      ]

      ((is-rigid-i p p2) or (is-rigid-i q q2)) or (is-rigid-i r r2) [ ; Flexible influencer
        set rig-influencer 0
        set flex-influencer flex-influencer + 1
      ]

      [ set rig-influencer 0 ; No influencer
        set flex-influencer 0
      ])
  ]
end

; Identifies current rigid and flexible influencers
to identify-influencers
  ; Compute future positions p2, q2, r2 for all agents
  get-updated-positions-total

  ask turtles [
    ; Compute current ratio, count self as own friend
    let friends-ratio (count turtle-set friendship-neighbors + 1) / (count link-neighbors + 1)

    let list1 (list p2 q2 r2)

    ; Compute future friends and relations given future positions
    let next-friends other turtles with [ member? (dist list1 (list p2 q2 r2)) fdist ]
    let next-relations other turtles with [ (member? (dist list1 (list p2 q2 r2)) fdist)
                                     or (member? (dist list1 (list p2 q2 r2)) acdist)
    ]

    ; Compute next ratio and compare with current ratio
    let next-friends-ratio (count next-friends + 1) / (count next-relations + 1)

    ifelse next-friends-ratio > friends-ratio [ check-inf-type self ] ; Check influencer type

    [ ; If next ratio <= ratio, no influencer
      set rig-influencer 0
      set flex-influencer 0
    ]

  ]
end

; Shows which turtles are currently influencers
to visualize-influencers
  ask turtles [
    (ifelse
      rig-influencer >= 1 [
        set size 4
        set color orange
      ]

      flex-influencer >=  1 [
        set size 4
        set color yellow
      ]

      [ ;no influencer
        set size 2
        set color blue
      ]
    )
  ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; POLARIZATION

; Returns intersection of two agentsets of turtles
to-report intersection [ set1 set2 ]
  report set1 with [ member? self set2 ]
end

; Reports list up to (and including) given index
to-report list-up-to-id [ mylist id ]
  report filter [ i -> (position i mylist) <= id ] mylist
end

; Adds next layer to currently existing layers extracted from the network
; Existing-layers is either layers or comp-layers
; Layer-kind is either "layers" or "comp-layers"
to add-next-layer [ existing-layers layer-kind ]

  ; If no agents left, end with no new layer
  ifelse empty? agents-left
  [ set next-layer-exists false ]

  [ ; Else try to add new layer
    let current-layer last existing-layers
    let next-layer []

    ; For each turtle in current layer, find its relations
    ask turtle-set current-layer [

      ; Add relevant relations to next layer
      ask link-neighbors [
        if member? self agents-left [ ; If no member of previous layers / next layer
          set next-layer lput self next-layer ; Add to next layer
          set agents-left remove self agents-left
        ]
      ]

    ]

    ; If next layer empty, end with no new layer
    ifelse empty? next-layer
    [ set next-layer-exists false ]

    ; Else add next layer to layers
    [ set next-layer-exists true
      set existing-layers lput next-layer existing-layers
    ]

  ]

  ifelse layer-kind = "layers"
  [ set layers existing-layers ] ; Store layers for balanced division
  [ set comp-layers existing-layers ] ; Store layers for components

end

; Extracts layers of neighbours with first agent of agents left as root
to construct-layers

  ; Set root as first layer
  set layers (list (list item 0 agents-left))
  set agents-left but-first agents-left ; Remove root from agents-left
  set next-layer-exists true

  while [ next-layer-exists ] [ add-next-layer layers "layers" ] ; Construct layers starting from root
  if empty? agents-left [ set is-connected true ] ; Check if connected
end

; Places agent in either X or Y given previously placed friends and acquaintances
to place-agent [ agent fr-prev-layers ac-prev-layers ]
  let set-X turtle-set X
  let set-Y turtle-set Y

  (ifelse

    ; Fail if a friend and acquaintance share group X or Y
    (any? fr-prev-layers with [ member? self set-X ])
    and (any? ac-prev-layers with [ member? self set-X ] ) [
      set X []
      set Y []
      set contr true
      set is-polarized false
    ]
    (any? fr-prev-layers with [ member? self set-Y ])
    and (any? ac-prev-layers with [ member? self set-Y ] ) [
      set X []
      set Y []
      set contr true
      set is-polarized false
    ]

    ; Else place in a group depending on placement of related agents
    (any? fr-prev-layers with [ member? self set-X ])
    or (any?  ac-prev-layers with [ member? self set-Y ]) [
      set X lput agent X
    ]
    (any? fr-prev-layers with [ member? self set-Y ])
    or (any? ac-prev-layers with [ member? self set-X ]) [
      set Y lput agent Y
    ]

    ; Alternative relevant for measure-complex-pol: no (previously placed) friends
    (not any? fr-prev-layers with [ member? self set-X ])
    and (not any?  ac-prev-layers with [ member? self set-X ])
    and (not any? fr-prev-layers with [ member? self set-Y ])
    and (not any?  ac-prev-layers with [ member? self set-Y ]) [

      ifelse empty? Y
      [ set Y lput agent Y ] ;place in Y if all previously placed agents are in X
      [ set X lput agent X ]
    ]
  )
end

; Tries to find a balanced division from a layered component of a network
to find-balanced-div-component
  set X item 0 layers ; Add root of layers to X

  foreach but-first layers [ ; For each layer in layers except for root
    layer ->

    ; Continue if no contradiction has been reached
    if not contr [

      ; Get agentset of agents in layers up to current layer
      let id position layer layers
      let prev-layers list-up-to-id layers id
      let prev-layers-agents turtle-set prev-layers

      ask turtle-set layer [ ; For each agent in current layer

          ; Continue if no contradiction has been reached
          if not contr [

            ; Determine friends and acquaintances in previous and current layers
            let fr friendship-neighbors
            let ac acquaintanceship-neighbors
            let fr-prev-layers intersection fr prev-layers-agents
            let ac-prev-layers intersection ac prev-layers-agents

            ; Try to place agent in either X or Y
            place-agent self fr-prev-layers ac-prev-layers

          ]

     ]

   ]

  ]
end

; Writes current information on polarization and connectedness to output
to output-polarization
  output-print("Network is polarized")

  ifelse is-connected
  [ output-print "Connected" ]
  [ output-print "Disconnected" ]

  ifelse (empty? Y and is-connected = true) ; If Y is empty on a connected network, we have consensus
  [ output-print "Consensus" ]
  [ output-print "No consensus" ]

  output-print ("Number of ticks:")
  output-print (ticks)

  output-print ("********************")
end

; Shows the two groups of agents in green and red resp.
to visualize-polarization
  ask turtles [
    set size 2
    (ifelse
      member? self X [ set color green ]
      member? self Y [ set color red ]
    )
  ]
end

; Measures polarization for networks where disconnected implies balanced
to measure-easy-pol
  ifelse is-connected
  [ ; If connected, try balanced division
    find-balanced-div-component

    ; If no contradiction, network is polarized
    ifelse contr
    [ set is-polarized false ]

    [ set is-polarized true
      output-polarization
    ]

  ]

  ; If disconnected, network is balanced
  [ set is-polarized true
    output-polarization

    ; Change X and Y to the two components
    set X turtle-set layers ; Convert layered component to agentset
    set Y turtle-set agents-left
  ]
end

; Measures polarization for networks where disconnected does not imply balanced
to measure-complex-pol
  ; Try balanced division for layered component
  find-balanced-div-component

  ifelse contr

  ; In case of contradiction, network is not polarized
  [ set is-polarized false ]

  ; Else, check if connected and divide any further components (agents left)
  [ ifelse is-connected
    [ set is-polarized true
      output-polarization
    ]

    ; If disconnected, place agents left
    [ ask turtle-set agents-left [

      ; Determine friends and acquaintances
      let fr friendship-neighbors
      let ac acquaintanceship-neighbors

      ; Place agent depending on friends and acquaintances (not in layers)
      place-agent self fr ac
      set agents-left remove self agents-left
      ]

      ; If no contradiction, network is polarized
      ifelse contr
      [ set is-polarized false ]

      [ set is-polarized true
        output-polarization
      ]

   ]

  ]
end

; Executes balance measure dependent on relation distances
to perform-balance-measure

  ; Reset variables
  set layers []
  set agents-left [self] of turtles
  set is-connected false
  set contr false
  set X []
  set Y []

  ; Construct layers starting from random root node
  construct-layers

  ; Determine measure
  ifelse theta-values = "configuration 3"
  [ measure-complex-pol ]
  [ measure-easy-pol ]
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ADDITIONAL VARIABLES

; Updates lists of rigid and flexible influencers, as well as the number of steps they've been influencers
to get-inf-lists
  set rig-inf-list []
  set flex-inf-list []
  ask turtles [

    ; Add to rig list if rigid inf, else add to flex list if flexible inf
    ifelse rig-influencer > 0
    [ set rig-inf-list lput (list who rig-influencer) rig-inf-list ]
    [ if flex-influencer > 0 [ set flex-inf-list lput (list who flex-influencer) flex-inf-list ] ]

  ]
end


; Updates lists of positions of the influencers
to get-inf-positions
  set rig-inf-pos []
  set flex-inf-pos []

  foreach rig-inf-list [
    turtle-inf ->
    let turtle-number item 0 turtle-inf
    set rig-inf-pos lput (list turtle-number [label] of turtle turtle-number) rig-inf-pos
  ]

  foreach flex-inf-list [
    turtle-inf ->
    let turtle-number item 0 turtle-inf
    set flex-inf-pos lput (list turtle-number [label] of turtle turtle-number) flex-inf-pos
  ]
end

; Stores the components of the network in case of polarization
to find-components
  set components []

  ifelse is-connected

  ; Connected case: only one component
  [ set components lput [self] of turtles components ]

  ; Disconnected cases
  [ ifelse theta-values = "configuration 3"

    ; If more than two components are possible
    [ set components lput reduce sentence layers components ; Set layered component as first component
      set agents-left [self] of turtles with [ not member? self turtle-set layers ] ; Get agents left

      set next-layer-exists true

      ; Find new components until no agents are left
      while [ next-layer-exists ] [

        ; Layer a new component
        set comp-layers (list (list item 0 agents-left)) ; Set new root
        set agents-left but-first agents-left ; Remove root from agents-left
        set next-layer-exists true

        while [ next-layer-exists ] [ add-next-layer comp-layers "comp-layers" ] ; Construct layers starting from new root
        set components lput reduce sentence comp-layers components ; Add set of new layers to list of components

      ]

    ]

    ; Only two components are possible: use balanced division
    [ set components lput [self] of X components
      set components lput [self] of Y components
    ]
  ]
end

; Given found components, checks if consensus was reached in any of the components
to check-consensus
  set consensus-list []

  foreach components [ ; Iterate over the components
    component ->

    ; Get the opinion of an agent in the component
    let opinion [label] of item 0 component


    ifelse any? turtles with [ (member? self turtle-set component) and (label != opinion) ]
    [ set consensus-list lput [] consensus-list ] ; If any turtle disagrees with given opinion, add empty list
    [ set consensus-list lput opinion consensus-list ] ; Else add unanimous opinion to list

  ]
end



@#$#@#$#@
GRAPHICS-WINDOW
498
10
1056
569
-1
-1
9.02
1
10
1
1
1
0
0
0
1
-30
30
-30
30
0
0
1
ticks
30.0

BUTTON
8
20
74
53
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

CHOOSER
8
71
237
116
network-type
network-type
"ring" "star" "double star" "fully connected" "Erdös-Rényi random network"
1

BUTTON
95
20
158
53
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
1

SLIDER
8
133
185
166
influence-threshold
influence-threshold
0
1
0.5
0.05
1
NIL
HORIZONTAL

CHOOSER
7
185
412
230
theta-values
theta-values
"configuration 1" "configuration 2" "configuration 3"
0

OUTPUT
11
320
224
472
13

SLIDER
275
71
447
104
num-turtles
num-turtles
1
500
1.0
1
1
NIL
HORIZONTAL

SLIDER
275
123
447
156
num-links
num-links
0
800
0.0
1
1
NIL
HORIZONTAL

TEXTBOX
274
18
424
63
num-turtles en num-links: only used for random network
12
0.0
1

TEXTBOX
234
315
384
390
output information per tick:\n- polarization\n- connectedness\n- number of ticks
12
0.0
1

TEXTBOX
9
237
369
304
theta-values:\n- configuration 1 with theta_1 = 1/3, theta_2 = 2/3\n- configuration 2 with  theta_1 = 0, theta_2 = 2/3\n- configuration 3 with  theta_1 = 0, theta_2 = 1/3
12
0.0
1

@#$#@#$#@
## WHAT IS IT?

This is the implementation of a Social Network Model created to research the development of polarization in relation with homophily and influential nodes. The agents alternately recreate friendship and acquaintanceship relations, and influence each other's opinions. The network runs in NetLogo-6.2.0. A detailed description of the model, its logical implications, and the results of a number of simulations is provided in my bachelor's thesis. 

## HOW IT WORKS

The model makes use of two main, threshold-based update rules: friendship selection (theta-values) and social influence (influence-threshold). The thresholds for both updates influence the development of the model and can be managed on the user interface. Furthermore, the random network can be run with varying numbers of turtles and links. The model keeps track of rigid influencers, as well as flexible influencers, and computes polarization by means of the balance measure after each round of updates. A large number of global variables is stored purely to run experiments in NetLogo's BehaviorSpace. As the include function does not work properly with globals, the code was written in one file, despite the large amount. 

## HOW TO USE IT

The network-type chooser contains several network-types that can be run. Whereas the random network is run with varying numbers of turtles and links, the other structures run with fixed numbers. The influence-threshold determines what proportion of neighbors is required to have some opinion in order to influence an agent, while the theta-values determine the maximum distance between two agents that is required to form a friendship or acquaintanceship, respectively.

## THINGS TO NOTICE

Notice how stricter theta-values (configuration 3) and influence-threshold values around 0.5 lead to extreme results of polarization, given by disconnectedness in social networks, with consensus in each respective component. 

## THINGS TO TRY

Run the same configurations multiple times in order to see the different results caused by the random initiation of opinions. Run the network with continuous updates to properly view the development of influential nodes. 

## RELATED MODELS

The model makes use of the basic algoritm to create an Erdös-Rényi (ER) random network that is provided by the "NetLogo random network model" in the models library. 


## CREDITS AND REFERENCES

Created by Djanira dos Santos Gomes for bachelor's thesis "A logical analysis of influencers and polarization in social networks" (bachelor Artificial Intelligence at Universiteit van Amsterdam, Netherlands), 2021.
Based on the model provided by Smets and Vélazquez-Quesada (2020).

S. Smets and F.R. Velázquez-Quesada. A logical analysis of the interplay between social influence and friendship selection. Lecture Notes in Computer Science Dynamic Logic. New Trends and Applications, page 71–87, 2020. doi: 10.1007/978-3-030-38808-9\_5.
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

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

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

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="exp_random" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <steppedValueSet variable="num-turtles" first="1" step="50" last="500"/>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Erdös-Rényi random network&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-links" first="0" step="100" last="800"/>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="relation-distances">
      <value value="&quot;0 = friendship, 1 = acquaintanceship, 2 or 3 = nothing&quot;"/>
      <value value="&quot;0 = friendship, 1 or 2 = acquaintanceship, 3 = nothing&quot;"/>
      <value value="&quot;0 or 1 = friendship, 2 = acquaintanceship, 3 = nothing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_ring" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;ring&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="relation-distances">
      <value value="&quot;0 = friendship, 1 = acquaintanceship, 2 or 3 = nothing&quot;"/>
      <value value="&quot;0 = friendship, 1 or 2 = acquaintanceship, 3 = nothing&quot;"/>
      <value value="&quot;0 or 1 = friendship, 2 = acquaintanceship, 3 = nothing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_star" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="relation-distances">
      <value value="&quot;0 = friendship, 1 = acquaintanceship, 2 or 3 = nothing&quot;"/>
      <value value="&quot;0 = friendship, 1 or 2 = acquaintanceship, 3 = nothing&quot;"/>
      <value value="&quot;0 or 1 = friendship, 2 = acquaintanceship, 3 = nothing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_d_star" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;double star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="relation-distances">
      <value value="&quot;0 = friendship, 1 = acquaintanceship, 2 or 3 = nothing&quot;"/>
      <value value="&quot;0 = friendship, 1 or 2 = acquaintanceship, 3 = nothing&quot;"/>
      <value value="&quot;0 or 1 = friendship, 2 = acquaintanceship, 3 = nothing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fully" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;fully connected&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="relation-distances">
      <value value="&quot;0 = friendship, 1 = acquaintanceship, 2 or 3 = nothing&quot;"/>
      <value value="&quot;0 = friendship, 1 or 2 = acquaintanceship, 3 = nothing&quot;"/>
      <value value="&quot;0 or 1 = friendship, 2 = acquaintanceship, 3 = nothing&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_random_goed" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <steppedValueSet variable="num-turtles" first="1" step="50" last="500"/>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Erdös-Rényi random network&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-links" first="0" step="100" last="800"/>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_ring_goed" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;ring&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_star_goed" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_d_star_goed" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;double star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fully_goed" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;fully connected&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_ring_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;ring&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_star_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_d_star_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;double star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fully_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;fully connected&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_random_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <steppedValueSet variable="num-turtles" first="1" step="50" last="500"/>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Erdös-Rényi random network&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-links" first="0" step="100" last="800"/>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_random_definitief2" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <steppedValueSet variable="num-turtles" first="1" step="50" last="500"/>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Erdös-Rényi random network&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-links" first="0" step="100" last="800"/>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_ring_echt_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;ring&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_star_echt_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_d_star_echt_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;double star&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_fully_echt_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;fully connected&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="exp_random_echt_definitief" repetitions="3" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>ticks = 10</exitCondition>
    <metric>is-connected</metric>
    <metric>is-polarized</metric>
    <metric>components</metric>
    <metric>consensus-list</metric>
    <metric>rig-inf-list</metric>
    <metric>flex-inf-list</metric>
    <metric>rig-inf-pos</metric>
    <metric>flex-inf-pos</metric>
    <steppedValueSet variable="num-turtles" first="1" step="50" last="500"/>
    <enumeratedValueSet variable="network-type">
      <value value="&quot;Erdös-Rényi random network&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="num-links" first="0" step="100" last="800"/>
    <steppedValueSet variable="influence-threshold" first="0" step="0.05" last="1"/>
    <enumeratedValueSet variable="theta-values">
      <value value="&quot;configuration 1&quot;"/>
      <value value="&quot;configuration 2&quot;"/>
      <value value="&quot;configuration 3&quot;"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
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
