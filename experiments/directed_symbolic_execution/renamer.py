#!/usr/bin/env python
import sys, csv

mapping = {
    "InterSDSE-efficient"                                  : "SDSE",
    "InterSDSE-probabilistic"                              : "SDSE-pr",
    "IntraSDSE"                                            : "SDSE-intra",
    "Batched(InterSDSE-efficient)"                         : "B(SDSE)",
    "Batched(InterSDSE-probabilistic)"                     : "B(SDSE-pr)",
    "Batched(InterSDSE-roundrobin)"                        : "B(SDSE-rr)",
    "RoundRobin(RandomPath,InterSDSE-efficient)"           : "RR(RP,SDSE)",
    "Batched(RoundRobin(RandomPath,InterSDSE-efficient))"  : "B(RR(RP,SDSE))",
    "Batched(RoundRobin(RandomPath,InterSDSE-roundrobin))" : "B(RR(RP,SDSE-rr))",
    "Batched(Phases(KLEE,InterSDSE-efficient))"            : "B(Ph(OtterKLEE,SDSE,3))",
    "Batched(Phases(KLEE,InterSDSE))"                      : "B(Ph(OtterKLEE,SDSE,3))",
    "CCBSE(InterSDSE)"                                     : "CCBSE(SDSE)",
    "CCBSE(random-path)"                                   : "CCBSE(RP)",
    "KLEE"                                                 : "OtterKLEE",
    "Mix(KLEE,0.75)"                                       : "Mix-CCBSE(OtterKLEE)",
    "SAGE"                                                 : "OtterSAGE",
    "Mix(SAGE,0.75)"                                       : "Mix-CCBSE(OtterSAGE)",
    "random-path"                                          : "RP",
    "Mix(random-path,0.75)"                                : "Mix-CCBSE(RP)",
    "RealKLEE"                                             : "KLEE",
    "ProSDSE"                                              : "Fig 3.1",
    "ProCCBSE"                                             : "Fig 3.3",
    "ProMix"                                               : "Fig 3.5",
    }

csv_reader = csv.reader(sys.stdin, delimiter=',', quotechar='"')
csv_writer = csv.writer(sys.stdout, delimiter=',', quotechar='"', quoting=csv.QUOTE_MINIMAL)

for row in csv_reader:
    row = map(lambda s: mapping[s] if s in mapping else s, row)
    csv_writer.writerow(row)
