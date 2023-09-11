strategies = [
    {'lt' : 1 ,  'lc' : 4 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(InterSDSE-efficient)'                        ,  'display' : 'B(SDSE)'},
    {'lt' : 2 ,  'lc' : 4 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(InterSDSE-probabilistic)'                    ,  'display' : 'B(SDSE-pr)'},
    {'lt' : 3 ,  'lc' : 4 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(InterSDSE-roundrobin)'                       ,  'display' : 'B(SDSE-rr)'},
    {'lt' : 1 ,  'lc' : 7 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(RoundRobin(RandomPath,InterSDSE-efficient))' ,  'display' : 'B(RR(RP,SDSE))'},
    {'lt' : 2 ,  'lc' : 7 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(RoundRobin(RandomPath,InterSDSE-efficient))' ,  'display' : 'B(RR(RP,SDSE-rr))'},
    {'lt' : 1 ,  'lc' : 9 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Batched(Phases(OtterKLEE,InterSDSE))'                ,  'display' : 'B(Ph(OtterKLEE,SDSE,3))'},
    {'lt' : 2 ,  'lc' : 5 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'CCBSE(random-path)'                                  ,  'display' : 'CCBSE(RP)'},
    {'lt' : 1 ,  'lc' : 1 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'OtterKLEE'                                           ,  'display' : 'OtterKLEE'},
    {'lt' : 2 ,  'lc' : 1 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Mix-CCBSE(OtterKLEE,0.75)'                           ,  'display' : 'Mix-CCBSE(OtterKLEE)'},
    {'lt' : 1 ,  'lc' : 2 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'OtterSAGE'                                           ,  'display' : 'OtterSAGE'},
    {'lt' : 2 ,  'lc' : 2 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Mix-CCBSE(OtterSAGE,0.75)'                           ,  'display' : 'Mix-CCBSE(OtterSAGE)'},
    {'lt' : 1 ,  'lc' : 3 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'random-path'                                         ,  'display' : 'RP'},
    {'lt' : 2 ,  'lc' : 3 ,  'lw' : 3 ,  'pt' : 7 ,  'name' : 'Mix-CCBSE(random-path,0.75)'                         ,  'display' : 'Mix-CCBSE(RP)'},
]

print 'plot \\'
print ', \\\n'.join(["sprintf(\"plots/%%s/%s/time.dat\", benchmark) using 2:1 title '%s' with lines lt %d lc %d lw %d pt %d " % (strategy['display'], strategy['display'], strategy['lt'], strategy['lc'], strategy['lw'], strategy['pt']) for strategy in strategies])
