import sys

def getOne(s): return s.__iter__().next()

def insert(setting,equivClasses):
    for equivClass in equivClasses:
        if neighbors[setting] == neighbors[equivClass[0]]:
            settingToEquivClass[setting] = equivClass
            equivClass.append(setting) # Add to an existing equivalence class
            return
    equivClasses.append([setting]) # Add a new equivalence class
    settingToEquivClass[setting] = equivClasses[-1]

def getEquivClassString(equivClass):
    if len(equivClass) == 1:
        return '%s=%d' % getOne(equivClass)
    optionToValues = {}
    for opt,val in equivClass:
        if opt in optionToValues:
            optionToValues[opt].append(val)
        else:
            optionToValues[opt] = [val]
    result = ''
    for opt,vals in sorted(optionToValues.iteritems(),key=lambda x:x[0]):
        result += opt + '='
        if len(vals) > 1:
            result += '{' + ','.join(map(str,sorted(vals))) + '},'
        else:
            result += str(vals[0]) + ','
    if len(optionToValues) == 1:
        return result[:-1]
    return '{' + result[:-1] + '}'

def getInteraction(file):
    while True:
        line = file.next()
        if line.startswith('Under'):
            result = set()
            line = file.next()
            while not line.startswith('these'):
                varVal = line[:-1].split('=')
                result.add( (varVal[0],int(varVal[1])) )
                line = file.next()
            return result

with open(sys.argv[1]) as f:
    file = f.xreadlines()
    currentInteraction = getInteraction(file)
    keepGoing = True
    while keepGoing:
        # type setting = string * int (* a (var,val) pair *)
        # The variable 'settings' is a set of settings
        settings = set() # set of (var,val) pairs
        # An 'interaction' is a set of settings
        # The variable 'interactions' is a list of interactions
        interactions = [] # list of sets of settings
        while True:
            settings |= currentInteraction
            interactions.append(currentInteraction)
            try:
                currentInteraction = getInteraction(file)
            except StopIteration:
                keepGoing = False
                break

        # 'neighbors' maps each setting to the set of (partial) interactions it occurs alongside,
        neighbors = dict()
        for setting in settings:
            neighbors[setting] = set([frozenset(x-set([setting])) for x in interactions if setting in x])
        settingToEquivClass = dict()
        equivClasses = []
        for setting in settings:
            insert(setting,equivClasses)

        # Replace each setting with its equivalence class
        for setting in settings:
            for interaction in interactions:
                try:
                    interaction.remove(setting)
                    interaction.add(frozenset(settingToEquivClass[setting]))
                except KeyError: pass

        for interaction in sorted(set([frozenset(x) for x in interactions]),key=len):
            if len(interaction) == 0:
                continue
            if len(interaction) == 1:
                print getEquivClassString(getOne(interaction))
                continue
            theString = ''
            # Sort equivalence classes based on the option within each
            # class with name which is earliest lexicographically
            for equivClass in sorted(interaction,key=lambda x:min([y[0] for y in x])):
                theString += getEquivClassString(sorted(equivClass)) + ':'
            print theString[:-1] # Drop final ':'
