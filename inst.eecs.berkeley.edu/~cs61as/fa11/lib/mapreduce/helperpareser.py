from optparse import OptionParser

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="Reads data from FILE outputs to FILEdata", metavar="FILE")
                  
(options, args) = parser.parse_args()



source = "pokemonstats"
destination = "magikarp"
inputFile = open(source,'r')
outputFile = open(destination,'w')

types = ["FIRE","WATER","PSYCHIC","GRASS","NORMAL","ELECTRIC","ICE", "FIGHTING", "POISON", "GROUND",  "FLYING", "BUG", "ROCK","GHOST", "DRAGON", "DARK", "STEEL", "FAIRY"]


def process(arr):
    x = str(arr[0]).replace(" ","") + " " + process2(arr[1:])
    print x 
    return x 

def process2(arr):
    if len(arr) == 0:
        return ""
    elif exception(arr[0]):
        return removeSpecialChars(str(arr[0])) + " " + process2(arr[1:])
    return str(arr[0]) + " " + process2(arr[1:])

specialChars = ["'",'"',';','(',')',".","#",". ","|","~","!",'@','#','$','%','^','&','*','-','+','=','{','}','[',']','<','>','?','/',':','_']

def exception(somestring):
    return True in map(lambda char: char in somestring, specialChars) 

def removeSpecialChars(somestring):
    output = somestring
    for char in specialChars:
        output = output.replace(char,'')
    return output

merge = False
for line in inputFile:
    line = line.strip()
    
    values = line.split("\t")
    if True or (("X" not in values) and ("Y" not in values)):
        type = values[1]
        if type not in types:
            values[1] = values[0] + values[1]
            values = values[1:]
        print values
        values = process(values)
        outputFile.write(values)
        outputFile.write("\n")

    merge = not merge
        
    
