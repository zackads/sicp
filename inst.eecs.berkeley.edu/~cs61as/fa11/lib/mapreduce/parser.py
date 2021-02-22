from optparse import OptionParser

parser = OptionParser()
parser.add_option("-f", "--file", dest="filename",
                  help="Reads data from FILE outputs to FILEdata", metavar="FILE")
parser.add_option("-s", "--subst", dest="substring",
                  help="A string that divides the input file", metavar="SUBSTRING")
                  
(options, args) = parser.parse_args()

source=options.filename
destination= source + "_data"

inputFile = open(source,'r')
outputFile = open(destination,'w')
divisor = options.substring


def process(arr):
    if len(arr) == 0:
        return ""
    elif exception(arr[0]):
        return removeSpecialChars(str(arr[0])) + " " + process(arr[1:])
    return str(arr[0]) + " " + process(arr[1:])

specialChars = ["'",'"',';','(',')',".","#",". ","|","~","!",'@','#','$','%','^','&','*','-','+','=','{','}','[',']','<','>','?','/',':','_']

def exception(somestring):
    return True in map(lambda char: char in somestring, specialChars) 

def removeSpecialChars(somestring):
    output = somestring
    for char in specialChars:
        output = output.replace(char,'')
    return output

def startNewStream():
    tokenLength = len(tokens) + closeParens
    return tokenLength >= 1000 or len(streams) == 0

def closePreviousStreams():
    return len(streams) > 0


streams = []
outputFile.write("(define s cons-stream)\n") # making the cons-stream token shorter
outputFile.write("(define i interleave)\n") # making the interleave token shorter
closeParens = 0 # Keeps track of the number of close tokens needed
streamNum = 0 # Keeps track of how many individual streams have been created. Used for generic naming streams
tokens = ""

count = 0
CEILING = 30000
for line in inputFile:
    if count < CEILING:
        count += 1
        if startNewStream():
            if closePreviousStreams(): # shouldnt close when starting the very first stream
                tokens += " the-empty-stream"
                outputFile.write(tokens)
                tokens = ""
                outputFile.write(")" * closeParens + "\n")
                closeParens = 0

            streamName = "s" + str(streamNum) + " "
            tokens += "(define " + streamName
            streams += [streamName]
            closeParens += 1
            streamNum += 1
    
        tokens += "(s "
        closeParens += 1
        line = line.strip()
        values = line.split(divisor)
        tokens += " '(" + process(values) + ") "


if closeParens > 0:
    tokens += "the-empty-stream"
    outputFile.write(tokens)
    outputFile.write(")" * closeParens + "\n")

allStreams = "(define data "
for stream in streams:
    allStreams = allStreams +  " (i " + stream
allStreams += " the-empty-stream"
allStreams += ")" * len(streams)
allStreams += ")" #closes define data
outputFile.write(allStreams)


outputFile.close()
inputFile.close()

