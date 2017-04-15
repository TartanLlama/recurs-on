"""A script which will create objects for you for the Haskell adventure

Read the usage instructions CAREFULLY before use as the script does not yet handle errors, it will simply create the objects wrongly

Usage:
Run this script with the following command line arguments
python createObject.py "<object name>" "<object data name>" "<object long name>" "<object description>" "<object command synonyms>" (one or more)

<object name> = the name which the object will be assigned to.
This should ALWAYS begin with a lower case letter and should contain only alphanumeric characters or underscores

<object data name> = the identifier of the data type for the object.
This should ALWAYS begin with an upper case letter and should only contain alphanumeric characters

<object long name> = the long name of the object.
Can be anything you want

<object description> = the description of the object.
Can be anything you want

<object synonyms> = different names that the object can be identified by with user commands.
Should be all lower case and only one word

For example:
If you wanted to create a television object
python createObject "television" "Television" "a television" "an incredible high-definition tv" "television" "tv" "tele"

If you wanted to create a sleeping barbarian called Geoff
python createObject "geoff" "Geoff" "a sleeping barbarian" "a barbarian named geoff who appears to be fast asleep" "geoff" "man" "person" "barbarian"
(of course you would only have 'man' and 'person' if this was the only man or person in the world
"""

import re
from string import *
import sys

def getNumCharsNeeded(lines, first_object):
    cont = True
    linum = first_object
    spaces = ''
    number_of_objects = 0

    while cont:
        line = re.match('(.+)=', lines[linum])
        if line == None:
            cont = False
        else:
            linum += 1
            number_of_objects += 1
            if len(line.group(0)) > len(spaces):
                spaces = rstrip(line.group(0), '=')

    return len(spaces) - 1, number_of_objects

def addSpacesToExisting(spaces_to_add, first_object, number_of_objects):
    global lines

    spaces = spaces_to_add * ' '
    cont = True

    for z in range(first_object + 1, first_object + number_of_objects + 1):
        lines[z] = replace(lines[z], ' ', (' ' + spaces), 1)
                               
            
object_name = sys.argv[1]
object_data_name = sys.argv[2]
object_longname = sys.argv[3]
object_description = sys.argv[4]

object_synonyms = []

for x in range(5, len(sys.argv)):
    object_synonyms.append(sys.argv[x])
    

file = open('World.hs', 'r')
lines = file.readlines()

theline = ''
linum = -1

for x in range(len(lines)):
    temp = re.match('data ObjName', lines[x])
    if temp != None:
        theline = temp.string
        linum = x
        
datachange = rstrip(theline, '\n')

datachange += ' | ' +  object_data_name + '\n'
lines[linum] = datachange


for x in range(len(lines)):
    temp = re.match('.+ \:\: Object', lines[x])
    if temp != None:
        theline = temp.string
        linum = x

typechange = replace(theline, ' :: Object\n', '')

typechange += ', ' + object_name + ' :: Object\n'

lines[linum] = typechange

spaces, number_of_objects = getNumCharsNeeded(lines, linum + 1)

spaces_to_add = spaces - len(object_name)

well_spaced_name = ''

if spaces_to_add < 0:
    addSpacesToExisting(-spaces_to_add, linum, number_of_objects)
    well_spaced_name = object_name
else:
    well_spaced_name = object_name + (spaces_to_add * ' ')

object_def = well_spaced_name + ' = Obj ' + object_data_name + ' "' + object_longname + '" "' + object_description + '"\n'

lines.insert(linum + 1, object_def)

for x in range(len(lines)):
    temp = re.match('transformObject s', lines[x])
    if temp != None:
        theline = temp.string
        linum = x
object_transformation = ''

for x in range(len('transformObject s ')):
    object_transformation += ' '

for x in object_synonyms:
    object_transformation += '| s == "' + x + '" |'

object_transformation = rstrip(object_transformation, '|')

object_transformation += ' = ' + object_data_name + '\n'

lines.insert(linum + 1, object_transformation)

newfile = open('out.hs', 'w')
for x in lines:
    newfile.write(x)




