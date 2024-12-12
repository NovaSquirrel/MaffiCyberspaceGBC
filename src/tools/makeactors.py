#!/usr/bin/env python3
import sys

# ---------------------------
# Configuration

ACTOR_POINTER_SECTION = 'SECTION FRAGMENT "ActorCode", ROMX, ALIGN[8]'
ACTOR_DATA_SECTION = 'SECTION FRAGMENT "ActorCode", ROMX'
ACTOR_ROM0_SECTION = 'SECTION FRAGMENT "ActorGlobal", ROM0'
ACTOR_GRAPHICS_SECTION = 'SECTION "ActorGraphics", ROMX'

# ---------------------------

if len(sys.argv) != 4:
	sys.exit('makeactors.py definition.txt data.asm enum.asm')

actor_definition_filename = sys.argv[1]
actor_data_filename       = sys.argv[2]
actor_enum_filename       = sys.argv[3]

# Actor information (for one actor)
actor = None

# Actor information (for all actors)
all_actors = []
all_tilesets = set()

# Utility functions
def separateFirstWord(text, lowercaseFirst=True):
	space = text.find(" ")
	command = text
	arg = ""
	if space >= 0:
		command = text[0:space]
		arg = text[space+1:]
	if lowercaseFirst:
		command = command.lower()
	return (command, arg)

def parseNumber(number):
	if number in aliases:
		return parseNumber(aliases[number])
	if number.startswith("$"):
		return int(number[1:], 16)
	return int(number)

# Read and process the file
with open(actor_definition_filename) as f:
    text = [s.rstrip() for s in f.readlines()]

def saveActor():
	if actor == None:
		return
	all_actors.append(actor)

for line in text:
	if not len(line):
		continue
	if line.startswith("#"): # comment
		continue
	if line.startswith("+"): # new actor
		saveActor()
		# Reset to prepare for the new actor
		actor = {"name": line[1:], "run": "ActorNothing", "health": "$10", "important": False, "unimportant": False, "tileset": None}
		continue
	word, arg = separateFirstWord(line)
	# Miscellaneous directives
	if word == "alias":
		name, value = separateFirstWord(arg)
		aliases[name] = value
	# Attributes
	elif word in ("important", "unimportant"):
		actor[word] = True
	elif word in ["health", "run"]:
		actor[word] = arg
	elif word == "tileset":
		actor[word] = arg
		all_tilesets.add(arg)

# Save the last one
saveActor()

all_tilesets = list(all_tilesets)

# -------------------------------------------------------------------

# Generate the output that's actually usable in the game
outfile = open(actor_data_filename, "w")

actor_count = len(all_actors)
print("%d actor types defined." % actor_count)
if actor_count > 128:
	sys.exit("That's too many! The maximum is 128.")

outfile.write('; This is automatically generated. Edit "%s" instead\n' % actor_definition_filename)

outfile.write('\n%s\n' % ACTOR_POINTER_SECTION)

outfile.write('\nActorPointers::\n')
for a in all_actors:
	outfile.write('dw %s\n' % a["run"])

outfile.write('\n%s\n' % ACTOR_DATA_SECTION)

outfile.write('\nActorHealth::\n')
for a in all_actors:
	outfile.write('db %s\n' % a["health"])

outfile.write('\nActorTilesetPointers::\n')
for tileset in all_tilesets:
	outfile.write('dw ActorTileset_Data_%s\n' % tileset)

outfile.write('\n%s\n' % ACTOR_ROM0_SECTION)

outfile.write('\nActorFlags::\n')
for a in all_actors:
	outfile.write('db %x|%x\n' % (a["unimportant"]*128, a["important"]*64))

outfile.write('\nActorTileset::\n')
for a in all_actors:
	if a["tileset"] == None:
		outfile.write('db 0\n')
	else:
		outfile.write('db %d ; %s\n' % (all_tilesets.index(a["tileset"])+1, a["tileset"]))

# Sprite tileset RAM
outfile.write('\nSECTION "FirstTileNumberForActorTileset", WRAM0\n')
outfile.write('FirstTileNumberForActorTileset:: ds %d\n' % (len(all_tilesets)+1))
outfile.write('FirstTileNumberForActorTilesetEnd::\n')

# Sprite tileset incbins
outfile.write('\n%s\n' % ACTOR_GRAPHICS_SECTION)
outfile.write('ActorTileset_Data::\n')
for tileset in all_tilesets:
	outfile.write('ActorTileset_Data_%s: incbin "res/tilesets_8x16/%s.pb16"\n' % (tileset, tileset))

outfile.close()

# -------------------------------------------------------------------

# Generate the enum in a separate file
outfile = open(actor_enum_filename, "w")
outfile.write('; This is automatically generated. Edit "%s" instead\n' % actor_definition_filename)

for i, b in enumerate(all_actors):
	outfile.write('DEF ActorType_%s EQU %d\n' % (b['name'], i))

for i, t in enumerate(all_tilesets):
	outfile.write('DEF ActorTileset_%s EQU %d\n' % (t, i))

outfile.write('\n')

outfile.close()
