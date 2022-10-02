#!/usr/bin/env python3
import sys

if len(sys.argv) != 4:
	sys.exit('makeblocks.py definition.txt data.asm enum.asm')

block_definition_filename = sys.argv[1]
block_data_filename       = sys.argv[2]
block_enum_filename       = sys.argv[3]

# Block information (for one block)
default_palette = 0
default_base = 0
block = None
priority = False

# Block information (for all blocks)
all_blocks = []
all_classes = set()
all_classes.add('None')
all_interactions = {}
all_markers = {}
aliases = {}

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

def parseMetatileTile(tile, default_palette, default_base, priority=False):
	""" Parse the nametable value for one tile """
	value = default_base

	if tile.find(":") >= 0: # Base override
		split = tile.split(":")
		value = parseNumber(split[0])
		tile = split[1]
	value = ((value - 0x8800)//16) ^ 128 # Divide by the bytes per tile to get tile number

	if tile.endswith("v"): # Vertical flip
		value |= 0x2000
		tile = tile[:-1]
	if tile.endswith("h"): # Horizontal flip
		value |= 0x4000
		tile = tile[:-1]
	if tile.endswith("_"): # No-op separator
		tile = tile[:-1]

	if priority:
		value |= 0x8000
	# Palette
	value |= default_palette << 8

	# Read the tile number in the format of x,y starting from the specified base
	if tile.find(",") >= 0:
		split = [parseNumber(s) for s in tile.split(",")]
		value += split[0]+split[1]*16
	else:
		value += parseNumber(tile)
	return value

# Read and process the file
with open(block_definition_filename) as f:
    text = [s.rstrip() for s in f.readlines()]

def saveBlock():
	if block == None:
		return
	all_blocks.append(block)

for line in text:
	if not len(line):
		continue
	if line.startswith("#"): # comment
		continue
	if line.startswith("+"): # new block
		saveBlock()
		# Reset to prepare for the new block
		priority = False
		block = {"name": line[1:], "solid": False, \
		  "tiles": [], "interaction": {}, "class": "None"}
		continue
	word, arg = separateFirstWord(line)

	# Miscellaneous directives
	if word == "alias":
		name, value = separateFirstWord(arg)
		aliases[name] = value
	elif word == "marker":
		all_markers[arg] = len(all_blocks) + 1

	# Tile info shared with several blocks
	elif word == "base":
		default_base = parseNumber(arg)
	elif word == "palette":
		default_palette = parseNumber(arg)

	elif word == "when": #Behaviors
		arg = arg.split(", ")
		if arg[0] not in all_interactions:
			all_interactions[arg[0]] = []
		all_interactions[arg[0]].append(arg[1])
	elif word == "autotile":
		block["autotile"] = arg

	elif word == "class":
		block["class"] = arg
		all_classes.add(arg)

	# Specifying tiles and tile attributes
	elif word == "solid":
		block["solid"] = True
	elif word == "priority":
		priority = True
	elif word == "no_priority":
		priority = False
	elif word == "t": # add tiles
		split = arg.split(" ")
		for tile in split:
			block["tiles"].append(parseMetatileTile(tile, default_palette, default_base, priority))
	elif word == "q": # add four tiles at once
		tile = parseMetatileTile(arg, default_palette, default_base, priority)
		block["tiles"] = [tile, tile+1, tile+2, tile+3]

# Save the last one
saveBlock()

# -------------------------------------------------------------------

# Generate the output that's actually usable in the game
outfile = open(block_data_filename, "w")

outfile.write('; This is automatically generated. Edit "%s" instead\n' % block_definition_filename)
outfile.write('include "res/blockenum.inc"\n\n')
outfile.write('\nSECTION "BlockAppearance", ROM0, ALIGN[8]\n\n')

# Block appearance information
outfile.write('BlockAppearance::\n')
for b in all_blocks:
	outfile.write('\tdb $%.2x, $%.2x, $%.2x, $%.2x ; %s\n' % (b['tiles'][0] & 255, b['tiles'][1] & 255, b['tiles'][2] & 255, b['tiles'][3] & 255, b['name']))

outfile.write('\nSECTION "BlockData", ROM0\n\n')

outfile.write('BlockAppearanceColor::\n')
for b in all_blocks:
	outfile.write('\tdb $%.2x ; %s\n' % (b['tiles'][0] >> 8, b['name']))

outfile.write('BlockFlags::\n')
for b in all_blocks:
	outfile.write('\tdb $%x|BlockClass_%s ; %s\n' % \
	  (b['solid'] * 0x80, b['class'], b['name']))

# Write all interaction type tables corresponding to each interaction set
for interaction, routines in all_interactions.items():
	outfile.write("BlockRoutines%s::\n" % interaction)
	for routine in routines:
		outfile.write('\tdw %s\n' % routine)

outfile.close()

# -------------------------------------------------------------------

# Generate the enum in a separate file
outfile = open(block_enum_filename, "w")
outfile.write('; This is automatically generated. Edit "%s" instead\n' % block_definition_filename)

for i, b in enumerate(all_blocks):
	outfile.write('DEF BlockType_%s EQU %d\n' % (b['name'], i))
outfile.write('\n')

for k, v in all_markers.items():
	outfile.write('DEF BlockMarker_%s EQU %d\n' % (k, v))
outfile.write('\n')

for i, b in enumerate(all_classes):
	outfile.write('DEF BlockClass_%s EQU %s\n' % (b, i))
outfile.write('\n')

outfile.close()
