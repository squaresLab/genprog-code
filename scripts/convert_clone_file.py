

#this is a short script to convert a Deckard clusters file into suitable input
#for Genprog

#arguments should be passed in as input_file output_file in that order
import sys

#in debug mode, will print lines being thrown out of clusters file
debug = False

def convert_file(input,output):
	input_file = open(input,'r')
	output_file = open(output,'w+')
	write_blank = False #make sure we don't start with a blank line
	                    #or write consecutive blank lines
	seen_patterns = set([]) #make sure we don't include duplicate clone groups
	building = '' #construct the clone group
	for line in input_file:
		write_line = True
		split_line = line.split()

		#in case different parameters are used and this changes the output,
		#don't assume the filename or line numbers are at a fixed index
		index = 0
		filename = ''
		startline = -1
		num_lines = -1
		if (len(split_line) == 0):
			if (write_blank):
				if (building.count('\n') != 1):
					if (building not in seen_patterns):
						seen_patterns.add(building)
						building = building + '\n'
						output_file.write(building)
					
					else:
						if (debug):
							print "FOUND DUPLICATE GROUP:\n"+building
				else:
					if (debug):
						print "FOUND SINGLE LINE CLONE:\n"+building
				write_blank = False
				building = ''

		else:
			for tok in split_line:
				if tok == 'FILE':
					filename = split_line[index+1]
				if tok[:4] == 'LINE':
					split_tok = tok.split(':')
					if (len(split_tok) != 3):
						print "BAD LINE DELIMITER AT LINE: " + line
					else:
						startline = int(split_tok[1])
						num_lines = int(split_tok[2])
				index += 1

			if (filename == ''):
				print "NO FILENAME FOUND AT LINE: " + line
				write_line = False
			if (startline == -1):
				print "NO STARTLINE FOUND AT LINE: " + line
				write_line = False
			if (num_lines == -1):
				print "NO NUM_LINES FOUND AT LINE: " + line
				write_line = False

			if write_line:
				write_blank = True
				toWrite = '%d,%d,%s\n' % (startline,num_lines,filename)
				if toWrite not in building:
					building = building + toWrite
				else:
					if (debug):
						print "FOUND DUPLICATE WITHIN A GROUP:\n"+toWrite

	input_file.close()
	output_file.close()
	return

def main():
	if (len(sys.argv) != 3):
		print "NEED 2 ARGUMENTS"
		print "ARGUMENT FORMAT: input_file output_file"
		return 1

	convert_file(sys.argv[1],sys.argv[2])
	return 0

main()


