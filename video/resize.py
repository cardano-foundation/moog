import json
import sys

# get the input file name as CLI arg
input_file = sys.argv[1]
# get the width and height from CLI args
width = int(sys.argv[2])
height = int(sys.argv[3])
# base name without extension
base_name = input_file.rsplit(".", 1)[0]
# extension
extension = input_file.rsplit(".", 1)[1]
#bail out if extension is not cast
if extension != "cast":
    print("Input file must be a .cast file")
    sys.exit(1)
# output file name
output_file = f"{base_name}-resized.cast"

with open(input_file) as f:
    # open the output file
    with open(output_file, "w") as out_f:
        # read all lines
        lines = f.readlines()
        # parse the first line as json
        header = json.loads(lines[0])
        # set width to 100
        header["width"] = width
        # set height to 35
        header["height"] = height
        # write the modified header line
        out_f.write(json.dumps(header) + "\n")
        # copy all subsequent lines as is
        for line in lines[1:]:
            out_f.write(line)

# move the output file to replace the input file
import os
os.replace(input_file, f"{input_file}.not-resized.bak")
os.replace(output_file, input_file)