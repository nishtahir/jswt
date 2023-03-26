#!/bin/bash

# Delete the grammar file if it exists
rm ./grammar.txt

# list of files to add to the grammar
files=(
    "jswt-parser/src/program.rs"
    "jswt-parser/src/lib.rs"
    "jswt-parser/src/import.rs"
    "jswt-parser/src/class.rs"
    "jswt-parser/src/function.rs"
    "jswt-parser/src/variable.rs"
    "jswt-parser/src/literal.rs"
    "jswt-parser/src/expression.rs"
)

# search each file in files
for file in "${files[@]}"; do
    # get the content of the file
    content=$(cat $file)
    # print lines that contain //// to the grammar file without the //// prefix. Remove the leading spaces
    echo "$content" | grep -E "////" | sed 's/\/\/\/\///g' | sed 's/^ *//g' >> ./grammar.txt
    # print a new line to the grammar file
    echo "" >> ./grammar.txt
done