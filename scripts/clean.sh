#!/bin/bash

# Shell script to clean up debug metadata from the project
# This script is intended to be run from the root of the project

# List of extensions to delete
extensions=(.ast .lowered.jswt .hir.jswt .mir.jswt .wast .mem)

# Loop through the list of extensions
for ext in "${extensions[@]}"
do
    # Delete all files with the extension
    find . -name "*$ext" -type f -delete
done

# delete coverage metadata files that begin with default_
find . -name "default_*" -type f -delete

