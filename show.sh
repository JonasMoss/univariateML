#!/usr/bin/env bash
set -euo pipefail

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 <output_file>"
  exit 1
fi

out_file="$1"
: > "$out_file"

# 1. DESCRIPTION
if [ -f DESCRIPTION ]; then
  echo "## DESCRIPTION" >> "$out_file"
  echo '```' >> "$out_file"
  cat DESCRIPTION >> "$out_file"
  echo '```' >> "$out_file"
else
  echo "WARNING: No DESCRIPTION file found." >&2
fi

# 2. R source files
echo -e "\n## R source files (R/)" >> "$out_file"
find R -type f \( -iname "*.R" -o -iname "*.r" \) | sort | \
while read -r file; do
  echo -e "\n### ${file#./}\n" >> "$out_file"
  echo '```r' >> "$out_file"
  cat "$file" >> "$out_file"
  echo '```' >> "$out_file"
done

# 3. src/ tree of .cpp only
if [ -d src ]; then
  echo -e "\n## src/ directory structure (only .cpp)\n" >> "$out_file"
  echo '```' >> "$out_file"
  tree -P '*.cpp' src
  echo '```' >> "$out_file"

  # 4. Dump all .cpp from src/
  echo -e "\n## src/ C++ source files\n" >> "$out_file"
  find src -type f -iname "*.cpp" | sort | \
  while read -r file; do
    echo -e "\n### ${file#./}\n" >> "$out_file"
    echo '```cpp' >> "$out_file"
    cat "$file" >> "$out_file"
    echo '```' >> "$out_file"
  done
else
  echo "WARNING: No src/ directory found." >&2
fi

echo -e "\n> Document generated at $(date) by generate_pkg_doc.sh" >> "$out_file"

