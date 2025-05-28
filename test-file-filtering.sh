#!/bin/bash

echo "=== Testing File Filtering Implementation ==="
echo

# Clean start
echo "1. Cleaning up..."
rm -f ~/.pif/pif.db
echo "✅ Database cleared"
echo

# Get absolute path to test filtering directory
TEST_FILTER_PATH="$(cd "$(dirname "$0")/test-filtering" && pwd)"

echo "2. Creating test project with file filtering..."
echo "Test directory: $TEST_FILTER_PATH"
echo

# First, let's see what files exist
echo "Files in test directory:"
find "$TEST_FILTER_PATH" -type f | sort
echo

# Create and open the project
npm start << EOF
add filter-test "$TEST_FILTER_PATH" "File Filtering Test"
open filter-test
exit
EOF

echo
echo "3. Checking indexed files..."
npm start << EOF
info filter-test
exit
EOF

echo
echo "=== Summary ==="
echo "Files that SHOULD have been indexed:"
echo "  - src/index.js"
echo "  - src/utils.ts"
echo "  - package.json"
echo "  - README.md"
echo
echo "Files that should NOT have been indexed:"
echo "  - node_modules/some-package/index.js (excluded by default)"
echo "  - dist/bundle.js (excluded by .gitignore)"
echo "  - debug.log (excluded by default pattern)"
echo "  - .env (excluded by .gitignore)"
echo "  - data.bin (not an indexable extension)"
echo
echo "✅ Test completed!"