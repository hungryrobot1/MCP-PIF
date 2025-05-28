#!/bin/bash

# Simple run script for ML module with minimal models

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Check if virtual environment exists
if [ ! -d "$SCRIPT_DIR/venv" ]; then
    echo "Virtual environment not found. Running setup first..."
    "$SCRIPT_DIR/setup.sh"
fi

# Activate virtual environment
source "$SCRIPT_DIR/venv/bin/activate"

# Set Python path to include the ml_module directory
export PYTHONPATH="$SCRIPT_DIR:$PYTHONPATH"

# Change to the ml_module directory
cd "$SCRIPT_DIR"

# Use simplified configuration
export ML_TEXT_MODEL="sentence-transformers/all-MiniLM-L6-v2"
export ML_CODE_MODEL="sentence-transformers/all-MiniLM-L6-v2"
export ML_DOCS_MODEL="sentence-transformers/all-MiniLM-L6-v2"
export ML_SQLITE_DB_PATH="../../pif.db"

# Run the ML module
echo "Starting ML module with simplified configuration..."
python main.py