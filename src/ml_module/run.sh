#!/bin/bash
# Run the stateless ML service

# Check if virtual environment exists
if [ ! -d "venv" ]; then
    echo "Creating virtual environment..."
    python3 -m venv venv
fi

# Activate virtual environment
source venv/bin/activate

# Install minimal requirements
echo "Installing requirements..."
pip install -q --upgrade pip
pip install -q fastapi uvicorn sentence-transformers numpy

# Set environment variables
export TEXT_MODEL="sentence-transformers/all-MiniLM-L6-v2"
export CODE_MODEL="microsoft/codebert-base"
export DOCS_MODEL="sentence-transformers/all-mpnet-base-v2"

# Run the stateless service
echo "Starting stateless ML service on port 8000..."
python main.py