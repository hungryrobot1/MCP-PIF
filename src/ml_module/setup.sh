#!/bin/bash

# Setup script for ML module

echo "Setting up Python virtual environment for ML module..."

# Find the best Python version
PYTHON_CMD=""
if command -v python3.12 &> /dev/null; then
    PYTHON_CMD="python3.12"
    echo "Using Python 3.12"
elif command -v python3.11 &> /dev/null; then
    PYTHON_CMD="python3.11"
    echo "Using Python 3.11"
elif command -v python3.10 &> /dev/null; then
    PYTHON_CMD="python3.10"
    echo "Using Python 3.10"
else
    PYTHON_CMD="python3"
    echo "Using default Python 3 ($(python3 --version))"
    echo "WARNING: Python 3.13 may have compatibility issues. Consider installing Python 3.11 or 3.12."
fi

# Remove existing venv if it exists (to ensure clean setup)
if [ -d "venv" ]; then
    echo "Removing existing virtual environment..."
    rm -rf venv
fi

# Create virtual environment
echo "Creating virtual environment..."
$PYTHON_CMD -m venv venv

# Activate virtual environment
echo "Activating virtual environment..."
source venv/bin/activate

# Upgrade pip and install setuptools
echo "Upgrading pip and setuptools..."
pip install --upgrade pip setuptools wheel

# Install requirements
echo "Installing requirements..."
pip install -r requirements.txt

if [ $? -eq 0 ]; then
    echo ""
    echo "Setup complete! 🎉"
    echo ""
    echo "To run the ML module:"
    echo "  1. Activate the virtual environment: source venv/bin/activate"
    echo "  2. Run the server: python main.py"
    echo ""
    echo "Or use the run script: ./run.sh"
else
    echo ""
    echo "Setup failed! Please check the error messages above."
    echo "You may need to install Python 3.11 or 3.12 for better compatibility."
    echo "On macOS with Homebrew: brew install python@3.11"
fi