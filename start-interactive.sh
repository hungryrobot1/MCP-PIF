#!/bin/bash

# Interactive startup script that gives you the CLI

echo "🚀 Starting MCP-PIF in interactive mode..."

# Get the directory of this script
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

# Function to cleanup on exit
cleanup() {
    echo -e "\n🛑 Shutting down ML module..."
    if [ ! -z "$ML_PID" ]; then
        kill $ML_PID 2>/dev/null
    fi
    exit 0
}

# Set up trap for cleanup on Ctrl+C
trap cleanup INT TERM

# Start ML module in background (silently)
echo "📊 Starting ML module in background..."
(
    cd "$SCRIPT_DIR/src/ml_module"
    if [ ! -d "venv" ]; then
        echo "Setting up ML module virtual environment..."
        ./setup.sh
    fi
    ./venv/bin/python main.py > /dev/null 2>&1
) &
ML_PID=$!

# Wait for ML module to be ready
echo "⏳ Waiting for ML module to start..."
MAX_RETRIES=30
RETRY_COUNT=0
while ! curl -s http://localhost:8001/health > /dev/null 2>&1; do
    sleep 1
    RETRY_COUNT=$((RETRY_COUNT + 1))
    if [ $RETRY_COUNT -ge $MAX_RETRIES ]; then
        echo "❌ ML module failed to start after $MAX_RETRIES seconds"
        cleanup
        exit 1
    fi
done

echo "✅ ML module is ready!"
echo "🔧 Starting interactive CLI..."
echo ""

# Start TypeScript server in foreground (interactive)
export ML_SERVICE_ENABLED=true
export ML_SERVICE_URL=http://localhost:8001

# Check if node_modules exists
if [ ! -d "node_modules" ]; then
    echo "Installing dependencies..."
    npm install
fi

# Check if dist exists
if [ ! -d "dist" ]; then
    echo "Building TypeScript..."
    npm run build
fi

# Run the TypeScript server in foreground
npm start

# This will only execute if npm start exits
cleanup