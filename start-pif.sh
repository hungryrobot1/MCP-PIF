#!/bin/bash

# MCP-PIF Smart Startup Script
# Usage: ./start-pif.sh [--dev] [--setup] [--stop]

set -e

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
ML_PORT=8002
ML_HOST="127.0.0.1"
PID_FILE=".pif-services.pid"

# Function to print colored output
log() {
    echo -e "${GREEN}[PIF]${NC} $1"
}

warn() {
    echo -e "${YELLOW}[PIF]${NC} $1"
}

error() {
    echo -e "${RED}[PIF]${NC} $1"
}

info() {
    echo -e "${BLUE}[PIF]${NC} $1"
}

# Function to check if a port is in use
check_port() {
    lsof -i :$1 >/dev/null 2>&1
}

# Function to wait for service
wait_for_service() {
    local url=$1
    local max_attempts=30
    local attempt=1
    
    info "Waiting for service at $url..."
    
    while [ $attempt -le $max_attempts ]; do
        if curl -s "$url" >/dev/null 2>&1; then
            log "Service is ready!"
            return 0
        fi
        
        printf "."
        sleep 1
        attempt=$((attempt + 1))
    done
    
    error "Service failed to start within ${max_attempts}s"
    return 1
}

# Function to setup dependencies
setup() {
    log "Setting up MCP-PIF dependencies..."
    
    # Check Node.js
    if ! command -v node &> /dev/null; then
        error "Node.js not found. Please install Node.js 18+"
        exit 1
    fi
    
    # Check Python
    if ! command -v python3 &> /dev/null; then
        error "Python 3 not found. Please install Python 3.8+"
        exit 1
    fi
    
    # Install Node dependencies
    log "Installing Node.js dependencies..."
    npm install
    
    # Setup Python environment
    log "Setting up Python ML module..."
    cd ml_module
    
    if [ ! -d "venv" ]; then
        python3 -m venv venv
    fi
    
    source venv/bin/activate
    pip install -r requirements.txt
    cd ..
    
    # Build TypeScript
    log "Building TypeScript..."
    npm run build
    
    log "Setup complete! Run './start-pif.sh' to start the system."
}

# Function to stop services
stop() {
    log "Stopping PIF services..."
    
    if [ -f "$PID_FILE" ]; then
        while read -r pid; do
            if kill -0 "$pid" 2>/dev/null; then
                kill "$pid" 2>/dev/null || true
                warn "Stopped process $pid"
            fi
        done < "$PID_FILE"
        rm "$PID_FILE"
    fi
    
    # Kill any remaining processes on our ports
    if check_port $ML_PORT; then
        warn "Force killing process on port $ML_PORT"
        lsof -ti :$ML_PORT | xargs kill -9 2>/dev/null || true
    fi
    
    log "Services stopped"
}

# Function to start services
start() {
    local dev_mode=$1
    
    # Stop any existing services
    stop
    
    log "Starting MCP-PIF system..."
    
    # Build if not in dev mode
    if [ "$dev_mode" != "dev" ]; then
        info "Building project..."
        npm run build
    fi
    
    # Start ML service
    info "Starting ML service..."
    cd ml_module
    source venv/bin/activate
    
    if [ "$dev_mode" = "dev" ]; then
        ML_DEBUG=true python server.py &
    else
        python server.py &
    fi
    
    ML_PID=$!
    echo $ML_PID >> "../$PID_FILE"
    cd ..
    
    # Wait for ML service
    if wait_for_service "http://$ML_HOST:$ML_PORT/health"; then
        # Initialize system
        log "Initializing PIF system..."
        ./dist/cli/index.js system init --ml-url "http://$ML_HOST:$ML_PORT"
        
        echo ""
        log "🚀 MCP-PIF is ready!"
        echo ""
        info "Try these commands:"
        echo "  pif project add myproject /path/to/project"
        echo "  pif search 'your search query'"
        echo "  pif project list"
        echo ""
        info "To stop: ./start-pif.sh --stop"
        
        # Keep script running to catch Ctrl+C
        trap stop EXIT INT TERM
        wait $ML_PID
    else
        error "Failed to start ML service"
        stop
        exit 1
    fi
}

# Main logic
case "${1:-start}" in
    --setup|setup)
        setup
        ;;
    --stop|stop)
        stop
        ;;
    --dev|dev)
        start "dev"
        ;;
    --start|start|"")
        start
        ;;
    --help|help)
        echo "MCP-PIF Startup Script"
        echo ""
        echo "Usage: $0 [COMMAND]"
        echo ""
        echo "Commands:"
        echo "  setup    Install dependencies and build"
        echo "  start    Start all services (default)"
        echo "  dev      Start in development mode"
        echo "  stop     Stop all services"
        echo "  help     Show this help"
        ;;
    *)
        error "Unknown command: $1"
        echo "Use '$0 help' for usage information"
        exit 1
        ;;
esac