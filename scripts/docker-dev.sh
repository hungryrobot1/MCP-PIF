#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Get script directory
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"

cd "$PROJECT_ROOT"

# Start services
start() {
    echo -e "${YELLOW}Starting MCP-PIF services...${NC}"
    docker-compose up -d
    
    echo -e "${YELLOW}Waiting for services to be healthy...${NC}"
    sleep 5
    
    docker-compose ps
    echo -e "${GREEN}Services started!${NC}"
}

# Stop services (keeps data)
stop() {
    echo -e "${YELLOW}Stopping services...${NC}"
    docker-compose stop
    echo -e "${GREEN}Services stopped${NC}"
}

# Stop and remove everything
clean() {
    echo -e "${YELLOW}Cleaning up everything...${NC}"
    docker-compose down -v
    docker image prune -f
    echo -e "${GREEN}Cleanup complete${NC}"
}

# Show logs
logs() {
    docker-compose logs -f "$@"
}

# Show service status
status() {
    echo -e "${YELLOW}Service status:${NC}"
    docker-compose ps
    
    echo -e "\n${YELLOW}Resource usage:${NC}"
    docker stats --no-stream $(docker-compose ps -q) 2>/dev/null || echo "No containers running"
    
    echo -e "\n${YELLOW}Disk usage:${NC}"
    docker system df
}

# Rebuild services
rebuild() {
    echo -e "${YELLOW}Rebuilding services...${NC}"
    docker-compose build --no-cache
    echo -e "${GREEN}Rebuild complete${NC}"
}

# Execute command in service
exec_cmd() {
    service=$1
    shift
    docker-compose exec "$service" "$@"
}

# Show help
help() {
    echo "Usage: $0 {start|stop|clean|logs|status|rebuild|exec|help}"
    echo ""
    echo "Commands:"
    echo "  start    - Start all services"
    echo "  stop     - Stop services (keeps data)"
    echo "  clean    - Stop services and remove all data"
    echo "  logs     - Show logs (optionally specify service)"
    echo "  status   - Show service status and resource usage"
    echo "  rebuild  - Rebuild service images"
    echo "  exec     - Execute command in service"
    echo ""
    echo "Examples:"
    echo "  $0 logs ml-service    # Show ML service logs"
    echo "  $0 exec neo4j bash    # Get shell in Neo4j container"
}

# Main command handling
case "$1" in
    start)
        start
        ;;
    stop)
        stop
        ;;
    clean)
        clean
        ;;
    logs)
        shift
        logs "$@"
        ;;
    status)
        status
        ;;
    rebuild)
        rebuild
        ;;
    exec)
        shift
        exec_cmd "$@"
        ;;
    help)
        help
        ;;
    *)
        echo -e "${RED}Invalid command${NC}"
        help
        exit 1
        ;;
esac
