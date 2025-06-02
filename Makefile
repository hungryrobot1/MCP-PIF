.PHONY: setup start stop clean dev

# Colors for output
GREEN = \033[0;32m
YELLOW = \033[1;33m
NC = \033[0m # No Color

setup: ## Install all dependencies
	@echo "$(YELLOW)Setting up MCP-PIF...$(NC)"
	npm install
	cd ml_module && python -m venv venv && source venv/bin/activate && pip install -r requirements.txt
	@echo "$(GREEN)Setup complete!$(NC)"

start: ## Start all services and initialize system
	@echo "$(YELLOW)Starting MCP-PIF services...$(NC)"
	@echo "Starting ML service..."
	cd ml_module && source venv/bin/activate && python server.py &
	@echo "$$!" > .ml_pid
	@sleep 3
	@echo "$(GREEN)Initializing system...$(NC)"
	npm run build
	./dist/cli/index.js system init --ml-url http://127.0.0.1:8002
	@echo "$(GREEN)🚀 MCP-PIF is ready! Try: pif project add <name> <path>$(NC)"

stop: ## Stop all services
	@echo "$(YELLOW)Stopping MCP-PIF services...$(NC)"
	@if [ -f .ml_pid ]; then kill `cat .ml_pid` 2>/dev/null || true; rm .ml_pid; fi
	@echo "$(GREEN)Services stopped$(NC)"

dev: ## Start in development mode
	@echo "$(YELLOW)Starting development mode...$(NC)"
	cd ml_module && source venv/bin/activate && ML_DEBUG=true python server.py &
	@echo "$$!" > .ml_pid
	@echo "$(GREEN)Development ML service started on port 8002$(NC)"
	@echo "Use: pif system init --ml-url http://127.0.0.1:8002"

clean: stop ## Clean build artifacts and stop services
	rm -rf dist node_modules ml_module/venv
	@echo "$(GREEN)Cleaned$(NC)"

help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-15s\033[0m %s\n", $$1, $$2}'