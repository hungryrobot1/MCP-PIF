FROM python:3.11-slim

# Install system dependencies
RUN apt-get update && apt-get install -y \
    gcc \
    g++ \
    curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app

# Create virtual environment
RUN python -m venv /app/venv
ENV PATH="/app/venv/bin:$PATH"
ENV VIRTUAL_ENV="/app/venv"

# Copy requirements first for better caching
COPY requirements.txt .
COPY test-requirements.txt .

# Install Python dependencies in venv
RUN pip install --upgrade pip && \
    pip install --no-cache-dir -r requirements.txt

# Install test dependencies conditionally
ARG INSTALL_DEV=false
RUN if [ "$INSTALL_DEV" = "true" ]; then \
    pip install --no-cache-dir -r test-requirements.txt; \
    fi

# Copy application code
COPY . .

# Create a non-root user
RUN useradd -m -u 1000 mlservice && \
    chown -R mlservice:mlservice /app
USER mlservice

# Ensure venv is activated for the user
ENV PATH="/app/venv/bin:$PATH"
ENV VIRTUAL_ENV="/app/venv"

# Expose port
EXPOSE 8002

# Start the server
CMD ["python", "-m", "server"]