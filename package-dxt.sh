#!/bin/bash

# Build the ClojureScript project first
echo "Building ClojureScript project..."
npx shadow-cljs compile mcp-server

# Create temporary directory for packaging
echo "Creating package structure..."
mkdir -p dxt-temp/out

# Copy necessary files
cp manifest.json dxt-temp/
cp -r out/mcp-server.js dxt-temp/out/
cp package.json dxt-temp/
cp package-lock.json dxt-temp/

# Copy documentation
cp CLAUDE_*.md dxt-temp/
cp DYNAMIC_TOOLS_SUMMARY.md dxt-temp/

# Install production dependencies
echo "Installing production dependencies..."
cd dxt-temp
npm ci --production
cd ..

# Create the .dxt file (which is just a zip)
echo "Creating mcp-pif-cljs.dxt..."
cd dxt-temp
zip -r ../mcp-pif-cljs.dxt .
cd ..

# Clean up
echo "Cleaning up..."
rm -rf dxt-temp

echo "Package created: mcp-pif-cljs.dxt"
echo "You can now install this in Claude Desktop!"