#!/usr/bin/env node

// Integration test for the ClojureScript MCP server

const { spawn } = require('child_process');

console.log('[TEST] Starting MCP server integration test...');
const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'inherit']
});

let responseBuffer = '';
let testResults = [];

server.stdout.on('data', (data) => {
  responseBuffer += data.toString();
  const lines = responseBuffer.split('\n');
  responseBuffer = lines.pop();
  
  lines.forEach(line => {
    if (line.trim() && line.startsWith('{')) {
      try {
        const response = JSON.parse(line);
        console.log('[RESPONSE]', JSON.stringify(response, null, 2));
        testResults.push(response);
      } catch (e) {
        // Not JSON, skip
      }
    }
  });
});

async function sendRequest(request) {
  return new Promise((resolve) => {
    console.log('[SENDING]', JSON.stringify(request));
    server.stdin.write(JSON.stringify(request) + '\n');
    setTimeout(resolve, 500); // Give time for response
  });
}

async function runTests() {
  // Test 1: Initialize
  await sendRequest({
    jsonrpc: "2.0",
    id: 1,
    method: "initialize",
    params: {
      protocolVersion: "2024-11-05",
      capabilities: {},
      clientInfo: {
        name: "integration-test",
        version: "1.0.0"
      }
    }
  });

  // Test 2: List tools
  await sendRequest({
    jsonrpc: "2.0",
    id: 2,
    method: "tools/list",
    params: {}
  });

  // Test 3: Use memory-store tool
  await sendRequest({
    jsonrpc: "2.0",
    id: 3,
    method: "tools/call",
    params: {
      name: "memory-store",
      arguments: {
        key: "test-key",
        value: "Hello from ClojureScript MCP!"
      }
    }
  });

  // Test 4: Retrieve from memory
  await sendRequest({
    jsonrpc: "2.0",
    id: 4,
    method: "tools/call",
    params: {
      name: "memory-retrieve",
      arguments: {
        key: "test-key"
      }
    }
  });

  // Test 5: Get journal entries
  await sendRequest({
    jsonrpc: "2.0",
    id: 5,
    method: "tools/call",
    params: {
      name: "journal-recent",
      arguments: {
        limit: 5
      }
    }
  });

  // Test 6: List capabilities
  await sendRequest({
    jsonrpc: "2.0",
    id: 6,
    method: "tools/call",
    params: {
      name: "list-capabilities",
      arguments: {}
    }
  });

  // Give time for all responses
  setTimeout(() => {
    console.log('\n[TEST SUMMARY]');
    console.log(`Total responses received: ${testResults.length}`);
    console.log(`Initialize response: ${testResults.find(r => r.id === 1) ? 'OK' : 'FAILED'}`);
    console.log(`Tools list response: ${testResults.find(r => r.id === 2) ? 'OK' : 'FAILED'}`);
    console.log(`Memory store response: ${testResults.find(r => r.id === 3) ? 'OK' : 'FAILED'}`);
    console.log(`Memory retrieve response: ${testResults.find(r => r.id === 4) ? 'OK' : 'FAILED'}`);
    console.log(`Journal response: ${testResults.find(r => r.id === 5) ? 'OK' : 'FAILED'}`);
    console.log(`Capabilities response: ${testResults.find(r => r.id === 6) ? 'OK' : 'FAILED'}`);
    
    server.stdin.end();
    process.exit(testResults.length === 6 ? 0 : 1);
  }, 2000);
}

// Start tests after server is ready
setTimeout(runTests, 1000);