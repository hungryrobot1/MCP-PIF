const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']
});

console.log('=== Testing Dynamic Tool Creation ===\n');

let requestId = 1;

// Capture stdout (JSON responses)
server.stdout.on('data', (data) => {
  const response = data.toString().trim();
  console.log('Response:', response);
  try {
    const json = JSON.parse(response);
    console.log('Parsed:', JSON.stringify(json, null, 2));
  } catch (e) {
    // Not JSON
  }
  console.log('---');
});

// Capture stderr (debug logs)
server.stderr.on('data', (data) => {
  console.log('Log:', data.toString().trim());
});

function sendRequest(method, params = {}) {
  const request = {
    jsonrpc: "2.0",
    id: requestId++,
    method: method,
    params: params
  };
  console.log('\nSending:', JSON.stringify(request));
  server.stdin.write(JSON.stringify(request) + '\n');
}

// Test sequence
setTimeout(() => {
  console.log('\n1. Initialize the server');
  sendRequest('initialize', { protocolVersion: "2024-11-05" });
}, 500);

setTimeout(() => {
  console.log('\n2. List initial tools');
  sendRequest('tools/list');
}, 1000);

setTimeout(() => {
  console.log('\n3. Create a new tool with meta-evolve');
  sendRequest('tools/call', {
    name: 'meta-evolve',
    arguments: {
      type: 'add-tool',
      name: 'square-number',
      description: 'Square a number',
      code: '(args) => args.n * args.n',
      tool_type: 'arithmetic',
      schema: {
        type: 'object',
        properties: {
          n: { type: 'number' }
        },
        required: ['n']
      }
    }
  });
}, 1500);

setTimeout(() => {
  console.log('\n4. List all tools (including runtime ones)');
  sendRequest('tools/call', {
    name: 'list-all-tools',
    arguments: {}
  });
}, 2000);

setTimeout(() => {
  console.log('\n5. Execute the dynamically created tool');
  sendRequest('tools/call', {
    name: 'execute-tool',
    arguments: {
      'tool-name': 'square-number',
      arguments: { n: 7 }
    }
  });
}, 2500);

setTimeout(() => {
  console.log('\n6. Try to call the tool directly (will fail in Claude)');
  sendRequest('tools/call', {
    name: 'square-number',
    arguments: { n: 7 }
  });
}, 3000);

setTimeout(() => {
  console.log('\nTest complete. Shutting down...');
  server.stdin.end();
}, 3500);