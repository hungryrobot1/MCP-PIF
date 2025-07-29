const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']  // separate stderr
});

console.log('=== Testing Clean Protocol ===\n');

// Capture stdout (should only be JSON)
server.stdout.on('data', (data) => {
  console.log('STDOUT:', data.toString().trim());
});

// Capture stderr (debug logs)
server.stderr.on('data', (data) => {
  console.log('STDERR:', data.toString().trim());
});

// Send initialize
setTimeout(() => {
  const request = JSON.stringify({
    jsonrpc: "2.0",
    id: 1,
    method: "initialize",
    params: { protocolVersion: "2024-11-05" }
  });
  console.log('\nSending:', request);
  server.stdin.write(request + '\n');
}, 500);

// Send list tools
setTimeout(() => {
  const request = JSON.stringify({
    jsonrpc: "2.0",
    id: 2,
    method: "tools/list",
    params: {}
  });
  console.log('\nSending:', request);
  server.stdin.write(request + '\n');
}, 1000);

setTimeout(() => {
  server.stdin.end();
}, 1500);