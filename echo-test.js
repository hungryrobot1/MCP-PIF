const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'inherit']  // inherit stderr to see errors directly
});

let responseData = '';
server.stdout.on('data', (data) => {
  responseData += data.toString();
  const lines = responseData.split('\n');
  responseData = lines.pop(); // Keep incomplete line
  
  lines.forEach(line => {
    if (line.trim()) {
      console.log('RESPONSE:', line);
    }
  });
});

// Send initialize request  
setTimeout(() => {
  const request = {
    jsonrpc: "2.0",
    id: 1,
    method: "initialize",
    params: {
      protocolVersion: "2024-11-05",
      capabilities: {},
      clientInfo: {
        name: "test-client",
        version: "1.0.0"
      }
    }
  };
  
  console.log('SENDING:', JSON.stringify(request));
  server.stdin.write(JSON.stringify(request) + '\n');
}, 500);

// Give time for response
setTimeout(() => {
  console.log('Closing server...');
  server.stdin.end();
}, 2000);

server.on('close', (code) => {
  console.log('Server exited with code:', code);
});