const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']
});

console.log('=== Lambda Debug Test ===\n');

let requestId = 1;

server.stdout.on('data', (data) => {
  const response = data.toString().trim();
  try {
    const json = JSON.parse(response);
    if (json.result && json.result.content) {
      console.log('Result:', json.result.content[0].text);
    }
  } catch (e) {}
});

server.stderr.on('data', (data) => {
  // Suppress logs
});

function sendRequest(method, params = {}) {
  const request = {
    jsonrpc: "2.0",
    id: requestId++,
    method: method,
    params: params
  };
  server.stdin.write(JSON.stringify(request) + '\n');
}

const tests = [
  { delay: 500, method: 'initialize', params: { protocolVersion: "2024-11-05" } },
  
  // Test 1: Without quotes - let Clojure read it
  {
    delay: 1000,
    name: 'Test 1: Vector with symbols',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "[λ x x]"  // No quotes, just symbols
      }
    }
  },
  
  // Test 2: With explicit lambda symbol
  {
    delay: 1500,
    name: 'Test 2: With lambda symbol',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "[(symbol \"λ\") (symbol \"x\") (symbol \"x\")]"
      }
    }
  },
  
  // Test 3: Using backslash lambda
  {
    delay: 2000,
    name: 'Test 3: Backslash lambda',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "[\\lambda x x]"
      }
    }
  },
  
  // Test 4: Application without quotes
  {
    delay: 2500,
    name: 'Test 4: Application (no quotes)',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "[[λ x x] 42]"
      }
    }
  },
  
  // Test 5: Using fn syntax
  {
    delay: 3000,
    name: 'Test 5: Clojure fn syntax',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "(fn [x] x)"
      }
    }
  }
];

tests.forEach(test => {
  setTimeout(() => {
    if (test.name) console.log(`\n${test.name}:`);
    sendRequest(test.method, test.params);
  }, test.delay);
});

setTimeout(() => {
  console.log('\nDone.');
  server.stdin.end();
  process.exit(0);
}, 4000);