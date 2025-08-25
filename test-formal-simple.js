const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']
});

console.log('=== Testing Formal Reasoning (Simplified) ===\n');

let requestId = 1;

// Capture stdout (JSON responses)
server.stdout.on('data', (data) => {
  const response = data.toString().trim();
  console.log('Response:', response);
  try {
    const json = JSON.parse(response);
    if (json.result && json.result.content) {
      console.log('Result:', json.result.content[0].text);
    }
  } catch (e) {
    // Not JSON
  }
  console.log('---');
});

// Capture stderr (debug logs)
server.stderr.on('data', (data) => {
  // console.log('Log:', data.toString().trim());
});

function sendRequest(method, params = {}) {
  const request = {
    jsonrpc: "2.0",
    id: requestId++,
    method: method,
    params: params
  };
  console.log('\nSending:', method, params.name || '');
  server.stdin.write(JSON.stringify(request) + '\n');
}

// Test sequence with clearer format
const tests = [
  {
    delay: 500,
    name: 'Initialize',
    method: 'initialize',
    params: { protocolVersion: "2024-11-05" }
  },
  {
    delay: 1000,
    name: 'Lambda - Simple Identity',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        // Using vector notation that Clojure can read
        expression: "['λ 'x 'x]"
      }
    }
  },
  {
    delay: 1500,
    name: 'Lambda - Identity Applied',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        // Application: (λx.x) 42
        expression: "[['λ 'x 'x] 42]"
      }
    }
  },
  {
    delay: 2000,
    name: 'Lambda - Church Zero',
    method: 'tools/call',
    params: {
      name: 'lambda-eval',
      arguments: {
        expression: "['λ 'f ['λ 'x 'x]]"
      }
    }
  },
  {
    delay: 2500,
    name: 'Type Check - Identity',
    method: 'tools/call',
    params: {
      name: 'type-check',
      arguments: {
        expression: "['λ 'x 'x]"
      }
    }
  },
  {
    delay: 3000,
    name: 'Type Check - Const',
    method: 'tools/call',
    params: {
      name: 'type-check',
      arguments: {
        expression: "['λ 'x ['λ 'y 'x]]"
      }
    }
  },
  {
    delay: 3500,
    name: 'Proof - Simple And',
    method: 'tools/call',
    params: {
      name: 'prove',
      arguments: {
        premises: ['P', 'Q'],
        goal: '[:and P Q]',
        method: 'natural-deduction'
      }
    }
  },
  {
    delay: 4000,
    name: 'Proof - Modus Ponens',
    method: 'tools/call',
    params: {
      name: 'prove',
      arguments: {
        premises: ['A', '[:implies A B]'],
        goal: 'B',
        method: 'natural-deduction'
      }
    }
  },
  {
    delay: 4500,
    name: 'Create Lambda Tool',
    method: 'tools/call',
    params: {
      name: 'meta-evolve',
      arguments: {
        type: 'add-tool',
        name: 'identity-fn',
        description: 'Identity function in lambda calculus',
        code: "['λ 'x 'x]",
        tool_type: 'lambda'
      }
    }
  },
  {
    delay: 5000,
    name: 'Execute Lambda Tool',
    method: 'tools/call',
    params: {
      name: 'execute-tool',
      arguments: {
        'tool-name': 'identity-fn',
        arguments: {
          expression: "[['λ 'x 'x] 'hello]"
        }
      }
    }
  }
];

// Execute tests
tests.forEach(test => {
  setTimeout(() => {
    console.log(`\n=== ${test.name} ===`);
    sendRequest(test.method, test.params);
  }, test.delay);
});

// Shutdown
setTimeout(() => {
  console.log('\nTest complete. Shutting down...');
  server.stdin.end();
  process.exit(0);
}, 6000);