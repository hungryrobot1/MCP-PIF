const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']
});

console.log('=== Testing Formal Reasoning Features ===\n');

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
  console.log('\n2. Test Lambda Calculus - Identity function');
  sendRequest('tools/call', {
    name: 'lambda-eval',
    arguments: {
      expression: '[(λ x x) 42]'
    }
  });
}, 1000);

setTimeout(() => {
  console.log('\n3. Test Lambda Calculus - Church numerals');
  sendRequest('tools/call', {
    name: 'lambda-eval',
    arguments: {
      expression: '[[(λ n (λ f (λ x [f [[n f] x]]))) (λ f (λ x x))] succ]'
    }
  });
}, 1500);

setTimeout(() => {
  console.log('\n4. Test Type Inference - Identity function');
  sendRequest('tools/call', {
    name: 'type-check',
    arguments: {
      expression: '(λ x x)'
    }
  });
}, 2000);

setTimeout(() => {
  console.log('\n5. Test Type Inference - Function composition');
  sendRequest('tools/call', {
    name: 'type-check',
    arguments: {
      expression: '(λ f (λ g (λ x [f [g x]])))'
    }
  });
}, 2500);

setTimeout(() => {
  console.log('\n6. Test Proof System - Modus Ponens');
  sendRequest('tools/call', {
    name: 'prove',
    arguments: {
      premises: ['A', '[:implies A B]'],
      goal: 'B',
      method: 'natural-deduction'
    }
  });
}, 3000);

setTimeout(() => {
  console.log('\n7. Test Proof System - Conjunction');
  sendRequest('tools/call', {
    name: 'prove',
    arguments: {
      premises: ['P', 'Q'],
      goal: '[:and P Q]',
      method: 'natural-deduction'
    }
  });
}, 3500);

setTimeout(() => {
  console.log('\n8. Create a Lambda-based tool');
  sendRequest('tools/call', {
    name: 'meta-evolve',
    arguments: {
      type: 'add-tool',
      name: 'church-add',
      description: 'Add two Church numerals',
      code: '(λ m (λ n (λ f (λ x [[m f] [[n f] x]]))))',
      tool_type: 'lambda'
    }
  });
}, 4000);

setTimeout(() => {
  console.log('\n9. Execute the Lambda-based tool');
  sendRequest('tools/call', {
    name: 'execute-tool',
    arguments: {
      'tool-name': 'church-add',
      arguments: {
        expression: '[[(λ m (λ n (λ f (λ x [[m f] [[n f] x]])))) two] three]'
      }
    }
  });
}, 4500);

setTimeout(() => {
  console.log('\nTest complete. Shutting down...');
  server.stdin.end();
}, 5000);