const { spawn } = require('child_process');

const server = spawn('node', ['out/mcp-server.js'], {
  stdio: ['pipe', 'pipe', 'pipe']
});

console.log('');
console.log('='.repeat(60));
console.log('   MCP-PIF-CLJS ENHANCED FORMAL REASONING TEST SUITE');
console.log('='.repeat(60));
console.log('');

let requestId = 1;
let testResults = [];
let currentTest = null;

// Capture stdout (JSON responses)
server.stdout.on('data', (data) => {
  const response = data.toString().trim();

  try {
    const json = JSON.parse(response);

    if (json.result && currentTest) {
      // Extract the content from the result
      let content = json.result.content;
      if (Array.isArray(content)) {
        content = content[0]?.text || '';
      }

      // Check if test passed based on expected patterns
      const passed = currentTest.validate ?
        currentTest.validate(content) :
        content.includes(currentTest.expected);

      testResults.push({
        name: currentTest.name,
        passed: passed,
        result: content
      });

      console.log(`   ${passed ? '✅' : '❌'} ${currentTest.name}`);
      if (!passed) {
        console.log(`      Expected: ${currentTest.expected}`);
        console.log(`      Got: ${content.substring(0, 100)}...`);
      }

      currentTest = null;
    }
  } catch (e) {
    // Not JSON or parsing error
  }
});

// Capture stderr (debug logs)
server.stderr.on('data', (data) => {
  // Suppress debug logs during test
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

function runTest(name, toolName, args, expected, validate = null) {
  currentTest = { name, expected, validate };
  sendRequest('tools/call', {
    name: toolName,
    arguments: args
  });
}

// Test sequence with delays
const tests = [
  // Initialize
  {
    delay: 500, action: () => {
      console.log('Initializing server...\n');
      sendRequest('initialize', { protocolVersion: "2024-11-05" });
    }
  },

  // Lambda Calculus Tests
  {
    delay: 1000, action: () => {
      console.log('🧮 LAMBDA CALCULUS EVALUATION\n');
      runTest(
        'Identity function: [[λ x x] 42]',
        'lambda-eval',
        { expression: '[[λ x x] 42]' },
        'Result: 42'
      );
    }
  },

  {
    delay: 1200, action: () => {
      runTest(
        'Constant function: [[[λ x [λ y x]] 1] 2]',
        'lambda-eval',
        { expression: '[[[λ x [λ y x]] 1] 2]' },
        'Result: 1'
      );
    }
  },

  {
    delay: 1400, action: () => {
      runTest(
        'SKK = I: [[[[λ f [λ g [λ x [[f x] [g x]]]]] [λ x [λ y x]]] [λ x [λ y x]]] 42]',
        'lambda-eval',
        { expression: '[[[[λ f [λ g [λ x [[f x] [g x]]]]] [λ x [λ y x]]] [λ x [λ y x]]] 42]' },
        'Result: 42'
      );
    }
  },

  {
    delay: 1600, action: () => {
      runTest(
        'Church boolean AND: [[[λ p [λ q [[p q] p]]] [λ x [λ y x]]] [λ x [λ y y]]]',
        'lambda-eval',
        { expression: '[[[λ p [λ q [[p q] p]]] [λ x [λ y x]]] [λ x [λ y y]]]' },
        '[λ x [λ y y]]'
      );
    }
  },

  // Type Inference Tests
  {
    delay: 2000, action: () => {
      console.log('\n🔍 TYPE INFERENCE (Improved)\n');
      runTest(
        'Identity type: [λ x x]',
        'type-check',
        { expression: '[λ x x]' },
        'Type: a → a',
        (result) => result.includes('→') && !result.includes('UUID')
      );
    }
  },

  {
    delay: 2200, action: () => {
      runTest(
        'Constant type: [λ x [λ y x]]',
        'type-check',
        { expression: '[λ x [λ y x]]' },
        'a → b → a',
        (result) => result.includes('→') && result.includes('b')
      );
    }
  },

  {
    delay: 2400, action: () => {
      runTest(
        'Composition type: [λ f [λ g [λ x [g [f x]]]]]',
        'type-check',
        { expression: '[λ f [λ g [λ x [g [f x]]]]]' },
        '(a → b) → (b → c) → a → c',
        (result) => result.includes('→') && result.includes('c')
      );
    }
  },

  {
    delay: 2600, action: () => {
      runTest(
        'Church numeral type: [λ f [λ x [f [f x]]]]',
        'type-check',
        { expression: '[λ f [λ x [f [f x]]]]' },
        '(a → a) → a → a',
        (result) => result.includes('(a → a) → a → a') ||
          result.includes('(a → a) → (a → a)')
      );
    }
  },

  // Proof System Tests
  {
    delay: 3000, action: () => {
      console.log('\n⚖️ PROOF SYSTEM (Enhanced)\n');
      runTest(
        'Identity proof: A ⊢ A',
        'prove',
        { premises: ['A'], goal: 'A', method: 'natural-deduction' },
        'Proof found'
      );
    }
  },

  {
    delay: 3200, action: () => {
      runTest(
        'Modus Ponens: A, A→B ⊢ B',
        'prove',
        { premises: ['A', '[:implies A B]'], goal: 'B', method: 'natural-deduction' },
        'Proof found'
      );
    }
  },

  {
    delay: 3400, action: () => {
      runTest(
        'Hypothetical Syllogism: A→B, B→C ⊢ A→C',
        'prove',
        { premises: ['[:implies A B]', '[:implies B C]'], goal: '[:implies A C]', method: 'natural-deduction' },
        'Proof found'
      );
    }
  },

  {
    delay: 3600, action: () => {
      runTest(
        'Complex proof: P→Q, Q→R, P ⊢ R',
        'prove',
        { premises: ['[:implies P Q]', '[:implies Q R]', 'P'], goal: 'R', method: 'auto' },
        'Proof found'
      );
    }
  },

  {
    delay: 3800, action: () => {
      runTest(
        'Conjunction: A, B ⊢ A∧B',
        'prove',
        { premises: ['A', 'B'], goal: '[:and A B]', method: 'natural-deduction' },
        'Proof found'
      );
    }
  },

  {
    delay: 4000, action: () => {
      runTest(
        'Disjunctive Syllogism: A∨B, ¬A ⊢ B',
        'prove',
        { premises: ['[:or A B]', '[:not A]'], goal: 'B', method: 'auto' },
        'Proof found'
      );
    }
  },

  // Metaprogramming Tests
  {
    delay: 4500, action: () => {
      console.log('\n🔧 METAPROGRAMMING\n');
      runTest(
        'Create Y combinator tool',
        'meta-evolve',
        {
          type: 'add-tool',
          name: 'y-combinator',
          description: 'Y combinator for recursion',
          code: '[λ f [[λ x [f [x x]]] [λ x [f [x x]]]]]',
          'tool-type': 'lambda'
        },
        'Tool \'y-combinator\' added successfully'
      );
    }
  },

  {
    delay: 4700, action: () => {
      runTest(
        'Create Church multiplication',
        'meta-evolve',
        {
          type: 'add-tool',
          name: 'church-mult',
          description: 'Church numeral multiplication',
          code: '[λ m [λ n [λ f [m [n f]]]]]',
          'tool-type': 'lambda'
        },
        'Tool \'church-mult\' added successfully'
      );
    }
  },

  // Memory Tests
  {
    delay: 5000, action: () => {
      console.log('\n💾 MEMORY SYSTEM\n');
      runTest(
        'Store lambda expression',
        'memory-store',
        { key: 'identity', value: '[λ x x]' },
        'Stored: identity'
      );
    }
  },

  {
    delay: 5200, action: () => {
      runTest(
        'Retrieve lambda expression',
        'memory-retrieve',
        { key: 'identity' },
        '[λ x x]'
      );
    }
  },

  {
    delay: 5400, action: () => {
      runTest(
        'Store proof result',
        'memory-store',
        { key: 'modus-ponens-valid', value: 'true' },
        'Stored: modus-ponens-valid'
      );
    }
  },

  // Server Info
  {
    delay: 5600, action: () => {
      console.log('\n📊 SERVER STATUS\n');
      runTest(
        'Server info with runtime tools',
        'server-info',
        {},
        'MCP-PIF-CLJS Server Info',
        (result) => result.includes('y-combinator') && result.includes('[RUNTIME]')
      );
    }
  },

  // Final report
  {
    delay: 6000, action: () => {
      console.log('\n' + '='.repeat(60));
      console.log('   TEST RESULTS SUMMARY');
      console.log('='.repeat(60) + '\n');

      const passed = testResults.filter(t => t.passed).length;
      const failed = testResults.filter(t => !t.passed).length;
      const total = testResults.length;

      console.log(`   Total Tests: ${total}`);
      console.log(`   ✅ Passed: ${passed}`);
      console.log(`   ❌ Failed: ${failed}`);
      console.log(`   Success Rate: ${((passed / total) * 100).toFixed(1)}%`);

      if (failed > 0) {
        console.log('\n   Failed Tests:');
        testResults.filter(t => !t.passed).forEach(t => {
          console.log(`   - ${t.name}`);
        });
      }

      console.log('\n' + '='.repeat(60));

      if (passed === total) {
        console.log('   🎉 ALL TESTS PASSED! 🎉');
      } else {
        console.log('   ⚠️  Some tests failed. Check the improvements.');
      }

      console.log('='.repeat(60) + '\n');

      server.stdin.end();

      // Exit with appropriate code
      setTimeout(() => {
        process.exit(failed > 0 ? 1 : 0);
      }, 500);
    }
  }
];

// Execute tests with delays
tests.forEach(test => {
  setTimeout(test.action, test.delay);
});
