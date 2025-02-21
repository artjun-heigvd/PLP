/**
 * The main entry point for the Moka parser.
 */

const { program } = require('commander');
const fs = require('node:fs');
const parse = require('./parser');
const tokenize = require('./tokenizer');
const Reporter = require('./reporter');

// Define the command line interface
program
  .name('mokac')
  .description('A parser for the Moka language')
  .argument('<filename>', 'The source file to parse')
  .option('--show-tokens', 'Show the tokens', false)
  .option('--show-ast', 'Show the parsed AST', false)
  .action((filename) => {
    if (!filename.endsWith('.moka')) {
      console.error(`Unrecognized file extension: ${filename}`);
      process.exit(1);
    }
  });

// Parse the command line arguments
program.parse(process.argv);

// Get the filename and options
const [filename] = program.args;
const options = program.opts();

// Read the source code from the file
let sourceCode;
try {
  sourceCode = fs.readFileSync(filename, 'utf-8');
} catch (err) {
  console.log(`Failed to read ${filename}: ${err}`);
  process.exit(1);
}

// Create an error reporter
const reporter = new Reporter(filename, sourceCode);

// Tokenize the source code
const tokens = tokenize(sourceCode, reporter);
if (options.showTokens) {
  console.log(JSON.stringify(tokens, null, 2));
}

// Parse the tokens into an abstract syntax tree (AST)
const ast = parse(tokens, reporter);
if (options.showAst) {
  console.log(JSON.stringify(ast, null, 2));
}
