const { red } = require('chalk');

/**
 * A class for reporting errors in the code.
 */
class Reporter {

  /**
   * Create a new error reporter.
   * @param {string} filename The name of the file.
   * @param {string} content The content of the file.
   */
  constructor(filename, content) {
    this.filename = filename;
    this.content = content;
  }

  /**
   * Report an error in the code and terminate the program.
   * @param {string} message The error message.
   * @param {number} offset The offset in the code where the error occurred.
   */
  error(message, offset) {
    const { line, column } = Reporter.#getLineAndColumn(this.content, offset);
    console.log(red(`${this.filename}:${line}:${column}: ${message}`));
    console.log(red(this.content.split('\n')[line - 1]));
    console.log(red(' '.repeat(column - 1) + '^'));
    process.exit(1);
  }

  /**
   * Get the line and column number for a given offset in the code.
   * @param {number} offset The offset in the code.
   * @returns {object} An object with the line and column numbers.
   */
  static #getLineAndColumn(content, offset) {
    let line = 1;
    let column = 1;
    for (let i = 0; i < offset; i++) {
      if (content[i] === '\n') {
        line++;
        column = 1;
      } else {
        column++;
      }
    }
    return { line, column };
  }
}

module.exports = Reporter;
