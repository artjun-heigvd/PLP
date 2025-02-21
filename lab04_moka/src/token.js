class Token {
    constructor(type, value) {
        this.type = type;
        this.value = value;
    }
}

class KeywordToken extends Token {
    constructor(keyword) {
        super('KEYWORD', keyword);
    }
}

class EqualToken extends Token {
    constructor() {
        super('EQUAL', "=");
    }
}

class DelimiterToken extends Token {
    constructor(delimiter) {
        super('DELIMITER', delimiter);
    }
}

class IdentifierToken extends Token {
    constructor(name) {
        super('IDENTIFIER', name);
    }
}

class LiteralToken extends Token {
    constructor(type, value) {
        super(type, value);
    }
}

class BooleanLiteralToken extends LiteralToken {
    constructor(value) {
        super('BOOLEAN_LITERAL', value);
    }
}

class IntegerLiteralToken extends LiteralToken {
    constructor(value) {
        super('INTEGER_LITERAL', value);
    }
}

// Predefined sets for quick lookups
const KEYWORDS = new Set([
    'class', 'extends', 'int', 'boolean', 'void',
    'if', 'while', 'return', 'break', 'new',
    'true', 'false'
]);

const EQUAL = '=';

const DELIMITERS = new Set([
    '{', '}', '(', ')', ';', '.'
]);

module.exports = {
    Token,
    KeywordToken,
    EqualToken,
    DelimiterToken,
    IdentifierToken,
    IntegerLiteralToken,
    BooleanLiteralToken,
    LiteralToken,
    KEYWORDS,
    EQUAL,
    DELIMITERS
};