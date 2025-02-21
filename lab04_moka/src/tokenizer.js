const {KEYWORDS, EQUAL, DELIMITERS, LiteralToken,
IntegerLiteralToken,BooleanLiteralToken,DelimiterToken,Token,IdentifierToken,KeywordToken,EqualToken} = require("./token");

/**
 * Tokenize the input string
 * @param {string} input The input string to tokenize
 * @param {Reporter} reporter The error reporter
 * @returns {Array} An array of tokens
 */
function tokenize(input, reporter) {

  let index = 0
  let tokens = []

  while (input.length > 0){

    let firstChar = getNextChar()

    if(firstChar === EQUAL){
      tokens.push(new EqualToken())
      continue
    }

    if(DELIMITERS.has(firstChar)){
      tokens.push(new DelimiterToken(firstChar))
      continue
    }

    //Potentiellement un keyword (on l'espère sinon ouille!)
    if(/[a-zA-Z]/.test(firstChar)){
      //TODO Calculer la taille du mot et indiquer combien char prendre
      let word = getUntil(/[a-zA-Z0-9]/, firstChar)
      //We got our word, now we need to check what it is
      if(KEYWORDS.has(word)){
        if(word === "true" || word === "false"){
          tokens.push(new BooleanLiteralToken(word))
        }
        else {
          tokens.push(new KeywordToken(word))
        }
      } else {
        tokens.push(new IdentifierToken(word))
      }
      continue
    }

    if(/[0-9]/.test(firstChar)){
      let number = getUntil(/[0-9]/,firstChar)
      tokens.push(new IntegerLiteralToken(number))
      continue
    }

    throw new Error("Unsupported character: " + firstChar + " in input")
  }

  return tokens

  function getNextChar(){
    let char = input.charAt(index)
    while(' \n'.includes(char)){
      index = index + 1
      char = input.charAt(index)
    }
    return char
  }

  function getUntil(regex, firstChar){
    let word = firstChar
    let nextChar = input.charAt(index + 1)

    while(regex.test(nextChar)){
      index = index + 1 //Validate the change in the index
      word += nextChar
      nextChar = input.charAt(index + 1)
    }

    return word
  }
}



module.exports = tokenize;
