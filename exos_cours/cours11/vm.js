const fs = require('fs')

const filename = process.argv[2]
if (!filename) {
    console.log('Usage: vm <file>')
    process.exit()
}

let contents
try {
    contents = fs.readFileSync(filename, 'utf8')
} catch (err) {
    console.log(`Failed to read ${filename}. Reason: ${err}`)
    process.exit()
}

const stack = []
const memory = {}

let pc = 0
const instructions = contents.split('\n').filter(instr => instr.trim() !== '')

while (pc < instructions.length) {
    const instr = instructions[pc]
    const [op, arg] = instr.split(' ') // arg peut être undefined si une simple instruction
    switch (op) {
        case 'Push': 
            stack.push(parseInt(arg))
            break
        case 'Display' :
            console.log(stack.pop())
            break
        case 'Store' : 
            memory[arg] = stack.pop()
            break
        case 'Load' : 
            stack.push(memory[arg])
            break
        case 'Plus' : 
            stack.push(stack.pop() + stack.pop())
            break
        case 'Minus' :
            stack.push(-stack.pop() + stack.pop())
            break
        case 'JMPC' :
            if (stack.pop() === 0){
                pc = pc + parseInt(arg)
                continue
            }
            break
        case 'JMPI' :
            pc = pc + parseInt(arg)
            continue
        default:
            console.log(`Unsupported operation ${op}`)
            process.exit()
    }
    pc = pc + 1
}
    