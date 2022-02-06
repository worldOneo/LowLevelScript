import { readFileSync } from "fs";
import { inspect } from "util";

type Token =
  | {
      type: "INT";
      data: number;
    }
  | {
      type: "OPERATOR";
      data: string;
    }
  | {
      type: "CONST" | "LET" | "FUNCTION" | "RETURN" | "IF" | "WHILE";
    }
  | {
      type: "IDENTIFIER";
      data: string;
    }
  | {
      type: "ASM";
      data: string;
    };

type BinaryOperation =
  | "ADD"
  | "SUB"
  | "MUL"
  | "MOD"
  | "DIV"
  | "XOR"
  | "AND"
  | "OR";

type AST =
  | {
      type: "DEFINE";
      modus: "CONST" | "LET";
      identifier: string;
      value: AST;
    }
  | {
      type: "INT";
      data: number;
    }
  | {
      type: "BLOCK";
      data: AST[];
    }
  | {
      type: "ASM";
      data: string;
    }
  | {
      type: "BINARY";
      operation: BinaryOperation;
      first: AST;
      second: AST;
    }
  | {
      type: "IDENTIFIER";
      data: string;
    }
  | {
      type: "ASSIGN";
      identifier: string;
      value: AST;
    }
  | {
      type: "FUNCTION";
      args: string[];
      body: AST;
      identifier: string;
    }
  | {
      type: "RETURN";
      value: AST;
    }
  | {
      type: "CALL";
      identifier: string;
      parameters: AST[];
    }
  | {
      type: "IF" | "WHILE";
      condition: AST;
      body: AST;
    };

const readFile = readFileSync(process.argv[2]).toString();

const isSpace = (c: string) => {
  return c === "\n" || c === " " || c === "\r" || c === "\t";
};

const isInt = (c: string) =>
  c.length > 0 &&
  c
    .split("")
    .map(c => c.charCodeAt(0))
    .every(c => c >= "0".charCodeAt(0) && c <= "9".charCodeAt(0));

const isOperator = (c: string) => {
  return [
    "{",
    "}",
    "[",
    "]",
    "=",
    "+",
    "-",
    "*",
    "%",
    "/",
    ",",
    "(",
    ")",
  ].includes(c);
};

const binaryOperatorMap: Record<string, BinaryOperation> = {
  "+": "ADD",
  "-": "SUB",
  "*": "MUL",
  "/": "DIV",
  "|": "OR",
  "^": "XOR",
  "&": "AND",
};

type State = "READ" | "ASM";

const readTokens = (source: string): Token[] => {
  const tokens = new Array<Token>();
  let buff = "";
  const code = source.split("");
  let state: State = "READ";
  while (code.length) {
    let c = code.shift();
    if (!c) break;
    if (state === "ASM") {
      if (c === "`") {
        tokens.push({
          type: "ASM",
          data: buff,
        });
        buff = "";
        state = "READ";
        continue;
      }
      buff += c;
      continue;
    }
    if (c === "`" && state === "READ") {
      state = "ASM";
      buff = "";
      continue;
    }

    if (isSpace(c) || isOperator(c) || code.length == 0) {
      if (code.length == 0 && !isSpace(c) && !isOperator(c)) buff += c;
      if (isOperator(buff)) {
        tokens.push({
          type: "OPERATOR",
          data: buff,
        });
      } else if (
        ["const", "let", "function", "return", "if", "while"].includes(buff)
      ) {
        tokens.push({
          type: buff.toUpperCase() as "CONST",
        });
      } else if (isInt(buff)) {
        tokens.push({
          type: "INT",
          data: parseInt(buff),
        });
      } else if (buff.length != 0) {
        tokens.push({
          type: "IDENTIFIER",
          data: buff,
        });
      }

      if (isOperator(c)) {
        tokens.push({
          type: "OPERATOR",
          data: c,
        });
      }

      buff = "";
    } else {
      buff += c;
    }
  }
  return tokens;
};

const pullParameterNames = (programm: Token[]): string[] => {
  const a: string[] = [];
  let first = programm[0];
  while ((first = programm[0]) && first.type === "IDENTIFIER") {
    a.push(first.data);
    programm.shift();
    first = programm[0];
    if (first.type !== "OPERATOR" || first.data !== ",") break;
    programm.shift();
  }
  return a;
};

const pullParameters = (programm: Token[]): AST[] => {
  const a: AST[] = [];
  let first = programm[0];
  while ((first = programm[0])) {
    a.push(pullValue(programm));
    first = programm[0];
    if (first.type !== "OPERATOR" || first.data !== ",") break;
    programm.shift();
  }
  return a;
};

const checkApendage = (programm: Token[], node: AST): AST => {
  const next = programm[0];
  if (!next) return node;
  if (next.type !== "OPERATOR") return node;
  if (next.data in binaryOperatorMap) {
    programm.shift();
    const second = pullValue(programm);
    return {
      type: "BINARY",
      operation: binaryOperatorMap[next.data as keyof typeof binaryOperatorMap],
      first: node,
      second,
    };
  }
  if (node.type === "IDENTIFIER") {
    if (next.data === "(") {
      programm.shift();
      const parameters = pullParameters(programm);
      closedBracket(programm);
      return {
        type: "CALL",
        identifier: node.data,
        parameters,
      };
    }
    if (next.data === "=") {
      programm.shift();
      const value = pullValue(programm);
      return {
        type: "ASSIGN",
        identifier: node.data,
        value,
      };
    }
  }
  return node;
};

const pullValue = (programm: Token[]): AST => {
  const next = programm.shift();
  if (!next) throw "Value expected";
  return checkApendage(
    programm,
    next.type === "IDENTIFIER" ? next : parseTokens([next])
  );
};

const reqOp = (program: Token[], data: string) => {
  const openBracket = program.shift();
  if (
    !openBracket ||
    openBracket.type !== "OPERATOR" ||
    openBracket.data !== data
  )
    throw `${data} expected`;
};

const openBracket = (program: Token[]) => reqOp(program, "(");
const closedBracket = (program: Token[]) => reqOp(program, ")");

const parseTokens = (program: Token[]): AST => {
  const pre = program[0];
  let returnOnClosed = pre && pre.type === "OPERATOR" && pre.data === "{";
  if (returnOnClosed) program.shift();
  const ast = new Array<AST>();
  while (program.length) {
    const first = program[0];
    if (returnOnClosed && first.type === "OPERATOR" && first.data === "}") {
      program.shift();
      return {
        type: "BLOCK",
        data: ast,
      };
    }
    switch (first.type) {
      case "CONST":
      case "LET":
        program.shift();
        const identifier = program.shift();
        if (!identifier || identifier.type !== "IDENTIFIER")
          throw "Identifier expected";
        const equals = program.shift();
        if (!equals || equals.type !== "OPERATOR" || equals.data !== "=")
          throw "= expected";
        ast.push({
          type: "DEFINE",
          identifier: identifier.data,
          modus: first.type,
          value: pullValue(program),
        });
        break;
      case "ASM":
        program.shift();
        ast.push(first);
        break;
      case "INT":
        program.shift();
        return checkApendage(program, first);
      case "IDENTIFIER":
        program.shift();
        ast.push(checkApendage(program, first));
        break;
      case "FUNCTION": {
        program.shift();
        const name = program.shift();
        if (!name || name.type !== "IDENTIFIER") throw "Identifier expected";
        openBracket(program);
        const args = pullParameterNames(program);
        closedBracket(program);
        const body = parseTokens(program);
        ast.push({
          type: "FUNCTION",
          args,
          body,
          identifier: name.data,
        });
        break;
      }
      case "WHILE":
      case "IF": {
        program.shift();
        openBracket(program);
        const condition = pullValue(program);
        closedBracket(program);
        const body = parseTokens(program);
        ast.push({
          type: first.type,
          condition,
          body,
        });
        break;
      }
      case "RETURN":
        program.shift();
        ast.push({
          type: "RETURN",
          value: pullValue(program),
        });
        break;
      case "OPERATOR":
        if (returnOnClosed && first.data === "}") {
          program.shift();
          return {
            type: "BLOCK",
            data: ast,
          };
        }
      default:
        console.log(program);
        throw "Invalid syntax";
    }
  }
  return {
    type: "BLOCK",
    data: ast,
  };
};

const walkAst = (ast: AST, func: (arg0: AST) => AST): AST => {
  let _ast = func(ast);
  switch (_ast.type) {
    case "BLOCK":
      _ast.data = _ast.data.map($ => walkAst($, func));
      break;
    case "BINARY":
      _ast.first = walkAst(_ast.first, func);
      _ast.second = walkAst(_ast.second, func);
      break;
    case "DEFINE":
      _ast.value = walkAst(_ast.value, func);
      break;
    case "FUNCTION":
      _ast.body = walkAst(_ast.body, func);
      break;
    case "RETURN":
      _ast.value = walkAst(_ast.value, func);
      break;
    case "IF":
      _ast.condition = walkAst(_ast.condition, func);
      _ast.body = walkAst(_ast.body, func);
      break;
  }
  return _ast;
};

const flattenOperations = (ast: AST): AST => {
  let counter = 0;
  const vars: string[] = [];
  const _flattenOperations = (ast: AST): AST =>
    walkAst(ast, ast => {
      if (ast.type === "FUNCTION") {
        ast.body = flattenOperations(ast.body);
        return ast;
      }
      if (ast.type === "BINARY") {
        let ft = ast.first.type;
        let st = ast.second.type;
        if (
          (ft === "IDENTIFIER" || ft === "INT") &&
          (st === "IDENTIFIER" || st === "INT")
        ) {
          return ast;
        }
        let local = " " + counter;
        counter++;
        let block: AST = {
          type: "BLOCK",
          data: [
            {
              type: "ASSIGN",
              identifier: local,
              value: ast.first,
            },
            {
              type: "BINARY",
              first: {
                type: "IDENTIFIER",
                data: local,
              },
              operation: ast.operation,
              second: ast.second,
            },
          ],
        };
        if (st !== "IDENTIFIER" && st !== "INT")
          block = {
            type: "BLOCK",
            data: [
              {
                type: "ASSIGN",
                identifier: local,
                value: ast.second,
              },
              {
                type: "BINARY",
                first: ast.first,
                operation: ast.operation,
                second: {
                  type: "IDENTIFIER",
                  data: local,
                },
              },
            ],
          };
        vars.push(local);
        return block;
      }
      return ast;
    });
  let flat = _flattenOperations(ast);
  let add = vars.map(
    c =>
      ({
        type: "DEFINE",
        identifier: c,
        modus: "LET",
        value: {
          type: "INT",
          data: 0,
        },
      } as AST)
  );
  if (flat.type !== "BLOCK") {
    return {
      type: "BLOCK",
      data: [...add, flat],
    };
  }

  return {
    type: "BLOCK",
    data: [...add, ...flat.data],
  };
};

type Program = {
  functions: [string, string][]; // [function[name, code]]
  code: string;
};

type Stack = {
  stack: Record<string, number>;
  ptr: number;
};

const asm = (s: string) => s.replace(/ {2,}/gi, "  ");

const compile = (ast: AST): string => {
  let functions: Record<string, string> = {};
  let addrCounter = 0;
  let out = "format ELF64 executable 3\nentry _start\n";

  const newProgram = (): Program => ({
    code: "",
    functions: [],
  });

  const pushReg = (ast: AST, reg: string, stack: Stack): Program => {
    const program = newProgram();
    if (ast.type === "IDENTIFIER" || ast.type === "INT") {
      program.code +=
        ast.type === "IDENTIFIER"
          ? movVarReg(ast.data, reg, stack)
          : movValReg(ast.data, reg);
      return program;
    }
    append(program, _compile(ast, stack));
    program.code += asm(`  mov ${reg}, rax\n`);
    return program;
  };

  const movVarReg = (identifier: string, reg: string, stack: Stack): String => {
    return `  mov ${reg}, [rbp - ${(stack.stack[identifier] + 1) * 8}]\n`;
  };

  const movValReg = (val: any, reg: string): String => {
    return `  mov ${reg}, ${val}\n`;
  };

  const movRegVar = (identifier: string, reg: string, stack: Stack): String => {
    return `  mov [rbp - ${(stack.stack[identifier] + 1) * 8}], ${reg}\n`;
  };

  const binaryExpression = (
    exp: BinaryOperation,
    l: AST,
    r: AST,
    stack: Stack
  ): Program => {
    const program = newProgram();
    append(program, pushReg(l, "r15", stack));
    append(program, pushReg(r, "r14", stack));
    program.code += asm(
      `  ${exp.toLocaleLowerCase()} r15, r14
       mov rax, r15\n`
    );
    return program;
  };

  const append = (p1: Program, p2: Program) => {
    p1.code += p2.code;
    p1.functions.push(...p2.functions);
  };

  const compileFunction = (ast: AST): Program => {
    if (ast.type !== "FUNCTION") throw "compileFunction requires function";
    addrCounter++;
    let name = `u_f${addrCounter}`; // user function n
    functions[ast.identifier] = name;
    const program = newProgram();
    let stack: Stack = {
      ptr: 0,
      stack: {},
    };
    ast.args.forEach(arg => {
      stack.stack[arg] = stack.ptr;
      stack.ptr++;
    });
    const body = _compile(ast.body, stack);
    let code = "";
    code += "  push rbp\n";
    code += "  mov rbp, rsp\n";
    code += `  sub rsp, ${Object.values(stack).length * 8}\n`;
    ast.args.forEach((arg, i) => {
      code += `  mov rax, [rbp + ${8 * (ast.args.length + 1) - 8 * i}]\n`;
      code += movRegVar(arg, "rax", stack);
    });
    code += body.code;
    code += `  add rsp, ${Object.values(stack).length * 8}\n`;
    code += "  pop rbp\n";
    code += "  ret";
    program.functions.push([name, code]);
    program.functions.push(...body.functions);
    return program;
  };

  const callFunction = (ast: AST, stack: Stack): Program => {
    if (ast.type !== "CALL") throw "callFunction requires call";
    const program = newProgram();
    ast.parameters.forEach(p => {
      append(program, pushReg(p, "rax", stack));
      program.code += "  push rax\n";
    });
    program.code += `  call ${functions[ast.identifier]}\n`;
    return program;
  };

  const compileIf = (ast: AST, stack: Stack): Program => {
    if (ast.type !== "IF") throw "compileIf requires if";
    const program = newProgram();
    addrCounter++;
    let addrName = `if${addrCounter}`;
    append(program, pushReg(ast.condition, "rax", stack));
    program.code += "  cmp rax, 0\n";
    program.code += `  je ${addrName}\n`;
    append(program, _compile(ast.body, stack));
    program.code += `${addrName}:\n`;
    return program;
  };

  const compileWhile = (ast: AST, stack: Stack): Program => {
    if (ast.type !== "WHILE") throw "compileWhile requires while";
    const program = newProgram();
    addrCounter++;
    let entry = `w_e${addrCounter}`; // while entry n
    let exit = `w_x${addrCounter}`; // while exit n

    program.code += `${entry}:\n`;
    append(program, pushReg(ast.condition, "rax", stack));
    program.code += "  cmp rax, 0\n";
    program.code += `  je ${exit}\n`;
    append(program, _compile(ast.body, stack));
    program.code += `  jmp ${entry}\n`;
    program.code += `${exit}:\n`;
    return program;
  };

  const _compile = (ast: AST, stack: Stack): Program => {
    const program = newProgram();
    switch (ast.type) {
      case "BLOCK":
        ast.data.forEach(ast => append(program, _compile(ast, stack)));
        break;
      case "DEFINE":
        stack.stack[ast.identifier] = stack.ptr;
        stack.ptr += 1;
      case "ASSIGN":
        append(program, pushReg(ast.value, "rax", stack));
        program.code += movRegVar(ast.identifier, "rax", stack);
        break;
      case "INT":
        program.code += `  mov rax, ${ast.data}\n`;
        break;
      case "BINARY":
        append(
          program,
          binaryExpression(ast.operation, ast.first, ast.second, stack)
        );
        break;
      case "ASM":
        program.code += ast.data;
        break;
      case "FUNCTION":
        program.functions.push(...compileFunction(ast).functions);
        break;
      case "CALL":
        append(program, callFunction(ast, stack));
        break;
      case "RETURN":
        append(program, pushReg(ast.value, "rax", stack));
        break;
      case "WHILE":
        append(program, compileWhile(ast, stack));
        break;
      case "IF":
        append(program, compileIf(ast, stack));
        break;
    }
    return program;
  };
  const stack: Record<string, number> = {};
  const compiled = _compile(ast, {
    stack,
    ptr: 0,
  });
  out += compiled.functions.map(f => `${f[0]}:\n${f[1]}\n`);
  out += "_start:\n";
  out += "  push rbp\n";
  out += "  mov rbp, rsp\n";
  out += `  sub rsp, ${Object.values(stack).length * 8}\n` + compiled.code;
  out += "  pop rbp\n";
  out += "  mov rax, 60\n  mov rdi, 0\n  syscall";
  return out;
};

/*console.log(
  inspect(flattenOperations(parseTokens(readTokens(readFile))), {
    depth: null,
  })
);*/

console.log(
  /*inspect(*/ compile(
    flattenOperations(parseTokens(readTokens(readFile))) /* , { depth: null })*/
  )
);