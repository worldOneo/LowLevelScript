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
    }
  | {
      type: "STRING";
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
  | "OR"
  | "GT"
  | "GTE"
  | "LT"
  | "LTE"
  | "EQ"
  | "NEQ"
  | "COR"
  | "CAND";

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
      function: Function;
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
    }
  | {
      type: "STRING";
      value: string;
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

const binaryOperatorMap: Record<string, BinaryOperation> = {
  "|": "OR",
  "^": "XOR",
  "&": "AND",
  "+": "ADD",
  "-": "SUB",
  "*": "MUL",
  "/": "DIV",
  ">": "GT",
  "<": "LT",
  ">=": "GTE",
  "<=": "LTE",
  "==": "EQ",
  "!=": "NEQ",
  "||": "COR",
  "&&": "CAND",
};

const operatorOrder = Object.values(binaryOperatorMap);

const isOperator = (c: string) => {
  return [
    ":",
    "{",
    "}",
    "[",
    "]",
    "=",
    ",",
    "(",
    ")",
    ...Object.keys(binaryOperatorMap),
  ].includes(c);
};

const isSpecialChar = (c: string) => ['"', "`"].includes(c);

const readTokens = (source: string): Token[] => {
  const tokens = new Array<Token>();
  let buff = "";
  const code = source.split("");
  while (code.length) {
    let c = code.shift();
    if (!c) break;

    if (isSpace(c) || isOperator(c) || isSpecialChar(c) || code.length == 0) {
      if (code.length == 0 && !isSpace(c) && !isOperator(c)) buff += c;
      if (isOperator(buff)) {
        if (isOperator(buff + code[0])) {
          buff += code.shift();
        }
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
        if (isOperator(c + code[0])) {
          c += code.shift();
        }
        tokens.push({
          type: "OPERATOR",
          data: c,
        });
      } else if (isSpecialChar(c)) {
        if (c === "`") {
          let asm = "";
          c = code.shift();
          while (c !== "`") {
            asm += c;
            c = code.shift();
          }
          tokens.push({
            type: "ASM",
            data: asm,
          });
        } else if (c === '"') {
          let str = "";
          c = code.shift();
          while (c !== '"') {
            str += c;
            c = code.shift();
          }
          tokens.push({
            type: "STRING",
            data: str,
          });
        }
      }

      buff = "";
    } else {
      buff += c;
    }
  }
  return tokens;
};

const pullParameterNames = (programm: Token[]): [string, string][] => {
  const a: [string, string][] = [];
  let first = programm[0];
  while ((first = programm[0]) && first && first.type === "IDENTIFIER") {
    programm.shift();
    const type = reqType(programm);
    a.push([first.data, type]);
    first = programm[0];
    if (first.type !== "OPERATOR" || first.data !== ",") break;
    programm.shift();
  }
  return a;
};

const pullParameters = (programm: Token[]): AST[] => {
  const a: AST[] = [];
  let first = programm[0];
  while ((first = programm[0]) && first) {
    a.push(pullValue(programm));
    first = programm[0];
    if (!first || first.type !== "OPERATOR" || first.data !== ",") break;
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
    const op = binaryOperatorMap[next.data as keyof typeof binaryOperatorMap];
    if (
      second.type === "BINARY" &&
      operatorOrder.indexOf(second.operation) > operatorOrder.indexOf(op)
    ) {
      let expr = {
        type: "BINARY",
        operation: op,
        first: node,
        second: second.first,
      } as AST;
      second.first = expr;
      return second;
    }
    return {
      type: "BINARY",
      operation: op,
      first: node,
      second,
    };
  }
  if (node.type === "IDENTIFIER") {
    if (next.data === "(") {
      programm.shift();
      let arg0 = programm[0];
      let parameters: AST[] = [];
      if (!(arg0 && arg0.type === "OPERATOR" && arg0.data === ")")) {
        parameters = pullParameters(programm);
      }
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

const reqType = (program: Token[]): string => {
  reqOp(program, ":");
  const next = program.shift();
  if (!next || next.type !== "IDENTIFIER") throw "Identifier expected";
  return next.data;
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
        const type = reqType(program);
        const body = parseTokens(program);
        ast.push({
          type: "FUNCTION",
          function: {
            params: args,
            body,
            return: type,
          },
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
      case "STRING":
        return {
          type: "STRING",
          value: first.data,
        };
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
      _ast.function.body = walkAst(_ast.function.body, func);
      break;
    case "RETURN":
      _ast.value = walkAst(_ast.value, func);
      break;
    case "IF":
    case "WHILE":
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
        ast.function.body = flattenOperations(ast.function.body);
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

type DataType = {
  name: string;
  size: number;
};

type Function = {
  params: [string, string][];
  body: AST;
  return: string;
};

type Stack = {
  stack: Record<
    string,
    {
      ptr: number;
      type: DataType;
    }
  >;
  ptr: number;
  return: string;
};

const asm = (s: string) => s.replace(/ {2,}/gi, "  ");

const comparisonMap: Partial<Record<BinaryOperation, string>> = {
  EQ: "cmove",
  GT: "cmovg",
  GTE: "cmovge",
  LT: "cmovl",
  LTE: "cmovle",
  NEQ: "cmovne",
};

const compile = (ast: AST): string => {
  let functions: Record<
    string,
    {
      name: string;
      function: Function;
    }
  > = {};
  let strings: Record<string, string> = {};
  let types: Record<string, DataType> = {
    Int: {
      name: "Int",
      size: 8,
    },
    Ptr: {
      name: "Ptr",
      size: 8,
    },
  };
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
    return `  mov ${reg}, [rbp - ${(stack.stack[identifier].ptr + 1) * 8}]\n`;
  };

  const movValReg = (val: any, reg: string): String => {
    return `  mov ${reg}, ${val}\n`;
  };

  const movRegVar = (identifier: string, reg: string, stack: Stack): String => {
    return `  mov [rbp - ${(stack.stack[identifier].ptr + 1) * 8}], ${reg}\n`;
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
    switch (exp) {
      case "XOR":
      case "AND":
      case "OR":
      case "SUB":
      case "ADD":
      case "SUB":
        program.code += asm(
          `  ${exp.toLocaleLowerCase()} r15, r14
         mov rax, r15\n`
        );
        break;
      case "MUL":
      case "DIV":
        program.code += asm(
          `  i${exp.toLocaleLowerCase()} r15, r14
         mov rax, r15\n`
        );
        break;
      case "EQ":
      case "LT":
      case "LTE":
      case "GT":
      case "GTE":
      case "NEQ":
        program.code += asm(
          `  mov rax, 0
            mov QWORD r13, 1
            cmp r15, r14
            ${comparisonMap[exp]} rax, r13\n`
        );
        break;
      case "CAND":
        program.code += `  and r15, r14\n`;
        program.code += asm(
          `   mov rax, 0
            mov QWORD r13, 1
            cmp r15, 0
            cmovne rax, r13\n`
        );
        break;
      case "COR":
        program.code += `  or r15, r14\n`;
        program.code += asm(
          `   mov rax, 0
            mov QWORD r13, 1
            cmp r15, 0
            cmovne rax, r13\n`
        );
        break;
      default:
        throw "Uncoverd case";
    }
    return program;
  };

  const append = (p1: Program, p2: Program) => {
    p1.code += p2.code;
    p1.functions.push(...p2.functions);
  };

  const getType = (type: string): DataType => {
    return (
      types[type] ||
      (() => {
        throw `Type ${type} doesnt exist`;
      })()
    );
  };

  const compileFunction = (ast: AST): Program => {
    if (ast.type !== "FUNCTION") throw "compileFunction requires function";
    addrCounter++;
    let label = `r_f${addrCounter}`; // return function n
    let name = `u_f${addrCounter}`; // user function n
    functions[ast.identifier] = {
      name,
      function: ast.function,
    };
    const program = newProgram();
    let stack: Stack = {
      ptr: 0,
      stack: {},
      return: label,
    };
    ast.function.params.forEach(([arg, type]) => {
      stack.stack[arg] = {
        ptr: stack.ptr,
        type: getType(type),
      };
      stack.ptr++;
    });
    const body = _compile(ast.function.body, stack);
    let code = "";
    code += "  push rbp\n";
    code += "  mov rbp, rsp\n";
    code += `  sub rsp, ${Object.values(stack).length * 8}\n`;
    ast.function.params.forEach(([arg], i) => {
      code += `  mov rax, [rbp + ${
        8 * (ast.function.params.length + 1) - 8 * i
      }]\n`;
      code += movRegVar(arg, "rax", stack);
    });
    code += body.code;
    code += `${label}:\n`;
    code += `  add rsp, ${Object.values(stack).length * 8}\n`;
    code += "  pop rbp\n";
    code += "  ret\n";
    program.functions.push([name, code]);
    program.functions.push(...body.functions);
    return program;
  };

  const callFunction = (ast: AST, stack: Stack): Program => {
    if (ast.type !== "CALL") throw "callFunction requires call";
    const program = newProgram();
    const callParamCount = ast.parameters.length;
    if (ast.identifier in types) {
      if (callParamCount !== 1) throw `Cast expects exactly one parameter`;
      return pushReg(ast.parameters[0], "rax", stack);
    }
    const functionParamCount = functions[ast.identifier].function.params.length;
    if (functionParamCount < callParamCount)
      throw `Parameter count missmatch, expected ${functionParamCount} got ${callParamCount}`;
    for (let i = 0; i < callParamCount; i++) {
      let callParam = resolveType(ast.parameters[i], stack).name;
      let funcParam = functions[ast.identifier].function.params[i][1];
      if (callParam !== funcParam) {
        throw `Type mismatch parameter ${i} expected type ${funcParam} got ${callParam}`;
      }
    }
    ast.parameters.forEach(p => {
      append(program, pushReg(p, "rax", stack));
      program.code += "  push rax\n";
    });
    program.code += `  call ${functions[ast.identifier].name}\n`;
    program.code += `  add rsp, ${ast.parameters.length * 8}\n`;
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

  const resolveType = (ast: AST, stack: Stack): DataType => {
    switch (ast.type) {
      case "IDENTIFIER":
        return stack.stack[ast.data].type;
      case "BINARY":
        return resolveType(ast.first, stack);
      case "INT":
        return types["Int"];
      case "CALL":
        if (ast.identifier in types) {
          return types[ast.identifier];
        }
        return getType(functions[ast.identifier].function.return);
      case "STRING":
        return types["Ptr"];
    }
    throw "Couldnt evaluate type";
  };

  const _compile = (ast: AST, stack: Stack): Program => {
    const program = newProgram();
    switch (ast.type) {
      case "BLOCK":
        ast.data.forEach(ast => append(program, _compile(ast, stack)));
        break;
      case "DEFINE":
        stack.stack[ast.identifier] = {
          ptr: stack.ptr,
          type: resolveType(ast.value, stack),
        };
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
        program.code += `  jmp ${stack.return}\n`;
        break;
      case "WHILE":
        append(program, compileWhile(ast, stack));
        break;
      case "IF":
        append(program, compileIf(ast, stack));
        break;
      case "STRING":
        addrCounter++;
        let name = `s${addrCounter}`; // string n
        strings[name] = ast.value;
        program.code += `  mov rax, ${name}\n`;
        break;
    }
    return program;
  };
  const stack: Record<
    string,
    {
      ptr: number;
      type: DataType;
    }
  > = {};
  const compiled = _compile(ast, {
    stack,
    ptr: 0,
    return: "o_e",
  });
  compiled.functions.forEach(f => (out += `${f[0]}:\n${f[1]}\n`));
  out += "_start:\n";
  out += "  push rbp\n";
  out += "  mov rbp, rsp\n";
  out += `  sub rsp, ${Object.values(stack).length * 8}\n` + compiled.code;
  out += "  pop rbp\n";
  out += asm(`o_e:
    mov rax, 60
    mov rdi, 0
    syscall\n`);
  Object.entries(strings).forEach(
    ([name, value]) =>
      (out += `${name}: db ${value
        .split("")
        .map(c => c.charCodeAt(0))
        .join(",")}\n`)
  );

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
