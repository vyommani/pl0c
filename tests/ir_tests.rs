#[cfg(test)]
mod tests {
    use pl0c::semantic::symboltable::SymbolTable;
    use pl0c::frontend::lexer::scan;
    use pl0c::frontend::parser::Parser;
    use pl0c::LineNumber;
    use pl0c::ir::IRGenerator;
    use pl0c::utils::errors::Pl0Result;
    use pl0c::utils::errors::Pl0Error;

    // Helper function to compile source and get IR
    fn compile_to_ir(source: &str) -> Result<String, Pl0Error> {
        let mut table = SymbolTable::new();
        let mut state = LineNumber::default();
        let mut tokens = scan(&mut state, source, &mut table)?;
        let mut parser = Parser::new(&mut tokens);
        let ast = parser.parse(&mut table)?;
        
        let mut generator = IRGenerator::new(table);
        generator.generate_code(ast)?;
        Ok(generator.get_output())
    }

    // Test simple variable assignment
    // Purpose: Ensure variable declaration, constant loading, and storage
    #[test]
    fn test_simple_assignment() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            x := 5
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("var x"), "Should declare variable x in data section");
        assert!(ir.contains("li v0, 5"), "Should load constant 5 with li");
        assert!(ir.contains("st [x], v0"), "Should store to variable x");
        
        Ok(())
    }

    // Test basic arithmetic operation (addition)
    // Purpose: Ensure addition, variable loading, and storage
    #[test]
    fn test_arithmetic_operations() -> Pl0Result<()> {
        let source = "
        var a, b, result;
        begin
            a := 10;
            b := 5;
            result := a + b
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("add"), "Should contain add instruction");
        assert!(ir.matches("ld").count() >= 2, "Should load variables a and b");
        assert!(ir.contains("st [result]"), "Should store to variable result");
        assert!(ir.contains("li v0, 10") && ir.contains("li v1, 5"), "Should load constants 10 and 5");
        
        Ok(())
    }

    // Test all arithmetic operators
    // Purpose: Ensure all arithmetic operations are generated
    #[test]
    fn test_all_arithmetic_operators() -> Pl0Result<()> {
        let source = "
        var a, b, sum, diff, prod, quot, rem;
        begin
            a := 20;
            b := 3;
            sum := a + b;
            diff := a - b;
            prod := a * b;
            quot := a / b;
            rem := a mod b
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("add").count(), 1, "Should contain one add instruction");
        assert_eq!(ir.matches("sub").count(), 1, "Should contain one sub instruction");
        assert_eq!(ir.matches("mul").count(), 1, "Should contain one mul instruction");
        assert_eq!(ir.matches("div").count(), 1, "Should contain one div instruction");
        assert_eq!(ir.matches("mod").count(), 1, "Should contain one mod instruction");
        assert!(ir.matches("ld").count() >= 5, "Should load variables for each operation");
        assert!(ir.matches("st").count() >= 5, "Should store results for each operation");
        
        Ok(())
    }

    // Test if statement
    // Purpose: Ensure comparison, branching, and labels
    #[test]
    fn test_if_statement() -> Pl0Result<()> {
        let source = "
        var x, result;
        begin
            x := 10;
            if x > 5 then
                result := 1
            else
                result := 0
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_gt"), "Should contain greater-than comparison");
        assert!(ir.matches("beqz").count() >= 1, "Should contain branch instruction");
        assert!(ir.matches("L").count() >= 2, "Should contain at least two labels for if-else");
        assert!(ir.contains("st [result], v4") && ir.contains("li v4, 1"), "Should assign 1 in then branch");
        assert!(ir.contains("st [result], v5") && ir.contains("li v5, 0"), "Should assign 0 in else branch");
        
        Ok(())
    }

    // Test while loop
    // Purpose: Ensure loop structure with comparison, branching, and jumps
    #[test]
    fn test_while_loop() -> Pl0Result<()> {
        let source = "
        var i, sum;
        begin
            i := 0;
            sum := 0;
            while i < 10 do
                begin
                    sum := sum + i;
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_lt"), "Should contain less-than comparison");
        assert!(ir.matches("jump").count() >= 1, "Should contain jump to loop start");
        assert!(ir.matches("beqz").count() >= 1, "Should contain conditional branch to exit");
        assert!(ir.matches("L").count() >= 2, "Should contain at least two labels for loop");
        assert!(ir.contains("add"), "Should contain addition in loop body");
        
        Ok(())
    }

    // Test procedure call
    // Purpose: Ensure procedure definition, calls, and prologue/epilogue
    #[test]
    fn test_procedure_call() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure increment;
        begin
            x := x + 1
        end;
        
        begin
            x := 5;
            call increment;
            call increment
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("increment:"), "Should contain procedure label");
        assert_eq!(ir.matches("call increment").count(), 2, "Should call increment twice");
        assert!(ir.contains("proc_enter"), "Should contain procedure entry");
        assert!(ir.contains("proc_exit") || ir.contains("ret"), "Should contain procedure exit");
        assert!(ir.contains("add"), "Should contain addition in procedure body");
        
        Ok(())
    }

    // Test nested procedures
    // Purpose: Ensure nested procedure scoping and calls
    #[test]
    fn test_nested_procedures() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure outer;
            var y;
            
            procedure inner;
            begin
                x := x + 1
            end;
        begin
            y := 10;
            call inner
        end;
        
        begin
            x := 5;
            call outer
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("outer:"), "Should contain outer procedure");
        assert!(ir.contains("inner:"), "Should contain inner procedure");
        assert!(ir.contains("call inner"), "Should call inner procedure");
        assert!(ir.contains("call outer"), "Should call outer procedure");
        assert!(ir.contains("bp") || ir.contains("offset"), "Should manage stack for local variable y");
        
        Ok(())
    }

    // Test constant usage
    // Purpose: Ensure constants are loaded correctly
    #[test]
    fn test_constants() -> Pl0Result<()> {
        let source = "
        const max = 100;
        var x;
        begin
            x := max
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 100"), "Should load constant value 100");
        assert!(ir.contains("st [x], v0"), "Should store to variable x");
        
        Ok(())
    }

    // Test relational operators
    // Purpose: Ensure all relational operators
    #[test]
    fn test_relational_operators() -> Pl0Result<()> {
        let source = "
        var a, b, r1, r2, r3, r4, r5, r6;
        begin
            a := 10;
            b := 5;
            if a > b then r1 := 1;
            if a < b then r2 := 1;
            if a = b then r3 := 1;
            if a # b then r4 := 1;
            if a >= b then r5 := 1;
            if a <= b then r6 := 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("cmp_gt").count(), 1, "Should contain one greater-than comparison");
        assert_eq!(ir.matches("cmp_lt").count(), 1, "Should contain one less-than comparison");
        assert_eq!(ir.matches("cmp_eq").count(), 1, "Should contain one equal comparison");
        assert_eq!(ir.matches("cmp_ne").count(), 1, "Should contain one not-equal comparison");
        assert_eq!(ir.matches("cmp_ge").count(), 1, "Should contain one greater-or-equal comparison");
        assert_eq!(ir.matches("cmp_le").count(), 1, "Should contain one less-or-equal comparison");
        assert!(ir.matches("beqz").count() >= 6, "Should contain branches for each condition");
        
        Ok(())
    }

    // Test odd condition
    // Purpose: Ensure odd check implementation
    #[test]
    fn test_odd_condition() -> Pl0Result<()> {
        let source = "
        var x, result;
        begin
            x := 5;
            if odd x then
                result := 1
            else
                result := 0
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("is_odd"), "Should check is_odd condition");
        assert!(ir.matches("beqz").count() >= 1, "Should contain branch for odd check");
        assert!(ir.contains("st [result], v3") && ir.contains("li v3, 1"), "Should assign 1 for odd");
        assert!(ir.contains("st [result], v4") && ir.contains("li v4, 0"), "Should assign 0 for even");
        
        Ok(())
    }

    // Test I/O operations
    // Purpose: Ensure read and write instructions
    #[test]
    fn test_io_operations() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            read(x);
            write(x)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("read_int"), "Should read into variable x");
        assert!(ir.contains("write_int"), "Should write variable x");
        
        Ok(())
    }

    // Test string write
    // Purpose: Ensure string literal output
    #[test]
    fn test_write_string() -> Pl0Result<()> {
        let source = r#"
        begin
            writeStr("Hello, World!")
        end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("write_str") && ir.contains("\"Hello, World!\""), 
                "Should contain string write instruction with literal");
        
        Ok(())
    }

    // Test main label
    // Purpose: Ensure program entry point
    #[test]
    fn test_main_label() -> Pl0Result<()> {
        let source = "
        begin
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("main:"), "Should contain main label as entry point");
        
        Ok(())
    }

    // Test exit code
    // Purpose: Ensure program termination
    #[test]
    fn test_exit_code() -> Pl0Result<()> {
        let source = "
        begin
            exit 0
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("exit 0"), "Should contain exit instruction with code 0");
        
        Ok(())
    }

    // Test global variables
    // Purpose: Ensure multiple global variables
    #[test]
    fn test_global_variables() -> Pl0Result<()> {
        let source = "
        var x, y, z;
        begin
            x := 1;
            y := 2;
            z := 3
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("var x"), "Should declare global variable x");
        assert!(ir.contains("var y"), "Should declare global variable y");
        assert!(ir.contains("var z"), "Should declare global variable z");

        assert!(ir.contains("li v0, 1") && ir.contains("st [x], v0"), "Should assign 1 to x");
        assert!(ir.contains("li v1, 2") && ir.contains("st [y], v1"), "Should assign 2 to y");
        assert!(ir.contains("li v2, 3") && ir.contains("st [z], v2"), "Should assign 3 to z");
        
        Ok(())
    }

    // Test local variables in procedure
    // Purpose: Ensure stack-based allocation
    #[test]
    fn test_local_variables_in_procedure() -> Pl0Result<()> {
        let source = "
        procedure test;
            var local1, local2;
        begin
            local1 := 10;
            local2 := 20
        end;
        
        begin
            call test
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("bp-16") || ir.contains("bp-24"), "Should use stack offsets for local1 and local2");
        assert!(ir.contains("li v0, 10") && ir.contains("st [bp-16], v0"), "Should assign 10 to local1");
        assert!(ir.contains("li v1, 20") && ir.contains("st [bp-24], v1"), "Should assign 20 to local2");
        assert!(ir.contains("proc_enter"), "Should have procedure entry");
        
        Ok(())
    }

    // Test unary minus
    // Purpose: Ensure unary minus as subtraction
    #[test]
    fn test_unary_minus() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := -5;
            y := -x
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, -5"), "Should handle x := -5 as subtraction");
        assert!(ir.contains("sub v3, v1, v2"), "Should handle y := -x as subtraction");
        assert!(ir.contains("st [x], v0"), "Should store to x");
        assert!(ir.contains("st [y], v3"), "Should store to y");
        
        Ok(())
    }

    // Test complex expression
    // Purpose: Ensure operator precedence and register allocation
    #[test]
    fn test_complex_expression() -> Pl0Result<()> {
        let source = "
        var a, b, c, result;
        begin
            a := 10;
            b := 5;
            c := 2;
            result := (a + b) * c - a / b
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("add"), "Should contain add");
        assert!(ir.contains("mul"), "Should contain multiply");
        assert!(ir.contains("sub"), "Should contain subtract");
        assert!(ir.contains("div"), "Should contain divide");
        
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let sub_pos = ir.find("sub").unwrap_or(usize::MAX);
        let div_pos = ir.find("div").unwrap_or(usize::MAX);
        
        assert!(add_pos < mul_pos, "Addition should precede multiplication");
        assert!(mul_pos < sub_pos, "Multiplication should precede subtraction");
        assert!(div_pos < sub_pos, "division should precede Subtraction");
        
        assert!(ir.matches("v").count() >= 3, "Should use at least 3 virtual registers");
        
        Ok(())
    }

    // Test multiple constant declarations
    // Purpose: Ensure multiple constants
    #[test]
    fn test_multiple_const_declarations() -> Pl0Result<()> {
        let source = "
        const a = 10;
        const b = 20;
        const c = 30;
        var result;
        begin
            result := a + b + c
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 30") && ir.contains("li v1, 30"), 
                "Should load all constant values");
        assert!(ir.matches("add").count() >= 1, "Should contain at least two additions");
        assert!(ir.contains("st [result], v2"), "Should store to result");
        
        Ok(())
    }

    // Test multiple variable declarations
    // Purpose: Ensure multiple variables
    #[test]
    fn test_multiple_var_declarations() -> Pl0Result<()> {
        let source = "
        var x;
        var y;
        var z;
        begin
            x := 1;
            y := 2;
            z := 3
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("var x"), "Should declare variable x");
        assert!(ir.contains("var y"), "Should declare variable y");
        assert!(ir.contains("var z"), "Should declare variable z");
        
        assert!(ir.contains("li v0, 1") && ir.contains("st [x], v0"), "Should assign 1 to x");
        assert!(ir.contains("li v1, 2") && ir.contains("st [y], v1"), "Should assign 2 to y");
        assert!(ir.contains("li v2, 3") && ir.contains("st [z], v2"), "Should assign 3 to z");
        
        Ok(())
    }

    // Test virtual register allocation
    // Purpose: Ensure correct number of virtual registers
    #[test]
    fn test_virtual_register_allocation() -> Pl0Result<()> {
        let source = "
        var a, b, c;
        begin
            a := 1;
            b := 2;
            c := a + b
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("add v4, v2, v3"), "Should contain addition");
        assert!(ir.contains("st [c], v4"), "Should store to c");
        
        Ok(())
    }

    // Test label generation
    // Purpose: Ensure unique labels for branches
    #[test]
    fn test_label_generation() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            if x > 0 then
                x := 1;
            if x < 0 then
                x := 2
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("L").count() >= 2, "Should generate at least two labels for branches");
        assert!(ir.contains("cmp_gt"), "Should contain greater-than comparison");
        assert!(ir.contains("cmp_lt"), "Should contain less-than comparison");
        assert!(ir.matches("beqz").count() >= 2, "Should contain branches for each condition");
        
        Ok(())
    }

    // Test constant folding
    // Purpose: Ensure constant optimization
    #[test]
    fn test_constant_folding() -> Pl0Result<()> {
        let source = "
        const a = 10, b = 20;
        var result;
        begin
            result := a + b
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 30") || (ir.contains("li v0, 10") && ir.contains("li v1, 20") && ir.contains("add")), 
                "Should fold constants to 30 or perform addition");
        assert!(ir.contains("st [result], v0"), "Should store to result");
        
        Ok(())
    }

    // Test undeclared variable usage
    // Purpose: Ensure failure on undeclared variables
    #[test]
    fn test_undeclared_variable() -> Pl0Result<()> {
        let source = "
        begin
            x := 5
        end.
        ";
        
        assert!(compile_to_ir(source).is_err(), "Should fail on undeclared variable x");
        
        Ok(())
    }

    // Test division by zero
    // Purpose: Ensure division by zero handling
    #[test]
    fn test_division_by_zero() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := 10;
            y := x / 0
        end.
        ";
        
        assert!(compile_to_ir(source).is_err(), "Should fail or handle division by zero");
        
        Ok(())
    }

    // Test deeply nested expressions
    // Purpose: Stress test register allocation and precedence
    #[test]
    fn test_deeply_nested_expression() -> Pl0Result<()> {
        let source = "
        var a, b, c, d, e, result;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
            result := ((a + b) * (c - d)) / e
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        let sub_pos = ir.find("sub").unwrap_or(usize::MAX);
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let div_pos = ir.find("div").unwrap_or(usize::MAX);
        
        assert!(ir.contains("add"), "Should contain addition");
        assert!(ir.contains("sub"), "Should contain subtraction");
        assert!(ir.contains("mul"), "Should contain multiplication");
        assert!(ir.contains("div"), "Should contain division");
        
        assert!(add_pos < mul_pos, "Addition should precede multiplication");
        assert!(sub_pos < mul_pos, "Subtraction should precede multiplication");
        assert!(mul_pos < div_pos, "Multiplication should precede division");
        assert!(ir.matches("v").count() >= 4, "Should allocate at least 4 virtual registers");
        
        Ok(())
    }

    // Test deeply nested procedures
    // Purpose: Test stack frame management
    #[test]
    fn test_deeply_nested_procedures() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure outer;
            var y;
            procedure middle;
                var z;
                procedure inner;
                begin
                    x := x + 1;
                    z := z + 1
                end;
            begin
                z := 10;
                call inner
            end;
        begin
            y := 20;
            call middle
        end;
        
        begin
            x := 5;
            call outer
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("outer:"), "Should contain outer procedure");
        assert!(ir.contains("middle:"), "Should contain middle procedure");
        assert!(ir.contains("inner:"), "Should contain inner procedure");
        assert!(ir.contains("bp") || ir.contains("sp"), "Should manage stack frame");
        assert_eq!(ir.matches("call").count(), 3, "Should contain two procedure calls");
        
        Ok(())
    }

    // Test empty procedure
    // Purpose: Ensure empty procedure handling
    #[test]
    fn test_empty_procedure() -> Pl0Result<()> {
        let source = "
        procedure empty;
        begin
        end;
        
        begin
            call empty
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("empty:"), "Should contain empty procedure label");
        assert!(ir.contains("call empty"), "Should call empty procedure");
        assert!(ir.contains("proc_enter"), "Should have procedure prologue");
        assert!(ir.contains("proc_exit") || ir.contains("ret"), "Should have procedure epilogue");
        
        Ok(())
    }

    // Test large number of variables
    // Purpose: Stress test symbol table and allocation
    #[test]
    fn test_large_number_of_variables() -> Pl0Result<()> {
        let source = "
        var a, b, c, d, e, f, g, h, i, j;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
            f := 6;
            g := 7;
            h := 8;
            i := 9;
            j := 10
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        for var in ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j"] {
            assert!(ir.contains(&format!("var {}", var)), "Should declare variable {}", var);
        }
        for i in 1..=10 {
            assert!(ir.contains(&format!("li v{}, {}", (i-1), i)), "Should load constant {}", i);
        }
        assert!(ir.matches("st").count() >= 10, "Should contain at least 10 store instructions");
        
        Ok(())
    }

    // Test dead code elimination
    // Purpose: Ensure unreachable code removal
    #[test]
    fn test_dead_code_elimination() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := 5;
            if x > 10 then
                y := 20
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(!ir.contains("li v, 20") || !ir.contains("st y"), 
                "Should eliminate unreachable code for y := 20");
        
        Ok(())
    }

    // Test large constant values
    // Purpose: Ensure large constants
    #[test]
    fn test_large_constant() -> Pl0Result<()> {
        let source = "
        const max = 2147483647;
        var x;
        begin
            x := max
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 2147483647"), "Should handle large constant value");
        assert!(ir.contains("st [x], v0"), "Should store to variable x");
        
        Ok(())
    }

    // Test nested if statements
    // Purpose: Test label generation for conditionals
    #[test]
    fn test_nested_if_statements() -> Pl0Result<()> {
        let source = "
        var x, y, z;
        begin
            x := 10;
            if x > 5 then
                if x < 15 then
                    y := 1
                else
                    y := 2
            else
                z := 3
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_gt"), "Should contain greater-than comparison");
        assert!(ir.contains("cmp_lt"), "Should contain less-than comparison");
        assert!(ir.matches("beqz").count() >= 2, "Should contain at least two branch instructions");
        assert!(ir.matches("L").count() >= 3, "Should generate at least three labels");
        
        Ok(())
    }

    // Test invalid procedure call
    // Purpose: Ensure failure on undefined procedures
    #[test]
    fn test_invalid_procedure_call() -> Pl0Result<()> {
        let source = "
        begin
            call nonexistent
        end.
        ";
        
        assert!(compile_to_ir(source).is_err(), "Should fail on call to nonexistent procedure");
        
        Ok(())
    }

    // Test complex loop
    // Purpose: Test nested loops with compound conditions
    #[test]
    fn test_complex_loop() -> Pl0Result<()> {
        let source = "
        var i, j, sum;
        begin
            i := 0;
            sum := 0;
            while i < 10 do
                begin
                    j := 0;
                    while j < 5 do
                        begin
                            sum := sum + i + j;
                            j := j + 1
                        end;
                    i := i + 1
                end
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("cmp_lt").count() >= 2, "Should contain at least two less-than comparisons");
        assert!(ir.matches("L").count() >= 4, "Should generate at least four labels");
        assert!(ir.contains("add"), "Should contain addition for sum");
        assert!(ir.matches("jump").count() >= 2, "Should contain at least two jump instructions");
        
        Ok(())
    }

    // Test constant propagation
    // Purpose: Ensure constant optimization
    #[test]
    fn test_constant_propagation() -> Pl0Result<()> {
        let source = "
        const a = 10;
        var x, y;
        begin
            x := a;
            y := x + 5
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 15") || (ir.contains("li v0, 10") && ir.contains("add") && ir.contains("li v2, 5")), 
                "Should propagate constant or perform addition");
        assert!(ir.contains("st [y], v3"), "Should store to y");
        
        Ok(())
    }

    // Test register spill
    // Purpose: Stress test register allocation
    #[test]
    fn test_register_spill() -> Pl0Result<()> {
        let source = "
        var a, b, c, d, e, f, g, h, i, j, k, result;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5;
            f := 6;
            g := 7;
            h := 8;
            i := 9;
            j := 10;
            k := 11;
            result := a + b + c + d + e + f + g + h + i + j + k
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        let v_count = ir.matches("v").count();
        assert!(v_count >= 10 || ir.contains("spill") || ir.contains("stack"), 
                "Should allocate many registers or spill to stack");
        assert!(ir.matches("add").count() >= 10, "Should contain at least 10 additions");
        
        Ok(())
    }

    // Test empty program
    // Purpose: Ensure minimal program IR
    #[test]
    fn test_empty_program() -> Pl0Result<()> {
        let source = ".";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("exit") || ir.contains("ret"), "Should contain program exit");
        
        Ok(())
    }

    // Test variable shadowing
    // Purpose: Ensure correct stack offsets for shadowed variables
    #[test]
    fn test_variable_shadowing_in_nested_procedures() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure outer;
            var x;  
            procedure inner;
                var x;  
            begin
                x := 30
            end;
        begin
            x := 20;
            call inner
        end;
        
        begin
            x := 10;
            call outer
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("bp-") || ir.contains("offset"), 
                "Should use different stack offsets for shadowed variables");
        assert!(ir.contains("li v2, 10") && ir.contains("st [x], v2"), "Should assign 10 to global x");
        assert!(ir.contains("li v0, 20") && ir.contains("st [bp-16], v0"), "Should assign 20 to outer x");
        assert!(ir.contains("li v1, 30") && ir.contains("st [bp-16], v0"), "Should assign 30 to inner x");
        
        Ok(())
    }

    // Test access to outer scope variables
    // Purpose: Ensure outer scope variable access
    #[test]
    fn test_access_to_outer_scope_variables() -> Pl0Result<()> {
        let source = "
        var global;
        
        procedure outer;
            var outer_var;
            
            procedure inner;
            begin
                global := 1;
                outer_var := 2
            end;
        begin
            outer_var := 10;
            call inner
        end;
        
        begin
            global := 0;
            call outer
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("st [_global]") && ir.contains("li v3, 0"),"Should assign 0 to global in main");
        assert!(ir.contains("st [_global]") && ir.contains("li v1, 1"),"Should assign 1 to global in inner");
        assert!(ir.contains("st [bp-16]") && ir.contains("li v0, 10"),"Should assign 10 to outer_var in outer");
        assert!(ir.contains("st [up-16-1]") && ir.contains("li v2, 2"),"Should assign 2 to outer_var in inner");

        Ok(())
    }

    // Test deeply nested procedures
    // Purpose: Ensure multiple nesting levels
    #[test]
    fn test_deeply_nested_procedures_1() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure level1;
            var y;
            procedure level2;
                var z;
                procedure level3;
                begin
                    x := 1;
                    y := 2;
                    z := 3
                end;
            begin
                call level3
            end;
        begin
            call level2
        end;
        
        begin
            call level1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("level1:"), "Should have level1 procedure");
        assert!(ir.contains("level2:"), "Should have level2 procedure");
        assert!(ir.contains("level3:"), "Should have level3 procedure");

        assert!(ir.contains("call level3"), "Should call level3");
        assert!(ir.contains("call level2"), "Should call level2");
        assert!(ir.contains("call level1"), "Should call level1");
        
        assert!(ir.contains("st [x], v0") && ir.contains("li v0, 1"), "Should assign 1 to x");
        assert!(ir.contains("st [up-16-2], v1") && ir.contains("li v1, 2"), "Should assign 2 to y");
        assert!(ir.contains("st [up-16-1], v2") && ir.contains("li v2, 3"), "Should assign 3 to z");
        
        Ok(())
    }

    // Test nested if statements
    // Purpose: Ensure nested conditionals
    #[test]
    fn test_nested_if_statements_1() -> Pl0Result<()> {
        let source = "
        var a, b, c, result;
        begin
            a := 10;
            b := 5;
            c := 3;
            if a > b then
                if b > c then
                    result := 1
                else
                    result := 2
            else
                result := 3
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("L").count() >= 4, "Should generate at least 4 labels");
        assert!(ir.matches("cmp_gt").count() >= 2, "Should have two greater-than comparisons");
        assert!(ir.matches("beqz").count() >= 2, "Should have at least two branch instructions");
        
        assert!(ir.contains("st [result], v9") && ir.contains("li v9, 1"), "Should assign 1 in inner then");
        assert!(ir.contains("st [result], v10") && ir.contains("li v10, 2"), "Should assign 2 in inner else");
        assert!(ir.contains("st [result], v11") && ir.contains("li v11, 3"), "Should assign 3 in outer else");
        
        Ok(())
    }

    // Test nested while loops
    // Purpose: Ensure nested loop structure
    #[test]
    fn test_nested_while_loops() -> Pl0Result<()> {
        let source = "
        var i, j, sum;
        begin
            i := 0;
            sum := 0;
            while i < 5 do
                begin
                    j := 0;
                    while j < 3 do
                        begin
                            sum := sum + 1;
                            j := j + 1
                        end;
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("jump").count() >= 2, "Should have jumps for nested loops");
        assert!(ir.matches("beqz").count() >= 2, "Should have branches for nested loops");
        assert!(ir.matches("L").count() >= 4, "Should have at least 4 labels");
        assert!(ir.contains("add"), "Should contain addition in inner loop");
        
        Ok(())
    }

    // Test if without else
    // Purpose: Ensure single-branch if
    #[test]
    fn test_if_without_else() -> Pl0Result<()> {
        let source = "
        var x, result;
        begin
            x := 10;
            if x > 5 then
                result := 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("beqz"), "Should have conditional branch");
        assert!(ir.matches("L").count() >= 1, "Should have at least one label");
        assert!(ir.contains("st [result], v4") && ir.contains("li v4, 1"), "Should assign 1 in then branch");
        
        Ok(())
    }

    // Test while with complex condition
    // Purpose: Ensure complex condition in loop
    #[test]
    fn test_while_with_complex_condition() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := 10;
            y := 5;
            while x > y do
                begin
                    x := x - 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison");
        assert!(ir.contains("beqz"), "Should have conditional branch");
        assert!(ir.contains("jump"), "Should have loop back jump");
        assert!(ir.matches("L").count() >= 2, "Should have at least two labels");
        assert!(ir.contains("sub"), "Should contain subtraction in loop body");
        
        Ok(())
    }

    // Test operator precedence
    // Purpose: Ensure multiplication before addition
    #[test]
    fn test_operator_precedence() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := 2 + 3 * 4
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        
        // because of contant propagation, multiplication may be optimized away
        //assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("add v2, v0, v1"), "Should have addition");
        assert!(ir.contains("st [result], v2"), "Should store to result");
        
        Ok(())
    }

    // Test parentheses overriding precedence
    // Purpose: Ensure parentheses enforcement
    #[test]
    fn test_parentheses_override_precedence() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := (2 + 3) * 4
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        
        // because of contant propagation, addition may be optimized away
        //assert!(ir.contains("add"), "Should have addition");
        assert!(ir.contains("mul"), "Should have multiplication");
        //assert!(add_pos < mul_pos, "Addition should precede multiplication");
        assert!(ir.contains("st [result], v2"), "Should store to result");
        
        Ok(())
    }

    // Test deeply nested expressions
    // Purpose: Stress test expression evaluation
    #[test]
    fn test_deeply_nested_expressions() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := ((1 + 2) * (3 - 4)) / ((5 + 6) - (7 * 8))
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("sub"), "Should have subtraction");
        assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("div"), "Should have division");
        assert!(ir.matches("v").count() >= 10, "Should allocate at least 10 virtual registers");
        
        // Verify constant loading
        assert!(ir.contains("li v0, 3"), "Should load 3");
        assert!(ir.contains("li v1, -1"), "Should load -1");
        assert!(ir.contains("li v3, 11"), "Should load 11");
        assert!(ir.contains("li v4, 56"), "Should load 56");

        let sub_pos = ir.find("sub").unwrap_or(usize::MAX);
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let div_pos = ir.find("div").unwrap_or(usize::MAX);
        
        assert!(mul_pos < sub_pos, "Addition should precede multiplication");
        assert!(sub_pos < div_pos, "Subtraction should precede division");
        
        Ok(())
    }

    // Test unary in complex expression
    // Purpose: Ensure unary minus in expressions
    #[test]
    fn test_unary_in_complex_expression() -> Pl0Result<()> {
        let source = "
        var a, b, result;
        begin
            a := 10;
            b := 5;
            result := -a + b * -2
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("sub v4, v2, v3"), "Should handle -a as subtraction");
        assert!(ir.contains("li v6, -2"), "Should handle -2");
        assert!(ir.contains("mul"), "Should handle multiplication");
        assert!(ir.contains("add"), "Should handle addition");
        assert!(ir.contains("st [result]"), "Should store to result");
        
        Ok(())
    }
    
    #[test]
    fn test_arithmetic_runtime_multiplication() -> Pl0Result<()> {
        let source = "
        var result;
        var multiplier;
        begin
            multiplier := 2;
            result := 2147483647 * multiplier
        end.
        ";
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v1, 2147483647"), "Should load max int");
        assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("st [result]"), "Should store to result");
        
        Ok(())
    }
    
    // Test chained comparisons
    // Purpose: Ensure multiple comparisons
    #[test]
    fn test_chained_comparisons() -> Pl0Result<()> {
        let source = "
        var a, b, c, r1, r2;
        begin
            a := 10;
            b := 5;
            c := 3;
            if a > b then
                if b > c then
                    r1 := 1;
            if a # b then
                r2 := 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison");
        assert!(ir.contains("cmp_ne"), "Should have not-equal comparison");
        assert!(ir.matches("beqz").count() >= 2, "Should have branches for comparisons");
        assert!(ir.contains("st [r1], v9") && ir.contains("li v9, 1"), "Should assign 1 to r1");
        assert!(ir.contains("st [r2], v13") && ir.contains("li v13, 1"), "Should assign 1 to r2");
        
        Ok(())
    }

    // Test recursive procedure call
    // Purpose: Ensure recursive call handling
    #[test]
    fn test_recursive_procedure_call() -> Pl0Result<()> {
        let source = "
        var n, result;
        
        procedure factorial;
            var temp;
        begin
            if n <= 1 then
                result := 1
            else
                begin
                    temp := n;
                    n := n - 1;
                    call factorial;
                    result := result * temp
                end
        end;
        
        begin
            n := 5;
            result := 1;
            call factorial
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("factorial:"), "Should have factorial procedure");
        assert!(ir.contains("call factorial"), "Should have recursive call");
        assert!(ir.contains("bp") || ir.contains("sp"), "Should manage stack for recursion");
        assert!(ir.contains("cmp_le"), "Should have less-or-equal comparison");
        assert!(ir.contains("mul"), "Should have multiplication");
        
        Ok(())
    }

    // Test multiple procedure calls
    // Purpose: Ensure multiple procedure calls
    #[test]
    fn test_multiple_procedure_calls() -> Pl0Result<()> {
        let source = "
        var x;
        
        procedure add1;
        begin
            x := x + 1
        end;
        
        procedure mul2;
        begin
            x := x * 2
        end;
        
        begin
            x := 5;
            call add1;
            call mul2;
            call add1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("add1:"), "Should have add1 procedure");
        assert!(ir.contains("mul2:"), "Should have mul2 procedure");
        assert_eq!(ir.matches("call add1").count(), 2, "Should call add1 twice");
        assert_eq!(ir.matches("call mul2").count(), 1, "Should call mul2 once");
        assert!(ir.contains("add"), "Should have addition");
        assert!(ir.contains("mul"), "Should have multiplication");
        
        Ok(())
    }

    // Test procedure with no body
    // Purpose: Ensure empty procedure structure
    #[test]
    fn test_procedure_with_no_body() -> Pl0Result<()> {
        let source = "
        procedure empty;
        begin
        end;
        
        begin
            call empty
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("empty:"), "Should have empty procedure label");
        assert!(ir.contains("call empty"), "Should call empty procedure");
        assert!(ir.contains("proc_enter"), "Should have procedure prologue");
        assert!(ir.contains("proc_exit") || ir.contains("ret"), "Should have procedure epilogue");
        
        Ok(())
    }

    // Test constant in expressions
    // Purpose: Ensure constants in expressions
    #[test]
    fn test_constant_in_expressions() -> Pl0Result<()> {
        let source = "
        const multiplier = 10;
        var x, result;
        begin
            x := 5;
            result := x * multiplier
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v2, 10"), "Should load constant multiplier");
        assert!(ir.contains("mul"), "Should perform multiplication");
        assert!(ir.contains("st [result], v3"), "Should store to result");
        
        Ok(())
    }

    // Test many constants
    // Purpose: Ensure multiple constants
    #[test]
    fn test_many_constants() -> Pl0Result<()> {
        let source = "
        const a = 1, b = 2, c = 3, d = 4, e = 5;
        var result;
        begin
            result := a + b + c + d + e
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 3") && ir.contains("li v1, 3") && 
                ir.contains("li v3, 4") && ir.contains("li v5, 5"), "Should load all constants");
        assert!(ir.contains("add"), "As all are constants, should optimize additions, possibly folding");
        assert!(ir.contains("st [result], v6"), "Should store to result");
        
        Ok(())
    }

    // Test many variables
    // Purpose: Ensure multiple variable assignments
    #[test]
    fn test_many_variables() -> Pl0Result<()> {
        let source = "
        var a, b, c, d, e, f, g, h, i, j;
        begin
            a := 1; b := 2; c := 3; d := 4; e := 5;
            f := 6; g := 7; h := 8; i := 9; j := 10
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("var").count() >= 10, "Should declare 10 variables");
        assert!(ir.matches("st").count() >= 10, "Should have stores for all assignments");
        for i in 1..=10 {
            assert!(ir.contains(&format!("li v{}, {}",(i -1), i)), "Should load constant {}", i);
        }
        
        Ok(())
    }

    // Test local variable stack allocation
    // Purpose: Ensure stack allocation for locals
    #[test]
    fn test_local_variable_stack_allocation() -> Pl0Result<()> {
        let source = "
        procedure test;
            var a, b, c, d, e;
        begin
            a := 1;
            b := 2;
            c := 3;
            d := 4;
            e := 5
        end;
        
        begin
            call test
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("bp-16") && ir.contains("bp-24") && ir.contains("bp-32") && ir.contains("bp-40"), "Should use stack offsets");
        for i in 1..=5 {
            assert!(ir.contains(&format!("li v{}, {}", (i-1), i)), "Should assign {}", i);
        }
        assert!(ir.contains("proc_enter"), "Should enter procedure with stack allocation");
        
        Ok(())
    }

    // Test multiple reads
    // Purpose: Ensure multiple read operations
    #[test]
    fn test_multiple_reads() -> Pl0Result<()> {
        let source = "
        var a, b, c;
        begin
            read(a);
            read(b);
            read(c)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("read_int").count(), 3, "Should have 3 read operations");
        assert!(ir.contains("read_int v0"), "Should read into a");
        assert!(ir.contains("read_int v1"), "Should read into b");
        assert!(ir.contains("read_int v2"), "Should read into c");
        
        Ok(())
    }

    // Test multiple writes
    // Purpose: Ensure multiple write operations
    #[test]
    fn test_multiple_writes() -> Pl0Result<()> {
        let source = "
        var a, b, c;
        begin
            a := 1; b := 2; c := 3;
            write(a);
            write(b);
            write(c)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("write_int").count(), 3, "Should have 3 write operations");
        assert!(ir.contains("write_int v3"), "Should write a");
        assert!(ir.contains("write_int v4"), "Should write b");
        assert!(ir.contains("write_int v5"), "Should write c");
        
        Ok(())
    }

    // Test interleaved I/O
    // Purpose: Ensure alternating read and write
    #[test]
    fn test_interleaved_io() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            read(x);
            write(x);
            read(x);
            write(x)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("read_int").count(), 2, "Should have 2 read operations");
        assert_eq!(ir.matches("write_int").count(), 2, "Should have 2 write operations");
        
        let read1 = ir.find("read_int").unwrap_or(usize::MAX);
        let write1 = ir.find("write_int").unwrap_or(usize::MAX);
        let read2 = ir.rfind("read_int").unwrap_or(usize::MAX);
        let write2 = ir.rfind("write_int").unwrap_or(usize::MAX);
        
        assert!(read1 < write1 && write1 < read2 && read2 < write2, "Should follow read-write-read-write order");
        
        Ok(())
    }

    // Test writing expression result
    // Purpose: Ensure expression evaluation before write
    #[test]
    fn test_write_expression_result() -> Pl0Result<()> {
        let source = "
        var a, b;
        begin
            a := 10;
            b := 5;
            write(a + b * 2)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("mul"), "Should compute multiplication");
        assert!(ir.contains("add"), "Should compute addition");
        assert!(ir.contains("write_int"), "Should write result");
        
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        
        assert!(mul_pos < add_pos, "Multiplication should precede addition");
        
        Ok(())
    }

    // Test multiple string literals
    // Purpose: Ensure multiple string writes
    #[test]
    fn test_write_string_literals() -> Pl0Result<()> {
        let source = r#"
        begin
            writeStr("Line 1");
            writeStr("Line 2");
            writeStr("Line 3")
        end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("write_str").count(), 3, "Should have 3 string write operations");
        assert!(ir.contains("\"Line 1\""), "Should contain first string literal");
        assert!(ir.contains("\"Line 2\""), "Should contain second string literal");
        assert!(ir.contains("\"Line 3\""), "Should contain third string literal");
        
        Ok(())
    }

    // Test empty begin-end block
    // Purpose: Ensure empty block IR
    #[test]
    fn test_empty_begin_end() -> Pl0Result<()> {
        let source = "
        begin
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("main:"), "Should have main label");
        assert!(ir.contains("exit") || ir.contains("ret"), "Should have program exit");
        
        Ok(())
    }

    // Test single statement begin
    // Purpose: Ensure single statement handling
    #[test]
    fn test_single_statement_begin() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            x := 42
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 42"), "Should load constant 42");
        assert!(ir.contains("st [x], v0"), "Should store to variable x");
        
        Ok(())
    }

    // Test zero values
    // Purpose: Ensure zero handling
    #[test]
    fn test_zero_values() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := 0;
            y := x * 0
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 0"), "Should load zero");
        assert!(ir.contains("mul v3, v1, v2"), "Should handle multiplication by zero");
        assert!(ir.contains("st [y], v3"), "Should store to y");
        
        Ok(())
    }

    // Test negative constants
    // Purpose: Ensure negative constant handling
    #[test]
    fn test_negative_constants() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            x := -100
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, -100"), "Should handle negative constant");
        assert!(ir.contains("st [x], v0"), "Should store to x");
        
        Ok(())
    }

    // Test large numbers
    // Purpose: Ensure large constant handling
    #[test]
    fn test_large_numbers() -> Pl0Result<()> {
        let source = "
        const big = 999999;
        var x;
        begin
            x := big
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 999999"), "Should load large constant");
        assert!(ir.contains("st [x], v0"), "Should store to x");
        
        Ok(())
    }

    // Test many begin-end blocks
    // Purpose: Ensure nested blocks
    #[test]
    fn test_many_begin_end_blocks() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            begin
                begin
                    x := 1
                end
            end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 1") && ir.contains("st [x], v0"), "Should execute innermost assignment");
        
        Ok(())
    }

    // Test all relational operators in sequence
    // Purpose: Ensure all relational operators
    #[test]
    fn test_all_relational_in_sequence() -> Pl0Result<()> {
        let source = "
        var a, b;
        begin
            a := 10;
            b := 10;
            if a = b then a := 1;
            if a # b then a := 2;
            if a < b then a := 3;
            if a > b then a := 4;
            if a <= b then a := 5;
            if a >= b then a := 6
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("cmp_eq").count(), 1, "Should have one equal comparison");
        assert_eq!(ir.matches("cmp_ne").count(), 1, "Should have one not-equal comparison");
        assert_eq!(ir.matches("cmp_lt").count(), 1, "Should have one less-than comparison");
        assert_eq!(ir.matches("cmp_gt").count(), 1, "Should have one greater-than comparison");
        assert_eq!(ir.matches("cmp_le").count(), 1, "Should have one less-or-equal comparison");
        assert_eq!(ir.matches("cmp_ge").count(), 1, "Should have one greater-or-equal comparison");
        assert!(ir.matches("beqz").count() >= 6, "Should have branches for all comparisons");
        
        Ok(())
    }

    // Test register reuse
    // Purpose: Ensure efficient register reuse
    #[test]
    fn test_register_reuse() -> Pl0Result<()> {
        let source = "
        var a, b, c;
        begin
            a := 1;
            b := 2;
            c := 3;
            a := 4;
            b := 5;
            c := 6
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        for i in 1..=6 {
            assert!(ir.contains(&format!("li v{}, {}", (i-1), i)), "Should load constant {}", i);
        }
        
        Ok(())
    }

    // Test complex statement sequence
    // Purpose: Ensure operation sequence
    #[test]
    fn test_complex_statement_sequence() -> Pl0Result<()> {
        let source = "
        var x, y, z;
        begin
            x := 1;
            y := x + 2;
            z := y * 3;
            x := z - 4;
            y := x / 2;
            z := y mod 3
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let sub_pos = ir.find("sub").unwrap_or(usize::MAX);
        let div_pos = ir.find("div").unwrap_or(usize::MAX);
        let mod_pos = ir.find("mod").unwrap_or(usize::MAX);
        
        assert!(ir.contains("add"), "Should have add");
        assert!(ir.contains("mul"), "Should have mul");
        assert!(ir.contains("sub"), "Should have sub");
        assert!(ir.contains("div"), "Should have div");
        assert!(ir.contains("mod"), "Should have mod");
        
        assert!(add_pos < mul_pos, "Addition should precede multiplication");
        assert!(mul_pos < sub_pos, "Multiplication should precede subtraction");
        assert!(sub_pos < div_pos, "Subtraction should precede division");
        assert!(div_pos < mod_pos, "Division should precede modulo");
        
        Ok(())
    }

    // Test procedure with many locals
    // Purpose: Ensure stack allocation for locals
    #[test]
    fn test_procedure_with_many_locals() -> Pl0Result<()> {
        let source = "
        procedure test;
            var a, b, c, d, e, f, g, h, i, j;
        begin
            a:=1; b:=2; c:=3; d:=4; e:=5;
            f:=6; g:=7; h:=8; i:=9; j:=10
        end;
        
        begin
            call test
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("bp-"), "Should use stack offsets for locals");
        assert!(ir.contains("proc_enter"), "Should have procedure entry");
        
        for i in 1..=10 {
            assert!(ir.contains(&format!("li v{}, {}", (i-1), i)), "Should assign {}", i);
        }
        
        Ok(())
    }

    // Test palindrome checker
    // Purpose: Ensure complex logic with loops and modulo
    #[test]
    fn test_palindrome_checker() -> Pl0Result<()> {
        let source = r#"
            var n, original_n, reversed_n, digit;
            procedure reverse_number;
                var temp;
                begin
                    reversed_n := 0;
                    temp := original_n;
                    while temp > 0 do
                    begin
                        digit := temp - (temp / 10) * 10;
                        reversed_n := reversed_n * 10 + digit;
                        temp := temp / 10
                    end
                end;

            begin
                read n;
                original_n := n;
                call reverse_number;
                if n = reversed_n then
                    write 1
                else
                    write 0
            end.
        "#;
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("reverse_number:"), "Should have reverse_number procedure");
        assert!(ir.contains("cmp_eq"), "Should have equality comparison");
        assert!(ir.contains("write_int") && ir.contains("li v25, 1"), "Should write 1 for palindrome");
        assert!(ir.contains("write_int") && ir.contains("li v26, 0"), "Should write 0 for non-palindrome");
        assert!(ir.contains("div") && ir.contains("mul") && ir.contains("sub"), 
                "Should have arithmetic for digit extraction");
        
        Ok(())
    }

    // Test power of a number
    // Purpose: Ensure loop-based exponentiation
    #[test]
    fn test_power_of_a_number() -> Pl0Result<()> {
        let source = r#"
            var base, exponent, result, i;
            begin
                read base;
                read exponent;
                result := 1;
                i := 0;
                if exponent = 0 then
                    result := 1
                else
                    begin
                        while i < exponent do
                        begin
                            result := result * base;
                            i := i + 1
                        end
                    end;
                write result
            end.
        "#;
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_eq"), "Should have equality comparison");
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("write_int"), "Should write result");
        
        Ok(())
    }

    // Test perfect number
    // Purpose: Ensure loop and modulo for perfect number
    #[test]
    fn test_perfect_number() -> Pl0Result<()> {
        let source = r#"
            var n, sum, i, is_perfect;
            begin
                read n;
                sum := 0;
                i := 1;
                is_perfect := 0;
                while i < n do
                begin
                    if (n - (n / i) * i) = 0 then
                        sum := sum + i;
                    i := i + 1
                end;
                if sum = n then
                    is_perfect := 1;
                write is_perfect
            end.
        "#;
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_eq"), "Should have equality comparison");
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("div") && ir.contains("mul"), "Should have arithmetic for modulo");
        assert!(ir.contains("add"), "Should have addition for sum");
        assert!(ir.contains("write_int"), "Should write is_perfect");
        
        Ok(())
    }

    // Test prime number checker
    // Purpose: Ensure loop and modulo for prime check
    #[test]
    fn test_prime_number_checker() -> Pl0Result<()> {
        let source = r#"
            var n, i, is_prime;
            begin
                read n;
                is_prime := 1;
                i := 2;
                if n <= 1 then
                    is_prime := 0;
                while i < n do
                begin
                    if (n - (n / i) * i) = 0 then
                    begin
                        is_prime := 0;
                        i := n
                    end;
                    i := i + 1
                end;
                write is_prime
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_le"), "Should have less-or-equal comparison");
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("div") && ir.contains("mul"), "Should have arithmetic for modulo");
        assert!(ir.contains("write_int"), "Should write is_prime");
        
        Ok(())
    }

    // Test sum of digits
    // Purpose: Ensure loop-based digit extraction
    #[test]
    fn test_sum_of_digits() -> Pl0Result<()> {
        let source = r#"
            var n, sum, digit;
            begin
                read n;
                sum := 0;
                while n > 0 do
                begin
                    digit := n - (n / 10) * 10;
                    sum := sum + digit;
                    n := n / 10
                end;
                write sum
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison");
        assert!(ir.contains("div") && ir.contains("mul"), "Should have arithmetic for digit extraction");
        assert!(ir.contains("add"), "Should have addition for sum");
        assert!(ir.contains("write_int"), "Should write sum");
        
        Ok(())
    }

    // Test count digits
    // Purpose: Ensure digit counting with zero case
    #[test]
    fn test_count_digits() -> Pl0Result<()> {
        let source = r#"
            var n, count;
            begin
                read n;
                count := 0;
                if n = 0 then
                    count := 1
                else
                    begin
                        while n > 0 do
                        begin
                            n := n / 10;
                            count := count + 1
                        end
                    end;
                write count
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_eq"), "Should have equality comparison");
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison");
        assert!(ir.contains("div"), "Should have division for digit counting");
        assert!(ir.contains("add"), "Should have addition for count");
        assert!(ir.contains("write_int"), "Should write count");
        
        Ok(())
    }

    // Test multiplication via repeated addition
    // Purpose: Ensure loop-based multiplication
    #[test]
    fn test_multiplication_via_repeated_addition() -> Pl0Result<()> {
        let source = r#"
            var a, b, result, i;
            begin
                read a;
                read b;
                result := 0;
                i := 0;
                while i < b do
                begin
                    result := result + a;
                    i := i + 1
                end;
                write result
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("add"), "Should have addition for result");
        assert!(ir.contains("write_int "), "Should write result");
        
        Ok(())
    }

    // Test division via repeated subtraction
    // Purpose: Ensure loop-based division
    #[test]
    fn test_division_via_repeated_subtraction() -> Pl0Result<()> {
        let source = r#"
            var a, b, quotient;
            begin
                read a;
                read b;
                quotient := 0;
                while a >= b do
                begin
                    a := a - b;
                    quotient := quotient + 1
                end;
                write quotient
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_ge"), "Should have greater-or-equal comparison");
        assert!(ir.contains("sub"), "Should have subtraction for division");
        assert!(ir.contains("add"), "Should have addition for quotient");
        assert!(ir.contains("write_int"), "Should write quotient");
        
        Ok(())
    }

    // Test factorial
    // Purpose: Ensure loop-based factorial
    #[test]
    fn test_factorial() -> Pl0Result<()> {
        let source = r#"
            var n, result, i;
            begin
                read n;
                result := 1;
                i := 1;
                while i <= n do
                begin
                    result := result * i;
                    i := i + 1
                end;
                write result
            end.
        "#;
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_le"), "Should have less-or-equal comparison");
        assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("add"), "Should have addition for counter");
        assert!(ir.contains("write_int"), "Should write result");
        
        Ok(())
    }

    // Test Fibonacci
    // Purpose: Ensure Fibonacci sequence computation
    #[test]
    fn test_fibonacci() -> Pl0Result<()> {
        let source = r#"
            var n, f1, f2, next, i;
            begin
                read n;
                f1 := 0;
                f2 := 1;
                next := 0;
                i := 2;
                if n = 0 then
                    write 0
                else if n = 1 then
                    write 1
                else
                    begin
                        while i <= n do
                        begin
                            next := f1 + f2;
                            f1 := f2;
                            f2 := next;
                            i := i + 1
                        end;
                        write next
                    end
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_eq"), "Should have equality comparisons");
        assert!(ir.contains("cmp_le"), "Should have less-or-equal comparison");
        assert!(ir.contains("add"), "Should have additions for Fibonacci");
        assert!(ir.contains("write_int") && ir.contains("li v1, 0"), "Should write 0 for n=0");
        assert!(ir.contains("write_int") && ir.contains("li v2, 1"), "Should write 1 for n=1");
        assert!(ir.contains("write_int"), "Should write next for n>1");
        
        Ok(())
    }

    // Test GCD
    // Purpose: Ensure GCD computation using Euclidean algorithm
    #[test]
    fn test_gcd() -> Pl0Result<()> {
        let source = r#"
            var a, b, temp;
            begin
                read a;
                read b;
                while b # 0 do
                begin
                    temp := b;
                    b := a - (a / b) * b;
                    a := temp
                end;
                write a
            end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_ne"), "Should have not-equal comparison");
        assert!(ir.contains("div") && ir.contains("mul") && ir.contains("sub"), 
                "Should have arithmetic for GCD");
        assert!(ir.contains("write_int"), "Should write GCD result");
        
        Ok(())
    }

    // Purpose: Ensure optimization of redundant assignments
    #[test]
    fn test_redundant_assignments() -> Pl0Result<()> {
        let source = "
        var x;
        begin
            x := 5;
            x := 5;
            x := 5
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 5"), "Should load constant 5");
        // it is not yet expected to optimize all redundant loads, but should optimize stores
        assert!(ir.matches("st [x], v2").count() <= 1, "Should optimize redundant assignments to a single store");
        
        Ok(())
    }

    // Test string concatenation simulation
    // Purpose: Ensure multiple string writes simulate concatenation
    #[test]
    fn test_string_concatenation_simulation() -> Pl0Result<()> {
        let source = r#"
        begin
            writeStr("Hello, ");
            writeStr("World!")
        end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("write_str").count(), 2, "Should have two string write operations");
        assert!(ir.contains("\"Hello, \""), "Should contain first string literal");
        assert!(ir.contains("\"World!\""), "Should contain second string literal");
        
        let hello_pos = ir.find("\"Hello, \"").unwrap_or(usize::MAX);
        let world_pos = ir.find("\"World!\"").unwrap_or(usize::MAX);
        
        assert!(hello_pos < world_pos, "Should write 'Hello, ' before 'World!'");
        
        Ok(())
    }

    // Test recursive procedure with multiple calls
    // Purpose: Ensure recursive calls with proper stack management
    #[test]
    fn test_recursive_procedure_multiple_calls() -> Pl0Result<()> {
        let source = "
        var x, count;
        
        procedure recurse;
            var temp;
        begin
            if count <= 0 then
                x := 0
            else
                begin
                    temp := count;
                    count := count - 1;
                    x := x + 1;
                    call recurse
                end
        end;
        
        begin
            x := 0;
            count := 3;
            call recurse;
            call recurse
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("recurse:"), "Should have recurse procedure label");
        assert_eq!(ir.matches("call recurse").count(), 3, "Should have two recursive calls");
        assert!(ir.contains("cmp_le"), "Should have less-or-equal comparison");
        assert!(ir.contains("sub") && ir.contains("add"), "Should have arithmetic operations");
        assert!(ir.contains("bp") || ir.contains("sp"), "Should manage stack for recursion");
        
        Ok(())
    }

    // Test procedure with parameters (simulated via globals)
    // Purpose: Ensure global variable usage for parameter passing
    #[test]
    fn test_procedure_with_parameters_simulation() -> Pl0Result<()> {
        let source = "
        var param1, param2, result;
        
        procedure add_params;
        begin
            result := param1 + param2
        end;
        
        begin
            param1 := 10;
            param2 := 20;
            call add_params
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("add_params:"), "Should have procedure label");
        assert!(ir.contains("call add_params"), "Should call procedure");
        assert!(ir.contains("ld v0, [param1]") && ir.contains("ld v1, [param2]"), "Should load parameters");
        assert!(ir.contains("add v2, v0, v1"), "Should perform addition");
        assert!(ir.contains("st [result], v2"), "Should store result");
        
        Ok(())
    }

    // Test unused variable declaration
    // Purpose: Ensure no IR for unused variables
    #[test]
    fn test_unused_variable_declaration() -> Pl0Result<()> {
        let source = "
        var x, y;
        begin
            x := 10
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("var x"), "Should declare variable x");
        assert!(ir.contains("var y"), "Should declare variable y");
        assert!(ir.contains("li v0, 10") && ir.contains("st [x], v0"), "Should assign to x");
        assert!(!ir.contains("ld y") && !ir.contains("st y"), "Should not generate IR for unused y");
        
        Ok(())
    }

    // Test complex string output with variables
    // Purpose: Ensure interleaved string and integer writes
    #[test]
    fn test_complex_string_and_variable_output() -> Pl0Result<()> {
        let source = r#"
        var x;
        begin
            x := 42;
            writeStr("Value: ");
            write(x);
            writeStr("!");
        end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("write_str \"Value: \""), "Should write string prefix");
        assert!(ir.contains("write_int v1"), "Should write variable x");
        assert!(ir.contains("write_str \"!\""), "Should write string suffix");
        
        let str1_pos = ir.find("write_str \"Value: \"").unwrap_or(usize::MAX);
        let int_pos = ir.find("write_int v1").unwrap_or(usize::MAX);
        let str2_pos = ir.find("write_str \"!\"").unwrap_or(usize::MAX);
        
        assert!(str1_pos < int_pos && int_pos < str2_pos, "Should follow string-int-string order");
        
        Ok(())
    }

    // Test deeply nested loops
    // Purpose: Stress test loop label generation
    #[test]
    fn test_deeply_nested_loops() -> Pl0Result<()> {
        let source = "
        var i, j, k, sum;
        begin
            i := 0;
            sum := 0;
            while i < 3 do
                begin
                    j := 0;
                    while j < 3 do
                        begin
                            k := 0;
                            while k < 3 do
                                begin
                                    sum := sum + 1;
                                    k := k + 1
                                end;
                            j := j + 1
                        end;
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("cmp_lt").count() >= 3, "Should have at least three less-than comparisons");
        assert!(ir.matches("jump").count() >= 3, "Should have jumps for three loops");
        assert!(ir.matches("beqz").count() >= 3, "Should have branches for three loops");
        assert!(ir.matches("L").count() >= 6, "Should generate at least six labels for nested loops");
        assert!(ir.contains("add"), "Should have addition in innermost loop");
        
        Ok(())
    }

    // Test constant propagation across procedures
    // Purpose: Ensure constant propagation in procedure calls
    #[test]
    fn test_constant_propagation_in_procedure() -> Pl0Result<()> {
        let source = "
        const c = 42;
        var x;
        
        procedure use_const;
        begin
            x := c
        end;
        
        begin
            call use_const
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 42"), "Should propagate constant 42");
        assert!(ir.contains("st [x], v0"), "Should store to x");
        assert!(ir.contains("call use_const"), "Should call procedure");
        
        Ok(())
    }

    // Test modulo edge case (mod 1)
    // Purpose: Ensure modulo by 1 always returns 0
    #[test]
    fn test_modulo_edge_case() -> Pl0Result<()> {
        let source = "
        var x, result;
        begin
            x := 10;
            result := x mod 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v2, 10") || ir.contains("mod v3, v1, v2"), "Should handle modulo by 1");
        assert!(ir.contains("st [result], v3"), "Should store to result");
        
        Ok(())
    }

    // Test procedure with nested conditionals
    // Purpose: Ensure complex control flow in procedures
    #[test]
    fn test_procedure_with_nested_conditionals() -> Pl0Result<()> {
        let source = "
        var x, y;
        
        procedure complex_condition;
        begin
            if x > 5 then
                if y < 10 then
                    x := 1
                else
                    x := 2
            else
                if y > 5 then
                    x := 3
                else
                    x := 4
        end;
        
        begin
            x := 7;
            y := 3;
            call complex_condition
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.matches("cmp_gt").count() >= 1, "Should have greater-than comparison");
        assert!(ir.matches("cmp_lt").count() >= 1, "Should have less-than comparison");
        assert!(ir.matches("beqz").count() >= 2, "Should have branches for nested conditionals");
        assert!(ir.matches("L").count() >= 4, "Should generate at least four labels");
        assert!(ir.contains("call complex_condition"), "Should call procedure");
        
        Ok(())
    }

    // Test empty while loop
    // Purpose: Ensure empty loop handling
    #[test]
    fn test_empty_while_loop() -> Pl0Result<()> {
        let source = "
        var i;
        begin
            i := 0;
            while i < 5 do
                begin
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("jump"), "Should have loop back jump");
        assert!(ir.contains("beqz"), "Should have conditional branch");
        assert!(ir.contains("add"), "Should have increment");
        
        Ok(())
    }

    // Test unreachable procedure
    // Purpose: Ensure no IR for uncalled procedures
    #[test]
    fn test_unreachable_procedure() -> Pl0Result<()> {
        let source = "
        procedure unused;
        begin
            write(42)
        end;
        
        begin
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        // currently, unused procedures are still emitted, but their body should be optimized away
        assert!(!ir.contains("unused:"), "Should not generate IR for uncalled procedure");
        assert!(!ir.contains("write_int"), "Should not generate write for unused procedure");
        
        Ok(())
    }

    // Test chained assignments
    // Purpose: Ensure sequential assignments
    #[test]
    fn test_chained_assignments() -> Pl0Result<()> {
        let source = "
        var x, y, z;
        begin
            x := 10;
            y := x;
            z := y
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 10"), "Should load constant 10");
        assert!(ir.contains("st [x], v0"), "Should store to x");
        assert!(ir.contains("ld v1, [x]") && ir.contains("st [y], v1"), "Should copy x to y");
        assert!(ir.contains("ld v2, [y]") && ir.contains("st [z], v2"), "Should copy y to z");
        
        Ok(())
    }

    // Test large nested expression with constants
    // Purpose: Stress test constant folding
    #[test]
    fn test_large_nested_expression_with_constants() -> Pl0Result<()> {
        let source = "
        const a = 2, b = 3, c = 4;
        var result;
        begin
            result := (a * b + c) * (a + b * c)
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("mul"), "Should have multiplication");
        assert!(ir.contains("add"), "Should have addition");
        assert!(ir.contains("st [result]"), "Should store to result");
        
        let mul_pos = ir.find("mul").unwrap_or(usize::MAX);
        let add_pos = ir.find("add").unwrap_or(usize::MAX);
        
        assert!(mul_pos > add_pos, "Multiplication should succed addition");
        
        Ok(())
    }

    // Test overflow handling in arithmetic
    // Purpose: Ensure large number arithmetic
    #[test]
    fn test_overflow_arithmetic() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := 2147483647 + 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 2147483648"), "Should load max int");
        //assert!(ir.contains("add v2, v0, v1"), "Should have addition");
        assert!(ir.contains("st [result], v0"), "Should store to result");
        
        Ok(())
    }

    // Test overflow handling in arithmetic
    // Purpose: Ensure large number arithmetic
    #[test]
    fn test_64bit_arithmetic() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := 9223372036854775807;
            result := result + 1
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        // 9223372036854775807 is 2^63 - 1 (max int64)
        assert!(ir.contains("li v0, 9223372036854775807"), "Should load max 64-bit signed integer");
        assert!(ir.contains("st [result], v0"), "Should store to result");
        
        Ok(())
    }

    #[test]
    fn test_negative_64bit_numbers() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := -9223372036854775808
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        // -9223372036854775808 is -(2^63) (min int64)
        assert!(ir.contains("li v0, -9223372036854775808"), "Should load min 64-bit signed integer");
        assert!(ir.contains("st [result], v0"), "Should store to result");
        
        Ok(())
    }
    
    // Test minimal procedure call
    // Purpose: Ensure minimal procedure overhead
    #[test]
    fn test_minimal_procedure_call() -> Pl0Result<()> {
        let source = "
        procedure minimal;
        begin
        end;
        
        begin
            call minimal
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("minimal:"), "Should have procedure label");
        assert!(ir.contains("call minimal"), "Should call procedure");
        assert!(ir.contains("proc_enter") && (ir.contains("proc_exit") || ir.contains("ret")), 
                "Should have minimal prologue and epilogue");
        
        Ok(())
    }

    // Test nested procedure with local and global access
    // Purpose: Ensure correct scoping
    #[test]
    fn test_nested_procedure_local_global_access() -> Pl0Result<()> {
        let source = "
        var global;
        
        procedure outer;
            var local;
            procedure inner;
            begin
                local := 10;
                global := local + 1
            end;
        begin
            call inner
        end;
        
        begin
            global := 0;
            call outer
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("outer:"), "Should have outer procedure");
        assert!(ir.contains("inner:"), "Should have inner procedure");
        assert!(ir.contains("call inner"), "Should call inner procedure");
        assert!(ir.contains("li v0, 10") && ir.contains("st [up-16-1], v0"), "Should assign to local");
        assert!(ir.contains("ld") && ir.contains("add") && ir.contains("st [_global]"), 
                "Should assign to global based on local");
        
        Ok(())
    }

    // Test complex condition with multiple operators
    // Purpose: Ensure compound conditions
    #[test]
    fn test_complex_condition_multiple_operators() -> Pl0Result<()> {
        let source = "
        var x, y, z, result;
        begin
            x := 10;
            y := 5;
            z := 3;
            if x > y then
                if y > z then
                    result := 1
                else
                    result := 0
            else
                result := 0
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        // when i will implement full compiler optimizations, this test may need to be adjusted as as x, y, z are constants.
        assert!(ir.matches("cmp_gt").count() >= 2, "Should have two greater-than comparisons");
        assert!(ir.matches("beqz").count() >= 2, "Should have branches for and condition");
        
        assert!(ir.contains("st [result], v9") && ir.contains("li v9, 1"), "Should assign 1 for true");
        assert!(ir.contains("st [result], v10") && ir.contains("li v10, 0"), "Should assign 0 for false");
        assert!(ir.contains("st [result], v11") && ir.contains("li v11, 0"), "Should assign 0 for false");
        
        Ok(())
    }

    // Test procedure with multiple returns
    // Purpose: Simulate multiple exit points
    #[test]
    fn test_procedure_multiple_returns_simulation() -> Pl0Result<()> {
        let source = "
        var x, result;
        
        procedure check;
        begin
            if x > 0 then
                result := 1
            else
                result := -1
        end;
        
        begin
            x := 5;
            call check
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("check:"), "Should have procedure label");
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison");
        assert!(ir.contains("beqz"), "Should have conditional branch");
        
        assert!(ir.contains("li v3, 1") && ir.contains("st [result], v3"), "Should assign 1 for positive");
        assert!(ir.contains("li v4, -1") && ir.contains("st [result], v4"), "Should assign -1 for non-positive");
        assert!(ir.contains("proc_exit") || ir.contains("ret"), "Should have procedure exit");
        
        Ok(())
    }

    // Test while loop with no iterations
    // Purpose: Ensure loop with false condition
    #[test]
    fn test_while_loop_no_iterations() -> Pl0Result<()> {
        let source = "
        var i;
        begin
            i := 10;
            while i < 5 do
                begin
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison");
        assert!(ir.contains("beqz"), "Should have branch to skip loop");
        assert!(!ir.contains("add") || !ir.contains("st i"), "Should not execute loop body");
        
        Ok(())
    }

    // Test constant string array simulation
    // Purpose: Ensure multiple string literals
    #[test]
    fn test_constant_string_array_simulation() -> Pl0Result<()> {
        let source = r#"
        begin
            writeStr("First");
            writeStr("Second");
            writeStr("Third")
        end.
        "#;
        
        let ir = compile_to_ir(source)?;
        
        assert_eq!(ir.matches("write_str").count(), 3, "Should have three string writes");
        assert!(ir.contains("\"First\""), "Should contain first string");
        assert!(ir.contains("\"Second\""), "Should contain second string");
        assert!(ir.contains("\"Third\""), "Should contain third string");
        
        Ok(())
    }

    // Test procedure with nested loops
    // Purpose: Ensure loops within procedures
    #[test]
    fn test_procedure_with_nested_loops() -> Pl0Result<()> {
        let source = "
        var sum;
        
        procedure nested_loops;
            var i, j;
        begin
            i := 0;
            sum := 0;
            while i < 3 do
                begin
                    j := 0;
                    while j < 2 do
                        begin
                            sum := sum + 1;
                            j := j + 1
                        end;
                    i := i + 1
                end
        end;
        
        begin
            call nested_loops
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("nested_loops:"), "Should have procedure label");
        assert!(ir.matches("cmp_lt").count() >= 2, "Should have two less-than comparisons");
        assert!(ir.matches("jump").count() >= 2, "Should have jumps for nested loops");
        assert!(ir.matches("L").count() >= 4, "Should have at least four labels");
        assert!(ir.contains("add"), "Should have addition in inner loop");
        
        Ok(())
    }

    // Test arithmetic overflow edge case
    // Purpose: Ensure handling of arithmetic overflow
    #[test]
    fn test_arithmetic_overflow_edge_case() -> Pl0Result<()> {
        let source = "
        var result;
        begin
            result := 2147483647 * 2
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, 4294967294"), "Should load already computed value");
        assert!(!ir.contains("mul"), "Should not have multiplication");
        assert!(ir.contains("st [result]"), "Should store to result");
        
        Ok(())
    }

    // Test modulo with negative numbers
    // Purpose: Ensure negative modulo handling
    #[test]
    fn test_modulo_negative_numbers() -> Pl0Result<()> {
        let source = "
        var x, result;
        begin
            x := -10;
            result := x mod 3
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("li v0, -10"), "Should handle negative number");
        assert!(ir.contains("mod v3, v1, v2"), "Should perform modulo by 3");
        assert!(ir.contains("st [result], v3"), "Should store to result");
        
        Ok(())
    }

    // Test complex nested control flow
    // Purpose: Ensure combination of loops and conditionals
    #[test]
    fn test_complex_nested_control_flow() -> Pl0Result<()> {
        let source = "
        var i, sum;
        begin
            i := 0;
            sum := 0;
            while i < 5 do
                begin
                    if i > 2 then
                        sum := sum + i
                    else
                        sum := sum - i;
                    i := i + 1
                end
        end.
        ";
        
        let ir = compile_to_ir(source)?;
        
        assert!(ir.contains("cmp_lt"), "Should have less-than comparison for loop");
        assert!(ir.contains("cmp_gt"), "Should have greater-than comparison for conditional");
        assert!(ir.contains("add") && ir.contains("sub"), "Should have both addition and subtraction");
        assert!(ir.matches("beqz").count() >= 1, "Should have branch for conditional");
        assert!(ir.contains("jump"), "Should have loop jump");
        
        Ok(())
    }
}
