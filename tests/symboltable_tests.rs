use pl0c::semantic::symboltable::Symbol;
use pl0c::semantic::symboltable::SymbolLocation;
use pl0c::semantic::symboltable::SymbolType;
use pl0c::semantic::symboltable::SymbolTable;
use pl0c::utils::errors::Pl0Error;

#[cfg(test)]
mod tests {
    use super::*;
    use std::mem::discriminant;

    #[test]
    fn test_symbol_creation() {
        let symbol = Symbol::new(
            SymbolType::Variable,
            10,
            SymbolLocation::StackOffset(4),
            false, // is_global
            1,     // level
        );

        assert!(matches!(symbol.symbol_type, SymbolType::Variable));
        assert_eq!(symbol.line_number, 10);
        assert!(matches!(symbol.location, SymbolLocation::StackOffset(4)));
        assert!(!symbol.is_global);
        assert!(!symbol.initialized);
        assert_eq!(symbol.level, 1);
    }

    #[test]
    fn test_symbol_table_creation() {
        let table = SymbolTable::new();
        assert_eq!(table.current_scope(), 0);
        assert_eq!(table.depth(), 1);
    }

    #[test]
    fn test_insert_and_get_symbol() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(
            SymbolType::Variable,
            1,
            SymbolLocation::StackOffset(8),
            false, // is_global
            0,     // level
        );

        table.insert("test_var", symbol).unwrap();

        let retrieved = table.get("test_var");
        assert!(retrieved.is_some());
        let retrieved = retrieved.unwrap();
        assert!(matches!(retrieved.symbol_type, SymbolType::Variable));
        assert_eq!(retrieved.line_number, 1);
    }

    #[test]
    fn test_get_nonexistent_symbol() {
        let table = SymbolTable::new();
        assert!(table.get("nonexistent").is_none());
    }

    #[test]
    fn test_scope_management() {
        let mut table = SymbolTable::new();

        // Initial scope
        assert_eq!(table.depth(), 1);

        // Enter new scope
        table.push_scope();
        assert_eq!(table.depth(), 2);

        // Enter another scope
        table.push_scope();
        assert_eq!(table.depth(), 3);

        // Exit scope
        table.pop_scope().unwrap();
        assert_eq!(table.depth(), 2);

        // Exit another scope
        table.pop_scope().unwrap();
        assert_eq!(table.depth(), 1);
    }

    #[test]
    fn test_push_scope_returns_new_id_and_enter_scope_revisits_it() {
        let mut table = SymbolTable::new();

        let id1 = table.push_scope();
        table.insert(
            "in_first",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, id1),
        ).unwrap();
        table.pop_scope().unwrap();

        let id2 = table.push_scope();
        assert_ne!(id1, id2, "each push_scope should hand out a fresh id");
        table.pop_scope().unwrap();

        // Both scopes were popped, so neither is visible from the root...
        assert!(table.get("in_first").is_none());

        // ...but enter_scope can still jump straight back into the first one.
        table.enter_scope(id1);
        assert!(table.get("in_first").is_some());
    }

    #[test]
    fn test_symbol_lookup_across_scopes() {
        let mut table = SymbolTable::new();

        table.insert(
            "global_var",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("global_var".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "local_var",
            Symbol::new(SymbolType::Variable, 5, SymbolLocation::StackOffset(4), false, 1),
        ).unwrap();

        assert!(table.get("global_var").is_some());
        assert!(table.get("local_var").is_some());

        let global = table.get("global_var").unwrap();
        assert!(global.is_global);

        table.insert(
            "global_var",
            Symbol::new(SymbolType::Constant(42), 10, SymbolLocation::Immediate(42), false, 1),
        ).unwrap();

        let retrieved = table.get("global_var").unwrap();
        assert!(matches!(retrieved.symbol_type, SymbolType::Constant(42)));
        assert_eq!(retrieved.line_number, 10);

        table.pop_scope().unwrap();
        assert!(table.get("local_var").is_none());
    }

    #[test]
    fn test_get_mut() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(
            SymbolType::Variable,
            1,
            SymbolLocation::StackOffset(8),
            false, // is_global
            0,     // level
        );

        table.insert("test_var", symbol).unwrap();

        // Get mutable reference and modify
        {
            let sym_mut = table.get_mut("test_var").unwrap();
            sym_mut.initialized = true;
        }

        // Verify changes
        let sym = table.get("test_var").unwrap();
        assert!(sym.initialized);
    }

    #[test]
    fn test_get_simple() {
        let mut table = SymbolTable::new();

        // Level 0 symbol (global)
        table.insert(
            "var0",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("var0".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        // Level 1 symbol (outer procedure)
        table.insert(
            "var1",
            Symbol::new(SymbolType::Variable, 5, SymbolLocation::StackOffset(4), false, 1),
        ).unwrap();

        table.push_scope();
        // Level 2 symbol (inner procedure)
        table.insert(
            "var2",
            Symbol::new(SymbolType::Variable, 10, SymbolLocation::StackOffset(8), false, 2),
        ).unwrap();

        // Test `get` from the innermost scope
        assert!(table.get("var0").is_some(), "Should access global var0 (Level: 0)");
        assert!(table.get("var1").is_some(), "Should access outer_proc var1 (Level: 1)");
        assert!(table.get("var2").is_some(), "Should access inner_proc var2 (Level: 2)");

        // Verify levels
        let var0 = table.get("var0").unwrap();
        assert_eq!(var0.level, 0);
        let var1 = table.get("var1").unwrap();
        assert_eq!(var1.level, 1);
        let var2 = table.get("var2").unwrap();
        assert_eq!(var2.level, 2);

        // Exit inner_proc scope
        table.pop_scope().unwrap();
        // Still in outer_proc
        assert!(table.get("var0").is_some());
        assert!(table.get("var1").is_some());
        assert!(table.get("var2").is_none());

        // Exit outer_proc scope, back to main
        table.pop_scope().unwrap();
        assert!(table.get("var0").is_some());
        assert!(table.get("var1").is_none());
        assert!(table.get("var2").is_none());
    }

    #[test]
    fn test_type_check_success() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(
            SymbolType::Variable,
            1,
            SymbolLocation::StackOffset(4),
            false, // is_global
            0,     // level
        );
        table.insert("test_var", symbol).unwrap();

        let result = table.type_check("test_var", &SymbolType::Variable, 5);
        assert!(result.is_ok());
    }

    #[test]
    fn test_type_check_failure() {
        let table = SymbolTable::new();

        let result = table.type_check("nonexistent", &SymbolType::Variable, 10);
        assert!(result.is_err());

        let error_msg = result.unwrap_err();
        assert_eq!(
            discriminant(&error_msg),
            discriminant(&Pl0Error::UndefinedSymbol { name: "".to_string(), line: 0 })
        );
    }

    #[test]
    fn test_type_check_different_symbol_types() {
        let mut table = SymbolTable::new();

        table.insert("missing_const", Symbol::new(SymbolType::Variable, 5, SymbolLocation::None, false, 0)).unwrap();
        let constant_result = table.type_check("missing_const", &SymbolType::Constant(42), 5);
        assert!(constant_result.is_err());
        assert!(constant_result.unwrap_err().to_string().contains("Type mismatch"));

        let mut table_id_test = SymbolTable::new();
        table_id_test.insert("missing_id", Symbol::new(SymbolType::Constant(1), 7, SymbolLocation::None, false, 0)).unwrap();
        let identifier_result = table_id_test.type_check("missing_id", &SymbolType::Identifier, 7);
        assert!(identifier_result.is_err());
        assert!(identifier_result.unwrap_err().to_string().contains("Type mismatch"));

        let mut table_proc_test = SymbolTable::new();
        table_proc_test.insert("missing_proc", Symbol::new(SymbolType::Variable, 3, SymbolLocation::None, false, 0)).unwrap();
        let procedure_result = table_proc_test.type_check("missing_proc", &SymbolType::Procedure, 3);
        assert!(procedure_result.is_err());
        assert!(procedure_result.unwrap_err().to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_symbol_location_variants() {
        let mut table = SymbolTable::new();

        let stack_symbol = Symbol::new(
            SymbolType::Variable,
            1,
            SymbolLocation::StackOffset(16),
            false, // is_global
            0,     // level
        );
        table.insert("stack_var", stack_symbol).unwrap();

        let global_symbol = Symbol::new(
            SymbolType::Variable,
            2,
            SymbolLocation::GlobalLabel("global_label".to_string()),
            true, // is_global
            0,    // level
        );
        table.insert("global_var", global_symbol).unwrap();

        let immediate_symbol = Symbol::new(
            SymbolType::Constant(100),
            3,
            SymbolLocation::Immediate(100),
            false, // is_global
            0,     // level
        );
        table.insert("const_val", immediate_symbol).unwrap();

        let none_symbol = Symbol::new(
            SymbolType::Procedure,
            4,
            SymbolLocation::None,
            false, // is_global
            0,     // level
        );
        table.insert("proc", none_symbol).unwrap();

        // Verify locations
        assert!(matches!(
            table.get("stack_var").unwrap().location,
            SymbolLocation::StackOffset(16)
        ));

        assert!(matches!(
            table.get("global_var").unwrap().location,
            SymbolLocation::GlobalLabel(_)
        ));

        assert!(matches!(
            table.get("const_val").unwrap().location,
            SymbolLocation::Immediate(100)
        ));

        assert!(matches!(
            table.get("proc").unwrap().location,
            SymbolLocation::None
        ));
    }

    #[test]
    fn test_complex_scoping_scenario() {
        let mut table = SymbolTable::new();

        table.insert(
            "x",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("global_x".to_string()), true, 0),
        ).unwrap();

        table.insert(
            "main",
            Symbol::new(SymbolType::Procedure, 2, SymbolLocation::GlobalLabel("main".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "y",
            Symbol::new(SymbolType::Variable, 5, SymbolLocation::StackOffset(4), false, 1),
        ).unwrap();

        table.insert(
            "x",
            Symbol::new(SymbolType::Variable, 6, SymbolLocation::StackOffset(8), false, 1),
        ).unwrap();

        assert!(table.get("x").is_some());
        assert!(table.get("y").is_some());
        assert!(table.get("main").is_some());

        let found_x = table.get("x").unwrap();
        assert_eq!(found_x.level, 1);
        assert!(!found_x.is_global);

        table.pop_scope().unwrap();
        assert!(table.get("y").is_none());
        assert!(table.get("x").is_some());

        let global_x_again = table.get("x").unwrap();
        assert_eq!(global_x_again.level, 0);
        assert!(global_x_again.is_global);
    }

    #[test]
    fn test_redefinition_in_same_scope() {
        let mut table = SymbolTable::new();
        let symbol1 = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);
        let symbol2 = Symbol::new(SymbolType::Constant(10), 2, SymbolLocation::Immediate(10), false, 0);

        table.insert("test_var", symbol1).unwrap();
        let result = table.insert("test_var", symbol2);

        // Assert that the result is an error
        assert!(result.is_err());

        // Extract the error and check its discriminant
        let error_msg = result.unwrap_err();
        assert_eq!(
            discriminant(&error_msg),
            discriminant(&Pl0Error::SymbolAlreadyDefined {
                name: "".to_string(),
                line: 0
            })
        );
    }

    #[test]
    fn test_symbol_lookup_after_scope_drop() {
        let mut table = SymbolTable::new();

        table.insert(
            "outer_var",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "inner_var",
            Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(8), false, 1),
        ).unwrap();

        assert!(table.get("outer_var").is_some());
        assert!(table.get("inner_var").is_some());

        table.pop_scope().unwrap();
        assert!(table.get("outer_var").is_some());
        assert!(table.get("inner_var").is_none());
    }

    #[test]
    fn test_empty_symbol_name() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);

        let result = table.insert("", symbol);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Invalid identifier '' at line 1"));
    }

    #[test]
    fn test_deeper_nested_scopes() {
        let mut table = SymbolTable::new();

        table.insert(
            "main_proc",
            Symbol::new(SymbolType::Procedure, 1, SymbolLocation::GlobalLabel("main_proc".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "var_a",
            Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(4), false, 1),
        ).unwrap();

        table.push_scope();
        table.insert(
            "var_b",
            Symbol::new(SymbolType::Variable, 3, SymbolLocation::StackOffset(8), false, 2),
        ).unwrap();

        assert!(table.get("var_a").is_some());
        assert!(table.get("var_b").is_some());
        assert!(table.get("main_proc").is_some());

        let found_a = table.get("var_a").unwrap();
        assert_eq!(found_a.level, 1);
        let found_b = table.get("var_b").unwrap();
        assert_eq!(found_b.level, 2);

        table.pop_scope().unwrap();
        assert!(table.get("var_b").is_none());
        assert!(table.get("var_a").is_some());

        table.pop_scope().unwrap();
        assert_eq!(table.depth(), 1);
        assert!(table.get("var_a").is_none());
    }

    #[test]
    fn test_type_check_with_mismatch() {
        let mut table = SymbolTable::new();
        let var_symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);
        table.insert("my_var", var_symbol).unwrap();

        let result = table.type_check("my_var", &SymbolType::Constant(0), 10);
        assert!(result.is_err());
        let error_msg = result.unwrap_err();
        assert!(error_msg.to_string().contains("Type mismatch"));
    }

    #[test]
    fn test_shadowing_across_multiple_levels() {
        let mut table = SymbolTable::new();

        table.insert(
            "x",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("x".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "x",
            Symbol::new(SymbolType::Constant(100), 2, SymbolLocation::Immediate(100), false, 1),
        ).unwrap();

        let retrieved_x = table.get("x").unwrap();
        assert_eq!(retrieved_x.level, 1);
        assert_eq!(retrieved_x.line_number, 2);
        assert!(matches!(retrieved_x.symbol_type, SymbolType::Constant(_)));

        table.pop_scope().unwrap();
        let retrieved_x_1 = table.get("x").unwrap();
        assert_eq!(retrieved_x_1.level, 0);
        assert_eq!(retrieved_x_1.line_number, 1);
        assert!(matches!(retrieved_x_1.symbol_type, SymbolType::Variable));
    }

    #[test]
    fn test_get_after_scope_drop() {
        let mut table = SymbolTable::new();

        table.insert(
            "var_a",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("var_a".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "var_b",
            Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(8), false, 1),
        ).unwrap();

        assert!(table.get("var_a").is_some());
        assert!(table.get("var_b").is_some());

        table.pop_scope().unwrap();
        assert_eq!(table.depth(), 1);

        assert!(table.get("var_a").is_some());
        assert!(table.get("var_b").is_none());
    }

    #[test]
    fn test_power_procedure_lookup() {
        let mut table = SymbolTable::new();

        table.insert(
            "result",
            Symbol::new(SymbolType::Variable, 0, SymbolLocation::GlobalLabel("result".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "counter",
            Symbol::new(SymbolType::Variable, 0, SymbolLocation::StackOffset(24), false, 1),
        ).unwrap();

        let found_counter = table.get("counter").unwrap();
        assert_eq!(found_counter.level, 1);

        let found_result = table.get("result").unwrap();
        assert_eq!(found_result.level, 0);

        table.pop_scope().unwrap();
        assert!(table.get("counter").is_none());
        assert!(table.get("result").is_some());
    }

    #[test]
    fn test_initialized_state_management() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);
        table.insert("my_var", symbol).unwrap();

        let retrieved_sym = table.get("my_var").unwrap();
        assert!(!retrieved_sym.initialized, "Symbol should be uninitialized by default.");

        let mut_sym = table.get_mut("my_var").unwrap();
        mut_sym.initialized = true;

        let updated_sym = table.get("my_var").unwrap();
        assert!(updated_sym.initialized, "Symbol should now be initialized.");
    }

    #[test]
    fn test_get_mut_on_nonexistent_symbol() {
        let mut table = SymbolTable::new();
        let retrieved = table.get_mut("nonexistent");
        assert!(retrieved.is_none(), "get_mut should return None for a non-existent symbol.");
    }

    #[test]
    fn test_get_mut_respects_shadowing() {
        let mut table = SymbolTable::new();

        table.insert(
            "x",
            Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("x_global".to_string()), true, 0),
        ).unwrap();

        table.push_scope();
        table.insert(
            "x",
            Symbol::new(SymbolType::Constant(100), 2, SymbolLocation::Immediate(100), false, 1),
        ).unwrap();

        let mut_sym = table.get_mut("x").unwrap();
        assert_eq!(mut_sym.level, 1, "get_mut should return the shadowed symbol from the current scope.");

        assert!(matches!(mut_sym.symbol_type, SymbolType::Constant(_)));
    }

    #[test]
    fn test_pop_global_scope_is_rejected() {
        let mut table = SymbolTable::new();
        assert_eq!(table.depth(), 1);
        assert!(table.pop_scope().is_err(), "Global scope should not be poppable");
        assert_eq!(table.depth(), 1);
    }

    #[test]
    fn test_pl0_program_scoping() {
        let mut table = SymbolTable::new();

        // Global scope (Level: 0)
        table.insert(
            "global_var",
            Symbol::new(
                SymbolType::Variable,
                1,
                SymbolLocation::GlobalLabel("global_var".to_string()),
                true,  // is_global
                0,     // level (overridden by insert)
            ),
        ).unwrap();

        // Enter outer_proc (Level: 1)
        table.push_scope();
        table.insert(
            "outer_var",
            Symbol::new(
                SymbolType::Variable,
                2,
                SymbolLocation::StackOffset(4),
                false, // is_global
                1,     // level (overridden by insert)
            ),
        ).unwrap();

        // Enter inner_proc (Level: 2)
        table.push_scope();
        table.insert(
            "inner_var",
            Symbol::new(
                SymbolType::Variable,
                3,
                SymbolLocation::StackOffset(8),
                false, // is_global
                2,     // level (overridden by insert)
            ),
        ).unwrap();

        // Test in inner_proc (Level: 2, in_procedure = true)
        assert!(table.get("global_var").is_some(), "Should access global_var (Level: 0) in inner_proc");
        assert!(table.get("outer_var").is_some(), "Should access outer_var (Level: 1) in inner_proc");
        assert!(table.get("inner_var").is_some(), "Should access inner_var (Level: 2) in inner_proc");
        assert!(table.get("sibling_var").is_none(), "Should not access sibling_var in inner_proc");

        // Verify levels
        let global_var = table.get("global_var").unwrap();
        assert_eq!(global_var.level, 0, "global_var should be Level: 0");
        let outer_var = table.get("outer_var").unwrap();
        assert_eq!(outer_var.level, 1, "outer_var should be Level: 1");
        let inner_var = table.get("inner_var").unwrap();
        assert_eq!(inner_var.level, 2, "inner_var should be Level: 2");

        // Exit inner_proc, back to outer_proc (Level: 1, in_procedure = true)
        table.pop_scope().unwrap();
        assert!(table.get("global_var").is_some(), "Should access global_var in outer_proc");
        assert!(table.get("outer_var").is_some(), "Should access outer_var in outer_proc");
        assert!(table.get("inner_var").is_none(), "Should not access inner_var in outer_proc");
        assert!(table.get("sibling_var").is_none(), "Should not access sibling_var in outer_proc");

        // Exit outer_proc, back to main (Level: 0)
        table.pop_scope().unwrap();
        assert!(table.get("global_var").is_some(), "Should access global_var in main");
        assert!(table.get("outer_var").is_none(), "Should not access outer_var in main");
        assert!(table.get("inner_var").is_none(), "Should not access inner_var in main");
        assert!(table.get("sibling_var").is_none(), "Should not access sibling_var in main");

        // Enter sibling_proc (Level: 1)
        table.push_scope();
        table.insert(
            "sibling_var",
            Symbol::new(
                SymbolType::Variable,
                4,
                SymbolLocation::StackOffset(12),
                false, // is_global
                1,     // level (overridden by insert)
            ),
        ).unwrap();

        // Test in sibling_proc (Level: 1, in_procedure = true)
        assert!(table.get("global_var").is_some(), "Should access global_var in sibling_proc");
        assert!(table.get("sibling_var").is_some(), "Should access sibling_var in sibling_proc");
        assert!(table.get("outer_var").is_none(), "Should not access outer_var in sibling_proc");
        assert!(table.get("inner_var").is_none(), "Should not access inner_var in sibling_proc");

        // Verify levels
        let sibling_var = table.get("sibling_var").unwrap();
        assert_eq!(sibling_var.level, 1, "sibling_var should be Level: 1");
    }

    #[test]
    fn test_insert_with_special_characters_in_name() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);

        let result1 = table.insert("_underscore_var", symbol.clone());
        assert!(result1.is_ok(), "Underscore names should be valid");

        let result2 = table.insert("var123", symbol.clone());
        assert!(result2.is_ok(), "Names with numbers should be valid");

        // SymbolTable::insert only rejects empty names and duplicates (see
        // symboltable.rs); it has no identifier-syntax validation of its own.
        // Rejecting a digit-leading name like this is the lexer's job, since
        // "123invalid" would never lex as a single Ident token in the first
        // place - it lexes as Number(123) followed by Ident("invalid").
        let result3 = table.insert("123invalid", symbol.clone());
        assert!(result3.is_ok(), "SymbolTable::insert does not itself validate identifier syntax");
    }

    #[test]
    fn test_type_check_with_all_symbol_types() {
        let mut table = SymbolTable::new();

        // Test all SymbolType variants
        table.insert("var", Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0)).unwrap();
        table.insert("const", Symbol::new(SymbolType::Constant(42), 2, SymbolLocation::Immediate(42), false, 0)).unwrap();
        table.insert("proc", Symbol::new(SymbolType::Procedure, 3, SymbolLocation::GlobalLabel("proc".to_string()), false, 0)).unwrap();
        table.insert("id", Symbol::new(SymbolType::Identifier, 4, SymbolLocation::None, false, 0)).unwrap();
        table.insert("str_lit", Symbol::new(SymbolType::StringLiteral, 5, SymbolLocation::None, false, 0)).unwrap();
        table.insert("num_lit", Symbol::new(SymbolType::NumericLiteral, 6, SymbolLocation::None, false, 0)).unwrap();

        // Test correct type matches
        assert!(table.type_check("var", &SymbolType::Variable, 7).is_ok());
        assert!(table.type_check("const", &SymbolType::Constant(42), 7).is_ok());
        assert!(table.type_check("proc", &SymbolType::Procedure, 7).is_ok());
        assert!(table.type_check("id", &SymbolType::Identifier, 7).is_ok());
        assert!(table.type_check("str_lit", &SymbolType::StringLiteral, 7).is_ok());
        assert!(table.type_check("num_lit", &SymbolType::NumericLiteral, 7).is_ok());

        // Test type mismatches
        assert!(table.type_check("var", &SymbolType::Procedure, 8).is_err());
        assert!(table.type_check("const", &SymbolType::Variable, 8).is_err());
        assert!(table.type_check("proc", &SymbolType::Identifier, 8).is_err());
        assert!(table.type_check("var", &SymbolType::Constant(0), 8).is_err());
        assert!(table.type_check("id", &SymbolType::Variable, 8).is_err());
    }

    #[test]
    fn test_constant_value_matching() {
        let mut table = SymbolTable::new();

        table.insert("const1", Symbol::new(SymbolType::Constant(100), 1, SymbolLocation::Immediate(100), false, 0)).unwrap();
        table.insert("const2", Symbol::new(SymbolType::Constant(200), 2, SymbolLocation::Immediate(200), false, 0)).unwrap();

        // Test matching constant values
        assert!(table.type_check("const1", &SymbolType::Constant(100), 5).is_ok());
        assert!(table.type_check("const2", &SymbolType::Constant(200), 5).is_ok());

        // Test mismatched constant values
        assert!(table.type_check("const1", &SymbolType::Constant(200), 5).is_err());
        assert!(table.type_check("const2", &SymbolType::Constant(100), 5).is_err());
    }

    #[test]
    fn test_symbol_levels_after_multiple_scope_operations() {
        let mut table = SymbolTable::new();

        // Level 0
        table.insert("l0", Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("l0".to_string()), true, 0)).unwrap();

        // Level 1
        table.push_scope();
        table.insert("l1", Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(4), false, 1)).unwrap();

        // Level 2
        table.push_scope();
        table.insert("l2", Symbol::new(SymbolType::Variable, 3, SymbolLocation::StackOffset(8), false, 2)).unwrap();

        // Level 3
        table.push_scope();
        table.insert("l3", Symbol::new(SymbolType::Variable, 4, SymbolLocation::StackOffset(12), false, 3)).unwrap();

        // Verify all levels are correct
        assert_eq!(table.get("l0").unwrap().level, 0);
        assert_eq!(table.get("l1").unwrap().level, 1);
        assert_eq!(table.get("l2").unwrap().level, 2);
        assert_eq!(table.get("l3").unwrap().level, 3);

        // Drop to level 1
        table.pop_scope().unwrap(); // level 3 -> 2
        table.pop_scope().unwrap(); // level 2 -> 1

        assert!(table.get("l0").is_some());
        assert!(table.get("l1").is_some());
        assert!(table.get("l2").is_none());
        assert!(table.get("l3").is_none());
    }

    #[test]
    fn test_edge_case_symbol_names() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);

        // Test very long name
        let long_name = "a".repeat(1000);
        let result = table.insert(&long_name, symbol.clone());
        // This should work unless you have length limits
        assert!(result.is_ok());

        // Test single character name
        let result2 = table.insert("a", symbol.clone());
        assert!(result2.is_ok());

        // Test name with mixed case
        let result3 = table.insert("MixedCase", symbol.clone());
        assert!(result3.is_ok());
    }

    #[test]
    fn test_symbol_table_state_consistency() {
        let mut table = SymbolTable::new();

        // Verify initial state
        assert_eq!(table.depth(), 1);
        assert!(table.get("a").is_none());

        // Add some symbols and verify lookup
        table.insert("a", Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0)).unwrap();
        table.insert("b", Symbol::new(SymbolType::Constant(10), 2, SymbolLocation::Immediate(10), false, 0)).unwrap();

        assert!(table.get("a").is_some());
        assert!(table.get("b").is_some());

        // Push scope and add more
        table.push_scope();
        assert_eq!(table.depth(), 2);

        table.insert("c", Symbol::new(SymbolType::Procedure, 3, SymbolLocation::GlobalLabel("c".to_string()), false, 1)).unwrap();

        // Verify all are still accessible
        assert!(table.get("a").is_some());
        assert!(table.get("b").is_some());
        assert!(table.get("c").is_some());
    }

    #[test]
    fn test_symbol_redefinition_across_different_scopes() {
        let mut table = SymbolTable::new();

        // Define in global scope
        let result1 = table.insert("x", Symbol::new(SymbolType::Variable, 1, SymbolLocation::GlobalLabel("x".to_string()), true, 0));
        assert!(result1.is_ok());

        // Redefine in new scope (should be OK - shadowing)
        table.push_scope();
        let result2 = table.insert("x", Symbol::new(SymbolType::Constant(10), 2, SymbolLocation::Immediate(10), false, 1));
        assert!(result2.is_ok(), "Shadowing in different scope should be allowed");

        // Try to redefine in same scope (should fail)
        let result3 = table.insert("x", Symbol::new(SymbolType::Procedure, 3, SymbolLocation::GlobalLabel("x_proc".to_string()), false, 1));
        assert!(result3.is_err(), "Redefinition in same scope should fail");
    }

    #[test]
    fn test_get_mut_modifications_persist() {
        let mut table = SymbolTable::new();
        let symbol = Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0);
        table.insert("test", symbol).unwrap();

        // Modify through get_mut
        {
            let sym = table.get_mut("test").unwrap();
            sym.initialized = true;
            sym.line_number = 999;
        }

        // Verify modifications persist
        let sym = table.get("test").unwrap();
        assert!(sym.initialized);
        assert_eq!(sym.line_number, 999);

        // Modify again
        {
            let sym = table.get_mut("test").unwrap();
            sym.initialized = false;
        }

        // Verify second modification
        let sym = table.get("test").unwrap();
        assert!(!sym.initialized);
        assert_eq!(sym.line_number, 999); // Should still be 999
    }

    #[test]
    fn test_symbol_location_edge_cases() {
        let mut table = SymbolTable::new();

        // Test edge values for StackOffset
        table.insert("stack_zero", Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(0), false, 0)).unwrap();
        table.insert("stack_large", Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(usize::MAX), false, 0)).unwrap();

        // Test edge values for Immediate
        table.insert("imm_zero", Symbol::new(SymbolType::Constant(0), 3, SymbolLocation::Immediate(0), false, 0)).unwrap();
        table.insert("imm_negative", Symbol::new(SymbolType::Constant(-1), 4, SymbolLocation::Immediate(-1), false, 0)).unwrap();
        table.insert("imm_max", Symbol::new(SymbolType::Constant(i64::MAX), 5, SymbolLocation::Immediate(i64::MAX), false, 0)).unwrap();

        // Test empty GlobalLabel
        table.insert("empty_label", Symbol::new(SymbolType::Procedure, 6, SymbolLocation::GlobalLabel("".to_string()), false, 0)).unwrap();

        // Verify all can be retrieved
        assert!(table.get("stack_zero").is_some());
        assert!(table.get("stack_large").is_some());
        assert!(table.get("imm_zero").is_some());
        assert!(table.get("imm_negative").is_some());
        assert!(table.get("imm_max").is_some());
        assert!(table.get("empty_label").is_some());
    }

    #[test]
    fn test_type_check_error_messages() {
        let mut table = SymbolTable::new();
        table.insert("var", Symbol::new(SymbolType::Variable, 5, SymbolLocation::StackOffset(4), false, 0)).unwrap();

        // Test type mismatch error
        let result = table.type_check("var", &SymbolType::Procedure, 10);
        assert!(result.is_err());
        let error = result.unwrap_err();
        let error_str = error.to_string();
        assert!(error_str.contains("Type mismatch"), "Error message should mention type mismatch");
        assert!(error_str.contains("10"), "Error message should contain the line number");

        // Test undefined symbol error
        let result2 = table.type_check("nonexistent", &SymbolType::Variable, 15);
        assert!(result2.is_err());
        let error2 = result2.unwrap_err();
        let error_str2 = error2.to_string();
        assert!(error_str2.contains("Undefined") || error_str2.contains("undefined"), "Error should mention undefined symbol");
    }

    #[test]
    fn test_concurrent_scope_and_symbol_operations() {
        let mut table = SymbolTable::new();

        // Mix scope operations with symbol operations
        table.insert("a", Symbol::new(SymbolType::Variable, 1, SymbolLocation::StackOffset(4), false, 0)).unwrap();
        table.push_scope();
        table.insert("b", Symbol::new(SymbolType::Variable, 2, SymbolLocation::StackOffset(8), false, 1)).unwrap();
        table.push_scope();
        table.insert("c", Symbol::new(SymbolType::Variable, 3, SymbolLocation::StackOffset(12), false, 2)).unwrap();

        // Verify state
        assert_eq!(table.depth(), 3);

        // Mix pops with lookups
        assert!(table.get("a").is_some());
        table.pop_scope().unwrap();
        assert!(table.get("c").is_none());
        assert!(table.get("b").is_some());
        table.pop_scope().unwrap();
        assert!(table.get("b").is_none());
        assert!(table.get("a").is_some());

        assert_eq!(table.depth(), 1);
    }

    #[test]
    fn test_scope_balance() {
        let mut table = SymbolTable::new();

        // Push 3 scopes
        table.push_scope();
        table.push_scope();
        table.push_scope();
        assert_eq!(table.depth(), 4);

        // Pop 3 scopes
        table.pop_scope().unwrap();
        table.pop_scope().unwrap();
        table.pop_scope().unwrap();
        assert_eq!(table.depth(), 1);

        // Trying to pop one more should fail
        let result = table.pop_scope();
        assert!(result.is_err());
    }

    #[test]
    fn test_distance_to_is_zero_for_current_scope() {
        let mut table = SymbolTable::new();
        assert_eq!(table.distance_to(table.current_scope()), 0);

        let id = table.push_scope();
        assert_eq!(table.distance_to(id), 0);
    }

    #[test]
    fn test_distance_to_counts_hops_to_an_ancestor() {
        let mut table = SymbolTable::new();
        let level1 = table.push_scope();
        let level2 = table.push_scope();
        let level3 = table.push_scope();

        assert_eq!(table.distance_to(level3), 0);
        assert_eq!(table.distance_to(level2), 1);
        assert_eq!(table.distance_to(level1), 2);
        assert_eq!(table.distance_to(0), 3);
    }

    #[test]
    fn test_enter_scope_recomputes_distance_for_a_sibling_procedure() {
        let mut table = SymbolTable::new();
        // Two sibling procedures, each nested inside the other's enclosing
        // scope but not inside each other.
        let sibling_a = table.push_scope();
        table.pop_scope().unwrap();
        let sibling_b = table.push_scope();
        table.pop_scope().unwrap();

        table.enter_scope(sibling_a);
        assert_eq!(table.distance_to(0), 1);

        table.enter_scope(sibling_b);
        assert_eq!(table.distance_to(0), 1);
        assert_eq!(table.distance_to(sibling_a), table.depth(), "sibling_a is not an ancestor of sibling_b");
    }
}
