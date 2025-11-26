# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Nova Lang is an AI-first coding environment and compiler written in Elixir. It implements a semantic development paradigm where code is represented as a versioned graph of functions and datatypes rather than text files. The system uses Hindley-Milner type inference and transpiles to Elixir.

## Build & Test Commands

```bash
mix deps.get              # Install dependencies
mix compile               # Compile the project
mix test                  # Run all tests
mix test test/parser.exs  # Run a specific test file
mix format                # Format code
mix format --check-formatted  # Check formatting without modifying
```

## Architecture

### Compilation Pipeline

```
Source → Tokenizer → Parser → NameResolver → TypeChecker → CodeGen → Elixir
```

- **Tokenizer** (`lib/tokenizer.ex`) - Lexical analysis with newline tracking
- **Parser** (`lib/parser.ex`) - Recursive descent parser producing AST nodes defined in `lib/ast.ex`
- **NameResolver** (`lib/name_resolver.ex`) - Validates type constructor references before type checking
- **TypeChecker** (`lib/typecheck.ex`) - Hindley-Milner Algorithm W implementation
- **Unify** (`lib/unify.ex`) - Type unification with occurs check
- **CodeGen** (`lib/codegen.ex`) - Transpiles typed AST to Elixir source

### Type System

Located in `lib/types.ex`:
- `TVar` - Type variables for polymorphism
- `TCon` - Type constructors (Int, String, Bool, List, etc.)
- `Scheme` - Quantified polymorphic types (∀)
- `Subst` - Type substitutions mapping TVar IDs to types

### InterfaceRegistry (`lib/interface_registry.ex`)

ETS-based layered type storage system:
- Immutable versioned layers with parent pointers
- Stores type schemes and AST nodes
- Supports transactional commit/discard semantics
- Hot path operations query only the "tip" layer for performance

### LLM Task Processing (`lib/task/`)

Built-in AI task queue:
- `manager.ex` - GenServer managing task queue and worker pool
- `worker.ex` - Individual workers processing LLM tasks
- `sup.ex` - Supervisor for the task system

### Workflow Steps (`lib/work_flow/`)

Multi-stage compilation with AI refinement:
- `step0.ex` - Parse function definitions, topological sort by dependencies
- `step1.ex`, `step2.ex`, `step3.ex` - Progressive compilation stages

### Application Entry (`lib/nova.ex`)

Starts three core processes:
1. DynamicSupervisor for domain processes
2. MCP Server on port 5000
3. HierarchicalFunctionManager

## Key Design Concepts

**Semantic Graph**: The codebase represents software as a layered graph where each layer is an immutable version. Operations on the "hot path" (code generation) query only the tip layer. Historical analysis traverses across layers.

**Semantic Integration**: Replaces traditional text-based merging. Orthogonal changes are auto-rebased; divergent architectural solutions are elevated for human review with comparative summaries.

## Test Organization

Tests use ExUnit in `test/`:
- `tokenizer_test.exs` - Lexical analysis
- `parser.exs`, `parser_extensive*.exs` - Syntax and AST
- `typecheck.exs` - Type inference
- `codegen*.exs` - Code generation
- `full_pipeline.exs` - End-to-end compilation
- `case_clauses.exs` - Pattern matching
- focus on compiling a purescript port of the existing files in order to bootstrap self compiling