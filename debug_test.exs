source = File.read!("purs/src/Nova/Compiler/Parser.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)

# Find where parseForallType starts (line 271)
tokens = Enum.drop_while(tokens, fn t ->
  not (t.type == :identifier && t.value == "parseForallType" && t.line == 271)
end)
IO.puts("Starting at: #{inspect(hd(tokens))}")

# Step through parse_function_with_type_signature manually
name = "parseForallType"
rest1 = tl(tokens)
{:ok, _, rest2} = Nova.Compiler.Parser.expect_operator(rest1, "::")
IO.puts("After ::: #{inspect(hd(rest2))}")

{type_tokens, rest3} = Nova.Compiler.Parser.split_type_and_rest(rest2, name)
IO.puts("Type tokens count: #{length(type_tokens)}")
IO.puts("Rest3 (start of function def): #{inspect(hd(rest3))}")

{:ok, type_ast, []} = Nova.Compiler.Parser.parse_type(Nova.Compiler.Parser.strip_newlines(type_tokens))
IO.puts("Type parsed OK")

# Now call parse_function_declaration
{:ok, fun_ast, final} = Nova.Compiler.Parser.parse_function_declaration(rest3)
IO.puts("Function parsed OK: #{fun_ast.name}")
IO.puts("Final rest: #{inspect(Enum.take(final, 5) |> Enum.map(fn t -> {t.line, t.column, t.value} end))}")
