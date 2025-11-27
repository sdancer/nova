source = File.read!("purs/src/Nova/Compiler/Parser.purs")
tokens = Nova.Compiler.Tokenizer.tokenize(source)

# Try to parse just the module header
{:ok, header, rest1} = Nova.Compiler.Parser.parse_module_header(tokens)
IO.puts("Module header: #{inspect(header.name)}")
IO.puts("Remaining after header: #{length(rest1)}")

# Parse declarations manually and see how many succeed
defmodule TestParser do
  def parse_all(tokens, acc, limit \\ 100)
  def parse_all(_tokens, acc, 0), do: {Enum.reverse(acc), []}
  def parse_all(tokens, acc, limit) do
    tokens = Nova.Compiler.Parser.skip_newlines(tokens)
    case tokens do
      [] -> {Enum.reverse(acc), []}
      _ ->
        case Nova.Compiler.Parser.parse_declaration(tokens) do
          {:ok, decl, rest} ->
            name = cond do
              Map.has_key?(decl, :name) -> "#{decl.__struct__}: #{inspect(decl.name)}"
              true -> "#{decl.__struct__}"
            end
            IO.puts("Parsed: #{name}")
            parse_all(rest, [decl | acc], limit - 1)
          {:error, msg} ->
            IO.puts("Error: #{msg}")
            IO.puts("At tokens: #{inspect(Enum.take(tokens, 3))}")
            {Enum.reverse(acc), tokens}
        end
    end
  end
end

{decls, remaining} = TestParser.parse_all(rest1, [])
IO.puts("\nParsed #{length(decls)} declarations")
IO.puts("Remaining tokens: #{length(remaining)}")
if length(remaining) > 0 do
  IO.puts("First remaining token:")
  IO.inspect(hd(remaining))
end
