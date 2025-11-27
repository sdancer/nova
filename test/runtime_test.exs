# Runtime test - compiles a PureScript file through Nova and runs it in Elixir

defmodule RuntimeTest do
  @moduledoc """
  Tests that the Nova.Runtime, Nova.Array, Nova.String, etc. modules
  work correctly with Nova-generated Elixir code.
  """

  def run do
    IO.puts("=== Nova Runtime Integration Test ===\n")

    # Read the simple test file
    purs_path = "purs/test/runtime/SimpleRuntimeTest.purs"
    IO.puts("Reading: #{purs_path}")

    case File.read(purs_path) do
      {:ok, source} ->
        IO.puts("Tokenizing...")
        tokens = Nova.Compiler.Tokenizer.tokenize(source)
        IO.puts("  Generated #{length(tokens)} tokens")

        IO.puts("Parsing...")
        case Nova.Compiler.Parser.parse_module(tokens) do
          {:ok, mod, _rest} ->
            IO.puts("  Parsed module: #{inspect(mod.name)}")
            IO.puts("  Declarations: #{length(mod.declarations)}")

            # Print each declaration name
            for decl <- mod.declarations do
              case decl do
                {:function, %{name: name}} ->
                  IO.puts("    - #{name}")
                _ ->
                  :ok
              end
            end

            IO.puts("\nGenerating Elixir code...")
            # Use Nova's CodeGen
            elixir_code = Nova.Compiler.CodeGen.compile(mod)
            lines = String.split(elixir_code, "\n") |> length()
            IO.puts("  Generated #{lines} lines of Elixir")

            # Write to file for inspection
            out_path = "test/output/SimpleRuntimeTest.ex"
            File.mkdir_p!("test/output")
            File.write!(out_path, elixir_code)
            IO.puts("  Written to: #{out_path}")

            # Try to compile and run
            IO.puts("\nCompiling generated Elixir...")
            try do
              Code.compile_string(elixir_code)
              IO.puts("  Compilation successful!")

              IO.puts("\nRunning tests...")
              result = apply(Test.Runtime.SimpleRuntimeTest, :runTests, [])
              IO.puts("  Result: #{inspect(result)}")

              if result do
                IO.puts("\n✓ All tests passed!")
              else
                IO.puts("\n✗ Some tests failed")
              end
            rescue
              e ->
                IO.puts("  Error: #{inspect(e)}")
                IO.puts("\nGenerated code preview (first 50 lines):")
                elixir_code
                |> String.split("\n")
                |> Enum.take(50)
                |> Enum.with_index(1)
                |> Enum.each(fn {line, n} -> IO.puts("#{String.pad_leading(Integer.to_string(n), 3)}: #{line}") end)
            end

          {:error, err} ->
            IO.puts("Parse error: #{err}")
        end

      {:error, reason} ->
        IO.puts("Failed to read file: #{reason}")
    end

    IO.puts("\n=== Test Complete ===")
  end
end

RuntimeTest.run()
