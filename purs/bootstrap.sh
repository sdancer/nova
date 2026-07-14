#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

npm exec --yes --package=purescript@0.15.15 --package=spago@0.21.0 -- \
  spago run -m Test.CodeGenElixir.CompileSelfTest

generated=(Ast Types Unify Tokenizer Parser TypeChecker CodeGen)
for module in "${generated[@]}"; do
  test -s "output/${module}.ex"
done

beam_dir=$(mktemp -d /tmp/nova-bootstrap.XXXXXX)
compile_log=$(mktemp /tmp/nova-bootstrap-log.XXXXXX)
trap 'rm -rf "$beam_dir" "$compile_log"' EXIT

if ! elixirc -o "$beam_dir" \
  ../lib/nova/runtime.ex \
  ../lib/nova/array.ex \
  ../lib/nova/map.ex \
  ../lib/nova/set.ex \
  ../lib/nova/string.ex \
  ../lib/nova/int.ex \
  ../lib/nova/number.ex \
  output/Ast.ex \
  output/Types.ex \
  output/Unify.ex \
  output/Tokenizer.ex \
  output/Parser.ex \
  output/TypeChecker.ex \
  output/CodeGen.ex >"$compile_log" 2>&1; then
  cat "$compile_log"
  exit 1
fi

elixir -pa "$beam_dir" -e '
source = "module Smoke where\nidentity x = x\n"
tokens = Nova.Compiler.Tokenizer.tokenize(source)

unless length(tokens) == 9 do
  raise "generated tokenizer returned #{length(tokens)} tokens, expected 9"
end

{:right, {:tuple, parsed, []}} = Nova.Compiler.Parser.parse_module(tokens)
{:right, _env} =
  Nova.Compiler.TypeChecker.check_module(
    Nova.Compiler.Types.empty_env(),
    parsed.declarations
  )

elixir_source = Nova.Compiler.CodeGen.gen_module(parsed)
Code.compile_string(elixir_source)

unless Smoke.identity(:bootstrap_ok) == :bootstrap_ok do
  raise "generated smoke module returned the wrong result"
end

IO.puts("Generated compiler tokenized, parsed, typechecked, emitted, compiled, and ran Smoke")

compiler_sources = [
  {"Unify", "src/Nova/Compiler/Unify.purs"},
  {"Types", "src/Nova/Compiler/Types.purs"},
  {"Ast", "src/Nova/Compiler/Ast.purs"},
  {"Tokenizer", "src/Nova/Compiler/Tokenizer.purs"},
  {"CodeGen", "src/Nova/Compiler/CodeGen.purs"},
  {"TypeChecker", "src/Nova/Compiler/TypeChecker.purs"},
  {"Parser", "src/Nova/Compiler/Parser.purs"}
]

Enum.each(compiler_sources, fn {name, path} ->
  source = File.read!(path)
  tokens = Nova.Compiler.Tokenizer.tokenize(source)
  {:right, {:tuple, parsed, []}} = Nova.Compiler.Parser.parse_module(tokens)
  regenerated = Nova.Compiler.CodeGen.gen_module(parsed)
  expected = File.read!("output/#{name}.ex")

  unless regenerated == expected do
    raise "#{name} is not at the self-hosted fixed point"
  end

  IO.puts("#{name}: self-hosted fixed point")
end)

IO.puts("Bootstrap gate passed")
'
