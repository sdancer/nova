defmodule Nova.String do
  @moduledoc """
  String operations for Nova-generated Elixir code.
  Maps PureScript's Data.String and Data.String.CodeUnits to Elixir string operations.
  """

  # ─────────────────────────────────────────────────────────────
  # Basic operations
  # ─────────────────────────────────────────────────────────────

  @doc "Get length of string"
  def length(s) when is_binary(s), do: String.length(s)

  @doc "Check if string is empty"
  def null(s) when is_binary(s), do: s == ""

  @doc "Singleton string from char"
  def singleton(c) when is_integer(c), do: <<c::utf8>>

  @doc "Convert char to code point"
  def toCodePointArray(s) when is_binary(s), do: String.to_charlist(s)

  @doc "Convert code points to string"
  def fromCodePointArray(xs) when is_list(xs), do: List.to_string(xs)

  # ─────────────────────────────────────────────────────────────
  # Substrings
  # ─────────────────────────────────────────────────────────────

  @doc "Take first n characters"
  def take(n, s) when is_integer(n) and is_binary(s), do: String.slice(s, 0, n)
  def take(n), do: fn s -> take(n, s) end

  @doc "Drop first n characters"
  def drop(n, s) when is_integer(n) and is_binary(s), do: String.slice(s, n..-1//1)
  def drop(n), do: fn s -> drop(n, s) end

  @doc "Take last n characters"
  def takeRight(n, s) when is_integer(n) and is_binary(s), do: String.slice(s, -n..-1//1)
  def takeRight(n), do: fn s -> takeRight(n, s) end

  @doc "Drop last n characters"
  def dropRight(n, s) when is_integer(n) and is_binary(s) do
    len = String.length(s)
    String.slice(s, 0, max(0, len - n))
  end

  def dropRight(n), do: fn s -> dropRight(n, s) end

  @doc "Take characters while predicate holds"
  def takeWhile(f, s) when is_binary(s) do
    s
    |> String.to_charlist()
    |> Enum.take_while(f)
    |> List.to_string()
  end

  def takeWhile(f), do: fn s -> takeWhile(f, s) end

  @doc "Drop characters while predicate holds"
  def dropWhile(f, s) when is_binary(s) do
    s
    |> String.to_charlist()
    |> Enum.drop_while(f)
    |> List.to_string()
  end

  def dropWhile(f), do: fn s -> dropWhile(f, s) end

  # ─────────────────────────────────────────────────────────────
  # Character access
  # ─────────────────────────────────────────────────────────────

  @doc "Get character at index"
  def charAt(i, s) when is_integer(i) and is_binary(s) do
    case String.at(s, i) do
      nil -> :nothing
      <<c::utf8>> -> {:just, c}
    end
  end

  def charAt(i), do: fn s -> charAt(i, s) end

  @doc "Get first character"
  def uncons(s) when is_binary(s) do
    case s do
      "" -> :nothing
      <<c::utf8, rest::binary>> -> {:just, {:tuple, c, rest}}
    end
  end

  # ─────────────────────────────────────────────────────────────
  # Searching
  # ─────────────────────────────────────────────────────────────

  @doc "Find index of substring"
  def indexOf(pattern, s) when is_binary(pattern) and is_binary(s) do
    case :binary.match(s, pattern) do
      {idx, _} -> {:just, idx}
      :nomatch -> :nothing
    end
  end

  def indexOf(pattern), do: fn s -> indexOf(pattern, s) end

  @doc "Find last index of substring"
  def lastIndexOf(pattern, s) when is_binary(pattern) and is_binary(s) do
    case :binary.matches(s, pattern) do
      [] -> :nothing
      matches -> {:just, elem(List.last(matches), 0)}
    end
  end

  def lastIndexOf(pattern), do: fn s -> lastIndexOf(pattern, s) end

  @doc "Check if string contains substring"
  def contains(pattern, s) when is_binary(pattern) and is_binary(s) do
    String.contains?(s, pattern)
  end

  def contains(pattern), do: fn s -> contains(pattern, s) end

  # ─────────────────────────────────────────────────────────────
  # Transformation
  # ─────────────────────────────────────────────────────────────

  @doc "Convert to lowercase"
  def toLower(s) when is_binary(s), do: String.downcase(s)

  @doc "Convert to uppercase"
  def toUpper(s) when is_binary(s), do: String.upcase(s)

  @doc "Trim whitespace from both ends"
  def trim(s) when is_binary(s), do: String.trim(s)

  @doc "Trim whitespace from start"
  def trimStart(s) when is_binary(s), do: String.trim_leading(s)

  @doc "Trim whitespace from end"
  def trimEnd(s) when is_binary(s), do: String.trim_trailing(s)

  @doc "Reverse string"
  def reverse(s) when is_binary(s), do: String.reverse(s)

  # ─────────────────────────────────────────────────────────────
  # Replacement
  # ─────────────────────────────────────────────────────────────

  @doc "Replace first occurrence"
  def replaceFirst(pattern, replacement, s)
      when is_binary(pattern) and is_binary(replacement) and is_binary(s) do
    String.replace(s, pattern, replacement, global: false)
  end

  def replaceFirst(pattern, replacement), do: fn s -> replaceFirst(pattern, replacement, s) end
  def replaceFirst(pattern), do: fn replacement -> fn s -> replaceFirst(pattern, replacement, s) end end

  @doc "Replace all occurrences"
  def replaceAll(pattern, replacement, s)
      when is_binary(pattern) and is_binary(replacement) and is_binary(s) do
    String.replace(s, pattern, replacement)
  end

  def replaceAll(pattern, replacement), do: fn s -> replaceAll(pattern, replacement, s) end
  def replaceAll(pattern), do: fn replacement -> fn s -> replaceAll(pattern, replacement, s) end end

  # ─────────────────────────────────────────────────────────────
  # Splitting and joining
  # ─────────────────────────────────────────────────────────────

  @doc "Split string by separator"
  def split(sep, s) when is_binary(sep) and is_binary(s), do: String.split(s, sep)
  def split(sep), do: fn s -> split(sep, s) end

  @doc "Join strings with separator"
  def joinWith(sep, xs) when is_binary(sep) and is_list(xs), do: Enum.join(xs, sep)
  def joinWith(sep), do: fn xs -> joinWith(sep, xs) end

  # ─────────────────────────────────────────────────────────────
  # Prefix/Suffix operations
  # ─────────────────────────────────────────────────────────────

  @doc "Strip prefix from string"
  def stripPrefix(prefix, s) when is_binary(prefix) and is_binary(s) do
    if String.starts_with?(s, prefix) do
      {:just, String.slice(s, String.length(prefix)..-1//1)}
    else
      :nothing
    end
  end

  def stripPrefix(prefix), do: fn s -> stripPrefix(prefix, s) end

  @doc "Strip suffix from string"
  def stripSuffix(suffix, s) when is_binary(suffix) and is_binary(s) do
    if String.ends_with?(s, suffix) do
      len = String.length(s) - String.length(suffix)
      {:just, String.slice(s, 0, len)}
    else
      :nothing
    end
  end

  def stripSuffix(suffix), do: fn s -> stripSuffix(suffix, s) end

  @doc "Check if string starts with prefix"
  def startsWith(prefix, s) when is_binary(prefix) and is_binary(s) do
    String.starts_with?(s, prefix)
  end

  def startsWith(prefix), do: fn s -> startsWith(prefix, s) end

  @doc "Check if string ends with suffix"
  def endsWith(suffix, s) when is_binary(suffix) and is_binary(s) do
    String.ends_with?(s, suffix)
  end

  def endsWith(suffix), do: fn s -> endsWith(suffix, s) end

  # ─────────────────────────────────────────────────────────────
  # CodeUnits aliases (for SCU/CU compatibility)
  # ─────────────────────────────────────────────────────────────

  @doc "Get character at index (CodeUnits style)"
  def charCodeAt(i, s), do: charAt(i, s)
  def charCodeAt(i), do: charAt(i)

  @doc "Slice string (CodeUnits style)"
  def slice(start, end_, s) when is_integer(start) and is_integer(end_) and is_binary(s) do
    String.slice(s, start, end_ - start)
  end

  def slice(start, end_), do: fn s -> slice(start, end_, s) end
  def slice(start), do: fn end_ -> fn s -> slice(start, end_, s) end end
end
