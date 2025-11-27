defmodule Nova.Runtime do
  @moduledoc """
  Runtime support functions for Nova-generated Elixir code.
  Provides prelude functions and fix-point combinators.
  """

  # ─────────────────────────────────────────────────────────────
  # Fix-point combinators for recursive let bindings
  # ─────────────────────────────────────────────────────────────

  @doc "Fix-point combinator for 1-argument recursive functions"
  def fix(f) do
    fn arg -> f.(fix(f)).(arg) end
  end

  @doc "Fix-point combinator for 2-argument recursive functions"
  def fix2(f) do
    fn arg1, arg2 -> f.(fix2(f)).(arg1, arg2) end
  end

  @doc "Fix-point combinator for 3-argument recursive functions"
  def fix3(f) do
    fn arg1, arg2, arg3 -> f.(fix3(f)).(arg1, arg2, arg3) end
  end

  @doc "Fix-point combinator for 4-argument recursive functions"
  def fix4(f) do
    fn arg1, arg2, arg3, arg4 -> f.(fix4(f)).(arg1, arg2, arg3, arg4) end
  end

  @doc "Fix-point combinator for 5-argument recursive functions"
  def fix5(f) do
    fn arg1, arg2, arg3, arg4, arg5 -> f.(fix5(f)).(arg1, arg2, arg3, arg4, arg5) end
  end

  # ─────────────────────────────────────────────────────────────
  # Basic prelude functions
  # ─────────────────────────────────────────────────────────────

  @doc "Identity function"
  def identity(x), do: x

  @doc "Constant function - returns first argument, ignores second"
  def const(x), do: fn _ -> x end

  @doc "Function composition: compose(f, g)(x) = f(g(x))"
  def compose(f, g), do: fn x -> f.(g.(x)) end

  @doc "Boolean true constant"
  def otherwise, do: true

  # ─────────────────────────────────────────────────────────────
  # Show / conversion
  # ─────────────────────────────────────────────────────────────

  @doc "Convert value to string representation"
  def show(x) when is_binary(x), do: x
  def show(x) when is_integer(x), do: Integer.to_string(x)
  def show(x) when is_float(x), do: Float.to_string(x)
  def show(true), do: "true"
  def show(false), do: "false"
  def show(x) when is_atom(x), do: Atom.to_string(x)
  def show(x) when is_list(x), do: "[" <> Enum.map_join(x, ", ", &show/1) <> "]"
  def show({:just, v}), do: "Just " <> show(v)
  def show(:nothing), do: "Nothing"
  def show({:left, v}), do: "Left " <> show(v)
  def show({:right, v}), do: "Right " <> show(v)
  def show({:tuple, a, b}), do: "Tuple " <> show(a) <> " " <> show(b)
  def show(x) when is_tuple(x), do: inspect(x)
  def show(x) when is_map(x), do: inspect(x)
  def show(x), do: inspect(x)

  # ─────────────────────────────────────────────────────────────
  # Functor operations
  # ─────────────────────────────────────────────────────────────

  @doc "Map a function over a list"
  def map(f, xs) when is_list(xs), do: Enum.map(xs, f)
  def map(f, {:just, x}), do: {:just, f.(x)}
  def map(_f, :nothing), do: :nothing
  def map(f, {:right, x}), do: {:right, f.(x)}
  def map(_f, {:left, x}), do: {:left, x}

  # Curried version
  def map(f), do: fn xs -> map(f, xs) end

  # ─────────────────────────────────────────────────────────────
  # Fold operations
  # ─────────────────────────────────────────────────────────────

  @doc "Left fold"
  def foldl(f, acc, xs) when is_list(xs), do: List.foldl(xs, acc, fn x, a -> f.(a).(x) end)

  # Curried versions
  def foldl(f, acc), do: fn xs -> foldl(f, acc, xs) end
  def foldl(f), do: fn acc -> fn xs -> foldl(f, acc, xs) end end

  @doc "Right fold"
  def foldr(f, acc, xs) when is_list(xs), do: List.foldr(xs, acc, fn x, a -> f.(x).(a) end)

  # Curried versions
  def foldr(f, acc), do: fn xs -> foldr(f, acc, xs) end
  def foldr(f), do: fn acc -> fn xs -> foldr(f, acc, xs) end end

  @doc "Monadic fold"
  def foldM(f, acc, xs) when is_list(xs) do
    Enum.reduce_while(xs, {:right, acc}, fn x, {:right, a} ->
      case f.(a).(x) do
        {:right, new_acc} -> {:cont, {:right, new_acc}}
        {:left, err} -> {:halt, {:left, err}}
      end
    end)
  end

  # Curried versions
  def foldM(f, acc), do: fn xs -> foldM(f, acc, xs) end
  def foldM(f), do: fn acc -> fn xs -> foldM(f, acc, xs) end end

  # ─────────────────────────────────────────────────────────────
  # Filter
  # ─────────────────────────────────────────────────────────────

  @doc "Filter a list"
  def filter(f, xs) when is_list(xs), do: Enum.filter(xs, f)

  # Curried version
  def filter(f), do: fn xs -> filter(f, xs) end

  # ─────────────────────────────────────────────────────────────
  # String operations
  # ─────────────────────────────────────────────────────────────

  @doc "Join strings with separator"
  def intercalate(sep, xs) when is_binary(sep) and is_list(xs), do: Enum.join(xs, sep)

  # Curried version
  def intercalate(sep), do: fn xs -> intercalate(sep, xs) end

  # ─────────────────────────────────────────────────────────────
  # List operations
  # ─────────────────────────────────────────────────────────────

  @doc "Get length of list"
  def length(xs) when is_list(xs), do: Kernel.length(xs)
  def length(xs) when is_binary(xs), do: String.length(xs)

  @doc "Cons operator"
  def cons(h, t) when is_list(t), do: [h | t]
  def cons(h), do: fn t -> cons(h, t) end

  @doc "Zip two lists"
  def zip(xs, ys) when is_list(xs) and is_list(ys) do
    Enum.zip(xs, ys) |> Enum.map(fn {a, b} -> {:tuple, a, b} end)
  end

  def zip(xs), do: fn ys -> zip(xs, ys) end

  @doc "Create a tuple"
  def tuple(a, b), do: {:tuple, a, b}
  def tuple(a), do: fn b -> tuple(a, b) end

  # ─────────────────────────────────────────────────────────────
  # Maybe operations
  # ─────────────────────────────────────────────────────────────

  @doc "Create Just value"
  def just(x), do: {:just, x}

  @doc "Nothing value"
  def nothing, do: :nothing

  @doc "Extract from Maybe with default"
  def fromMaybe(default, {:just, x}), do: x
  def fromMaybe(default, :nothing), do: default
  def fromMaybe(default), do: fn m -> fromMaybe(default, m) end

  @doc "Apply function to Maybe"
  def maybe(default, f, {:just, x}), do: f.(x)
  def maybe(default, _f, :nothing), do: default
  def maybe(default, f), do: fn m -> maybe(default, f, m) end
  def maybe(default), do: fn f -> fn m -> maybe(default, f, m) end end

  @doc "Check if Maybe is Just"
  def isJust({:just, _}), do: true
  def isJust(:nothing), do: false

  @doc "Check if Maybe is Nothing"
  def isNothing(:nothing), do: true
  def isNothing({:just, _}), do: false

  # ─────────────────────────────────────────────────────────────
  # Either operations
  # ─────────────────────────────────────────────────────────────

  @doc "Create Left value"
  def left(x), do: {:left, x}

  @doc "Create Right value"
  def right(x), do: {:right, x}

  @doc "Apply functions to Either"
  def either(f, _g, {:left, x}), do: f.(x)
  def either(_f, g, {:right, x}), do: g.(x)
  def either(f, g), do: fn e -> either(f, g, e) end
  def either(f), do: fn g -> fn e -> either(f, g, e) end end

  # ─────────────────────────────────────────────────────────────
  # Applicative operations
  # ─────────────────────────────────────────────────────────────

  @doc "Wrap value in Right (for Either monad)"
  def pure(x), do: {:right, x}
end
