defmodule Nova.Set do
  @moduledoc """
  Set operations for Nova-generated Elixir code.
  Maps PureScript's Data.Set to Elixir MapSet operations.
  """

  # ─────────────────────────────────────────────────────────────
  # Construction
  # ─────────────────────────────────────────────────────────────

  @doc "Empty set"
  def empty, do: MapSet.new()

  @doc "Singleton set"
  def singleton(x), do: MapSet.new([x])

  @doc "Create set from list"
  def fromFoldable(xs) when is_list(xs), do: MapSet.new(xs)

  # ─────────────────────────────────────────────────────────────
  # Basic operations
  # ─────────────────────────────────────────────────────────────

  @doc "Check if set is empty"
  def isEmpty(s), do: MapSet.size(s) == 0

  @doc "Get size of set"
  def size(s), do: MapSet.size(s)

  @doc "Check if element is in set"
  def member(x, s), do: MapSet.member?(s, x)
  def member(x), do: fn s -> member(x, s) end

  # ─────────────────────────────────────────────────────────────
  # Insertion / Deletion
  # ─────────────────────────────────────────────────────────────

  @doc "Insert element into set"
  def insert(x, s), do: MapSet.put(s, x)
  def insert(x), do: fn s -> insert(x, s) end

  @doc "Delete element from set"
  def delete(x, s), do: MapSet.delete(s, x)
  def delete(x), do: fn s -> delete(x, s) end

  # ─────────────────────────────────────────────────────────────
  # Combine
  # ─────────────────────────────────────────────────────────────

  @doc "Union of two sets"
  def union(s1, s2), do: MapSet.union(s1, s2)
  def union(s1), do: fn s2 -> union(s1, s2) end

  @doc "Intersection of two sets"
  def intersection(s1, s2), do: MapSet.intersection(s1, s2)
  def intersection(s1), do: fn s2 -> intersection(s1, s2) end

  @doc "Difference of two sets"
  def difference(s1, s2), do: MapSet.difference(s1, s2)
  def difference(s1), do: fn s2 -> difference(s1, s2) end

  # ─────────────────────────────────────────────────────────────
  # Predicates
  # ─────────────────────────────────────────────────────────────

  @doc "Check if s1 is subset of s2"
  def subset(s1, s2), do: MapSet.subset?(s1, s2)
  def subset(s1), do: fn s2 -> subset(s1, s2) end

  @doc "Check if s1 is proper subset of s2"
  def properSubset(s1, s2), do: MapSet.subset?(s1, s2) and MapSet.size(s1) < MapSet.size(s2)
  def properSubset(s1), do: fn s2 -> properSubset(s1, s2) end

  # ─────────────────────────────────────────────────────────────
  # Traversal
  # ─────────────────────────────────────────────────────────────

  @doc "Map over set"
  def map(f, s), do: MapSet.new(s, f)
  def map(f), do: fn s -> map(f, s) end

  @doc "Filter set"
  def filter(f, s), do: MapSet.filter(s, f)
  def filter(f), do: fn s -> filter(f, s) end

  @doc "Fold over set"
  def foldl(f, acc, s) do
    Enum.reduce(s, acc, fn x, a -> f.(a).(x) end)
  end

  def foldl(f, acc), do: fn s -> foldl(f, acc, s) end
  def foldl(f), do: fn acc -> fn s -> foldl(f, acc, s) end end

  @doc "Right fold over set"
  def foldr(f, acc, s) do
    s |> Enum.reverse() |> Enum.reduce(acc, fn x, a -> f.(x).(a) end)
  end

  def foldr(f, acc), do: fn s -> foldr(f, acc, s) end
  def foldr(f), do: fn acc -> fn s -> foldr(f, acc, s) end end

  # ─────────────────────────────────────────────────────────────
  # Conversion
  # ─────────────────────────────────────────────────────────────

  @doc "Convert set to list"
  def toUnfoldable(s), do: MapSet.to_list(s)

  @doc "Convert set to sorted list"
  def toAscList(s), do: s |> MapSet.to_list() |> Enum.sort()
end
