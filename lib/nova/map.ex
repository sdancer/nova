defmodule Nova.Map do
  @moduledoc """
  Map operations for Nova-generated Elixir code.
  Maps PureScript's Data.Map to Elixir map operations.
  """

  # ─────────────────────────────────────────────────────────────
  # Construction
  # ─────────────────────────────────────────────────────────────

  @doc "Empty map"
  def empty, do: %{}

  @doc "Singleton map"
  def singleton(k, v), do: %{k => v}
  def singleton(k), do: fn v -> singleton(k, v) end

  @doc "Create map from list of tuples"
  def fromFoldable(xs) when is_list(xs) do
    Enum.reduce(xs, %{}, fn
      {:tuple, k, v}, acc -> Map.put(acc, k, v)
      {k, v}, acc -> Map.put(acc, k, v)
    end)
  end

  # ─────────────────────────────────────────────────────────────
  # Basic operations
  # ─────────────────────────────────────────────────────────────

  @doc "Check if map is empty"
  def isEmpty(m) when is_map(m), do: map_size(m) == 0

  @doc "Get size of map"
  def size(m) when is_map(m), do: map_size(m)

  @doc "Check if key is in map"
  def member(k, m) when is_map(m), do: Map.has_key?(m, k)
  def member(k), do: fn m -> member(k, m) end

  @doc "Lookup value by key"
  def lookup(k, m) when is_map(m) do
    case Map.fetch(m, k) do
      {:ok, v} -> {:just, v}
      :error -> :nothing
    end
  end

  def lookup(k), do: fn m -> lookup(k, m) end

  @doc "Lookup with default"
  def findWithDefault(default, k, m) when is_map(m) do
    Map.get(m, k, default)
  end

  def findWithDefault(default, k), do: fn m -> findWithDefault(default, k, m) end
  def findWithDefault(default), do: fn k -> fn m -> findWithDefault(default, k, m) end end

  # ─────────────────────────────────────────────────────────────
  # Insertion
  # ─────────────────────────────────────────────────────────────

  @doc "Insert key-value pair"
  def insert(k, v, m) when is_map(m), do: Map.put(m, k, v)
  def insert(k, v), do: fn m -> insert(k, v, m) end
  def insert(k), do: fn v -> fn m -> insert(k, v, m) end end

  @doc "Insert with function (if key exists, apply function to old and new value)"
  def insertWith(f, k, v, m) when is_map(m) do
    case Map.fetch(m, k) do
      {:ok, old_v} -> Map.put(m, k, f.(v).(old_v))
      :error -> Map.put(m, k, v)
    end
  end

  def insertWith(f, k, v), do: fn m -> insertWith(f, k, v, m) end
  def insertWith(f, k), do: fn v -> fn m -> insertWith(f, k, v, m) end end
  def insertWith(f), do: fn k -> fn v -> fn m -> insertWith(f, k, v, m) end end end

  # ─────────────────────────────────────────────────────────────
  # Deletion
  # ─────────────────────────────────────────────────────────────

  @doc "Delete key from map"
  def delete(k, m) when is_map(m), do: Map.delete(m, k)
  def delete(k), do: fn m -> delete(k, m) end

  # ─────────────────────────────────────────────────────────────
  # Update
  # ─────────────────────────────────────────────────────────────

  @doc "Update value at key with function"
  def update(f, k, m) when is_map(m) do
    case Map.fetch(m, k) do
      {:ok, v} ->
        case f.(v) do
          {:just, new_v} -> Map.put(m, k, new_v)
          :nothing -> Map.delete(m, k)
        end

      :error ->
        m
    end
  end

  def update(f, k), do: fn m -> update(f, k, m) end
  def update(f), do: fn k -> fn m -> update(f, k, m) end end

  @doc "Alter value at key (can insert, update, or delete)"
  def alter(f, k, m) when is_map(m) do
    current =
      case Map.fetch(m, k) do
        {:ok, v} -> {:just, v}
        :error -> :nothing
      end

    case f.(current) do
      {:just, v} -> Map.put(m, k, v)
      :nothing -> Map.delete(m, k)
    end
  end

  def alter(f, k), do: fn m -> alter(f, k, m) end
  def alter(f), do: fn k -> fn m -> alter(f, k, m) end end

  # ─────────────────────────────────────────────────────────────
  # Combine
  # ─────────────────────────────────────────────────────────────

  @doc "Union of two maps (left-biased)"
  def union(m1, m2) when is_map(m1) and is_map(m2), do: Map.merge(m2, m1)
  def union(m1), do: fn m2 -> union(m1, m2) end

  @doc "Union with function to combine values"
  def unionWith(f, m1, m2) when is_map(m1) and is_map(m2) do
    Map.merge(m1, m2, fn _k, v1, v2 -> f.(v1).(v2) end)
  end

  def unionWith(f, m1), do: fn m2 -> unionWith(f, m1, m2) end
  def unionWith(f), do: fn m1 -> fn m2 -> unionWith(f, m1, m2) end end

  @doc "Intersection of two maps"
  def intersection(m1, m2) when is_map(m1) and is_map(m2) do
    Map.take(m1, Map.keys(m2))
  end

  def intersection(m1), do: fn m2 -> intersection(m1, m2) end

  @doc "Difference of two maps (keys in m1 but not in m2)"
  def difference(m1, m2) when is_map(m1) and is_map(m2) do
    Map.drop(m1, Map.keys(m2))
  end

  def difference(m1), do: fn m2 -> difference(m1, m2) end

  # ─────────────────────────────────────────────────────────────
  # Traversal
  # ─────────────────────────────────────────────────────────────

  @doc "Map over values"
  def map(f, m) when is_map(m), do: Map.new(m, fn {k, v} -> {k, f.(v)} end)
  def map(f), do: fn m -> map(f, m) end

  @doc "Map over key-value pairs"
  def mapWithKey(f, m) when is_map(m), do: Map.new(m, fn {k, v} -> {k, f.(k).(v)} end)
  def mapWithKey(f), do: fn m -> mapWithKey(f, m) end

  @doc "Filter map by predicate on values"
  def filter(f, m) when is_map(m), do: Map.filter(m, fn {_k, v} -> f.(v) end)
  def filter(f), do: fn m -> filter(f, m) end

  @doc "Filter map by predicate on keys and values"
  def filterWithKey(f, m) when is_map(m), do: Map.filter(m, fn {k, v} -> f.(k).(v) end)
  def filterWithKey(f), do: fn m -> filterWithKey(f, m) end

  @doc "Fold over values"
  def foldl(f, acc, m) when is_map(m) do
    Enum.reduce(m, acc, fn {_k, v}, a -> f.(a).(v) end)
  end

  def foldl(f, acc), do: fn m -> foldl(f, acc, m) end
  def foldl(f), do: fn acc -> fn m -> foldl(f, acc, m) end end

  @doc "Fold over key-value pairs"
  def foldlWithKey(f, acc, m) when is_map(m) do
    Enum.reduce(m, acc, fn {k, v}, a -> f.(k).(v).(a) end)
  end

  def foldlWithKey(f, acc), do: fn m -> foldlWithKey(f, acc, m) end
  def foldlWithKey(f), do: fn acc -> fn m -> foldlWithKey(f, acc, m) end end

  # ─────────────────────────────────────────────────────────────
  # Conversion
  # ─────────────────────────────────────────────────────────────

  @doc "Get all keys as list"
  def keys(m) when is_map(m), do: Map.keys(m)

  @doc "Get all values as list"
  def values(m) when is_map(m), do: Map.values(m)

  @doc "Convert to list of tuples"
  def toUnfoldable(m) when is_map(m) do
    Enum.map(m, fn {k, v} -> {:tuple, k, v} end)
  end

  @doc "Get key-value pairs as list"
  def toAscList(m) when is_map(m) do
    m
    |> Enum.sort_by(fn {k, _v} -> k end)
    |> Enum.map(fn {k, v} -> {:tuple, k, v} end)
  end
end
