defmodule Nova.Array do
  @moduledoc """
  Array operations for Nova-generated Elixir code.
  Maps PureScript's Data.Array to Elixir list operations.
  """

  # ─────────────────────────────────────────────────────────────
  # Basic operations
  # ─────────────────────────────────────────────────────────────

  @doc "Get length of array"
  def length(xs) when is_list(xs), do: Kernel.length(xs)

  @doc "Check if array is empty"
  def null(xs) when is_list(xs), do: xs == []

  @doc "Check if element is in array"
  def elem_(x, xs) when is_list(xs), do: x in xs
  def elem_(x), do: fn xs -> elem_(x, xs) end

  @doc "Get head of array"
  def head([]), do: :nothing
  def head([h | _]), do: {:just, h}

  @doc "Get tail of array"
  def tail([]), do: :nothing
  def tail([_ | t]), do: {:just, t}

  @doc "Get last element"
  def last([]), do: :nothing
  def last(xs), do: {:just, List.last(xs)}

  @doc "Get init (all but last)"
  def init([]), do: :nothing
  def init(xs), do: {:just, Enum.drop(xs, -1)}

  @doc "Get element at index"
  def index(xs, i) when is_list(xs) and is_integer(i) do
    if i >= 0 and i < Kernel.length(xs) do
      {:just, Enum.at(xs, i)}
    else
      :nothing
    end
  end

  def index(xs), do: fn i -> index(xs, i) end

  # ─────────────────────────────────────────────────────────────
  # Deconstruction
  # ─────────────────────────────────────────────────────────────

  @doc "Uncons - split head and tail"
  def uncons([]), do: :nothing
  def uncons([h | t]), do: {:just, {:tuple, h, t}}

  @doc "Unsnoc - split init and last"
  def unsnoc([]), do: :nothing
  def unsnoc(xs) do
    {init, [last]} = Enum.split(xs, -1)
    {:just, {:tuple, init, last}}
  end

  # ─────────────────────────────────────────────────────────────
  # Construction
  # ─────────────────────────────────────────────────────────────

  @doc "Cons - add element to front"
  def cons(x, xs) when is_list(xs), do: [x | xs]
  def cons(x), do: fn xs -> cons(x, xs) end

  @doc "Snoc - add element to end"
  def snoc(xs, x) when is_list(xs), do: xs ++ [x]
  def snoc(xs), do: fn x -> snoc(xs, x) end

  @doc "Insert at index"
  def insertAt(i, x, xs) when is_list(xs) and is_integer(i) do
    {before, after_} = Enum.split(xs, i)
    {:just, before ++ [x | after_]}
  end

  def insertAt(i, x), do: fn xs -> insertAt(i, x, xs) end
  def insertAt(i), do: fn x -> fn xs -> insertAt(i, x, xs) end end

  @doc "Delete at index"
  def deleteAt(i, xs) when is_list(xs) and is_integer(i) do
    if i >= 0 and i < Kernel.length(xs) do
      {:just, List.delete_at(xs, i)}
    else
      :nothing
    end
  end

  def deleteAt(i), do: fn xs -> deleteAt(i, xs) end

  @doc "Update at index"
  def updateAt(i, x, xs) when is_list(xs) and is_integer(i) do
    if i >= 0 and i < Kernel.length(xs) do
      {:just, List.replace_at(xs, i, x)}
    else
      :nothing
    end
  end

  def updateAt(i, x), do: fn xs -> updateAt(i, x, xs) end
  def updateAt(i), do: fn x -> fn xs -> updateAt(i, x, xs) end end

  @doc "Modify at index with function"
  def modifyAt(i, f, xs) when is_list(xs) and is_integer(i) do
    if i >= 0 and i < Kernel.length(xs) do
      {:just, List.update_at(xs, i, f)}
    else
      :nothing
    end
  end

  def modifyAt(i, f), do: fn xs -> modifyAt(i, f, xs) end
  def modifyAt(i), do: fn f -> fn xs -> modifyAt(i, f, xs) end end

  # ─────────────────────────────────────────────────────────────
  # Transformations
  # ─────────────────────────────────────────────────────────────

  @doc "Reverse array"
  def reverse(xs) when is_list(xs), do: Enum.reverse(xs)

  @doc "Concatenate two arrays"
  def concat(xs, ys) when is_list(xs) and is_list(ys), do: xs ++ ys
  def concat(xs), do: fn ys -> concat(xs, ys) end

  @doc "Filter array"
  def filter(f, xs) when is_list(xs), do: Enum.filter(xs, f)
  def filter(f), do: fn xs -> filter(f, xs) end

  @doc "Filter with index"
  def filterWithIndex(f, xs) when is_list(xs) do
    xs
    |> Enum.with_index()
    |> Enum.filter(fn {x, i} -> f.(x).(i) end)
    |> Enum.map(&elem(&1, 0))
  end

  def filterWithIndex(f), do: fn xs -> filterWithIndex(f, xs) end

  @doc "Partition array by predicate"
  def partition(f, xs) when is_list(xs) do
    {yes, no} = Enum.split_with(xs, f)
    %{yes: yes, no: no}
  end

  def partition(f), do: fn xs -> partition(f, xs) end

  @doc "Map over array"
  def map(f, xs) when is_list(xs), do: Enum.map(xs, f)
  def map(f), do: fn xs -> map(f, xs) end

  @doc "Map with index"
  def mapWithIndex(f, xs) when is_list(xs) do
    xs
    |> Enum.with_index()
    |> Enum.map(fn {x, i} -> f.(i).(x) end)
  end

  def mapWithIndex(f), do: fn xs -> mapWithIndex(f, xs) end

  @doc "Map and filter (mapMaybe)"
  def mapMaybe(f, xs) when is_list(xs) do
    xs
    |> Enum.map(f)
    |> Enum.filter(fn
      {:just, _} -> true
      :nothing -> false
    end)
    |> Enum.map(fn {:just, x} -> x end)
  end

  def mapMaybe(f), do: fn xs -> mapMaybe(f, xs) end

  @doc "Concatenate all nested arrays"
  def flatten(xss) when is_list(xss), do: List.flatten(xss)

  @doc "FlatMap / concatMap"
  def concatMap(f, xs) when is_list(xs), do: Enum.flat_map(xs, f)
  def concatMap(f), do: fn xs -> concatMap(f, xs) end

  # ─────────────────────────────────────────────────────────────
  # Sublists
  # ─────────────────────────────────────────────────────────────

  @doc "Take first n elements"
  def take(n, xs) when is_list(xs) and is_integer(n), do: Enum.take(xs, n)
  def take(n), do: fn xs -> take(n, xs) end

  @doc "Take elements while predicate holds"
  def takeWhile(f, xs) when is_list(xs), do: Enum.take_while(xs, f)
  def takeWhile(f), do: fn xs -> takeWhile(f, xs) end

  @doc "Take last n elements"
  def takeEnd(n, xs) when is_list(xs) and is_integer(n), do: Enum.take(xs, -n)
  def takeEnd(n), do: fn xs -> takeEnd(n, xs) end

  @doc "Drop first n elements"
  def drop(n, xs) when is_list(xs) and is_integer(n), do: Enum.drop(xs, n)
  def drop(n), do: fn xs -> drop(n, xs) end

  @doc "Drop elements while predicate holds"
  def dropWhile(f, xs) when is_list(xs), do: Enum.drop_while(xs, f)
  def dropWhile(f), do: fn xs -> dropWhile(f, xs) end

  @doc "Drop last n elements"
  def dropEnd(n, xs) when is_list(xs) and is_integer(n), do: Enum.drop(xs, -n)
  def dropEnd(n), do: fn xs -> dropEnd(n, xs) end

  @doc "Span - split at first element not satisfying predicate"
  def span(f, xs) when is_list(xs) do
    {init, rest} = Enum.split_while(xs, f)
    %{init: init, rest: rest}
  end

  def span(f), do: fn xs -> span(f, xs) end

  @doc "Split at index"
  def splitAt(n, xs) when is_list(xs) and is_integer(n) do
    {before, after_} = Enum.split(xs, n)
    {:tuple, before, after_}
  end

  def splitAt(n), do: fn xs -> splitAt(n, xs) end

  @doc "Slice from index to index"
  def slice(start, end_, xs) when is_list(xs) and is_integer(start) and is_integer(end_) do
    Enum.slice(xs, start, end_ - start)
  end

  def slice(start, end_), do: fn xs -> slice(start, end_, xs) end
  def slice(start), do: fn end_ -> fn xs -> slice(start, end_, xs) end end

  # ─────────────────────────────────────────────────────────────
  # Searching
  # ─────────────────────────────────────────────────────────────

  @doc "Find first element satisfying predicate"
  def find(f, xs) when is_list(xs) do
    case Enum.find(xs, f) do
      nil -> :nothing
      x -> {:just, x}
    end
  end

  def find(f), do: fn xs -> find(f, xs) end

  @doc "Find index of first element satisfying predicate"
  def findIndex(f, xs) when is_list(xs) do
    case Enum.find_index(xs, f) do
      nil -> :nothing
      i -> {:just, i}
    end
  end

  def findIndex(f), do: fn xs -> findIndex(f, xs) end

  @doc "Find index of element"
  def elemIndex(x, xs) when is_list(xs) do
    case Enum.find_index(xs, &(&1 == x)) do
      nil -> :nothing
      i -> {:just, i}
    end
  end

  def elemIndex(x), do: fn xs -> elemIndex(x, xs) end

  @doc "Check if any element satisfies predicate"
  def any(f, xs) when is_list(xs), do: Enum.any?(xs, f)
  def any(f), do: fn xs -> any(f, xs) end

  @doc "Check if all elements satisfy predicate"
  def all(f, xs) when is_list(xs), do: Enum.all?(xs, f)
  def all(f), do: fn xs -> all(f, xs) end

  # ─────────────────────────────────────────────────────────────
  # Folding
  # ─────────────────────────────────────────────────────────────

  @doc "Left fold"
  def foldl(f, acc, xs) when is_list(xs), do: List.foldl(xs, acc, fn x, a -> f.(a).(x) end)
  def foldl(f, acc), do: fn xs -> foldl(f, acc, xs) end
  def foldl(f), do: fn acc -> fn xs -> foldl(f, acc, xs) end end

  @doc "Right fold"
  def foldr(f, acc, xs) when is_list(xs), do: List.foldr(xs, acc, fn x, a -> f.(x).(a) end)
  def foldr(f, acc), do: fn xs -> foldr(f, acc, xs) end
  def foldr(f), do: fn acc -> fn xs -> foldr(f, acc, xs) end end

  # ─────────────────────────────────────────────────────────────
  # Zipping
  # ─────────────────────────────────────────────────────────────

  @doc "Zip two arrays into array of tuples"
  def zip(xs, ys) when is_list(xs) and is_list(ys) do
    Enum.zip(xs, ys) |> Enum.map(fn {a, b} -> {:tuple, a, b} end)
  end

  def zip(xs), do: fn ys -> zip(xs, ys) end

  @doc "Zip with function"
  def zipWith(f, xs, ys) when is_list(xs) and is_list(ys) do
    Enum.zip_with(xs, ys, fn a, b -> f.(a).(b) end)
  end

  def zipWith(f, xs), do: fn ys -> zipWith(f, xs, ys) end
  def zipWith(f), do: fn xs -> fn ys -> zipWith(f, xs, ys) end end

  @doc "Unzip array of tuples"
  def unzip(xys) when is_list(xys) do
    {as, bs} =
      Enum.reduce(xys, {[], []}, fn {:tuple, a, b}, {as, bs} ->
        {[a | as], [b | bs]}
      end)

    {:tuple, Enum.reverse(as), Enum.reverse(bs)}
  end

  # ─────────────────────────────────────────────────────────────
  # Sorting
  # ─────────────────────────────────────────────────────────────

  @doc "Sort array"
  def sort(xs) when is_list(xs), do: Enum.sort(xs)

  @doc "Sort array by function"
  def sortBy(f, xs) when is_list(xs), do: Enum.sort_by(xs, f)
  def sortBy(f), do: fn xs -> sortBy(f, xs) end

  @doc "Sort array with comparison function"
  def sortWith(cmp, xs) when is_list(xs) do
    Enum.sort(xs, fn a, b ->
      case cmp.(a).(b) do
        :lt -> true
        :eq -> true
        :gt -> false
      end
    end)
  end

  def sortWith(cmp), do: fn xs -> sortWith(cmp, xs) end

  # ─────────────────────────────────────────────────────────────
  # Set-like operations
  # ─────────────────────────────────────────────────────────────

  @doc "Remove duplicates"
  def nub(xs) when is_list(xs), do: Enum.uniq(xs)

  @doc "Remove duplicates by function"
  def nubBy(f, xs) when is_list(xs), do: Enum.uniq_by(xs, f)
  def nubBy(f), do: fn xs -> nubBy(f, xs) end

  @doc "Delete first occurrence of element"
  def delete(x, xs) when is_list(xs), do: List.delete(xs, x)
  def delete(x), do: fn xs -> delete(x, xs) end

  @doc "Array difference"
  def difference(xs, ys) when is_list(xs) and is_list(ys) do
    Enum.reject(xs, &(&1 in ys))
  end

  def difference(xs), do: fn ys -> difference(xs, ys) end

  @doc "Array intersection"
  def intersect(xs, ys) when is_list(xs) and is_list(ys) do
    Enum.filter(xs, &(&1 in ys))
  end

  def intersect(xs), do: fn ys -> intersect(xs, ys) end

  @doc "Array union"
  def union(xs, ys) when is_list(xs) and is_list(ys) do
    xs ++ Enum.reject(ys, &(&1 in xs))
  end

  def union(xs), do: fn ys -> union(xs, ys) end

  # ─────────────────────────────────────────────────────────────
  # Range / replicate
  # ─────────────────────────────────────────────────────────────

  @doc "Create array from range"
  def range(start, end_) when is_integer(start) and is_integer(end_) do
    Enum.to_list(start..end_)
  end

  def range(start), do: fn end_ -> range(start, end_) end

  @doc "Replicate element n times"
  def replicate(n, x) when is_integer(n), do: List.duplicate(x, n)
  def replicate(n), do: fn x -> replicate(n, x) end
end
