defmodule Test.Runtime.SimpleRuntimeTest do
  def runTests() do
    ((Nova.Array.take(2, [1, 2, 3, 4]) == [1, 2]) && ((Nova.Array.drop(2, [1, 2, 3, 4]) == [3, 4]) && ((Nova.Array.reverse([1, 2, 3]) == [3, 2, 1]) && ((Nova.Array.concat([1, 2], [3, 4]) == [1, 2, 3, 4]) && ((Nova.Array.length([1, 2, 3, 4, 5]) == 5) && ((Nova.String.length("hello") == 5) && ((Nova.String.take(3, "hello") == "hel") && ((Nova.String.drop(2, "hello") == "llo") && ((true && true) && ((false || true) && not(false)))))))))))
  end
end