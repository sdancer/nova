defmodule Nova.Number do
  @moduledoc "Transitional Data.Number support for Nova-generated Elixir."

  def from_string(value) when is_binary(value) do
    case Float.parse(value) do
      {number, ""} -> {:just, number}
      _ -> :nothing
    end
  end
end
