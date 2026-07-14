defmodule Nova.Int do
  @moduledoc "Transitional Data.Int support for Nova-generated Elixir."

  def from_string(value) when is_binary(value) do
    case Integer.parse(value) do
      {number, ""} -> {:just, number}
      _ -> :nothing
    end
  end
end
