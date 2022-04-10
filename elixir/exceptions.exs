defmodule MyAppError do
  defexception [:message]

  @impl true
  def exception(value) do
    msg = "did not get what was expected, got: #{inspect(value)}"
    %MyAppError{message: msg}
  end

  def test do
    try do
      raise __MODULE__, "some value"
    rescue
      MyAppError -> IO.puts("rescued")
    end
  end
end

MyAppError.test()
