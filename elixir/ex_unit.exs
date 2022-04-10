ExUnit.start

defmodule Tests do
  use ExUnit.Case

  test "something something" do
    data = %{bla: "bla"}
    assert %{bla: value} = data
    assert value = "bla"
  end

  describe "tests with shared setup" do
    @value "a value"

    setup do
      {:ok, data: %{my: @value}}
    end

    test "receives setup data", %{data: data} do
      assert data.my == "a value"
    end
  end

  describe "setup with tags" do

    setup %{option: option} do
      {:ok, option: option}
    end

    @tag option: "this is my option"
    test "can pass values to the setup block via tags", %{option: opt} do
      assert opt == "this is my option"
    end
  end
end
