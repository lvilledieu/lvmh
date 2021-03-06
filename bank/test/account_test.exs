defmodule AccountTest do
  use ExUnit.Case

  test "initial balance is zero" do
    {:ok, pid} = Account.start_link

    assert 0 == Account.get_balance(pid)
  end

  test 'depositing money changes the balance' do
    {:ok, pid} = Account.start_link

    Account.deposit(pid, 10.0)
  end

end
