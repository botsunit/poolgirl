# File: Poolgirl.ex
# This file was generated from poolgirl.beam
# Using rebar3_elixir (https://github.com/botsunit/rebar3_elixir)
# MODIFY IT AT YOUR OWN RISK AND ONLY IF YOU KNOW WHAT YOU ARE DOING!
defmodule Poolgirl do
  def unquote(:"add_pool")(arg1, arg2) do
    :erlang.apply(:"poolgirl", :"add_pool", [arg1, arg2])
  end
  def unquote(:"add_pool")(arg1, arg2, arg3) do
    :erlang.apply(:"poolgirl", :"add_pool", [arg1, arg2, arg3])
  end
  def unquote(:"remove_pool")(arg1) do
    :erlang.apply(:"poolgirl", :"remove_pool", [arg1])
  end
  def unquote(:"remove_pools")(arg1) do
    :erlang.apply(:"poolgirl", :"remove_pools", [arg1])
  end
  def unquote(:"remove_all_pools")() do
    :erlang.apply(:"poolgirl", :"remove_all_pools", [])
  end
  def unquote(:"checkout")(arg1) do
    :erlang.apply(:"poolgirl", :"checkout", [arg1])
  end
  def unquote(:"checkin")(arg1) do
    :erlang.apply(:"poolgirl", :"checkin", [arg1])
  end
  def unquote(:"size")(arg1) do
    :erlang.apply(:"poolgirl", :"size", [arg1])
  end
  def unquote(:"pools")() do
    :erlang.apply(:"poolgirl", :"pools", [])
  end
  def unquote(:"assigned")(arg1) do
    :erlang.apply(:"poolgirl", :"assigned", [arg1])
  end
  def unquote(:"transaction")(arg1, arg2) do
    :erlang.apply(:"poolgirl", :"transaction", [arg1, arg2])
  end
end
