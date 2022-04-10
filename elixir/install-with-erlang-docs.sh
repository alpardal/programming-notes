
# need opt 23+ and elixir 1.11+

asdf update
asdf plugin-update --all
export KERL_BUILD_DOCS=yes
asdf list-all erlang
asdf install erlang 23.1.1
asdf global erlang 23.1.1
asdf list-all elixir
asdf install elixir 1.11.0-otp-23
asdf global  elixir 1.11.0-otp-23
elixir -v

# test it out:
iex
iex(1)> h :erlang.binary_to_term/1
