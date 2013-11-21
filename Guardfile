require 'guard/haskell'

notification :tmux

guard :haskell, all_on_start: true, all_on_pass: true, top_spec: "tests/Spec.hs",
  ghci_options: ["-ignore-dot-ghci"] do
  watch(%r{tests/.+Spec.l?hs$})
  watch(%r{src/.+.l?hs$})
end
