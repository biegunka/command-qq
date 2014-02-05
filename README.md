# command-qq
[![Hackage](https://budueba.com/hackage/command-qq)](https://hackage.haskell.org/package/command-qq)
[![Build Status](https://secure.travis-ci.org/biegunka/command-qq.png?branch=master)](https://travis-ci.org/biegunka/command-qq)
[![Build Status](https://drone.io/github.com/biegunka/command-qq/status.png)](https://drone.io/github.com/biegunka/command-qq/latest)

```
>>> import System.Command.QQ
>>> putStr =<< unlines . reverse . lines <$> [sh|cowsay "Hello, I am command-qq!"|]
                ||     ||
                ||----w |
            (__)\       )\/\
         \  (oo)\_______
        \   ^__^
 -------------------------
< Hello, I am command-qq! >
 _________________________
```

## Install

```
% cabal install command-qq
```

## Features

### Quasiquotation syntax for external interpreters

```
>>> [sh_| echo hello world! |]
hello world!
```

### Custom quasiquoters

```
ghci = quoter $ callCommand "ghc" ["-ignore-dot-ghci", "-e"]
```

Then you can use `ghci` in ghci!

```
>>> [ghci| putStrLn "hello world!" |] :: IO ()
hello world!
```

For more examples, see [`System.Command.QQ.Predef`][0]

### Haskell values embedding

Let's define `Embed` instance for a custom data type:

```haskell
data Bang = Bang

instance Embed Bang where
  embed Bang = "!"
```

Then you can use variables of `Bang` type in quoted strings!

```
>>> [sh_| echo hello#{Bang} |]
hello!
>>> let bang = Bang in [sh_| echo hello#{bang} |]
hello!
```

Note, `command-qq` does not support full Haskell in embeddings,
only variables/constructors names and literals

### DSLs

See [`examples/CommandT.hs`][1]

  [0]: https://github.com/biegunka/command-qq/blob/master/src/System/Command/QQ/Predef.hs
  [1]: https://github.com/biegunka/command-qq/blob/master/examples/CommandT.hs
