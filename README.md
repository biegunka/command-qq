# shell-qq
[![Build Status](https://secure.travis-ci.org/biegunka/shell-qq.png?branch=master)](http://travis-ci.org/biegunka/shell-qq)

```
>>> putStrLn =<< unlines . reverse . lines <$> [sh|cowsay "Hello, I am shell-qq!"|]
                ||     ||
                ||----w |
            (__)\       )\/\
         \  (oo)\_______
        \   ^__^
 -----------------------
< Hello, I am shell-qq! >
 _______________________
```
