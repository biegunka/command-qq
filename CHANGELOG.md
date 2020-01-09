0.3.1.0
=======

  * Improved exception handling

  * Relaxed the licesnse to BSD2

  * Made compatible with GHC 8.8

0.3.0.0
=======

  * Added another way to avoid variable expansion (`\\#{foo}` is replaced with `#{foo}`)

0.2.2.0
=======

  * Exported `substituteVars`

0.2.1.0
=======

  * Added a bunch of predefined quasiquoters to `System.Command.QQ.Predef`

  * Added `Embed` instances for `Data.Text.Text` and `Data.Text.Lazy.Text`

0.2.0.0
=======

  * Added `sh_` quasiquoter to avoid type annotations for trivial quotes

  * Moved `Eval` onto `Text` to speed I/O up.

  * Added support for embedding literals and constructors with no arguments

  * Mored `Embed` instances
