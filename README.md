# accompll

A tool performing AC-completion for left-linear TRSs written in Haskell. The used completion
technique does not have to deal with AC unification but only works if the resulting system is
left-linear.

The tool follows the approach of KBCV[^1] to implement completion with termination tools:
There are two threads where one prefers to orient equations from left to right and the other
from right to left. The result is taken from the thread which finishes first.

To compile, simply use `cabal install` which stores the executable in `$HOME/.cabal` on Unix systems
and`%APPDATA%\cabal` in Windows. With `cabal install --installdir=.` the executable is stored in the
current directory. However, it makes sense to add `$HOME/.cabal/bin` to your `$PATH` variable such
that the tool is globally available as `accompll`. 

Usage: `accompll [options] <file>`

The file given in the parameter  `<file>` has to be in the WST format[^2].
The option `--simp` or `-s` controls the
simplifaction of equations and right-hand sides of rules in the completion process. Use `-s R/AC`,
`-s R,AC` or `-s R` to simplify with the chosen rewrite relation whenever it is allowed during
the completion process. By default, the normal rewrite relation is used. If the flag `--pcp`
is given, only prime critical pairs are produced.

The option `--vp` allows you to provide an equation for solving validity problems
with respect to the equational theory defined in the TRS file. The output is either `YES`/`NO`
and an AC complete representation of the input or `MAYBE` if no complete representation could be
found. The input format is as follows: `--vp "[VAR x y z][s == t]"`. The variable names and terms
must not contain the symbols `[`, `]`,`=` or some white space.

With `--tt` you can specify the path to some executable which runs a termination tool.
For NaTT[^3] and TTT2[^4] respectively, a sample script is provided. If TTT2 and NaTT are accessible
as `ttt2` and `natt` on your system by putting the executables in some of the directories
listed in your `$PATH` variable, you do not have to change these scripts.
You can also set the timeout for each call of the
termination tool in these scripts. With `--ti` you can control in which format termination problems
are sent to the termination tool. The options are `NaTTXML`[^5] and `WST`. Make sure that your
termination tool understands this! The only other constraint for using other termination tools
is that any output which starts with something different from `YES`, `NO`, `MAYBE` or `TIMEOUT` is treated as an error. A new unofficial
version of TTT2 supports a new interactive mode which allows to start the process only once. It can
be activated by the flag `--interactive` or `-i`. Again, a sample script is provided which expects
this version of TTT2 to be accessible as `ttt2_custom`.

In order to implement this interactive mode in another termination
tool, just allow interactive communication in the sense that the tool expects a sequence of problems
seperated by `(RUN)` and gives `YES` or `NO` or `MAYBE` followed by a newline symbol as answer. Note
that also any error should be printed within one line due to technical simplicity of the
implementation.

Example usage with NaTT (default):

`accompll -s R --tt ./callNaTT.sh --ti NaTTXML <file>`

Example usage with TTT2:

`accompll -s R --tt ./callTTT2.sh --ti WST <file>`

Example usage with TTT2 (interactive mode):

`accompll -s R --tt ./callTTT2interactive.sh --ti WST -i <file>`

For help, type `./accompll -h`.

In the folder `examples/`, in addition to various examples in the WST format, you also find
infrastructure for doing experiments. The following snippet should give you an idea on how to use it:

```
cd examples
./run.rb <timeout> <script.sh>
./table.cgi > a.html # please press control-d
```

The script which was used to obtain the official experimental data is located at
`examples/run_experiments.sh`.

At the moment, the experimental data can be found [here](http://jniederhauser.at/accompll/a.html).

**Warning:** The implemented AC matching procedure gets "fresh" variables by naming them
`変数i` for some nonzero natural number `i`. Hence, please make sure that no variable in the input ES is named
like that. Note that 変数 (read: hensuu) is a Japanese word for *variable*.

[^1]: http://cl-informatik.uibk.ac.at/software/kbcv/
[^2]: https://www.lri.fr/~marche/tpdb/format.html
[^3]: https://www.trs.cm.is.nagoya-u.ac.jp/NaTT/
[^4]: http://cl-informatik.uibk.ac.at/software/ttt2/
[^5]: https://www.trs.cm.is.nagoya-u.ac.jp/NaTT/natt-xml.html