ocalisp
==

ocalisp is a tiny Lisp-1 implementation.
This is my hobby project to learn OCaml.

## Build

You can use either ocamlbuild or omake:

    $ ocamlbuild -use-menhir ocalisp.native
    $ omake

## Internal

The design of the implementation is heavily inspired by [SECD machine](https://en.wikipedia.org/wiki/SECD_machine).
For example, the semantics of [Vm.state](vm.ml#L39) is almost same as SECD machine registers.

At [Vm.eval](vm.ml#L342), every S-expression is compiled into [a code, a sequence of VM instructions](vm.ml#L4) after macro expansion.
After the compilation, code execution process is performed by running instruction cycle at [Vm.Exec.run](vm.ml#L304).

### Examples of VM codes

TODO Add descriptions

#### ldc

```
> "literal"
[0 entry]
  ldc "literal"
```

```
> '(foo bar)
[0 entry]
  ldc (foo bar)
```

#### ldv

```
> hoge
[0 entry]
  ldv hoge
```

#### sel and leave

```
> (if foo bar baz)
[0 entry]
  ldv foo
  sel [1 then] [2 else]
[1 then]
  ldv bar
  leave
[2 else]
  ldv baz
  leave
```

#### app

```
> (compare a b)
[0 entry]
  ldv compare
  ldv a
  ldv b
  app 2
```

```
(+ foo (* bar baz) qux)
[0 entry]
  ldv +
  ldv foo
  ldv *
  ldv bar
  ldv baz
  app 2
  ldv qux
  app 3
```

#### ldf and leave

```
> ((fun (x y) (println y)) "hoge" "fuga")
[0 entry]
  ldf [1 fun (x y)]
  ldc "hoge"
  ldc "fuga"
  app 2
[1 fun (x y)]
  ldv println
  ldv y
  app 1
  leave
```

#### pop

```
> (begin (print "A") (println))
[0 entry]
  ldv print
  ldc "A"
  app 1
  pop
  ldv println
  app 0
```

#### def

```
> (def x 123)
[0 entry]
  ldc 123
  def x
  ldc ()
```

