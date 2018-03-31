ocalisp
==

ocalisp is a tiny Lisp-1 implementation.
This is my hobby project to learn OCaml.

## Build and run

    $ omake
    $ ./ocalisp lispboot/examples/conways-gol.lisp

## Internal

The design of the implementation is heavily inspired by [SECD machine](https://en.wikipedia.org/wiki/SECD_machine).
For example, the semantics of each fields of [Vm.state](lib/vm.ml#L41) is almost same as SECD machine registers.

At [Vm.eval](lib/vm.ml#L400), every S-expression is macro-expanded and then compiled into [a code, a sequence of VM instructions](lib/vm.ml#L4).
After the compilation, code execution process is performed by running instruction cycle at [Vm.Exec.run](lib/vm.ml#L348).

### Examples of VM codes

#### ldc

```
> "literal"
[0 entry]
  ldc "literal"

> '(foo bar)
[0 entry]
  ldc (foo bar)
```

Expressions like literals or quoted S-expressions are compiled into `ldc`.

#### ldv

```
> hoge
[0 entry]
  ldv hoge
```

Unlike `ld` of SECD machine, the variable is determined dynamically by lookup on the current environment.

#### sel, leave

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

#### ldf, ldm

```
> (fun () 123)
[0 entry]
  ldf [1 fun ()]
[1 fun ()]
  ldc 123
  leave

> (macro () 456)
[0 entry]
  ldm [1 macro ()]
[1 macro ()]
  ldc 456
```

Notice that I merged SECD machine's `join` and `ret` into one instruction named `leave`.

#### app

```
> (compare a b)
[0 entry]
  ldv compare
  ldv a
  ldv b
  app 2

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

#### def, set

```
> (def x 123)
[0 entry]
  ldc 123
  def x
  ldc ()

> (set! x 123)
[0 entry]
  ldc 123
  set x
  ldc ()
```

