# ELT0

ELT0 is a general-purpose typed assembly language.

## Resources

- [Guide to understand the syntax of ELT0](https://github.com/elpinal/elt0/wiki/Syntax)
- [Roadmap](https://github.com/elpinal/elt0/issues/1)
- [Formal definition](https://github.com/elpinal/elt0/blob/master/doc/definition.pdf) (outdated)

## Install

```
$ stack install
```

## Code example

Write the following program to a file, say, `prod.elt0`.

```
main Code:
mov R1 21
mov R2 4
jmp prod

prod Code{
        R1: Int,
        R2: Int
}:
mov R3 0
jmp loop

loop Code{
        R1: Int,
        R2: Int,
        R3: Int
}:
if R1 done
add R3 R2 R3
sub R1 R1 1
jmp loop

done Code{
        R1: Int,
        R2: Int,
        R3: Int
}:
halt
```

```
$ elt0 build a.out prod.elt0
$ elt0 eval a.out
fromList [(1,0),(2,4),(3,84)]
```

`R3` is finally assigned 84 which is the product of 21 and 4.

## References

- Greg Morrisett. _Typed Assembly Language_. In
Benjamin C. Pierce, editor. _Advanced Topics in Types and Programming Languages_. Chapter 4. MIT Press. 2004.

- Greg Morrisett, David Walker, Karl Crary, and Neal Grew. _From System F to Typed Assembly Language_.
In the Twenty-Fifth ACM SIGPLAN-SIGACT Symposium on Principles of Programming Languages, pages 85-97, San Diego, CA, USA, January 1998. 

- Greg Morrisett, Karl Crary, Neal Glew, and David Walker. _Stack-Based Typed Assembly Language_.
In the 1998 Workshop on Types in Compilation, Kyoto, Japan, March 1998.
