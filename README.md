# Luminary

![an animated luminary](./Luminary%20Logo.svg)

A toy compiler for Lua

## Currently Supported Lua

At this stage, the only supported Lua code is assignments, math operators, numbers, booleans, string
literals and calling the `print` function with 1 value (additional values will be ignored).

An example of what is supported would look like this:

```lua
-- example.lua
a = 1
print(a)
b = 2.5
print(b)
c = a + b
print(c)
print "hello world"
print(true)
print(false)
print(nil)
```

compiling and running this would look something like this:

```shell
luminary example.lua -o example
./example
1.000000
2.500000
3.500000
"hello world"
true
false
nil
```

## Dependencies

`clang` is used for linking the final output of an executable output.
