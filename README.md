<!-- cargo-rdme start -->

# `varlen`
Ergonomic variable-length types.

1. [Summary](#summary)
1. [Motivation](#motivation)
1. [Examples](#examples)
1. [Overview of types](#overview-of-types)
1. [Use of Pin](#use-of-pin)
1. [Feature flags](#feature-flags)

# Summary

`varlen` defines foundational types and traits for working with variable-length types in Rust.

The main example of variable-length type is a struct that stores a dynamically-sized array
directly in its storage, without requiring a pointer to a separate memory allocation. `varlen`
helps you define such types, and lets you build arbitrary concatenations and structs of them.
Additionally, it provides equivalents of the standard library's `Box<T>` and `Vec<T>` types
that are adapted to work well with variable-length types.

If you want to reduce the number of pointer indirections in your types by storing 
variable-sized arrays directly in your objects, then `varlen` is the library for you.

# Motivation

Traditionally when we use variable-sized data such as strings or arrays, we use a pointer
to a separately allocated object. For example, the following object ...

```rust
type Person = (/* age */ usize, /* name */ Box<str>, /* email */ Box<str>);
let person: Person = 
    (16, Box::from("Harry Potter"), Box::from("harry.potter@example.com"));
```

... is represented in memory like this, with three separately allocated objects:

```svgbob
"Person"
+------------+--------------------+-------------------+--------------------+-------------------+
| "16 (age)" | "0x1234 (str ptr)" | "12 (str length)" | "0x5678 (str ptr)" | "24 (str length)" |
+------------+--------------------+-------------------+--------------------+-------------------+
                |                                       |
                |                                       |
    .-----------'                         .-------------'
    |                                     |
    v "str payload"                       v "str payload"
    +------------------+                  +------------------------------+
    | "'Harry Potter'" |                  | "'harry.potter@example.com'" |
    +------------------+                  +------------------------------+
```

Sometimes we can reduce the number of object allocations by bringing the variable-length
storage directly into the parent object, perhaps with a memory layout like this:

```svgbob
+-----+
| ptr |
+-----+
  | 
  |
  |  "Person"
  |  +-------------+-------------------+------------------+-------------------+------------------------------+
  '->| "16 (age)"  | "12 (str length)" | "'Harry Potter'" | "24 (str length)" | "'harry.potter@example.com'" |
     +-------------+-------------------+------------------+-------------------+------------------------------+
```

This layout reduced the number of object allocations from 3 to 2, potentially improving
memory allocator performance, and potentially also improving [CPU cache locality](https://en.wikipedia.org/wiki/Locality_of_reference).
It also reduced the number of pointers from 3 to 2, saving memory.

The main disadvantage of this layout is that size and layout of the `Person` object is not
known at compile time; it is only known at runtime, when the lengths of the strings are known.
Working with such layouts in plain Rust is cumbersome, and also requires unsafe code to do
the necessary pointer arithmetic.

`varlen` lets you easily define and use such types, without you having to write any
unsafe code. The following code will create an object with the memory layout from above:

```rust
use varlen::prelude::*;
type Person = Tup3</* age */ FixedLen<usize>, /* name */ Str, /* email */ Str>;
let person: VBox<Person> = VBox::new(tup3::Init(
    FixedLen(16),
    Str::copy("Harry Potter"),
    Str::copy("harry.potter@example.com"),
));
```

# Examples

```rust
use varlen::prelude::*;

// Define a variable-length tuple:
type MyTuple = Tup3<FixedLen<usize>, Str, Array<u16>>;
let my_tuple: VBox<MyTuple> = VBox::new(tup3::Init(
    FixedLen(16), Str::copy("hello"), Array::copy(&[1u16, 2])));

// Put multiple objects in a sequence, with tightly packed memory layout:
let sequence: Seq<MyTuple> = seq![my_tuple.vcopy(), my_tuple.vcopy()];

// Or arena-allocate them, if the "bumpalo" crate feature is enabled:
let arena = bumpalo::Bump::new();
let arena_tuple: Owned<MyTuple> = Owned::new_in(my_tuple.vcopy(), &arena);

// Define a newtype wrapper for the tuple:
define_varlen_newtype! {
    #[repr(transparent)]
    pub struct MyStruct(MyTuple);

    with init: struct MyStructInit<_>(_);
    with inner_ref: fn inner(&self) -> &_;
    with inner_mut: fn inner_mut(self: _) -> _;
}
let my_struct: VBox<MyStruct> = VBox::new(MyStructInit(my_tuple));

// Define a variable-length struct via a procedural macro, if the "macro"
// crate feature is enabled.
#[define_varlen]
struct MyMacroStruct {
    age: usize,
    #[varlen]
    name: Str,
    #[varlen]
    email: Str,
    #[varlen]
    child: MyStruct,
}
let s: VBox<MyMacroStruct> = VBox::new(
    my_macro_struct::Init{
        age: 16,
        name: Str::copy("Harry Potter"),
        email: Str::copy("harry.potter@example.com"),
        child: my_struct,
    }
);

// #[define_varlen] also let you directly specify array lengths:
#[define_varlen]
struct MultipleArrays {
    #[controls_layout]
    len: usize,

    #[varlen_array]
    array1: [u16; *len],

    #[varlen_array]
    array2: [u8; *len],

    #[varlen_array]
    half_array: [u16; (*len) / 2],
}
let base_array = vec![0u16, 64000, 13, 105];
let a: VBox<MultipleArrays> = VBox::new(multiple_arrays::Init{
    len: base_array.len(),
    array1: FillSequentially(|i| base_array[i]),
    array2: FillSequentially(|i| base_array[base_array.len() - 1 - i] as u8),
    half_array: FillSequentially(|i| base_array[i * 2]),
});

```

# Overview of types

`varlen` provides variable-length versions of various standard-library types and traits.
This table gives the correspondence:


| Name                     | Fixed-length type `T` | Variable-length type `T`                      | Notes                                                                                   |
|--------------------------|-----------------------|-----------------------------------------------|-----------------------------------------------------------------------------------------|
| Immutable reference      | `&T`                  | `&T`                                          |                                                                                         |
| Mutable reference        | `&mut T`              | [`Pin<&mut T>`](std::pin::Pin)                | `Pin<>` required for safety, see below                                                  |
| Owning, non-allocated    | `T`                   | [`Owned<'storage, T>`](https://docs.rs/varlen/latest/varlen/owned/struct.Owned.html)   | `Owned<T>` is still a pointer to `T`'s payload                                          |
| Owning, allocated        | [`Box<T>`]            | [`VBox<T>`](https://docs.rs/varlen/latest/varlen/vbox/struct.VBox.html)                |                                                                                         |
| Sequence                 | [`Vec<T>`]            | [`Seq<T>`](https://docs.rs/varlen/latest/varlen/seq/struct.Seq.html)                   | `Seq` has tightly-packed _variable-size_ elements. Random access is somewhat restricted |
| String                   | [`String`]            | `Str`                      | String payload immediately follows the size, no pointer following                       |
| Array (fixed-size elems) | [`Box<[u16]>`]        | [`Array<u16>`](https://docs.rs/varlen/latest/varlen/array/struct.Array.html)           | Array payload immediately follows the size, no pointer following                        |
| Tuple                    | `(T, U)`              | `Tup2<T, U>`            | Field `U` might not be at a statically known offset from start of object                |
| Clone                    | `Clone::clone()`      | `VClone::vclone()`   |                                                                                         |
| Copy                     | <implicit>            | `VCopy::vcopy()`       |                                                                                         |


# Use of `Pin`

Mutable references to variable-length types use [`Pin<&mut T>`](std::pin::Pin) rather than
`&mut T`. By doing so, we prevent patterns such as calling [`std::mem::swap`] on
variable-length types. Such patterns would be a safety hazard, because the part of the type
that the Rust compiler knows about when calling [`std::mem::swap`] is just the
"fixed-size head" of the type. However, almost all variable-length types additionally have a
"variable-sized tail" that the Rust compiler doesn't know about. Swapping the head but not
the tail could violate a type's invariants, potentially breaking safety.

If you never write `unsafe` code, you don't need to worry about this issue. The only practical
consequence is that mutable access to a variable-length type is always mediated through
[`Pin<&mut T>`](std::pin::Pin) rather than `&mut T`, and you will have to work with the slightly
more cumbersome `Pin` APIs.

On the other hand, if you write `unsafe` code, you may have to be aware of the following
invariant. If `T` is a variable-length type, we require that any reference `&T` points to
a "valid" `T`, which we consider to be one which has a fixed-length head
(of size `std::mem::size_of::<T>()`) followed by a variable-length tail, and the head and
tail are "consistent" with each other. Here, "consistent" means that they were produced by
a call to one of the type's [`Initializer`] instances. In `unsafe` code, where you might
have access to a `&mut T` (without a `Pin`), you must avoid code patterns which modify the
head without also correspondingly modifying the tail.

# Feature flags

This crate has no *required* dependencies. The following feature flags exist, which can turn
on some dependencies.

* `bumpalo`. Enables support for allocating an [`Owned<T>`](https://docs.rs/varlen/latest/varlen/owned/struct.Owned.html) in an `bumpalo::Bump` arena. Adds a dependency on `bumpalo`.
* `macro`. Enables procedural macro support, for defining variable-length structs using `#[define_varlen]`. Adds a dependency on `varlen_macro`, `syn` and `quote`.
* `doc`. Enables pretty SVG diagrams in documentation. Adds a lot of dependencies.

<!-- cargo-rdme end -->
