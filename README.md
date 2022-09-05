# ada-auth ARM

This is a Git mirror of
[Ada Conformity Assessment Authority](http://www.ada-auth.org/)
[CVS repository](http://www.ada-auth.org/cgi-bin/cvsweb.cgi/arm/).
It contains

 * Ada Reference Manual
 * ACATS User Guide
 * Source code of Ada formatting tool

See details about Ada formatting tool in [command.txt](progs/command.txt)

## Purpose

This fork adds two additional formatters to the [original project:](https://github.com/reznikmm/ada-auth)

1. Tracer - For understanding what is being emitted by the formatter program.
2. Ada_Lang_IO - For emitting the .md(x) files needed to put the ARM on https://ada-lang.io/

## Building

This is an Alire crate that you can build with:

```
alr build
```

## When updating the source docs

If you use case sensitive filesystem you need edit master file converting
file names to lowercase:

```
cd source
sed -i -e '/@Source/s/<[^>]*>/\L\0/' aa-aarm.msm
mkdir output
../.obj/ada_form aa-aarm.msm
```
