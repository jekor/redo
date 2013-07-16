# Redo: A Simple Way to (Re)build Targets from Sources

Redo allows you to rebuild files from source files when they've changed. It's simpler than other build systems such as Make or SCons and more generic than language-specific build systems such as Cabal or Apache Ant.

Redo gains its power and simplicity by leveraging other tools (in the Unix tradition). Build scripts for redo are simply shell scripts that follow a few conventions.

## Usage

To build a target file, invoke redo with one or more target arguments.

`redo` *target* [*target*...]

There are no command-line options. Unlike Make, there is no default target for when you invoke `redo` with no arguments (it will just return silently after doing no work).

## What You Can Use Redo for

You can use `redo` any time you want to automatically build some file(s) from some other file(s). For example:

### Compiling Source Code

#### Compiling a .c File into a .o File

```
redo-ifchange $2.c
redo-ifchange $(./cc -MM $2.c | tr -d '\n\\' | cut -d':' -f2)
./cc -c $2.c -o $3
```

### Converting Markup into Documents

#### Generating an HTML Document from a Markdown Document

```
redo-ifchange $2.markdown
pandoc $2.markdown
```

## Advantages Over Make

 * More flexibility thanks to .do scripts being full shell scripts.
 * No need to learn Make's syntax or programming constructs.
 * No need to worry about incompatibilities between different versions of Make.
 * Better detection of actual changes through checksums instead of timestamps.
 * Modularity without a speed penalty since redo is recursive by design.

## Limitations

 * No supported method of building multiple targets with a single .do script.
 * No built-in method to depend on environment variables or the contents of directories changing.
 * The need to distribute a redo implementation if you're distributing a source archive.

Note that you can find a minimal implementation of redo written in bourne shell [here](https://raw.github.com/apenwarr/redo/blob/master/minimal/do). It's called "do" since it doesn't track changes to dependencies and always fully rebuilds the target(s), but it should be suitable for distributing with your source archive.

## How to Use Redo in Your Project

### .do Scripts

Redo builds a target file by looking for a corresponding .do script. For example, you might want to build the file `index.html` from `index.markdown`. When you invoke `redo index.html`, redo will first look for a script named `index.html.do`. If it fails to find that, it will then look for `default.html.do`. If it finds a .do script, it will invoke it with 3 arguments:

 * `$1`: unused (`0`) for backwards compatibility with older redo implementations
 * `$2`: the base name of the target (e.g. `index` for the target `index.html`)
 * `$3`: the temporary file that you should output the resulting target file contents to

Your script is not required to use any of these arguments, although you will usually want to use `$2`.

Scripts are executed by `sh` which you should assume is a standard Bourne Shell.

### Atomic Output

Anything output to standard output will automatically be appended to the temporary output file (`$3`). The `$3` argument is provided for programs that don't output to standard output or don't provide an option to do so (such as compilers that need to rewind file pointers, etc.).

A temporary file is used (rather than outputting directly to the target file) to ensure that the target file only appears if it has been fully and correctly rebuilt (instead of partially rebuilt due to some interruption).

For example, if you wanted to convert `index.markdown` to `index.html` using pandoc, in your `index.html.do` file, instead of writing:

    pandoc -o index.html index.markdown

you should write:

    pandoc -o $3 index.markdown

or, you could take advantage of the fact that pandoc outputs to stdout by default and write:

    pandoc index.markdown

and generalizing further with the `$2` argument:

    pandoc $2.markdown

### Specifying Dependencies

To specify that a target file depends on 1 or more source files, use the command `redo-ifchange` inside of your .do script. For example, to specify that `index.html` depends on the file `index.markdown`, in the `index.html.do` script you would write:

    redo-ifchange index.markdown

or more flexibly:

    redo-ifchange $2.markdown

Redo will track the checksums of any dependencies specified this way and only build the target file if at least 1 of its dependencies has changed or the target file is missing.

### Recursion

Redo is able to track dependencies recursively. For example, if you were not creating `index.markdown` directly but instead compiling it from some chapter files (such as `chapters/intro.markdown`, `chapters/body.markdown`, and `chapters/appendix.markdown`), you could create a new `index.markdown.do` file without making any changes to your `index.html.do` (or `default.html.do` file). Your new `index.markdown.do` file could look something like:

    redo-ifchange chapters/*.markdown
    cat chapters/*.markdown > $3

Note that since `redo-ifchange` is simply a command that will be executed by the shell, we can use `*` for filename globbing.

Redo will know when checking whether to rebuild `index.html` that it should also check that the dependencies of `index.markdown` are up-to-date as well.

Similarly, if you later wanted to build one of the chapter files from some other files, you just need to add an appropriate .do script (e.g. `chapters/body.markdown.do`).

### Defaults

Redo supports writing generic .do scripts based on file extensions. For example, if you had multiple Markdown files that you wanted to convert to HTML files, rather than write multiple .do scripts, you could instead take your existing `index.html.do` and rename it to `default.html.do`. Then, assuming that you'd written your .do script to take advantage of the `$2` argument, i.e.:

    redo-ifchange $2.markdown
    pandoc $2.markdown

`default.html.do` will be run by redo to build any target ending in `.html` (assuming that a more specific .do script doesn't exist).

## Stability

I consider redo to be stable but not feature complete.

Redo is used to build itself. However, it is not a very complicated program.

Most of redis's build system (~200 lines of Make) was converted to .do scripts as a test of redo's suitability.

Redo has only been tested on FreeBSD and Linux.

## Implementation Details

Redo is written in Haskell. It was written "on-camera" by Chris Forno (jekor) as part of a tutorial video series on real-world Haskell development. You can find the videos on [YouTube](http://www.youtube.com/watch?v=zZ_nI9E9g0I&list=PLxj9UAX4Em-Ij4TKwKvo-SLp-Zbv-hB4B).

## Credits

D. J. Bernstein conceived the idea behind redo and wrote some initial documentation at [http://cr.yp.to/redo.html](http://cr.yp.to/redo.html).

Alan Grosskurth wrote a thesis "Purely top-down software building" that includes an analysis and implementation of redo.

Mitchell Rosen, Göktuğ Kayaalp, and Rotten194 contributed suggestions on the tutorial videos during the process of writing the program. Many other viewers provided silent code review.
