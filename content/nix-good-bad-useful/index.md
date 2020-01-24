+++
title = "Nix, The Good, The Bad, and The Useful"
outputs = ["Reveal"]
layout = "list"
+++

Hey Polyglots :)
================

---

Why I wanted to give this talk
------------------------------

---

### share the feelings

I want to enthuse and vent about a useful thing I\'m using and learning
about

I\'ve been using it on and off for 3 years, and I haven\'t gotten bored
yet

---

### new kind of intro

I want to try out a different approach to introduce Nix

Others don\'t focus enough on the underlying theorhetical basis of the
tech, imo

### new conversations

That\'s one of the best parts of Polyglots: our conversations

And you aren\'t able to come in on person, let\'s talk online!

Mastodon: @sehqlr@mastodon.technology

stl-tech.slack.com: @sehqlr

talks+nix\@samhatfield.me

what I\'ll be talking about
---------------------------

-   What is Nix?
-   Nix Store, the Good
-   Nix Language, the Bad
-   Nix build system, the Useful
-   Nix ecosystem
-   Some conversation starters

So, What **is** Nix?
====================

A \"new\" word
--------------

> The name Nix is derived from the Dutch word [niks]{.underline},
> meaning [nothing]{.underline}; build actions do not see anything that
> has not been explicitly declared as an input.
>
> -- \"Nix: A Safe and Policy-Free System for Software Deployment\",
> footnote 1

an FP package manager
---------------------

### packages are the immutable outputs of pure functions

$$ (\lambda s.\!p)(s) $$

-   $s$ is a specification of a package
-   $p$ is the resulting package

FP DSL
------

for specifing those packages

Writing code in this language is

SPOILER ALERT

the part I don\'t like

an entire ecosystem
-------------------

-   an operating system
-   DevOps tool(s)
-   declarative user configuration mgmt
-   and much more

MY OPINION
----------

a breath of fresh air for

-   building
-   configuring
-   packagng

software

What it isn\'t
--------------

### well documented

The manuals are alright, but there are a lot of bad examples in the
wider ecosystem ![](to-be-done.png)

This isn\'t unique to Nix, **BUT** I wish I could recommend this as a
tool, not an independent study

### a good DSL

SARCASM: Haskell and JSON had a baby, but can\'t agree on parenting
styles

BTW, if there are any good arguments for anything I\'m complaining
about, talk to me

Remember: opinions are like armpits. Everyone has one, and most of them
stink

### perfect

no software is perfect

I said this project was a breath of fresh air.

Well, fresh air has bad smells too

Why is it worth talking about?
------------------------------

### Unique approach

Nix has a unique approach to package management, with many useful
implications

1.  all install, upgrade, and rollback operations are atomic

2.  helps with difficult dependency management

3.  share reproducible build envs in source and binary forms

4.  sarcasm/ learn another DevOps system with terrible syntax /s

    no but really I like it

### builds upon existing Unix technologies

1.  default env is close to `build-essentials` in Debian

2.  lots of CLI tools (that are included in the slides)

3.  most CLI options are also configurable with env vars

Nix Store, the Good
===================

Motivation for learning the less good parts

The \"other\" package managers
------------------------------

The details here are very generalized, and for contrasting with Nix only

### Filesystem Hierarchy Standard

many different directories contain files for programs and configuration

1.  In Ubuntu, the Emacs executable is installed at `/usr/bin/emacs25`

    ![](ubuntu-emacs-package.png)

    ::: {.NOTES}
    <https://packages.ubuntu.com/bionic/amd64/emacs25/filelist>
    :::

2.  Emacs elisp packages are installed in `~/.emacs.d`

3.  Emacs config files are in `$HOME` or elsewhere

### in-place changes

1.  when emacs is updated, the data at `/usr/bin/emacs25` is overwritten

2.  if the update process crashes or is interrupted, it could be in
    partial state

    many mechanisms prevent this, but Nix does it differently, as we
    shall see

### MITM

In order to make sure your package hasn\'t been compromised on its way
to you by a MITM attack, you need to verify it with a cryptographic
hashing process

typically, you download the hash the server has, perform the hashing on
your computer, then compare the two hashes to make sure they match

Some package managers do this automatically, but some don\'t and you
have to do it by hand

### dependency management

dependency management is a large part of what the package manager does,
and when it goes wrong, your packages are broken

When the best standard you have is `semver` and Firefox doesn\'t use it,
you\'ve got a hard job on your hands

### CONCURRENT dep mgmt

wow, is that impossible? How do you manage this?

-   `chroot`?
-   sandboxes?
-   DOCKER!?

maybe this one reason is why every language has their package manager?

... and tools built on top? to manage concurrent versions?

Nix Does It Different
---------------------

how it compares to \"the others\"

``` {.bash org-language="sh" results="value file" file="dependencies.png" exports="both"}
nix-store -q --graph $(realpath $(which curl)) | dot -Tpng
```

::: {.NOTES}
The image for the Emacs dep graph was too big
:::

### CRYPTO ALL THE THINGS!

Nix hashes **all** packages it builds, everytime it builds them

the verification step and the build step are one and the same

changes to the package result in different hashes

every package has a unique ID based on its contents

MITM attacks are less likely

### /nix/store

all packages that are built by nix go into this directory

YES, this breaks FHS, but we\'ll return to that in a few slides

the directory template is `$hash-$name`, where hash is the hash from the
build

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
nix path-info nixpkgs.curl
```

``` {.example}
/nix/store/yb6s1k41s7sydr6q3nzmayhvbkzhydvf-curl-7.64.0-bin
```

### Dependencies...

are included within the package!

::: {.NOTES}
There\'s a \"wrapping\" mechanism for compatibility with Nix

In this case, each executable in this directory has a wrapped
counterpart
:::

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
find $(nix path-info nixpkgs.emacs) -executable -type f -not -name '*wrapped'
```

``` {.example}
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/libexec/emacs/26.1/x86_64-pc-linux-gnu/movemail
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/libexec/emacs/26.1/x86_64-pc-linux-gnu/rcs2log
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/libexec/emacs/26.1/x86_64-pc-linux-gnu/profile
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/libexec/emacs/26.1/x86_64-pc-linux-gnu/hexl
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/bin/emacsclient
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/bin/ctags
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/bin/ebrowse
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/bin/emacs-26.1
/nix/store/8j5qqfk1qnz1mjw0z72ih336dzkpkl8w-emacs-26.1/bin/etags
```

This means that dependency tracking happens during the build step

this mitigates the broken dependency problem because each package brings
their own

### install via symlink

installs are done with symlinks from outside the store

``` {.bash org-language="sh" cache="yes" exports="both" results="output"}
ls -l $(which curl)
```

``` {.example}
lrwxrwxrwx 1 root root 68 Dec 31  1969 /run/current-system/sw/bin/curl -> /nix/store/nakc4z4vz69sq2jjlakp64s04qgggvja-curl-7.64.1-bin/bin/curl
```

Because you can symlink anywhere, we can reestablish FHS via symlinks.
NixOS doesn\'t, but you could!

BTW, Nix resets all timestamps, that\'s why the file is so old

### updates... also via symlink

because the installation of a file is just a symlink to the store

updates are just changing the symlink of a package with a different hash

### different versions, different paths, same store

Because changing a package changes the hash, each different build
results in a different path

The Nix store keeps paths around as long as there is a reference to them

packages are immutable; only additions, no updates, which means

ALL VERSIONS ARE CONCURRENT VERSIONS

Intermission: DRV
-----------------

$$ (\lambda s.\!p)(s) $$

derivations: the unit of composition in nix

### it\'s a file!

surprise!

it\'s JSON

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
nix show-derivation nixpkgs.jq | nix-shell -p jq --run jq | head -n10
```

``` {.example}
{
  "/nix/store/7g5n296kyk2n11bki54cwpn2n27x597z-jq-1.6.drv": {
    "outputs": {
      "bin": {
        "path": "/nix/store/czdpwxms57dqqv4vixcx6pg6xl8lmsjv-jq-1.6-bin"
      },
      "dev": {
        "path": "/nix/store/zbrd5iga16dagl99bkk8y354rxvsrpqg-jq-1.6-dev"
      },
      "doc": {
```

::: {.NOTES}
I\'m creating a subshell that has jq in the env, running it, to
pretty-print the JSON of it\'s derivation. Are you not impressed??
:::

### derivation keys

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
nix show-derivation nixpkgs.jq | nix-shell -p jq --run "jq '.[] | keys'"
```

``` {.example}
[
  "args",
  "builder",
  "env",
  "inputDrvs",
  "inputSrcs",
  "outputs",
  "platform"
]
```

A derivation contains all the information that Nix needs to build a
package

Here is where dependency tracking happens: `inputDrvs`

``` {.bash org-language="sh" cache="yes" exports="both" results="output"}
nix show-derivation nixpkgs.jq | nix-shell -p jq --run "jq '.[].inputDrvs'"
```

``` {.example}
{
  "/nix/store/0si75icim8ajxcsp25d9c52m42kqg1xj-stdenv-linux.drv": [
    "out"
  ],
  "/nix/store/1kircip4wskspsqqzxbmh6ss73iqh9ah-bash-4.4-p23.drv": [
    "out"
  ],
  "/nix/store/4ss7qn4n2nrc0r98ly33hw7s3brvwgcb-jq-1.6.tar.gz.drv": [
    "out"
  ],
  "/nix/store/9gg731fwsxxrl2qmwb17aq4w5r8s5l76-onig-6.9.1.drv": [
    "out"
  ]
}
```

::: {.NOTES}
That `stdenv-linux.drv` is the `build-essentials`-alike I mentioned
earlier
:::

Further Implications
--------------------

If you thought your mind was blown with concurrent deps, we are just
getting started

### caching

the store is a cache for everything already, and not just for your
machine

in fact, you can serve your `/nix/store` as a binary cache for **other**
machines

### installs, updates, and rollbacks are atomic

because a symlink either points to another file or it doesn\'t, install
and update actions are never in a partial state.

AND, because previous packages are still available when you build a new
package, a rollback is **exactly** the same as an update

### nix env generations

Nix has this mechanism called *generations* where each iteration of an
update to an environment is tracked by entries in the store

This means that, in a basic sense, your package actions are version
controlled.

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
nix-env --list-generations | tail -n5
```

``` {.example}
 163   2019-04-28 14:49:06   
 164   2019-04-28 14:51:55   
 165   2019-04-28 17:32:00   
 166   2019-05-02 12:18:02   
 167   2019-05-02 15:04:45   (current)
```

And now you will hear my tale of woe... and hope

::: {.NOTES}
LAPTOP WIFI STORY

-   happened this month
-   changing config to make my setup behave like a nixos module
-   made a change that didn\'t include networkmanager
-   before, I\'d move the laptop to connect to the router via Ethernet
-   but this time, I realized that I could do a rollback
-   rebooted, selected the GRUB entry that I wanted, and BAM! Wifi works
-   that was it!
-   I fixed the config, tested this time, and was able to move forward
    without losing wifi again
:::

### per-user package management

because each user get\'s their own environment with their own
generations, each user can manage their own packages **without sudo**

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
ls -l ~/.nix-profile
```

``` {.example}
lrwxrwxrwx 1 sam users 42 Dec 13 20:06 /home/sam/.nix-profile -> /nix/var/nix/profiles/per-user/sam/profile
```

### nix-shell

Nix comes with a command that can load in a set of packages on demand
into a subshell

``` {.bash org-language="sh" cache="yes" results="output" exports="both"}
which ruby || echo "no ruby"
nix-shell -p ruby --run 'which ruby || echo "no ruby"'
```

``` {.example}
no ruby
/nix/store/rwp5fpzqssf5m9dzbgbwsfgdzw8xajra-ruby-2.5.5/bin/ruby
```

which means...

SANDBOXES FOR EVERYTHING, no containers required

Nix comes with tooling to manage this, specifically `nix-shell`

more on that later

### bad packages are isolated

1.  malicious

    No Trojan horses here!

2.  poorly written

    I can\'t screw up the store due to incompetence

    some mistakes become build errors instead of system crashes

### reproducability

[Is NixOS Reproducible?](https://r13y.com)

This is no ordinary caching, it\'s deterministic builds

### content-addressability

[Cachix - Nix binary cache hosting](https://cachix.org)

instead of computing a hash on a package as you build it, you can ask to
download a valid package with the computed hash.

### config-file management

any program and uses files for configuration can be configured with Nix

any file can be in the Store, and symlinks can put them wherever the
program expects a file to be.

This is how NixOS and home-manager work

### possibly more??

I\'m convinced that I\'ve missed some, and that there are even more to
be thought up

Nix Lang, the bad
=================

basic description
-----------------

### the common term for a program in this language is a \"Nix Expression\" or \"nixexpr\"

In many parts of the documentation, it\'s referred to as the \"Nix
Expression Language\"

I\'ll be using the \"nixexpr\" term for the remainder of the talk

### Implemented in C++

for portability and speed

Otherwise, it\'d probably be an EDSL in Haskell, which would have raised
other complaints

### no specifying document that I could find

-   no formal grammar
-   no language specification

Most of the research in this part is based on [the
manual](https://nixos.org/nix/manual/#ch-expression-language).

This does not include the functions that are in `nixpkgs`

which could be considered a stdlib for the lang

### dynamically typed

types are checked at runtime

``` {.bash org-language="sh" results="output" session="*nix repl*" cache="no" exports="both"}
1 + 1
"a" + 1
```

``` {.example}
1 + 1
2
"a" + 1
error: cannot coerce an integer to a string, at (string):1:1
echo 'org_babel_sh_eoe'
```

::: {.NOTES}
I\'m using an org-babel session to evaluation the nix code since
spacemacs doesn\'t have a nix repl feature that I\'m aware of, so I
guess the echo part is with us for the remainder of this trip
:::

### lazy evaluated

lazy eval is like putting stuff in an online shopping cart.

You could have something in there for months, but until you actually pay
for it, it won\'t show up at your door.

### purely functional

$$ (\lambda s.\!p)(s) $$

no side-effects, only inputs and outputs, and outputs are completely
dependent on inputs. Same inputs, same outputs

### domain-specific

nixexprs are ultimately for building packages, so the design is
constrained on purpose.

The unsurprising stuff
----------------------

These parts of the language very much look like either Haskell or
Javascript, so I\'m going to group them at the beginning, and get to the
surprising stuff later

### Bitwise operations

1.  bitAnd

2.  bitOr

3.  bitXor

4.  no shifts

### Numbers

1.  ints: `-10000`

2.  floating point: `123.5467e9`

### numerical operations

1.  arithmetic has an operator and function each

    -   `+`; `add`
    -   `-`; `sub` (also negation)
    -   `*`; `mul`
    -   `/`; `div` (`/` is overloaded for paths too)

2.  comparisons

    -   `<`; `lessThan`
    -   `<=`, `>`, `=>`; no function counterparts

3.  equality operators (also for booleans)

    -   `==` equality
    -   `!=` inequality
    -   no function counterparts for these

    ``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no" exports="both"}
    1 + 2 - 3 * 4 / 5.0
    map (builtins.mul 1.0e4) [1 2 3 4]
    ```

    ``` {.example}
    1 + 2 - 3 * 4 / 5.0
    0.6
    map (builtins.mul 1.0e4) [1 2 3 4]
    [ 10000 20000 30000 40000 ]
    echo 'org_babel_sh_eoe'
    ```

### Booleans

1.  literals: `true`, `false`

2.  operations

    1.  equalities: `==`, `!=`

    2.  and/or: `&&`, `||`

    3.  logical implication `->` (`!x || y`)

        ``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no" exports="both"}
        true -> 1 < 3
        ```

        ``` {.example}
        true
        echo 'org_babel_sh_eoe'
        ```

### null

`:t` is a REPL command that prints out the type of an expression

``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no" exports="both"}
:t null
```

``` {.example}
null
echo 'org_babel_sh_eoe'
```

The builtin function `typeOf` also does this

### Comments

`#` for one line comments

`/* ... */` for multiline

### let-expressions

lexical scoping

You can bind many variables, and later bindings have earlier ones in
scope

``` {.nix eval="no"}
let pkgs = import <nixpkgs> {};
    config = import <nixpkgs/nixos> {};
in
{ config, pkgs, ... }: { ... }
```

### if then else

no `case`, `switch`, or `elif` here

### abort, throw

1.  `abort` is like panic

    This expression `abort "Don't Panic"`{.nix eval="no" exports="code"}
    would halt evaluation and prints `"Don't Panic"` as an error message

2.  `throw` does exactly the same thing except it is ignored sometimes

    like `nix-env -qa`

    don\'t worry, it\'s magic to me too

### assert

1.  `assert x; y`

    the semicolon in the args list is supposed to mean that evaluation
    continues to the next statement, not that it takes two args

2.  used together with `->`

    ` { httpd, httpServer ? false `{.nix eval="no" exports="code"}:
    assert httpServer -\> httpd != null; }

### with-expressions

dynamic scoping

``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no" exports="both"}
with builtins; map (mul 2) (genList (x: x*x) 7) 
:a builtins # :a is a repl command that brings a set's attrs into scope
readDir (dirOf (getEnv "HOME"))
:r # :r reloads all the files
```

``` {.example}
with builtins; map (mul 2) (genList (x: x*x) 7) 
[ 0 2 8 18 32 50 72 ]
:a builtins # :a is a repl command that brings a set's attrs into scope
Added 101 variables.
readDir (dirOf (getEnv "HOME"))
{ guest = "directory"; sam = "directory"; }
:r # :r reloads all the files
'...
Added 10135 variables.
echo 'org_babel_sh_eoe'
```

three kinds of strings
----------------------

regular, indented, and URL

### \"\" (double quotes)

like your grandboss used to make

BTW, there\'s no description of how variables work exactly in Nixexprs

I looked everywhere (in the documentation)

1.  string literal

    `"no tool is the best tool"`{.nix eval="no" exports="code"}

2.  operations

    ``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
    :a builtins
    fromJSON ''{"x": [1, 2, 3], "y": null}''
    hashString "sha256" "moose and squirrel"
    match "ab" "abc"
    match "abc" "abc"
    match "a(b)(c)" "abc"
    :r
    ```

    ``` {.example}
    :a builtins
    Added 101 variables.
    fromJSON ''{"x": [1, 2, 3], "y": null}''
    { x = [ ... ]; y = null; }
    hashString "sha256" "moose and squirrel"
    "ea48540bf8671e37d7e5d3d0b0fea6a0b6b37d06fc977b773a24b52997f036f5"
    match "ab" "abc"
    null
    match "abc" "abc"
    [ ]
    match "a(b)(c)" "abc"
    [ "b" "c" ]
    :r
    '...
    Added 10135 variables.
    echo 'org_babel_sh_eoe'
    ```

### \'\' \'\' (indented)

two single quotes together at each end

``` {.nix eval="no"}
postInstall =
  ''
    mkdir $out/bin $out/etc
    cp foo $out/bin
    echo "Hello World" > $out/etc/foo.conf
  '';
```

This is often used for config file templating

-   when evaluated, the text is shifted to the left-most character then
    stops
-   all other indentation is preserved
-   similar to `<<-HEREDOC`

::: {.NOTES}
COMPLAINT: I don\'t know why, but the default indentation settings for
`nix-mode` for my install of spacemacs is horrible. May be a source of
my bias against the language, I can\'t objectively say.
:::

### unquoted URIs

` thisTalksRepo = https://gitlab.com/sehqlr/talks; `{.nix eval="no"
exports="code"}

\${}
----

antiquotation, AKA string interpolation

replaces variables with their values (as long as it isn\'t `null`)

antiquotations can include any Nix expressions, including those with
antiquotation

` "${if email then "noreply@${domain`{.nix eval="no" exports="code"}\"
else \"\"}\" }

`null` cannot be antiquoted

unless it is an attribute key

(more on this later)

Version string operations
-------------------------

There are more but here is a sample

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
:a builtins
compareVersions "1.2.3" "1.2.3"
compareVersions "1.2.1" "1.2.3"
compareVersions "1.2.5" "1.2.3"
splitVersion "1.2.3"
:r
```

``` {.example}
:a builtins
Added 101 variables.
compareVersions "1.2.3" "1.2.3"
0
compareVersions "1.2.1" "1.2.3"
-1
compareVersions "1.2.5" "1.2.3"
1
splitVersion "1.2.3"
[ "1" "2" "3" ]
:r
'...
Added 10135 variables.
echo 'org_babel_sh_eoe'
```

Paths
-----

### / style

1.  literals

    similar to Unix paths. A slash is required.

    ` specialNixexpr = import ./special.nix; `{.nix eval="no"
    exports="code"}

2.  operations

    ``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
    :a builtins
    baseNameOf /nix/store
    dirOf /nix/store
    :r
    ```

    ``` {.example}
    :a builtins
    Added 101 variables.
    baseNameOf /nix/store
    "store"
    dirOf /nix/store
    /nix
    :r
    '...
    Added 10135 variables.
    echo 'org_babel_sh_eoe'
    ```

    Why are those two not symmetrical???

### Converting strings to paths... WHY????

``` {.nix eval="no"}
absPath = /. + "/hello";
relPath = ./. + "/hello";
```

### \<\> style (`$NIX_PATH` is searched)

``` {.nix eval="no"}
pkgs = import <nixpkgs> {};
```

`<nixpkgs>` in a default Nix setup is a *channel*, which is a collection
of nixexprs in a specific directory structure, with a `default.nix` file
up top

more on that later

### which type of path to use?

In my mind, the way to remember is that `<>` is for \"vendered\" code,
and `/` is for local code. IF there is a better way to explain this I\'d
love to hear it

Modularization
--------------

### import function

takes a path to a nixexpr or a directory containing a file called
`default.nix`, parses it, and returns it. You\'ll use `import` for
qualified imports

### imports attribute in `nixpkgs`

This is a feature in `nixpkgs`, not the language, but here\'s a tip:

If you are composing configuration for many different packages together
(in NixOS, home-manager, etc) use this feature!

It will load in the config attributes into scope of your top-level
nixexpr AND merge the sets in an unsurprising way

### fetch\* functions

builtins that download stuff from the internet

This expression loads a particular version of nixpkgs with
`fetchTarball` into the current scope

``` {.nix eval="no"}
with import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-14.12.tar.gz) {};
```

Lists
-----

### zero indexed, square brackets, no commas

``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no"}
[ 1 2 3 4 ]
```

``` {.example}
[ 1 2 3 4 ]
echo 'org_babel_sh_eoe'
```

### heterogenous

``` {.bash org-language="sh" session="*nix repl*" results="output" cache="no"}
[ "foo" 73.1 [ "hello" "world" ] ]
```

``` {.example}
[ "foo" 73.1 [ ... ] ]
echo 'org_babel_sh_eoe'
```

### lazy in values, strict in length

no infinite lists for you!

### operations and functions

1.  Fairly standard FP functions on lists:

    `head` `tail` `length` `sort` `any` `all` `concatLists`

    `++` is the concatenation operator

2.  `map`, `filter`, but NO `reduce`

    only `foldl'`

3.  `elem` for testing membership, `elemAt` for accessing value at index

    out-of-bounds access results in a fatal error!

Sets
----

### the most important type

Since this is the type that derivations inhabit, sets are the most
important one

### literals {#literals-1}

`{ name = "world"; greeting = "hello"; `{.nix eval="no" exports="code"}}

this is why nixexprs look the same as JSON at a distance

### access attributes with `.` syntax

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
let S = { x = 123; foo = "bar"; }; in S.x
```

``` {.example}
123
echo 'org_babel_sh_eoe'
```

OR, return a default

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
{ x = 123; foo = "bar"; }.y or "default!"
```

``` {.example}
"default!"
echo 'org_babel_sh_eoe'
```

This is the only time that `or` makes an appearance

### keys are strings

1.  the quotation marks can be dropped most of the time

2.  because they are strings, keys can contain antiquotation

3.  if antiquote evals to `null`, attr is dropped from set

### \"dunder fuctors\"

If a set has an attribute named `__functor`, it becomes callable

Because this is Python-esque, and it sounds funny, I\'m calling this a
\"dunder functor\"

This example comes straight from the bottom of the documentation for
sets

``` {.nix eval="no"}
let add = { __functor = self: x: x + self.x; };
    inc = add // { x = 1; };
in inc 1 # this evaluates to 2
```

Then, they say this:

> This can be used to attach metadata to a function without the caller
> needing to treat it specially, or to implement a form of
> object-oriented programming, for example.

I think I love Dunder Functors now?

::: {.NOTES}
OK, OK, *so*... Nix has half-assed FP, quarter-assed OOP... if you added
in a fourth of Forth you\'d probably have a whole language here

I came up with that joke in the shower
:::

set operations
--------------

If you looked carefully in that last code source, they used a set
operator

### merge or `//`

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
{ sound = 0; dog = "good"; } // { sound = "BARK BARK"; }
```

``` {.example}
{ dog = "good"; sound = "BARK BARK"; }
echo 'org_babel_sh_eoe'
```

### set membership test

1.  static `?`

    takes an identifier

2.  dynamic `hasAttr`

    takes an expression

    ``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
    :a builtins
    { x = 123; dog = "good"; } ? dog
    hasAttr (head ["x"]) { x = 123; dog = "good"; }
    { x = 123; dog = "good"; } ? (head ["x"])

    :r
    ```

    ``` {.example}
    :a builtins
    Added 101 variables.
    { x = 123; dog = "good"; } ? dog
    true
    hasAttr (head ["x"]) { x = 123; dog = "good"; }
    true
    { x = 123; dog = "good"; } ? (head ["x"])
    error: syntax error, unexpected '(', expecting ID or OR_KW or DOLLAR_CURLY or '"', at (string):1:30

    :r
    '...
    Added 10135 variables.
    echo 'org_babel_sh_eoe'
    ```

### return attribute from set

1.  static `.`

2.  dynamic `getAttr`

### and so many more

there are a lot, and the only reference for these is the manual, which
has a glossary, and that\'s it. And the manual only comes in one size:
the whole thing

I mean, <https://orgmode.org> may have old fashioned documentation, but
at least they have indexes, and different sized chunks that you can view
it in

attribute scope
---------------

OK, we are almost done with sets

### Recursive sets

normal sets don\'t have internal scope?

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
{ x = y; y = 123; }.x
let y = "yogurt"; in { x = y; y = 123; }.x
rec {x = y; y = 123;}.x
```

``` {.example}
{ x = y; y = 123; }.x
error: undefined variable 'y' at (string):1:7
let y = "yogurt"; in { x = y; y = 123; }.x
"yogurt"
rec {x = y; y = 123;}.x
123
echo 'org_babel_sh_eoe'
```

### inherit

copy vars from the surrounding lexical scope, aka propogation

these two statements are equivalent

``` {.nix eval="no"}
inherit x y z;
x = x; y = y; z = z;
```

these two statements are equivalent

``` {.nix eval="no"}
inherit (src-set) a b c;
a = src-set.a; b = src-set.b; c = src-set.c
```

Lambdas
-------

this was not included in the Values section in the docs even though this
is an FP lang. Curious.

### whitespace is function application

`builtins.add 1 2`{.nix eval="no" exports="code"}

### partial application

``` {.bash org-language="sh" session="*nix repl*" cache="no" results="output" exports="both"}
:t builtins.add 1
```

``` {.example}
the partially applied built-in function 'add'
echo 'org_babel_sh_eoe'
```

three styles for lambda literals
--------------------------------

### single identifier pattern

If there is a single identifier in front of the colon, the function
matches any argument. If you nest many of these together, you get
positional arguments, just like in the lambda calculus

``` {.nix eval="no"}
id = x: x
times = x: n: x * n
```

I personally call these \"lambda-style\"

common for classic FP patterns and overrides

### set pattern

matches a set containing the listed attributes, and bind those values to
variables

``` {.nix eval="no"}
{ x, y, z }: x + y + z
```

This lambda only allows for a set containing **exactly** those
attributes

This is useful for writing functions that have a precise semantic
meaning, e.g. the input set represents a point in 3D space

AKA the best you can do to write your own types

### variadic args with `...`

This form allows for any set containing **at least** those attributes

` { config, pkgs, ... `{.nix eval="no" exports="code"}: { ... } }

Not to be confused by the \"etcetera\" meaning of the ellipse used
elsewhere

### default values

` { name ? "World", greeting ? "Hello" `{.nix eval="no" exports="code"}:
\"\${greeting}, \${name}!\" }

Notice our friend `?`

Notice how the semantics are the same as `or`, NOT as the set membership
operator `?`

I only fully figured this out YESTERDAY

### @-pattern

This is an extension of set patterns, mostly as a way of referring to
those variadic attributes

It can be written in two ways

``` {.nix eval="no"}
args@{ x, y, z, ... }: z + y + x + args.a
{ x, y, z, ... } @ args: z + y + x + args.a
```

### the only time commas are used

This was an early reason why I soured on Nixlang, but then I found so
many other things that irked me way more,

::: {.NOTES}
LIKE THE `?` THING
:::

Other Options? Sorta??
======================

So, on that rage crescendo, let\'s talk about alternatives

Can we use Nix without writing nixexprs? Sorta??

I give you a two imperfect examples from two of the best language
families: Lisp, and ML

Guix
----

pronounce it like \"geeks\"

### BREAKING NEWS

They **just** released a version 1.0, *this morning*
<http://guix.gnu.org/blog/2019/gnu-guix-1.0.0-released/>

### reimplimentation in Guile Scheme

That means Lisp everywhere

That\'s the dream right?

### Guix only packages GNU software

1.  If you are a free-software advocate or hardliner, you should try
    this

2.  If you [really]{.underline} hate systemd, they use GNU Shepard

3.  If you want to package other stuff, you may or may not be out of
    luck, idk

### Have not installed it :(

I want to try it at some point, but I might need a Lisper-pal to pair
with

dhall
-----

it\'s dhall y\'all!

<https://dhall-lang.org/>

### \"The non-repetitive alternative to YAML\"

...wait, how is this an alternative to Nix-lang??
<https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-nix>

> This \[...\] package provides a Dhall to Nix compiler. \[...\] This
> package targets people who wish Nix had a type system.

Wow, I **am** one of those people!

> \[...\] because Dhall cannot encode many common NixPkgs/NixOS idioms
> \[...\] \[y\]ou can use this project to embed existing Dhall code with
> Nix, but probably not as a general-purpose Nix replacement

tradeoffs, amirite?

### So, why use Dhall at all?

Well, for one thing, it integrates with the existing Nix system

it comes with a CLI tool for compiling

`nix-shell -p dhall-nix`{.bash org-language="sh" eval="no"
exports="code"}

``` {.example}
[41 of 45] Compiling Nix.Thunk.Standard ( src/Nix/Thunk/Standard.hs, dist/build/Nix/Thunk/Standard.o )
[42 of 45] Compiling Nix.XML          ( src/Nix/XML.hs, dist/build/Nix/XML.o )
[43 of 45] Compiling Nix.Builtins     ( src/Nix/Builtins.hs, dist/build/Nix/Builtins.o )

src/Nix/Builtins.hs:96:51: error:
    Module
    ‘System.Nix.Internal.Hash’
    does not export
    ‘printHashBytes32’
   |
96 | import           System.Nix.Internal.Hash       ( printHashBytes32 )
   |                                                   ^^^^^^^^^^^^^^^^
[45 of 45] Compiling Paths_hnix       ( dist/build/autogen/Paths_hnix.hs, dist/build/Paths_hnix.o )
builder for '/nix/store/m9l9pwaivpzda5vl66rix9kmsxx0k41a-hnix-0.6.0.drv' failed with exit code 1
cannot build derivation '/nix/store/jwgwm2gmnmkv4nn95g3clc26fvgj12hy-dhall-nix-1.1.6.drv': 1 dependencies couldn't be built
error: build of '/nix/store/jwgwm2gmnmkv4nn95g3clc26fvgj12hy-dhall-nix-1.1.6.drv' failed

```

::: {.NOTES}
I ran out of time to really fix this while prepping for this talk, I
should write a follow up on this part

FYI: it\'s currently marked as broken, so I was warned
:::

### Dhall is a total language (lifted from [this wiki page on Dhall\'s safety guarantees](https://github.com/dhall-lang/dhall-lang/wiki/Safety-guarantees))

the reason why I still want to look at it as a Nix suppliment

1.  you can always type-check an expression in a finite amount of time

2.  If an expression type-checks then evaluating that expression always
    succeeds in a finite amount of time

3.  there are many implications that come from this

Nix build system, the useful
============================

We started so high, we went so low, lets go visit the useful part of
Nix, the actual build system

what version am I using currently?
----------------------------------

``` {.bash org-language="sh" exports="both" cache="yes" results="output"}
nix --version
```

Let\'s write a nixexpr
----------------------

### what are we building?

Piet is an esoteric language that takes in pixel art as input

![](Piet_hello_big.png)

### prefetch to get hash

``` {#hash .bash org-language="sh" results="output verbatim" cache="yes" exports="both"}
nix-shell -p nix-prefetch-scripts --run "nix-prefetch-url https://www.bertnase.de/npiet/npiet-1.3e.tar.gz"
```

``` {.example}
1i9ihbjmravid3h7wvns712axdl5xn398hk12pvzl79fs5kcf6g8
```

### default.nix

``` {.nix eval="no" noweb="yes" tangle="default.err.nix"}
{ pkgs ? import <nixpkgs> {}, ... }:
pkgs.stdenv.mkDerivation {
   name = "npiet-1.3e";
   src = pkgs.fetchurl {
     url = https://www.bertnase.de/npiet/npiet-1.3e.tar.gz;
     sha256 = "<<hash()>>";
   };
}

```

### lets build it!

`nix build`{.bash org-language="sh" eval="no" exports="code"}

``` {.example}
builder for '/nix/store/8wgwpvh5wh2v145mdfz60g9rcpybcrcy-npiet-1.3e.drv' failed with exit code 2; last 10 log lines:
  npiet.c:2234:7: warning: variable 'pre_xpos' set but not used [-Wunused-but-set-variable]
     int pre_xpos, pre_ypos;
         ^~~~~~~~
  gcc -g  -o npiet npiet.o -lm 
  gcc -g -O2    -Wall -DHAVE_CONFIG_H   -c npiet-foogol.c
  npiet-foogol.y:50:10: fatal error: gd.h: No such file or directory
   #include <gd.h>
            ^~~~~~
  compilation terminated.
  make: *** [Makefile:100: npiet-foogol.o] Error 1
error: build of '/nix/store/8wgwpvh5wh2v145mdfz60g9rcpybcrcy-npiet-1.3e.drv' failed

```

### PORQUE??

Turns out `gd.h` is in `libgd`, which is available on nix under
`nixpkgs.gd`

Just to save some time, it turns out that `groff` is another dependency

``` {.nix eval="no" noweb="yes" tangle="default.nix"}
{ pkgs ? import <nixpkgs> {}, ... }:
pkgs.stdenv.mkDerivation {
  name = "npiet-1.3e";
  src = pkgs.fetchurl {
    url = https://www.bertnase.de/npiet/npiet-1.3e.tar.gz;
    sha256 = "<<hash()>>";
  };
  buildInputs = with pkgs; [ gd groff ];
}
```

### will it build?

``` {.bash org-language="sh" exports="both" results="output" cache="yes"}
nix-build && find result -executable -type f
nix-env -i -f .
npiet Piet_hello.png
```

``` {.example}
/nix/store/9v03fb58vvp3r7mjlzymy3m0mmsqrnqk-npiet-1.3e
Hello world!
```

### cleaning up

I wrote this to reset the env for the slidedeck, but it also demos the
garbage collector

``` {.bash org-language="sh" exports="both" results="output" cache="yes"}
nix-env -e -f .
rm result && nix-collect-garbage
```

``` {.example}
1138 store paths deleted, 919.07 MiB freed
```

The Nix Ecosystem
=================

Lets take a brief tour of the larger ecosystem, both \"official\" and
unofficial

NixOS projects
--------------

### nixpkgs

the collection of nixexprs that define the software that Nix ships with

1.  github project for main repo

2.  inclusion into nixpkgs is done via Issues and PRs

3.  search and discovery are not great

    `nix-env` and `nix search` together make a decent search tool

    the most efficient discovery is from browsing the src directories

### NixOS

configure your whole OS in nix

base system config found in nixpkgs

### NixOps

builds on NixOS

infrastructure automation with NixOS machines

### cache.nixos.org

binary cache for public use

### Hydra

Official build farm and CI system

This populates the public cache

### Disnix

This was really hard to research, but probably a k8s analog?

beyond
------

### home-manager

<https://github.com/rycee/home-manager>

### lorri

<https://github.com/target/lorri>

### cachix

<https://cachix.org/>

### static nix

<https://matthewbauer.us/blog/static-nix.html>

Questions for Further Conversation
==================================

I have questions that fit within 3 broader questions

Is the Nix Expression Language redeemable?
------------------------------------------

-   is it all that bad?
-   can it be made better?
-   standardize DRV?

Is GitHub the best place for nixpkgs to live?
---------------------------------------------

-   is there a different platform that would be better?

-   I miss AUR and PPA. Could we make distribution more decentralized?

    ::: {.NOTES}
    IPFS? Dat?
    :::

Could we make a GUI for nix-env?
--------------------------------

-   an app store that works with nix binary caches
-   what about within the enterprise?

Contact
=======

THANK YOU

Mastodon: @sehqlr@mastodon.technology

stl-tech.slack.com: @sehqlr

talks+nix\@samhatfield.me
