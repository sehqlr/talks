---
title: "Nix and NixOS: Configure an OS with a package manager?! How??"
author: Sam Hatfield \<hey@samhatfield.me\>
---

Hello, LUG!
=============

contact
--------

email/website: hey@samhatfield.me

Mastodon: @sehqlr@mastodon.technology

stl-tech.slack.com: @sehqlr

about me
--------

I've been a developer for 5 years, a NixOS user for about 1.5. I was an English major before that.

I started a freelancing business at the beginning of the year, all the MBA stuff
is going **slowly**. I am looking for new clients, so if you are interested,
lets talk.

new kind of intro to Nix
-----------------

I want to try out a different approach to introduce Nix

Others don\'t focus enough on the underlying theorhetical basis of the
tech, imo


What does a package manager have to do with OS config?
------------------------------------------------------

Package managers put files places, and those files then can configure the OS.

-   What is Nix?
-   Nix Store
-   A Demo Nixexpr
-   Nix Ecosystem
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

packages are the immutable outputs of pure functions

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
==============

well documented
---

This isn\'t unique to Nix, **BUT** I wish I could recommend this as a
tool, not an independent study

a good DSL
---

Hot Take: Haskell and JSON had a baby, but can\'t agree on parenting
styles

BTW, if there are any good arguments for anything I\'m complaining
about, talk to me

Remember: opinions are like armpits. Everyone has one, and most of them
stink

perfect
---

no software is perfect

I said this project was a breath of fresh air.

Well, fresh air has bad smells too

Why is it worth talking about?
==============================

Unique approach
---

Nix has a unique approach to package management, with many useful
implications

1.  all install, upgrade, and rollback operations are atomic

2.  helps with difficult dependency management

3.  share reproducible build envs in source and binary forms

4.  sarcasm/ learn another DevOps system with terrible syntax /s

    no but really I like it

builds upon existing Unix technologies
---

1.  default env is close to `build-essentials` in Debian

2.  lots of CLI tools (that are included in the slides)

3.  most CLI options are also configurable with env vars


The \"other\" package managers
==============================

The details here are very generalized, and for contrasting with Nix only

Filesystem Hierarchy Standard
---

many different directories contain files for programs and configuration

1.  In Ubuntu, the Emacs executable is installed at `/usr/bin/emacs25`

    ::: {.NOTES}
    <https://packages.ubuntu.com/bionic/amd64/emacs25/filelist>
    :::

2.  Emacs elisp packages are installed in `~/.emacs.d`

3.  Emacs config files are in `$HOME` or elsewhere

in-place changes
---

1.  when emacs is updated, the data at `/usr/bin/emacs25` is overwritten

2.  if the update process crashes or is interrupted, it could be in
    partial state

    many mechanisms prevent this, but Nix does it differently, as we
    shall see

MITM
---

In order to make sure your package hasn\'t been compromised on its way
to you by a MITM attack, you need to verify it with a cryptographic
hashing process

typically, you download the hash the server has, perform the hashing on
your computer, then compare the two hashes to make sure they match

Some package managers do this automatically, but some don\'t and you
have to do it by hand

dependency management
---

dependency management is a large part of what the package manager does,
and when it goes wrong, your packages are broken

CONCURRENT dep mgmt
---

wow, is that impossible? How do you manage this?

-   `chroot`?
-   sandboxes?
-   DOCKER!?

maybe this one reason is why every language has their package manager?

... and tools built on top? to manage concurrent versions?

Nix Does It Different
=====================

how it compares to \"the others\"

``` {.bash}
nix-store -q --graph $(realpath $(which curl)) | dot -Tpng
```

CRYPTO ALL THE THINGS!
---

Nix hashes **all** packages it builds, everytime it builds them

the verification step and the build step are one and the same

changes to the package result in different hashes

every package has a unique ID based on its contents

MITM attacks are less likely

/nix/store
---

all packages that are built by nix go into this directory

YES, this breaks FHS, but we\'ll return to that in a few slides

the directory template is `$hash-$name`, where hash is the hash from the
build

``` {.bash}
nix path-info nixpkgs.curl
```

``` {.example}
/nix/store/yb6s1k41s7sydr6q3nzmayhvbkzhydvf-curl-7.64.0-bin
```

Dependencies...
---

are included within the package!

``` {.bash}
find $(nix path-info nixpkgs.emacs) -executable -type f
```

This means that dependency tracking happens during the build step

this mitigates the broken dependency problem because each package brings
their own

install via symlink
---

installs are done with symlinks from outside the store

``` {.bash}
ls -l $(which curl)
```

``` {.example}
lrwxrwxrwx 1 root root 68 Dec 31  1969 /run/current-system/sw/bin/curl -> /nix/store/nakc4z4vz69sq2jjlakp64s04qgggvja-curl-7.64.1-bin/bin/curl
```

Because you can symlink anywhere, we can reestablish FHS via symlinks.
NixOS doesn\'t, but you could!

BTW, Nix resets all timestamps, that\'s why the file is so old

updates... also via symlink
---

because the installation of a file is just a symlink to the store

updates are just changing the symlink of a package with a different hash

different versions, different paths, same store
---

Because changing a package changes the hash, each different build
results in a different path

The Nix store keeps paths around as long as there is a reference to them

packages are immutable; only additions, no updates, which means

ALL VERSIONS ARE CONCURRENT VERSIONS

caching
---

the store is a cache for everything already, and not just for your
machine

in fact, you can serve your `/nix/store` as a binary cache for **other**
machines

installs, updates, and rollbacks are atomic
---

because a symlink either points to another file or it doesn\'t, install
and update actions are never in a partial state.

AND, because previous packages are still available when you build a new
package, a rollback is **exactly** the same as an update

nix env generations
---

Nix has this mechanism called *generations* where each iteration of an
update to an environment is tracked by entries in the store

This means that, in a basic sense, your package actions are version
controlled.

``` {.bash}
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

-   changing my laptop config
-   made a change that didn\'t include networkmanager
-   before, I\'d move the laptop to connect to the router via Ethernet
-   but this time, I realized that I could do a rollback
-   rebooted, selected the GRUB entry that I wanted, and BAM! Wifi works
-   that was it!
-   I fixed the config, tested this time, and was able to move forward
    without losing wifi again
:::

per-user package management
---

because each user gets their own environment with their own
generations, each user can manage their own packages **without sudo**

``` {.bash}
ls -l ~/.nix-profile
```

``` {.example}
lrwxrwxrwx 1 sam users 42 Dec 13 20:06 /home/sam/.nix-profile -> /nix/var/nix/profiles/per-user/sam/profile
```

nix-shell
---

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

bad packages are isolated
---

1.  malicious

    No Trojan horses here!

2.  poorly written

    I can\'t screw up the store due to incompetence

    some mistakes become build errors instead of system crashes

reproducability
---

[Is NixOS Reproducible?](https://r13y.com)

This is no ordinary caching, it\'s deterministic builds

content-addressability
---

[Cachix - Nix binary cache hosting](https://cachix.org)

instead of computing a hash on a package as you build it, you can ask to
download a valid package with the computed hash.

config-file management
---

any program and uses files for configuration can be configured with Nix

any file can be in the Store, and symlinks can put them wherever the
program expects a file to be.

This is how NixOS works

possibly more??
---

I\'m convinced that I\'ve missed some, and that there are even more to
be thought up

Nix Expression Language
=================

the common term for a program in this language is \"nixexpr\" which I'll be
using for the remainder of the talk

Implemented in C++
---

for portability and speed

Otherwise, it\'d probably be an EDSL in Haskell, which would have raised
other complaints

no specifying document that I could find
---

-   no formal grammar
-   no language specification

Most of the research in this part is based on [the
manual](https://nixos.org/nix/manual/#ch-expression-language).

This does not include the functions that are in `nixpkgs`

which could be considered a stdlib for the lang

dynamically typed
---

types are checked at runtime

``` {.bash}
1 + 1
"a" + 1
```

``` {.example}
1 + 1
2
"a" + 1
error: cannot coerce an integer to a string, at (string):1:1
```

lazy evaluated
---

lazy eval is like putting stuff in an online shopping cart.

You could have something in there for months, but until you actually pay
for it, it won\'t show up at your door.

purely functional
---

$$ (\lambda s.\!p)(s) $$

no side-effects, only inputs and outputs, and outputs are completely
dependent on inputs. Same inputs, same outputs

domain-specific
---

nixexprs are ultimately for building packages, so the design is
constrained on purpose.

Let\'s write a nixexpr
======================

I'll be introducing the nix expression language syntax as we go along, and try
to point out any footguns and other pitfalls as needed.

what are we building?
---

I'm pretty sure I was thinking about old-school UNIX text-based games, of which
I did not play very much, except one time I'm pretty sure I played a MUD.

[mud-pi source code](https://github.com/Frimkron/mud-pi)

-------------------------------------------------------------------------------

The default mode of operation for this application is to run on a Raspberry Pi,
and have clients connect to the game via telnet, and send and receive plaintext
to play the game. I have a copy of the repo, I'll show you how it works outside
of NixOS.

-------------------------------------------------------------------------------

What we are going to do is to build a nixexpr that builds the package, and put
it in a file called `build.nix`. Then, we are going to then build a nixexpr for
configuring a systemd service that autoruns `mud-pi`

Package definition `build.nix`
------------------------------

1. Bring in nixpkgs 
2. mkDerivation & name
3. fetchFromGitHub
4. nix-prefetch-git
5. builder.sh

Configuration definition `config.nix`
---------------------------------------------

1. Bring in config, lib, & packages
2. let cfg...
3. options.services.mud-pi.enable
4. config && systemd

`configuration.nix` our NixOS entrypoint
----------------------------------------

1. Open up configuration.nix
2. Import `config.nix`
3. Enabling the service in NixOS
4. Enabling the systemd service
5. Demo the new service

The NixOS Ecosystem
=================

Lets take a brief tour of the larger ecosystem, both \"official\" and
unofficial

Official Projects
==============

These are the ones listed on their main website, nixos.org

nixpkgs
---

the collection of nixexprs that define the software that Nix ships with and the
default configuration of NixOS

1.  github project for main repo

2.  inclusion into nixpkgs is done via Issues and PRs

3.  search and discovery are not great

    `nix-env` and `nix search` together make a decent search tool

    the most efficient discovery is from browsing the src directories

NixOps
---

builds on NixOS

infrastructure automation with NixOS machines

cache.nixos.org
---

binary cache for public use

Hydra
---

Official build farm and CI system

This populates the public cache

Disnix
---

This was really hard to research, but probably a k8s analog?



Related Projects
======

Guix
----

pronounce it like \"geeks\"

reimplementation in Guile Scheme

GuixSD only packages GNU software

1.  If you are a free-software advocate or hardliner, you should try
    this

2.  If you **really** hate systemd, they use GNU Shepard

3.  If you want to package other stuff, you may or may not be out of luck


home-manager
-----------

<https://github.com/rycee/home-manager>

lorri
---

<https://github.com/target/lorri>

cachix
---

<https://cachix.org/>

static nix
---

<https://matthewbauer.us/blog/static-nix.html>


Conclusion
==========

I believe very strongly that the Nix Store is the most interesting part of this
whole system, and it makes a very interesting Linux distro. I cannot recommend
it to a general audience, or even many programmers, since it takes quite a bit
of effort to become productive in this system. However, I will say that as more
useful tools are created, this distribution or spinoffs could really change the
relationship between package managers, build systems, and operating systems.

Questions for Further Conversation
==================================

I have questions that fit within 3 broader questions

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

Are there other ways that this system could be made more usable?
--------------------------------------------------------------------


Contact
=======

THANK YOU

email/website: hey@samhatfield.me

Mastodon: @sehqlr@mastodon.technology

stl-tech.slack.com: @sehqlr
