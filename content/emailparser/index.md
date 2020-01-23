+++
title = "From emailregex.com to Attoparsec"
outputs = ["Reveal"]
layout = "list"
+++

Preamble
========

``` {.bash org-language="sh" eval="never"}
notmuch search --output=files --format=text0 tag:lists or from:meetup.com or from:lobste.rs | xargs -0 cat | grep '^From:.*@' | sort | uniq | cut -c7- > addresses.txt
```

Opening
=======

emailregex.com
--------------

img title img regex

the regex
---------

``` {.bash org-language="sh" results="output" exports="both"}
wc -c ./emailregex.txt
fold ./emailregex.txt
```

``` {.example}
(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"(?:[\x01-\x0
8\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@(?:
(?:[a-z0-9](?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9](?:[a-z0-9-]*[a-z0-9])?|\[(?:(?:25
[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?
|[a-z0-9-]*[a-z0-9]:(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x
09\x0b\x0c\x0e-\x7f])+)\])
```

This is 427 characters long, or six lines when line width is set to 80

Why is it so complex?
---------------------

This email regex doesn\'t just include the kind of email address that
you include in registration forms, like this one: `hey@samhatfield.me`

This regex implements RFC5322, which is mentioned at the top of the
website.

The regex (and our reimplementation) has to be able to parse more...

NOTE These are the `From` headers from my notmuch email database. I have
searched for emails from mailing lists, meetup.com, and lobste.rs, to
keep these limited to public sources.

``` {.example}
Sam H <samuel.e.hatfield@gmail.com>
rebecca skinner <Haskell-Users-Group-St-Louis-announce@meetup.com>
WU ICTS -  Health Informatics and Data Science Meetup <info@meetup.com>
"0x70532007" <0x70532007@lobste.rs>
```

Walkthrough
===========

First, we need to parse the parser
----------------------------------

So, how do we reimplement this thing with Attoparsec? Let\'s walk thru a
possible path together. Obviously, each reimplementation you may attempt
in future are going to be their own particular journey.

The very first thing we are NOT going to do is start at the beginning of
the expression and start to try to implement each parser function, which
was my first instinct. Instead, since we are following an ideal path, we
are going to think about the patterns and strutures in this problem, and
break the regex into subregexes, implement those into parser functions,
then use the combinators to arrive at our solution

but first, lets make this a little easier to read

``` {.bash org-language="sh" results="output verbatim" exports="both"}
cat ./emailregex.txt | sed 's/(?:/\n(?:/g'
```

``` {.example}

(?:[a-z0-9!#$%&'*+/=?^_`{|}~-]+
(?:\.[a-z0-9!#$%&'*+/=?^_`{|}~-]+)*|"
(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")@
(?:
(?:[a-z0-9]
(?:[a-z0-9-]*[a-z0-9])?\.)+[a-z0-9]
(?:[a-z0-9-]*[a-z0-9])?|\[
(?:
(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?|[a-z0-9-]*[a-z0-9]:
(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])
```

Attoparsec basics
-----------------

at this time, i realized that there is one character that all email
addresses have... `@`! and turns out that this is a great place to start
with some attoparsec

What is parser? What is combinator?

at :: Parser Char at = char \'@\'

parse at \"@\"

Regex metacharacters to Attoparsec parsers, part I
--------------------------------------------------

### Match any single character

Regex
:   `.` matches anything, including newlines sometimes

Attoparsec
:   `anyChar :: Parser Char`

### Match the end of line

Regex
:   `$` matches the end of a line

Attoparsec
:   `endOfLine :: Parser ()`

### NOTE

Since functions are now orthogonal to text data, there are no more
metacharacters, and therefore, no more metacharacter escaping. If you
want to match an opening square bracket, `char '['` is all you need!
This will become important later

Characters Classes and Capture Groups
-------------------------------------

### Classes

So, in regexes, there are character classes, delimited by square
brackets, and capture groups, delimited by parentheses.

As I looked at this thing, I noticed that there were a LOT of character
classes, some of them for binary character codes, too. I thought that
was odd.

ASIDE: Did you know that Emacs doesn\'t seem to have a regex mode? Like,
builtin, I mean. It has a regex builder, and uses regexes as function
args EVERYWHERE, but there isn\'t an editor mode for them. Yet another
reason why what I\'m showing tonight is useful: your Haskell PCs are
just Haskell.

So, I decided to extract out all the character classes, deduplicate
them, and see if I can understand their semantics

1.  Alphanumeric-ish character classes

    So, the first three are **really** easy to translate, both
    syntactically and semantically. Attoparsec defines a function
    `inClass :: String -> Char ->
        Bool`. As you can see, it doesn\'t return a `Parser` type. I
    looked at the source, and it actually uses an unboxed `FastSet` type
    for performant membership testing. Since this library is meant for
    performance over everything else, there are little oddities... at
    least, that\'s what I tell myself.

    Back to the character class...

    1.  Match `a` to `z` and `0` to `9`

        Regex
        :   `[a-z0-9]`

        Attop
        :   `inClass "a-z0-9"`

    2.  [TODO]{.todo .TODO} Same as above, but with a hyphen

        Regex
        :   `[a-z0-9-]`

        Attop
        :   `inClass "a-z0-9-"`

    3.  Whatever this grabbag of characters is supposed to be

        Regex
        :   `[a-z0-9!#$%&'*+/`?\^\_\`{\|}\~-\]=

        Attop
        :   `inClass "a-z0-9!#$%&'*+/`?\^\_\`{\|}\~-\"=

2.  interlude: start replacing formatted regex with Haskell

    ``` {.haskell}
    at :: Parser Char
    at = char '@'

    inAlnum :: Char -> Bool
    inAlnum = inClass "a-z0-9-"

    inAlnumHyphen :: Char -> Bool
    inAlnumHyphen = inClass "a-z0-9-"

    inGrabBag :: Char -> Bool
    inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

    {-
    (?: inGrabBag +
    (?:\. inGrabBag +)*|"
    (?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")
    at
    (?:
    (?: inAlnum
    (?: inAlnumHyphen * inAlnum )?\.)+ inAlnum
    (?: inAlnumHyphen * inAlnum )?|\[
    (?:
    (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
    (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?| inAlnum * inAlnum :
    (?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])
    -}
    ```

Combinators
-----------

### IP parser

take a look at this

``` {.example}
(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?| inAlnum * inAlnum :
```

That kinda looks like an IP address parser to me!

Since the IP address has to be a valid one, we have to make sure that
the numbers are within a certain range. The regex does this in a very
literal way: it checks to see if the numerals line up. But, we can do
better, with `satisfy :: (Char -> Bool) -> Parser Char`

BTW, there is a library called `ip` that does this better, and returns
data rather than just validation.

``` {.haskell}
dot :: Parser Char
dot = char '.'

ip :: Parser Text
ip = do
  count 3 $ octet <*> dot
  octet
  where
    octet = decimal >>= limitSize
    limitSize i =
      if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i
```

so now, with a the IP part looks like

``` {.haskell}
at :: Parser Char
at = char '@'

inAlnum :: Char -> Bool
inAlnum = inClass "a-z0-9-"

inAlnumHyphen :: Char -> Bool
inAlnumHyphen = inClass "a-z0-9-"

inGrabBag :: Char -> Bool
inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

dot :: Parser Char
dot = char '.'

ip :: Parser Text
ip = do
  count 3 $ octet <*> dot
  octet <|> hyphenSepAlnum <*> char ':' <*>  
  where
    octet = decimal >>= limitSize
    limitSize i =
      if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i

{-
(?: inGrabBag +
(?: dot inGrabBag +)*|"
(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")
at
(?:
(?: inAlnum
(?: inAlnumHyphen * inAlnum )? dot)+ inAlnum
(?: inAlnumHyphen * inAlnum )?
(?: char '[' <*> ip char ']'
-}
```

### ASCII Sequences

These are slices of the ASCII character set excluding certain characters

1.  Excludes NL, CR (newline, carriage return)

    Regex
    :   `[\x01-\x09\x0b\x0c\x0e-\x7f]`

    Attop
    :   `notInClass "\n\r"`

2.  Excludes NL, CR, TAB, Space, and \\

    Regex
    :   `[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]`

    Attop
    :   `notInClass "\n\r\t \\"`

3.  Excludes NL, CR, TAB, Space, \\, and \" (double quote)

    Regex
    :   `[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]`

    Attop
    :   `notInClass "\n\r\t \\\""`

### combining them?

``` {.haskell}
-- something about escaping characters in here
notInClass "\n\r\t \\\"" <|> char '\\' <*> notInClass "\n\r"
```

### interlude

``` {.haskell}
at :: Parser Char
at = char '@'

inAlnum :: Char -> Bool
inAlnum = inClass "a-z0-9-"

inAlnumHyphen :: Char -> Bool
inAlnumHyphen = inClass "a-z0-9-"

inGrabBag :: Char -> Bool
inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

dot :: Parser Char
dot = char '.'

ip :: Parser Text
ip = do
  count 3 $ octet <*> dot
  octet
  where
    octet = decimal >>= limitSize
    limitSize i =
      if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i

{-
(?: inGrabBag +
(?: dot inGrabBag +)*|"
(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21\x23-\x5b\x5d-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])*")
at
(?:
(?: inAlnum
(?: inAlnumHyphen * inAlnum )? dot)+ inAlnum
(?: inAlnumHyphen * inAlnum )?|\[
(?: ip | inAlnum * inAlnum :
(?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])
-}
```

### regex metacharacters to attoparsec parsers, part II

1.  \* == many :: f a -\> f \[a\]

2.  \+ == many1 :: Alternative f `> f a -> f [a]
    **** ? =`

3.  \| == (\<\|\>) :: f a -\> f a -\> f a

### another interlude

``` {.haskell tangle="email.hs"}
at = char '@'

inAlnum = inClass "a-z0-9-"

inAlnumHyphen = inClass "a-z0-9-"

inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

dotSep p = p `sepBy` (char '.')

hyphenSepAlnum = inAlnum `sepBy` (char '-') 

ip :: Parser Text
ip = do
  count 3 $ octet <*> dot
  octet <|> group
  where
    group = hyphenSepAlnum <*> char ':' <*> many1 $ notInClass "\n\r\t \\" <|> char '\\' <*> notInClass "\n\r"
    octet = decimal >>= limitSize
    limitSize i =
      if i > 255
      then fail "All octets in an ipv4 address must be between 0 and 255"
      else return i

emailValidator = do
  many $ dotSep inGrabBag <|> char '"' <*> many _binaryStuff1 <*> char '"'
  <*> at
  <*> many $ dotSep <$> hyphenSepAlnum <|> char '[' <*> ip <*> char ']'
```

Closing
=======

Contact
=======

THANK YOU

Mastodon: @sehqlr@mastodon.technology

stl-tech.slack.com: @sehqlr
