= Opening

== hello, this was unexpected

== emailregex.com

=== show site, imgs
=== `fold ./emailregex.txt`

== why??
=== RFC5322
=== `bat ./addresses.txt`

== Slightly easier to read regex

split the regex on `(?:` and drop

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

= Code

> module Main where

We need the Applicative module, Attoparsec assumes that many and <|> are in
 scope

> import Control.Applicative
> import Data.Attoparsec.Text

Attoparsec dont want no Strings,
Strings are data that wont get parsed, no way

> import qualified Data.Text as T
> import qualified Data.Text.IO as TIO

So, we need to solve some subsets of this problem to make it manageable. After
staring at this monstrosity for a long time, I realized that all email
addresses have an @ symbol, right?

== Parsing a single character

And, it just so happens that parsing a single character is a really good place
to start with Attoparsec

> at :: Parser Char
> at = char '@'


> dot :: Parser Char
> dot = char '.'

== Character Classes

In a regex, to match a single character from a set of characters, you enclose
the possible set of characters with square brackets. There is a specialized
range notation that makes for more compact regexes. As it turns out, there is an
Attoparsec function that takes a similar notation as a string: `inClass`

So, all you need to do to translate character classes into a parser function is
to replace the square brackets with double quote marks and put `inClass` in
front

=== inAlnum

Original regex:
    [a-z0-9]

> inAlnum :: Char -> Bool
> inAlnum = inClass "a-z0-9-"

=== inAlnumHyphen

Original regex:

    [a-z0-9-]

> inAlnumHyphen = inClass "a-z0-9-"

=== inGrabBag

I called it that because I had no idea what the semantics were

Original regex:

    [a-z0-9!#$%&'*+/=?^_`{|}~-]

> inGrabBag = inClass "a-z0-9!#$%&'*+/=?^_`{|}~-"

The equivalent of matching anything NOT in the class `[^ ]` is handled by the
function `notInClass`, which will come up later.

=== update regex

Now, lets try to replace some of the formatted regex with our function names,
just to see how much more sense we can get from it

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

== IPv4 address

Take a look at these lines from the regex:

    (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}
    (?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?| inAlnum * inAlnum :
    (?:[\x01-\x08\x0b\x0c\x0e-\x1f\x21-\x5a\x53-\x7f]|\\[\x01-\x09\x0b\x0c\x0e-\x7f])+)\])

Those first two lines kinda looks like an IP address parser to me! HOWEVER, we
have to actually include the last line because the capture group after the
`{3}` goes the 4th last character in the regex

Also, theres a mistake?? `\x21-\x5a\x53-\x7f == \x21-\x7f`, right??

Since the IP address has to be a valid one, we have to make sure that the
numbers are within a certain range. The regex does this in a very literal
way: it checks to see if the numerals line up. But, we can do better.

BTW, there is a library called =ip= that does this better, and returns data
rather than just validation.

BTWBTW, This is where I started having problems with translating

> ip = do

So, the first line in the pattern seems to match up with this closely:
You get a number between 0 and 255 (the octet) followed by a period. Do this
three times

`count :: Monad m => Int -> m a -> m [a]`

>   count 3 $ octet <*> dot

Then, you have a choice between a final octet and a "mail group", which is a
term from the RFC that describes that sequence after the IPv4 octets. The
binary sequences cover most of ASCII execpt for certain characters, most
commonly LF and CR. Im pretty sure the alternative starting with

    char '\\`'

is for escaped characters within the address itself.

>   octet <|> group
>   where
>     group = hyphenSepAlnum <*> char ':' <*> many1 $ notInClass "\n\r\t \\" <|> (char '\\' <*> notInClass "\n\r")
>     octet =

This code below was stolen from that `ip` package I mentioned before

>       decimal >>= limitSize
>     limitSize i =
>       if i > 255
>       then fail "All octets in an ipv4 address must be between 0 and 255"
>       else return i


=== another update

    (?: inGrabBag +
    (?: dot inGrabBag +)*|"
    (?: notInClass "\n\r\t \\\"" | char '\\' notInClass "\n\r")*")
    at
    (?:
    (?: inAlnum
    (?: inAlnumHyphen * inAlnum )? dot)+ inAlnum
    (?: inAlnumHyphen * inAlnum )?|\[(ip \])

== Taking a second look

At around this time, the text starting becoming clear enought that I was able to
take a look a structural properties again. What I realized was that some of the
more confusing patterns were describing the contraint that a region of text
may have a certain character(s) act as a separating character, but it cannot be
in the front or end position. Really hard to encode in regex, but only one
function call for Attoparsec!

=== sepBy

> dotSep p = p `sepBy` dot
> hyphenSepAlnum = inAlnum `sepBy` (char '-')

=== include those

    (?: dotSep inGrabBag |"
    (?: notInClass "\n\r\t \\\"" | char '\\' notInClass "\n\r")*")
    at
    (?: dotSep (hyphenSepAlnum)|\[ ip \])

== Quantifiers and Choice

Almost done!

One last thing we need to translate before we get to putting the pieces
together: quantifiers and the choice operator

- `*` to `many`
- `+` to `many1`
- `|` to `<|>`

== Putting it all together: emailValidator

> emailValidator :: Parser T.Text
> emailValidator =
>   many <$>
>     dotSep inGrabBag
>   <|> (char '"') <*> (many $ notInClass "\n\r\t \\\"") <*> char '"'
>   <*> at
>   <*> hyphenSepAlnum <*> dotSep  <|> char '[' <*> ip <*> char ']'

== The main event

> main :: IO ()
> main = do
>   addrs <- TIO.readFile "./addresses.txt"
>   parseTest emailValidator addrs

= The test: can we run the parser and have it succeed on a line from `addresses.txt`
