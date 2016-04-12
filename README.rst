Markovpass
==========

A markov chain based passphrase generator with entropy estimation.
:code:`markovpass` generates randomized passphrases based on a markov chain
along with the total shannon entropy of the nodes traversed. This generates
human sounding phrases, which attempt to strike a balance between ease of
memorization, length and password quality. Results will be something like::

    qurken ret which bettle nurence
    
or::

    fateus lodges ink which minio

Installation
------------

:code:`markovpass` depends on the :code:`Control.Monad.Random` library. Pretty
much everything else should be a standard part of your :code:`ghc` install. At
some point I hope to package this properly, but I'm still fairly new to
Haskell, so that's not yet my highest priority!

Usage
-----

::

  Usage: markovpass [OPTION]... [FILE]...
    -m MINENTROPY  --minentropy=MINENTROPY  Minumum Entropy (default 60)
    -n NUMBER      --number=NUMBER          Number of passphrases to generate (default 1)
    -l LENGTH      --length=LENGTH          NGram Length (default 3)
    -w LENGTH      --minwordlength=LENGTH   Minimum word length for corpus (default 1)
    -s             --suppress               Suppress entropy output
    -h             --help                   Print this help message

:code:`markovpass` requires a corpus to work with. It accepts multiple file
arguments, or can take input from STDIN. It can take pretty much any input and
will strip the input of any non-alphabetic characters (and discard words
containing non-alphabetic characters). The more and more varied the corpus the
greater the entropy, but you'll hit diminishing returns fairly quickly. The
`-w` option can be used to remove short words from the corpus which will
increase the average length of words in your passphrase, but not guarantee a
minimum length.

Guesswork?
----------

I believe that for moderately long passphrases the total shannon entropy of the
nodes visited probably provides a reasonably good estimate of the difficulty in
guessing a :code:`markovpass` passphrase. That said, there are some definite
issues with this approach. Not all passphrases are created equal: for a given
shannon entropy there will be more and less likely passphrases, and the more
likely passphrases will be easier to guess. As an example, suppose I generate a
simple passphrase that's either 'a' with probability ¾ or 'b' with probability
¼.  The entropy for the decision will be the same either way, but it should be
clear that 'a' is easier to guess. In the limiting case of very long
passphrases we should expect passphrases to be of asymptotically equal
likelihood. So for long enough passphrases, entropy should be a fairly good
estimate of the difficulty of guessing. What qualifies as 'long enough' it not
totally clear to me, so use this at your own risk!
