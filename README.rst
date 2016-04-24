Markovpass
==========

A Markov chain based passphrase generator with entropy estimation.
:code:`markovpass` generates randomized passphrases based on a Markov chain
along with the total Shannon entropy of the nodes traversed. This generates
human sounding phrases, which attempt to strike a balance between ease of
memorization, length and password quality. Results will be something like::

    qurken ret which bettle nurence
    
or::

    facupid trible taxed partice

Installation
------------

:code:`markovpass` depends on the :code:`Control.Monad.Random` library. Pretty
much everything else should be a standard part of your :code:`ghc` install. At
some point I hope to package this properly, but I'm still fairly new to
Haskell, and that's not yet my highest priority.

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
arguments, or can take input from STDIN. It can take pretty much any text input
and will strip the input of any non-alphabetic characters (and discard words
containing non-alphabetic characters, but keep words sandwiched by
non-alphabetic characters). The larger and more varied the corpus the greater
the entropy, but you'll hit diminishing returns fairly quickly. I recommend
using `Project Guttenberg <https://www.gutenberg.org/>`_. The :code:`-w` option
can be used to remove short words from the corpus which will increase the
average length of words in your passphrase, but not guarantee a minimum length
(the minimum word length will be the lesser of the :code:`-w` and :code:`-l`
options).

Shannon Entropy and Guesswork
-----------------------------

Shannon entropy provides a good estimate of the lower bound of the average
guesswork required to guess a password (to within an equivalent of a little
over 1.47 bits) [1]_, but average guesswork is not necessarily a reliable proxy
for difficulty in guessing a password [2]_. Consider the following password
generation method: I choose 500 characters and for each character there is a
0.999 chance I choose 'a' and a 0.001 chance I choose 'b'. The Shannon entropy
for this process is about 5.7 bits, which based on [1]_ should give an average
number of guesses needed of at least 17.9. Yet an adversary who knows my method
will guess 'aaaaa...' and get my password right on the first guess 60.4% of the
time. So you should treat Shannon entropy estimates with caution.

That said, I suspect that for moderately long :code:`markovpass` passphrases
using a representative corpus of language, Shannon entropy is probably a good
proxy for difficulty in guessing. The fundamental problem with average
guesswork is that the distribution of passphrase probabilities isn't
necessarily flat. If the distribution has a strong peak (or multiple peaks) and
a long tail of lower probability passphrases then average guesswork is going to
be a poor proxy for difficulty of guessing. In the case of :code:`markovpass`
if trained on a reasonably representative corpus of language, over a large
enough series of decisions the probability distribution of passphrases should
look more or less gaussian (`Central limit theorem
<https://en.wikipedia.org/wiki/Central_limit_theorem>`_, right? At least I
think there are variants that apply to Markov chains. I'd love to hear from
someone who knows more about this). While a Gaussian distribution isn't a flat
distribution, it's also a long ways from my pathological example above. The
Shannon entropy given is definitely an overestimate of the difficulty in
guessing, but probably not a terrible one.

.. [1] J. L. Massey, “Guessing and entropy,” in Proc. IEEE Int. Symp.
  Information Theory, 1994, p. 204.
.. [2] D. Malone and W.G. Sullivan, “Guesswork and Entropy,” IEEE Transactions
  on Information Theory, vol. 50, 525-526, March 2004.

Todo
----

:code:`markovpass` is slow! Passphrase generation can take half a minute on a
largish corpus, which is fine for most practical purposes but hardly ideal. I
suspect that using :code:`Data.Text` instead of :code:`String` could be a lot
faster. Second the algorithm I'm using has all sorts of inefficiencies. It
should be possible to generate the Markov chain in O(n) time, and then walk it
in constant time (in the size of the corpus). One way to do this would be to
build a hash table of counts for each node then calculate the entropy and set
up a table for the `Alias Method <https://en.wikipedia.org/wiki/Alias_method>`_
for each node. In any case, the current algorithm is slow, but not impractical.

Acknowledgements
----------------

Thanks to Gavin Wahl for the idea for this project, and also for, after I had
it working, questioning the whole idea, and forcing me to think about guesswork.
