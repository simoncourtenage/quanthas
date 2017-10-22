# QuantHas

### Description

The QuantHas project is an attempt to port QuantLib C++ code to the functional programming language Haskell.

This project is a work in progress.  Much of the code written so far is in draft form.  The approach taken so far has been
to read through the Quantlib C++ code and work out the best way to represent it in Haskell.  This is not the best approach,
because it is fairly bottom-up.  But once a significant part of the code has been translated, and the translation problem better understood, the architectural aspects will be revisited.

### Update

22/10/17 - the current work has concentrated on the Time classes in QuantLib.  Most of the Calendar and Date
functionality exists.  However, the Schedule class is only partially complete (this is what I am working on now.)

### Contact me

Any questions, please contact me via email (see profile) or raise an issue via github.
