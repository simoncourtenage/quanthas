# QuantHas

### Description

The QuantHas project is an attempt to port the open source quantitative finance library, [QuantLib](https://github.com/lballabio/QuantLib), which is written in C++, to the functional programming language [Haskell](https://www.haskell.org/).

The project is a work in progress, with current work focussing on the QuantLib Time classes.

### Design Philosophy

There are two considerations to take into account in the design and development of QuantHas.

1. We need to be able to regression test QuantHas against QuantLib, and
2. We need to be able to keep up-to-date with changes and developments in QuantLib.

With this in mind, current development is directed by the structure of the QuantLib C++ code.  This has its own challenges - taking object-oriented code (in C++) and producing Haskell.  So this is not the best approach, because it is fairly bottom-up.  But once a significant part of the code has been translated, and the translation problem better understood, the architectural aspects will be revisited with the aim of refactoring and restructuring, whilst still achieving the two design objectives.

### Trying it out

You can build and test QuantHas using stack.

Testing uses the tasty framework - specifically HUnit support within the tasty framework.  See the testsuite directory for details.  Tests are currently unit tests, for example, of Schedule functionality.

### Update

Currently, I'm working on completing the Schedule module - which brings into QuantHas the functionality of the Schedule class from QuantLib.  This is used to create, for example, a schedule of payment dates for financial instruments.  It's not a particularly well-documented class in QuantLib, so working through all the paths in the C++ code (particularly the Schedule constructor), and deciding on the best strategy for representing the same meaning in Haskell is taking a little while. 

### Contact me

Any questions, please contact me via email (see profile) or raise an issue via github.
