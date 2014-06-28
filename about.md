---
layout: page
permalink: /about/
title: 
tags: [bio]
modified: 2014-06-07
image:
  feature: so-simple-sample-image-4.jpg
  credit: Michael Rose
  creditlink: http://mademistakes.com
---

This is the official site for the Tonto library.

Tonto is available to non-commercial entities under the GPLv2 open source
license.

## Contacts

If you are experiencing technical problems you can contact Dylan Jayatilaka.

Commercial entities or those requiring consultation for using
diffraction-related applications should contact Simon Grabowsky.

## What it does

Tonto is best known for it’s ability to:

* Refine X-ray crystal structures for molecular crystals using 
  ab initio wavefunctions --- using “tailor-made” structure factors. This
  allows the refinement of
  ** Hydrogen atom positions and atomic displacement parameters (ADPs)
  ** Anharmonic vibration parameters for heavy atoms

* Obtain “experimental wavefunctions” by constraining a single determinant
  Hartree-Fock (HF) or density functional theory (DFT) wavefunctions to
  reproduce X-ray diffraction data.

* Obtain a large array of wavefunction analyses including one- and two-electron
  properties, and bond-index analyses.

## History

Tonto was written at the turn of the century from scratch to be object-oriented
and `Fortran`-based. At that time it was not clear if Fortran would continue as the
dominant language for numerical analysis and high performance computing. Therefore,
bets were hedged and the code was written in a custom language `Foo` which
translated into Fortran. The intention was to be able to transform `Foo` code
into `C++` or another language should it become necessary.

Fifteen years on, the situation is as unclear as it was before. 

Certainly `Fortran 2008` and `Foo` have converged, but the latter still has
extra and useful mixin and procedure annotation features. And now there are
other contenders such as the `Julia` language. Developments in functional
programming languages mean that the `perl`-based `Foo` translator can be
replaced by efficient and easy-to-use monadic parsers generators, say using the
beautiful `Haskell` language. As a matter of fact, this development now make it a
realistic possibility for an individual or small group to develop a fully
compiled language --- say through the the `LLVM` compiler back end --- exactly
as the `Julia` folk have done.

It seems the decision to use a domain-specific language has been vindicated and
the future looks bright.

## Contributors

The contributors are

* Lukas Bucinsky
* Hans-Beat Buergi
* Patrick Cassam-Chenai
* Birger Dittrich
* Simon Grabowsky
* Daniel Grimwood
* Dylan Jayatilaka
* Peter Spackman
* Mark Spackman
* Mike J. Turner
* Magda Woinska
* Stephen K. Wolff

