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

This is the official site for the Tonto program package.

Tonto is available to non-commercial entities under the GPLv2 open source
license.

## Contacts

If you are experiencing technical problems you can contact Dylan Jayatilaka.

Commercial entities or those requiring consultation for using
diffraction-related applications should contact Simon Grabowsky.

## Highlights

The main feature of the package is that it can refine X-ray crystal
structures for molecular crystals using structure factors obtained from ab
initio molecular wavefunctions embedded in their own crystal field --- so
called tailor-made structure factors. This allows the refinement of
* Hydrogen atom positions and their anisotropic atomic displacement parameters
* Anharmonic vibration parameters for heavy atoms

It can also obtain “experimental wavefunctions” from X-ray diffraction
data by constraining a single determinant Hartree-Fock or density
functional theory wavefunctions to reproduce X-ray diffraction data.

It can perform a large array of properties and property densities, both
electrical and magnetic, based on the electron density and current density, and
using non relativistic and relativistic single determinant wavefunctions.

## History

Tonto was written at the turn of the century from scratch to be object-oriented
and Fortran-based. At that time it was not clear if Fortran would continue as the
dominant language for numerical analysis and high performance computing. Therefore,
bets were hedged and the code was written in a custom language `Foo` which
translated into Fortran. The intention was to be able to transform the code
into C++ or another language should that become necessary in the future. 

Fifteen years on, the situation is as unclear as it was before. Certainly
the Fortran 2008 language has evolved into `Foo` (although it still retains
some very useful mixin and proceudure annotation features). And now there
are other contenders such as the Julia language. Developments in functional
programming languages mean that the ad-hoc based `perl` translator used to for
translation can be replaced by efficient and easy-to-use monadic parsers
generators, say using the beautiful Haskell language. As a matter of fact, this
development now make it a realistic possibility for an individual or small
group to develop a fully compiled language --- say through the the LLVM compiler 
back end, exactly as the Julia folk have done.

It seems the decision to use a custom language has been vindicated.

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

