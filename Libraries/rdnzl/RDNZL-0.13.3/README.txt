Installation
------------

First, put the file 'RDNZL.dll' somewhere where the foreign language
interface of your Lisp can find it.  A safe bet is to put it in the
folder where your Lisp image starts up.

Probably the easiest way to install RDNZL is to LOAD the file
'load.lisp' which comes with the distribution.  Evaluate a form like

  (load "c:/path/to/rdnzl/load.lisp")

or use the facilities of your IDE to LOAD this file.

This should compile and load RDNZL on most Common Lisp
implementations.

As an alternative you can use ASDF, RDNZL comes with an ASDF system
definition file 'rdnzl.asd'.


Documentation
-------------

Complete documentation for RDNZL can be found in the 'doc' folder.

RDNZL also supports Nikodemus Siivola's HYPERDOC, see
<http://common-lisp.net/project/hyperdoc/> and
<http://www.cliki.net/hyperdoc>.
