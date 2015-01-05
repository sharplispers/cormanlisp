This directory contains code specific for Corman Lisp 1.41 to enable
the SQL-ODBC library to work with that compiler. It has been tested
with Corman Lisp 1.41 and SQL-ODBC V0.85.

As Corman Lisp does not have DEFSYSTEM I've used the Corman Lisp
module mechanism to load the SQL-ODBC packages. If the forms in
SYSTEM-INIT.LISP are evaluated this will enable SQL and ODBC to work
with PROVIDE. For example, once evaluating SYSTEM-INIT.LISP you can:

  (require 'odbc)

to load the SQL and ODBC packages along with the required patches for
Corman Lisp. The forms in SYSTEM-INIT.LISP can be placed in the Corman
Lisp INIT.LISP file, which gets run whenever the Corman Lisp IDE is
started. 

The constant *sql-odbc-directory* in SYSTEM-INIT.LISP will need to be
modified to point to the directory containing the SQL-ODBC root.

Corman Lisp does not yet have support for EQL specialisers and :class
allocation slots. Workarounds are provided for these. Note that the
EQL specialiser workaround makes all method dispatch in the image
slower and more inefficient. See details in CLOS-EQL-PATCH.LISP for
more information.

Corman Lisp does not have CLOS based streams so the SQL-ODBC stream
support is not included.

Questions, comments or suggestions about the Corman Lisp specific
portions of the SQL-ODBC port can be addressed to Chris Double
(chris@double.co.nz).

Chris Double
chris@double.co.nz
1 September 2000.



