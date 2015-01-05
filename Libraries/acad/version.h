// some common definition (for headers and resources)
#define STRINGIFY(x) "x"

// #define ALL_IN_ONE_FILE
#define CONSOLE_APP

#define VERMAJOR   0
#define VERMINOR   2

/* built no. 0-999 */
#define BUILD      10

// for the resources
#define VER_FILEVERSION			VERMAJOR,VERMINOR,0,BUILD
#define VER_FILEVERSION_STR		" 0.2.010\0"
#define VER_FILEBASE_STR		"cormanlisp"
#define VER_PRODUCTVERSION		VERMAJOR,VERMINOR,0,BUILD
#define VER_PRODUCTVERSION_STR  	VER_FILEVERSION_STR

#define VER_COMPANYNAME_STR		"x-ray\0"
#define VER_PRODUCTNAME_STR		"CormanLISP.ARX\0"
#define VER_LEGALCOPYRIGHT_STR  "© 1999 Reini Urban and Roger Corman\0"
#define VER_COMMONCOMMENTS_STR  " See http://www.cormanlisp.com and http://xarch.tu-graz.ac.at/autocad/lsp_tools/#cormanlisp\0"
