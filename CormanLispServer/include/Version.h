#pragma once

// Corman Lisp Version Information

///
#define VERSION_MAJOR 3
#define VERSION_MINOR 1
#define VERSION_PATCH 2
///

#define STRINGIZE2(s) #s
#define STRINGIZE(s) STRINGIZE2(s)

#define VERSION_STRING STRINGIZE(VERSION_MAJOR) "." STRINGIZE(VERSION_MINOR) "." STRINGIZE(VERSION_PATCH)
