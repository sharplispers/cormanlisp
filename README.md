# Corman Lisp

This is the open-source source release of Corman Lisp. The canonical
home of this release is https://github.com/sharplispers/cormanlisp.

From the [Corman Lisp 'Features' page](http://www.cormanlisp.com/features.html):

> Corman Lisp is a Common Lisp development environment for Microsoft Windows operating systems running on Intel platforms. Corman Lisp consists of a Common Lisp native code compiler for Intel processors, 80x86 assembler and disassembler, incremental linker and multi-window text editor. It requires a system running a Microsoft Windows operating system (such as Windows XP, Windows 2000, Windows ME or Windows NT). It is fully integrated with the Win32 API, and all the Windows API functions are readily available from Lisp.

> Corman Lisp incorporates state-of-the-art compiler technology to bring you a Common Lisp system unmatched on Windows platforms.

## Source release history

In June 2013, Zach Beane contacted Roger Corman about the future of
Corman Lisp. Roger replied:

> Hi Zach,

> Thanks for your interest in Corman Lisp. I have been unable to
> commit any resources to developing Corman Lisp for the last couple
> of years. I would be willing to convert it to open source, and let
> others take it over, as I don't expect to be able to get back to
> it any time soon. [...]

> I see two primary issues at this point. First, the Corman Lisp
> kernel needs to be built with Visual Studio 2005, which is an old
> version of VS. At one point I built it with VS2008 and it built
> and ran, but some problem with the FFI caused applications saved,
> and the IDE, to have problems. This type of thing is fairly common
> as Microsoft makes subtle changes to the code generation of their
> C compiler, which in turn affects the Lisp kernel which has very
> tight rules about how it expects its code to look. I did a lot to
> reduce the kernel (move things from the kernel to Lisp code, which
> is then compiled safely), but did not fully resolve the VS 2008
> issues. Now there is VS 2010 and 2012, and I never tried building
> with either of those. In general I find each newer version of VS
> is worse for native C/C++ debugging, as .NET has become
> Microsoft's primary platform for new software.

> The other issue is running on 64-bit OS. Again, the FFI misbehaves
> when you have saved an application, or just doing a bunch of FFI
> stuff in an application. It is something to do with GC and the
> 64-bit support for running 32-bit applications (which is what CL
> is). It could be related to the same issue as I encountered with
> building under VS2008.
>
> Tracking down this problem is difficult.
>
> Regarding converting to a full 64-bit Lisp, this would be a much
> bigger effort. Corman Lisp gets such good performance by a large
> amount of hand-optimized x86 assembly code. This would all have to
> be rewritten/replaced in a 64-bit Lisp.
>
> When Corman Lisp was first developed in the late 90s, there was
> really no competition on Windows platforms. Franz only had a
> 16-bit Lisp they were selling, and not really taking the Windows
> market seriously. Mac Common Lisp was on 680x0 and later PowerPC
> but didn't get to Windows for a very long time, and LispWorks
> didn't support Windows until much later. Now I think most all of
> those have good implementations on Windows (though only MCL is
> free). I have used Allegro extensively for a large contract with
> SRI and I see the value of a mature cross-platform system like
> they have. It is however extremely expensive, and developing
> applications for distribution requires lots of negotiation and
> per-license payments to Franz. Corman Lisp still has some
> advantages, I think--it is really a good hackers Lisp, when you
> want to easily get down into the assembly level, or in the guts of
> the compiler, to do almost anything. You have the power of Lisp to
> do those things with macros and a nice high-level dynamic
> environment.
>
> I definitely want to see it live on, so am open to whatever
> enables that. I don't currently derive any significant income from
> it so that's not a concern for me.

In January 2015, Roger Corman agreed to release the Corman Lisp
sources under the MIT license. He added:

> Going forward with the code, I would probably say that the first
> step is getting the kernel (Corman Lisp server) to compile using a
> new version of MS Visual Studio (such as VS2012 or VS2013)--it was
> last compiled with VS2005. I tried at one point to get it to build
> with VS2008 (and I think the project files are still there) but
> ran into some bugs and didn't resolve them.
>
> Getting the kernel to compile properly and the GC and FFI to work
> properly is tricky. The kernel is very sensitive to the generated
> code, and you pretty much have to disable all optimizations (since
> the GC depends on things like proper stack frames and the various
> code patterns when it is looking for references). The Makefiles
> and project files should have the settings set correctly (for
> VS2005) but newer compiler versions try to add new sophisticated
> optimizations, which then causes new issues.
>
> Turning off optimizations shouldn't really negatively affect
> performance, as most of the kernel code gets replaced by compiled
> Lisp code during the build process. Some functions don't, however
> (the kernel code generator, for instance) and these are probably
> the source of the compiled code sensitivity.

Roger provided the source code for Corman Lisp to Zach, who updated
the license information and posted the code to github.

## Support

Corman Lisp is no longer supported, commercially or otherwise. For
discussion of Corman Lisp, see the unofficial [Corman Lisp mailing
list](https://groups.google.com/d/forum/cormanlisp).


## License Information

Nearly all of the code is originally by Roger Corman. There are some
contributions from Vassili Bykov and Reini Urban. With their explicit
consent, their code is available under the MIT license:

      Permission is hereby granted, free of charge, to any person obtaining
      a copy of this software and associated documentation files (the
      "Software"), to deal in the Software without restriction, including
      without limitation the rights to use, copy, modify, merge, publish,
      distribute, sublicense, and/or sell copies of the Software, and to
      permit persons to whom the Software is furnished to do so, subject to
      the following conditions:

      The above copyright notice and this permission notice shall be
      included in all copies or substantial portions of the Software.

      THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
      EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
      MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
      NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
      LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
      OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
      WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

Some code is provided by Chris Double. It is available under the
following terms:

      This software is provided 'as-is', without any express or implied
      warranty. In no event will the author be held liable for any damages
      arising from the use of this software.
     
      Permission is granted to anyone to use this software for any purpose,
      including commercial applications, and to alter it and redistribute
      it freely, subject to the following restrictions:
     
      1. The origin of this software must not be misrepresented; you must
         not claim that you wrote the original software. If you use this
         software in a product, an acknowledgment in the product documentation
         would be appreciated but is not required.
     
      2. Altered source versions must be plainly marked as such, and must
         not be misrepresented as being the original software.
     
      3. This notice may not be removed or altered from any source 
         distribution.
     
zlib is included under the following terms:

      Copyright (C) 1995-2005 Jean-loup Gailly and Mark Adler

      This software is provided 'as-is', without any express or implied
      warranty.  In no event will the authors be held liable for any damages
      arising from the use of this software.

      Permission is granted to anyone to use this software for any purpose,
      including commercial applications, and to alter it and redistribute it
      freely, subject to the following restrictions:

      1. The origin of this software must not be misrepresented; you must not
         claim that you wrote the original software. If you use this software
         in a product, an acknowledgment in the product documentation would be
         appreciated but is not required.
      2. Altered source versions must be plainly marked as such, and must not be
         misrepresented as being the original software.
      3. This notice may not be removed or altered from any source distribution.

      Jean-loup Gailly        Mark Adler
      jloup@gzip.org          madler@alumni.caltech.edu

Certain code in the Modules/ and Libraries/ subdirectories carries
different licensing terms. See the individual modules and libraries
for details.

The Common Lisp HyperSpec(TM) is Copyright 1996-2005, LispWorks
Ltd. All Rights Reserved.  It is included under specific [conditions
of
use](http://www.lispworks.com/documentation/HyperSpec/Front/Help.htm#Legal). It
is complete and unmodified.
