The AllegroServe Webserver
copyright (c) 1999,2000 Franz Inc.



!!! Very Important Note !!!!!!

     This readme.txt is for people receiving the binary only
     distribution of iserve.

     If you received a source distribution, then follow
     the instructions in source-readme.txt instead.

!!! Very Important Note !!!!!!






This is a distribution of the current state of the AllegroServe web server.
This is not a finished product.   We are distributing this so that
people can see what directions we are taking in our design.


== The files in this distribution:

aserve.fasl -- This includes the iserve web server and htmlgen html generation
    code.

doc/aserve.html -- documentation on the web server
htmlgen.html -- documentation on the html generation system

examples.cl -- load this into lisp to publish sample urls.  Read  this file
            to see how publishing is done.

examples.fasl -- just a compiled version of examples.cl 


other files are present to support the examples in examples/examples.cl.



== running the web server

1. cd to the directory containing the distribution and start Allegro cl 5.0.1
   (or start Allegro cl 5.0.1 and use the toplevel ":cd" command to cd 
   to the directory containing the iserve distribution).

2. load aserve.fasl
    
        user(1): :ld aserve.fasl

3. load the examples (either the compiled or source version)

        user(2): :ld examples/examples

4. start the webserver

        user(3):  (net.aserve:start :port 8010)


5. go to a web browser and select this machine and the port you chose:
   if the web browser is on the same machine you can give it:

        http://localhost:8010/



Notes:
    steps 3 and 4 can be interchanged.

    if you're running on a PC (or running as root on Unix) you can allocate 
    port 80, so you don't have to specify a port when running
    the iserve:start function.




    
       


