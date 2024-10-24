.. -*- mode: rst -*-

.. include:: course.links
   
.. _hellocaml:

HW1: Hellocaml
==============

Overview
--------

This project provides a refresher on OCaml programming and some
warm-up exercises involving tree manipulation and recursive
programming (both of which will be highly useful when building the
compiler). It will also familiarize you with the basic workflow of the
projects in this course, including the testing framework that we will
use to (partially) automate the grading of your projects.

Before you begin
----------------

For help on how to get started with OCaml see the :ref:`toolchain web pages <toolchain>`
and the `OCaml web site <https://ocaml.org/>`_.

Please also take some time to skim the available resources on the
course homepage -- in particular, the book `Introduction to Objective
Caml <../../../current/_static/files/ocaml-book.pdf>`_
provides a very good reference for learning OCaml. In the problems
below when you see a note like "See IOC 5.2" please refer to the
corresponding section of the book.


**Getting Started**

Unlike future projects, most of the instructions for this project are
found as comments in the source files. To get started on this project, 
run ``make`` from the project root directory (in VSCode or in a terminal) 
and then continue to the ``bin/hellocaml.ml`` file and follow the instructions 
(in comments) there.

**Building the Project**

It is recommended that you compile your projects from the command
line, using ``make``. We have included a ``Makefile`` that provides
several make targets that can help you with the homework::

  make       --  builds oatc using dune
  make dev   --  runs dune build in "watch" mode for more interactive errors
  make test  --  runs the test suite
  make clean --  cleans your project directory
  make utop  --  starts a utop for the project
  make zip   --  creates a zip file with the code you need to submit

For example, using make we can build the project and run the tests all
in one go::

  > make test
  dune build
  ... [[ warnings omitted ]]
  ./oatc --test        
  Running test Student-Provided Tests For Problem 1-3
  Running test Problem1-1
  Running test Problem1-2
  ...

Command-line Running and Testing Projects
-----------------------------------------

After compiling the project, you can run it from the command line.

The projects in this course are designed to have a single, top-level
entry point in the file ``main.ml``. Upon running ``make``, it compiles to
an executable ``main.exe``, and copies it as ``oatc`` to the root of the project.

The ``oatc`` program provides a test harness that can be used from the command
line with a variety of switches and command-line arguments, just like
any other compiler. You can always check which command-line switches
are available by using the ``-help`` or ``--help`` flags. For example,
HW1 supports only one interesting command-line option ``--test``::

 > ./oatc -help
 Main test harness 

   --test run the test suite, ignoring other inputs
   -help  Display this list of options
   --help  Display this list of options

All of our projects will support the ``--test`` option, which will
simply run the project's unit tests, print a summary of the results
and then exit. It might give output something like this (bogus sample)
that will give you some idea about how much of the project you've
completed::

 > ./oatc --test

 Test1:
   case1: failed - not equal
   case2: failed - assert fail
   case3: failed - test threw an unknown exception
 Test2:
   OK
 Test3 (3/10 points)
   case1: failed - not equal
   case2: failed - not equal
   case3: passed
 Test4-Manual (0/3 points = 0/1 tests)
 FAILED - manually: assert fail
 Test5 (??/20 points):
   Hidden
 Test5 (10/10 points):
   OK
 ---------------------------------------------------
 Passed: 5/10
 Failed: 5/10
 Score: 13/20 (given)
        ??/20 (hidden)

**Note:** problems that will be manually graded after you submit the
homework are considered to "fail" according to the test harness.
	
Once the compiler projects reach the stage where we can generate good
assembly output, ``oatc`` will support more interesting
command-line options and be able to process input files in a way that
should be familiar if you've ever used gcc or another compiler.

Grading
-------

Submit your solution to this assignment by following the :ref:`submission instructions <submit>`

**Projects that do not compile will receive no credit!**

Your grade for this project will be based on:

* 64 Points for the test cases that are visible to you
* 23 Points for the hidden test cases
* 13 Points of manually graded parts
   * 3 points for the type annotations in problem 2-1 (manually graded)
   * 5 points for proper tail call implementation in 3-4 (manually graded)
   * 5 points for "Style" and additional test cases for 4-3 and 5 (manually graded)
