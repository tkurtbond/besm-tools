BESM Tools
@@@@@@@@@@

.. role:: app(strong)

Tools for the *Big Eyes, Small Mouth* RPG (BESM).

besm4-rst and besm2-rst
=======================

Currently, this contains two programs, :app:`besm4-rst` and
:app:`besm2-rst`, which transforms BESM characters, templates, or
items in the form of a YAML_ file into three forms of
reStructuredText_ output: (1) using :app`reST` grid tables, (2) using
`tbl <https://man7.org/linux/man-pages/man1/tbl.1.html`_, (both in a
format similar to that used in *BESM 4E*), and (3) in the terse format
used in *BESM 1E* and *BESM 2E* products.  The :app:`reST` grid tables
allow me to also output HTML.  I use `pandoc <https://pandoc.org/`_ to
convert these various :app:`reST` output formats into PDF and HTML.

Originally I just output :app:`reST` grid tables, but those are actually
fixed width tables in all output except HTML, and don't use up the
full width of the output line in :app:`pandoc`\ 's *ms* output, which makes
them cramped and leaves empty blank space on the right side of the
tabel.  So I modified the program to actually generate the :app:`tbl`
directly and output in a :app:`reST` “raw ms” block.  At some point I
modified the program to produce the terse format that was used in *BESM 1E*
and *BESM 2E* versions.

.. _YAML: https://yaml.org/
.. _reStructuredText: https://docutils.sourceforge.io/rst.html
