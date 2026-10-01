BESM Tools
@@@@@@@@@@

.. role:: app(strong)

Tools for the *Big Eyes, Small Mouth* RPG (BESM_).

.. _BESM: https://en.wikipedia.org/wiki/Big_Eyes,_Small_Mouth

besm4-rst and besm2-rst
=======================

Currently, this contains two programs, :app:`besm4-rst` and
:app:`besm2-rst`, which transforms BESM characters, templates, or
items in the form of a YAML_ file into three forms of
reStructuredText_ output: (1) using :app:`reST` grid tables, (2) using
`tbl <https://man7.org/linux/man-pages/man1/tbl.1.html>`_, (both in a
format similar to that used in *BESM 4E*), and (3) in the terse format
used in *BESM 1E* and *BESM 2E* products.  The :app:`reST` grid tables
allow me to also output HTML.  I use `pandoc <https://pandoc.org/>`_ to
convert these various :app:`reST` output formats into PDF and HTML.

Originally I just output :app:`reST` grid tables, but those are actually
fixed width tables in all output except HTML, and don't use up the
full width of the output line in :app:`pandoc`\ 's *ms* output, which makes
them cramped and leaves empty blank space on the right side of the
tabel.  So I modified the program to actually generate the :app:`tbl`
directly and output in a :app:`reST` “raw ms” block.  At some point I
modified the program to produce the terse format that was used in *BESM 1E*
and *BESM 2E* versions.

Test data
=========

``test-data/`` holds the YAML files ``make rst``, ``make hmm`` and the
other targets read: the ``-4e`` files for :app:`besm4-rst`, and the
``-2e`` files for :app:`besm2-rst` and its variants.

Six of the ``-2e`` files come from the golden tests of besm2_fmt, the
Ada port of :app:`besm2-rst` (``~/Repos/RPG/Tools/besm2_fmt``, its
``test/data/``), where they are also the fixtures of its two Oberon-2
ports: ``composite-2e``, ``composite-multi-doc-2e``, ``sections-2e``,
``synthetic-2e``, ``empty-items-2e`` and ``empty-strings-2e``.  With
``enyon-boase-2e`` and ``FV2021-Coleopteran-2e``, which all four have,
the 2E test data is the same here and there, apart from comments.

besm2_fmt is the reference for those golden tests, and it departs from
:app:`besm2-rst` on purpose in a few places (its Oberon-2 port's
``ADA-DIFFERENCES.md``, section 4, and section 2 for its own defects).
So :app:`besm2-rst`'s output on these files is not besm2_fmt's, and
that is accepted.  With the minus sign mapped (``-n`` is the other way
round in besm2_fmt, which uses U+2212 by default):

``composite-2e``, ``sections-2e``, ``enyon-boase-2e``, ``FV2021-Coleopteran-2e``
    The same in every format.

``composite-multi-doc-2e``
    Two documents.  :app:`besm2-rst` and :app:`besm2-rst-e` read only
    the last with the yaml egg (the default), and only the first with
    ``-f``; :app:`besm2-rst-f`, :app:`besm2-rst-f-e` and besm2_fmt read
    both (4.4).  So ``compare-fyaml``, ``compare-treefyaml`` and
    ``compare-entitytree`` leave this file out
    (``ENTITY_NAMES_2E_ONE_DOC`` in the GNUmakefile); ``compare-entity``,
    whose two programs read it the same way, keeps it.

``synthetic-2e``
    ``effective: ""`` gives ``1 ()`` here, ``1`` in besm2_fmt (4.2); a
    level and its ``(effective)`` are joined by a no-break space here,
    an ordinary space there (4.3); a header underline is as long as the
    name in characters here, in bytes there (2.2); and ``élan vital``
    sorts differently, since besm2_fmt folds case by byte (2.1).

``empty-items-2e``, ``empty-strings-2e``
    An empty list (``alternatives: []``, ``specialisations: []``), and
    attribute or defect details that come out empty (``details: ""``,
    blank details, ``elements: [""]``), are written here as an empty
    ``()`` (grid, raw ms) or a stray ``.`` before the points (terse,
    h-m-m); besm2_fmt leaves them out, as if absent (4.10).

.. _YAML: https://yaml.org/
.. _reStructuredText: https://docutils.sourceforge.io/rst.html
