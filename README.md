This package is under construction! 
=================================
- There are tests, but they are not complete.
- Functions should work, but may change or break at any time.
- Documentation may be wrong or missing.

If you intend to use this package before version 1.0, email me (conway.max1@gmail.com), and I'll prepare a consistent version for you.

![under construction](http://images.all-free-download.com/images/graphiclarge/under_construction_template_241387.jpg)

GDMOr
=====
This package is intended to provide tools for multi-objective metabolic engineering.
The functions can be split into optimization functions, and exploration functions:
- `GDMO` provides a generic, pure R multi-objective optimization framework, based on NSGA-II. It uses `mature`, `nondomsort`, `reproduce`, and `select` to perform different parts of the optimization procedure.
- The other functions are used to analyse and understand the results of multi-objective optimization. This optimization can be using this package, or outside packages, including the use of the `load_matlab` function