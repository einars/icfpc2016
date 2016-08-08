Infrastructure of the ICFP-2016 contest — solver wrapper, problem storage and
information pages.

Branches:
- #master - LISP solver
- #probro - Ocaml problem generator
- #vis    - visualizer and infrastructure

all-problems:
  - full list of all problems
  /all-problems/index.php : web view of all problems
  /all-problems/leaders.php: web views of leader score approximations

blob:
  - blob.json (contest data) storage and manual updater
  /blob/index.php - problems submitted by team

problems:
  - thumbnails of problems
  /problems/index.php - big preview of first 10 problems
  make — generate missing thumbnails

render-thumb:
  thumbnail generator

submissionary:
  - wrapper for the lisp solver, keeps track of problems solved in stats.json
  

/index.html — canvas problem visualizer
