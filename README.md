# grading
Tool to generate PDF grade sheets from YAML formatted rubric and student scores/comments.

`grading` relies on pdflatex to generate PDFs.

# Usage

```
usage: grading [ <option> ... ] <filename>

<option> is one of

  -p <path>, --path-to-latex <path>
     Path to LaTeX
  -f, --force-overwrite
     Overwrite preexisting files
/ -t <output-path>, --totals <output-path>
|    output totals
| -i, --individual-grades
\    generate individual grades (default)
  --help, -h
     Show this help
  --
     Do not treat any remaining argument as a switch (at this level)

 /|\ Brackets indicate mutually exclusive options.

 Multiple single-letter switches can be combined after
 one `-`. For example, `-h-` is the same as `-h --`.
 ```
