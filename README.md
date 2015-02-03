# Software Foundations Grader

`sf-grader` is a simple program for grading student submissions of
exercises in Software Foundations. It compares the students' solutions
against the reference ones in the Software Foundations sources,
outputting a report with the total score of the student.

## Requirements

- OCaml > 4.0
- Coq 8.4
- The Software Foundations sources

## Compiling

Just hitting `make` should be enough

## Usage

Make sure that your Software Foundations sources are compiled. To
grade file `File.v`, type

    ./grader --sf-path <path-to-sf-sources> File.v

where `<path-to-sf-sources>` is the directory that contains the SF
sources. Notice that the submission file name should match *exactly*
the name of the corresponding SF file; in order for the above to work,
we assume that there is a file named `File.v` that has been
successfully compiled into a `File.vo` in the SF directory. We also
assume that the original SF file has been correctly annotated with
grading directives for each exercise (see below).

If it runs successfully, the grader should produce two files on the
same directory as the submission file, `File.out` and
`File.res`. `File.out` contains the output produced while grading the
file (mostly useful for debugging), whereas `File.res` contains the
grading report, telling how many points the student scored on each
exercise, how many points the assignment was worth in total, and which
exercises are left to be graded manually.

If you want, you can specify once and for all where the SF sources
live by creating a `.sf-grader` file on your home directory that looks
like this:

    sf-path = <path-to-sf-sources>

Then, you can invoke the grader by entering

    ./grader File.v

### Canvas Support

If you happen to be using [Canvas][1] for your class, the grader can
process the zip file downloaded from Canvas containing all the
student submissions for a given assignment at once. Just run

    ./grader --sf-path <path-to-sf-sources> submissions.zip \
        [-o <result-path>]

This will create a new directory `<result-path>`, with one
subdirectory of the form `firstname-lastname` for each student found
in the zip file. The student submissions will be unpacked on the
corresponding student directory and graded there. If `<result-path>`
or any of the student subdirectories exist already, they will just be
updated, with the Coq files that are present in the zip being
overwritten, but nothing else. If omitted, the `-o` defaults to
`submissions`.

## Syntax

Each exercise to be graded must be explicitly marked in the
corresponding SF file with a list of one or more _grading
directives_, which can have the following form:

    (* GRADE_THEOREM <n>: <theorem-name> [( <axiom-1> ... <axiom-n> )] *)
    (* GRADE_TEST <n>: <test-fun> <exercise-name> *)
    (* GRADE_MANUAL <n>: <comment> *)

(See file `Basics.v` for some examples.)

All the directives that correspond to an exercise _must_ be enclosed
by that exercise's opening and closing tags (i.e., between `(* EXn
..*)` and `(** [] *)` tags). Also, notice that right now cannot span
over more than one line (patches are welcome).

The `<n>` argument on each tag is an integer saying how many points
that item is worth. Right now, we make the total number of points on
each exercise be equal to its number of stars, but this is not required.

`GRADE_THEOREM` ensures that the student submission contains a
definition named `<theorem-name>` whose type matches the one of the
corresponding term in the original SF sources. It also checks that it
wasn't admitted or contains any axioms. If you want to allow specific
axioms, you can mark those in a space-separated list enclosed by
parentheses.

`GRADE_TEST` looks for a definition `<test-fun>` in the SF sources,
which should have type `A -> bool`. It then looks for a definition
named `<exercise-name>` in the student's submission, and tries to run
`<test-fun>` on `<exercise-name>`. If the term evaluates to `true`,
then the corresponding points are added to the score. (See
`equiv_classes` on `Equiv.v` for an example.)

Finally, `GRADE_MANUAL` is just a reminder for a part of an exercise
that should be graded manually. The `<comment>` argument in the
directive is printed in the grading report to remind the human grader
what to look for when going through the exercise.

### Caveats

When referring to an identifier inside a module, you must write it
down fully qualified, with all module names preceding it, _even when
the grading directive occurs inside that module_.

  [1]: https://canvas.instructure.com
