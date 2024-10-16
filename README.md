# CPSC 312 Project

Project Template for CPSC 312 Projects. Use this for both your proposal and final project submission.

(Since you are submitting a link to a specific commit, we will be able to tell the difference.)

The template to edit begins below. You can delete this header section once you've begun.

We will post some additional material to include when you reach the final project stage.

# Insert My Excellent Project Title Here

This will be replaced with a very brief (one paragraph, 2-3 sentences) overview of the project.

Leave the following sentence in so you can easily link back to the requirements and *especially* rubric while editing your project:

This project is in fulfillment of the [CPSC 312 2024W1 project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html).

## Team Members

Our team is:

+ Team Member Name 1 (student # 1): optional awesome nickname 1
+ Team Member Name 2 (student # 2): optional awesome nickname 2
+ Team Member Name 3 (student # 3): optional awesome nickname 3
+ If you have a fourth member: Team Member Name 4 (student # 4): optional awesome nickname 4
+ If you have a fifth member: Team Member Name 5 (student # 5): optional awesome nickname 5

We call ourselves: optional awesome team name.

## Acknowledgments

We surely built on the work of others! Here are resources and people we got support from:

+ A list of acknowledgments with a brief explanation of support received, e.g.:
  "We used Github Copilot extensively to generate first-pass implementations of chunks of the project that we had specified.
  We then manually adjusted and adapted these to our needs."

## Product Pitch

Replace this with a pitch for your project and the problem it solves. This is your vision for what the project
would like like as a complete product, ready for awesome action. (Yes, awesomeness seems to be a theme.)
It may be as short as a couple of paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Be sure that this touches clearly on the [project requirements](https://steven-wolfman.github.io/cpsc-312-website-2024W1/project.html#project-requirements).

Good goals to aim for are from the top two rubric items for proposal grading:

> Exciting and language-appropriate product idea tackling a problem that is clearly compelling to a significant audience.

Or:

> Solid and language-appropriate product idea with a problem that is of real interest to the submitting team.

(It's easy to focus on the product and not the problem. Remember to include both!)

## Minimal Viable Project

Replace this with a description of the minimal viable project you will actually build for CPSC 312 (if this becomes your final project).
It may be as short as a few paragraphs, or it may be longer. It should **definitely** take less than 4 minutes
to read carefully and thoroughly.

Make clear:
+ how this builds meaningfully toward your product pitch above, without being nearly as much work,
+ how it builds on the strength and power of the language, and
+ how it leads naturally to learning and applying some new element of the language (including what that element is!)

Good goals to aim for are from the top two rubric items for proposal grading:

> The minimal viable project (MVP) builds on the strengths and power of the language in exciting ways that will clearly lead to excellent learning for students.

Or:

> The MVP clearly builds significantly on the language and will lead in interesting and natural ways to learning for the students.

## Proof of Concept

Replace this with a description of your proof-of-concept. This may be as short as a few paragraphs, or it may be longer.
It should **definitely** take less than 4 minutes to read carefully and thoroughly, though working through and running the
code may take an extra 4 minutes. (Your guidance and links should make it easy for us to work through the code.)

Tell us:

+ what key element of your project the proof-of-concept focuses on
+ what makes that such an important element
+ how completing this gives you confidence that, with sufficient work, you could complete the full (minimal viable) project

Include links (likely even line-level links, which are easy to create in Github) throughout to critical pieces of
the code to make it easy for us to understand what you've accomplished and how it fulfills the requirements.

Also include instructions for us to test and run your code. (See our guidelines below.)

A good goal to aim for is the top rubric item from proposal grading:

> Fully functional proof-of-concept is easy to use and review, and it clearly demonstrates a key element necessary for the overall project.

### How to test and run the code: Haskell

Replace this section with instructions to us for how to test and run your code.

As it is currently set up, editing works best if you first `cd` into the `haskell` subdirectory and open VS Code on that directory (`code .`). There is a `Makefile` with some helpful aliases, but you can also just use `stack` as normal.

Note: We expect to be able to test your code by running `stack test`. Included among your tests should be some that demonstrate the core functionality of your code. (We will be running `make haskell-eval` from the project root.)

We should be able to further explore your code's functionality by running `stack ghci`, and you should instruct us on some interesting cases to try.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!

### How to test and run the code: Prolog

Replace this section with instructions to us for how to test and run your code.

We have set up a simple test file for you to extend using [Prolog Unit Testing](https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)) library for testing. Included among your tests should be some that demonstrate the core functionality of your code. Please remove the example tests before you submit or you will lose marks. (We will be running `make prolog-eval` from the project root.)

In the `prolog` directory, you can run `make test` to run the unit tests. You can also load the test file into the swipl repl with `make test-repl` and in that repl you can run `run_tests.` to run those tests.

If you include instructions different from these, be **absolutely sure** that they will work well for us in whatever environment we run your code and that they will be as easy to use as the instructions above!
