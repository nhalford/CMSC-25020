Files:

interlin.txt        The transcribed Voynich manuscript

voynich_dx1         Compiled binary of the program for OS X.
                        Run with ./voynich_dx1

voynich_dx1_linux   Compiled binary of the program for Linux.
                        Run with ./voynich_dx1_linux

voynich_dx1.hs      Haskell source file. Compile with "ghc voynich_dx1"

README.txt          This file

output/             Directory containing output .dx1 files. Note that
                        this directory must exist in order for the
                        program to run: Haskell will not create the
                        directory if it does not exist.


There were a number of issues encountered when making dx1 files
from the transcription of the Voynich manuscript. The first issue
was how to deal with different hands. Some pages of the manuscript
are labeled as hand "4?" so this was taken to be separate from
hand 4. It may be interesting to compare these dx1 files to those
from hand 4. In addition, there were some pages with no hand labeled,
so these were taken as hand "0". This seems to be the only reasonable
way to handle the unlabeled hands.

The other main problem was the question of how to deal with variations,
i.e., transcriptions of the form [A|B]. I treated any string of the form
xxx[A|B]xxx as one word, regardless of what was inside the brackets.
In many cases, the brackets contained only [|.], and these strings
were still treated as one word. It may be interesting to, instead
of treating these as one word, do some sort of post-processing to
see what they are most likely to be. That is, given A[B|C]D, look
at the final dx1 file and look at the frequencies of ABD and ACD
(or in the case of A[|.]D, look at AD as compared to A and D).
This may help to reduce the ambiguity and to produce cleaner dx1 files.

Finally, one line (4238) of interlin.txt contained commas between
other characters. It was unclear exactly what these commas meant,
so they were treated the same way as other characters. It is
possible, however, that they were meant to be interpreted the same
way as periods, i.e., as spaces between words. This could easily
be accounted for, but it is probably inconsequential.
