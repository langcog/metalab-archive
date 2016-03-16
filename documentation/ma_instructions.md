Motivation and background
=========================

### Why do a meta-analysis? 

Meta-analyses (MAs) can be useful for two main purposes: (1) theory
building and evaluation and (2) practical decisions during study design.
This section starts with some basics on why single studies might not be
as reliable as an MA and then explains how exactly MAs overcome this
problem. You can either browse through the whole background section or
directly skip to the practical instructions which start on page 4.

#### 1.1. Building and evaluating theories

When thinking about development, we often wonder which abilities infants
display at what age. To this end, we regularly look at published studies
that set up smart experiments to test whether infants have specific
abilities, for example whether infants treat native vowels differently
from nonnative ones, and when that ability develops (for more details on
this specific topic see
[*inphondb.acristia.org*](http://inphondb.acristia.org/)).

When trying to solve the puzzle of language acquisition, it is crucial
to have a clear picture of infants' abilities, and when evaluating an
existing theory, we want to know whether new studies confirm or contrast
with predictions made. That is why we run experiments. However, the
results of one single experiment does not allow us to directly conclude
something about their underlying abilities: Each experiment measures
behavior of a set of infants in a very specific situation, which might
not be generalizable to other situations. Moreover, there might be a
measurement error in this one-time snapshot of reality. The biggest
problem when consulting single published studies is the false positive,
which the next section discusses, along with the two most common causes:
practices that increase the chance of a significant p-value and biases.

##### 1.1.1. False positives: The Type I Error

Every study we run has (at least) a 5% chance of telling us that infants
can do something when this is not true, according to the significance
threshold alpha (commonly set to .05). p-values that fall under this
threshold are supposed to tell us that the results we observed are not
very likely due to chance.

This likelihood becomes bigger when researchers are victims of their own
**biases** or engage in seemingly innocent and possibly common
**practices that increase the chance of a false positive**. None of
these necessarily come with bad intentions and a sense of wrongdoing, so
it is worth discussing them briefly.

Let's start with a very strong **motivation** for obtaining positive
results, independent of them being true or false: journal publications.
Articles are the key to being considered a smart and valuable scientist.
Journals, especially the big names, want to publish new, exciting, and
sometimes surprising findings! So all the incentives right now push
researchers to obtaining that wonderfully significant p-value.[^1]

**Increasing the chance of a false positive** can be due to a number of
practices, such as analyzing the same dataset in multiple ways (t-Test,
ANOVA, collapsing or splitting groups, etc etc) and only reporting (the
single) one significant outcome without correcting; or rejecting
participants based on the dependent variable (e.g., excluding all
infants that did not look at the named picture in a task that tests
infants' ability to recognize object labels).

**Biases** are nearly omnipresent in human cognition. When we expect
something to be true, we are unconsciously prone to either make it
happen (think self-fulfilling prophecies) or to perceive the world so
that it aligns with our expectations. Biases also might affect
researchers: Random patterns can become important results (it turns out
girls are able to solve this task, but boys are not!) or after months
and months of running the experiment and digging through the data the
original hypothesis and analysis plan are lost and the significant
finding (obtained through the practices mentioned in the previous
paragraph) was the thing we were looking for all along. Biases can even
go so far as to make us unconsciously influence participants or results
when we know the condition (for example being much more lenient with
babies' looking on target when the "correct" trials are presented and
being much more strict for the "incorrect" trials; looking times will
systematically differ in this case). This last bias is often avoided by
blinding the researcher to what is going on, but the other two are
harder to avoid and require conscious efforts such as pre-registering
analysis plans.[^2]

Collecting many study results from different researchers is a way to try
and make up for the possibility that biases influenced the outcome. We
can even **use MAs to check for biases**, such as asking whether a
suspicious number of p-values is just below the significance threshold
or whether results are systematically skewed in one direction. Why
biases matter is wonderfully illustrated here:
[*http://www.alltrials.net/news/the-economist-publication-bias/*](http://www.alltrials.net/news/the-economist-publication-bias/).
Checking for biased results is a whole literature on its own, but as a
start tools such as p-curving apps are easily available for every
researcher. [*http://www.p-curve.com/*](http://www.p-curve.com/) or
[*http://shinyapps.org/apps/p-checker/*](http://shinyapps.org/apps/p-checker/)
are two well-documented examples.

#### 1.2. Practical decisions during study design

##### 1.2.2. False negatives: The Type II Error 

In addition to the probability to obtain false positives, there is also
the possibility to be unable to measure an effect despite it being
there. This issue has two implications. First, not being able to
replicate a study might not mean that some previous finding is a false
positive, but it could point to noisy measures, small effects (what
effects are and how we measure them will be explained below in section
2), and consequently low power. This means that a typical infant study
which tests, often at great cost (both in personal investment and with
respect to money), between 12 and 24 infants per condition, might not be
able to reliably pick up an effect even though it is truly present when
the phenomenon in question leads only to a small change in the dependent
variable.

We often do not know about these non-significant findings because it is
quite difficult to publish them. But they might happen to all of us. MAs
help us in experiment design so we can **avoid false negatives due to
low power.** When the size of an effect is known and with a fixed
significance threshold, calculating power is straightforward. Here is a
simulation of how all ingredients fit together:
[*rpsychologist.com/d3/NHST/*](http://rpsychologist.com/d3/NHST/)

##### 1.2.3 How MAs help

To increase power and make up for small effects, we need to test more
babies. But we do not want to needlessly spend time and money in the
lab, so finding a balance is important. To this end, it is very useful
to have a good idea of the effect size in question, based on a MA. Once
we have the effect size, we can **calculate the minimal number of babies
needed to be able to observe an existing effect with sufficiently high
probability** (usually 80%).

MAs can also help with **study design**, especially when they come in
the format of a CAMA (community-augmented MA), which codes many, many
design variables. Examples include the words or sounds used, how long
trials were, etc. Instead of doing a tiresome literature review, the
most common procedure or the one associated with the biggest effect can
be obtained by looking at the data in a CAMA, which come with detailed
instructions on how to download and inspect their contents as simple
spreadsheets.

###  2. What are effect sizes?

In MAs we express the outcome of a single experiment in a way that
captures how big an effect is and how much it varies. There are 3 groups
of effect sizes: (1) effect sizes based on means, which includes Cohen's
*d* on which we focus from here on; (2) effect sizes based on binary
data; and (3) effect sizes based on correlations.

Since most developmental studies in the lab use mean responses of two
groups or of the same infant in two (or more) conditions, Cohen's *d* is
the appropriate effect size measure. In this
[*chapter*](http://www.meta-analysis.com/downloads/Meta-analysis%20Choosing%20an%20effect%20size.pdf)
and the following ones, a gentle introduction to effect sizes is
provided. Cohen's *d* is based on standardized mean differences. To get
a feel for Cohen's *d* I highly recommend to play with the
[*visualization*](http://rpsychologist.com/d3/cohend/) of RPsychlogist.

In a typical infant study, babies might hear two types of trials and the
responses to each are compared. In most papers, it is sufficient that
the difference between the trial types reaches statistical significance,
but in a meta-analyses we care about the size of this single observed
effect and its variance. This allows us to pool over several studies,
weigh each datapoint, and arrive at an estimate of the underlying, true
effect. This then allows us to calculate power and check how effect
sizes might be systematically affected by variables such as infant age
in "moderator analyses" (see
[*metalab.stanford.edu*](http://metalab.stanford.edu/)).

### 3. How to arrive at a well-defined MA research question?

It is important to strike a balance between choosing a topic that is too
broad (how do children learn words?) and too narrow (how do Dutch
children segment monosyllabic words from speech?).

Steps to build a MA
===================

### Make a decision spreadsheet and a PRISMA flowchart

It is important that you build traceability of your work from the start,
particularly since in larger MAs other people than you may finish up the
work or you want to check later on why you decided to exclude a given
paper. So to make sure that all of your decisions are recorded and
clear, make a copy of this [*decision
spreadsheet*](https://docs.google.com/spreadsheets/d/1R4zgvsTZ35nWw8Fh7NbfKaOFlkeECiz4X7ZILVAe9FY/edit?usp=sharing).
Don’t forget to rename it, to give us a “viewing” link, and clean it up
as follows.

Step 1: Click on “File” and select the “Make a copy…” option

![](media/image04.jpg){width="4.2755686789151355in"
height="3.2031255468066493in"}

Step 2: In the window that appears, change the name to something like
“MA\_TOPIC"

![](media/image03.jpg){width="4.588542213473316in"
height="3.392189413823272in"}

Step 3: Click on the blue button “Share” on the top right.

Step 4: In the ensuing menu, click on “Get shareable link” on the top
right

Step 5: Copy the link and send it to us.

![](media/image05.jpg){width="5.220304024496938in"
height="3.7343755468066493in"}

Step 6: Clean up

The model spreadsheet contains some fake entries and notes. Our
recommendation is, so as not to get confused, to remove the instructions
found on the top lines of each sheet and the fake information that is
already entered - except for a couple of exceptions: the pink columns
(A, B and W) in the Relevant\_studies\_search sheet contain formulas
that may be useful to you. So you might want to delete the contents of
the other columns and keep those two in order to reuse the formulas.

\*\*\*\*\*\*\*\*\*\*\*\*

Additionally, make a copy of [*this
flowchart*](https://docs.google.com/presentation/d/1DKY8BTZZ82bGyGwpGsyyzsilOqE1F1NcDLTKWtCe9AY/edit?usp=sharing),
rename and share as you did for your spreadsheet above. This figure
gives you an overview of the process, and you will be filling in the
boxes with the right numbers as you go along so that people who continue
this MA and/or those interested in assessing this work can make sure
that you followed the procedure.

### Build your initial study list 

The goal of this step is to put together a list of publications that you
will look at (abstract only in step 3, and for a subset of those in full
in step 4) and consider for inclusion. In a typical MA, you make the
most comprehensive list possible in order to answer a specific research
question and/or to cover a given phenomenon. This typically means going
through 1,000 abstracts, and reading in full 100 papers.

### Screen papers

In this step, you will go through the initial list you put together in
step 2, and make decisions to include/exclude papers, mostly based on
the abstract. In addition to creating your sample for data entry (step
4) you will start honing your *inclusion criteria*. Typically, these
will include:

-   **a homogeneous scientific question**: make sure you have clearly
    > defined the purview of e.g. cross-situational learning (e.g., this
    > name itself is vague to those outside the domain, so define it in
    > a more specific way: “exposure to sets of images paired with
    > wordforms with the goal of studying word-form image association,
    > but crucially multiple images are shown at once (unlike e.g. the
    > switch procedure)”)

-   **a homogeneous infant population**: typically-developing children,
    > between the ages of XX and YY (the precise ages may stem from your
    > seminal paper; perhaps to start with, you could set the maximum to
    > 36 months, the minimum to 0 months); consider whether you also
    > need to restrict the sample based on infants’ native language *on
    > theoretical reasons*

The last one is perhaps the trickiest. Staying close to your seminal
paper will allow you to reduce the amount of variation in your sample
due to methodological “details”, and to make it easier for yourself to
enter data, because all the results will be structured in similar ways.
But it’s important to know that this is a potential source of bias. For
instance, you could decide that you will only input data using a
specific kind of artificial language because you know that papers not
using this language have smaller effects. This will end up being a
self-confirmation exercise -- unless there are a priori strong
theoretical reasons to exclude other kinds of language /// to assume
that the learning algorithms attributed to the infant cannot be
generalized to these other languages.

Every time you make a decision regarding these and other key criteria,
remember to note it in your decision spreadsheet, in the last sheet
called “Notes\_inclusion”. For example, mine looks like this:

  **Question**                            **Decision**                                                                                                                                    **Date**
  --------------------------------------- ----------------------------------------------------------------------------------------------------------------------------------------------- ------------
  **a homogeneous scientific question**   learning of speech sound categories, where the categories are represented by a multimodal versus unimodal distribution of acoustic correlates   10/19/2015
  **a homogeneous infant population**     typically-developing children, between the ages of 0 and 36 months                                                                              10/19/2015
  **a homogeneous procedure**             passive exposure in the lab, testing via any behavioral or non-behavioral method                                                                10/19/2015

Having explained the goal of this step, let us move on to how exactly
you could go about looking through your initial list and making
decisions. As before, we propose two potential routes, one without and
one with little programming.

***What if the title and abstract doesn’t allow me to decide?***

Then play it safe and include the paper to check based on the full text.

***What if the title and abstract doesn’t allow me to decide, but in
fact I know the paper and I know it needs to be excluded?***

Then you probably have already seen the full text of the paper, so say
“yes” for the screening decision, and then “no” for the full-text
decision.

### Decide on data structure for MA and create the spreadsheet

We are hoping that eventually all of these MAs may be included in
[*MetaLab*](http://metalab.stanford.edu), so we ask you to use the [*MA
template*](https://docs.google.com/spreadsheets/d/12Y_2BcFSu48t0F8a_xrY1Ro2fJoCIV1h8O627WNcrjY/edit?usp=sharing)
(create a copy, as you did in step 1), and follow the [*coding
book*](https://docs.google.com/document/d/1szXxrv75qTVx-4adlLrwmjWSDIBJfMH09dzh9733mog/edit?usp=sharing).
Ideally, you would code all potentially relevant moderator variables
(e.g., experimental manipulations) in addition to the core
characteristics (columns in red; e.g. means). However, in the interest
of time, you can get started with the core characteristics only.
Remember once more to give us viewing rights (see step 1 for
instructions).

### Retrieve and enter studies

In a typical full MA, you go through the whole list and only then start
entering. The procedure is as follows. Go back to your spreadsheet, and
for each study that has been decided as a “yes” during screening, try to
retrieve the full text for the paper as you normally would (e.g., search
through scholar.google.com; regular google; your institution’s library,
etc.) If you cannot retrieve it, update your spreadsheet sheet
Relevant\_studies\_search to mark this paper as “no” in column F
entitled “Fulltext\_retrieved”. If you want, you can contact the authors
to try to get the full text from them, in which case you can note this
on column G.

If you do find the full text, go through the paper to find the first
experiment reported. You will enter all experiments and conditions one
at a time, and fill in their information in the MA spreadsheet you
created in step 4.

IMPORTANT: You should work backwards from the **results** section: look
at what dependent measures are reported fully enough that you will be
able to extract an effect size from them. The following information
allows one to calculate an effect size (we are sticking to experimental
designs, since most of our MAs are experimental):

-   ****between-participant studies**: **Means and SDs (not SEs!) of the
    > dependent variable for each infant group** are all that is
    > required for the calculation of Cohen's d. Sometimes, means and
    > SDs are not available as numbers. If there are clear figures, you
    > can try to estimate means and SDs using** [**this online
    > app**](http://arohatgi.info/WebPlotDigitizer/)*. If you decide to
    > estimate values from figures, add a column to keep track of this.
    > Finally, **t or F values for the *main* effect in combination with
    > sample sizes** can be used to calculate Cohen's d. Note them
    > when available. *

-   ****within-participant studies**: Effect sizes for this type of
    > study are calculated the **same way as in between-participant
    > studies**, but in order to calculate the weight of these studies
    > the **correlation** between the first and second measurements is
    > required (to account for the amount of
    > within-participant variation). Since this measure is usually not
    > reported, we provide below median and range for correlations found
    > in existing MAs. **

    -   *Infant word segmentation from native speech: 0.641 (range:
        > 0.140 to 0.921)*

    -   *Infant vowel discrimination (native and nonnative): 0.496
        > (range: -0.413 to 0.855)*

At the beginning, we can help you decide what kind of experiment you are
looking at, and what kind of procedure you should follow.

When entering papers, please remember a key thing: all analyses are done
by machines, and machines cannot read text! So if a column is “numeric”,
please do ***not*** enter things that aren’t numbers (such as text,
spaces, \~, etc). This is particularly important for the dependent
measures!

At this stage, you might find that a given paper does not contain the
right information for being included. In this case, you can and should
exclude it. If you have already started entering it, you can leave the
information you entered and put in “comments” that the entry is
incomplete (although if you followed our advice above, you won’t have
wasted time entering it!). Remember to update your spreadsheet with each
paper you read and made a decision on.

If you have questions at this stage, please check our
[*FAQ*](https://docs.google.com/document/d/1_7mgeaARlYK6TecyUqzk9SSr0pLAkV3pQZE4qYT7CMA/edit?usp=sharing).
If you find your answer, great -- if not, tell us about it, and we’ll
answer you (and add it to the FAQ).

### 6. Calculate Effect Sizes

We use R to calculate effect sizes. For now, each MA does so
independently, see
[*https://github.com/christinabergmann/InWordDB/wiki*](https://github.com/christinabergmann/InWordDB/wiki)
for an example (not using the metalab template, though!).

We recommend the following for an introduction to effect sizes:

1\. Textbooks are great to get a basic overview of how to calculate
effect sizes. We consulted: Lipsey, M. W. & Wilson, D. B. (2001).
Practical meta-analysis. Thousand Oaks, CA: Sage.

2\. A great primer and a spreadsheet document to calculate effect sizes
by hand can be found via: D. Lakens. (2013). Calculating and Reporting
Effect Sizes to Facilitate Cumulative Science: A Practical Primer for
t-tests and ANOVAs . Frontiers in Psychology 4:863. Material:
[*osf.io/ixgcd/files/*](https://osf.io/ixgcd/files/)

3\. Since textbooks do not cover every possible question that different
meta-analysts may encounter, we turned to articles for more specific
questions. We found this article useful for considering the
comparability of effect sizes from within- and between-participant
designs: Morris, S. B., & DeShon, R. P. (2002). Combining Effect Size
Estimates in Meta-Analysis With Repeated Measures and Independent-Groups
Designs. Psychological Methods, 7(1), 1805-125. doi:
[*10.1037//1082-989X.7.1.105
*](http://dx.doi.org/10.1037//1082-989X.7.1.105)

Resources
=========

[*MA
template*](https://docs.google.com/spreadsheets/d/12Y_2BcFSu48t0F8a_xrY1Ro2fJoCIV1h8O627WNcrjY/edit?usp=sharing)

[*Coding
book*](https://docs.google.com/document/d/1szXxrv75qTVx-4adlLrwmjWSDIBJfMH09dzh9733mog/edit?usp=sharing)

[*Interrogating PubMed via a
script*](https://gist.github.com/mcfrank/c1ec74df1427278cbe53)

[*Selecting for
inclusion*](https://www.getdatajoy.com/project/561388dfb485274e40055563)

MA
[*FAQ*](https://docs.google.com/document/d/1_7mgeaARlYK6TecyUqzk9SSr0pLAkV3pQZE4qYT7CMA/edit?usp=sharing)

Example MAs:
[*InWordDB*](https://docs.google.com/spreadsheets/d/1XN6VaYRs7CStrINct_rr2d6rh6WN28xFiIGB5T1vkzY/edit?usp=sharing)

[*Instructions for creating a
CAMA*](https://sites.google.com/site/infantdbs/create-your-own-cama)
(including further resources)

[^1]: People are trying to change this but nonetheless common practice
    is still this way.

[^2]: Some further reading on biases:
    [*https://explorable.com/research-bias*](https://explorable.com/research-bias)
    and
    [*http://www.nature.com/news/how-scientists-fool-themselves-and-how-they-can-stop-1.18517*](http://www.nature.com/news/how-scientists-fool-themselves-and-how-they-can-stop-1.18517)
