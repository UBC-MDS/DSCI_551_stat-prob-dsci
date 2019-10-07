# Depicting Uncertainty

_September 9, 2019_

Welcome to the course! The __syllabus__ is on the [README of the `_students` repo](https://github.ubc.ca/MDS-2019-20/DSCI_551_stat-prob-dsci_students).

Today's topics: probability, followed by distributions. 

## Lecture Learning Objectives

From today's lecture, students are expected to be able to:

- Identify probability as a proportion that converges to the truth as you collect more data. 
- Calculate probabilities using the inclusion-exclusion principle, the law of total probability, and probability distributions.
- Convert between and interpret odds and probability. 
- Specify the usefulness of odds over probability.
- Be aware that probability has multiple interpretations/philosophies.
- Calculate and interpret mean, mode, entropy, variance, and standard deviation, from both a distribution and a sample.

(Hint: we make the quizzes based on lecture learning objectives)

## Thinking about Probability

### Defining Probability (5 min)

I like to play Mario Kart 8, a racing game with some "combat" involved using items. In the game, you are given an item at random whenever you get an "item box".

Suppose you're playing the game, and so far have gotten the following items in total:

|Item                                  | Name    | Count|
|:------------------------------------:|:-------:|:----:|
| ![](img/banana.png)  | Banana  |     7|
| ![](img/bobomb.png) | Bob-omb |     3|
| ![](img/coin.png)   | Coin    |    37|
| ![](img/horn.png)   | Horn    |     1|
| ![](img/shell.png)  | Shell   |     2|
| Total: | | 50 |

Attribution: images from [pngkey](https://www.pngkey.com/detail/u2w7e6o0i1q8i1y3_randome-clipart-mario-kart-mario-kart-8-deluxe/).



Questions that we'll address:

- What's the probability that your next item is a coin? 
- How would you find the _actual_ probability? 
- From this, how might you define probability?

In general, the probability of an event $A$ occurring is denoted $P(A)$ and is defined as $$\frac{\text{Number of times event } A \text{ is observed}}{\text{Total number of events observed}}$$ as the number of events goes to infinity.

### Calculating Probabilities using Logic

We'll look at two laws for calculating probabilities of events. Suppose the table below show the true probabilities of each item. Also, let's add some properties to these items. 

|Item                                  | Name    | Probability| Combat Type  | Defeats blue shells |
|-----------------------------------:|:-------:|:----------:|------|----|---|
| ![](img/banana.png) | Banana  |     0.12| contact | no |
| ![](img/bobomb.png) | Bob-omb |     0.05| explosion  | no |
| ![](img/coin.png)   | Coin    |     0.75| ineffective | no |
| ![](img/horn.png)   | Horn    |     0.03| explosion | yes |
| ![](img/shell.png)  | Shell   |     0.05| contact | no |

Disclaimer: I don't think these are the true probabilities, but I'm pretty sure the coin probability is correct, as long as you're in the lead.

#### Law of Total Probability (5 min)

- According to this table, are there any other items possible? Why or why not?
- What's the probability of getting something other than a coin? How did you arrive at that number?

Concept: When partitioning the _sample space_ (= the set of all possibilities), the probabilities of each piece should add to one. That is, in this case,
$$1 = P(\text{Banana}) + P(\text{Bob-omb}) + P(\text{Coin}) + P(\text{Horn}) + P(\text{Shell}).$$

A special case of this involves the _complement_ of an event. This partitions the sample space into two -- for example, getting a coin or not a coin. For a general event $A$, the law becomes: $$1 = P(A) + P(\neg A),$$ where $\neg$ means the complement (read "not").

#### Inclusion-Exclusion (5 min)

Let's answer these questions by counting:

1\. What's the probability of getting an item that has an explosion combat type?

2\. What's the probability of getting an item that is both an explosion item _and_ defeats blue shells?

This is written $P(\text{explosion} \cap \text{defeats blue shells})$, where $\cap$ means "and".

3\. What's the probability of getting an item that is an explosion item _or_ an item that defeats blue shells?

This is written $P(\text{explosion} \cup \text{defeats blue shells})$, where $\cup$ means "or".

In general, we can answer the third question with the _inclusion-exclusion_ principle: for events $A$ and $B$, 
$$P(A \cup B) = P(A) + P(B) - P(A \cap B).$$

We can extend this to three events, too:
$$P(A \cup B \cup C) = P(A) + P(B) + P(C) - P(A \cap B) - P(B \cap C) - P(A \cap C) + P(A \cap B \cap C).$$


### Comparing Probabilities (8 min)

True or False (2-3 min):

> Suppose Vincenzo often wins at a game of solitaire, but that Tom is twice as good as Vincenzo. This means that $P(\text{Tom wins}) = 2 \times P(\text{Vincenzo wins})$.


Probability is quite useful for communicating the chance of an event happening in an absolute sense, but is not useful for comparing probabilities. Odds, on the other hand, are useful for comparing the chance of two events. If $p$ is the chance that Vincenzo wins at solitaire, his _odds of winning_ is defined as $$\text{Odds} = \frac{p}{1-p}.$$ This means that, if his odds are $o$, then the probability of winning is $$\text{Probability} = \frac{o}{o+1}.$$

For example, if Vincenzo wins 80% of the time, his odds are $0.8/0.2 = 4$. This is sometimes written as 4:1 odds -- that is, _four wins for every loss_. If Tom is twice as good as Vincenzo, it's _most useful_ to say that this means Tom wins twice as many games before experiencing a loss (on average) -- that is, 8:1 odds, or simply 8, and a probability of $8/9=0.888\ldots$.

### Interpreting Probability (5 min)

Thought experiment:

1. What's the probability of seeing a 6 after rolling a die?
2. I roll a die, and cover the outcome. What's the probability of seeing a 6 after I uncover the face?

No philosophy is "wrong"! But why is this relevant in practice?

- It often doesn't actually make sense to talk about _the_ probability of an event, such as the probability that a patient has a particular disease. Instead, it's a belief system that can be modified.
- It influences our choice of whether we choose a _Bayesian_ or _Frequentist_ analysis. More on this later in MDS. 


## Probability Distributions

So far, we've been discussing probabilities of single events. But it's often useful to characterize the full "spectrum" of uncertainty associated with an outcome. The set of all outcomes and their corresponding probabilities is called a __probability distribution__ (or, often, just __distribution__). 

The outcome itself, which is uncertain, is called a __random variable__. (Note: technically, this definition only holds if the outcome is _numeric_, not categorical like our Mario Kart example, but we won't concern ourselves with such details)

When the outcomes are _discrete_, the distributions are called __probability mass functions__ (or _pmf_'s for short).

### Examples of Probability Distributions (3 min)

__Mario Kart Example__: 

The distribution of items is given by the following table:

|Item                                  | Name    | Probability|
|-----------------------------------:|:-------:|:----------:|
| ![](img/banana.png) | Banana  |     0.12| 
| ![](img/bobomb.png) | Bob-omb |     0.05| 
| ![](img/coin.png)   | Coin    |     0.75|
| ![](img/horn.png)   | Horn    |     0.03| 
| ![](img/shell.png)  | Shell   |     0.05|

The distribution of combat type is given by the following table:

| Combat Type    | Probability| 
|:-------:|:----------:|
| contact  |     0.17| 
| explosion |     0.08| 
| ineffective    |     0.75|

The distribution of defeating blue shells is given by the following table:

| Defeats blue shells | Probability |
|-----|------|
| no  | 0.97 |
| yes | 0.03 |

__Ship example (New)__:

Suppose a ship that arrives at the port of Vancouver will stay at port according to the following distribution:

| Length of stay (days) | Probability |
|---|------|
| 1 | 0.25 |
| 2 | 0.50 |
| 3 | 0.15 |
| 4 | 0.10 |

The fact that the outcome is _numeric_ means that there are more ways we can talk about things, as we will see.


### Measures of central tendency and uncertainty

(3 min)

There are two concepts when communicating an uncertain outcome:

- __Central tendency__: a "typical" value of the outcome.
- __Uncertainty__: how "random" the outcome is.

There are many ways to _measure_ these two concepts. They're defined using a probability distribution, but just as probability can be defined as the limit of a fraction based on a sample, these measures often have a _sample version_ (aka _empirical version_) from which they are derived. 

As such, let's call $X$ the random outcome, and $X_1, \ldots, X_n$ a set of $n$ _observations_ that form a _sample_ (see the [terminology page](https://ubc-mds.github.io/resources_pages/terminology/#sample) for alternative uses of the word _sample_).

#### Mode and Entropy (5 min)

No matter what scale a distribution has, we can always calculate the mode and entropy. And, when the outcome is categorical (like the Mario Kart example), we are pretty much stuck with these as our choices.

The __mode__ of a distribution is the outcome having highest probability.

- A measure of central tendency.
- The sample version is the observation you saw the most.
- Measured as an _outcome_, not as the probabilities.

The __entropy__ of a distribution is defined as $$-\displaystyle \sum_x P(X=x)\log(P(X=x)).$$

- A measure of uncertainty.
- Probably the only measure that didn't originate from a sample version (comes from information theory).
- Measured as a transformation of probabilities, not as the outcomes -- so, hard to interpret on its own.
- Cannot be negative; zero-entropy means no randomness.

#### Mean and Variance (10 min)

When our outcome is numeric, we can take advantage of the numeric property and calculate the _mean_ and _variance_: 

The __mean__ (aka expected value, or expectation) is defined as $$\displaystyle \sum_x x\cdot P(X=x).$$

- A measure of central tendency, denoted $E(X)$.
- Its sample version is $\bar{X} = \frac{1}{n} \sum_{i=1}^n X_i,$ which gets closer and closer to the true mean as $n \rightarrow \infty$ (this is in fact how the mean is originally defined!)
- Useful if you're wanting to compare _totals_ of a bunch of observations (just multiply the mean by the number of observations to get a sense of the total).
- Probably the most popular measure of central tendency.
- Note that the mean might not be a possible outcome!

The __variance__ is defined as $$E[(X-E(X))^2],$$ or this works out to be equivalent to the (sometimes) more useful form, $$E[X^2]-E[X]^2.$$

- A measure of uncertainty, denoted $\text{Var}(X)$.
- Yes! This is an expectation -- of the squared deviation from the mean.
- Its sample version is $s^2 = \frac{1}{n-1} \sum_{i=1}^n (X_i - \bar{X})^2$, or sometimes $s^2 = \frac{1}{n} \sum_{i=1}^n (X_i - \bar{X})^2$ -- both get closer and closer to the true variance as $n \rightarrow \infty$ (you'll be able to compare the goodness of these at estimating the true variance in DSCI 552 next block). 
- Like entropy, cannot be negative, and a zero variance means no randomness.  
- Unlike entropy, depends on the actual values of the random variable.

The __standard deviation__ is the square root of the variance.

- Useful because it's measured on the same scale as the outcome, as opposed to variance, which takes on squared outcome measurements.


Note: you may have heard of the __median__ -- we'll hold off on this until later.
