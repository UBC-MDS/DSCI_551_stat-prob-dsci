# Bayes' Theorem

**Brain teaser:** A heritable disease occurs randomly in 10% of the population. If someone has the disease, it is passed on to their children with probability 50%. A mother has 1 healthy child. Given this, what's the conditional probability that the mother has the disease? 

- Is the answer 10%? Less? More? How do we quantify it?
  - Let $M$ be the event that the mother has the disease.
  - Let $C$ be the event that the child has the disease.
  - We want $P(M\mid \textrm{not } C)$. We have $P(M)=0.1$ and $P(\textrm{not }C\mid M)=0.5$.

Solution:

$$P(M \mid \textrm{not } C) = \frac{P(\textrm{not } C \mid M)P(M)}{P(\textrm{not } C)}$$

So we still need $P(\textrm{not } C)$. This could happen in 2 ways ("law of total probability")

$$P(\textrm{not } C)=P(\textrm{not } C \mid M)P(M) + P(\textrm{not } C \mid \textrm{not } M)P(\textrm{not } M)$$

We know $P(\textrm{not } M)=1-P(M)=0.9$. And we assume $P( C \mid \textrm{not } M)=0.1$ because the child can randomly get the disease like anyone else, so then $P(\textrm{not } C \mid \textrm{not } M)=1-P( C \mid \textrm{not } M)=0.9$. Finally, then, we're left with:

$$P(M \mid \textrm{not } C) = \frac{0.5 \times 0.1}{0.5\times 0.1 + 0.9 \times 0.9}$$

Sanity check: this is less than 10%. That's what our intuition told us.

- We can get what we need using **Bayes' Theorem**.
- We've seen above that, for events $A$ and $B$, $P(A,B)=P(A\mid B)P(B)$. 
- We can also write this as $P(A,B)=P(B\mid A)P(A)$. 
- Since these are equal, we get the famous Bayes' theorem:

$$P(A\mid B)=\frac{P(B\mid A)P(A)}{P(B)}$$


```r
(0.5*0.1)/(0.5*0.1+0.9*0.9)
```

```
## [1] 0.05813953
```

Follow-up question: What's the conditional probability that the next child will have the disease?

Suppose a drug test produces a positive result with probability 0.99 for drug users, $P(T = 1\mid D = 1) = 0.99$. It also produces a negative result with probability $0.99$ for non-drug users, $P(T = 0\mid D = 0) = 0.99$. The probability that a random person uses the drug is $0.001$, so $P(D = 1) = 0.001$. What is the probability that a random person who tests positive is not actually a user, $P(D = 0 \mid T = 1)$?
