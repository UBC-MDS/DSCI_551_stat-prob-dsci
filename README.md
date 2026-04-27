# Welcome to Regression II!

DSCI 562 explores regression techniques that go beyond ordinary least-squares (OLS). In particular, we will ask questions like:

- What if the response is still continuous but constrained (e.g., non-negative), or the observations are no longer independent?
- What if the response is binary, a count, or categorical?
- What if the data are censored (for example, due to limits of detection or incomplete follow-up)?
- What if we are interested in something other than the conditional mean (such as conditional quantiles) because different data science applications call for different inferential responses?

To address these settings, we will study practical extensions of classical linear regression, including generalized linear models (GLMs), mixed-effects models, local regression, survival analysis, and quantile regression, as well as methods for handling missing data.

## High-Level Goals

By the end of the course, students are expected to:

- Describe the risk and value of making parametric assumptions in regression.
- Fit model functions that represent probabilistic quantities besides the mean.
- Identify situations where OLS regression is sub-optimal, and apply alternative regression methods that better address the situation.

## Lecture Topics

This course occurs during **Block 4** in the school year. Typically, you should review these notes before each lecture. 

| Lecture Topic/Notes | Required Readings | Optional Readings |
| :---:   | :---: | :---: |
| Link Functions and Count Regression | [`lecture1` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture1-glm-link-functions-and-count-regression.html) | [Chapter 1 (*Review of Multiple Linear Regression*): BMLR](https://bookdown.org/roback/bookdown-BeyondMLR/ch-MLRreview.html) (*as a review*) <br> [Chapter 6 (*Logistic Regression*): BMLR](https://bookdown.org/roback/bookdown-BeyondMLR/ch-logreg.html) (*as a review*) <br> [Chapter 5 (*Generalized Linear Models: A Unifying Theory*): BMLR](https://bookdown.org/roback/bookdown-BeyondMLR/ch-glms.html#learning-objectives-4) (*short chapter*) <br> [Chapter 4 (*Poisson Regression*): BMLR](https://bookdown.org/roback/bookdown-BeyondMLR/ch-poissonreg.html) |
| Model Selection and Multinomial Logistic Regression | [`lecture2` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture2_glm_model_selection_multinomial.html) | [Chapter 2 (Beyond Least Squares: Using Likelihoods): BMLR](https://bookdown.org/roback/bookdown-BeyondMLR/ch-beyondmost.html#summary-of-model-building) <br> [Faraway: 5.1 (Multinomial Logit Model)](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000296029&title=Extending%20the%20Linear%20Model%20with%20R%3A%20Generalized%20Linear%2C%20Mixed%20Effects%20and%20Nonparametric%20Regression%20Models) |
| Ordinal Logistic Regression | [`lecture3` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture3_glm_ordinal_regression.html) | [Faraway: 5.3 (Ordinal Multinomial Responses)](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000296029&title=Extending%20the%20Linear%20Model%20with%20R%3A%20Generalized%20Linear%2C%20Mixed%20Effects%20and%20Nonparametric%20Regression%20Models) |
| Linear Mixed-effects Models | [`lecture4` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture4_linear_mixed_effects_models.html) | [Initial motivation (ISL): 3.3.3 Potential Problems (*2. Correlation of Error Terms*)](https://www.statlearning.com) <br> [Chapter 8 (*Introduction to Multilevel Models*): BMLR (from 8.1 to 8.8)](https://bookdown.org/roback/bookdown-BeyondMLR/ch-multilevelintro.html) |
| Survival Analysis | [`lecture5` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture5_survival_analysis.html) |See Kleinbaum and Klein (2005) in [reference material](#reference-material) |
| Local Regression | [`lecture6` notes]([notes/lecture6_local_regression.qmd](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture6_local_regression.html)) | [ISL](https://www.statlearning.com) 7.2. Step Functions <br> [ISL](https://www.statlearning.com) 7.6. Local Regression |
| Quantile Regression | [`lecture7` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture7_quantile_regression.html) | [Fahrmeir (2013)](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000904267&title=Regression%20Models%2C%20Methods%20and%20Applications) <br> 10. Quantile Regression (except 10.2.2. Bayesian Quantile Regression) |
| Missing Data | [`lecture8` notes](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/lecture8_missing_data.html) | See van Buuren (2012) in [reference material](#reference-material) |

## Regression Mind Map

[**Here**](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/appendix-reg-mindmap.html) is a mind map we created to summarize all regression models to be covered in this course.

## Cheat Sheet

[**Here**](https://pages.github.ubc.ca/mds-2025-26/DSCI_562_regr-2_students/notes/appendix-reg-cheatsheet.html) is a cheat sheet we created to summarize the main formulas and concepts covered in DSCI 562.

## Deliverables

This is an **assignment-based course**. The following deliverables will determine your course grade:

| Assessment       | Weight  | 
| :---:            | :---:   |
| Lab Assignment 1 | 12.5%   |
| Lab Assignment 2 | 12.5%   |
| Lab Assignment 3 | 12.5%   |
| Lab Assignment 4 | 12.5%   |
| Quiz 1           | 25%     |
| Quiz 2           | 25%     |

## Lab Topis

|      | **Lab Topic**        |
| :---:| :---:            |
| **1**    | Introduction to Generalized Linear Models <br> (Lectures 1 and 2) | 
| **2**    | Ordinal and Mixed-effects Regression Models <br> (Lectures 3 and 4) |
| **3**    | Survival Analysis and Local Regression <br> (Lectures 5 and 6) |
| **4**    | Quantile Regression and Missing Data Imputation <br> (Lectures 7 and 8) |

## Use of Generative AI (GenAI)

GenAI tools (e.g., ChatGPT) can be useful when used responsibly. In this course, you may use these tools to gather information, review concepts, or brainstorm. If you use GenAI in any graded work, you must clearly cite it (including what tool you used and how you used it). What is **not** permitted is submitting work that is primarily written by a GenAI tool; for example, copying and pasting AI-generated responses into an assignment. For details and expectations, please review the [**MDS policies**](https://ubc-mds.github.io/policies/).

## Reference Material

- Agresti, A (2013). *Categorical Data Analysis*, John Wiley & Sons, Incorporated. ProQuest Ebook Central.
    * The e-book is available through the [UBC Library](https://ebookcentral.proquest.com/lib/ubc/detail.action?docID=1168529). You can obtain a PDF copy with your CWL account. This book is helpful for GLMs with discrete responses.
- Collett, D. (2003). *Modelling Binary Data (2nd ed.)*. Chapman and Hall/CRC.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0001459465&title=Modelling%20binary%20data).
- Fahrmeir, L. (2013). *Regression Models, Methods and Aplications*. Springer Berlin Heidelberg.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000904267&title=Regression%20Models%2C%20Methods%20and%20Applications). You can obtain a PDF copy with your CWL account.
- Faraway, Julian J. (2005). *Extending the Linear Model with `R`: Generalized Linear, Mixed Effects and Nonparametric Regression Models*, CRC Press LLC. ProQuest Ebook Central.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000296029&title=Extending%20the%20Linear%20Model%20with%20R%3A%20Generalized%20Linear%2C%20Mixed%20Effects%20and%20Nonparametric%20Regression%20Models). You can obtain a PDF copy with your CWL account. This book is great for learning how to work within the `R` environment with the models we will be working on. Its approach is essentially practical.     
- Gelman, A. and Hill, J. (2007). *Data Analysis Using Regression and Multilevel/Hierarchical Models*. Analytical Methods for Social Research. Cambridge University Press.
    * The physical book is available through the [UBC Library](https://go.exlibris.link/1yK2jY6n). This book is pretty useful and practical as introductory material.
- Hastie, T., Tibshirani, R., and Friedman, J. H. (2009). *The Elements of Statistical Learning: Data Mining, Inference, and Prediction*, Springer Publising Company, Incorporated.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000145389&title=The%20Elements%20of%20Statistical%20Learning%3A%20Data%20Mining%2C%20Inference%2C%20and%20Prediction%2C%20Second%20Edition).
- James, G., Witten, D., Hastie, T., and Tibshirani, R. (2014). [*An Introduction to Statistical Learning: with Applications in `R`*](https://www.statlearning.com). Springer Publishing Company, Incorporated.
- Kleinbaum, D. G. and Klein, M. (2005). *Survival analysis : A Self-Learning Text*. Springer.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/?sid=sersol&SS_jc=TC0000320278&title=Survival%20analysis%20%3A%20a%20self-learning%20text). This book is a good start for Survival Analysis:
        + **Chapter 1 (Introduction):** Introduction to Survival Analysis (I), Censored Data (II), Terminology and Notation (III).
        + **Chapter 2 (Kaplan-Meier Curves):** Review (I), Example of Kaplan-Meier Curves (II), General Features of  Kaplan-Meier Curves (III), Confidence Intervals for Kaplan-Meier Curves (VII and VIII).
        + **Chapter 3 (Cox Proportional Hazards Model):** Example of Cox Proportional Hazards Model (I), Formula of Cox Proportional Hazards Model (II), Why the Cox Proportional Hazards Model is Popular (III), Estimation of the Cox Proportional Hazards Model (IV).
        + **Chapter 7 (Parametric Model):** Overview (I), Relationship Between the Probability Density Function with Hazard and Survival Functions (II), Weibull Example (IV).
- Rousseeuw, L. P. J. and Leroy A. M. (2003). *Robust Regression and Outlier Detection*. Hoboken, NJ : Wiley-Interscience.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/log?L=GW2JH3XR2C&D=ZEEST&J=TC0000239030&P=Link&PT=EZProxy&H=48e3fef958&U=https%3A%2F%2Fezproxy.library.ubc.ca%2Flogin%3Furl%3Dhttps%3A%2F%2Fonlinelibrary.wiley.com%2Fdoi%2Fbook%2F10.1002%2F0471725382). You can obtain a PDF copy with your CWL account.
- Roback, P. and  Legler, J. (2020). [*Beyond Multiple Linear Regression*](https://bookdown.org/roback/bookdown-BeyondMLR/).
- Rubin, D. B. (1987). *Multiple Imputation for Nonresponse in Surveys*. Wiley.
    * The e-book is available through the [UBC Library](https://gw2jh3xr2c.search.serialssolutions.com/log?L=GW2JH3XR2C&D=ZEEST&J=TC0000340639&P=Link&PT=EZProxy&H=6c88ebd0c3&U=https%3A%2F%2Fezproxy.library.ubc.ca%2Flogin%3Furl%3Dhttps%3A%2F%2Fonlinelibrary.wiley.com%2Fdoi%2Fbook%2F10.1002%2F9780470316696). You can obtain a PDF copy with your CWL account.
- van Buuren, S. (2012). [*Flexible Imputation of Missing Data*](https://stefvanbuuren.name/fimd/):
    * **1.1 The problem of missing data.**
    * **1.2 Concepts of MCAR, MAR and MNAR.**
    * **1.3 Ad-hoc solutions.**
    * **1.4 Multiple imputation in a nutshell.**

## Recommended Course Reviews

This course is entirely taught in `R` (we will follow the [`tidyverse` style guide](https://style.tidyverse.org/index.html)) with a reasonable mathematical and statistical basis. We strongly recommend reviewing the following courses:

- **DSCI 551: Descriptive Statistics and Probability for Data Science**, for basic statistical concepts and familiarity with the mathematical notation.
- **DSCI 552: Statistical Inference and Computation I**, for statistical inference concepts with a frequentist approach.
- **DSCI 561: Regression I**, since the topics of this course follow the same thread.
- **DSCI 531: Data Visualization I**, for plotting tools using the package `ggplot2`.
    
## Dataset References 

These are the papers from which each dataset used in the lectures comes from. If you are interested in knowing more about them, you can obtain a PDF copy of each paper with your CWL account via the UBC library:

- Brockmann, H.J. (1996). [Satellite Male Groups in Horseshoe Crabs, Limulus polyphemus](https://ubc.summon.serialssolutions.com/2.0.0/link/0/eLvHCXMwrV3JasMwEBUlpZBLl7Sl6YY_oE4sy5sgFEpICKU9NadcjFYSmtghCyRf0t-tJC_EPhRaepMHS8jSSPM0M3oGALkdx67tCYRyKV0GacCp9CBX60RCySMcYiigy2qpOq_F1ZiMLqL0v-mFYrZvvd4JXXcPEnOUZbU1z6W-fxdoh6eO-XQUvDyGCnXrbK_hxCkDDCHKgs9KH20FelDOR_pzWxXbdYhljTEanoF10e8iC6V2SbDK9Pg_H3gOTnPsar1kynYBjkTSAs1yC923wMkkNaVL8NVbkNXn8wcxjJ8b0euaZysTvyuzVJUYH9i6Kpsl1ijVOSbTtPZ2f6V6nYuectnbbLGdb2tNLNP5fjkVi1J-BcbDwbg_svOfQNhMIRdsu4KHEnIsIiQYJoGQvsKQmAuOsE8lojRnrPEIlZQHHIUEEs8PBVHQjaJr0EjSRNwAS8ebmMNk4DHkRYgQSt2ARoxRnzuhJG2AitmNlxnVR3x4REI41sMf6-GP8-GPd20QmVn7RZV4MB7p0u3fq96BZpY-rn1B96CxWW3FgyGOeDR6_g1rIQSN). *Ethology*, 102: 1-21.
- Deb, P. and Trivedi, P. (1997). [Demand for medical care by the elderly: a finite mixture approach](https://ubc.summon.serialssolutions.com/2.0.0/link/0/eLvHCXMwnV1tS8MwEA5uftAvvk6cL6N_oK5L0pfJUEr34gQZ4j75JaZLCsOtzqng_os_1kuTDjsVwU-lF9oL5Mg9x909hxDBZ469cick3G0GggcJ9YXHSUIDb6RyXn6DgsPmcqVUx89bY1SVZVYmmCX1AS_FE1nHOHCxiy9nz7aaHqWyrGaURgmVwEI1Te7yPlYM7BpLEtvHLi54IF2E-AWZUuNautvoId9FXlOy0vJX5G38x3Z30JaBnVao7WQXrcl0D23kXckv--ijNeXzx4u2nPJUtOrZi6VlAGstk88pLqjepaIkXlgAKK2OGv09WZi1c7MYWt2xQrjFT27G7yqJURSGhuncSCto2O0MoyvbTG6wAT4Ejg0xVSAVdTyn2PeJkBC4yKRBhUdHHuFurPphMSAzAXCPOwnAGgHOWkoVrwEoPUDl9CmVh6rySgDkGIlmTByaSCfmIwExYRx7SSBjX1bRID9MNtP8HEwzMWPG7vpRX6XYmwxgnMtUTY0DD8wIIw3CWNhvs-uwQ6kDEodFA4ZZr4oq2REuf2fOr4puMxv5puYPLT8qMZKjX3Qdo01Nk6sKK09Q-XX-Jk8zXogaWm9H94NeLbPxT5yWAM4). *Journal of Applied Econometrics*, 12(3), 313-336.
- Grunfeld Y. (1958). [The determinants of corporate investment](https://www.proquest.com/docview/301899386?pq-origsite=summon&accountid=14656). Ph.D. thesis, Department of Economics, University of Chicago.
- Harrison, D. and Rubinfeld, D.L. (1978). [Hedonic prices and the demand for clean air](http://ubc.summon.serialssolutions.com/2.0.0/link/0/eLvHCXMwnV07T8MwELYQDLDwKCDeyghDaOy0TixVSICourBVYrRc50wBNVRtQeLfc-fEpYAQiMF5nB9JbOf8Obn7zFgqzpP4i04wgANV7iQgPC-gZTOligRsAiYRhfWLOSya6pz_8EOfyyZhgjiRSp5m-RmteClj0sBpm5NBV-_2aq6H88QvLTfPEBznfijk88BE9pkWQd-nMWplAmOwC-NPd4PdBS-eYHjyxS_wG7njH59pk63XkDS6rPrQFluCssFWg8fytMF2bz684TBhrQ6m22zWGZnJ00UPCiLY7TT9WTR8JmP6-6iKHBNr0TTEmbKIEHLWcQWMUBDiED7XcrwNUwZxJTIPk1qww_rdm_51L66XcYh5KomtVGZGtJwcEO-NQplVvCDqQwlKcGvBtp2yhjxghZFu4KRSTqZcDFomFZDusuXyuYQ9FqlMICISMLA0L3WpqrhpXAvz2MLl-ywOTajHFVmHDlZsVMea6lhnufZ1rAWmD-38x_RZ6Ay6BigV8NDYor_kvPR9Z34ZAHgEqmT9qlPTxvCGgeOcHncPdIhhjCHnWKrQw9no4N9XP2Rr3DMp05ejI7Y8m7zAsaeZOPGvjN9evwO2Ww2r). *Journal of Environmental Economics and Management*, 5, 81–102.
- Mangasarian, O. L., Street, W. N., and Wolberg, W. H. (1995). [Breast cancer diagnosis and prognosis via linear programming](http://ubc.summon.serialssolutions.com/2.0.0/link/0/eLvHCXMwlV1Nb9QwEB2xPSA4tHQLohRKDoDgsDSJndiRKlApVBx74PNk2bGDKui2jbf8Ff4uM46tbpZKFZdIO55NvNLLeLx-8waAla_z2UpMEAZXtlZKzllZGSk7WTBdtDa3ts6N61aoOnUqjSGWZaAJhkN9zJfML7dHCi-yfnt-MaPmUXTIGjtpTGAi2cDr-rKkvFsPbQwYBpyafxsvQMTDbPEOVxE5yQSH8iZKHH2iKv4TrsMadLQBKk03kU9WagPHAo___7vuwXpMT7ODAU-bcMvNp3A7seOnsJG6QGQxKEzh7pKk4RQ2o91nL6Oi9ast-LN_qvufb94RAX6xvxc-ZIPtkFDXj23vB_rfiR-b9dxmx_3ZdUO_T_TYgFtsfIXHtuOBinaK84wD9-Hz0YdPhx9nsSPEDPeBJWmp6pJ3VS1tzlvR8Ua6WljWlm3RaMkMs52R1jmJWGu4a6wwtugaaauGtQWaH8Da_GzuHkJWVlpwbgpthKOz3aZsmeRWC9mJxnG7DS8STNT5IPyhaMOEO0xF_WkUZ4qrSuTomEB0k-MzgpiK3UXx4un_F_9DX3qvDjCPw2yPMbxfcCPwLXrd6lgngdMmqa5lx6cJqyoiNTzQLz3xeRq4YWZbAYxXXgGJ27CT8K5iZPOqJEFAUnl8dP2XduDOUPVPjObHsLboL92TIGmxCxPx9TteMcDshnf0L2aRR50). *Operations Research*, 43(4), 570-577.
- Wolberg, W. H. and Mangasarian, O. L. (1990). [Multisurface method of pattern separation for medical diagnosis applied to breast cytology](https://libkey.io/libraries/498/articles/35797998/full-text-file?utm_source=api_542). *Proceedings of the National Academy of Sciences of the United States of America*, 87(23), 9193–9196.
- Wood, P. (1967). [Algebraic Model of the Lactation Curve in Cattle](https://ubc.summon.serialssolutions.com/2.0.0/link/0/eLvHCXMwfV1LS8NAEF6kIHhRW98voic9xGYzyW4WilCqxYN4ED2XzXRXim2qrRX99242SW0i7TGzEwiZycw3zOQbQsC_9txKTIg8yYw_U2awHBUyFIwr1JE0RZEG3sfKqM7FkoY-RE3fQBQWSFulc56WW0-Pt39DHRXe5YJuduHGcgJK5zDRgLs5k-ZCduluZWuKpgVvwvdP9a-_f9SNK554m2zmINNpZ15RJ2sqaZB1O-yJ0wap5x_01LnMWaevdshHayQnbzft4WvaSh5gq2mvnUycrkwblkVj7Rjw6DxIzLr55dPObPKlyqJB4nQsZXIu3iUv3bvnzr2b72Nw0WR5z6WxgV_UZ5iCCm1qF8VBMlTgaRUjgKAKgcUgI_SBa8H6QkrsM8Yx5MA57JFaMk7UAXGCCEMQ0gQ3oYKYU6GFBMG0L1CHmseH5LwwUu89o93o2XY5RL3ifRqdwnordPYzs841ipOjpSfHZMOEJZ4mKBqckNrnZKZOLSnDmXW2X2pLzng). *Nature*, 216, 164–165.

## Policies

See the general [MDS policies](https://ubc-mds.github.io/policies/).

## Attribution
    
The course is built upon previous years' materials developed by previous instructors.

## License

© 2026 G. Alexi Rodríguez-Arelis, Payman Nickchi, Rodolfo Lourenzutti, and Vincenzo Coia.

Software licensed under [the MIT License](https://spdx.org/licenses/MIT.html), non-software content licensed under [the Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International (CC BY-NC-SA 4.0) License](https://creativecommons.org/licenses/by-nc-sa/4.0/). See the [license file](LICENSE.qmd) for more information.
