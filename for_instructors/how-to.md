# Where are my content

Lecture notes

-   `lecture_notes/`

Lab content

-   `lab/code/` stores code

-   `lab/data/` stores data

-   `lab/Aure_2015.pdf` is the paper

-   `lab/StatPrinciples_RLab.pdf` is the lab notes

# The course website

The website is linked to the github repository, specifically,

-   [Homepage](https://ocbe-uio.github.io/2022_bioinformatics_workshop/) links to `index.qmd`

-   [Preparation](https://ocbe-uio.github.io/2022_bioinformatics_workshop/part0_prep.html) links to `part0_prep.qmd`

-   [R Lab - Part I](https://ocbe-uio.github.io/2022_bioinformatics_workshop/part1_eda.html) links to `part1_eda.qmd`

-   [R Lab - Part II](https://ocbe-uio.github.io/2022_bioinformatics_workshop/part2_model.html) links to `part2_model.qmd`

-   [About](https://ocbe-uio.github.io/2022_bioinformatics_workshop/about.html) links to `about.qmd`

The overall appearance is controlled by `_quarto.yml`.

# How to modify the content

The website is made by [quarto](https://quarto.org), which is a better version of Rmarkdown. The way code chunk works is exactly the same as Rmarkdown (only that it supports a few more languages in the same file).

You might need to [install quarto](https://quarto.org/docs/get-started/) and upgrade Rstudio to the latest version, to be able to render the documents by yourself.

Make sure that the path are set properly so that files can be loaded properly.

*Deploy changes to the website*: push all modifications to github, the site will update itself momentarily (after the CI/CD are successful)

You can also let me know if you need any help with the website or text.
