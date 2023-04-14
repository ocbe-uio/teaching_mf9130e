# DO NOT EDIT

`doc` folder is generated automatically when render. Do not edit by hand.



# Where are my content

Lecture notes

-   `course_material/course_material_overview.qmd`: overview with a table. this is where you edit the links to each topic 
-   `notes_diagnostic_tests.qmd`: this is an example of `qmd` notes. renders to html.
-   `example.pdf`: an example of `pdf` notes. downloadable by students directly from the browser

Lab content

-   `lab/code/` stores code
-   `lab/data/` stores data



# The course website

The website is linked to the github repository, specifically,

-   [Home](https://ocbe-uio.github.io/teaching_mf9130e/) links to `index.qmd`

- [Get started](https://ocbe-uio.github.io/teaching_mf9130e/get_started/get_started.html) links to `get_started/get_started.qmd`

- [Course material](https://ocbe-uio.github.io/teaching_mf9130e/course_material/course_material_overview.html) links to `course_material/course_material_overview.qmd`

- [R labs and code](https://ocbe-uio.github.io/teaching_mf9130e/lab/overview.html) links to `lab/overview.qmd`, which links a bunch of sub material

  

The overall appearance is controlled by `_quarto.yml`.

# How to modify the content

The website is made by [quarto](https://quarto.org), which is a better version of Rmarkdown. The way code chunk works is exactly the same as Rmarkdown (only that it supports a few more languages in the same file).

You might need to [install quarto](https://quarto.org/docs/get-started/) and upgrade Rstudio to the latest version, to be able to render the documents by yourself.

### Recommended workflow

1. Clone the repo to your local 
2. Make a branch
3. test render `index.qmd`, see if the whole website renders
4. change something on the branch
5. test `render` the `qmd` files you modified. Make sure that the path are set properly so that files can be loaded properly
6. push to remote
7. make a pull request, someone will take over from here



### Deploy changes to the website (avoid this if you are not sure!)

push all modifications to github, the site will update itself momentarily (after the CI/CD are successful)

You can also let me know if you need any help with the website or text.
