<h1 align="center">
  <div style="margin:10px;">
    <img src="https://github.com/fediazgon/yelp-business-viz/blob/assets/logo.png?raw=true" alt="project-logo" width="200px">
  </div>
  yelp-business-viz
</h1>

<h4 align="center">
Visualizing <a href="https://www.yelp.com/dataset/challenge">yelp challeng's</a> data with <a href="https://shiny.rstudio.com/">Shiny!</a>
</h4>

<p align="center">
  <a href="#live-demo">Live Demo</a> •
  <a href="#getting-started">Getting started</a> •
  <a href="#idioms">Idioms</a> •
  <a href="#authors-es-blue_heart-it">Authors</a>
  <!-- <a href="#license">License</a> -->
 </p>

## About This Project
The aim of this project is to develop an interactive visualization tool going trough
all the design abstraction levels needed to properly structure a complex
visualization project.

## Live Demo

The easyest way to get a grasp on what the project does is to look at [our live demo](https://fediazgon.shinyapps.io/yelp-business-viz/).

## Getting Started
We suggest you to install [RStudio](https://www.rstudio.com/).

Once installed you can easily install shiny issuing the following command in RStudio
console:
```R
> install.packages("shiny")
```

## The Dataset
The dataset used is the [yelp visualization challenge dataset](https://www.yelp.com/dataset/challenge) which collects various informations
about businesses around the world and check-in events generated by users.

## Questions
Good visualization are aimed to answer defined questions.
For this reason we selected some interesting questions concerning the dataset:

1. How are the business geographically distributed?
2. What is the distribution of review score on a determined geography? Are there areas that are more "picky"?
3. Is there a relationship between the number of check-in and the number
of reviews?
4. Which is the relationship between the most common business cate-
gories?
5. In which time of the day the customer check-in? Does it coincides with
the opening hours?
6. Is it true that different categories have different opening hours? Like
bars opens until late and restaurants close sooner.

## Idioms

The project implements various idioms (plots) in different tabs.

#### Map
The map tab aims to answer the first three questions. For that reason we used
facetig of multiple idioms. A choroplet map for the geographical distribution of
the businesses, an histogram of the review score and a scatter plot between the logarithm of the number of reviews and the number of check-in.

Businesses can be filtered by review score and the size and color of the bubbles
can encode different values.

All the idioms are coordinated, as can be seen in the following demo:
![map-demo](https://github.com/fediazgon/yelp-business-viz/blob/assets/demo1.gif?raw=true)



#### Adiacency Matrix
We represented the relationship between the businesses categories as a network, having as nodes the names of the categories and a link every time two categories appears in the same business. The weight
associated to link will increase each time the link appears in the dataset.
Being inspired by the ["les miserables" co-occurrence adiacency matrix](https://bost.ocks.org/mike/miserables/), we decided to use the same idiom.
The user will be able to manipulate the view by selecting the sorting criteria
of the matrix by name of category, frequency and cluster.

![adjacency-matrix-demo](https://github.com/fediazgon/yelp-business-viz/blob/assets/demo2.gif?raw=true)

#### Heatmap

For investigating the similarities between check-in hours and opening hours
we decided to apply some aggregation building two matrix with day of the
week and hour as rows and columns index. We calculated a scalar field where
each cell rapresent the number of check-in in that hour/day and the number
of business open in that hour/day. The user have the capability to filter by
selecting the category to analyze. We choose a purely sequential colormap without a central reference. The user is also be able to apply smoothing to the heat map to
ease the detection of particular patterns, like you can see in the demo below.

![heatmap-demo](https://github.com/fediazgon/yelp-business-viz/blob/assets/demo3.gif?raw=true)


## Learn More
In case you want to learn more about the design decisions taken and the selection
of the idioms used we seggest you to read the [report](report.pdf) we
redacted.


<!-- The easiest way to run this project is by cloning the project locally, create a fat jar using Maven and executing the shell
script that can be found on the project's root directory.

```bash
mvn clean package
./run.sh
```

It is possible to active/deactivate the explore stage with the `--explore` flag (add/remove this flag inside the
`run.sh` script).

The output should be similar to the following one:

![project-demo](https://github.com/fdiazgon/fdiazgon.github.io/blob/master/art/yelp-business-viz-demo1.gif?raw=true)

You can also import it to your favourite IDE, but keep in mind that the program requires one argument, which is the dataset
to process. You can find multiple valid datasets at this link: [Airline On-Time Statistics and Delay Causes](http://stat-computing.org/dataexpo/2009/the-data.html).

Be aware that it can take a lot of time with a large dataset (14 models are trained with 10 folds cross-validation).
This is why we included a small `tuning.csv` file in the `raw` folder. Please, consider using this dataset to check
that the program works properly.

## Validation process

The general workflow on the program is shown in the image below:

![project-flow](https://github.com/fdiazgon/fdiazgon.github.io/blob/master/art/sparkml-flights-delay-flow.png?raw=true)

Hyperparameter tuning and model selection are carried out using cross-validation on the training dataset. In this stage,
a grid search is performed using two different models: Linear Regression and Random Forest (you can add your own
extending the `CVTuningPipeline` class). Finally, the test error of the best model is obtained using the test set. -->

## Authors :es: :blue_heart: :it:

* **Fernando Díaz**
* **Giorgio Ruffa**

<!-- ## License

This project is licensed under the MIT License - see the [LICENSE.md](LICENSE.md) file for details -->
