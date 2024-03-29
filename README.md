





<!-- Improved compatibility of back to top link: See: https://github.com/othneildrew/Best-README-Template/pull/73 -->
<a name="readme-top"></a>
<!--
*** Thanks for checking out the Best-README-Template. If you have a suggestion
*** that would make this better, please fork the repo and create a pull request
*** or simply open an issue with the tag "enhancement".
*** Don't forget to give the project a star!
*** Thanks again! Now go create something AMAZING! :D
-->



<!-- PROJECT SHIELDS -->
<!--
*** I'm using markdown "reference style" links for readability.
*** Reference links are enclosed in brackets [ ] instead of parentheses ( ).
*** See the bottom of this document for the declaration of the reference variables
*** for contributors-url, forks-url, etc. This is an optional, concise syntax you may use.
*** https://www.markdownguide.org/basic-syntax/#reference-style-links
-->
[![Contributors][contributors-shield]][contributors-url]
[![Forks][forks-shield]][forks-url]
[![Stargazers][stars-shield]][stars-url]
[![Issues][issues-shield]][issues-url]
[![LinkedIn][linkedin-shield]][linkedin-url]


<h1 align="center"> actuarialmetrics (R package)</h3>

In development. Expected delivery date : August 2023. 

<!-- Access -->
## Access

```
## Install the package
devtools::install_github("Olicoside/actuarialmetrics")

## Load the package
library("actuarialmetrics")
```

<!-- About -->
## About

Package for basic metrics and graphs related to actuarial science. Useful and elegant visualisation based on `ggplot2`, but applied to actuarial science. 

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- Example -->
## Example

Here is a basic example, which shows the need for an english implementation (which should be the default)
```
table_to_g <- actuarialmetrics::get_lr_table(preds %>% 
                                 mutate(n = 1),
                               ref_name = "ign",
                               comp_name = "conf",
                               expo_name = "n",
                               loss_name = "Y",
                               n_cuts = 6)

actuarialmetrics::dbl_lift_chart(table_to_g,
                                 Prem_names = c("ign",
                                                "conf"))
```

![](picture/example_dbl_chart.png)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP -->
## Contributors
- [ ] [Olivier Côté](mailto:Olivier.cote.12@ulaval.ca)

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- ROADMAP -->
## Ideas/To do 

- [ ] Make groups based on distance metrics (k-means) for various graphs
- [ ] Make groups based on PCA? 
- [ ] Finish documentation
- [ ] Make a pretty example for the `readme`
- [ ] Arrange packages requirements for smooth use
- [ ] Add dataset to my package
- [ ] Add equation on graphs using `latex2exp`
- [ ] Add exploratory functions from industrial partnership
- [ ] Make a general function with an argument to specify the type of graph to use

<p align="right">(<a href="#readme-top">back to top</a>)</p>

<!-- CONTACT -->
## Contact

[Olivier Côté](mailto:olivier.cote.12@ulaval.ca)

Project Link: [https://github.com/OliCoSide/actuarialmetrics](https://github.com/OliCoSide/actuarialmetrics)

<p align="right">(<a href="#readme-top">back to top</a>)</p>


<!-- MARKDOWN LINKS & IMAGES -->
<!-- https://www.markdownguide.org/basic-syntax/#reference-style-links -->
[contributors-shield]: https://img.shields.io/github/contributors/OliCoSide/actuarialmetrics.svg?style=for-the-badge
[contributors-url]: https://github.com/OliCoSide/actuarialmetrics/graphs/contributors
[forks-shield]: https://img.shields.io/github/forks/OliCoSide/actuarialmetrics.svg?style=for-the-badge
[forks-url]: https://github.com/OliCoSide/actuarialmetrics/network/members
[stars-shield]: https://img.shields.io/github/stars/OliCoSide/actuarialmetrics.svg?style=for-the-badge
[stars-url]: https://github.com/OliCoSide/actuarialmetrics/stargazers
[issues-shield]: https://img.shields.io/github/issues/OliCoSide/actuarialmetrics.svg?style=for-the-badge
[issues-url]: https://github.com/OliCoSide/actuarialmetrics/issues
[license-shield]: https://img.shields.io/github/license/OliCoSide/actuarialmetrics.svg?style=for-the-badge
[license-url]: https://github.com/OliCoSide/actuarialmetrics/blob/master/LICENSE.txt
[linkedin-shield]: https://img.shields.io/badge/-LinkedIn-black.svg?style=for-the-badge&logo=linkedin&colorB=555
[linkedin-url]: https://linkedin.com/in/olivier-cote-act
[product-screenshot]: images/screenshot.png
[Next.js]: https://img.shields.io/badge/next.js-000000?style=for-the-badge&logo=nextdotjs&logoColor=white
[Next-url]: https://nextjs.org/
[React.js]: https://img.shields.io/badge/React-20232A?style=for-the-badge&logo=react&logoColor=61DAFB
[React-url]: https://reactjs.org/
[Vue.js]: https://img.shields.io/badge/Vue.js-35495E?style=for-the-badge&logo=vuedotjs&logoColor=4FC08D
[Vue-url]: https://vuejs.org/
[Angular.io]: https://img.shields.io/badge/Angular-DD0031?style=for-the-badge&logo=angular&logoColor=white
[Angular-url]: https://angular.io/
[Svelte.dev]: https://img.shields.io/badge/Svelte-4A4A55?style=for-the-badge&logo=svelte&logoColor=FF3E00
[Svelte-url]: https://svelte.dev/
[Laravel.com]: https://img.shields.io/badge/Laravel-FF2D20?style=for-the-badge&logo=laravel&logoColor=white
[Laravel-url]: https://laravel.com
[Bootstrap.com]: https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white
[Bootstrap-url]: https://getbootstrap.com
[JQuery.com]: https://img.shields.io/badge/jQuery-0769AD?style=for-the-badge&logo=jquery&logoColor=white
[JQuery-url]: https://jquery.com 
