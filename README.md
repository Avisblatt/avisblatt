# {avisblatt} R package 


## Structure of the Avisblatt GH Organization

The avisblatt project constists of 3 repositories: 

- avisblatt package source code repository
- avis-data 
- avis-analysis

## Installation of the package (from GitHub)

Simply run 

```
devtools::install_github("Avisblatt/avisblatt", auth_token = "YOUR_TOKEN")
```

in an R session. Because this repository is currently private, you will need an access token to do so. Here's how 
to generate a [GitHub access](https://github.com/settings/tokens) token.

Note: If you have trouble installing {devtools} or are not an {avisblatt} package developer you can use the {remotes} package which has fewer dependencies and is easy to install. 


```
remotes::install_github("Avisblatt/avisblatt", auth_token = "YOUR_TOKEN")
```



## Related Reads

- [quanteda](https://quanteda.io)
- [quanteda tutorials](https://tutorials.quanteda.io)
- [quanteda cheatsheet](https://muellerstefan.net/files/quanteda-cheatsheet.pdf)

- [SO: Clustering with a distance matrix](https://stats.stackexchange.com/questions/2717/clustering-with-a-distance-matrix)
- [DiagrammeR](https://rich-iannone.github.io/DiagrammeR/#features)


