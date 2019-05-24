The Semantic Librarian currently searches 27,560 APA article abstracts from experimental psychology journals between the years 1894 - 2016.

### Article Space

The scatterplot shows semantic relationships between the articles returned from the search. Articles that are closer together in the plot, are closer together in semantic meaning.

- **click on a dot:** and the abstract for the article will appear in the sidebar
- **number of articles:** controls how many articles are shown in plot
- **number of clusters:** controls how many natural "clusters" will be shown in different colors. We use a k-nearest neighbors clustering algorithm that find sub-groups based on a similarity analysis. This can be useful for identifying groups and themes based on semantic grouping
- **years:** The default is to compare the search terms to all articles in the database for all years. Restrict the range to only search for articles in particular years.

### Top 100 Articles

In addition to the plot, the table below the plot shows the top 100 most semantically similar articles to the search terms

### Author Space

The bottom figure shows a plot of authors in the database who are semantically similar to the search terms. Every author is treated as the sum of their abstracts. The resulting author vector is then compared to the vector for the search terms.

