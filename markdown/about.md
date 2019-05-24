# The Semantic Librarian

The Semantic Librarian is a search Engine built from vector-space models of semantics.

The Semantic Librarian currently searches 27,560 APA article abstracts from Experimental Psychology journals between the years 1894 - 2016.

## What is this search engine doing?

This search engine uses vector-space models of semantics to convert words, sentences, and documents into points in a high-dimensional semantic meaning space. In this space, texts that are closer together are more similar in their semantic meaning. So, this search engine computes and returns the similarities between different kinds of text:

- **Semantic Search**: Enter search terms, and compare the semantic similarity of those terms to all article abstracts in the database.
- **Article Similarity**: Search for a specific article in the database, and the articles that are most semantically similar.
- **Author Similarity**: Search for an author and find similar authors. We represent each author as the sum of all of their abstracts. Authors who have written semantically similar abstracts will be similar to each other.
- **Article Author**: Search for an article, and find similar authors. Development note: this option is "working", but does not appear to "work" as well in the way one might expect. For example, the primary author of a paper is not routinely returned as the most similar author.

## Contributors

This project is a collaboration between [Randall Jamieson](https://umcognitivesciencelaboratory.weebly.com) and Matt Cook (University of Manitoba), [Harinder Aujla](http://ion.uwinnipeg.ca/~haujla/) (University of Winnipeg), and [Matthew Crump](https://crumplab.github.io) (Brooklyn College of the City University of New York). 


