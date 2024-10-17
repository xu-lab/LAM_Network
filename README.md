# LAM_Network

## PACKAGES REQUIRED:

1. shiny (version 1.9.1)
2. igraph (version 2.0.3)
3. visNetwork (version 2.1.2)
4. DT (version 0.33)
5. shinyGizmo (version 0.4.2)
6. xfun (version 0.47)
7. readxl (version 1.4.3)
8. data.table (version 1.16.0)
9. ggvenn (version 0.1.10)
10. tidyverse (version 2.0.0)
11. shinycssloaders (version 1.1.0)


## DESCRIPTION:

### Gene Network
The app reads in a file where each row is a connection between a transcription factor and a target gene. The input file must contain one or more columns with numerals; these are interpreted as scores that the user can display.
In the main panel of the app, a directed graph of all connections between nodes (transcription factors or target genes) is displayed. The thicknesses of the edges depends on the scores for each connection. (One score set at a time is selected by the user.)

On the left sidebar of the app are the following:
- A dropdown menu that allows the user to select an input file.
- A list of nodes to subset the graph by. If the full graph is too large or cluttered, this feature will allow the user to select one of more nodes and view their interacting partners.
- A double-slider-bar to subset the graph by score. The user can choose a minimum and a maximum. The boundaries of the bar are tailored to the score set and the maximum and minimum score of the current graph—subsetted or not—displayed on screen.
- A histogram to show the distribution of scores of the edges on screen. This will inform the user if they decide to use the slider bar to further subset the graph.
- If the graph is still too large, the user can also type in a maximum number of edges to view. The edges with the highest scores are prioritized.
- A data table showing the raw data for the edges on screen.

The input file must have at least two columns: “TF” for transcription factors and “TG” for target genes.
- Nodes that are classified as transcription factors are denoted by triangles.
- Nodes that are classified solely as target genes are denoted by circles.
- There are transcription factors that also act as target genes in some cases; these are still denoted by triangles.
- The user can illuminate nodes by their type in the app. (Illuminate either only the transcription factors, or only the target genes)

If the input file contains a column titled “regulation”, this is interpreted as a column to dictate whether interactions are up or down regulated.
- A ”1” denotes an upregulated interaction, and thus, the edge will be colored green.
- A “-1” denotes a downregulated interaction, and this, the edge will be colored red.
- This column is NOT required. If it is absent, all edges will be colored light-blue.

Networks are created with the R package visNetwork.

### Compare Target Genes

The user can select an input file from a dropdown menu in the sidebar panel.
Once the file is selected, two dropdown menus will appear below the aforementioned menu, both containing the names of the transcription factors.
In the main panel, a Venn diagram will appear; each circle is titled according to the two transcription factors the user selects.
The numbers that appear in each compartment of the diagram correspond to the number of target genes activated by transcription factor 1, the number of target genes activated by transcription factor 2, and the number of target genes activated by both transcription factors 1 and 2.

A data table is displayed beneath the Venn diagram.
- The rows with target genes activated solely by transcription factor 1 are colored in blue.
- The rows with target genes activated solely by transcription factor 2 are colored in yellow.
- The rows with target genes activated by both transcription factors 1 and 2 are colored in green.
- Percentages are also displayed in each compartment of the Venn diagram for convenience.

The R package ggvenn was used for this visualization.