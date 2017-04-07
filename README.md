# US_Congress

## Create an interactive map of Members of Congress

This project is intended to help answer the question, "Do I know anyone whose Representative is on the Xx Committee?"

The initial implementation is as an RStudio Shiny application. Data on Members of Congress and committee assignment is from 
https://github.com/unitedstates/congress-legislators which must be in a sibling directory. District boundaries are
from US Census TIGER/Line files and are included in the `data` directory, as well as a reduced version created with `rmapshaper`.
