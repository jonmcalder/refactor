# refactor 0.1.1

* Update breaks_mode with new 'default' method to emulate cut.default
* Rename existing breaks_mode method to 'spread'
* Updates to documentation to reflect these functional changes and further 
  clarify behavior
* Updates to testing of cut.integer based on the new modes
* Updates to package vignette

### Bug Fixes

# refactor 0.1.0.9001

* Style: All lines of code are now (roughly) 80 characters long, T and F changed 
  into explicit notation TRUE and FALSE
  
#### cut.integer

* floor() is used rather than round() when breaks are supplied as decimal 
  numbers
* The quantile method has been dropped

#### index_cfactor

* Arguments are only recycled if length = 1 (previously full recycling was 
  possible)
* Improve formatting of documentation and corrected spelling 
* Add more details on how this packages methods deviate from base methods
* Eliminate undocumented function arguments
* More meaningful helpfile titles
* Wrap relevant expressions in documentation so they appear as code
* Remove remaining quantile method and descriptions snippets
* Update examples so they are in line with the vignette

# refactor 0.1.0.9000

* Substantial updates to cfactor, cut.integer and cut.ordered
* More assertive tests and warnings
* Add new function index_cfactor()
* Updates to function documentation
* Updates to README
* Include package vignette with examples of the problems addressed

# refactor 0.1.0

* Updates to cut.integer and improved documentation
* Additional testing
* Include checkmate package to facilitate runtime checking of function 
  arguments