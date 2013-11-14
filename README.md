a11y-checker
============

*Web Accessibility Evaluation library* written in [**Erlang**](http://www.erlang.org/).


The library tests web pages for conformance to the [*Web Content Accessibility Guidelines 2.0*](http://www.w3.org/TR/WCAG20/): **WGAC 2.0**.

It provides test of the level A of conformance for the HTML technology, allowing to:

  * Validate the level A of conformance for HTML.
  * Individually test the success criteria related to the level A of conformance over HTML.
  * Individually test a subset of the HTML techniques considered as sufficient by the WCAG 2.0 to achieve the level A of conformance.

The library was designed in a way that eases the addition of new levels of conformance and technologies to test.

During the development process, **Property-based testing** was used to verify the HTML techniques, making use of the [**PropEr**](http://proper.softlab.ntua.gr/) tool. An automated, random HTML generator was implemented as well during the testing phase.

*a11y-checker*, its *Property-based tests* and its *automatic random HTML generator* are licensed under **GPLv3**.

Requirements
------------

  * Erlang/OTP and *xmerl_scan* parser.
  * *PropEr* for the Property-based tests ([PropEr installation instructions](https://github.com/manopapad/proper/blob/master/README.md))


Installation
------------

*a11y-checker* comes with an EMakefile you can use to compile the modules that compose the library by typing inside an Erlang console:

    cd("path to the project").
    make:all().

To load the modules that compose *a11y-checker*, type inside the Erlang console:

    cd(ebin).
    l(wcag_2).
    l(html_wcag2).
    l(html_techniques).

If you are interested in trying the *Property-based tests*, also load:

    l(html_prop).
    l(html_gen).

and the PropEr modules:

    l(proper).
    l(proper_gen).

Once you have done this, you will have access to the APIs defined by the different modules. You can find examples of use in the next section.

Usage
-----

*a11y-checher* is composed by several Erlang modules, each of one providing different APIs.

Those modules are:

### wcag_2
Test levels of conformance and individual success criteria for different technologies.

#### Functions

    conformance_level(LevelOfConformance, TechnologyList, Content, EntryType) -> boolean()

    success_criterion(LevelOfConformance, TechnologyList, Content, EntryType) -> boolean()

functions of the last type look like  *success_criterion_n_n_n_description*
being the *n* the nomenclature given by the WCGA 2.0.

#### Parameters

* *LevelOfConformace*, currently supported level is Level A.
* *TechnologyList*, currently sopported technology is HTML.
* *Content*, an string containing the HTML or a path to a file.
* *EntryType = {string, file}* depending on the nature of *Content*.

#### Output

* *True*, if the content matches the conformance level or success criterion
* *False*, otherwise. Information of the failing HTML techniques and the failing HTML element are shown.

#### Example

    wcag_2:conformance_level(a, [html], "<html><head>Ejemplo</head><body><img>imagen</img></body></html>", string).

Output: *(success messages are omitted for brevity)*

    Failing element: [{html,1},{body,2},{img,1}]
    HTML H37: failed
    Failing element: [{html,1},{body,2},{img,1}]
    HTML H67: failed
    Success Criterion 1.1.1: failed
    .......
    HTML H25: failed
    Success Criterion 2.4.2: failed
    .......
    HTML H57: failed
    Success Criterion 3.1.1: failed
    .......
    {validation_error,{error_missing_element_declaration_in_DTD,html}
    HTML H88: failed
    Success Criterion 4.1.1: failed
    Success Criterion 4.1.2: failed
    ......
    Level A of Conformance: denied
    false

Explanation:

* *Success criterion 1.1.1.* fails because the image doesn't contain a textual alternative [(H37)](http://www.w3.org/TR/WCAG-TECHS/H37.html), or a *NULL* *alt* attribute [(H63)](http://www.w3.org/TR/WCAG-TECHS/H67.html).
* *Success criterion 2.4.2.* fails because the document doesn't contain a title  [(H25)](http://www.w3.org/TR/WCAG-TECHS/H25.html).
* *Success criterion 3.1.1. fails because the language attribute is not set in the *html* element [(H57)](http://www.w3.org/TR/WCAG-TECHS/H57.html).
* *Success criteria 4.1.1. and 4.1.2* fail because HTML is not used according to its specs, the DTD declaration is missing. [(H88)](http://www.w3.org/TR/WCAG-TECHS/H88.html).

### html_wcag2

Tests if a certain success criterion is met by the HTML of the page, for a certain level of conformance.

Its functionality is equivalent to the one provided by the *wcag_2:success_criterion* functions, because the technology currently supported is HTML.

### html_tech
Tests if a certain HTML Technique is correctly applied by the web content.

#### Functions

    success_criterion_n(XmlElement) -> boolean()

named following the nomenclature used by the WCAG 2.0 (Hnum), plus a brief description of the technique.

#### Parameters

* *XmlElement*, structure representing the HTML document, obtained using the function *scan()* defined in the module is used:

    scan (Html, type)

* *Html*, an string containing the HTML or a path to a file.
* *type = {string, file}* depending on the nature of *Html*.

#### Output

* *True*, if the HTML applied correctly the technique
* *False* otherwise. Information of the failing HTML element is shown.

#### Example
Testing
[H37 HTML technique] (http://www.w3.org/TR/WCAG-TECHS/H37.html):

    html_tech:h37_only_well_formed_images(html_tech:scan("<html><head>Example</head><body><img>Image</img></body></html>", string)).

Output:

    false

The image doesn't contain a textual alternative.

### *Property-based tests*

There are two modules related to the *Property-based tests*.
To try them you have to get PropEr installed.

#### html_gen

Automatic and random HTML generator.

##### Example

Generate random HTML:

    proper_gen:pick(html_gen:html()).

More information about [*proper_gen:pick*](http://proper.softlab.ntua.gr/doc/proper_gen.html#pick-1) and [proper_gen](http://proper.softlab.ntua.gr/doc/proper_gen.html).


#### html_prop

Defines the properties that the HTML Techniques should follow.

##### Example

Test the property related to [(H37)](http://www.w3.org/TR/WCAG-TECHS/H37.html) over randomly generated HTML:

    proper:quickcheck(html_prop:prop_well_formed_images()).

Proper looks for counterexamples (HTML code) where the property fails and provides an shrinked counterexample.

More information about PropEr [here](http://proper.softlab.ntua.gr/doc/).

Known issues
============

* *a11y-checker* uses an XML parser: *xmerl_scan* to parse the HTML content. Hence, if your page is not well-formed *a11y-checher* will fail to parse the document.

* *a11y-checker* currently supports HTML, if your page contains javascript code, parsing errors can happen when parsing the document using *xmerl_scan*.

* Sometimes we have detected problems when parsing content of certain HTML attributes if it contains special characters.

* The DTD's address specified in the HTML DOCTYPE declaration must be a local file, *xmerl_scan* doesn't have support to retrieve it from the web.
You can use: `<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "../src/html/data/DTD/xhtml1-transitional.dtd">`
