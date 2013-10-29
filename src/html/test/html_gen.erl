%%% Copyright 2013 Antia Puentes <antiapuentes@gmail.com>
%%%
%%% This file is part of a11y-checker.
%%%
%%% a11y-checker is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.

%%% a11y-checker is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.

%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%
%% HTML 4.01 generator
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% NOTE: To use the generator compile with debug_info, and execute:
%       proper_gen:sample(html_gen:html()).

-module(html_gen).

-include_lib("proper/include/proper.hrl").

-export([html/0]).


% -----------------------
% Markup helper functions
% -----------------------
open(GenSection) ->
    ?LET(Section,
         GenSection,
         "<" ++ Section ++ ">").

open_with_attributes(GenSection, GenAttribute) ->
    ?LET({Section, Attribute},
         {GenSection, GenAttribute},
         "<" ++ Section ++ Attribute ++ ">").

open_single(GenSection) ->
    ?LET(Section,
         GenSection,
         "<" ++ Section ++ " />").

close(GenSection) ->
    ?LET(Section,
         GenSection,
         "</" ++ Section ++ ">").

attribute(GenAttributeName, GenAttributeValue) ->
    ?LET({AttributeName, AttributeValue},
         {GenAttributeName, GenAttributeValue},
         " " ++ AttributeName ++ "=" ++ "\"" ++ AttributeValue ++ "\"").

composite(HtmlElementBody) ->
    lists:flatten(HtmlElementBody).

ascii_string() ->
    list(elements([choose(48,57),      % digits
		   choose(65,90),      % uppercase
		   choose(97,122)])).  % lowercase

% -----------------------
% HTML document structure
% -----------------------

% <title>
% -------
title_tag() ->
    "title".

open_title() ->
    open(title_tag()).

title_body() ->
    non_empty(ascii_string()).

close_title() ->
    close(title_tag()).

title() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_title(), title_body(), close_title()},
         OpenTag ++ Body ++ CloseTag).


% <head>
% ------
head_tag() ->
    "head".

open_head() ->
    open(head_tag()).

head_body() ->
    title().

close_head() ->
    close(head_tag()).

head() ->
    ?LET({HeadOpenTag, Title, HeadCloseTag},
         {open_head(), head_body(), close_head()},
	 HeadOpenTag ++ Title ++ HeadCloseTag).


% <body>
% ------
body_tag() ->
    "body".

open_body() ->
    open(body_tag()).

document_body() ->
    non_empty(list(elements([
                             paragraph(),
                             heading1(),
                             heading2(),
                             heading3(),
                             heading4(),
                             heading5(),
                             heading6(),
                             link(),
                             image(),
                             horizontal_line(),
                             break_line(),
                             orderer_list(),
                             unorderer_list(),
                             description_list(),
                             table(),
                             div_block(),
                             object(),
                             form()
                            ]))).
close_body() ->
    close(body_tag()).

body() ->
    ?LET({BodyOpenTag, Body, BodyCloseTag},
         {open_body(), document_body(), close_body()},
	 BodyOpenTag ++ composite(Body) ++ BodyCloseTag).


% <html>
% ------
doc_tag() ->
    "html".

open_doc() ->
    open_with_attributes(doc_tag(), attribute_doc()).

attribute_lang_value() ->
    oneof([
           "en",
           "es",
           "gl",
           "ca",
           "eu"
          ]).

attribute_doc_lang()->
    ?LET(Attr, attribute("lang", attribute_lang_value()), Attr).

attribute_doc_xml_lang()->
    ?LET(Attr, attribute("xml:lang", attribute_lang_value()), Attr).

attribute_doc() ->
    oneof([
           attribute_doc_lang(),
           attribute_doc_xml_lang(),
           "" %Sometimes the document won't have attributes
          ]).

close_doc() ->
    close(doc_tag()).


html_declaration() ->
    "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"../src/html/data/DTD/xhtml1-transitional.dtd\">".

html() ->
    ?LET({DocumentOpenTag, Head, Body, DocumentCloseTag},
         {open_doc(), head(), body(), close_doc()},
	 html_declaration() ++ DocumentOpenTag ++ Head ++ Body ++ DocumentCloseTag).




% ----------------------
% Global HTML attributes
% ----------------------
attribute_accesskey_value() ->
    [elements([choose(65,90),      % uppercase
               choose(97,122)])].  % lowercase

attribute_accesskey() ->
     ?LET(Attr,
          attribute("accesskey", attribute_accesskey_value()),
          Attr).

attribute_id() ->
    ?LET(Attr,
         attribute("id", ascii_string()),
         Attr).

attribute_title() ->
    ?LET(Attr,
         attribute("title", ascii_string()),
         Attr).

attribute_dir_value() ->
    oneof(["ltr","rtl","auto"]).

attribute_dir() ->
    ?LET(Attr,
         attribute("dir", attribute_dir_value()),
         Attr).

attribute_lang() ->
    ?LET(Attr,
         attribute("lang", attribute_lang_value()),
         Attr).

attribute_tabindex_value() ->
    [choose(49, 57)].  % digit > 0

attribute_tabindex() ->
    ?LET(Attr,
         attribute("tabindex", attribute_tabindex_value()),
         Attr).

global_attribute() ->
    elements([
              attribute_accesskey(),
              attribute_id(),
              attribute_title(),
              attribute_dir(),
              attribute_lang(),
              attribute_tabindex()
             ]).


% ----------
% HTML forms
%-----------

% <form>
% ------
form_tag() ->
    "form".

attribute_form_action() ->
    ?LET(Attr,
         attribute("action", ascii_string()),
         Attr).

attribute_form_method_value() ->
    oneof([
           "get",
           "post"
          ]).

attribute_form_method() ->
    ?LET(Attr,
         attribute("method", attribute_form_method_value()),
         Attr).

attribute_form_name() ->
    ?LET(Attr, attribute("name", ascii_string()), Attr).

attribute_target_value() ->
    oneof([
           "_blank",
           "_parent",
           "_self",
           "_top"
          ]).

attribute_form_target() ->
    ?LET(Attr,
         attribute("target", attribute_target_value()),
         Attr).

attribute_form() ->
    elements([
              attribute_form_action(),
              attribute_form_method(),
              attribute_form_name(),
              attribute_form_target()
             ]).

form_body() ->
    non_empty(list(elements([
                             form_input(),
                             form_textarea(),
                             form_fieldset(),
                             form_select(),
                             label(),
                             form_button()
                            ]))).

form() ->
    ?LET({FormOpenTag, FormBody, FormCloseTag},
         {open_with_attributes(form_tag(), attribute_form()), form_body(), close(form_tag())},
         FormOpenTag ++ composite(FormBody) ++ FormCloseTag).


% <fieldset>
% ----------
form_fieldset_tag() ->
    "fieldset".

form_fieldset_body() ->
    non_empty(list(elements([
                             form_input(),
                             form_textarea(),
                             form_select(),
                             form_button(),
                             form_fieldset_legend()
                            ]))).

form_fieldset()->
  ?LET({OpenTag, Body, CloseTag},
       {open(form_fieldset_tag()), form_fieldset_body(), close(form_fieldset_tag())},
       OpenTag ++ composite(Body) ++ CloseTag).


% <legend>
% --------
form_fieldset_legend_tag() ->
    "legend".

form_fieldset_legend_body() ->
    non_empty(ascii_string()).

form_fieldset_legend()->
    ?LET({OpenTag, Body, CloseTag},
         {open(form_fieldset_legend_tag()), form_fieldset_legend_body(), close(form_fieldset_legend_tag())},
         OpenTag ++ Body ++ CloseTag).


% <input>
% -------
form_input_tag() ->
    "input".

attribute_form_input_type_value() ->
    oneof([
           "button",
           "checkbox",
           "color",
           "date",
           "datetime",
           "datetime-local",
           "email",
           "file",
           "hidden",
           "image",
           "month",
           "number",
           "password",
           "radio",
           "range",
           "reset",
           "search",
           "submit",
           "tel",
           "text",
           "time",
           "url",
           "week"
          ]).

attribute_form_input_type() ->
    ?LET(Attr,
         attribute("type", attribute_form_input_type_value()),
         Attr).

attribute_form_input_value() ->
    ?LET(Attr,
         attribute("value", ascii_string()),
         Attr).

attribute_form_input() ->
    elements([
              attribute_form_input_type(),
              attribute_form_input_value(),
              global_attribute()
             ]).

form_input_body() ->
    non_empty(ascii_string()).

form_input() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_with_attributes(form_input_tag(), attribute_form_input()), form_input_body(), close(form_input_tag())},
         OpenTag ++ Body ++ CloseTag).


% <textarea>
% ----------
form_textarea_tag() ->
    "textarea".

form_textarea_body() ->
    non_empty(ascii_string()).

form_textarea() ->
    ?LET({OpenTag, Body, CloseTag},
         {open(form_textarea_tag()), form_textarea_body(), close(form_textarea_tag())},
         OpenTag ++ Body ++ CloseTag).


% <select>
% --------
form_select_tag() ->
    "select".

form_select_body() ->
    non_empty(list(form_select_option())).

form_select()->
    ?LET({OpenTag, Body, CloseTag},
         {open(form_select_tag()), form_select_body(), close(form_select_tag())},
         OpenTag ++ composite(Body) ++ CloseTag).


% <option>
% --------
form_select_option_tag() ->
    "option".

attribute_form_select_option_value() ->
    ?LET(Attr, attribute("value", ascii_string()), Attr).

attribute_form_select_option() ->
    attribute_form_select_option_value().

form_select_option_body() ->
    non_empty(ascii_string()).

form_select_option()->
    ?LET({OpenTag, Body, CloseTag},
         {open_with_attributes(form_select_option_tag(), attribute_form_select_option()), form_select_option_body(), close(form_select_option_tag())},
         OpenTag ++ Body ++ CloseTag).


% <button>
% --------
form_button_tag() ->
    "button".

attribute_form_button_type_value() ->
    oneof([
           "button",
           "reset",
           "summit"
          ]).

attribute_form_button_type() ->
    ?LET(Attr,
         attribute("type", attribute_form_button_type_value()),
         Attr).

attribute_form_button() ->
    elements([
             attribute_form_button_type(),
             global_attribute()
            ]).

form_button_body() ->
    non_empty(ascii_string()).

form_button()->
    ?LET({OpenTag, Body, CloseTag},
         {open_with_attributes(form_button_tag(), attribute_form_button()), form_button_body(), close(form_button_tag())},
         OpenTag ++ Body ++ CloseTag).

% <label>
% -------
label_tag() ->
    "label".

attribute_label_for() ->
   ?LET(Attr,
        attribute("for", ascii_string()),
        Attr).

attribute_label() ->
    oneof([
           attribute_label_for(),
           ""
          ]).

label() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_with_attributes(label_tag(), attribute_label()), ascii_string(), close(label_tag())},
         OpenTag ++ Body ++ CloseTag).


% ------------------------------
% HTML links, images and objects
% ------------------------------

% <link>
% ------
link_tag() ->
    "a".

open_link() ->
    open_with_attributes(link_tag(), attribute_link()).

attribute_link_href() ->
    ?LET(Attr, attribute("href", ascii_string()), Attr).

attribute_link_name() ->
    ?LET(Attr, attribute("name", ascii_string()), Attr).

attribute_link_target() ->
    ?LET(Attr, attribute("target", attribute_target_value()), Attr).

attribute_link() ->
    elements([
              attribute_link_href(),
              attribute_link_name(),
              attribute_link_target()
             ]).

link_body() ->
    non_empty(ascii_string()).

close_link() ->
    close(link_tag()).

link()->
    ?LET({OpenTag, Body, CloseTag},
         {open_link(), link_body(), close_link()},
         OpenTag ++ Body ++ CloseTag).


% <image>
% -------
image_tag() ->
    "img".

open_image() ->
    open_with_attributes(image_tag(), attribute_image()).

attribute_image_src() ->
    ?LET(Attr, attribute("src", ascii_string()), Attr).

attribute_image_alt() ->
    ?LET(Attr, attribute("alt", ascii_string()), Attr).

attribute_image() ->
    elements([
              attribute_image_src(),
              attribute_image_alt()
             ]).

image_body() ->
    non_empty(ascii_string()).

close_image() ->
    close(image_tag()).

image()->
    ?LET({OpenTag, Body, CloseTag},
         {open_image(), image_body(), close_image()},
         OpenTag ++ Body ++ CloseTag).


% <object>
% --------
object_tag() ->
    "object".

open_object() ->
    open(object_tag()).

object_body() ->
    non_empty(list(object_param())).

close_object() ->
    close(object_tag()).

object() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_object(), object_body(), close_object()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <param>
% -------
object_param_tag() ->
    "param".

open_object_param() ->
    open_with_attributes(object_param_tag(), attribute_object_param()).

attribute_object_param_name() ->
    ?LET(Attr, attribute("name", ascii_string()), Attr).

attribute_object_param_value() ->
    ?LET(Attr, attribute("value", ascii_string()), Attr).

attribute_object_param() ->
    elements([
              attribute_object_param_name(),
              attribute_object_param_value()
             ]).

object_param_body() ->
    "".

close_object_param() ->
    close(object_param_tag()).

object_param() ->
     ?LET({OpenTag, Body, CloseTag},
          {open_object_param(), object_param_body(), close_object_param()},
         OpenTag ++ Body ++ CloseTag).


% -------------------------------
% HTML headings, paragraphs, divs
% -------------------------------

heading_body() ->
    non_empty(ascii_string()).

% <h1>
% ----
heading1_tag() ->
    "h1".

open_heading1() ->
    open(heading1_tag()).

close_heading1() ->
    close(heading1_tag()).

heading1() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading1(), heading_body(), close_heading1()},
         OpenTag ++ Body ++ CloseTag).


% <h2>
% ----
heading2_tag() ->
    "h2".

open_heading2() ->
    open(heading2_tag()).

close_heading2() ->
    close(heading2_tag()).

heading2() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading2(), heading_body(), close_heading2()},
         OpenTag ++ Body ++ CloseTag).


% <h3>
% ----
heading3_tag() ->
    "h3".

open_heading3() ->
    open(heading3_tag()).

close_heading3() ->
    close(heading3_tag()).

heading3() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading3(), heading_body(), close_heading3()},
         OpenTag ++ Body ++ CloseTag).


% <h4>
% ----
heading4_tag() ->
    "h4".

open_heading4() ->
    open(heading4_tag()).

close_heading4() ->
    close(heading4_tag()).

heading4() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading4(), heading_body(), close_heading4()},
         OpenTag ++ Body ++ CloseTag).

% <h5>
% ----
heading5_tag() ->
    "h5".

open_heading5() ->
    open(heading5_tag()).

close_heading5() ->
    close(heading5_tag()).

heading5() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading5(), heading_body(), close_heading5()},
         OpenTag ++ Body ++ CloseTag).


% <h6>
% ----
heading6_tag() ->
    "h6".

open_heading6() ->
    open(heading6_tag()).

close_heading6() ->
    close(heading6_tag()).

heading6() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_heading6(), heading_body(), close_heading6()},
         OpenTag ++ Body ++ CloseTag).


% <p>
% ---
paragraph_tag() ->
    "p".

open_paragraph() ->
    open(paragraph_tag()).

paragraph_body() ->
    non_empty(ascii_string()).

close_paragraph() ->
    close(paragraph_tag()).

paragraph() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_paragraph(), paragraph_body(), close_paragraph()},
         OpenTag ++ Body ++ CloseTag).


% <br>
% ----
break_line_tag() ->
    "br".

open_break_line() ->
    oneof([
           open(break_line_tag()),
           open_single(break_line_tag())
          ]).

break_line() ->
    ?LET(OpenTag, open_break_line(), OpenTag).

% <hr>
% ----
horizontal_line_tag() ->
    "hr".

open_horizontal_line() ->
    oneof([
           open(horizontal_line_tag()),
           open_single(horizontal_line_tag())
          ]).

horizontal_line() ->
    ?LET(OpenTag, open_horizontal_line(), OpenTag).


% <div>
% -----
div_block_tag() ->
    "div".

open_div_block() ->
    open(div_block_tag()).

div_block_body() ->
    non_empty(ascii_string()).

close_div_block() ->
    close(div_block_tag()).

div_block() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_div_block(), div_block_body(), close_div_block()},
         OpenTag ++ Body ++ CloseTag).


% ----------
% HTML lists
% ----------

list_body()->
    non_empty(list(list_element())).

% <ol>
% ----
orderer_list_tag() ->
    "ol".

open_orderer_list() ->
    open(orderer_list_tag()).

close_orderer_list() ->
    close(orderer_list_tag()).

orderer_list() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_orderer_list(), list_body(), close_orderer_list()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <ul>
% ----
unorderer_list_tag() ->
    "ul".

open_unorderer_list() ->
    open(unorderer_list_tag()).

close_unorderer_list() ->
    close(unorderer_list_tag()).

open_description_list() ->
    open(description_list_tag()).

close_description_list() ->
    close(description_list_tag()).

unorderer_list() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_unorderer_list(), list_body(), close_unorderer_list()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <li>
% ----
list_element_tag() ->
    "li".

open_list_element() ->
    open(list_element_tag()).

list_element_body() ->
    non_empty(ascii_string()).

close_list_element() ->
    close(list_element_tag()).

list_element() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_list_element(), list_element_body(), close_list_element()},
         OpenTag ++ Body ++ CloseTag).


% <dl>
% ----
description_list_tag() ->
    "dl".

description_list_body() ->
    non_empty(list(elements([
                             description_list_term_element(),
                             description_list_description_element()
                            ]))).

description_list() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_description_list(), description_list_body(), close_description_list()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <dt>
% ----
description_list_term_element_tag() ->
    "dt".

open_description_list_term_element() ->
    open(description_list_term_element_tag()).

close_description_list_term_element() ->
    close(description_list_term_element_tag()).

description_list_term_element() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_description_list_term_element(), list_element_body(), close_description_list_term_element()},
         OpenTag ++ Body ++ CloseTag).


% <dd>
% ----
description_list_description_element_tag() ->
    "dd".

open_description_list_description_element() ->
    open(description_list_description_element_tag()).

close_description_list_description_element() ->
    close(description_list_description_element_tag()).

description_list_description_element() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_description_list_description_element(), list_element_body(), close_description_list_description_element()},
         OpenTag ++ Body ++ CloseTag).


% -----------
% HTML tables
% -----------

% <table>
% -------
table_tag() ->
    "table".

open_table() ->
    open(table_tag()).

table_body() ->
    elements([non_empty(list(table_row())),
              table_caption()]).

close_table() ->
    close(table_tag()).

table() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_table(), table_body(), close_table()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <tr>
% ----
table_row_tag() ->
    "tr".

open_table_row() ->
    open(table_row_tag()).

table_row_body() ->
    elements([
              list(table_cell()),
              list(table_header())
             ]).

close_table_row() ->
    close(table_row_tag()).

table_row() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_table_row(), table_row_body(), close_table_row()},
         OpenTag ++ composite(Body) ++ CloseTag).


% <td>
% ----
table_cell_tag() ->
    "td".

open_table_cell() ->
    open(table_cell_tag()).

table_cell_body() ->
    non_empty(ascii_string()).

close_table_cell() ->
    close(table_cell_tag()).

table_cell() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_table_cell(), table_cell_body(), close_table_cell()},
         OpenTag ++ Body ++ CloseTag).


% <th>
% ----
table_header_tag() ->
    "th".

open_table_header() ->
    open(table_header_tag()).

table_header_body() ->
    non_empty(ascii_string()).

close_table_header() ->
    close(table_header_tag()).

table_header() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_table_header(), table_header_body(), close_table_header()},
         OpenTag ++ Body ++ CloseTag).


% <caption>
% ---------
table_caption_tag() ->
    "caption".

open_table_caption() ->
    open(table_caption_tag()).

close_table_caption() ->
    close(table_caption_tag()).

table_caption_body() ->
    non_empty(ascii_string()).

table_caption() ->
    ?LET({OpenTag, Body, CloseTag},
         {open_table_caption(), table_caption_body(), close_table_caption()},
         OpenTag ++ Body ++ CloseTag).





