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
%% PTB: Properties to test the implemented HTML techniques behaviour.
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(html_prop).

-include_lib("proper/include/proper.hrl").

-compile([export_all]).


% ---------------------------
% Testing the HTML Techniques
% ---------------------------

% H37: Using alt attributes on img elements
prop_well_formed_images() ->
    ?FORALL(Html, html_gen:html(), html_tech:h37_only_well_formed_images(html_tech:scan(Html, string))).

% H36: Using alt attributes on images used as submit buttons
prop_well_formed_input_images() ->
    ?FORALL(Html, html_gen:html(), html_tech:h36_only_well_formed_input_images(html_tech:scan(Html, string))).

% H57: Using language attributes on the html element
prop_language_is_declared() ->
    ?FORALL(Html, html_gen:html(), html_tech:h57_language_is_declared(html_tech:scan(Html, string))).

% H25: Providing a title using the title element
prop_page_is_titled() ->
    ?FORALL(Html, html_gen:html(), htlm_tech:h25_page_is_titled(html_tech:scan(Html, string))).

% H71: Providing a description for groups of form controls using fieldset and legend elements
prop_descriptive_fielsets() ->
    ?FORALL(Html, html_gen:html(), html_tech:h71_only_descriptive_fieldsets(html_tech:scan(Html, string))).

% H44: Using label elements to associate text labels with form controls
prop_labeled_form_controls() ->
    ?FORALL(Html, html_gen:html(), html_tech:h44_only_labeled_form_controls(html_tech:scan(Html, string))).

% H65: Using the title attribute to identify form controls when the label element cannot be used
prop_labeled_or_titled_form_controls() ->
    ?FORALL(Html, html_gen:html(), html_tech:h65_only_labeled_or_titled_form_controls(html_tech:scan(Html, string))).

% H64: Using the title attribute of the frame and iframe elements
prop_titled_frames_and_iframes() ->
    ?FORALL(Html, html_gen:html(), html_tech:h64_only_titled_frames_and_iframes(html_tech:scan(Html, string))).

% H53: Using the body of the object element
prop_descriptive_objects() ->
    ?FORALL(Html, html_gen:html(), html_tech:h53_only_descriptive_objects(html_tech:scan(Html, string))).

% H30: Providing link text that describes the purpose of a link for anchor elements
prop_descriptive_links() ->
    ?FORALL(Html, html_gen:html(), html_tech:h30_only_descriptive_links(html_tech:scan(Html, string))).

% H76: Using meta refresh to create an instant client-side redirect. (Needed for Level AAA)
prop_meta_does_not_refresh_or_redirect() ->
    ?FORALL(Html, html_gen:html(), html_tech:h76_meta_does_not_refresh_or_redirect(html_tech:scan(Html, string))).

% H32: Providing submit buttons
prop_submit_able_forms() ->
    ?FORALL(Html, html_gen:html(), html_tech:h32_only_submit_able_forms(html_tech:scan(Html, string))).

% H67: Using null alt text and no title attribute on img elements for images that AT should ignore
prop_well_formed_or_ignored_images() ->
    ?FORALL(Html, html_gen:html(), html_tech:h67_only_well_formed_or_ignored_images(html_tech:scan(Html, string))).

% H94: Ensuring that elements do not contain duplicate attributes
prop_no_duplicated_attributes() ->
    ?FORALL(Html, html_gen:html(), html_tech:h94_no_duplicated_attributes(Html, string)).

% H74: Ensuring that opening and closing tags are used according to specification
prop_well_formed_tags() ->
    ?FORALL(Html, html_gen:html(), html_tech:h74_only_well_formed_tags(Html, string)).

% H75: Ensuring that Web pages are well-formed
prop_well_formed_page() ->
    ?FORALL(Html, html_gen:html(), html_tech:h75_page_is_well_formed(Html, string)).

% H93: Ensuring that id attributes are unique on a Web page
prop_unique_ids() ->
    ?FORALL(Html, html_gen:html(), html_tech:h93_unique_ids(Html, string)).

% H88: Using HTML according to spec
prop_valid_page() ->
    ?FORALL(Html, html_gen:html(), html_tech:h88_page_is_valid(Html, string)).


% ---------------------------------
% Testing HTML auxiliary techniques
% ---------------------------------

prop_labeled_textareas() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_textareas(html_tech:scan(Html, string))).

prop_labeled_selects() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_selects(html_tech:scan(Html, string))).

prop_labeled_form_input_texts() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_form_input_texts(html_tech:scan(Html, string))).

prop_labeled_form_input_files() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_form_input_files(html_tech:scan(Html, string))).

prop_labeled_form_input_passwords() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_form_input_passwords(html_tech:scan(Html, string))).

prop_labeled_checkboxes() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_checkboxes(html_tech:scan(Html, string))).

prop_labeled_radio_buttons() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_radio_buttons(html_tech:scan(Html, string))).

prop_labeled_or_titled_textareas() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_textareas(html_tech:scan(Html, string))).

prop_labeled_or_titled_selects() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_selects(html_tech:scan(Html, string))).

prop_labeled_or_titled_input_texts() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_input_texts(html_tech:scan(Html, string))).

prop_labeled_or_titled_input_files() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_input_files(html_tech:scan(Html, string))).

prop_labeled_or_titled_input_passwords() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_input_passwords(html_tech:scan(Html, string))).

prop_labeled_or_titled_input_checkboxes() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_input_checkboxes(html_tech:scan(Html, string))).

prop_labeled_or_titled_input_radios() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_labeled_or_titled_input_radios(html_tech:scan(Html, string))).

prop_well_formed_attributes() ->
    ?FORALL(Html, html_gen:html(), html_tech:only_well_formed_attributes(Html, string)).

