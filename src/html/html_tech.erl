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
%% WCAG 2.0: HTML techniques for (Level A of Conformance).
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(html_tech).

-include_lib("xmerl/include/xmerl.hrl").

-export([
         % ------------------------
         % WCAG 2.0 HTML techniques
         % ------------------------
         h37_only_well_formed_images/1,
         h36_only_well_formed_input_images/1,
         h57_language_is_declared/1,
         h25_page_is_titled/1,
         h71_only_descriptive_fieldsets/1,
         h44_only_labeled_form_controls/1,
         h65_only_labeled_or_titled_form_controls/1,
         h64_only_titled_frames_and_iframes/1,
         h53_only_descriptive_objects/1,
         h30_only_descriptive_links/1,
         h32_only_submit_able_forms/1,
         h67_only_well_formed_or_ignored_images/1,
         h94_no_duplicated_attributes/2,
         h74_only_well_formed_tags/2,
         h75_page_is_well_formed/2,
         h93_unique_ids/2,
         h88_page_is_valid/2,
         h76_meta_does_not_refresh_or_redirect/1,
         % -------------------------
         % HTML auxiliary techniques
         % -------------------------
         only_well_formed_attributes/2,
         only_labeled_textareas/1,
         only_labeled_selects/1,
         only_labeled_form_input_texts/1,
         only_labeled_form_input_files/1,
         only_labeled_form_input_passwords/1,
         only_labeled_checkboxes/1,
         only_labeled_radio_buttons/1,
         only_labeled_or_titled_textareas/1,
         only_labeled_or_titled_selects/1,
         only_labeled_or_titled_input_texts/1,
         only_labeled_or_titled_input_files/1,
         only_labeled_or_titled_input_passwords/1,
         only_labeled_or_titled_input_checkboxes/1,
         only_labeled_or_titled_input_radios/1,
         % -------
         % Parsing
         % -------
         scan/2
        ]).


% -----------------
% Parsing functions
% -----------------

scan(Html, EntryType) ->
    {XmlElement, _} = case EntryType of
                          string -> xmerl_scan:string(Html,[{comments, false}]);
                          file   -> xmerl_scan:file(Html, [{comments, false}]);
						  url    -> {_, { _, _, HtmlData}} = httpc:request(Html),
									xmerl_scan:string(HtmlData,[{comments, false}])
                      end,
    XmlElement.

validate(Html, EntryType) ->
    {XmlElement, _} =  case EntryType of
                           string -> xmerl_scan:string(Html, [{validation, dtd}, {comments, false}]);
                           file   -> xmerl_scan:file(Html, [{validation, dtd},{comments, false}]);
						   url    -> {_, { _, _, HtmlData}} = httpc:request(Html),
									 xmerl_scan:string(HtmlData,[{validation, dtd}, {comments, false}])
                       end,
    XmlElement.

well_formed_html(Html, EntryType) ->
    try scan(Html, EntryType) of
         _ -> ok
    catch
        exit:{fatal,{Reason, File, Line, Col}} -> case Reason of
                                                      {error, {unique_att_spec_required, Attribute}} ->
                                                          {error, {duplicated_attributes, Attribute}, File, Line, Col}; %% Duplicated attributes
                                                      expected_element_start_tag ->
                                                          {error, missing_start_tag, File, Line, Col}; %% Missing start tag
                                                      unexpected_end ->
                                                          {error, missing_end_tag, File, Line, Col};   %% Missing end tag
                                                      {endtag_does_not_match, {was, Tag, should_have_been, CorrectTag}} ->
                                                          {error, {invalid_end_tag, {{invalid_value,Tag}, {correct_value, CorrectTag}}}, File, Line, Col};   %% Invalid end tag
                                                      {invalid_name, Name} ->
                                                          {error, {invalid_tag_name, Name}, File, Line, Col};  %% Invalid tag name
                                                      unexpected_char -> {error, malformed_attribute, File, Line, Col};
                                                      assignment_expected -> {error, attribute_assignment_expected, File, Line, Col};
                                                      {whitespace_required_between_attributes} -> {error, missing_whitespace_between_attributes, File, Line, Col};
                                                      _ -> {error, Reason, File, Line, Col}
                                                  end;
        error:function_clause -> {error, malformed_attribute}; % TODO: Refine the pattern maching: [{xmerl_scan, scan_att_value, _}|_]
        _:Error -> {error, Error}
    end.

valid_html(Html, EntryType) ->
    case well_formed_html(Html, EntryType) of
        ok -> try validate(Html, EntryType) of
                  _ -> ok
              catch
                  exit:{fatal,{Reason, File, Line, Col}} -> case Reason of
                                                        {values_must_be_unique,'ID', Id} -> {validation_error, {no_unique_ids, Id}, File, Line, Col};
                                                        {failed_validation, Error} -> {validation_error, Error, File, Line, Col};
                                                        {error, Error} -> {validation_error, Error, File, Line, Col};
                                                        _ -> {validation_error, Reason, File, Line, Col}
                                                    end;
                  _:Error -> {validation_error, Error}
              end;
        Error -> Error
    end.


% ---------------
% Logic functions
% ---------------

if_any(List, Fun) ->
    lists:foldl(fun(A, B) -> A or B end,
                false,
                [Fun(A) || A <- List ]).

for_all(List, Fun) ->
    lists:foldl(fun(A, B) -> A and B end,
                true,
                [Fun(A) || A <- List ]).

for_all(List, Fun, Arg1) ->
      lists:foldl(fun(A, B) -> A and B end,
                true,
                [Fun(A, Arg1) || A <- List ]).

for_all(List, Fun, Arg1, Arg2) ->
      lists:foldl(fun(A, B) -> A and B end,
                true,
                [Fun(A, Arg1, Arg2) || A <- List ]).

% ----------------
% Output functions
% ----------------

print_element(XmlElement) when is_record(XmlElement, xmlElement) ->
    io:format(
      "~nFailing element: ~p~n",
       [lists:append(lists:reverse(XmlElement#xmlElement.parents), [{XmlElement#xmlElement.name, XmlElement#xmlElement.pos}])]
     ).

print_error(Error) ->
    io:format("~nError: ~p~n", [Error]).


% ----------------
% Eval functions
% ----------------
eval(XmlElement, Fun, Args) when is_record(XmlElement, xmlElement) ->
    Result = case Args of
                 []                 -> Fun();
                 [Arg]              -> Fun(Arg);
                 [Arg1, Arg2]       -> Fun(Arg1, Arg2);
                 [Arg1, Arg2, Arg3] -> Fun(Arg1, Arg2, Arg3)
             end,
    case Result of
        false -> print_element(XmlElement);
        _     -> ""
    end,
    Result;
eval(_,_,_) ->false.



% ------------------------------------------
% Helper functions for HTML elements
% ------------------------------------------

is_named(Name, #xmlElement{name = Name}) ->
    true;
is_named(_, XmlElement) when is_record(XmlElement, xmlElement) ->
    false.

get_attribute_value(AttributeName, XmlElement) when is_record(XmlElement, xmlElement) ->
    case lists:keyfind(AttributeName, 2, XmlElement#xmlElement.attributes) of
        false -> false;
        {xmlAttribute,AttributeName, _, _, _, _, _, _, AttributeValue, _}  -> {true, AttributeValue}
    end.

has_attribute_equal_to({AttributeName, AttributeValue}, XmlElement) when is_record(XmlElement, xmlElement) ->
    case get_attribute_value(AttributeName, XmlElement) of
        false         -> false;
        {true, Value} -> AttributeValue == Value
    end.

has_attribute(AttributeName, XmlElement) ->
   case get_attribute_value(AttributeName, XmlElement) of
       false     -> false;
       {true, _} -> true
   end.

is_textarea(XmlElement) ->
    is_named(textarea, XmlElement).

is_select(XmlElement) ->
    is_named(select, XmlElement).

is_input_text(XmlElement) ->
    is_named(input, XmlElement) andalso
        has_attribute_equal_to({type, "text"}, XmlElement).

is_input_file(XmlElement) ->
    is_named(input, XmlElement) andalso
        has_attribute_equal_to({type, "file"}, XmlElement).

is_input_password(XmlElement) ->
    is_named(input, XmlElement) andalso
        has_attribute_equal_to({type, "password"}, XmlElement).

is_input_checkbox(XmlElement) ->
    is_named(input, XmlElement) andalso
        has_attribute_equal_to({type, "checkbox"}, XmlElement).

is_radio_button(XmlElement) ->
    is_named(input, XmlElement) andalso
        has_attribute_equal_to({type, "radio"}, XmlElement).

is_input_submit(XmlElement) ->
    is_named(input, XmlElement)
        andalso has_attribute_equal_to({type, "submit"}, XmlElement).

is_input_image(XmlElement) ->
    is_named(input, XmlElement)
        andalso has_attribute_equal_to({type, "image"}, XmlElement).

is_submit_button(XmlElement) ->
    is_named(button, XmlElement)
        andalso has_attribute_equal_to({type, "submit"}, XmlElement).

is_text(XmlText) when is_record(XmlText, xmlText) ->
    true;
is_text(_) ->
    false.

% --------------------------------------------
% Helper functions for HTML attributes
% --------------------------------------------
is_lang(#xmlAttribute{name = lang}) ->
    true;
is_lang(#xmlAttribute{name = 'xml:lang'}) ->
    true;
is_lang(_) ->
    false.

is_alt(#xmlAttribute{name = alt}) ->
    true;
is_alt(_) ->
    false.

is_title(#xmlAttribute{name = title}) ->
    true;
is_title(_) ->
    false.

is_empty_attribute([]) ->
    true;
is_empty_attribute(" ") ->
    true;
is_empty_attribute(_AttributeValue) ->
    false.

% ---------------
% HTML Techniques
% ---------------

% H37: Using alt attributes on img elements
% -----------------------------------------
h37_only_well_formed_images(XmlElement = #xmlElement{name = img}) ->
    eval(XmlElement, fun if_any/2, [XmlElement#xmlElement.attributes, fun is_alt/1]);
h37_only_well_formed_images(#xmlElement{content = []}) ->
    true;
h37_only_well_formed_images(XmlElement) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun h37_only_well_formed_images/1);
h37_only_well_formed_images(_) ->
    true.


% H36: Using alt attributes on images used as submit buttons
% ----------------------------------------------------------
h36_only_well_formed_input_images(XmlElement) when is_record(XmlElement, xmlElement) ->
    case is_input_image(XmlElement) of
        true  -> eval(XmlElement, fun if_any/2, [XmlElement#xmlElement.attributes, fun is_alt/1]);
        false -> case XmlElement#xmlElement.content of
                     [] -> true;
                     _  -> for_all(XmlElement#xmlElement.content, fun h36_only_well_formed_input_images/1)
                 end
    end;
h36_only_well_formed_input_images(_) ->
    true.


% H57: Using language attributes on the html element
% --------------------------------------------------
h57_language_is_declared(XmlElement = #xmlElement{name = html}) ->
    if_any(XmlElement#xmlElement.attributes, fun is_lang/1).


% H25: Providing a title using the title element
% ----------------------------------------------
h25_page_is_titled(XmlElement = #xmlElement{name = title, parents = [{head, _}, {html, _}] }) ->
    case XmlElement#xmlElement.content of
        [] -> false;
        _  -> true
    end;
h25_page_is_titled(#xmlElement{name = title}) ->
    false;
h25_page_is_titled(#xmlElement{content = []}) ->
    false;
h25_page_is_titled(XmlElement) when is_record(XmlElement, xmlElement)->
    if_any(XmlElement#xmlElement.content, fun h25_page_is_titled/1);
h25_page_is_titled(_) ->
    false.


% H71: Providing a description for groups of form controls using fieldset and legend elements
% -------------------------------------------------------------------------------------------
is_non_empty_legend(XmlElement = #xmlElement{name = legend}) ->
    case XmlElement#xmlElement.content of
        [] -> false;
        _  -> true
    end;
is_non_empty_legend(_) ->
    false.

h71_only_descriptive_fieldsets(XmlElement = #xmlElement{name = fieldset}) ->
    eval(XmlElement, fun if_any/2, [XmlElement#xmlElement.content, fun is_non_empty_legend/1]);
h71_only_descriptive_fieldsets(#xmlElement{content = []}) ->
    true;
h71_only_descriptive_fieldsets(XmlElement) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun h71_only_descriptive_fieldsets/1);
h71_only_descriptive_fieldsets(_) ->
    true.

% H44: Using label elements to associate text labels with form controls
% ---------------------------------------------------------------------
is_label_for_element(Label = #xmlElement{name = label}, XmlElement) when is_record(XmlElement, xmlElement) ->
    LabelFor  = get_attribute_value(for, Label),
    ElementId = get_attribute_value(id,  XmlElement),
    case {LabelFor, ElementId} of
        { false, _ } -> false;
        { _, false } -> false;
        { A, A }     -> true;
        _            -> false
    end;
is_label_for_element(_, XmlElement) when is_record(XmlElement, xmlElement) ->
    false.

has_left_label(XmlElement, [XmlElement]) ->
    false;
has_left_label(XmlElement, [XmlElement | _] ) ->
    false;
has_left_label(XmlElement, ParentContent) when is_record(XmlElement, xmlElement)->
    PreviousElement = lists:nth(XmlElement#xmlElement.pos - 1, ParentContent),
    case is_record(PreviousElement, xmlElement) of
        true  -> PreviousElement#xmlElement.content /= [] andalso
                     is_label_for_element(PreviousElement, XmlElement);
        false  -> false
    end.

has_right_label(XmlElement, [XmlElement]) ->
    false;
has_right_label(XmlElement, ParentContent) when is_record(XmlElement, xmlElement) ->
    LastPos = length(ParentContent),
    case XmlElement#xmlElement.pos of
        LastPos -> false;
        _ -> NextElement = lists:nth(XmlElement#xmlElement.pos + 1, ParentContent),
             case is_record(NextElement, xmlElement) of
                 true  -> NextElement#xmlElement.content /= [] andalso
                              is_label_for_element(NextElement, XmlElement);
                 false -> false
             end
    end.

look_for_labels(XmlElement, ElementFilter, ParentContent) when is_record(XmlElement, xmlElement) ->
    case ElementFilter(XmlElement) of
        {true, SearchFun}  ->  eval(XmlElement, SearchFun, [XmlElement, ParentContent]);
        false ->  case XmlElement#xmlElement.content of
                      [] -> true;
                      _  -> for_all(XmlElement#xmlElement.content,
                                    fun look_for_labels/3,
                                    ElementFilter,
                                    XmlElement#xmlElement.content)
                  end
    end;
look_for_labels(_, _ ,_) ->
    true.

needs_label(XmlElement) when is_record(XmlElement, xmlElement)->
    case needs_right_label(XmlElement) of
        true  -> {true, fun has_right_label/2};
        false -> case needs_left_label(XmlElement) of
                     true  -> {true, fun has_left_label/2};
                     false -> false
                 end
    end;
needs_label(_) -> false.


needs_right_label(XmlElement) ->
    is_radio_button(XmlElement)
        orelse
        is_input_checkbox(XmlElement).

needs_left_label(XmlElement) ->
    is_textarea(XmlElement)
        orelse
        is_select(XmlElement)
        orelse
        is_input_text(XmlElement)
        orelse
        is_input_file(XmlElement)
        orelse
        is_input_password(XmlElement).

h44_only_labeled_form_controls(XmlElement) ->
    look_for_labels(XmlElement, fun needs_label/1, []).


% H65: Using the title attribute to identify form controls when the label element cannot be used
% ----------------------------------------------------------------------------------------------
has_right_label_or_title(XmlElement, ParentContent) ->
    has_right_label(XmlElement, ParentContent) orelse has_attribute(title, XmlElement).

has_left_label_or_title(XmlElement, ParentContent) ->
    has_left_label(XmlElement, ParentContent) orelse has_attribute(title, XmlElement).

needs_label_or_title(XmlElement) ->
    case needs_right_label(XmlElement) of
        true  -> {true, fun has_right_label_or_title/2};
        false -> case needs_left_label(XmlElement) of
                     true  -> {true, fun has_left_label_or_title/2};
                     false -> false
                 end
    end.

h65_only_labeled_or_titled_form_controls(XmlElement) ->
    look_for_labels(XmlElement, fun needs_label_or_title/1, XmlElement).

% H64: Using the title attribute of the frame and iframe elements
% ---------------------------------------------------------------
h64_only_titled_frames_and_iframes(XmlElement = #xmlElement{name = frame}) ->
    eval(XmlElement, fun if_any/2, [XmlElement#xmlElement.attributes, fun is_title/1]);
h64_only_titled_frames_and_iframes(XmlElement = #xmlElement{name = iframe}) ->
    eval(XmlElement, fun if_any/2, [XmlElement#xmlElement.attributes, fun is_title/1]);
h64_only_titled_frames_and_iframes(#xmlElement{content = []}) ->
    true;
h64_only_titled_frames_and_iframes(XmlElement) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun h64_only_titled_frames_and_iframes/1);
h64_only_titled_frames_and_iframes(_) ->
    true.


% H53: Using the body of the object element
% -----------------------------------------
h53_only_descriptive_objects(XmlElement) ->
    only_elements_with_descriptive_body(XmlElement, object).


% H30: Providing link text that describes the purpose of a link for anchor elements
% ---------------------------------------------------------------------------------
is_well_formed_image(XmlElement = #xmlElement{name = img}) ->
    if_any(XmlElement#xmlElement.attributes, fun is_alt/1);
is_well_formed_image(_) ->
    false.

is_descriptive_body([]) ->
    false;
is_descriptive_body([XmlText]) when is_record(XmlText, xmlText) ->
    true;
is_descriptive_body([XmlElement = #xmlElement{name = img}]) ->
    if_any(XmlElement#xmlElement.attributes, fun is_alt/1); % FIXME: Should be a non-empty alt
is_descriptive_body(Body) ->
    if_any(Body, fun is_text/1) orelse if_any(Body, fun is_well_formed_image/1).

only_elements_with_descriptive_body(XmlElement = #xmlElement{name = ElementName, content = []}, ElementName) ->
    print_element(XmlElement),
    false;
only_elements_with_descriptive_body(XmlElement = #xmlElement{name = ElementName}, ElementName) ->
    eval(XmlElement, fun is_descriptive_body/1, [XmlElement#xmlElement.content]);
only_elements_with_descriptive_body(XmlElement, ElementName) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun only_elements_with_descriptive_body/2, ElementName);
only_elements_with_descriptive_body(_, _) ->
    true.

h30_only_descriptive_links(XmlElement) ->
    only_elements_with_descriptive_body(XmlElement, a).


% H32: Providing submit buttons
% -----------------------------
contains(Content, IsElement) ->
    case Content of
        []  -> false;
        [E] -> case is_record(E, xmlElement) of
                   true ->  case IsElement(E) of
                                true  -> true;
                                false -> eval(E, fun contains/2, [E#xmlElement.content, fun is_submit_element/1])
                            end;
                   false -> false
                   end;
        [H|_] -> eval(H, fun contains/2, [H, fun is_submit_element/1])
    end.


is_submit_element(XmlElement) when is_record(XmlElement, xmlElement) ->
    is_input_submit(XmlElement) orelse is_input_image(XmlElement) orelse is_submit_button(XmlElement);
is_submit_element(_) ->
    false.

h32_only_submit_able_forms(XmlElement = #xmlElement{name = form}) ->
    eval(XmlElement, fun contains/2, [XmlElement#xmlElement.content, fun is_submit_element/1]);
h32_only_submit_able_forms(#xmlElement{content = []}) ->
    true;
h32_only_submit_able_forms(XmlElement) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun h32_only_submit_able_forms/1);
h32_only_submit_able_forms(_) ->
    true.

% H67: Using null alt text and no title attribute on img elements for images that AT should ignore
% ------------------------------------------------------------------------------------------------
is_image(XmlElement) ->
    is_named(img, XmlElement) orelse
        is_input_image(XmlElement).

is_well_formed_or_ignored_image(XmlElement) ->
    Alt   = get_attribute_value(alt, XmlElement),
    Title = get_attribute_value(title, XmlElement),
    case {Alt, Title} of
        {false, _} -> false;
        {_, false} -> true;
        {{_, AltValue}, {_, TitleValue}} -> case is_empty_attribute(AltValue) of
                                                false -> true;
                                                true  -> is_empty_attribute(TitleValue)
                                            end
    end.

h67_only_well_formed_or_ignored_images(XmlElement) when is_record(XmlElement, xmlElement) ->
    case is_image(XmlElement) of
        true  -> eval(XmlElement, fun is_well_formed_or_ignored_image/1, [XmlElement]);
        false -> case XmlElement#xmlElement.content of
                     [] -> true;
                     _  -> for_all(XmlElement#xmlElement.content, fun h67_only_well_formed_or_ignored_images/1)
                 end
    end;
h67_only_well_formed_or_ignored_images(_) ->
    true.


% H94: Ensuring that elements do not contain duplicate attributes
% ---------------------------------------------------------------
h94_no_duplicated_attributes(Html, EntryType) ->
    Result = well_formed_html(Html, EntryType),
    case Result of
        {error, {duplicated_attributes, _}, _, _, _} -> print_error(Result),
                                                        false;
        _ -> true
    end.


% H74: Ensuring that opening and closing tags are used according to specification
% -------------------------------------------------------------------------------
h74_only_well_formed_tags(Html, EntryType) ->
    Result = well_formed_html(Html, EntryType),
    case Result of
        {error, missing_start_tag, _, _, _}     -> print_error(Result),
                                                   false;
        {error, missing_end_tag, _, _, _}       -> print_error(Result),
                                                   false;
        {error, {invalid_end_tag, _}, _, _, _}  -> print_error(Result),
                                                   false;
        {error, {invalid_tag_name, _}, _, _, _} -> print_error(Result),
                                                   false;
        _ -> true
    end.


% H75: Ensuring that Web pages are well-formed
% --------------------------------------------
h75_page_is_well_formed(Html, EntryType) ->
    case well_formed_html(Html, EntryType) of
        ok     -> true;
        Error  -> print_error(Error),
                  false
    end.


% H93: Ensuring that id attributes are unique on a Web page
% ---------------------------------------------------------
h93_unique_ids(Html, EntryType) ->
    Result = valid_html(Html, EntryType),
    case Result of
        {validation_error, {no_unique_ids, _}, _, _, _} -> print_error(Result),
                                                           false;
        _  -> true
    end.


% H88: Using HTML according to spec
% ---------------------------------
h88_page_is_valid(Html, EntryType) ->
    case valid_html(Html, EntryType) of
        ok -> true;
        Error  -> print_error(Error),
                  false
    end.


% H76: Using meta refresh to create an instant client-side redirect. (Level AAA)
% ----------------------------------------------------------------------------------------
h76_meta_does_not_refresh_or_redirect(XmlElement = #xmlElement{name = meta})  ->
    case has_attribute_equal_to({'http-equiv', "refresh"}, XmlElement) of
        false -> true;
        true  -> case get_attribute_value(content, XmlElement) of
                     false     -> true;
                     {true, _} -> false
                 end
    end;
h76_meta_does_not_refresh_or_redirect(#xmlElement{content = []}) ->
    true;
h76_meta_does_not_refresh_or_redirect(XmlElement) when is_record(XmlElement, xmlElement) ->
    for_all(XmlElement#xmlElement.content, fun h76_meta_does_not_refresh_or_redirect/1);
h76_meta_does_not_refresh_or_redirect(_) ->
    true.


% -------------------------
% HTML auxiliary techniques
% -------------------------

only_well_formed_attributes(Html, EntryType)->
    case well_formed_html(Html, EntryType) of
        {error, malformed_attribute} -> false;
        {error, attribute_assignment_expected} -> false;
        {error, missing_whitespace_between_attributes} -> false;
        _ -> true
    end.

right_label(XmlElement, ElementFilter) ->
    case ElementFilter(XmlElement) of
        true  -> {true, fun has_right_label/2};
        false -> false
    end.

left_label(XmlElement, ElementFilter) ->
    case ElementFilter(XmlElement) of
        true  -> {true, fun has_left_label/2};
        false -> false
    end.

right_label_or_title(XmlElement, ElementFilter) ->
    case ElementFilter(XmlElement) of
        true  -> {true, fun has_right_label_or_title/2};
        false -> false
    end.

left_label_or_title(XmlElement, ElementFilter) ->
    case ElementFilter(XmlElement) of
        true  -> {true, fun has_left_label_or_title/2};
        false -> false
    end.

look_for_label_if_radio_button(XmlElement) ->
    right_label(XmlElement, fun is_radio_button/1).

look_for_label_or_title_if_radio_button(XmlElement) ->
    right_label_or_title(XmlElement, fun is_radio_button/1).

look_for_label_if_input_checkbox(XmlElement) ->
    right_label(XmlElement, fun is_input_checkbox/1).

look_for_label_or_title_if_input_checkbox(XmlElement) ->
    right_label_or_title(XmlElement, fun is_input_checkbox/1).

look_for_label_if_textarea(XmlElement) ->
    left_label(XmlElement, fun is_textarea/1).

look_for_label_or_title_if_textarea(XmlElement) ->
    left_label_or_title(XmlElement, fun is_textarea/1).

look_for_label_if_select(XmlElement) ->
    left_label(XmlElement, fun is_select/1).

look_for_label_or_title_if_select(XmlElement) ->
    left_label_or_title(XmlElement, fun is_select/1).

look_for_label_if_input_text(XmlElement) ->
    left_label(XmlElement, fun is_input_text/1).

look_for_label_or_title_if_input_text(XmlElement) ->
    left_label_or_title(XmlElement, fun is_input_text/1).

look_for_label_if_input_file(XmlElement) ->
    left_label(XmlElement, fun is_input_file/1).

look_for_label_or_title_if_input_file(XmlElement) ->
    left_label_or_title(XmlElement, fun is_input_file/1).

look_for_label_if_input_password(XmlElement) ->
    left_label(XmlElement, fun is_input_password/1).

look_for_label_or_title_if_input_password(XmlElement) ->
    left_label_or_title(XmlElement, fun is_input_password/1).


only_labeled_textareas(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_textarea/1, []).

only_labeled_selects(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_select/1, []).

only_labeled_form_input_texts(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_input_text/1, []).

only_labeled_form_input_files(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_input_file/1, []).

only_labeled_form_input_passwords(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_input_password/1, []).

only_labeled_checkboxes(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_input_checkbox/1, []).

only_labeled_radio_buttons(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_if_radio_button/1, []).

only_labeled_or_titled_textareas(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_textarea/1, []).

only_labeled_or_titled_selects(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_select/1, []).

only_labeled_or_titled_input_texts(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_input_text/1, []).

only_labeled_or_titled_input_files(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_input_file/1, []).

only_labeled_or_titled_input_passwords(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_input_password/1, []).

only_labeled_or_titled_input_checkboxes(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_input_checkbox/1, []).

only_labeled_or_titled_input_radios(XmlElement) ->
    look_for_labels(XmlElement, fun look_for_label_or_title_if_radio_button/1, []).

