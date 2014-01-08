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
%%  WCAG 2.0: Success criterions (Level A of Conformance)
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module('wcag_2').

-export([success_criterion_1_1_1_non_text_content/4,
         success_criterion_1_2_1_time_based_media/4,
         success_criterion_1_2_2_captions/4,
         success_criterion_1_2_3_audio_desc_or_media_alternative/4,
         success_criterion_1_3_1_info_and_relationships/4,
         success_criterion_1_3_2_meaningful_sequence/4,
         success_criterion_1_3_3_audio_sensory_characteristics/4,
         success_criterion_1_4_1_use_of_color/4,
         success_criterion_1_4_2_audio_control/4,
         success_criterion_2_1_1_keyboard/4,
         success_criterion_2_1_2_no_keyboard_trap/4,
         success_criterion_2_2_1_timing_adjustable/4,
         success_criterion_2_2_2_pause_stop_hide/4,
         success_criterion_2_3_1_three_flashes_or_below_threshold/4,
         success_criterion_2_4_1_bypass_blocks/4,
         success_criterion_2_4_2_page_titled/4,
         success_criterion_2_4_3_focus_order/4,
         success_criterion_2_4_4_link_purpose/4,
         success_criterion_3_1_1_language_of_page/4,
         success_criterion_3_2_1_on_focus/4,
         success_criterion_3_2_2_on_input/4,
         success_criterion_3_3_1_error_identification/4,
         success_criterion_3_3_2_labels_or_instructions/4,
         success_criterion_4_1_1_parsing/4,
         success_criterion_4_1_2_name_role_value/4,
         conformance_level/4
        ]).

-include("../include/a11y-checker.hrl").

% ----------------
% Output functions
% ----------------
eval_technology(Technology, Fun, Args, SuccessCriterionName) ->
    Result = erlang:apply(Fun, Args),
    case Result of
        true  -> io:format("Success Criterion ~s: (~s) ok~n",     [SuccessCriterionName, Technology]);
        false -> io:format("Success Criterion ~s: (~s) failed~n", [SuccessCriterionName, Technology])
    end,
    Result.


eval(TechnologyList, SuccessCriterionName) ->
    Result = lists:foldl(fun(A, B) -> A and B end,
                         true,
                         [eval_technology(Technology, Fun, Args, SuccessCriterionName) || {Technology, Fun, Args} <- TechnologyList]),
    case Result of
        true  -> io:format("Success Criterion ~s: ok~n",     [SuccessCriterionName]);
        false -> io:format("Success Criterion ~s: failed~n", [SuccessCriterionName])
    end,
    Result.


eval_success_criterion(Fun, Args) ->
    io:format("~n----------------------~n~n"),
	erlang:apply(Fun, Args).


eval_conformance_level(SuccessCriterionList, LevelOfConformance) ->
    Result = lists:foldl(fun(A, B) -> A and B end,
                         true,
                         [eval_success_criterion(SuccessCriterionFun, Args) || {SuccessCriterionFun, Args} <- SuccessCriterionList]),
    case Result of
        true  -> io:format("Level ~s of Conformance: achieved~n", [LevelOfConformance]);
        false -> io:format("Level ~s of Conformance: denied~n",   [LevelOfConformance])
    end,
    Result.

%==============================================================================
%% Principle 1: Perceivable - Information and user interface components must be
%% presentable to users in ways they can perceive.
%==============================================================================

%% Guideline 1.1 Text Alternatives: Provide text alternatives for any non-text
%% content so that it can be changed into other forms people need, such as
%% large print, braille, speech, symbols or simpler language.
%% ----------------------------------------------------------------------------

%% Success Criterion 1.1.1 Non-text Content: All non-text content that is
%% presented to the user has a text alternative that serves the equivalent
%% purpose, except for the situations listed below.
%%   * Controls, Input: If non-text content is a control or accepts user input,
%% then it has a name that describes its purpose. (See Guideline 4.1).
%%   * Time-Based Media: If non-text content is time-based media, then text
%% alternatives at least provide descriptive identification of the non-text
%% content. (See Guideline 1.2).
%%   * Test: If non-text content is a test or exercise that would be invalid
%% if presented in text, then text alternatives at least provide descriptive
%% identification of the non-text content.
%%   * Sensory: If non-text content is primarily intended to create a specific
%% sensory experience, then text alternatives at least provide descriptive
%% identification of the non-text content.
%%   * CAPTCHA: If the purpose of non-text content is to confirm that content
%% is being accessed by a person rather than a computer, then text alternatives
%% that identify and describe the purpose of the non-text content are provided,
%% and alternative forms of CAPTCHA using output modes for different types of
%% sensory perception are provided to accommodate different disabilities.
%%     * Decoration, Formatting, Invisible: If non-text content is pure
%% decoration, is used only for visual formatting, or is not presented to users,
%% then it is implemented in a way that it can be ignored by assistive
%% technology.
success_criterion_1_1_1_non_text_content(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_1_1_non_text_content/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.1.1").


%% Guideline 1.2 Time-based Media: Provide alternatives for time-based media.
%% ----------------------------------------------------------------------------

%% Success Criterion 1.2.1 Audio-only and Video-only (Prerecorded): For
%% prerecorded audio-only and prerecorded video-only media, the following are
%% true, except when the audio or video is a media alternative for text and is
%% clearly labeled as such:
%%     * Prerecorded Audio-only: An alternative for time-based media is
%% provided that presents equivalent information for prerecorded audio-only content.
%%     * Prerecorded Video-only: Either an alternative for time-based media or
%% an audio track is provided that presents equivalent information for
%% prerecorded video-only content.
success_criterion_1_2_1_time_based_media(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_2_1_time_based_media/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.2.1").


%% Success Criterion 1.2.2 Captions (Prerecorded): Captions are provided for all
%% prerecorded audio content in synchronized media, except when the media is a
%% media alternative for text and is clearly labeled as such.
success_criterion_1_2_2_captions(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_2_2_captions/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.2.2").


%% Success Criterion 1.2.3 Audio Description or Media Alternative (Prerecorded):
%% An alternative for time-based media or audio description of the prerecorded
%% video content is provided for synchronized media, except when the media is
%% a media alternative for text and is clearly labeled as such.
success_criterion_1_2_3_audio_desc_or_media_alternative(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_2_3_audio_desc_or_media_alternative/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.2.3").


%% Guideline 1.3 Adaptable: Create content that can be presented in different
%% ways (for example simpler layout) without losing information or structure.
%% ----------------------------------------------------------------------------

%% Success Criterion 1.3.1 Info and Relationships: Information, structure, and
%% relationships conveyed through presentation can be programmatically
%% determined or are available in text.
success_criterion_1_3_1_info_and_relationships(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_3_1_info_and_relationships/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.3.1").


%% Success Criterion 1.3.2 Meaningful Sequence: When the sequence in which
%% content is presented affects its meaning, a correct reading sequence can be
%% programmatically determined.
success_criterion_1_3_2_meaningful_sequence(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_3_2_meaningful_sequence/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.3.2").


%% Success Criterion 1.3.3 Sensory Characteristics: Instructions provided for
%% understanding and operating content do not rely solely on sensory
%% characteristics of components such as shape, size, visual location,
%% orientation, or sound.
%% - Note: For requirements related to color, refer to Guideline 1.4.
success_criterion_1_3_3_audio_sensory_characteristics(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_3_3_audio_sensory_characteristics/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.3.3").


%% Guideline 1.4 Distinguishable: Make it easier for users to see and hear
%% content including separating foreground from background
%% ----------------------------------------------------------------------------

%% Success Criterion 1.4.1 Use of Color: Color is not used as the only visual
%% means of conveying information, indicating an action, prompting a response,
%% or distinguishing a visual element.
%% - Note: This success criterion addresses color perception specifically.
%% Other forms of perception are covered in Guideline 1.3 including
%% programmatic access to color and other visual presentation coding.
success_criterion_1_4_1_use_of_color(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_4_1_use_of_color/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.4.1").


%% Success Criterion 1.4.2 Audio Control: If any audio on a Web page plays
%% automatically for more than 3 seconds, either a mechanism is available to
%% pause or stop the audio, or a mechanism is available to control audio volume
%% independently from the overall system volume level.
%% - Note: Since any content that does not meet this success criterion can
%% interfere with a user's ability to use the whole page, all content on the
%% Web page (whether or not it is used to meet other success criteria) must
%% meet this success criterion. See Conformance Requirement 5: Non-Interference.
success_criterion_1_4_2_audio_control(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_1_4_2_audio_control/3, [LevelOfConformance, Content, EntryType]}
         ],
         "1.4.2").


%==============================================================================
%% Principle 2: Operable - User interface components and navigation must be
%% operable.
%==============================================================================

%% Guideline 2.1 Keyboard Accessible: Make all functionality available from a
%% keyboard.
%% ----------------------------------------------------------------------------

%% Success Criterion 2.1.1 Keyboard: All functionality of the content is
%% operable through a keyboard interface without requiring specific timings for
%% individual keystrokes, except where the underlying function requires input
%% that depends on the path of the user's movement and not just the endpoints.
%%
%% - Note 1: This exception relates to the underlying function, not the input
%% technique. For example, if using handwriting to enter text, the input
%% technique (handwriting) requires path-dependent input but the underlying
%% function (text input) does not.
%% - Note 2: This does not forbid and should not discourage providing mouse
%% input or other input methods in addition to keyboard operation.
success_criterion_2_1_1_keyboard(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_1_1_keyboard/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.1.1").


%% Success Criterion 2.1.2 No Keyboard Trap: If keyboard focus can be moved to a
%% component of the page using a keyboard interface, then focus can be moved
%% away from that component using only a keyboard interface, and, if it requires
%% more than unmodified arrow or tab keys or other standard exit methods, the
%% user is advised of the method for moving focus away.
%% - Note: Since any content that does not meet this success criterion can
%% interfere with a user's ability to use the whole page, all content on the
%% Web page (whether it is used to meet other success criteria or not) must meet
%% this success criterion. See Conformance Requirement 5: Non-Interference.
success_criterion_2_1_2_no_keyboard_trap(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_1_2_no_keyboard_trap/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.1.2").


%% Guideline 2.2 Enough Time: Provide users enough time to read and use content
%% ----------------------------------------------------------------------------

%% Success Criterion 2.2.1 Timing Adjustable: For each time limit that is set by
%% the content, at least one of the following is true:
%%     * Turn off: The user is allowed to turn off the time limit before
%% encountering it; or
%%     * Adjust: The user is allowed to adjust the time limit before
%% encountering it over a wide range that is at least ten times the length of
%% the default setting; or
%%     * Extend: The user is warned before time expires and given at least 20
%% seconds to extend the time limit with a simple action (for example, "press
%% the space bar"), and the user is allowed to extend the time limit at least
%% ten times; or
%%     * Real-time Exception: The time limit is a required part of a real-time
%% event (for example, an auction), and no alternative to the time limit is
%% possible; or
%%     * Essential Exception: The time limit is essential and extending it would
%% invalidate the activity; or
%%     20 Hour Exception: The time limit is longer than 20 hours.
%% - Note: This success criterion helps ensure that users can complete tasks
%% without unexpected changes in content or context that are a result of a time
%% limit. This success criterion should be considered in conjunction with
%% Success Criterion 3.2.1, which puts limits on changes of content or context
%% as a result of user action.
success_criterion_2_2_1_timing_adjustable(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_2_1_timing_adjustable/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.2.1").


%% Success Criterion 2.2.2 Pause, Stop, Hide: For moving, blinking, scrolling,
%% or auto-updating information, all of the following are true:
%%     * Moving, blinking, scrolling: For any moving, blinking or scrolling
%% information that (1) starts automatically, (2) lasts more than five seconds,
%% and (3) is presented in parallel with other content, there is a mechanism for
%% the user to pause, stop, or hide it unless the movement, blinking, or
%% scrolling is part of an activity where it is essential; and
%%     * Auto-updating: For any auto-updating information that (1) starts
%% automatically and (2) is presented in parallel with other content, there is
%% a mechanism for the user to pause, stop, or hide it or to control the
%% frequency of the update unless the auto-updating is part of an activity
%% where it is essential.
%% - Note 1: For requirements related to flickering or flashing content, refer
%% to Guideline 2.3.
%% - Note 2: Since any content that does not meet this success criterion can
%% interfere with a user's ability to use the whole page, all content on the
%% Web page (whether it is used to meet other success criteria or not) must meet
%% this success criterion. See Conformance Requirement 5: Non-Interference.
%% - Note 3: Content that is updated periodically by software or that is
%% streamed to the user agent is not required to preserve or present information
%% that is generated or received between the initiation of the pause and
%% resuming presentation, as this may not be technically possible, and in many
%% situations could be misleading to do so.
%% - Note 4: An animation that occurs as part of a preload phase or similar
%% situation can be considered essential if interaction cannot occur during that
%% phase for all users and if not indicating progress could confuse users or
%% cause them to think that content was frozen or broken.
success_criterion_2_2_2_pause_stop_hide(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_2_2_pause_stop_hide/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.2.2").


%% Guideline 2.3 Seizures: Do not design content in a way that is known to
%% cause seizures.
%% ----------------------------------------------------------------------------

%% Success Criterion 2.3.1 Three Flashes or Below Threshold: Web pages do not
%% contain anything that flashes more than three times in any one second period,
%% or the flash is below the general flash and red flash thresholds.
%% - Note: Since any content that does not meet this success criterion can
%% interfere with a user's ability to use the whole page, all content on the
%% Web page (whether it is used to meet other success criteria or not) must
%% meet this success criterion. See Conformance Requirement 5:
%% Non-Interference.
success_criterion_2_3_1_three_flashes_or_below_threshold(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_3_1_three_flashes_or_below_threshold/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.3.1").


%% Guideline 2.4 Navigable: Provide ways to help users navigate, find content,
%% and determine where they are.
%% ----------------------------------------------------------------------------

%% Success Criterion 2.4.1 Bypass Blocks: A mechanism is available to bypass
%% blocks of content that are repeated on multiple Web pages.
success_criterion_2_4_1_bypass_blocks(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_4_1_bypass_blocks/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.4.1").


%% Success Criterion 2.4.2 Page Titled: Web pages have titles that describe
%% topic or purpose.
success_criterion_2_4_2_page_titled(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_4_2_page_titled/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.4.2").


%% Success Criterion 2.4.3 Focus Order: If a Web page can be navigated
%% sequentially and the navigation sequences affect meaning or operation,
%% focusable components receive focus in an order that preserves meaning and
%% operability.
success_criterion_2_4_3_focus_order(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_4_3_focus_order/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.4.3").


%% Success Criterion 2.4.4 Link Purpose (In Context): The purpose of each link
%% can be determined from the link text alone or from the link text together
%% with its programmatically determined link context, except where the purpose
%% of the link would be ambiguous to users in general.
success_criterion_2_4_4_link_purpose(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_2_4_4_link_purpose/3, [LevelOfConformance, Content, EntryType]}
         ],
         "2.4.4").


%==============================================================================
%% Principle 3: Understandable - Information and the operation of user
%% interface must be understandable.
%==============================================================================

%% Guideline 3.1 Readable: Make text content readable and understandable.
%% ----------------------------------------------------------------------------

%% Success Criterion 3.1.1 Language of Page: The default human language of each
%% Web page can be programmatically determined.
success_criterion_3_1_1_language_of_page(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_3_1_1_language_of_page/3, [LevelOfConformance, Content, EntryType]}
         ],
         "3.1.1").


%% Guideline 3.2 Predictable: Make Web pages appear and operate in predictable
%% ways.
%% ----------------------------------------------------------------------------

%% Success Criterion 3.2.1 On Focus: When any component receives focus, it does
%% not initiate a change of context.
success_criterion_3_2_1_on_focus(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_3_2_1_on_focus/3, [LevelOfConformance, Content, EntryType]}
         ],
         "3.2.1").


%% Success Criterion 3.2.2 On Input: Changing the setting of any user interface
%% component does not automatically cause a change of context unless the user
%% has been advised of the behavior before using the component.
success_criterion_3_2_2_on_input(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_3_2_2_on_input/3, [LevelOfConformance, Content, EntryType]}
         ],
         "3.2.2").


%% Guideline 3.3 Input Assistance: Help users avoid and correct mistakes.
%% ----------------------------------------------------------------------------

%% Success Criterion 3.3.1 Error Identification: If an input error is
%% automatically detected, the item that is in error is identified and the
%% error is described to the user in text.
success_criterion_3_3_1_error_identification(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_3_3_1_error_identification/3, [LevelOfConformance, Content, EntryType]}
         ],
         "3.3.1").


%% Success Criterion 3.3.2 Labels or Instructions: Labels or instructions are
%% provided when content requires user input.
success_criterion_3_3_2_labels_or_instructions(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_3_3_2_labels_or_instructions/3, [LevelOfConformance, Content, EntryType]}
         ],
         "3.3.2").


%==============================================================================
%% Principle 4: Robust - Content must be robust enough that it can be
%% interpreted reliably by a wide variety of user agents, including assistive
%% technologies.
%==============================================================================

%% Guideline 4.1 Compatible: Maximize compatibility with current and future
%% user agents, including assistive technologies.
%% ----------------------------------------------------------------------------

%% Success Criterion 4.1.1 Parsing: In content implemented using markup
%% languages, elements have complete start and end tags, elements are nested
%% according to their specifications, elements do not contain duplicate
%% attributes, and any IDs are unique, except where the specifications allow
%% these features.
%% - Note: Start and end tags that are missing a critical character in their
%% formation, such as a closing angle bracket or a mismatched attribute value
%% quotation mark are not complete.
success_criterion_4_1_1_parsing(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_4_1_1_parsing/3, [LevelOfConformance, Content, EntryType]}
         ],
         "4.1.1").


%% Success Criterion 4.1.2 Name, Role, Value: For all user interface components
%% (including but not limited to: form elements, links and components generated
%% by scripts), the name and role can be programmatically determined; states,
%% properties, and values that can be set by the user can be programmatically
%% set; and notification of changes to these items is available to user agents,
%% including assistive technologies.
%% - Note: This success criterion is primarily for Web authors who develop or
%% script their own user interface components. For example, standard HTML
%% controls already meet this success criterion when used according to
%% specification.
success_criterion_4_1_2_name_role_value(LevelOfConformance, [?HTML_TECH] = _TechnologyList, Content, EntryType) ->
    eval([
          {?HTML_LABEL, fun html_wcag2:success_criterion_4_1_2_name_role_value/3, [LevelOfConformance, Content, EntryType]}
         ],
         "4.1.2").


%% Check the Success Criterions related to the Level A of Conformance.
conformance_level(?ClevelA = _LevelOfConformance, TechnologyList, Content, EntryType) ->
    Args = [?ClevelA, TechnologyList, Content, EntryType],
    eval_conformance_level([
                            {fun success_criterion_1_1_1_non_text_content/4,                 Args},
                            {fun success_criterion_1_2_1_time_based_media/4,                 Args},
                            {fun success_criterion_1_2_2_captions/4,                         Args},
                            {fun success_criterion_1_2_3_audio_desc_or_media_alternative/4,  Args},
                            {fun success_criterion_1_3_1_info_and_relationships/4,           Args},
                            {fun success_criterion_1_3_2_meaningful_sequence/4,              Args},
                            {fun success_criterion_1_3_3_audio_sensory_characteristics/4,    Args},
                            {fun success_criterion_1_4_1_use_of_color/4,                     Args},
                            {fun success_criterion_1_4_2_audio_control/4,                    Args},
                            {fun success_criterion_2_1_1_keyboard/4,                         Args},
                            {fun success_criterion_2_1_2_no_keyboard_trap/4,                 Args},
                            {fun success_criterion_2_2_1_timing_adjustable/4,                Args},
                            {fun success_criterion_2_2_2_pause_stop_hide/4,                  Args},
                            {fun success_criterion_2_3_1_three_flashes_or_below_threshold/4, Args},
                            {fun success_criterion_2_4_1_bypass_blocks/4,                    Args},
                            {fun success_criterion_2_4_2_page_titled/4,                      Args},
                            {fun success_criterion_2_4_3_focus_order/4,                      Args},
                            {fun success_criterion_2_4_4_link_purpose/4,                     Args},
                            {fun success_criterion_3_1_1_language_of_page/4,                 Args},
                            {fun success_criterion_3_2_1_on_focus/4,                         Args},
                            {fun success_criterion_3_2_2_on_input/4,                         Args},
                            {fun success_criterion_3_3_1_error_identification/4,             Args},
                            {fun success_criterion_3_3_2_labels_or_instructions/4,           Args},
                            {fun success_criterion_4_1_1_parsing/4,                          Args},
                            {fun success_criterion_4_1_2_name_role_value/4,                  Args}
                           ],
                           ?ClevelA_LABEL).
