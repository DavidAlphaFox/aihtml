%% Element records of aihtml_display (designs/05-records.md). Field names
%% follow the catalog: modifier groups, flags and options are fields of
%% the same name, with the catalog's defaults (aihtml_display_tests
%% checks that they agree). Option fields default to what the component
%% used when the option was left out.
-ifndef(AIHTML_DISPLAY_HRL).
-define(AIHTML_DISPLAY_HRL, true).

-include("aihtml_element.hrl").

-type ah_disp_color() :: primary | secondary | success | warning | error | info.
%% A theme colour atom or a CSS colour (#hex, rgb(...), a name).
-type ah_disp_css_color() :: ah_disp_color() | iodata().
-type ah_disp_kpi_icon() :: users | download | install | star | trending_up | trending_down.
%% #{date, title, subtitle, icon, description, dot, expanded}
-type ah_disp_timeline_item() :: #{atom() => term()}.
%% #{name, value, rank, secondary, sub_value, code, tag, attrs}
-type ah_disp_ranking_item() :: #{atom() => term()}.
%% #{label, value, url} | {Label, Value} | {Label, Value, Url}
-type ah_disp_tag() :: #{atom() => term()} | {aihtml_html:html(), number()}
                     | {aihtml_html:html(), number(), iodata()}.

%% An image with an initials fallback (`body'). No postback event.
-record(ah_avatar, {?AH_BASE(aihtml_display),
                    body = [] :: aihtml_html:html(),
                    size = md :: sm | md | lg | xl,
                    shape = circle :: circle | square | rounded,
                    color = primary :: ah_disp_color(),
                    src = undefined :: undefined | iodata(),
                    alt = <<>> :: iodata()}).

%% A count or status dot on the corner of `body' (standalone when blank).
%% No postback event.
-record(ah_badge, {?AH_BASE(aihtml_display),
                   body = [] :: aihtml_html:html(),
                   variant = standard :: standard | dot | online | away | busy | offline
                                       | invisible,
                   color = primary :: default | ah_disp_color(),
                   overlap = rect :: rect | circular,
                   vertical = top :: top | bottom,
                   horizontal = right :: left | right,
                   show_zero = false :: boolean(),
                   count = undefined :: undefined | number() | aihtml_html:html(),
                   max = 99 :: number()}).

%% A compact label. `value' defaults to a binary `body'. Postback fires on
%% change (after the chip removed itself) for a removable chip that is not
%% clickable, otherwise on click.
-record(ah_chip, {?AH_BASE(aihtml_display),
                  body = [] :: aihtml_html:html(),
                  variant = filled :: filled | outlined | soft,
                  color = default :: default | ah_disp_color(),
                  size = medium :: small | medium,
                  removable = false :: boolean(),
                  clickable = false :: boolean(),
                  disabled = false :: boolean(),
                  avatar = undefined :: aihtml_html:html(),
                  icon = undefined :: aihtml_html:html(),
                  value = undefined :: term()}).

%% A box locked to `ratio' (<<"16/9">>, <<"4:3">>, 1.5, {W, H}; undefined
%% is 16 / 9). `style' is appended to the aspect-ratio style. No postback
%% event.
-record(ah_aspect_ratio, {?AH_BASE(aihtml_display),
                          body = [] :: aihtml_html:html(),
                          ratio = undefined :: undefined | number() | {number(), number()}
                                             | iodata(),
                          style = undefined :: undefined | iodata()}).

%% One key, or a combination when `keys' is a list of keys. No postback
%% event.
-record(ah_kbd, {?AH_BASE(aihtml_display),
                 keys = [] :: aihtml_html:html() | [aihtml_html:html()],
                 size = md :: md | lg,
                 separator = <<"+">> :: aihtml_html:html()}).

%% Relative time. `timestamp' is Unix seconds, a UTC datetime or an RFC
%% 3339 binary; `now' undefined is the time of rendering. No postback
%% event.
-record(ah_time_ago, {?AH_BASE(aihtml_display),
                      timestamp = undefined :: undefined | integer() | calendar:datetime()
                                             | binary(),
                      now = undefined :: undefined | integer(),
                      labels = #{} :: #{atom() => iodata()},
                      live = true :: boolean(),
                      title = true :: boolean()}).

%% Text cut at `threshold' characters with a toggle; postback fires on
%% 'ah:toggle'.
-record(ah_expandable_text, {?AH_BASE(aihtml_display),
                             text = <<>> :: undefined | unicode:chardata(),
                             threshold = 100 :: non_neg_integer(),
                             expanded = false :: boolean(),
                             expand_label = <<"展开"/utf8>> :: aihtml_html:html(),
                             collapse_label = <<"收起"/utf8>> :: aihtml_html:html()}).

%% An inline message box. `icon' is true (the variant's icon), false or
%% HTML. Postback fires on 'ah:dismiss'.
-record(ah_alert, {?AH_BASE(aihtml_display),
                   body = [] :: aihtml_html:html(),
                   variant = info :: info | success | warning | error,
                   dismissible = false :: boolean(),
                   title = undefined :: aihtml_html:html(),
                   icon = true :: boolean() | aihtml_html:html()}).

%% Linear progress; `value' may be undefined when indeterminate. Postback
%% fires on change (setValue in the browser).
-record(ah_progressbar, {?AH_BASE(aihtml_display),
                         value = undefined :: undefined | number(),
                         orientation = horizontal :: horizontal | vertical,
                         layout = normal :: normal | reverse,
                         color = primary :: primary | success | warning | error | info,
                         show_text = false :: boolean(),
                         disabled = false :: boolean(),
                         indeterminate = false :: boolean(),
                         striped = false :: boolean(),
                         animated = false :: boolean(),
                         min = 0 :: number(),
                         max = 100 :: number(),
                         text = undefined :: aihtml_html:html(),
                         color_ranges = [] :: [{number(), ah_disp_css_color()}]}).

%% Circular progress, `value' 0..100. Postback fires on change (setValue
%% in the browser).
-record(ah_progress_circle, {?AH_BASE(aihtml_display),
                             value = undefined :: undefined | number(),
                             size = md :: sm | md | lg,
                             color = primary :: primary | success | warning | info | error,
                             disabled = false :: boolean(),
                             indeterminate = false :: boolean(),
                             label = undefined :: aihtml_html:html(),
                             show_value = true :: boolean()}).

%% A measurement in [min, max], coloured low / optimum / high. No postback
%% event.
-record(ah_meter, {?AH_BASE(aihtml_display),
                   value = 0 :: number(),
                   size = md :: sm | md | lg,
                   min = 0 :: number(),
                   max = 100 :: number(),
                   low = undefined :: undefined | number(),
                   high = undefined :: undefined | number(),
                   optimum = undefined :: undefined | number(),
                   label = undefined :: aihtml_html:html(),
                   helper_text = undefined :: aihtml_html:html(),
                   show_value = false :: boolean()}).

%% A number with title, prefix / suffix and a delta arrow; a non-number
%% `value' is shown as is. No postback event.
-record(ah_statistic, {?AH_BASE(aihtml_display),
                       value = 0 :: number() | aihtml_html:html(),
                       color = default :: default | primary | success | warning | error,
                       loading = false :: boolean(),
                       title = undefined :: aihtml_html:html(),
                       prefix = undefined :: aihtml_html:html(),
                       suffix = undefined :: aihtml_html:html(),
                       precision = undefined :: undefined | non_neg_integer(),
                       group_separator = true :: boolean(),
                       delta = undefined :: undefined | number()}).

%% A metric card; `trend' is a percentage (> 0 up). No postback event.
-record(ah_kpi_card, {?AH_BASE(aihtml_display),
                      value = [] :: aihtml_html:html(),
                      color = undefined :: undefined | primary | success | warning | info
                                         | error,
                      disabled = false :: boolean(),
                      title = undefined :: aihtml_html:html(),
                      trend = undefined :: undefined | number(),
                      trend_label = undefined :: aihtml_html:html(),
                      icon = undefined :: undefined | ah_disp_kpi_icon()
                                        | aihtml_html:html()}).

%% Events along an axis; cards with a description expand on click.
%% Postback fires on 'ah:toggle'.
-record(ah_timeline, {?AH_BASE(aihtml_display),
                      items = [] :: [ah_disp_timeline_item()],
                      position = both :: both | near | far,
                      horizontal = false :: boolean(),
                      disabled = false :: boolean(),
                      collapsible = true :: boolean()}).

%% A top N list. Postback fires on 'ah:item-click' (clickable rows).
-record(ah_ranking_list, {?AH_BASE(aihtml_display),
                          items = [] :: [ah_disp_ranking_item()],
                          dense = false :: boolean(),
                          disabled = false :: boolean(),
                          clickable = false :: boolean(),
                          title = undefined :: aihtml_html:html(),
                          max_items = undefined :: undefined | non_neg_integer(),
                          show_rank = true :: boolean(),
                          flag_style = emoji :: emoji | flag_icons | none,
                          tag_colors = #{} :: #{binary() => success | warning | error | info}}).

%% Tags sized by weight. Postback fires on 'ah:tag-click'.
-record(ah_tag_cloud, {?AH_BASE(aihtml_display),
                       items = [] :: [ah_disp_tag()],
                       disabled = false :: boolean(),
                       min_font_size = 10 :: number(),
                       max_font_size = 24 :: number(),
                       font_size_unit = px :: px | em | 'rem' | pt | '%' | binary(),
                       url_base = <<>> :: iodata(),
                       display_value = false :: boolean(),
                       sort_by = none :: none | label | value,
                       sort_order = ascending :: ascending | descending,
                       text_case = none :: none | all_lower | all_upper | first_upper
                                         | title_case,
                       text_color = undefined :: undefined | ah_disp_css_color(),
                       min_color = undefined :: undefined | iodata(),
                       max_color = undefined :: undefined | iodata(),
                       min_value = 0 :: number(),
                       max_value = 0 :: number(),
                       display_limit = undefined :: undefined | pos_integer(),
                       take_top_weighted = false :: boolean()}).

-endif.
