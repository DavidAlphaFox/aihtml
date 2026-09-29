%% The fields every aihtml element record starts with (designs/05-records.md).
%%
%%   -record(ah_button, {?AH_BASE(aihtml_form_buttons), body = [], ...}).
%%
%% Their positions are part of the contract: the renderer reads `module'
%% at position 2 and aihtml_element:base/1 reads the others by position,
%% so a record's own fields always come after these.
%%
%% `delegate' defaults to ?MODULE, which is expanded in the module that
%% includes this header: a page writing `#ah_button{postback = {save, Id}}'
%% gets its own module as the action module.
-ifndef(AIHTML_ELEMENT_HRL).
-define(AIHTML_ELEMENT_HRL, true).

-define(AH_BASE(Module),
        module = Module :: module(),
        id = undefined :: undefined | atom() | iodata(),
        css = [] :: aihtml_html:css(),
        attrs = [] :: aihtml_html:attrs(),
        postback = undefined :: undefined | aihtml_element:postback(),
        delegate = ?MODULE :: module()).

-endif.
