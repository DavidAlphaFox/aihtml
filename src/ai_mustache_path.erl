%%%-------------------------------------------------------------------
%%% @doc Deprecated forwarding shell for {@link ai_html_path}.
%%%
%%% Path resolution and template-attribute scanning are engine-neutral and now
%%% live in `ai_html_path', with the attribute name as a parameter
%%% (designs/08-jinja-architecture.md section 1.1). Scheduled for removal in
%%% v0.6.
%%% @end
%%%-------------------------------------------------------------------
-module(ai_mustache_path).

-export([resolve/2, template_spec/1, scan/1]).

-deprecated([{resolve,       2, "use ai_html_path:resolve/2"},
             {template_spec, 1, "use ai_html_path:template_spec/1"},
             {scan,          1, "use ai_html_path:scan/2"}]).

-type template() :: ai_html_path:template().
-export_type([template/0]).

-spec resolve(file:filename_all(), [file:filename_all()]) ->
          {ok, file:filename_all()} | {error, {not_found, [file:filename_all()]}}.
resolve(Path, Dirs) -> ai_html_path:resolve(Path, Dirs).

-spec template_spec(term()) -> {ok, template()} | error.
template_spec(Term) -> ai_html_path:template_spec(Term).

%% @doc As ai_html_path:scan/2 with the `mustache_template' attribute.
-spec scan(file:filename_all()) -> {ok, [template()]} | {error, term()}.
scan(File) -> ai_html_path:scan(File, mustache_template).
