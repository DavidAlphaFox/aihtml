%% Import every aihtml builder so pages read as markup:
%%
%%   -include_lib("aihtml/include/aihtml.hrl").
%%
%%   'div'([button(<<"Save">>, save, [primary], [])], [<<"p-4">>], []).
%%
%% Leave this header out and call aihtml:button/4 etc. when an imported
%% name would clash with a local function.
-import(aihtml,
        ['div'/1, 'div'/3, span/1, span/3, p/1, p/3, a/1, a/3,
         h1/1, h1/3, h2/1, h2/3, h3/1, h3/3, h4/1, h4/3,
         ul/1, ul/3, ol/1, ol/3, li/1, li/3, dl/1, dl/3, dt/1, dt/3, dd/1, dd/3,
         section/1, section/3, article/1, article/3, aside/1, aside/3,
         header/1, header/3, footer/1, footer/3, nav/1, nav/3, main/1, main/3,
         form/1, form/3, fieldset/1, fieldset/3, legend/1, legend/3, label/1, label/3,
         strong/1, strong/3, em/1, em/3, small/1, small/3, code/1, code/3,
         pre/1, pre/3, blockquote/1, blockquote/3,
         table/1, table/3, thead/1, thead/3, tbody/1, tbody/3,
         tr/1, tr/3, th/1, th/3, td/1, td/3,
         br/0, hr/2, img/2, text/1, safe/1, fetch/3, fetch/4, on/2, on/3,
         button/4, checkbox/4, radio/4, switch/4,
         input/3, textarea/3, select/4, field/4,
         card/3, alert/3, badge/3, tabs/4, theme_switcher/2]).
