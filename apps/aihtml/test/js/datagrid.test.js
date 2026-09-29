/* datagrid: the datagrid controller on markup the server renders. SERVER
 * holds renders of aihtml_datagrid:datagrid/4 (ids "g", "gg", "rg") and
 * the operations datagrid_rows/4 sends for page 2 of the remote grid;
 * "rlinked" is a remote grid with the href option (id "rl", columns id,
 * name, age, page_size 2, total 5, href /grid?page={page}&size={size}&sort={sort});
 * regenerate them from Erlang if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
 "rlinked": "<div class=\"ah-dg\" id=\"rl\" role=\"grid\" aria-rowcount=\"6\" aria-colcount=\"3\" data-ah=\"datagrid\" data-ah-value=\"\" data-ah-selection=\"single\" data-ah-edit-mode=\"dblclick\" data-ah-remote data-ah-pageable data-ah-header-rows=\"1\" data-ah-export-name=\"data\" data-ah-labels=\"{&quot;avg&quot;:&quot;Avg&quot;,&quot;clear_groups&quot;:&quot;Clear all groups&quot;,&quot;column_menu&quot;:&quot;Column menu&quot;,&quot;columns&quot;:&quot;Columns&quot;,&quot;count&quot;:&quot;Count&quot;,&quot;empty&quot;:&quot;No data&quot;,&quot;export_csv&quot;:&quot;CSV&quot;,&quot;export_pdf&quot;:&quot;PDF&quot;,&quot;export_xlsx&quot;:&quot;Excel&quot;,&quot;filter&quot;:&quot;Filter...&quot;,&quot;first_page&quot;:&quot;First page&quot;,&quot;group_by&quot;:&quot;Group by this column&quot;,&quot;hide_column&quot;:&quot;Hide column&quot;,&quot;last_page&quot;:&quot;Last page&quot;,&quot;loading&quot;:&quot;Loading...&quot;,&quot;max&quot;:&quot;Max&quot;,&quot;min&quot;:&quot;Min&quot;,&quot;next_page&quot;:&quot;Next page&quot;,&quot;no&quot;:&quot;No&quot;,&quot;page_size&quot;:&quot;Rows per page&quot;,&quot;pages&quot;:&quot;Pages&quot;,&quot;per_page&quot;:&quot;{0} / page&quot;,&quot;pin&quot;:&quot;Pin to left&quot;,&quot;prev_page&quot;:&quot;Previous page&quot;,&quot;search&quot;:&quot;Search...&quot;,&quot;select_all&quot;:&quot;Select all rows&quot;,&quot;select_row&quot;:&quot;Select row&quot;,&quot;sort_asc&quot;:&quot;Sort ascending&quot;,&quot;sort_clear&quot;:&quot;Clear sort&quot;,&quot;sort_desc&quot;:&quot;Sort descending&quot;,&quot;sum&quot;:&quot;Sum&quot;,&quot;total&quot;:&quot;Total {0}&quot;,&quot;ungroup&quot;:&quot;Ungroup this column&quot;,&quot;unpin&quot;:&quot;Unpin&quot;,&quot;yes&quot;:&quot;Yes&quot;}\" data-ah-loaded=\"true\" data-ah-href=\"/grid?page={page}&amp;size={size}&amp;sort={sort}\" data-sort=\"[]\" data-filter=\"{}\" data-page=\"1\" data-page-size=\"2\" data-group-by=\"[]\" data-render=\"g3QAAAAJdwJpZG0AAAACcmx3B2NvbHVtbnNsAAAAA3QAAAADdwV0aXRsZW0AAAACSUR3A2tleXcCaWR3BXdpZHRoYTJ0AAAAAncFdGl0bGVtAAAABE5hbWV3A2tleXcEbmFtZXQAAAAEdwR0eXBldwZudW1iZXJ3BXRpdGxlbQAAAANBZ2V3A2tleXcDYWdldwVhbGlnbncFcmlnaHRqdwhwYWdlYWJsZXcEdHJ1ZXcJcGFnZV9zaXplYQJ3CnBhZ2Vfc2l6ZXNrAAICBHcEaHJlZm0AAAApL2dyaWQ_cGFnZT17cGFnZX0mc2l6ZT17c2l6ZX0mc29ydD17c29ydH13BmxhYmVsc3QAAAAAdwlrZXlfZmllbGR3AmlkdwlzZWxlY3Rpb253BnNpbmdsZQ.76qqKXxYk_wbFEO0nvWE8ZNpIhujsGkG4-lr72P7aCg\"><div class=\"ah-dg-container\"><div class=\"ah-dg-header-wrap\"><div class=\"ah-dg-header\" role=\"rowgroup\"><div class=\"ah-dg-header-row\" role=\"row\" aria-rowindex=\"1\"><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"id\" aria-sort=\"none\" style=\"width:50px\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">ID</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"id\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"name\" aria-sort=\"none\" style=\"width:100px\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Name</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"name\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-right ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"age\" aria-sort=\"none\" style=\"width:100px\" data-type=\"number\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Age</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"age\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div></div></div></div><div class=\"ah-dg-body-wrap\"><div class=\"ah-dg-body\" id=\"rl-body\" role=\"rowgroup\"><div class=\"ah-dg-row ah-dg-row-even\" id=\"rl-r-1\" role=\"row\" data-key=\"1\" aria-selected=\"false\" aria-rowindex=\"2\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">1</span></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"name\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Ann</span></div><div class=\"ah-dg-cell ah-dg-align-right\" role=\"gridcell\" data-field=\"age\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">30</span></div></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"rl-r-2\" role=\"row\" data-key=\"2\" aria-selected=\"false\" aria-rowindex=\"3\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">2</span></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"name\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">bob</span></div><div class=\"ah-dg-cell ah-dg-align-right\" role=\"gridcell\" data-field=\"age\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">25</span></div></div><div class=\"ah-dg-empty-message\" hidden>No data</div></div></div><div class=\"ah-dg-pager-wrap\" id=\"rl-pager\"><div class=\"ah-dg-pager\" role=\"navigation\" aria-label=\"Pages\"><div class=\"ah-dg-pager-info\" aria-live=\"polite\">Total 5</div><div class=\"ah-dg-pager-controls\"><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"First page\" disabled>|&lt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"Previous page\" disabled>&lt;</button><a class=\"ah-dg-pager-button ah-dg-pager-button-active\" href=\"/grid?page=1&amp;size=2&amp;sort=\" data-page=\"1\" aria-current=\"page\">1</a><a class=\"ah-dg-pager-button\" href=\"/grid?page=2&amp;size=2&amp;sort=\" data-page=\"2\">2</a><a class=\"ah-dg-pager-button\" href=\"/grid?page=3&amp;size=2&amp;sort=\" data-page=\"3\">3</a><a class=\"ah-dg-pager-button\" href=\"/grid?page=2&amp;size=2&amp;sort=\" data-page=\"2\" aria-label=\"Next page\">&gt;</a><a class=\"ah-dg-pager-button\" href=\"/grid?page=3&amp;size=2&amp;sort=\" data-page=\"3\" aria-label=\"Last page\">&gt;|</a></div><div class=\"ah-dg-pager-size\"><select class=\"ah-dg-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2 / page</option><option value=\"4\">4 / page</option></select></div></div></div></div><div class=\"ah-dg-column-menu\" id=\"rl-menu\" role=\"menu\"></div><div class=\"ah-dg-resize-line\" aria-hidden=\"true\"></div><div class=\"ah-dg-loading-overlay\" style=\"display:none;\"><div class=\"ah-dg-loading-message\">Loading...</div></div><div class=\"ah-dg-query\" id=\"rl-q\" hidden data-grid=\"rl\" data-ah-sync=\"replace\" data-ah-on=\"ah:query:g2gDdxVhaWh0bWxfZGF0YWdyaWRfdGVzdHN3BnBlb3BsZXQAAAAA.JMnO4flxnm816wXqaAfXYBkBPAD8AUBBubZPBS8G-fk\"></div></div>",
 "local": "<div class=\"ah-dg\" id=\"g\" role=\"grid\" aria-multiselectable=\"true\" aria-rowcount=\"7\" aria-colcount=\"5\" data-ah=\"datagrid\" data-ah-value=\"\" data-ah-selection=\"multi\" data-ah-edit-mode=\"dblclick\" data-ah-pageable data-ah-header-rows=\"2\" data-ah-export-name=\"people\" data-ah-labels=\"{&quot;clear_groups&quot;:&quot;Clear all groups&quot;,&quot;sum&quot;:&quot;Sum&quot;,&quot;max&quot;:&quot;Max&quot;,&quot;select_all&quot;:&quot;Select all rows&quot;,&quot;pin&quot;:&quot;Pin to left&quot;,&quot;total&quot;:&quot;Total {0}&quot;,&quot;prev_page&quot;:&quot;Previous page&quot;,&quot;sort_desc&quot;:&quot;Sort descending&quot;,&quot;ungroup&quot;:&quot;Ungroup this column&quot;,&quot;loading&quot;:&quot;Loading...&quot;,&quot;columns&quot;:&quot;Columns&quot;,&quot;count&quot;:&quot;Count&quot;,&quot;yes&quot;:&quot;Yes&quot;,&quot;pages&quot;:&quot;Pages&quot;,&quot;group_by&quot;:&quot;Group by this column&quot;,&quot;page_size&quot;:&quot;Rows per page&quot;,&quot;select_row&quot;:&quot;Select row&quot;,&quot;column_menu&quot;:&quot;Column menu&quot;,&quot;search&quot;:&quot;Search...&quot;,&quot;export_pdf&quot;:&quot;PDF&quot;,&quot;last_page&quot;:&quot;Last page&quot;,&quot;sort_asc&quot;:&quot;Sort ascending&quot;,&quot;min&quot;:&quot;Min&quot;,&quot;filter&quot;:&quot;Filter...&quot;,&quot;first_page&quot;:&quot;First page&quot;,&quot;export_csv&quot;:&quot;CSV&quot;,&quot;next_page&quot;:&quot;Next page&quot;,&quot;per_page&quot;:&quot;{0} / page&quot;,&quot;empty&quot;:&quot;No data&quot;,&quot;export_xlsx&quot;:&quot;Excel&quot;,&quot;hide_column&quot;:&quot;Hide column&quot;,&quot;sort_clear&quot;:&quot;Clear sort&quot;,&quot;unpin&quot;:&quot;Unpin&quot;,&quot;avg&quot;:&quot;Avg&quot;,&quot;no&quot;:&quot;No&quot;}\" data-sort=\"[]\" data-filter=\"{}\" data-page=\"1\" data-page-size=\"3\" data-group-by=\"[]\" data-render=\"g3QAAAAIdwJpZG0AAAABZ3cHY29sdW1uc2wAAAAFdAAAAAN3BXRpdGxlbQAAAAJJRHcDa2V5dwJpZHcFd2lkdGhhMnQAAAADdwV0aXRsZW0AAAAETmFtZXcDa2V5dwRuYW1ldwhlZGl0YWJsZXcEdHJ1ZXQAAAAFdwR0eXBldwZzZWxlY3R3B29wdGlvbnNsAAAAAmgCdwNlbmdtAAAAC0VuZ2luZWVyaW5naAJ3A29wc20AAAAKT3BlcmF0aW9uc2p3BXRpdGxlbQAAAAREZXB0dwNrZXl3BGRlcHR3CGVkaXRhYmxldwR0cnVldAAAAAZ3BHR5cGV3Bm51bWJlcncFdGl0bGVtAAAAA0FnZXcDa2V5dwNhZ2V3CGVkaXRhYmxldwR0cnVldwphZ2dyZWdhdGVzbAAAAAJ3A3N1bXcDYXZnancFYWxpZ253BXJpZ2h0dAAAAAR3BHR5cGV3BGJvb2x3BXRpdGxlbQAAAAZBY3RpdmV3A2tleXcGYWN0aXZldwhlZGl0YWJsZXcEdHJ1ZWp3CHBhZ2VhYmxldwR0cnVldwlwYWdlX3NpemVhA3cKcGFnZV9zaXplc2sAAgMKdwZsYWJlbHN0AAAAAHcJa2V5X2ZpZWxkdwJpZHcJc2VsZWN0aW9udwVtdWx0aQ.4QBoVa7eUmEdOfj7KqwB4lSaEFXuIE7TGeosvAUH1lc\"><div class=\"ah-dg-container\"><div class=\"ah-dg-toolbar-wrap\"><div class=\"ah-dg-toolbar\" role=\"toolbar\"><button class=\"ah-dg-toolbar-btn\" type=\"button\" data-export=\"csv\"><span class=\"ah-dg-toolbar-btn-icon\" aria-hidden=\"true\">⤓</span><span class=\"ah-dg-toolbar-btn-text\">CSV</span></button><input class=\"ah-dg-search-input\" type=\"search\" placeholder=\"Search...\" aria-label=\"Search...\" autocomplete=\"off\"></div></div><div class=\"ah-dg-header-wrap\"><div class=\"ah-dg-header\" role=\"rowgroup\"><div class=\"ah-dg-header-row\" role=\"row\" aria-rowindex=\"1\"><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"id\" aria-sort=\"none\" style=\"width:50px\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">ID</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"id\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"name\" aria-sort=\"none\" style=\"width:100px\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Name</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"name\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"dept\" aria-sort=\"none\" style=\"width:100px\" data-type=\"select\" data-editable=\"true\" data-min-width=\"40\" data-options=\"[[&quot;eng&quot;,&quot;Engineering&quot;],[&quot;ops&quot;,&quot;Operations&quot;]]\"><span class=\"ah-dg-header-cell-content\">Dept</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"dept\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-right ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"age\" aria-sort=\"none\" style=\"width:100px\" data-type=\"number\" data-editable=\"true\" data-min-width=\"40\" data-aggs=\"sum,avg\"><span class=\"ah-dg-header-cell-content\">Age</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"age\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"active\" aria-sort=\"none\" style=\"width:100px\" data-type=\"bool\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Active</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"active\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div></div><div class=\"ah-dg-header-filter-row\" role=\"row\" aria-rowindex=\"2\"><div class=\"ah-dg-filter-cell\" data-field=\"id\" style=\"width:50px\"><input class=\"ah-dg-filter-input\" type=\"text\" data-field=\"id\" value=\"\" placeholder=\"Filter...\" autocomplete=\"off\" aria-label=\"ID\"></div><div class=\"ah-dg-filter-cell\" data-field=\"name\" style=\"width:100px\"><input class=\"ah-dg-filter-input\" type=\"text\" data-field=\"name\" value=\"\" placeholder=\"Filter...\" autocomplete=\"off\" aria-label=\"Name\"></div><div class=\"ah-dg-filter-cell\" data-field=\"dept\" style=\"width:100px\"><input class=\"ah-dg-filter-input\" type=\"text\" data-field=\"dept\" value=\"\" placeholder=\"Filter...\" autocomplete=\"off\" aria-label=\"Dept\"></div><div class=\"ah-dg-filter-cell\" data-field=\"age\" style=\"width:100px\"><input class=\"ah-dg-filter-input\" type=\"text\" data-field=\"age\" value=\"\" placeholder=\"Filter...\" autocomplete=\"off\" aria-label=\"Age\"></div><div class=\"ah-dg-filter-cell\" data-field=\"active\" style=\"width:100px\"><input class=\"ah-dg-filter-input\" type=\"text\" data-field=\"active\" value=\"\" placeholder=\"Filter...\" autocomplete=\"off\" aria-label=\"Active\"></div></div></div></div><div class=\"ah-dg-body-wrap\"><div class=\"ah-dg-body\" id=\"g-body\" role=\"rowgroup\"><div class=\"ah-dg-row ah-dg-row-even\" id=\"g-r-1\" role=\"row\" data-key=\"1\" aria-selected=\"false\" aria-rowindex=\"3\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">1</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Ann</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">30</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"g-r-2\" role=\"row\" data-key=\"2\" aria-selected=\"false\" aria-rowindex=\"4\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">2</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">bob</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">25</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-row ah-dg-row-even\" id=\"g-r-3\" role=\"row\" data-key=\"3\" aria-selected=\"false\" aria-rowindex=\"5\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">3</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Cy</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">41</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-even ah-dg-row-off\" id=\"g-r-4\" role=\"row\" data-key=\"4\" aria-selected=\"false\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">4</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Dee</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">35</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-row ah-dg-row-even ah-dg-row-off\" id=\"g-r-5\" role=\"row\" data-key=\"5\" aria-selected=\"false\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">5</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Eve</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">28</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-empty-message\" hidden>No data</div></div></div><div class=\"ah-dg-statusbar-wrap\"><div class=\"ah-dg-statusbar\" role=\"status\"><div class=\"ah-dg-statusbar-row\"><div class=\"ah-dg-statusbar-cell\" data-field=\"id\" style=\"width:50px\"></div><div class=\"ah-dg-statusbar-cell\" data-field=\"name\" style=\"width:100px\"></div><div class=\"ah-dg-statusbar-cell\" data-field=\"dept\" style=\"width:100px\"></div><div class=\"ah-dg-statusbar-cell\" data-field=\"age\" style=\"width:100px\"><span class=\"ah-dg-statusbar-item\" data-agg=\"sum\"><span class=\"ah-dg-statusbar-label\">Sum: </span><span class=\"ah-dg-statusbar-value\">159</span></span><span class=\"ah-dg-statusbar-item\" data-agg=\"avg\"><span class=\"ah-dg-statusbar-label\">Avg: </span><span class=\"ah-dg-statusbar-value\">31.80</span></span></div><div class=\"ah-dg-statusbar-cell\" data-field=\"active\" style=\"width:100px\"></div></div></div></div><div class=\"ah-dg-pager-wrap\" id=\"g-pager\"><div class=\"ah-dg-pager\" role=\"navigation\" aria-label=\"Pages\"><div class=\"ah-dg-pager-info\" aria-live=\"polite\">Total 5</div><div class=\"ah-dg-pager-controls\"><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"First page\" disabled>|&lt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"Previous page\" disabled>&lt;</button><button type=\"button\" class=\"ah-dg-pager-button ah-dg-pager-button-active\" data-page=\"1\" aria-current=\"page\">1</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"2\">2</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"2\" aria-label=\"Next page\">&gt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"2\" aria-label=\"Last page\">&gt;|</button></div><div class=\"ah-dg-pager-size\"><select class=\"ah-dg-pager-size-select\" aria-label=\"Rows per page\"><option value=\"3\" selected>3 / page</option><option value=\"10\">10 / page</option></select></div></div></div></div><div class=\"ah-dg-column-menu\" id=\"g-menu\" role=\"menu\"></div><div class=\"ah-dg-resize-line\" aria-hidden=\"true\"></div><div class=\"ah-dg-loading-overlay\" style=\"display:none;\"><div class=\"ah-dg-loading-message\">Loading...</div></div></div>",
 "remote": "<div class=\"ah-dg\" id=\"rg\" role=\"grid\" aria-rowcount=\"6\" aria-colcount=\"5\" data-ah=\"datagrid\" data-ah-value=\"\" data-ah-selection=\"single\" data-ah-edit-mode=\"dblclick\" data-ah-remote data-ah-pageable data-ah-header-rows=\"1\" data-ah-export-name=\"data\" data-ah-labels=\"{&quot;clear_groups&quot;:&quot;Clear all groups&quot;,&quot;sum&quot;:&quot;Sum&quot;,&quot;max&quot;:&quot;Max&quot;,&quot;select_all&quot;:&quot;Select all rows&quot;,&quot;pin&quot;:&quot;Pin to left&quot;,&quot;total&quot;:&quot;Total {0}&quot;,&quot;prev_page&quot;:&quot;Previous page&quot;,&quot;sort_desc&quot;:&quot;Sort descending&quot;,&quot;ungroup&quot;:&quot;Ungroup this column&quot;,&quot;loading&quot;:&quot;Loading...&quot;,&quot;columns&quot;:&quot;Columns&quot;,&quot;count&quot;:&quot;Count&quot;,&quot;yes&quot;:&quot;Yes&quot;,&quot;pages&quot;:&quot;Pages&quot;,&quot;group_by&quot;:&quot;Group by this column&quot;,&quot;page_size&quot;:&quot;Rows per page&quot;,&quot;select_row&quot;:&quot;Select row&quot;,&quot;column_menu&quot;:&quot;Column menu&quot;,&quot;search&quot;:&quot;Search...&quot;,&quot;export_pdf&quot;:&quot;PDF&quot;,&quot;last_page&quot;:&quot;Last page&quot;,&quot;sort_asc&quot;:&quot;Sort ascending&quot;,&quot;min&quot;:&quot;Min&quot;,&quot;filter&quot;:&quot;Filter...&quot;,&quot;first_page&quot;:&quot;First page&quot;,&quot;export_csv&quot;:&quot;CSV&quot;,&quot;next_page&quot;:&quot;Next page&quot;,&quot;per_page&quot;:&quot;{0} / page&quot;,&quot;empty&quot;:&quot;No data&quot;,&quot;export_xlsx&quot;:&quot;Excel&quot;,&quot;hide_column&quot;:&quot;Hide column&quot;,&quot;sort_clear&quot;:&quot;Clear sort&quot;,&quot;unpin&quot;:&quot;Unpin&quot;,&quot;avg&quot;:&quot;Avg&quot;,&quot;no&quot;:&quot;No&quot;}\" data-ah-loaded=\"true\" data-sort=\"[]\" data-filter=\"{}\" data-page=\"1\" data-page-size=\"2\" data-group-by=\"[]\" data-render=\"g3QAAAAIdwJpZG0AAAACcmd3B2NvbHVtbnNsAAAABXQAAAADdwV0aXRsZW0AAAACSUR3A2tleXcCaWR3BXdpZHRoYTJ0AAAAA3cFdGl0bGVtAAAABE5hbWV3A2tleXcEbmFtZXcIZWRpdGFibGV3BHRydWV0AAAABXcEdHlwZXcGc2VsZWN0dwdvcHRpb25zbAAAAAJoAncDZW5nbQAAAAtFbmdpbmVlcmluZ2gCdwNvcHNtAAAACk9wZXJhdGlvbnNqdwV0aXRsZW0AAAAERGVwdHcDa2V5dwRkZXB0dwhlZGl0YWJsZXcEdHJ1ZXQAAAAGdwR0eXBldwZudW1iZXJ3BXRpdGxlbQAAAANBZ2V3A2tleXcDYWdldwhlZGl0YWJsZXcEdHJ1ZXcKYWdncmVnYXRlc2wAAAACdwNzdW13A2F2Z2p3BWFsaWdudwVyaWdodHQAAAAEdwR0eXBldwRib29sdwV0aXRsZW0AAAAGQWN0aXZldwNrZXl3BmFjdGl2ZXcIZWRpdGFibGV3BHRydWVqdwhwYWdlYWJsZXcEdHJ1ZXcJcGFnZV9zaXplYQJ3CnBhZ2Vfc2l6ZXNrAAICBHcGbGFiZWxzdAAAAAB3CWtleV9maWVsZHcCaWR3CXNlbGVjdGlvbncGc2luZ2xl.cxmVnaLQiYyHsyBw1Dn-jPuAbQusIRWFv5iEZ5fNV_c\"><div class=\"ah-dg-container\"><div class=\"ah-dg-header-wrap\"><div class=\"ah-dg-header\" role=\"rowgroup\"><div class=\"ah-dg-header-row\" role=\"row\" aria-rowindex=\"1\"><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"id\" aria-sort=\"none\" style=\"width:50px\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">ID</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"id\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"name\" aria-sort=\"none\" style=\"width:100px\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Name</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"name\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"dept\" aria-sort=\"none\" style=\"width:100px\" data-type=\"select\" data-editable=\"true\" data-min-width=\"40\" data-options=\"[[&quot;eng&quot;,&quot;Engineering&quot;],[&quot;ops&quot;,&quot;Operations&quot;]]\"><span class=\"ah-dg-header-cell-content\">Dept</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"dept\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-right ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"age\" aria-sort=\"none\" style=\"width:100px\" data-type=\"number\" data-editable=\"true\" data-min-width=\"40\" data-aggs=\"sum,avg\"><span class=\"ah-dg-header-cell-content\">Age</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"age\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"active\" aria-sort=\"none\" style=\"width:100px\" data-type=\"bool\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Active</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"active\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div></div></div></div><div class=\"ah-dg-body-wrap\"><div class=\"ah-dg-body\" id=\"rg-body\" role=\"rowgroup\"><div class=\"ah-dg-row ah-dg-row-even\" id=\"rg-r-1\" role=\"row\" data-key=\"1\" aria-selected=\"false\" aria-rowindex=\"2\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">1</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Ann</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">30</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"rg-r-2\" role=\"row\" data-key=\"2\" aria-selected=\"false\" aria-rowindex=\"3\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">2</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">bob</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">25</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-empty-message\" hidden>No data</div></div></div><div class=\"ah-dg-pager-wrap\" id=\"rg-pager\"><div class=\"ah-dg-pager\" role=\"navigation\" aria-label=\"Pages\"><div class=\"ah-dg-pager-info\" aria-live=\"polite\">Total 5</div><div class=\"ah-dg-pager-controls\"><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"First page\" disabled>|&lt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"Previous page\" disabled>&lt;</button><button type=\"button\" class=\"ah-dg-pager-button ah-dg-pager-button-active\" data-page=\"1\" aria-current=\"page\">1</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"2\">2</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"3\">3</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"2\" aria-label=\"Next page\">&gt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"3\" aria-label=\"Last page\">&gt;|</button></div><div class=\"ah-dg-pager-size\"><select class=\"ah-dg-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2 / page</option><option value=\"4\">4 / page</option></select></div></div></div></div><div class=\"ah-dg-column-menu\" id=\"rg-menu\" role=\"menu\"></div><div class=\"ah-dg-resize-line\" aria-hidden=\"true\"></div><div class=\"ah-dg-loading-overlay\" style=\"display:none;\"><div class=\"ah-dg-loading-message\">Loading...</div></div><div class=\"ah-dg-query\" id=\"rg-q\" hidden data-grid=\"rg\" data-ah-sync=\"replace\" data-ah-on=\"ah:query:g2gDdwhmaXh0dXJlc3cBcXQAAAAA.3jgAeAj39ZAXsZT7vzQXu2G0mfJukMPjUzt_pJoaQQ8\"></div></div>",
 "grouped": "<div class=\"ah-dg\" id=\"gg\" role=\"grid\" aria-multiselectable=\"true\" aria-rowcount=\"8\" aria-colcount=\"6\" data-ah=\"datagrid\" data-ah-value=\"2\" data-ah-selection=\"checkbox\" data-ah-edit-mode=\"dblclick\" data-ah-header-rows=\"1\" data-ah-export-name=\"data\" data-ah-labels=\"{&quot;clear_groups&quot;:&quot;Clear all groups&quot;,&quot;sum&quot;:&quot;Sum&quot;,&quot;max&quot;:&quot;Max&quot;,&quot;select_all&quot;:&quot;Select all rows&quot;,&quot;pin&quot;:&quot;Pin to left&quot;,&quot;total&quot;:&quot;Total {0}&quot;,&quot;prev_page&quot;:&quot;Previous page&quot;,&quot;sort_desc&quot;:&quot;Sort descending&quot;,&quot;ungroup&quot;:&quot;Ungroup this column&quot;,&quot;loading&quot;:&quot;Loading...&quot;,&quot;columns&quot;:&quot;Columns&quot;,&quot;count&quot;:&quot;Count&quot;,&quot;yes&quot;:&quot;Yes&quot;,&quot;pages&quot;:&quot;Pages&quot;,&quot;group_by&quot;:&quot;Group by this column&quot;,&quot;page_size&quot;:&quot;Rows per page&quot;,&quot;select_row&quot;:&quot;Select row&quot;,&quot;column_menu&quot;:&quot;Column menu&quot;,&quot;search&quot;:&quot;Search...&quot;,&quot;export_pdf&quot;:&quot;PDF&quot;,&quot;last_page&quot;:&quot;Last page&quot;,&quot;sort_asc&quot;:&quot;Sort ascending&quot;,&quot;min&quot;:&quot;Min&quot;,&quot;filter&quot;:&quot;Filter...&quot;,&quot;first_page&quot;:&quot;First page&quot;,&quot;export_csv&quot;:&quot;CSV&quot;,&quot;next_page&quot;:&quot;Next page&quot;,&quot;per_page&quot;:&quot;{0} / page&quot;,&quot;empty&quot;:&quot;No data&quot;,&quot;export_xlsx&quot;:&quot;Excel&quot;,&quot;hide_column&quot;:&quot;Hide column&quot;,&quot;sort_clear&quot;:&quot;Clear sort&quot;,&quot;unpin&quot;:&quot;Unpin&quot;,&quot;avg&quot;:&quot;Avg&quot;,&quot;no&quot;:&quot;No&quot;}\" data-sort=\"[]\" data-filter=\"{}\" data-page=\"1\" data-page-size=\"10\" data-group-by=\"[&quot;dept&quot;]\" data-render=\"g3QAAAAIdwJpZG0AAAACZ2d3B2NvbHVtbnNsAAAABXQAAAADdwV0aXRsZW0AAAACSUR3A2tleXcCaWR3BXdpZHRoYTJ0AAAAA3cFdGl0bGVtAAAABE5hbWV3A2tleXcEbmFtZXcIZWRpdGFibGV3BHRydWV0AAAABXcEdHlwZXcGc2VsZWN0dwdvcHRpb25zbAAAAAJoAncDZW5nbQAAAAtFbmdpbmVlcmluZ2gCdwNvcHNtAAAACk9wZXJhdGlvbnNqdwV0aXRsZW0AAAAERGVwdHcDa2V5dwRkZXB0dwhlZGl0YWJsZXcEdHJ1ZXQAAAAGdwR0eXBldwZudW1iZXJ3BXRpdGxlbQAAAANBZ2V3A2tleXcDYWdldwhlZGl0YWJsZXcEdHJ1ZXcKYWdncmVnYXRlc2wAAAACdwNzdW13A2F2Z2p3BWFsaWdudwVyaWdodHQAAAAEdwR0eXBldwRib29sdwV0aXRsZW0AAAAGQWN0aXZldwNrZXl3BmFjdGl2ZXcIZWRpdGFibGV3BHRydWVqdwhwYWdlYWJsZXcFZmFsc2V3CXBhZ2Vfc2l6ZWEKdwpwYWdlX3NpemVzawAEChQyZHcGbGFiZWxzdAAAAAB3CWtleV9maWVsZHcCaWR3CXNlbGVjdGlvbncIY2hlY2tib3g.H1CZiZS58x4U19Blm-ejEt8FWn1KAm_uq6V2s-zabsA\"><div class=\"ah-dg-container\"><div class=\"ah-dg-header-wrap\"><div class=\"ah-dg-header\" role=\"rowgroup\"><div class=\"ah-dg-header-row\" role=\"row\" aria-rowindex=\"1\"><div class=\"ah-dg-header-cell ah-dg-header-cell-checkbox ah-dg-align-center\" role=\"columnheader\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-select-all ah-dg-header-checkbox\" type=\"checkbox\" tabindex=\"-1\" data-indeterminate=\"true\" aria-label=\"Select all rows\"></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"id\" aria-sort=\"none\" style=\"width:50px\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">ID</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"id\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"name\" aria-sort=\"none\" style=\"width:100px\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Name</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"name\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"dept\" aria-sort=\"none\" style=\"width:100px\" data-type=\"select\" data-editable=\"true\" data-min-width=\"40\" data-options=\"[[&quot;eng&quot;,&quot;Engineering&quot;],[&quot;ops&quot;,&quot;Operations&quot;]]\"><span class=\"ah-dg-header-cell-content\">Dept</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"dept\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-right ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"age\" aria-sort=\"none\" style=\"width:100px\" data-type=\"number\" data-editable=\"true\" data-min-width=\"40\" data-aggs=\"sum,avg\"><span class=\"ah-dg-header-cell-content\">Age</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"age\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div><div class=\"ah-dg-header-cell ah-dg-align-left ah-dg-header-cell-resizable\" role=\"columnheader\" data-field=\"active\" aria-sort=\"none\" style=\"width:100px\" data-type=\"bool\" data-editable=\"true\" data-min-width=\"40\"><span class=\"ah-dg-header-cell-content\">Active</span><span class=\"ah-dg-header-sort-icon\" aria-hidden=\"true\"></span><span class=\"ah-dg-column-menu-btn\" data-field=\"active\" aria-hidden=\"true\">⋮</span><div class=\"ah-dg-resize-handle\" aria-hidden=\"true\"></div></div></div></div></div><div class=\"ah-dg-body-wrap\"><div class=\"ah-dg-body\" id=\"gg-body\" role=\"rowgroup\"><div class=\"ah-dg-group-row\" role=\"row\" data-group-id=\"dept:eng\" data-level=\"0\" aria-level=\"1\" aria-expanded=\"true\"><span class=\"ah-dg-group-indent\" style=\"width:0px\"></span><span class=\"ah-dg-group-toggle ah-dg-group-toggle-open\" aria-hidden=\"true\">▶</span><span class=\"ah-dg-group-title\" role=\"gridcell\" aria-colspan=\"6\">Engineering (3)</span><span class=\"ah-dg-group-aggregates\"><span class=\"ah-dg-group-agg-item\">Age: sum=99, avg=33</span></span></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"gg-r-1\" role=\"row\" data-key=\"1\" aria-selected=\"false\" aria-rowindex=\"2\" data-i=\"0\"><div class=\"ah-dg-cell ah-dg-cell-checkbox ah-dg-align-center\" role=\"gridcell\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">1</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Ann</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">30</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-even\" id=\"gg-r-3\" role=\"row\" data-key=\"3\" aria-selected=\"false\" aria-rowindex=\"3\" data-i=\"2\"><div class=\"ah-dg-cell ah-dg-cell-checkbox ah-dg-align-center\" role=\"gridcell\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">3</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Cy</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">41</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"gg-r-5\" role=\"row\" data-key=\"5\" aria-selected=\"false\" aria-rowindex=\"4\" data-i=\"4\"><div class=\"ah-dg-cell ah-dg-cell-checkbox ah-dg-align-center\" role=\"gridcell\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">5</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Eve</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">28</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-group-row\" role=\"row\" data-group-id=\"dept:ops\" data-level=\"0\" aria-level=\"1\" aria-expanded=\"true\"><span class=\"ah-dg-group-indent\" style=\"width:0px\"></span><span class=\"ah-dg-group-toggle ah-dg-group-toggle-open\" aria-hidden=\"true\">▶</span><span class=\"ah-dg-group-title\" role=\"gridcell\" aria-colspan=\"6\">Operations (2)</span><span class=\"ah-dg-group-aggregates\"><span class=\"ah-dg-group-agg-item\">Age: sum=60, avg=30</span></span></div><div class=\"ah-dg-row ah-dg-row-selected ah-dg-row-odd\" id=\"gg-r-2\" role=\"row\" data-key=\"2\" aria-selected=\"true\" aria-rowindex=\"5\" data-i=\"1\"><div class=\"ah-dg-cell ah-dg-cell-checkbox ah-dg-align-center\" role=\"gridcell\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" checked aria-label=\"Select row\"></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">2</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">bob</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">25</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-row ah-dg-row-even\" id=\"gg-r-4\" role=\"row\" data-key=\"4\" aria-selected=\"false\" aria-rowindex=\"6\" data-i=\"3\"><div class=\"ah-dg-cell ah-dg-cell-checkbox ah-dg-align-center\" role=\"gridcell\" data-field=\"__checkbox\" style=\"width:40px\"><input class=\"ah-dg-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></div><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">4</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Dee</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">35</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-empty-message\" hidden>No data</div></div></div></div><div class=\"ah-dg-column-menu\" id=\"gg-menu\" role=\"menu\"></div><div class=\"ah-dg-resize-line\" aria-hidden=\"true\"></div><div class=\"ah-dg-loading-overlay\" style=\"display:none;\"><div class=\"ah-dg-loading-message\">Loading...</div></div><input type=\"hidden\" name=\"ids\" value=\"2\" data-ah-input></div>",
 "page2": [
  {
   "id": "rg-body",
   "op": "html",
   "html": "<div class=\"ah-dg-row ah-dg-row-even\" id=\"rg-r-3\" role=\"row\" data-key=\"3\" aria-selected=\"false\" aria-rowindex=\"4\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">3</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Cy</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"eng\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Engineering</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">41</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"true\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Yes</span></div></div><div class=\"ah-dg-row ah-dg-row-odd\" id=\"rg-r-4\" role=\"row\" data-key=\"4\" aria-selected=\"false\" aria-rowindex=\"5\"><div class=\"ah-dg-cell ah-dg-align-left\" role=\"gridcell\" data-field=\"id\" style=\"width:50px\"><span class=\"ah-dg-cell-content\">4</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"name\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Dee</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"dept\" data-v=\"ops\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">Operations</span></div><div class=\"ah-dg-cell ah-dg-align-right ah-dg-cell-editable\" role=\"gridcell\" data-field=\"age\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">35</span></div><div class=\"ah-dg-cell ah-dg-align-left ah-dg-cell-editable\" role=\"gridcell\" data-field=\"active\" data-v=\"false\" aria-readonly=\"false\" style=\"width:100px\"><span class=\"ah-dg-cell-content\">No</span></div></div><div class=\"ah-dg-empty-message\" hidden>No data</div>",
   "swap": "morph_inner"
  },
  {
   "id": "rg-pager",
   "op": "html",
   "html": "<div class=\"ah-dg-pager\" role=\"navigation\" aria-label=\"Pages\"><div class=\"ah-dg-pager-info\" aria-live=\"polite\">Total 5</div><div class=\"ah-dg-pager-controls\"><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"First page\">|&lt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\" aria-label=\"Previous page\">&lt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"1\">1</button><button type=\"button\" class=\"ah-dg-pager-button ah-dg-pager-button-active\" data-page=\"2\" aria-current=\"page\">2</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"3\">3</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"3\" aria-label=\"Next page\">&gt;</button><button type=\"button\" class=\"ah-dg-pager-button\" data-page=\"3\" aria-label=\"Last page\">&gt;|</button></div><div class=\"ah-dg-pager-size\"><select class=\"ah-dg-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2 / page</option><option value=\"4\">4 / page</option></select></div></div>",
   "swap": "morph_inner"
  },
  {
   "args": [
    5,
    2
   ],
   "id": "rg",
   "op": "call",
   "method": "rowsLoaded"
  }
 ]
};

  async function mount(fx, name, noServer) {
    fx.innerHTML = SERVER[name];
    if (noServer) {
      fx.querySelectorAll("[data-ah-on]").forEach(function (n) { n.removeAttribute("data-ah-on"); });
    }
    await T.ready(fx);
    return fx.firstChild;
  }
  function wait(ms) { return new Promise(function (ok) { setTimeout(ok, ms); }); }
  function q(el, sel) { return el.querySelector(sel); }
  function qa(el, sel) { return Array.from(el.querySelectorAll(sel)); }
  function child(el, sel) {
    return Array.prototype.filter.call(el.children, function (c) { return c.matches(sel); })[0];
  }
  function kidsOf(el, sel) {
    return Array.prototype.filter.call(el.children, function (c) { return c.matches(sel); });
  }
  function click(el, init) { return T.fire(el, "click", init); }
  function set(el, v, type) { el.value = v; T.fire(el, type || "input"); }
  function display(el) { return getComputedStyle(el).display; }
  function shown(el) {
    return qa(el, ".ah-dg-body > .ah-dg-row:not(.ah-dg-row-off)").map(function (r) {
      return child(r, "[data-field=name]").textContent;
    });
  }
  function head(el, f) { return q(el, ".ah-dg-header-cell[data-field=" + f + "]"); }
  function cell(el, key, f) { return q(el, ".ah-dg-row[data-key='" + key + "'] > [data-field=" + f + "]"); }
  function key(target, k, opts) { return T.key(target, k, opts); }
  // Captures the file an export downloads.
  function captureDownload() {
    var got = { blob: null, name: null };
    var orig = URL.createObjectURL;
    var click0 = HTMLAnchorElement.prototype.click;
    URL.createObjectURL = function (b) { got.blob = b; return "blob:x"; };
    HTMLAnchorElement.prototype.click = function () { got.name = this.download; };
    got.restore = function () {
      URL.createObjectURL = orig;
      HTMLAnchorElement.prototype.click = click0;
    };
    return got;
  }

  T.test("datagrid: sort cycles asc / desc / none and pages", async function (fx) {
    var el = await mount(fx, "local");
    var sorts = [];
    el.addEventListener("ah:sort", function () { sorts.push(el.getAttribute("data-field") + ":" + el.getAttribute("data-value")); });
    T.eq(shown(el), ["Ann", "bob", "Cy"]);
    T.eq(q(el, ".ah-dg-pager-info").textContent, "Total 5");
    click(head(el, "name"));
    T.eq(head(el, "name").getAttribute("aria-sort"), "ascending");
    click(head(el, "name"));
    T.eq(shown(el), ["Eve", "Dee", "Cy"], "desc, case-insensitive");
    T.eq(JSON.parse(el.getAttribute("data-sort")), [["name", "desc"]]);
    click(head(el, "name"));
    T.eq(head(el, "name").getAttribute("aria-sort"), "none");
    T.eq(shown(el), ["Ann", "bob", "Cy"], "source order again");
    T.eq(sorts, ["name:asc", "name:desc", "name:"]);
    T.eq(el.hasAttribute("data-field"), false, "details removed after the event");
    // numeric sort, then shift adds a second key
    click(head(el, "age"));
    T.eq(shown(el), ["bob", "Eve", "Ann"]);
    click(q(el, ".ah-dg-pager-button[data-page='2']"));
    T.eq(shown(el), ["Dee", "Cy"]);
    T.eq(el.getAttribute("data-page"), "2");
    T.eq(q(el, ".ah-dg-pager-button-active").textContent, "2");
    set(q(el, ".ah-dg-pager-size-select"), "10", "change");
    T.eq(shown(el).length, 5);
    T.eq(el.getAttribute("data-page-size"), "10");
  });

  T.test("datagrid: filter row, search, status bar, empty message", async function (fx) {
    var el = await mount(fx, "local");
    var sum = function () { return q(el, ".ah-dg-statusbar-item[data-agg=sum] .ah-dg-statusbar-value").textContent; };
    T.eq(sum(), "159");
    set(q(el, ".ah-dg-filter-input[data-field=dept]"), "eng");
    await wait(260);
    T.eq(shown(el), ["Ann", "Cy", "Eve"], "raw or shown text matches");
    T.eq(sum(), "99");
    T.eq(q(el, ".ah-dg-statusbar-item[data-agg=avg] .ah-dg-statusbar-value").textContent, "33.00");
    T.ok(q(el, ".ah-dg-filter-cell[data-field=dept]").classList.contains("ah-dg-filter-cell-active"));
    T.eq(JSON.parse(el.getAttribute("data-filter")), { dept: "eng" });
    set(q(el, ".ah-dg-search-input"), "zzz");
    await wait(260);
    T.eq(shown(el), []);
    T.ok(q(el, ".ah-dg-body").classList.contains("ah-dg-body-empty"));
    T.eq(q(el, ".ah-dg-empty-message").hidden, false);
    T.eq(sum(), "");
    AH.invoke(el, "search", "");
    AH.invoke(el, "filter", "dept", "");
    T.eq(shown(el).length, 3);
    T.eq(q(el, ".ah-dg-empty-message").hidden, true);
  });

  T.test("datagrid: multi selection with ctrl and shift, change once per change", async function (fx) {
    var el = await mount(fx, "local");
    var changes = 0;
    el.addEventListener("change", function (e) { if (e.target === el) { changes++; } });
    click(cell(el, 1, "name"));
    T.eq(el.getAttribute("data-ah-value"), "1");
    click(cell(el, 3, "name"), { shiftKey: true });
    T.eq(el.getAttribute("data-ah-value"), "1,2,3");
    click(cell(el, 2, "name"), { ctrlKey: true });
    T.eq(el.getAttribute("data-ah-value"), "1,3");
    T.eq(q(el, ".ah-dg-row[data-key='2']").getAttribute("aria-selected"), "false");
    T.ok(q(el, ".ah-dg-row[data-key='3']").classList.contains("ah-dg-row-selected"));
    T.eq(changes, 3);
    AH.invoke(el, "setValue", ["2"]);
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes, 3, "setValue is silent");
  });

  T.test("datagrid: keyboard moves the active cell, pages and selects", async function (fx) {
    var el = await mount(fx, "local");
    T.eq(head(el, "id").getAttribute("tabindex"), "0", "first header cell is the tab stop");
    head(el, "id").focus();
    key(head(el, "id"), "ArrowDown");
    T.eq(document.activeElement, cell(el, 1, "id"));
    key(document.activeElement, "ArrowRight");
    T.eq(document.activeElement, cell(el, 1, "name"));
    T.ok(document.activeElement.classList.contains("ah-dg-cell-focused"));
    T.eq(el.querySelectorAll("[tabindex='0']").length, 1, "one tab stop");
    key(document.activeElement, "End");
    T.eq(document.activeElement, cell(el, 1, "active"));
    key(document.activeElement, "End", { ctrlKey: true });
    T.eq(document.activeElement, cell(el, 3, "active"));
    key(document.activeElement, " ");
    T.eq(el.getAttribute("data-ah-value"), "3");
    key(document.activeElement, "PageDown");
    T.eq(el.getAttribute("data-page"), "2");
    T.eq(document.activeElement, cell(el, 4, "active"));
    key(document.activeElement, "a", { ctrlKey: true });
    T.eq(el.getAttribute("data-ah-value"), "3,4,5");
    key(document.activeElement, "Home", { ctrlKey: true });
    T.eq(document.activeElement, head(el, "active"));
    key(document.activeElement, "Enter");
    T.eq(head(el, "active").getAttribute("aria-sort"), "ascending", "Enter sorts on a header");
  });

  T.test("datagrid: inline editing fires ah:edit with the details", async function (fx) {
    var el = await mount(fx, "local");
    var edits = [], details = [];
    el.addEventListener("ah:edit", function (e) {
      edits.push([el.getAttribute("data-key"), el.getAttribute("data-field"), el.getAttribute("data-value"), el.getAttribute("data-old")]);
      details.push(e.detail);
    });
    T.fire(cell(el, 2, "name"), "dblclick");
    var ed = q(cell(el, 2, "name"), "input.ah-dg-editor");
    T.ok(ed && document.activeElement === ed, "editor focused");
    ed.value = "Bobby";
    key(ed, "Enter");
    T.eq(cell(el, 2, "name").textContent, "Bobby");
    T.eq(edits, [["2", "name", "Bobby", "bob"]]);
    T.eq(details, [{ key: "2", field: "name", value: "Bobby", old: "bob" }], "the detail too");
    T.eq(document.activeElement, cell(el, 2, "name"), "focus back on the cell");
    // Escape cancels
    key(cell(el, 2, "name"), "F2");
    q(cell(el, 2, "name"), "input").value = "X";
    key(q(cell(el, 2, "name"), "input"), "Escape");
    T.eq(cell(el, 2, "name").textContent, "Bobby");
    T.eq(edits.length, 1);
    // select: options with labels, the cell shows the label, data-v the value
    T.fire(cell(el, 1, "dept"), "dblclick");
    var sel = q(cell(el, 1, "dept"), "select");
    T.eq(Array.from(sel.children).map(function (o) { return o.text; }), ["Engineering", "Operations"]);
    T.eq(sel.value, "eng");
    sel.value = "ops";
    key(sel, "Tab");
    T.eq(cell(el, 1, "dept").textContent, "Operations");
    T.eq(cell(el, 1, "dept").getAttribute("data-v"), "ops");
    T.ok(qa(cell(el, 1, "age"), "input[type=number]").length === 1, "Tab edits the next editable cell");
    key(q(cell(el, 1, "age"), "input"), "Escape");
    // bool toggles at once
    key(cell(el, 1, "active"), "F2");
    T.eq(cell(el, 1, "active").textContent, "No");
    T.eq(edits[edits.length - 1], ["1", "active", "false", "true"]);
    // blur commits
    T.fire(cell(el, 3, "name"), "dblclick");
    var ed3 = q(cell(el, 3, "name"), "input.ah-dg-editor");
    ed3.value = "Cyrus";
    ed3.blur();
    T.eq(cell(el, 3, "name").textContent, "Cyrus", "blur saves");
    T.eq(q(el, ".ah-dg-editor"), null);
    T.eq(edits[edits.length - 1], ["3", "name", "Cyrus", "Cy"]);
  });

  T.test("datagrid: column menu sorts, hides, shows and pins", async function (fx) {
    var el = await mount(fx, "local");
    click(q(el, ".ah-dg-column-menu-btn[data-field=name]"));
    var menu = child(el, ".ah-dg-column-menu");
    T.ok(menu.classList.contains("ah-dg-column-menu-open"));
    T.eq(document.activeElement, child(menu, ".ah-dg-column-menu-item"));
    T.eq(kidsOf(menu, "[data-action=toggle-column]").length, 5);
    click(child(menu, "[data-action=sort-desc]"));
    T.eq(shown(el)[0], "Eve");
    T.eq(menu.classList.contains("ah-dg-column-menu-open"), false);
    T.eq(document.activeElement, head(el, "name"), "focus back on the header");
    key(head(el, "name"), "ArrowDown", { altKey: true });
    T.ok(menu.classList.contains("ah-dg-column-menu-open"), "Alt+Down opens it");
    key(document.activeElement, "ArrowDown");
    key(document.activeElement, "Escape");
    T.eq(menu.classList.contains("ah-dg-column-menu-open"), false);
    click(q(el, ".ah-dg-column-menu-btn[data-field=age]"));
    click(child(menu, "[data-action=hide]"));
    T.ok(head(el, "age").classList.contains("ah-dg-col-hidden"));
    T.ok(cell(el, 1, "age").classList.contains("ah-dg-col-hidden"));
    T.eq(el.getAttribute("aria-colcount"), "4");
    click(q(el, ".ah-dg-column-menu-btn[data-field=name]"));
    var toggle = child(menu, "[data-action=toggle-column][data-field=age]");
    T.eq(toggle.getAttribute("aria-checked"), "false");
    click(toggle);
    T.eq(head(el, "age").classList.contains("ah-dg-col-hidden"), false);
    T.ok(menu.classList.contains("ah-dg-column-menu-open"), "stays open");
    click(child(menu, "[data-action=pin]"));
    T.eq(cell(el, 1, "name").style.position, "sticky");
    T.eq(cell(el, 1, "name").style.left, "0px");
    AH.invoke(el, "pinColumn", "id", true);
    T.eq(cell(el, 1, "name").style.left, "50px", "offset after the pinned id column");
    AH.invoke(el, "setColumnWidth", "id", 80);
    T.eq(head(el, "id").style.width, "80px");
    T.eq(cell(el, 3, "name").style.left, "80px");
    // a mousedown outside closes it
    click(q(el, ".ah-dg-column-menu-btn[data-field=name]"));
    T.ok(menu.classList.contains("ah-dg-column-menu-open"));
    T.fire(document.body, "mousedown");
    T.eq(menu.classList.contains("ah-dg-column-menu-open"), false);
  });

  T.test("datagrid: groups with aggregates collapse; check boxes select", async function (fx) {
    var el = await mount(fx, "grouped");
    var groups = qa(el, ".ah-dg-group-row");
    T.eq(groups.map(function (g) { return g.getAttribute("data-group-id"); }), ["dept:eng", "dept:ops"]);
    T.eq(q(groups[0], ".ah-dg-group-title").textContent, "Engineering (3)");
    T.eq(q(groups[0], ".ah-dg-group-agg-item").textContent, "Age: sum=99, avg=33");
    T.eq(q(groups[groups.length - 1], ".ah-dg-group-agg-item").textContent, "Age: sum=60, avg=30");
    var toggles = [];
    el.addEventListener("ah:group-toggle", function (e) { toggles.push(e.detail.value + ":" + e.detail.expanded); });
    click(groups[0]);
    T.eq(shown(el), ["bob", "Dee"]);
    T.eq(q(el, ".ah-dg-group-row").getAttribute("aria-expanded"), "false");
    T.eq(toggles, ["dept:eng:false"]);
    T.eq(child(el, "input[name=ids]").value, "2");
    var all = q(el, ".ah-dg-select-all");
    all.checked = true;
    T.fire(all, "change");
    T.eq(el.getAttribute("data-ah-value"), "2,4");
    q(el, ".ah-dg-row[data-key='4'] .ah-dg-row-checkbox").click();
    T.eq(el.getAttribute("data-ah-value"), "2", "a click unchecks");
    T.eq(q(el, ".ah-dg-select-all").indeterminate, true);
    AH.invoke(el, "groupBy", []);
    T.eq(qa(el, ".ah-dg-group-row").length, 0);
    T.eq(shown(el).length, 5);
  });

  T.test("datagrid: remote mode asks the server and adopts its rows", async function (fx) {
    var el = await mount(fx, "remote", true);
    var qe = child(el, ".ah-dg-query");
    var asked = [];
    qe.addEventListener("ah:query", function () {
      asked.push({ page: qe.getAttribute("data-page"), sort: qe.getAttribute("data-sort"),
                   render: qe.getAttribute("data-render") === el.getAttribute("data-render"),
                   exp: qe.getAttribute("data-export") });
    });
    T.eq(asked.length, 0, "rows came with the page");
    click(q(el, ".ah-dg-row[data-key='1']"));
    click(q(el, ".ah-dg-pager-button[data-page='2']"));
    T.eq(asked.length, 1);
    T.eq(asked[0].page, "2");
    T.ok(asked[0].render, "the render token goes along");
    T.eq(display(child(el, ".ah-dg-loading-overlay")), "flex");
    T.eq(el.getAttribute("aria-busy"), "true");
    AH.apply(SERVER.page2);
    T.eq(display(child(el, ".ah-dg-loading-overlay")), "none");
    T.eq(el.getAttribute("aria-busy"), "false");
    T.eq(shown(el), ["Cy", "Dee"]);
    T.eq(q(el, ".ah-dg-pager-button-active").textContent, "2");
    click(q(el, ".ah-dg-row[data-key='4']"));
    T.eq(el.getAttribute("data-ah-value"), "4");
    T.ok(q(el, ".ah-dg-row[data-key='4']").classList.contains("ah-dg-row-selected"));
    click(head(el, "age"));
    T.eq(JSON.parse(asked[1].sort), [["age", "asc"]]);
    AH.invoke(el, "exportData", "csv");
    T.eq(asked[2].exp, "csv");
    T.eq(qe.hasAttribute("data-export"), false);
    // a failed request ends the loading state (ah:error on the query element)
    T.eq(display(child(el, ".ah-dg-loading-overlay")), "flex");
    qe.dispatchEvent(new CustomEvent("ah:error", { bubbles: true, detail: { status: 500 } }));
    T.eq(display(child(el, ".ah-dg-loading-overlay")), "none");
    // a search goes to the server too, from page 1
    AH.invoke(el, "search", "Dee");
    T.eq(asked[asked.length - 1].page, "1");
    T.eq(qe.getAttribute("data-search"), "Dee");
  });

  T.test("datagrid: CSV export of the filtered, sorted rows", async function (fx) {
    var el = await mount(fx, "local");
    var got = captureDownload();
    try {
      click(head(el, "age"));
      click(head(el, "age"));
      click(q(el, ".ah-dg-toolbar-btn[data-export=csv]"));
    } finally { got.restore(); }
    T.eq(got.name, "people.csv");
    var bytes = new Uint8Array(await got.blob.arrayBuffer());
    T.eq([bytes[0], bytes[1], bytes[2]], [0xEF, 0xBB, 0xBF], "UTF-8 BOM for Excel");
    var txt = await got.blob.text();
    T.eq(txt.split("\n")[0], "ID,Name,Dept,Age,Active");
    T.eq(txt.split("\n")[1], "3,Cy,Engineering,41,Yes");
    T.eq(txt.split("\n").length, 6, "all pages");
  });

  T.test("datagrid: exportData with the server's headers and rows (CSV)", async function (fx) {
    var el = await mount(fx, "local");
    var got = captureDownload();
    try {
      await AH.invoke(el, "exportData", "csv", ["A", "B"], [["1", "x,y"], ["2", 'say "hi"']]);
    } finally { got.restore(); }
    T.eq(got.name, "people.csv");
    var txt = await got.blob.text();
    T.eq(txt.replace(/^﻿/, "").split("\n"), ["A,B", '1,"x,y"', '2,"say ""hi"""']);
  });

  T.test("datagrid: Excel export loads xlsx on demand", async function (fx) {
    var el = await mount(fx, "local");
    var got = captureDownload();
    try { await AH.invoke(el, "exportData", "xlsx"); } finally { got.restore(); }
    T.eq(got.name, "people.xlsx");
    var bytes = new Uint8Array(await got.blob.arrayBuffer());
    T.eq([bytes[0], bytes[1]], [0x50, 0x4B], "a zip file");
    // SheetJS stores the parts uncompressed: the texts and six rows are readable
    var txt = new TextDecoder("latin1").decode(bytes);
    T.ok(txt.indexOf("Name") >= 0 && txt.indexOf("Engineering") >= 0, "the texts");
    T.ok(txt.indexOf('<row r="6"') >= 0 && txt.indexOf('<row r="7"') < 0, "header and five rows");
  });

  T.test("datagrid: PDF export loads jspdf and autotable on demand", async function (fx) {
    var el = await mount(fx, "local");
    var got = captureDownload();
    try { await AH.invoke(el, "exportData", "pdf"); } finally { got.restore(); }
    T.eq(got.name, "people.pdf");
    var txt = await got.blob.text();
    T.eq(txt.slice(0, 5), "%PDF-");
    T.ok(/\/Count 1\b/.test(txt), "one page: " + (txt.match(/\/Count \d+/) || ["?"])[0]);
  });

  // ---- pager links (href) ----------------------------------------------

  // Link clicks after the grid handled them (a listener on the root added
  // after the controller's): whether the grid took over the link; the
  // test page itself never navigates.
  function watchClicks(el) {
    var seen = [];
    el.addEventListener("click", function (e) {
      if (e.target.closest && e.target.closest("a")) {
        seen.push(e.defaultPrevented);
        e.preventDefault();
      }
    });
    return seen;
  }

  T.test("datagrid: a pager link re-pages in place and pushes its URL (local)", async function (fx) {
    var original = location.href;
    fx.innerHTML = SERVER.local;
    fx.firstChild.setAttribute("data-ah-href", "/people?p={page}&n={size}&s={sort}");
    await T.ready(fx);
    var el = fx.firstChild;
    var seen = watchClicks(el);
    try {
      var link = q(el, "a.ah-dg-pager-button[data-page='2']");
      T.ok(link, "the pager renders links");
      T.eq(link.getAttribute("href"), "/people?p=2&n=3&s=");
      T.eq(q(el, ".ah-dg-pager-button[aria-label]").tagName, "BUTTON", "first page: first is a disabled button");
      click(head(el, "age"));
      link = q(el, "a.ah-dg-pager-button[data-page='2']");
      T.eq(link.getAttribute("href"), "/people?p=2&n=3&s=age:asc", "the sort goes into the links");
      click(link, { button: 0 });
      T.eq(seen, [true], "the grid took the click");
      T.eq(shown(el), ["Dee", "Cy"]);
      T.eq(el.getAttribute("data-page"), "2");
      T.ok(location.href.endsWith("/people?p=2&n=3&s=age:asc"), location.href);
      T.eq(history.state && history.state.ah, true);
      // a ctrl-click is the browser's (a new tab): no re-page, no push
      click(q(el, "a.ah-dg-pager-button[data-page='1']"), { button: 0, ctrlKey: true });
      T.eq(seen, [true, false]);
      T.eq(el.getAttribute("data-page"), "2");
      T.ok(location.href.endsWith("/people?p=2&n=3&s=age:asc"));
      // keyboard paging does not push
      history.replaceState(null, "", original);
      AH.invoke(el, "goToPage", 1);
      T.eq(location.href, original);
    } finally {
      history.replaceState(null, "", original);
    }
  });

  T.test("datagrid: a pager link queries the server and pushes its URL (remote)", async function (fx) {
    var original = location.href;
    var el = await mount(fx, "rlinked", true);
    var qe = child(el, ".ah-dg-query");
    var asked = [];
    qe.addEventListener("ah:query", function () { asked.push(qe.getAttribute("data-page")); });
    await new Promise(function (ok) { setTimeout(ok, 20); });
    T.eq(asked.length, 0, "no query on mount: the server rendered the first page");
    T.eq(el.getAttribute("aria-busy"), null);
    var seen = watchClicks(el);
    try {
      var link = q(el, "a.ah-dg-pager-button[data-page='3']");
      T.eq(link.getAttribute("href"), "/grid?page=3&size=2&sort=");
      click(link, { button: 0 });
      T.eq(seen, [true]);
      T.eq(asked, ["3"]);
      T.ok(location.href.endsWith("/grid?page=3&size=2&sort="), location.href);
      click(q(el, "a.ah-dg-pager-button[data-page='2']"), { button: 0, metaKey: true });
      T.eq(seen, [true, false], "a meta-click is not taken");
      T.eq(asked, ["3"]);
    } finally {
      history.replaceState(null, "", original);
    }
  });

  T.test("datagrid: an empty remote grid shows its empty state, asks nothing", async function (fx) {
    fx.innerHTML = SERVER.rlinked;
    fx.querySelectorAll("[data-ah-on]").forEach(function (n) { n.removeAttribute("data-ah-on"); });
    var body = q(fx, ".ah-dg-body");
    fx.firstChild.removeAttribute("data-ah-loaded");
    var asked = 0;
    child(fx.firstChild, ".ah-dg-query").addEventListener("ah:query", function () { asked++; });
    await T.ready(fx);
    await new Promise(function (ok) { setTimeout(ok, 20); });
    T.eq(asked, 0, "not even without data-ah-loaded");
    T.ok(body.querySelector(".ah-dg-row"), "the server rows stay");
    AH.invoke(fx.firstChild, "refresh");
    T.eq(asked, 1, "refresh loads on demand");
  });

  T.test("datagrid: removed and inserted again it works (cleanup)", async function (fx) {
    var el = await mount(fx, "local");
    click(q(el, ".ah-dg-column-menu-btn[data-field=name]"));
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
    var sorts = 0;
    el.addEventListener("ah:sort", function () { sorts++; });
    click(head(el, "name"));
    T.eq(sorts, 1, "one controller, one event");
    T.eq(shown(el), ["Ann", "bob", "Cy"]);
    fx.innerHTML = "";
    await wait(0);
    var again = await mount(fx, "local");
    click(q(again, ".ah-dg-pager-button[data-page='2']"));
    T.eq(shown(again), ["Dee", "Eve"]);
  });
})(window.AHTest, window.AH);
