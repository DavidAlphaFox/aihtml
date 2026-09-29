/* datagrid: the datagrid behaviour on markup the server renders. SERVER
 * holds renders of aihtml_datagrid:datagrid/4 (ids "g", "gg", "rg") and
 * the operations datagrid_rows/4 sends for page 2 of the remote grid;
 * regenerate them from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
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

  function mount(fx, name, noServer) {
    fx.innerHTML = SERVER[name];
    if (noServer) { $(fx).find("[data-ah-on]").removeAttr("data-ah-on"); }
    AH.mount(fx);
    return fx.firstChild;
  }
  function wait(ms) { return new Promise(function (ok) { setTimeout(ok, ms); }); }
  function shown(el) {
    return $(el).find(".ah-dg-body > .ah-dg-row").not(".ah-dg-row-off").map(function () {
      return $(this).children("[data-field=name]").text();
    }).get();
  }
  function head(el, f) { return $(el).find(".ah-dg-header-cell[data-field=" + f + "]")[0]; }
  function cell(el, key, f) { return $(el).find(".ah-dg-row[data-key=" + key + "] > [data-field=" + f + "]")[0]; }
  function key(target, k, opts) {
    var e = $.Event("keydown", $.extend({ key: k }, opts || {}));
    $(target).trigger(e);
    return e;
  }

  T.test("datagrid: sort cycles asc / desc / none and pages", function (fx) {
    var el = mount(fx, "local");
    var sorts = [];
    $(el).on("ah:sort", function () { sorts.push(el.getAttribute("data-field") + ":" + el.getAttribute("data-value")); });
    T.eq(shown(el), ["Ann", "bob", "Cy"]);
    T.eq($(el).find(".ah-dg-pager-info").text(), "Total 5");
    $(head(el, "name")).trigger("click");
    T.eq(head(el, "name").getAttribute("aria-sort"), "ascending");
    $(head(el, "name")).trigger("click");
    T.eq(shown(el), ["Eve", "Dee", "Cy"], "desc, case-insensitive");
    T.eq(JSON.parse(el.getAttribute("data-sort")), [["name", "desc"]]);
    $(head(el, "name")).trigger("click");
    T.eq(head(el, "name").getAttribute("aria-sort"), "none");
    T.eq(shown(el), ["Ann", "bob", "Cy"], "source order again");
    T.eq(sorts, ["name:asc", "name:desc", "name:"]);
    T.eq(el.hasAttribute("data-field"), false, "details removed after the event");
    // numeric sort, then shift adds a second key
    $(head(el, "age")).trigger("click");
    T.eq(shown(el), ["bob", "Eve", "Ann"]);
    $(el).find(".ah-dg-pager-button[data-page=2]").trigger("click");
    T.eq(shown(el), ["Dee", "Cy"]);
    T.eq(el.getAttribute("data-page"), "2");
    T.eq($(el).find(".ah-dg-pager-button-active").text(), "2");
    $(el).find(".ah-dg-pager-size-select").val("10").trigger("change");
    T.eq(shown(el).length, 5);
    T.eq(el.getAttribute("data-page-size"), "10");
  });

  T.test("datagrid: filter row, search, status bar, empty message", async function (fx) {
    var el = mount(fx, "local");
    var sum = function () { return $(el).find(".ah-dg-statusbar-item[data-agg=sum] .ah-dg-statusbar-value").text(); };
    T.eq(sum(), "159");
    $(el).find(".ah-dg-filter-input[data-field=dept]").val("eng").trigger("input");
    await wait(260);
    T.eq(shown(el), ["Ann", "Cy", "Eve"], "raw or shown text matches");
    T.eq(sum(), "99");
    T.eq($(el).find(".ah-dg-statusbar-item[data-agg=avg] .ah-dg-statusbar-value").text(), "33.00");
    T.ok($(el).find(".ah-dg-filter-cell[data-field=dept]").hasClass("ah-dg-filter-cell-active"));
    T.eq(JSON.parse(el.getAttribute("data-filter")), { dept: "eng" });
    $(el).find(".ah-dg-search-input").val("zzz").trigger("input");
    await wait(260);
    T.eq(shown(el), []);
    T.ok($(el).find(".ah-dg-body").hasClass("ah-dg-body-empty"));
    T.eq($(el).find(".ah-dg-empty-message")[0].hidden, false);
    T.eq(sum(), "");
    AH.invoke(el, "search", "");
    AH.invoke(el, "filter", "dept", "");
    T.eq(shown(el).length, 3);
    T.eq($(el).find(".ah-dg-empty-message")[0].hidden, true);
  });

  T.test("datagrid: multi selection with ctrl and shift, change once per change", function (fx) {
    var el = mount(fx, "local");
    var changes = 0;
    $(el).on("change", function (e) { if (e.target === el) { changes++; } });
    $(cell(el, 1, "name")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "1");
    $(cell(el, 3, "name")).trigger($.Event("click", { shiftKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "1,2,3");
    $(cell(el, 2, "name")).trigger($.Event("click", { ctrlKey: true }));
    T.eq(el.getAttribute("data-ah-value"), "1,3");
    T.eq($(el).find(".ah-dg-row[data-key=2]").attr("aria-selected"), "false");
    T.ok($(el).find(".ah-dg-row[data-key=3]").hasClass("ah-dg-row-selected"));
    T.eq(changes, 3);
    AH.invoke(el, "setValue", ["2"]);
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes, 3, "setValue is silent");
  });

  T.test("datagrid: keyboard moves the active cell, pages and selects", function (fx) {
    var el = mount(fx, "local");
    T.eq(head(el, "id").getAttribute("tabindex"), "0", "first header cell is the tab stop");
    head(el, "id").focus();
    key(head(el, "id"), "ArrowDown");
    T.eq(document.activeElement, cell(el, 1, "id"));
    key(document.activeElement, "ArrowRight");
    T.eq(document.activeElement, cell(el, 1, "name"));
    T.ok($(document.activeElement).hasClass("ah-dg-cell-focused"));
    T.eq($(el).find("[tabindex='0']").length, 1, "one tab stop");
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

  T.test("datagrid: inline editing fires ah:edit with the details", function (fx) {
    var el = mount(fx, "local");
    var edits = [];
    $(el).on("ah:edit", function (e, d) {
      edits.push([el.getAttribute("data-key"), el.getAttribute("data-field"), el.getAttribute("data-value"), el.getAttribute("data-old")]);
    });
    $(cell(el, 2, "name")).trigger("dblclick");
    var ed = $(cell(el, 2, "name")).find("input.ah-dg-editor")[0];
    T.ok(ed && document.activeElement === ed, "editor focused");
    ed.value = "Bobby";
    key(ed, "Enter");
    T.eq($(cell(el, 2, "name")).text(), "Bobby");
    T.eq(edits, [["2", "name", "Bobby", "bob"]]);
    T.eq(document.activeElement, cell(el, 2, "name"), "focus back on the cell");
    // Escape cancels
    key(cell(el, 2, "name"), "F2");
    $(cell(el, 2, "name")).find("input")[0].value = "X";
    key($(cell(el, 2, "name")).find("input")[0], "Escape");
    T.eq($(cell(el, 2, "name")).text(), "Bobby");
    T.eq(edits.length, 1);
    // select: options with labels, the cell shows the label, data-v the value
    $(cell(el, 1, "dept")).trigger("dblclick");
    var sel = $(cell(el, 1, "dept")).find("select")[0];
    T.eq($(sel).children().map(function () { return this.text; }).get(), ["Engineering", "Operations"]);
    T.eq(sel.value, "eng");
    sel.value = "ops";
    key(sel, "Tab");
    T.eq($(cell(el, 1, "dept")).text(), "Operations");
    T.eq(cell(el, 1, "dept").getAttribute("data-v"), "ops");
    T.ok($(cell(el, 1, "age")).find("input[type=number]").length === 1, "Tab edits the next editable cell");
    key($(cell(el, 1, "age")).find("input")[0], "Escape");
    // bool toggles at once
    key(cell(el, 1, "active"), "F2");
    T.eq($(cell(el, 1, "active")).text(), "No");
    T.eq(edits[edits.length - 1], ["1", "active", "false", "true"]);
  });

  T.test("datagrid: column menu sorts, hides, shows and pins", function (fx) {
    var el = mount(fx, "local");
    $(el).find(".ah-dg-column-menu-btn[data-field=name]").trigger("click");
    var menu = $(el).children(".ah-dg-column-menu")[0];
    T.ok($(menu).hasClass("ah-dg-column-menu-open"));
    T.eq(document.activeElement, $(menu).children(".ah-dg-column-menu-item")[0]);
    T.eq($(menu).children("[data-action=toggle-column]").length, 5);
    $(menu).children("[data-action=sort-desc]").trigger("click");
    T.eq(shown(el)[0], "Eve");
    T.eq($(menu).hasClass("ah-dg-column-menu-open"), false);
    T.eq(document.activeElement, head(el, "name"), "focus back on the header");
    key(head(el, "name"), "ArrowDown", { altKey: true });
    T.ok($(menu).hasClass("ah-dg-column-menu-open"), "Alt+Down opens it");
    key(document.activeElement, "ArrowDown");
    key(document.activeElement, "Escape");
    T.eq($(menu).hasClass("ah-dg-column-menu-open"), false);
    $(el).find(".ah-dg-column-menu-btn[data-field=age]").trigger("click");
    $(menu).children("[data-action=hide]").trigger("click");
    T.ok($(head(el, "age")).hasClass("ah-dg-col-hidden"));
    T.ok($(cell(el, 1, "age")).hasClass("ah-dg-col-hidden"));
    T.eq(el.getAttribute("aria-colcount"), "4");
    $(el).find(".ah-dg-column-menu-btn[data-field=name]").trigger("click");
    var toggle = $(menu).children("[data-action=toggle-column][data-field=age]");
    T.eq(toggle.attr("aria-checked"), "false");
    toggle.trigger("click");
    T.eq($(head(el, "age")).hasClass("ah-dg-col-hidden"), false);
    T.ok($(menu).hasClass("ah-dg-column-menu-open"), "stays open");
    $(menu).children("[data-action=pin]").trigger("click");
    T.eq(cell(el, 1, "name").style.position, "sticky");
    T.eq(cell(el, 1, "name").style.left, "0px");
    AH.invoke(el, "pinColumn", "id", true);
    T.eq(cell(el, 1, "name").style.left, "50px", "offset after the pinned id column");
    AH.invoke(el, "setColumnWidth", "id", 80);
    T.eq(head(el, "id").style.width, "80px");
    T.eq(cell(el, 3, "name").style.left, "80px");
  });

  T.test("datagrid: groups with aggregates collapse; check boxes select", function (fx) {
    var el = mount(fx, "grouped");
    var groups = $(el).find(".ah-dg-group-row");
    T.eq(groups.map(function () { return this.getAttribute("data-group-id"); }).get(), ["dept:eng", "dept:ops"]);
    T.eq(groups.first().find(".ah-dg-group-title").text(), "Engineering (3)");
    T.eq(groups.first().find(".ah-dg-group-agg-item").text(), "Age: sum=99, avg=33");
    T.eq(groups.last().find(".ah-dg-group-agg-item").text(), "Age: sum=60, avg=30");
    var toggles = [];
    $(el).on("ah:group-toggle", function (e, d) { toggles.push(d.value + ":" + d.expanded); });
    groups.first().trigger("click");
    T.eq(shown(el), ["bob", "Dee"]);
    T.eq($(el).find(".ah-dg-group-row").first().attr("aria-expanded"), "false");
    T.eq(toggles, ["dept:eng:false"]);
    T.eq($(el).children("input[name=ids]").val(), "2");
    $(el).find(".ah-dg-select-all").prop("checked", true).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2,4");
    $(el).find(".ah-dg-row[data-key=4] .ah-dg-row-checkbox").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "2", "a click unchecks");
    T.eq($(el).find(".ah-dg-select-all")[0].indeterminate, true);
    AH.invoke(el, "groupBy", []);
    T.eq($(el).find(".ah-dg-group-row").length, 0);
    T.eq(shown(el).length, 5);
  });

  T.test("datagrid: remote mode asks the server and adopts its rows", function (fx) {
    var el = mount(fx, "remote", true);
    var q = $(el).children(".ah-dg-query")[0];
    var asked = [];
    $(q).on("ah:query", function () {
      asked.push({ page: q.getAttribute("data-page"), sort: q.getAttribute("data-sort"),
                   render: q.getAttribute("data-render") === el.getAttribute("data-render"),
                   exp: q.getAttribute("data-export") });
    });
    T.eq(asked.length, 0, "rows came with the page");
    $(el).find(".ah-dg-row[data-key=1]").trigger("click");
    $(el).find(".ah-dg-pager-button[data-page=2]").trigger("click");
    T.eq(asked.length, 1);
    T.eq(asked[0].page, "2");
    T.ok(asked[0].render, "the render token goes along");
    T.eq($(el).children(".ah-dg-loading-overlay").css("display"), "flex");
    AH.apply(SERVER.page2);
    T.eq($(el).children(".ah-dg-loading-overlay").css("display"), "none");
    T.eq(shown(el), ["Cy", "Dee"]);
    T.eq($(el).find(".ah-dg-pager-button-active").text(), "2");
    $(el).find(".ah-dg-row[data-key=4]").trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "4");
    T.ok($(el).find(".ah-dg-row[data-key=4]").hasClass("ah-dg-row-selected"));
    $(head(el, "age")).trigger("click");
    T.eq(JSON.parse(asked[1].sort), [["age", "asc"]]);
    AH.invoke(el, "exportData", "csv");
    T.eq(asked[2].exp, "csv");
    T.eq(q.hasAttribute("data-export"), false);
  });

  T.test("datagrid: CSV export of the filtered, sorted rows", async function (fx) {
    var el = mount(fx, "local");
    var blob = null, name = null;
    var orig = URL.createObjectURL;
    var click = HTMLAnchorElement.prototype.click;
    URL.createObjectURL = function (b) { blob = b; return "blob:x"; };
    HTMLAnchorElement.prototype.click = function () { name = this.download; };
    try {
      $(head(el, "age")).trigger("click");
      $(head(el, "age")).trigger("click");
      $(el).find(".ah-dg-toolbar-btn[data-export=csv]").trigger("click");
    } finally {
      URL.createObjectURL = orig;
      HTMLAnchorElement.prototype.click = click;
    }
    T.eq(name, "people.csv");
    var bytes = new Uint8Array(await blob.arrayBuffer());
    T.eq([bytes[0], bytes[1], bytes[2]], [0xEF, 0xBB, 0xBF], "UTF-8 BOM for Excel");
    var txt = await blob.text();
    T.eq(txt.split("\n")[0], "ID,Name,Dept,Age,Active");
    T.eq(txt.split("\n")[1], "3,Cy,Engineering,41,Yes");
    T.eq(txt.split("\n").length, 6, "all pages");
  });

  T.test("datagrid: Excel export loads xlsx on demand", async function (fx) {
    var el = mount(fx, "local");
    var XLSX = await AH.vendor("xlsx");
    var orig = XLSX.writeFile, got = null;
    XLSX.writeFile = function (wb, n) { got = { wb: wb, n: n }; };
    try { await AH.invoke(el, "exportData", "xlsx"); } finally { XLSX.writeFile = orig; }
    T.eq(got.n, "people.xlsx");
    var rows = XLSX.utils.sheet_to_json(got.wb.Sheets.Sheet1, { header: 1 });
    T.eq(rows[0], ["ID", "Name", "Dept", "Age", "Active"]);
    T.eq(rows.length, 6);
  });

  T.test("datagrid: PDF export loads jspdf and autotable on demand", async function (fx) {
    var el = mount(fx, "local");
    var libs = await AH.vendor(["jspdf", "jspdf-autotable"]);
    var proto = libs[0].jsPDF.API, orig = proto.save, saved = null, pages = 0;
    proto.save = function (n) { saved = n; pages = this.getNumberOfPages(); return this; };
    try { await AH.invoke(el, "exportData", "pdf"); } finally { proto.save = orig; }
    T.eq(saved, "people.pdf");
    T.eq(pages, 1);
  });
})(window.AHTest, window.jQuery, window.AH);
