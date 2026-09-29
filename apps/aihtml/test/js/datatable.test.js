/* datatable: the datatable behaviour on markup the server renders. SERVER
 * holds renders of aihtml_datatable:datatable/4 (ids "dt": pages of 2,
 * filter row, checkboxes, details, editing, chooser; "dta": advanced
 * filters, multiple; "dts": search, single; "dtr": remote); OPS the
 * operations the actions answer with (datatable_rows/3 for page 2 by age
 * desc, datatable_row/4 for row 2 of "dt"). Regenerate them from Erlang
 * if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "dt": "<div class=\"ah-dt\" id=\"dt\" role=\"grid\" aria-multiselectable=\"true\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"checkbox\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"row\" data-page=\"1\" data-page-size=\"2\" data-editable=\"true\" data-edit=\"g2gDdwFtdwRlZGl0dAAAAAA.BRZJlYeNycVBw--vCPtB75I0jGWEPMaDhwXjsXgH2VQ\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><div class=\"ah-dt-chooser-wrap\"><button class=\"ah-dt-chooser-btn\" type=\"button\" title=\"Columns\" aria-label=\"Columns\" aria-haspopup=\"true\" aria-expanded=\"false\">☰</button></div><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:36px;min-width:36px;\"><col style=\"width:40px;min-width:40px;\"><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-expand\" role=\"columnheader\"></th><th class=\"ah-dt-th ah-dt-th-checkbox\" role=\"columnheader\"><input class=\"ah-dt-header-checkbox\" type=\"checkbox\" aria-label=\"Select all rows\"></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\"></td><td class=\"ah-dt-filter-cell\"></td><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:36px;min-width:36px;\"><col style=\"width:40px;min-width:40px;\"><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dt-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-1-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-1-d\" data-key=\"1\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Ann</div></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dt-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-2-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-2-d\" data-key=\"2\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Bob</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-3-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-3-d\" data-key=\"3\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Cid</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-4-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-4-d\" data-key=\"4\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Dan</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-5-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-5-d\" data-key=\"5\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Eve</div></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"5\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"2\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div><div class=\"ah-dt-chooser-panel\" role=\"group\" aria-label=\"Columns\"><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"name\" checked>Name</label><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"age\" checked>Age</label><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"city\" checked>City</label></div><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "dta": "<div class=\"ah-dt\" id=\"dta\" role=\"grid\" aria-multiselectable=\"true\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"multiple\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"advanced\" data-page=\"1\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row ah-dt-filter-row-advanced\" role=\"row\"><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"name\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"name\" aria-label=\"Name\"><option value=\"contains\" selected>Contains</option><option value=\"not_contains\">Not Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"starts_with\">Starts With</option><option value=\"ends_with\">Ends With</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"Name\"></div></td><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"age\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"age\" aria-label=\"Age\"><option value=\"contains\" selected>Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"gt\">Greater Than</option><option value=\"gte\">Greater or Equal</option><option value=\"lt\">Less Than</option><option value=\"lte\">Less or Equal</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"Age\"></div></td><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"city\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"city\" aria-label=\"City\"><option value=\"contains\" selected>Contains</option><option value=\"not_contains\">Not Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"starts_with\">Starts With</option><option value=\"ends_with\">Ends With</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"City\"></div></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dta-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dta-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dta-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div></div>",
    "dts": "<div class=\"ah-dt\" id=\"dts\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"search\" data-page=\"1\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><div class=\"ah-dt-search-bar\"><input class=\"ah-dt-search-input\" type=\"text\" value=\"\" placeholder=\"Search...\" aria-label=\"Search...\"></div><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dts-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dts-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dts-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div></div>",
    "dtr": "<div class=\"ah-dt\" id=\"dtr\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"remote\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"row\" data-page=\"1\" data-page-size=\"2\" data-total=\"5\" data-ah-on=\"ah:query:g2gDdwFtdwFxdAAAAAA.LoM_TD-cwiHIKOUY0TP0WZaRfgMp97FmoaLkH17ZhuI\" data-ah-sync=\"replace\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dtr-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dtr-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dtr-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"2\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>"
  };

  var OPS = {
    "query": [{"id":"dtr","op":"html","html":"<div class=\"ah-dt\" id=\"dtr\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"3\" data-selection=\"single\" data-mode=\"remote\" data-sortable=\"true\" data-alt-rows=\"true\" data-sort-field=\"age\" data-sort-dir=\"desc\" data-filter=\"row\" data-page=\"2\" data-page-size=\"2\" data-total=\"5\" data-ah-on=\"ah:query:g2gDdwFtdwFxdAAAAAA.LoM_TD-cwiHIKOUY0TP0WZaRfgMp97FmoaLkH17ZhuI\" data-ah-sync=\"replace\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable ah-dt-sort-desc\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" aria-sort=\"descending\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dtr-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dtr-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dtr-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">3-4 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\">‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"1\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"2\" aria-current=\"page\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>","swap":"morph"}],
    "edit": [{"id":"dt-r-2","op":"html","html":"<tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-2\" role=\"row\" data-key=\"2\" data-i=\"0\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-2-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Robert</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"26\">26 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr>","swap":"morph"},{"id":"dt-r-2-d","op":"html","html":"<tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-2-d\" data-key=\"2\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Robert</div></td></tr>","swap":"morph"},{"args":[],"id":"dt","op":"call","method":"refresh"}]
  };

  function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    AH.mount(fx);
    return fx.firstChild;
  }

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  function key(el, k, extra) {
    var e = $.Event("keydown", $.extend({ key: k }, extra || {}));
    $(el).trigger(e);
    return e;
  }

  function rows(el, pre) { return $(el).find("tbody > tr." + pre + "-row").toArray(); }

  function keys(el, pre) { return rows(el, pre).map(function (r) { return r.getAttribute("data-key"); }); }

  function shown(el, pre) {
    return rows(el, pre).filter(function (r) { return !r.hidden; })
      .map(function (r) { return r.getAttribute("data-key"); });
  }

  function row(el, pre, k) { return $(el).find("tbody > tr." + pre + "-row[data-key='" + k + "']")[0]; }

  function focusedKey() { return document.activeElement && document.activeElement.getAttribute("data-key"); }

  // Answers the next actions with the given operations, as an AG-UI stream.
  function stubFetch(queue, calls) {
    var orig = window.fetch;
    window.fetch = function (url, opts) {
      calls.push(JSON.parse(opts.body));
      var ops = queue.shift() || [];
      var ev = function (o) { return "data: " + JSON.stringify(o) + "\n\n"; };
      var body = ev({ type: "RUN_STARTED" }) + ev({ type: "CUSTOM", name: "aihtml.ui", value: ops }) +
        ev({ type: "RUN_FINISHED" });
      return Promise.resolve(new Response(body, { status: 200, headers: { "Content-Type": "text/event-stream" } }));
    };
    return function () { window.fetch = orig; };
  }

  T.test("datatable: pages, sorts and filters locally", async function (fx) {
    var el = mount(fx, "dt");
    var pages = [];
    $(el).on("ah:page", function (e, d) { pages.push(d.page); });
    T.eq(shown(el, "ah-dt"), ["1", "2"]);
    $(el).find(".ah-dt-pager-btn-next").trigger("click");
    T.eq(shown(el, "ah-dt"), ["3", "4"]);
    T.eq($(el).find(".ah-dt-pager-info").text(), "3-4 of 5");
    T.eq($(el).find(".ah-dt-pager-btn-active").attr("data-page"), "2");
    $(el).find(".ah-dt-pager-btn-num[data-page=3]").trigger("click");
    T.eq(shown(el, "ah-dt"), ["5"]);
    T.ok($(el).find(".ah-dt-pager-btn-next").prop("disabled"));
    T.eq(pages, [2, 3]);
    // sort by the raw age (the cells render "31 y"), back on page 1
    $(el).find("th[data-field=age]").trigger("click");
    T.eq(el.getAttribute("data-page"), "1");
    T.eq(keys(el, "ah-dt"), ["4", "2", "1", "5", "3"]);
    T.eq(shown(el, "ah-dt"), ["4", "2"]);
    // details rows travel with their rows
    T.eq($(row(el, "ah-dt", "4")).next().attr("data-key"), "4");
    T.ok($(row(el, "ah-dt", "4")).next().hasClass("ah-dt-row-details"));
    T.eq(shown(el, "ah-dt").map(function (k) { return $(row(el, "ah-dt", k)).hasClass("ah-dt-row-alt") ? 1 : 0; }).join(""), "01");
    $(el).find(".ah-dt-pager-size-select").val("5").trigger("change");
    T.eq(shown(el, "ah-dt"), ["4", "2", "1", "5", "3"]);
    // filter row, debounced
    $(el).find(".ah-dt-filter-input[data-field=city]").val("o").trigger("input");
    T.eq(shown(el, "ah-dt").length, 5, "not before the debounce");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["2", "1", "3"]);
    T.eq($(el).find(".ah-dt-pager-info").text(), "1-3 of 3");
    $(el).find(".ah-dt-filter-input[data-field=age]").val("4").trigger("input");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["3"], "filters match the raw value");
    AH.invoke(el, "clearFilters");
    T.eq(shown(el, "ah-dt").length, 5);
    $(el).find(".ah-dt-filter-input[data-field=name]").val("zzz").trigger("input");
    await wait(260);
    T.eq(shown(el, "ah-dt"), []);
    T.ok(!$(el).find(".ah-dt-row-empty")[0].hidden, "the empty row shows");
    T.ok(row(el, "ah-dt", "1").nextSibling.hidden, "details hide with their row");
  });

  T.test("datatable: checkbox selection and the page's header box", function (fx) {
    var el = mount(fx, "dt");
    var changes = 0;
    $(el).on("change", function () { changes++; });
    $(row(el, "ah-dt", "2")).find(".ah-dt-row-checkbox").prop("checked", true).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq($(el).children("input[name=sel]").val(), "2");
    T.ok($(el).find(".ah-dt-header-checkbox").prop("indeterminate"));
    $(el).find(".ah-dt-pager-btn-next").trigger("click");
    T.ok(!$(el).find(".ah-dt-header-checkbox").prop("checked") &&
         !$(el).find(".ah-dt-header-checkbox").prop("indeterminate"));
    $(el).find(".ah-dt-header-checkbox").prop("checked", true).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2,3,4", "the page's rows join the selection");
    $(el).find(".ah-dt-header-checkbox").prop("checked", false).trigger("change");
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(changes, 3);
    // keyboard: Space toggles in checkbox mode
    var r3 = row(el, "ah-dt", "3");
    r3.focus();
    key(r3, " ");
    T.eq(el.getAttribute("data-ah-value"), "2,3");
    key(r3, "ArrowDown");
    T.eq(focusedKey(), "4");
    key(document.activeElement, "ArrowUp");
    T.eq(focusedKey(), "3");
  });

  T.test("datatable: single and multiple selection", function (fx) {
    var el = mount(fx, "dts");
    $(row(el, "ah-dt", "2")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "2");
    $(row(el, "ah-dt", "3")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "3");
    $(row(el, "ah-dt", "3")).trigger("click");
    T.eq(el.getAttribute("data-ah-value"), "", "a click on the selected row clears it");
    var m = mount(fx, "dta");
    $(row(m, "ah-dt", "1")).trigger("click");
    $(row(m, "ah-dt", "4")).trigger($.Event("click", { shiftKey: true }));
    T.eq(m.getAttribute("data-ah-value"), "1,2,3,4");
    $(row(m, "ah-dt", "2")).trigger($.Event("click", { metaKey: true }));
    T.eq(m.getAttribute("data-ah-value"), "1,3,4");
  });

  T.test("datatable: search and advanced filters", async function (fx) {
    var el = mount(fx, "dts");
    $(el).find(".ah-dt-search-input").val(" OSL ").trigger("input");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["1", "3"]);
    AH.invoke(el, "setSearch", "e");
    T.eq(shown(el, "ah-dt"), ["2", "5"]);
    T.eq(el.getAttribute("data-search"), "e");
    var a = mount(fx, "dta");
    $(a).find(".ah-dt-adv-filter-select[data-field=age]").val("gte").trigger("change");
    T.eq(shown(a, "ah-dt").length, 5, "no value, no filter");
    $(a).find(".ah-dt-adv-filter-input[data-field=age]").val("31").trigger("input");
    await wait(260);
    T.eq(shown(a, "ah-dt"), ["1", "3", "5"]);
    $(a).find(".ah-dt-adv-filter-select[data-field=city]").val("empty").trigger("change");
    T.ok($(a).find(".ah-dt-adv-filter-input[data-field=city]").prop("disabled"));
    T.eq(shown(a, "ah-dt"), []);
    $(a).find(".ah-dt-adv-filter-select[data-field=city]").val("not_empty").trigger("change");
    T.eq(shown(a, "ah-dt"), ["1", "3", "5"]);
    $(a).find(".ah-dt-adv-filter-select[data-field=name]").val("ends_with").trigger("change");
    $(a).find(".ah-dt-adv-filter-input[data-field=name]").val("E").trigger("input");
    await wait(260);
    T.eq(shown(a, "ah-dt"), ["5"]);
  });

  T.test("datatable: row details open and close", function (fx) {
    var el = mount(fx, "dt");
    var ev = [];
    $(el).on("ah:row-expand ah:row-collapse", function (e, d) { ev.push(e.type + ":" + d.key); });
    var btn = $(row(el, "ah-dt", "2")).find(".ah-dt-expand-btn");
    var details = row(el, "ah-dt", "2").nextSibling;
    T.ok($(details).hasClass("ah-dt-row-details-hidden"));
    btn.trigger("click");
    T.ok(!$(details).hasClass("ah-dt-row-details-hidden"));
    T.eq(btn.attr("aria-expanded"), "true");
    T.eq(el.getAttribute("data-expanded"), "2");
    T.eq(el.getAttribute("data-ah-value"), "", "the arrow does not select");
    key(row(el, "ah-dt", "2"), "ArrowLeft");
    T.ok($(details).hasClass("ah-dt-row-details-hidden"));
    T.eq(el.hasAttribute("data-expanded"), false);
    AH.invoke(el, "expandRow", "1");
    T.eq(el.getAttribute("data-expanded"), "1");
    T.eq(ev, ["ah:row-expand:2", "ah:row-collapse:2"]);
  });

  T.test("datatable: edits cells and sends them to the edit action", async function (fx) {
    var el = mount(fx, "dt");
    var calls = [];
    var restore = stubFetch([OPS.edit], calls);
    try {
      var edits = [];
      $(el).on("ah:cell-edit", function (e, d) { edits.push(d); });
      var cell = $(row(el, "ah-dt", "1")).children("td[data-field=name]")[0];
      $(cell).trigger("dblclick");
      var input = cell.querySelector(".ah-dt-editor");
      T.ok(input && input.type === "text" && input.value === "Ann");
      input.value = "Anna";
      key(input, "Escape");
      T.eq(cell.textContent, "Ann", "Escape cancels");
      T.eq(edits.length, 0);
      // numbers edit their raw value
      var age = $(row(el, "ah-dt", "2")).children("td[data-field=age]")[0];
      $(age).trigger("dblclick");
      T.eq(age.querySelector("input").type + ":" + age.querySelector("input").value, "number:25");
      key(age.querySelector("input"), "Tab", { shiftKey: true });
      var name2 = $(row(el, "ah-dt", "2")).children("td[data-field=name]")[0];
      T.ok(name2.querySelector(".ah-dt-editor"), "Shift+Tab goes to the previous editable cell");
      name2.querySelector("input").value = "Bobby";
      key(name2.querySelector("input"), "Enter");
      T.eq(edits, [{ key: "2", field: "name", value: "Bobby", old: "Bob" }]);
      T.eq(name2.textContent, "Bobby");
      T.eq([name2.getAttribute("data-key"), name2.getAttribute("data-old"), name2.getAttribute("data-table")],
           ["2", "Bob", "dt"]);
      await wait(80);
      T.eq(calls.length, 1);
      T.eq(calls[0].event.type, "ah:cell-edit");
      T.eq([calls[0].event.data.key, calls[0].event.data.field, calls[0].event.data.value], ["2", "name", "Bobby"]);
      // the server's row (edit ops morph it and call refresh)
      T.eq($(row(el, "ah-dt", "2")).children("td[data-field=name]").text(), "Robert");
      T.eq($(row(el, "ah-dt", "2")).children("td[data-field=age]").text(), "26 y");
      T.ok(!row(el, "ah-dt", "2").hidden && row(el, "ah-dt", "3").hidden, "refresh keeps the page");
      T.eq(shown(el, "ah-dt").map(function (k) { return $(row(el, "ah-dt", k)).hasClass("ah-dt-row-alt") ? 1 : 0; }).join(""), "01");
    } finally { restore(); }
  });

  T.test("datatable: column chooser and resize", function (fx) {
    var el = mount(fx, "dt");
    var ev = [];
    $(el).on("ah:columns", function (e, d) { ev.push(d.hidden.join(",")); });
    $(el).find(".ah-dt-chooser-btn").trigger("click");
    var panel = $(el).children(".ah-dt-chooser-panel");
    T.ok(panel.hasClass("ah-dt-chooser-panel-open"));
    T.eq($(el).find(".ah-dt-chooser-btn").attr("aria-expanded"), "true");
    panel.find(".ah-dt-chooser-checkbox[data-field=city]").prop("checked", false).trigger("change");
    T.ok($(el).find("th[data-field=city]")[0].hidden);
    T.ok($(row(el, "ah-dt", "1")).children("td[data-field=city]")[0].hidden);
    T.ok($(el).find(".ah-dt-filter-cell[data-field=city]")[0].hidden);
    T.eq($(el).find(".ah-dt-row-details-cell").first().attr("colspan"), "4");
    T.eq(el.getAttribute("data-hidden"), "city");
    $(document).trigger($.Event("keydown", { key: "Escape" }));
    T.ok(!panel.hasClass("ah-dt-chooser-panel-open"));
    AH.invoke(el, "showColumn", "city");
    T.ok(!$(el).find("th[data-field=city]")[0].hidden);
    T.ok(panel.find(".ah-dt-chooser-checkbox[data-field=city]").prop("checked"));
    T.eq(ev, ["city", ""]);
    // resize: drag the name column's handle by 40px
    var resized = [];
    $(el).on("ah:column-resize", function (e, d) { resized.push(d); });
    var th = $(el).find("th[data-field=name]")[0];
    var w0 = th.getBoundingClientRect().width;
    $(th).find(".ah-dt-resize-handle").trigger($.Event("mousedown", { button: 0, pageX: 100 }));
    $(document).trigger($.Event("mousemove", { pageX: 140 }));
    $(document).trigger($.Event("mouseup", { pageX: 140 }));
    T.eq(resized.length, 1);
    T.eq(resized[0].field, "name");
    T.eq(resized[0].width, Math.round(w0 + 40));
    var idx = Array.prototype.indexOf.call(th.parentNode.children, th);
    T.eq($(el).find(".ah-dt-body colgroup")[0].children[idx].style.width, Math.round(w0 + 40) + "px");
  });

  T.test("datatable: remote queries the server and takes its page", async function (fx) {
    var el = mount(fx, "dtr");
    var calls = [];
    var restore = stubFetch([[], OPS.query], calls);
    try {
      T.eq(shown(el, "ah-dt"), ["1", "2"]);
      var queries = [];
      $(el).on("ah:query", function (e, d) { queries.push(d.sort + ":" + d.dir + ":" + d.page); });
      $(el).find("th[data-field=age]").trigger("click");
      T.ok($(el).children(".ah-dt-content").hasClass("ah-dt-loading"));
      T.eq(keys(el, "ah-dt"), ["1", "2"], "no local sorting");
      $(el).find("th[data-field=age]").trigger("click");
      $(el).find(".ah-dt-filter-input[data-field=city]").val("o");
      el.setAttribute("data-page", "2");
      AH.invoke(el, "goToPage", 2);
      T.eq(queries, ["age:asc:1", "age:desc:1", "age:desc:2"]);
      await wait(120);
      T.ok(calls.length >= 2);
      var last = calls[calls.length - 1].event;
      T.eq(last.type, "ah:query");
      T.eq(last.id, "dtr");
      T.eq([last.data.sortField, last.data.sortDir, last.data.page, last.data.pageSize],
           ["age", "desc", "2", "2"]);
      T.eq(JSON.parse(last.data.filters), { city: "o" });
      // the morph put the server's page in: ages desc 47 38 | 31 25 | 19
      T.eq(keys(el, "ah-dt"), ["1", "2"]);
      T.eq($(el).find(".ah-dt-pager-info").text(), "3-4 of 5");
      T.eq(el.getAttribute("data-ah-value"), "3");
      T.ok(!$(el).children(".ah-dt-content").hasClass("ah-dt-loading"));
      T.ok($(el).find("th[data-field=age]").hasClass("ah-dt-sort-desc"));
    } finally { restore(); }
  });
})(window.AHTest, window.jQuery, window.AH);
