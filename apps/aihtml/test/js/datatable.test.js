/* datatable: the datatable controller on markup the server renders. SERVER
 * holds renders of aihtml_datatable:datatable/4 (ids "dt": pages of 2,
 * filter row, checkboxes, details, editing, chooser; "dta": advanced
 * filters, multiple; "dts": search, single; "dtr": remote); OPS the
 * operations the actions answer with (datatable_rows/3 for page 2 by age
 * desc, datatable_row/4 for row 2 of "dt"); "dth" (local) and "dthr"
 * (remote) have href "/t?p={page}&s={size}&o={sort}&q={search}". Regenerate them from Erlang
 * if the markup changes. */
(function (T, AH) {
  "use strict";

  var SERVER = {
    "dt": "<div class=\"ah-dt\" id=\"dt\" role=\"grid\" aria-multiselectable=\"true\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"checkbox\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"row\" data-page=\"1\" data-page-size=\"2\" data-editable=\"true\" data-edit=\"g2gDdwFtdwRlZGl0dAAAAAA.BRZJlYeNycVBw--vCPtB75I0jGWEPMaDhwXjsXgH2VQ\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><div class=\"ah-dt-chooser-wrap\"><button class=\"ah-dt-chooser-btn\" type=\"button\" title=\"Columns\" aria-label=\"Columns\" aria-haspopup=\"true\" aria-expanded=\"false\">☰</button></div><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:36px;min-width:36px;\"><col style=\"width:40px;min-width:40px;\"><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-expand\" role=\"columnheader\"></th><th class=\"ah-dt-th ah-dt-th-checkbox\" role=\"columnheader\"><input class=\"ah-dt-header-checkbox\" type=\"checkbox\" aria-label=\"Select all rows\"></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div><div class=\"ah-dt-resize-handle\" aria-hidden=\"true\"></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\"></td><td class=\"ah-dt-filter-cell\"></td><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:36px;min-width:36px;\"><col style=\"width:40px;min-width:40px;\"><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dt-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-1-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-1-d\" data-key=\"1\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Ann</div></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dt-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-2-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-2-d\" data-key=\"2\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Bob</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-3-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-3-d\" data-key=\"3\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Cid</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-4-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-4-d\" data-key=\"4\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Dan</div></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-5-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-5-d\" data-key=\"5\" hidden><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Eve</div></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"5\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"2\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div><div class=\"ah-dt-chooser-panel\" role=\"group\" aria-label=\"Columns\"><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"name\" checked>Name</label><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"age\" checked>Age</label><label class=\"ah-dt-chooser-item\"><input class=\"ah-dt-chooser-checkbox\" type=\"checkbox\" data-field=\"city\" checked>City</label></div><input type=\"hidden\" name=\"sel\" value=\"\" data-ah-input></div>",
    "dta": "<div class=\"ah-dt\" id=\"dta\" role=\"grid\" aria-multiselectable=\"true\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"multiple\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"advanced\" data-page=\"1\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row ah-dt-filter-row-advanced\" role=\"row\"><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"name\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"name\" aria-label=\"Name\"><option value=\"contains\" selected>Contains</option><option value=\"not_contains\">Not Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"starts_with\">Starts With</option><option value=\"ends_with\">Ends With</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"Name\"></div></td><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"age\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"age\" aria-label=\"Age\"><option value=\"contains\" selected>Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"gt\">Greater Than</option><option value=\"gte\">Greater or Equal</option><option value=\"lt\">Less Than</option><option value=\"lte\">Less or Equal</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"Age\"></div></td><td class=\"ah-dt-filter-cell ah-dt-filter-cell-advanced\" data-field=\"city\"><div class=\"ah-dt-adv-filter-wrap\"><select class=\"ah-dt-adv-filter-select\" data-field=\"city\" aria-label=\"City\"><option value=\"contains\" selected>Contains</option><option value=\"not_contains\">Not Contains</option><option value=\"equals\">Equals</option><option value=\"not_equals\">Not Equals</option><option value=\"starts_with\">Starts With</option><option value=\"ends_with\">Ends With</option><option value=\"empty\">Empty</option><option value=\"not_empty\">Not Empty</option></select><input class=\"ah-dt-adv-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Value...\" value=\"\" aria-label=\"City\"></div></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dta-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dta-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dta-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dta-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div></div>",
    "dts": "<div class=\"ah-dt\" id=\"dts\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"search\" data-page=\"1\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><div class=\"ah-dt-search-bar\"><input class=\"ah-dt-search-input\" type=\"text\" value=\"\" placeholder=\"Search...\" aria-label=\"Search...\"></div><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dts-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dts-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\">47 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dts-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\">19 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dts-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\">38 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div></div>",
    "dtr": "<div class=\"ah-dt\" id=\"dtr\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"remote\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"row\" data-page=\"1\" data-page-size=\"2\" data-total=\"5\" data-ah-on=\"ah:query:g2gDdwFtdwFxdAAAAAA.LoM_TD-cwiHIKOUY0TP0WZaRfgMp97FmoaLkH17ZhuI\" data-ah-sync=\"replace\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dtr-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dtr-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dtr-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"2\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>",
    "dth": "<div class=\"ah-dt\" id=\"dth\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"local\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"none\" data-page=\"1\" data-page-size=\"2\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dth-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dth-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\"><span>31</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dth-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\"><span>25</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dth-r-3\" role=\"row\" data-key=\"3\" data-i=\"2\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Cid</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"47\"><span>47</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dth-r-4\" role=\"row\" data-key=\"4\" data-i=\"3\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Dan</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"19\"><span>19</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span></span></td></tr><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dth-r-5\" role=\"row\" data-key=\"5\" data-i=\"4\" aria-selected=\"false\" tabindex=\"-1\" hidden><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Eve</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"38\"><span>38</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Lima</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\" data-href=\"/t?p={page}&amp;s={size}&amp;o={sort}&amp;q={search}\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><a class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" href=\"/t?p=2&amp;s=2&amp;o=&amp;q=\" data-page=\"2\">2</a><a class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" href=\"/t?p=3&amp;s=2&amp;o=&amp;q=\" data-page=\"3\">3</a><a class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" href=\"/t?p=2&amp;s=2&amp;o=&amp;q=\" aria-label=\"Next page\">›</a></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>",
    "dthr": "<div class=\"ah-dt\" id=\"dthr\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"\" data-selection=\"single\" data-mode=\"remote\" data-sortable=\"true\" data-alt-rows=\"true\" data-filter=\"none\" data-page=\"1\" data-page-size=\"2\" data-total=\"5\" data-ah-on=\"ah:query:g2gDdxZhaWh0bWxfZGF0YXRhYmxlX3Rlc3RzdwVxdWVyeXQAAAAA.dfsi-HxzNUn2jFMb8yQgBDDi3WnGFHc3nhg6N5l8zbM\" data-ah-sync=\"replace\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dthr-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dthr-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\"><span>31</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dthr-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\"><span>25</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\" data-href=\"/t?p={page}&amp;s={size}&amp;o={sort}&amp;q={search}\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">1-2 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\" disabled>‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"1\" aria-current=\"page\">1</button><a class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" href=\"/t?p=2&amp;s=2&amp;o=&amp;q=\" data-page=\"2\">2</a><a class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" href=\"/t?p=3&amp;s=2&amp;o=&amp;q=\" data-page=\"3\">3</a><a class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" href=\"/t?p=2&amp;s=2&amp;o=&amp;q=\" aria-label=\"Next page\">›</a></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>"
  };

  var OPS = {
    "query": [{"id":"dtr","op":"html","html":"<div class=\"ah-dt\" id=\"dtr\" role=\"grid\" data-ah=\"datatable\" data-ah-value=\"3\" data-selection=\"single\" data-mode=\"remote\" data-sortable=\"true\" data-alt-rows=\"true\" data-sort-field=\"age\" data-sort-dir=\"desc\" data-filter=\"row\" data-page=\"2\" data-page-size=\"2\" data-total=\"5\" data-ah-on=\"ah:query:g2gDdwFtdwFxdAAAAAA.LoM_TD-cwiHIKOUY0TP0WZaRfgMp97FmoaLkH17ZhuI\" data-ah-sync=\"replace\"><div class=\"ah-dt-content\"><div class=\"ah-dt-header\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><thead role=\"rowgroup\"><tr class=\"ah-dt-header-row\" role=\"row\"><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"name\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Name</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable ah-dt-sort-desc\" role=\"columnheader\" data-field=\"age\" data-type=\"number\" style=\"text-align:right;\" aria-sort=\"descending\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">Age</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th><th class=\"ah-dt-th ah-dt-th-sortable\" role=\"columnheader\" data-field=\"city\" data-type=\"text\" style=\"text-align:left;\" tabindex=\"0\"><div class=\"ah-dt-th-content\"><span class=\"ah-dt-th-text\">City</span><span class=\"ah-dt-sort-icon\" aria-hidden=\"true\"></span></div></th></tr><tr class=\"ah-dt-filter-row\" role=\"row\"><td class=\"ah-dt-filter-cell\" data-field=\"name\"><input class=\"ah-dt-filter-input\" data-field=\"name\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Name\"></td><td class=\"ah-dt-filter-cell\" data-field=\"age\"><input class=\"ah-dt-filter-input\" data-field=\"age\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"Age\"></td><td class=\"ah-dt-filter-cell\" data-field=\"city\"><input class=\"ah-dt-filter-input\" data-field=\"city\" type=\"text\" placeholder=\"Filter...\" value=\"\" aria-label=\"City\"></td></tr></thead></table></div><div class=\"ah-dt-body\"><table class=\"ah-dt-table\" role=\"presentation\"><colgroup><col style=\"width:120px;min-width:120px;\"><col><col></colgroup><tbody role=\"rowgroup\" id=\"dtr-rows\"><tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dtr-r-1\" role=\"row\" data-key=\"1\" data-i=\"0\" aria-selected=\"false\" tabindex=\"0\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Ann</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"31\">31 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Oslo</span></td></tr><tr class=\"ah-dt-row ah-dt-row-alt ah-dt-row-hover\" id=\"dtr-r-2\" role=\"row\" data-key=\"2\" data-i=\"1\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Bob</span></td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"25\">25 y</td><td class=\"ah-dt-cell\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr><tr class=\"ah-dt-row-empty\" hidden><td class=\"ah-dt-cell-empty\" colspan=\"3\">No data to display</td></tr></tbody></table></div><div class=\"ah-dt-loading-overlay\"><div class=\"ah-dt-loading-spinner\"></div></div></div><div class=\"ah-dt-pager-container\" data-info=\"{start}-{end} of {total}\" data-prev=\"Previous page\" data-next=\"Next page\" data-size=\"Rows per page\" data-sizes=\"5,10,25,50\"><div class=\"ah-dt-pager\"><div class=\"ah-dt-pager-info\">3-4 of 5</div><div class=\"ah-dt-pager-buttons\"><button class=\"ah-dt-pager-btn ah-dt-pager-btn-prev\" type=\"button\" aria-label=\"Previous page\">‹</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"1\">1</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num ah-dt-pager-btn-active\" type=\"button\" data-page=\"2\" aria-current=\"page\">2</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-num\" type=\"button\" data-page=\"3\">3</button><button class=\"ah-dt-pager-btn ah-dt-pager-btn-next\" type=\"button\" aria-label=\"Next page\">›</button></div><div class=\"ah-dt-pager-size\"><select class=\"ah-dt-pager-size-select\" aria-label=\"Rows per page\"><option value=\"2\" selected>2</option><option value=\"5\">5</option><option value=\"10\">10</option><option value=\"25\">25</option><option value=\"50\">50</option></select></div></div></div></div>","swap":"morph"}],
    "edit": [{"id":"dt-r-2","op":"html","html":"<tr class=\"ah-dt-row ah-dt-row-hover\" id=\"dt-r-2\" role=\"row\" data-key=\"2\" data-i=\"0\" aria-selected=\"false\" tabindex=\"-1\"><td class=\"ah-dt-cell ah-dt-expand-cell\" role=\"gridcell\"><button class=\"ah-dt-expand-btn\" type=\"button\" tabindex=\"-1\" aria-expanded=\"false\" aria-label=\"Details\" aria-controls=\"dt-r-2-d\">›</button></td><td class=\"ah-dt-cell ah-dt-checkbox-cell\" role=\"gridcell\"><input class=\"ah-dt-row-checkbox\" type=\"checkbox\" tabindex=\"-1\" aria-label=\"Select row\"></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"name\" style=\"text-align:left;\"><span>Robert</span></td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"age\" style=\"text-align:right;\" data-value=\"26\">26 y</td><td class=\"ah-dt-cell ah-dt-cell-editable\" role=\"gridcell\" data-field=\"city\" style=\"text-align:left;\"><span>Rome</span></td></tr>","swap":"morph"},{"id":"dt-r-2-d","op":"html","html":"<tr class=\"ah-dt-row-details ah-dt-row-details-hidden\" id=\"dt-r-2-d\" data-key=\"2\"><td class=\"ah-dt-row-details-cell\" colspan=\"5\"><div class=\"ah-dt-row-details-content\">About Robert</div></td></tr>","swap":"morph"},{"args":[],"id":"dt","op":"call","method":"refresh"}]
  };

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    await T.ready(fx);
    return fx.firstChild;
  }

  function wait(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  function key(el, k, extra) { return T.key(el, k, extra); }
  function click(el, init) { return T.fire(el, "click", init); }
  function q(el, sel) { return el.querySelector(sel); }
  function set(el, v, type) { el.value = v; T.fire(el, type || "input"); }
  function check(el, on) { el.checked = on; T.fire(el, "change"); }
  function on(el, types, fn) {
    types.split(" ").forEach(function (t) { el.addEventListener(t, fn); });
  }
  function child(el, sel) {
    return Array.prototype.filter.call(el.children, function (c) { return c.matches(sel); })[0];
  }

  function rows(el, pre) { return Array.from(el.querySelectorAll("tbody > tr." + pre + "-row")); }

  function keys(el, pre) { return rows(el, pre).map(function (r) { return r.getAttribute("data-key"); }); }

  function shown(el, pre) {
    return rows(el, pre).filter(function (r) { return !r.hidden; })
      .map(function (r) { return r.getAttribute("data-key"); });
  }

  function row(el, pre, k) { return el.querySelector("tbody > tr." + pre + "-row[data-key='" + k + "']"); }

  function alt(el, k) { return row(el, "ah-dt", k).classList.contains("ah-dt-row-alt") ? 1 : 0; }

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
    var el = await mount(fx, "dt");
    var pages = [];
    on(el, "ah:page", function (e) { pages.push(e.detail.page); });
    T.eq(shown(el, "ah-dt"), ["1", "2"]);
    click(q(el, ".ah-dt-pager-btn-next"));
    T.eq(shown(el, "ah-dt"), ["3", "4"]);
    T.eq(q(el, ".ah-dt-pager-info").textContent, "3-4 of 5");
    T.eq(q(el, ".ah-dt-pager-btn-active").getAttribute("data-page"), "2");
    click(q(el, ".ah-dt-pager-btn-num[data-page='3']"));
    T.eq(shown(el, "ah-dt"), ["5"]);
    T.ok(q(el, ".ah-dt-pager-btn-next").disabled);
    T.eq(pages, [2, 3]);
    // sort by the raw age (the cells render "31 y"), back on page 1
    click(q(el, "th[data-field=age]"));
    T.eq(el.getAttribute("data-page"), "1");
    T.eq(keys(el, "ah-dt"), ["4", "2", "1", "5", "3"]);
    T.eq(shown(el, "ah-dt"), ["4", "2"]);
    // details rows travel with their rows
    T.eq(row(el, "ah-dt", "4").nextElementSibling.getAttribute("data-key"), "4");
    T.ok(row(el, "ah-dt", "4").nextElementSibling.classList.contains("ah-dt-row-details"));
    T.eq(shown(el, "ah-dt").map(function (k) { return alt(el, k); }).join(""), "01");
    set(q(el, ".ah-dt-pager-size-select"), "5", "change");
    T.eq(shown(el, "ah-dt"), ["4", "2", "1", "5", "3"]);
    // filter row, debounced
    set(q(el, ".ah-dt-filter-input[data-field=city]"), "o");
    T.eq(shown(el, "ah-dt").length, 5, "not before the debounce");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["2", "1", "3"]);
    T.eq(q(el, ".ah-dt-pager-info").textContent, "1-3 of 3");
    set(q(el, ".ah-dt-filter-input[data-field=age]"), "4");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["3"], "filters match the raw value");
    AH.invoke(el, "clearFilters");
    T.eq(shown(el, "ah-dt").length, 5);
    set(q(el, ".ah-dt-filter-input[data-field=name]"), "zzz");
    await wait(260);
    T.eq(shown(el, "ah-dt"), []);
    T.ok(!q(el, ".ah-dt-row-empty").hidden, "the empty row shows");
    T.ok(row(el, "ah-dt", "1").nextSibling.hidden, "details hide with their row");
  });

  T.test("datatable: checkbox selection and the page's header box", async function (fx) {
    var el = await mount(fx, "dt");
    var changes = 0;
    on(el, "change", function () { changes++; });
    check(q(row(el, "ah-dt", "2"), ".ah-dt-row-checkbox"), true);
    T.eq(el.getAttribute("data-ah-value"), "2");
    T.eq(child(el, "input[name=sel]").value, "2");
    T.ok(q(el, ".ah-dt-header-checkbox").indeterminate);
    click(q(el, ".ah-dt-pager-btn-next"));
    T.ok(!q(el, ".ah-dt-header-checkbox").checked && !q(el, ".ah-dt-header-checkbox").indeterminate);
    check(q(el, ".ah-dt-header-checkbox"), true);
    T.eq(el.getAttribute("data-ah-value"), "2,3,4", "the page's rows join the selection");
    check(q(el, ".ah-dt-header-checkbox"), false);
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

  T.test("datatable: single and multiple selection", async function (fx) {
    var el = await mount(fx, "dts");
    click(row(el, "ah-dt", "2"));
    T.eq(el.getAttribute("data-ah-value"), "2");
    click(row(el, "ah-dt", "3"));
    T.eq(el.getAttribute("data-ah-value"), "3");
    click(row(el, "ah-dt", "3"));
    T.eq(el.getAttribute("data-ah-value"), "", "a click on the selected row clears it");
    var m = await mount(fx, "dta");
    click(row(m, "ah-dt", "1"));
    click(row(m, "ah-dt", "4"), { shiftKey: true });
    T.eq(m.getAttribute("data-ah-value"), "1,2,3,4");
    click(row(m, "ah-dt", "2"), { metaKey: true });
    T.eq(m.getAttribute("data-ah-value"), "1,3,4");
  });

  T.test("datatable: search and advanced filters", async function (fx) {
    var el = await mount(fx, "dts");
    set(q(el, ".ah-dt-search-input"), " OSL ");
    await wait(260);
    T.eq(shown(el, "ah-dt"), ["1", "3"]);
    AH.invoke(el, "setSearch", "e");
    T.eq(shown(el, "ah-dt"), ["2", "5"]);
    T.eq(el.getAttribute("data-search"), "e");
    var a = await mount(fx, "dta");
    set(q(a, ".ah-dt-adv-filter-select[data-field=age]"), "gte", "change");
    T.eq(shown(a, "ah-dt").length, 5, "no value, no filter");
    set(q(a, ".ah-dt-adv-filter-input[data-field=age]"), "31");
    await wait(260);
    T.eq(shown(a, "ah-dt"), ["1", "3", "5"]);
    set(q(a, ".ah-dt-adv-filter-select[data-field=city]"), "empty", "change");
    T.ok(q(a, ".ah-dt-adv-filter-input[data-field=city]").disabled);
    T.eq(shown(a, "ah-dt"), []);
    set(q(a, ".ah-dt-adv-filter-select[data-field=city]"), "not_empty", "change");
    T.eq(shown(a, "ah-dt"), ["1", "3", "5"]);
    set(q(a, ".ah-dt-adv-filter-select[data-field=name]"), "ends_with", "change");
    set(q(a, ".ah-dt-adv-filter-input[data-field=name]"), "E");
    await wait(260);
    T.eq(shown(a, "ah-dt"), ["5"]);
  });

  T.test("datatable: row details open and close", async function (fx) {
    var el = await mount(fx, "dt");
    var ev = [];
    on(el, "ah:row-expand ah:row-collapse", function (e) { ev.push(e.type + ":" + e.detail.key); });
    var btn = q(row(el, "ah-dt", "2"), ".ah-dt-expand-btn");
    var details = row(el, "ah-dt", "2").nextSibling;
    T.ok(details.classList.contains("ah-dt-row-details-hidden"));
    click(btn);
    T.ok(!details.classList.contains("ah-dt-row-details-hidden"));
    T.eq(btn.getAttribute("aria-expanded"), "true");
    T.eq(el.getAttribute("data-expanded"), "2");
    T.eq(el.getAttribute("data-ah-value"), "", "the arrow does not select");
    key(row(el, "ah-dt", "2"), "ArrowLeft");
    T.ok(details.classList.contains("ah-dt-row-details-hidden"));
    T.eq(el.hasAttribute("data-expanded"), false);
    AH.invoke(el, "expandRow", "1");
    T.eq(el.getAttribute("data-expanded"), "1");
    T.eq(ev, ["ah:row-expand:2", "ah:row-collapse:2"]);
  });

  T.test("datatable: edits cells and sends them to the edit action", async function (fx) {
    var el = await mount(fx, "dt");
    var calls = [];
    var restore = stubFetch([OPS.edit], calls);
    try {
      var edits = [];
      on(el, "ah:cell-edit", function (e) { edits.push(e.detail); });
      var cell = child(row(el, "ah-dt", "1"), "td[data-field=name]");
      T.fire(cell, "dblclick");
      var input = cell.querySelector(".ah-dt-editor");
      T.ok(input && input.type === "text" && input.value === "Ann");
      input.value = "Anna";
      key(input, "Escape");
      T.eq(cell.textContent, "Ann", "Escape cancels");
      T.eq(edits.length, 0);
      // numbers edit their raw value
      var age = child(row(el, "ah-dt", "2"), "td[data-field=age]");
      T.fire(age, "dblclick");
      T.eq(age.querySelector("input").type + ":" + age.querySelector("input").value, "number:25");
      key(age.querySelector("input"), "Tab", { shiftKey: true });
      var name2 = child(row(el, "ah-dt", "2"), "td[data-field=name]");
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
      T.eq(child(row(el, "ah-dt", "2"), "td[data-field=name]").textContent, "Robert");
      T.eq(child(row(el, "ah-dt", "2"), "td[data-field=age]").textContent, "26 y");
      T.ok(!row(el, "ah-dt", "2").hidden && row(el, "ah-dt", "3").hidden, "refresh keeps the page");
      T.eq(shown(el, "ah-dt").map(function (k) { return alt(el, k); }).join(""), "01");
    } finally { restore(); }
  });

  T.test("datatable: column chooser and resize", async function (fx) {
    var el = await mount(fx, "dt");
    var ev = [];
    on(el, "ah:columns", function (e) { ev.push(e.detail.hidden.join(",")); });
    click(q(el, ".ah-dt-chooser-btn"));
    var panel = child(el, ".ah-dt-chooser-panel");
    T.ok(panel.classList.contains("ah-dt-chooser-panel-open"));
    T.eq(q(el, ".ah-dt-chooser-btn").getAttribute("aria-expanded"), "true");
    check(q(panel, ".ah-dt-chooser-checkbox[data-field=city]"), false);
    T.ok(q(el, "th[data-field=city]").hidden);
    T.ok(child(row(el, "ah-dt", "1"), "td[data-field=city]").hidden);
    T.ok(q(el, ".ah-dt-filter-cell[data-field=city]").hidden);
    T.eq(q(el, ".ah-dt-row-details-cell").getAttribute("colspan"), "4");
    T.eq(el.getAttribute("data-hidden"), "city");
    key(document, "Escape");
    T.ok(!panel.classList.contains("ah-dt-chooser-panel-open"));
    AH.invoke(el, "showColumn", "city");
    T.ok(!q(el, "th[data-field=city]").hidden);
    T.ok(q(panel, ".ah-dt-chooser-checkbox[data-field=city]").checked);
    T.eq(ev, ["city", ""]);
    // resize: drag the name column's handle by 40px
    var resized = [];
    on(el, "ah:column-resize", function (e) { resized.push(e.detail); });
    var th = q(el, "th[data-field=name]");
    var w0 = th.getBoundingClientRect().width;
    T.fire(q(th, ".ah-dt-resize-handle"), "mousedown", { button: 0, clientX: 100 });
    T.fire(document, "mousemove", { clientX: 140 });
    T.fire(document, "mouseup", { clientX: 140 });
    T.eq(resized.length, 1);
    T.eq(resized[0].field, "name");
    T.eq(resized[0].width, Math.round(w0 + 40));
    var idx = Array.prototype.indexOf.call(th.parentNode.children, th);
    T.eq(q(el, ".ah-dt-body colgroup").children[idx].style.width, Math.round(w0 + 40) + "px");
    T.fire(document, "mousemove", { clientX: 200 });
    T.eq(q(el, ".ah-dt-body colgroup").children[idx].style.width, Math.round(w0 + 40) + "px",
         "the drag ended on mouseup");
  });

  T.test("datatable: remote queries the server and takes its page", async function (fx) {
    var el = await mount(fx, "dtr");
    var calls = [];
    var restore = stubFetch([[], OPS.query], calls);
    try {
      T.eq(shown(el, "ah-dt"), ["1", "2"]);
      var queries = [];
      on(el, "ah:query", function (e) { var d = e.detail; queries.push(d.sort + ":" + d.dir + ":" + d.page); });
      click(q(el, "th[data-field=age]"));
      T.ok(child(el, ".ah-dt-content").classList.contains("ah-dt-loading"));
      T.eq(keys(el, "ah-dt"), ["1", "2"], "no local sorting");
      click(q(el, "th[data-field=age]"));
      q(el, ".ah-dt-filter-input[data-field=city]").value = "o";
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
      T.eq(q(el, ".ah-dt-pager-info").textContent, "3-4 of 5");
      T.eq(el.getAttribute("data-ah-value"), "3");
      T.ok(!child(el, ".ah-dt-content").classList.contains("ah-dt-loading"));
      T.ok(q(el, "th[data-field=age]").classList.contains("ah-dt-sort-desc"));
    } finally { restore(); }
  });

  T.test("datatable: keyboard sorts headers; methods sort, page and select silently", async function (fx) {
    var el = await mount(fx, "dts");
    var changes = 0, sorts = [];
    on(el, "change", function (e) { if (e.target === el) { changes++; } });
    on(el, "ah:sort", function (e) { sorts.push(e.detail.field + ":" + e.detail.dir); });
    var th = q(el, "th[data-field=age]");
    key(th, "Enter");
    T.eq(th.getAttribute("aria-sort"), "ascending");
    T.eq(sorts, ["age:asc"]);
    AH.invoke(el, "sort", "age", "desc");
    T.eq(el.getAttribute("data-sort-dir"), "desc");
    T.ok(th.classList.contains("ah-dt-sort-desc"));
    AH.invoke(el, "sort", "age", null);
    T.eq(th.hasAttribute("aria-sort"), false);
    T.eq(keys(el, "ah-dt"), ["1", "2", "3", "4", "5"]);
    AH.invoke(el, "setValue", "4");
    T.eq(AH.invoke(el, "getValue"), "4");
    T.eq(row(el, "ah-dt", "4").getAttribute("aria-selected"), "true");
    AH.invoke(el, "clearSelection");
    T.eq(AH.invoke(el, "getValue"), "");
    T.eq(changes, 0, "methods fire no change");
    var clicks = [];
    on(el, "ah:row-click ah:row-dblclick", function (e) { clicks.push(e.type + ":" + e.detail.key); });
    click(row(el, "ah-dt", "5"));
    T.fire(row(el, "ah-dt", "5"), "dblclick");
    T.eq(clicks, ["ah:row-click:5", "ah:row-dblclick:5"]);
    T.eq(el.getAttribute("data-key"), "5");
    T.eq(changes, 1);
  });

  // ---- links (href) ---------------------------------------------------

  // Clicks on the fixture after the table handled them: whether the
  // table took them (defaultPrevented); then keeps the page from leaving.
  function watchClicks(fx) {
    var seen = [];
    var h = function (e) { seen.push(e.defaultPrevented); e.preventDefault(); };
    fx.addEventListener("click", h);
    return seen;
  }

  T.test("datatable: href pager links page in place and push the URL", async function (fx) {
    var start = location.href;
    try {
      var el = await mount(fx, "dth");
      var seen = watchClicks(fx);
      var link = q(el, "a.ah-dt-pager-btn-num[data-page='2']");
      T.eq(link.getAttribute("href"), "/t?p=2&s=2&o=&q=");
      var pages = [];
      on(el, "ah:page", function (e) { pages.push(e.detail.page); });
      click(link, { button: 0 });
      T.eq(seen, [true], "a plain click is taken");
      T.eq(shown(el, "ah-dt"), ["3", "4"]);
      T.eq(pages, [2]);
      T.ok(location.href.endsWith("/t?p=2&s=2&o=&q="), location.href);
      T.eq(history.state && history.state.ah, true);
      // the pager re-rendered here links with the current sort
      click(q(el, "th[data-field=age]"));
      var next = q(el, "a.ah-dt-pager-btn-next");
      T.eq(next.getAttribute("href"), "/t?p=2&s=2&o=age%3Aasc&q=");
      T.eq(q(el, "button.ah-dt-pager-btn-prev").disabled, true, "disabled prev is no link");
      click(next, { button: 0 });
      T.ok(location.href.endsWith("/t?p=2&s=2&o=age%3Aasc&q="), location.href);
      T.eq(shown(el, "ah-dt"), ["1", "5"]);
      // modified clicks are left to the browser (new tab / window)
      var before = location.href;
      click(q(el, "a.ah-dt-pager-btn-prev"), { button: 0, ctrlKey: true });
      click(q(el, "a.ah-dt-pager-btn-prev"), { button: 1 });
      T.eq(seen.slice(3), [false, false]);
      T.eq(shown(el, "ah-dt"), ["1", "5"]);
      T.eq(location.href, before);
      // methods page without touching the history
      AH.invoke(el, "goToPage", 3);
      T.eq(shown(el, "ah-dt"), ["3"]);
      T.eq(location.href, before);
    } finally { history.replaceState(null, "", start); }
  });

  T.test("datatable: href links in remote mode query the server and push", async function (fx) {
    var start = location.href;
    var calls = [];
    var restore = stubFetch([[]], calls);
    try {
      var el = await mount(fx, "dthr");
      var seen = watchClicks(fx);
      var queries = [];
      on(el, "ah:query", function (e) { queries.push(e.detail.page); });
      click(q(el, "a.ah-dt-pager-btn-next"), { button: 0 });
      T.eq(seen, [true]);
      T.eq(queries, [2]);
      T.ok(location.href.endsWith("/t?p=2&s=2&o=&q="), location.href);
      await wait(60);
      T.eq(calls.length, 1);
      T.eq(calls[0].event.data.page, "2");
    } finally { restore(); history.replaceState(null, "", start); }
  });

  T.test("datatable: removed and inserted again it works (cleanup)", async function (fx) {
    var el = await mount(fx, "dt");
    click(q(el, ".ah-dt-chooser-btn"));
    fx.removeChild(el);
    await wait(0);
    fx.appendChild(el);
    await T.ready(fx);
    // a moved element keeps its controller: still one listener, one change
    var changes = 0;
    on(el, "change", function (e) { if (e.target === el) { changes++; } });
    check(q(row(el, "ah-dt", "1"), ".ah-dt-row-checkbox"), true);
    T.eq(el.getAttribute("data-ah-value"), "1");
    T.eq(changes, 1);
    // a fresh copy of the markup works from scratch
    fx.innerHTML = "";
    await wait(0);
    var again = await mount(fx, "dt");
    click(q(again, ".ah-dt-pager-btn-next"));
    T.eq(shown(again, "ah-dt"), ["3", "4"]);
  });
})(window.AHTest, window.AH);
