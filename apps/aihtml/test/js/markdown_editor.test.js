/* markdown_editor: the markdown-editor behaviour on markup the server
 * renders, with the real ProseMirror bundle loaded through AH.vendor as on
 * a page. SERVER holds renders of aihtml_markdown_editor:markdown_editor/3:
 *   basic     (<<"# Title\n\nSome **bold** text.\n">>, [], [{id, m1}, {name, doc}])
 *   empty     (<<>>, [], [{id, m2}, {placeholder, <<"Write here">>}, {max_chars, 10}])
 *   readonly  (<<"- [x] done\n- [ ] todo\n">>, [readonly], [{id, m3}])
 * regenerate them from Erlang if the markup changes. */
(function (T, $, AH) {
  "use strict";

  var SERVER = {
    "basic": "<div class=\"ah-md-editor\" id=\"m1\" data-ah=\"markdown-editor\" data-ah-value=\"# Title\n\nSome **bold** text.\n\" data-ah-placeholder=\"Start typing...\" data-ah-labels=\"{&quot;editor&quot;:&quot;Markdown editor&quot;,&quot;enter_url&quot;:&quot;Enter URL:&quot;,&quot;enter_image_url&quot;:&quot;Image URL:&quot;}\"><div class=\"ah-pm-content\"><textarea class=\"ah-md-editor-source\" id=\"m1-source\" name=\"doc\" aria-label=\"Markdown editor\" placeholder=\"Start typing...\" rows=\"10\" spellcheck=\"false\"># Title\n\nSome **bold** text.\n</textarea><div class=\"ah-pm-block-handle\" aria-hidden=\"true\"><button class=\"ah-pm-block-handle-add\" type=\"button\" tabindex=\"-1\" title=\"Add Block\" aria-label=\"Add Block\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"><line x1=\"8\" y1=\"3\" x2=\"8\" y2=\"13\"/><line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/></svg></button><button class=\"ah-pm-block-handle-drag\" type=\"button\" tabindex=\"-1\" title=\"Drag to Sort\" aria-label=\"Drag to Sort\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"currentColor\"><circle cx=\"6\" cy=\"4\" r=\"1.2\"/><circle cx=\"10\" cy=\"4\" r=\"1.2\"/><circle cx=\"6\" cy=\"8\" r=\"1.2\"/><circle cx=\"10\" cy=\"8\" r=\"1.2\"/><circle cx=\"6\" cy=\"12\" r=\"1.2\"/><circle cx=\"10\" cy=\"12\" r=\"1.2\"/></svg></button></div><div class=\"ah-pm-drag-indicator\" aria-hidden=\"true\"></div><span class=\"ah-md-editor-caret\" aria-hidden=\"true\"></span><div class=\"ah-pm-slash-menu\" id=\"m1-menu\"><div class=\"ah-pm-slash-menu-tabs\" aria-hidden=\"true\"><button class=\"ah-pm-slash-menu-tab active\" type=\"button\" tabindex=\"-1\" data-group=\"Text\">Heading</button><button class=\"ah-pm-slash-menu-tab\" type=\"button\" tabindex=\"-1\" data-group=\"List\">List</button><button class=\"ah-pm-slash-menu-tab\" type=\"button\" tabindex=\"-1\" data-group=\"Advanced\">Other</button></div><div class=\"ah-pm-slash-menu-content\" id=\"m1-menu-list\" role=\"listbox\" aria-label=\"Markdown editor\"><div class=\"ah-pm-slash-menu-group\" data-group=\"Text\" role=\"group\" aria-labelledby=\"m1-menu-Text\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m1-menu-Text\">Heading</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item selected\" id=\"m1-menu-0\" role=\"option\" data-type=\"paragraph\" aria-selected=\"true\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M13 4v16\"/><path d=\"M3 4h18\"/><path d=\"M7 4v4\"/><path d=\"M17 4v4\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Paragraph</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-1\" role=\"option\" data-type=\"heading\" data-level=\"1\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M17 12l3-2v8\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 1</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-2\" role=\"option\" data-type=\"heading\" data-level=\"2\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M21 18h-4c0-4 4-3 4-6 0-1.5-2-2.5-4-1\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 2</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-3\" role=\"option\" data-type=\"heading\" data-level=\"3\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M17.5 15.5c1 1 2.5 1 3.5 0 .5-.5.5-1.5 0-2-.5-.5-1.5-.5-2 0\"/><path d=\"M17 10.5c1-1 2.5-1 3.5 0 .5.5.5 1.5 0 2\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 3</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-4\" role=\"option\" data-type=\"blockquote\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M3 21c3 0 7-1 7-8V5c0-1.25-.756-2.017-2-2H4c-1.25 0-2 .75-2 1.972V11c0 1.25.75 2 2 2 1 0 1 0 1 1v1c0 1-1 2-2 2s-1 .008-1 1.031V21z\"/><path d=\"M15 21c3 0 7-1 7-8V5c0-1.25-.757-2.017-2-2h-4c-1.25 0-2 .75-2 1.972V11c0 1.25.75 2 2 2h.75c0 2.25.25 4-2.75 4v3z\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Quote</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-5\" role=\"option\" data-type=\"horizontal_rule\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"3\" y1=\"12\" x2=\"21\" y2=\"12\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Horizontal Rule</span></div></div></div><div class=\"ah-pm-slash-menu-group\" data-group=\"List\" role=\"group\" aria-labelledby=\"m1-menu-List\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m1-menu-List\">List</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-6\" role=\"option\" data-type=\"bullet_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"9\" y1=\"6\" x2=\"20\" y2=\"6\"/><line x1=\"9\" y1=\"12\" x2=\"20\" y2=\"12\"/><line x1=\"9\" y1=\"18\" x2=\"20\" y2=\"18\"/><circle cx=\"4\" cy=\"6\" r=\"1\" fill=\"currentColor\"/><circle cx=\"4\" cy=\"12\" r=\"1\" fill=\"currentColor\"/><circle cx=\"4\" cy=\"18\" r=\"1\" fill=\"currentColor\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Bullet List</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-7\" role=\"option\" data-type=\"ordered_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"10\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"10\" y1=\"12\" x2=\"21\" y2=\"12\"/><line x1=\"10\" y1=\"18\" x2=\"21\" y2=\"18\"/><text x=\"2\" y=\"8\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">1</text><text x=\"2\" y=\"14\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">2</text><text x=\"2\" y=\"20\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">3</text></svg></span><span class=\"ah-pm-slash-menu-item-label\">Ordered List</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-8\" role=\"option\" data-type=\"task_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"5\" width=\"6\" height=\"6\" rx=\"1\"/><path d=\"M3 17l2 2 4-4\"/><line x1=\"13\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"13\" y1=\"12\" x2=\"21\" y2=\"12\"/><line x1=\"13\" y1=\"18\" x2=\"21\" y2=\"18\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Task List</span></div></div></div><div class=\"ah-pm-slash-menu-group\" data-group=\"Advanced\" role=\"group\" aria-labelledby=\"m1-menu-Advanced\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m1-menu-Advanced\">Other</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-9\" role=\"option\" data-type=\"image\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\" ry=\"2\"/><circle cx=\"8.5\" cy=\"8.5\" r=\"1.5\"/><polyline points=\"21 15 16 10 5 21\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Image</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-10\" role=\"option\" data-type=\"code_block\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><polyline points=\"16 18 22 12 16 6\"/><polyline points=\"8 6 2 12 8 18\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Code Block</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m1-menu-11\" role=\"option\" data-type=\"table\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\"/><line x1=\"3\" y1=\"9\" x2=\"21\" y2=\"9\"/><line x1=\"3\" y1=\"15\" x2=\"21\" y2=\"15\"/><line x1=\"9\" y1=\"3\" x2=\"9\" y2=\"21\"/><line x1=\"15\" y1=\"3\" x2=\"15\" y2=\"21\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Table</span></div></div></div></div></div></div></div>",
    "empty": "<div class=\"ah-md-editor\" id=\"m2\" data-ah=\"markdown-editor\" data-ah-value=\"\" data-ah-placeholder=\"Write here\" data-ah-max-chars=\"10\" data-ah-labels=\"{&quot;editor&quot;:&quot;Markdown editor&quot;,&quot;enter_url&quot;:&quot;Enter URL:&quot;,&quot;enter_image_url&quot;:&quot;Image URL:&quot;}\"><div class=\"ah-pm-content\"><textarea class=\"ah-md-editor-source\" id=\"m2-source\" aria-label=\"Markdown editor\" placeholder=\"Write here\" rows=\"10\" spellcheck=\"false\"></textarea><div class=\"ah-pm-block-handle\" aria-hidden=\"true\"><button class=\"ah-pm-block-handle-add\" type=\"button\" tabindex=\"-1\" title=\"Add Block\" aria-label=\"Add Block\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\"><line x1=\"8\" y1=\"3\" x2=\"8\" y2=\"13\"/><line x1=\"3\" y1=\"8\" x2=\"13\" y2=\"8\"/></svg></button><button class=\"ah-pm-block-handle-drag\" type=\"button\" tabindex=\"-1\" title=\"Drag to Sort\" aria-label=\"Drag to Sort\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 16 16\" fill=\"currentColor\"><circle cx=\"6\" cy=\"4\" r=\"1.2\"/><circle cx=\"10\" cy=\"4\" r=\"1.2\"/><circle cx=\"6\" cy=\"8\" r=\"1.2\"/><circle cx=\"10\" cy=\"8\" r=\"1.2\"/><circle cx=\"6\" cy=\"12\" r=\"1.2\"/><circle cx=\"10\" cy=\"12\" r=\"1.2\"/></svg></button></div><div class=\"ah-pm-drag-indicator\" aria-hidden=\"true\"></div><span class=\"ah-md-editor-caret\" aria-hidden=\"true\"></span><div class=\"ah-pm-slash-menu\" id=\"m2-menu\"><div class=\"ah-pm-slash-menu-tabs\" aria-hidden=\"true\"><button class=\"ah-pm-slash-menu-tab active\" type=\"button\" tabindex=\"-1\" data-group=\"Text\">Heading</button><button class=\"ah-pm-slash-menu-tab\" type=\"button\" tabindex=\"-1\" data-group=\"List\">List</button><button class=\"ah-pm-slash-menu-tab\" type=\"button\" tabindex=\"-1\" data-group=\"Advanced\">Other</button></div><div class=\"ah-pm-slash-menu-content\" id=\"m2-menu-list\" role=\"listbox\" aria-label=\"Markdown editor\"><div class=\"ah-pm-slash-menu-group\" data-group=\"Text\" role=\"group\" aria-labelledby=\"m2-menu-Text\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m2-menu-Text\">Heading</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item selected\" id=\"m2-menu-0\" role=\"option\" data-type=\"paragraph\" aria-selected=\"true\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M13 4v16\"/><path d=\"M3 4h18\"/><path d=\"M7 4v4\"/><path d=\"M17 4v4\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Paragraph</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-1\" role=\"option\" data-type=\"heading\" data-level=\"1\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M17 12l3-2v8\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 1</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-2\" role=\"option\" data-type=\"heading\" data-level=\"2\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M21 18h-4c0-4 4-3 4-6 0-1.5-2-2.5-4-1\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 2</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-3\" role=\"option\" data-type=\"heading\" data-level=\"3\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M4 12h8\"/><path d=\"M4 4v16\"/><path d=\"M12 4v16\"/><path d=\"M17.5 15.5c1 1 2.5 1 3.5 0 .5-.5.5-1.5 0-2-.5-.5-1.5-.5-2 0\"/><path d=\"M17 10.5c1-1 2.5-1 3.5 0 .5.5.5 1.5 0 2\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Heading 3</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-4\" role=\"option\" data-type=\"blockquote\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><path d=\"M3 21c3 0 7-1 7-8V5c0-1.25-.756-2.017-2-2H4c-1.25 0-2 .75-2 1.972V11c0 1.25.75 2 2 2 1 0 1 0 1 1v1c0 1-1 2-2 2s-1 .008-1 1.031V21z\"/><path d=\"M15 21c3 0 7-1 7-8V5c0-1.25-.757-2.017-2-2h-4c-1.25 0-2 .75-2 1.972V11c0 1.25.75 2 2 2h.75c0 2.25.25 4-2.75 4v3z\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Quote</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-5\" role=\"option\" data-type=\"horizontal_rule\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"3\" y1=\"12\" x2=\"21\" y2=\"12\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Horizontal Rule</span></div></div></div><div class=\"ah-pm-slash-menu-group\" data-group=\"List\" role=\"group\" aria-labelledby=\"m2-menu-List\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m2-menu-List\">List</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-6\" role=\"option\" data-type=\"bullet_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"9\" y1=\"6\" x2=\"20\" y2=\"6\"/><line x1=\"9\" y1=\"12\" x2=\"20\" y2=\"12\"/><line x1=\"9\" y1=\"18\" x2=\"20\" y2=\"18\"/><circle cx=\"4\" cy=\"6\" r=\"1\" fill=\"currentColor\"/><circle cx=\"4\" cy=\"12\" r=\"1\" fill=\"currentColor\"/><circle cx=\"4\" cy=\"18\" r=\"1\" fill=\"currentColor\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Bullet List</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-7\" role=\"option\" data-type=\"ordered_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><line x1=\"10\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"10\" y1=\"12\" x2=\"21\" y2=\"12\"/><line x1=\"10\" y1=\"18\" x2=\"21\" y2=\"18\"/><text x=\"2\" y=\"8\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">1</text><text x=\"2\" y=\"14\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">2</text><text x=\"2\" y=\"20\" font-size=\"7\" fill=\"currentColor\" stroke=\"none\" font-weight=\"bold\">3</text></svg></span><span class=\"ah-pm-slash-menu-item-label\">Ordered List</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-8\" role=\"option\" data-type=\"task_list\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"5\" width=\"6\" height=\"6\" rx=\"1\"/><path d=\"M3 17l2 2 4-4\"/><line x1=\"13\" y1=\"6\" x2=\"21\" y2=\"6\"/><line x1=\"13\" y1=\"12\" x2=\"21\" y2=\"12\"/><line x1=\"13\" y1=\"18\" x2=\"21\" y2=\"18\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Task List</span></div></div></div><div class=\"ah-pm-slash-menu-group\" data-group=\"Advanced\" role=\"group\" aria-labelledby=\"m2-menu-Advanced\"><div class=\"ah-pm-slash-menu-group-heading\" id=\"m2-menu-Advanced\">Other</div><div class=\"ah-pm-slash-menu-group-items\" role=\"presentation\"><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-9\" role=\"option\" data-type=\"image\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\" ry=\"2\"/><circle cx=\"8.5\" cy=\"8.5\" r=\"1.5\"/><polyline points=\"21 15 16 10 5 21\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Image</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-10\" role=\"option\" data-type=\"code_block\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><polyline points=\"16 18 22 12 16 6\"/><polyline points=\"8 6 2 12 8 18\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Code Block</span></div><div class=\"ah-pm-slash-menu-item\" id=\"m2-menu-11\" role=\"option\" data-type=\"table\" aria-selected=\"false\"><span class=\"ah-pm-slash-menu-item-icon\" aria-hidden=\"true\"><svg width=\"16\" height=\"16\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\"><rect x=\"3\" y=\"3\" width=\"18\" height=\"18\" rx=\"2\"/><line x1=\"3\" y1=\"9\" x2=\"21\" y2=\"9\"/><line x1=\"3\" y1=\"15\" x2=\"21\" y2=\"15\"/><line x1=\"9\" y1=\"3\" x2=\"9\" y2=\"21\"/><line x1=\"15\" y1=\"3\" x2=\"15\" y2=\"21\"/></svg></span><span class=\"ah-pm-slash-menu-item-label\">Table</span></div></div></div></div></div></div><div class=\"ah-pm-stats\"><span class=\"ah-pm-stats__item\"><span class=\"ah-pm-stats__label\">Chars</span><span class=\"ah-pm-stats__value\" data-stat=\"chars\">\u00e2\u0080\u0093</span></span><span class=\"ah-pm-stats__sep\" aria-hidden=\"true\">|</span><span class=\"ah-pm-stats__item\"><span class=\"ah-pm-stats__label\">Words</span><span class=\"ah-pm-stats__value\" data-stat=\"words\">\u00e2\u0080\u0093</span></span><span class=\"ah-pm-stats__sep\" aria-hidden=\"true\">|</span><span class=\"ah-pm-stats__item\"><span class=\"ah-pm-stats__label\">Paragraphs</span><span class=\"ah-pm-stats__value\" data-stat=\"paragraphs\">\u00e2\u0080\u0093</span></span><span class=\"ah-pm-stats__sep\" aria-hidden=\"true\">|</span><span class=\"ah-pm-stats__item\"><span class=\"ah-pm-stats__label\">Limit</span><span class=\"ah-pm-stats__value\" data-stat=\"limit\">\u00e2\u0080\u0093/10</span></span><span class=\"ah-pm-stats__warning\" hidden>Limit exceeded</span></div></div>",
    "readonly": "<div class=\"ah-md-editor ah-md-editor-readonly\" id=\"m3\" data-ah=\"markdown-editor\" data-ah-value=\"- [x] done\n- [ ] todo\n\" data-ah-placeholder=\"Start typing...\" data-ah-readonly data-ah-labels=\"{&quot;editor&quot;:&quot;Markdown editor&quot;,&quot;enter_url&quot;:&quot;Enter URL:&quot;,&quot;enter_image_url&quot;:&quot;Image URL:&quot;}\"><div class=\"ah-pm-content\"><textarea class=\"ah-md-editor-source\" id=\"m3-source\" aria-label=\"Markdown editor\" placeholder=\"Start typing...\" rows=\"10\" spellcheck=\"false\" readonly>- [x] done\n- [ ] todo\n</textarea></div></div>"
  };

  // aihtml.css is not on the test page; ProseMirror needs its white-space
  // rule (extra/markdown_editor.css) to read typed spaces as spaces.
  $("<style>.ProseMirror { white-space: pre-wrap; }</style>").appendTo(document.head);

  function sleep(ms) { return new Promise(function (r) { setTimeout(r, ms); }); }

  async function mount(fx, name) {
    fx.innerHTML = SERVER[name];
    var el = fx.firstChild;
    AH.mount(fx);
    var view = await AH.invoke(el, "ready");
    return { el: el, view: view };
  }

  // Types like a user: each character goes through the browser's
  // insertText, which ProseMirror reads back from the DOM (running input
  // rules and handleTextInput as for real key presses).
  async function type(view, text) {
    view.focus();
    for (var i = 0; i < text.length; i++) {
      document.execCommand("insertText", false, text[i]);
      await sleep(0);
    }
    await sleep(20);
  }

  function key(view, k, opts) {
    var e = new KeyboardEvent("keydown", $.extend({ key: k, bubbles: true, cancelable: true }, opts));
    view.dom.dispatchEvent(e);
  }

  function events(el) {
    var log = [];
    $(el).on("input change", function (e) {
      if (e.target === el) { log.push(e.type + ":" + el.getAttribute("data-ah-value")); }
    });
    return log;
  }

  T.test("markdown_editor: loads ProseMirror, renders the Markdown, hides the textarea", async function (fx) {
    var m = await mount(fx, "basic"), el = m.el;
    AHTest.ok(el.classList.contains("ah-md-editor-ready"));
    AHTest.ok(el.querySelector(".ah-md-editor-source").hidden, "textarea hidden");
    var pm = el.querySelector(".ProseMirror");
    AHTest.eq(pm.parentNode, el.querySelector(".ah-pm-content"));
    AHTest.eq(pm.querySelector("h1").textContent, "Title");
    AHTest.eq(pm.querySelector("h1").className, "ah-md-h1");
    AHTest.eq(pm.querySelector("p strong").textContent, "bold");
    AHTest.eq(pm.getAttribute("role"), "textbox");
    AHTest.eq(pm.getAttribute("aria-label"), "Markdown editor");
    AHTest.eq(pm.getAttribute("aria-controls"), "m1-menu-list");
    // untouched: the value stays what the server sent
    AHTest.eq(AH.invoke(el, "getValue"), "# Title\n\nSome **bold** text.\n");
  });

  T.test("markdown_editor: typing fires input, updates data-ah-value and the textarea", async function (fx) {
    var m = await mount(fx, "basic"), el = m.el, view = m.view, log = events(el);
    view.dispatch(view.state.tr.setSelection(
      AHProseMirror.state.Selection.atEnd(view.state.doc)));
    await type(view, " More");
    AHTest.eq(el.getAttribute("data-ah-value"), "# Title\n\nSome **bold** text. More");
    AHTest.eq(el.querySelector("textarea").value, "# Title\n\nSome **bold** text. More");
    AHTest.eq(el.querySelector("textarea").name, "doc");
    AHTest.ok(log.length >= 5 && log.every(function (l) { return l.indexOf("input:") === 0; }), log.join("|"));
  });

  T.test("markdown_editor: input rules (heading, bold, italic, code, lists, quote, link)", async function (fx) {
    var m = await mount(fx, "empty"), el = m.el, view = m.view;
    await type(view, "# Head");
    key(view, "Enter");
    await type(view, "a **b** and *i* `c` ~~s~~ [l](http://x.io)");
    key(view, "Enter");
    await type(view, "- one");
    key(view, "Enter");
    await type(view, "two");
    key(view, "Enter");
    key(view, "Enter");
    await type(view, "1. first");
    key(view, "Enter");
    key(view, "Enter");
    await type(view, "> quote");
    AHTest.eq(AH.invoke(el, "getValue"),
              "# Head\n\na **b** and *i* `c` ~~s~~ [l](http://x.io)\n\n- one\n- two\n\n" +
              "1. first\n\n> quote");
    AHTest.ok(view.dom.querySelector("h1") && view.dom.querySelector("blockquote"));
  });

  T.test("markdown_editor: code block, rule, task list, typography", async function (fx) {
    var m = await mount(fx, "empty"), view = m.view;
    await type(view, "``` ");
    await type(view, "x = 1");
    AHTest.eq(view.state.doc.firstChild.type.name, "code_block");
    key(view, "ArrowDown");
    await type(view, "--- ");
    await type(view, "[ ] task");
    AHTest.eq(AH.invoke(m.el, "getValue"), "```\nx = 1\n```\n\n---\n\n- [ ] task");
    AH.invoke(m.el, "setValue", "");
    await type(view, "wait... a--b \"q\"");
    AHTest.eq(AH.invoke(m.el, "getValue"), "wait… a—b “q”");
  });

  T.test("markdown_editor: change fires on blur, once, with the Markdown", async function (fx) {
    var m = await mount(fx, "empty"), el = m.el, view = m.view, log = events(el);
    var input = document.createElement("input");
    fx.appendChild(input);
    await type(view, "hi");
    AHTest.eq(log.filter(function (l) { return /^change/.test(l); }).length, 0, "no change while typing");
    input.focus();
    await sleep(10);
    AHTest.eq(log.slice(-1)[0], "change:hi");
    view.focus();
    input.focus();
    await sleep(10);
    AHTest.eq(log.filter(function (l) { return /^change/.test(l); }).length, 1, "no change without an edit");
  });

  T.test("markdown_editor: setValue is silent, exec without focus fires change", async function (fx) {
    var m = await mount(fx, "basic"), el = m.el, log = events(el);
    AH.invoke(el, "setValue", "plain *text*");
    AHTest.eq(log.length, 0);
    AHTest.eq(AH.invoke(el, "getValue"), "plain *text*");
    AHTest.eq(el.querySelector("textarea").value, "plain *text*");
    AHTest.eq(m.view.dom.querySelector("em").textContent, "text");
    var view = m.view;
    view.dispatch(view.state.tr.setSelection(AHProseMirror.state.TextSelection.create(view.state.doc, 1, 6)));
    AH.invoke(el, "exec", "bold");
    AHTest.eq(AH.invoke(el, "getValue"), "**plain** *text*");
    AHTest.eq(log.slice(-1)[0], "change:**plain** *text*");
    AH.invoke(el, "exec", "heading", { level: 2 });
    AHTest.eq(AH.invoke(el, "getValue"), "## **plain** *text*");
    AH.invoke(el, "exec", "undo");
    AHTest.eq(AH.invoke(el, "getValue"), "**plain** *text*");
    AHTest.eq(AH.invoke(el, "getHtml"), "<p><strong>plain</strong> <em>text</em></p>");
    AHTest.eq(AH.invoke(el, "getJson").content[0].type, "paragraph");
  });

  T.test("markdown_editor: slash menu opens on /, navigates with keys, inserts a block", async function (fx) {
    var m = await mount(fx, "empty"), el = m.el, view = m.view;
    var menu = el.querySelector(".ah-pm-slash-menu");
    await type(view, "/");
    await sleep(40);
    AHTest.ok(menu.classList.contains("ah-pm-slash-menu--visible"), "menu open");
    AHTest.eq(view.dom.getAttribute("aria-expanded"), "true");
    AHTest.eq(view.dom.getAttribute("aria-activedescendant"), "m2-menu-0");
    AHTest.eq(menu.style.position, "fixed");
    key(view, "ArrowDown");
    key(view, "ArrowDown");
    AHTest.eq(view.dom.getAttribute("aria-activedescendant"), "m2-menu-2");
    AHTest.eq(menu.querySelector(".selected .ah-pm-slash-menu-item-label").textContent, "Heading 2");
    key(view, "Enter");
    AHTest.ok(!menu.classList.contains("ah-pm-slash-menu--visible"), "menu closed");
    AHTest.eq(view.dom.getAttribute("aria-expanded"), "false");
    await type(view, "Sub");
    AHTest.eq(AH.invoke(el, "getValue"), "## Sub");
    // Escape closes; a click on an item inserts it
    key(view, "Enter");
    await type(view, "/");
    await sleep(40);
    key(view, "Escape");
    AHTest.ok(!menu.classList.contains("ah-pm-slash-menu--visible"));
    AHTest.eq(AH.invoke(el, "getValue"), "## Sub\n\n/");
    document.execCommand("delete");
    await sleep(20);
    await type(view, "/");
    await sleep(40);
    AHTest.ok(menu.classList.contains("ah-pm-slash-menu--visible"), "menu open again");
    var item = menu.querySelector('[data-type="bullet_list"]');
    item.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, cancelable: true }));
    await type(view, "x");
    AHTest.eq(AH.invoke(el, "getValue"), "## Sub\n\n- x");
  });

  T.test("markdown_editor: tables and task lists round-trip", async function (fx) {
    var m = await mount(fx, "empty"), el = m.el;
    var md = "| a | **b** |\n| --- | --- |\n| 1 | 2 |\n\n- [x] done\n- [ ] todo";
    AH.invoke(el, "setValue", md);
    var view = m.view;
    AHTest.ok(view.dom.querySelector("table.ah-md-table th strong"));
    AHTest.eq(view.dom.querySelectorAll(".task-list-item-checkbox").length, 2);
    var cb = view.dom.querySelectorAll(".task-list-item-checkbox")[1];
    cb.click();
    AHTest.eq(AH.invoke(el, "getValue"), md.replace("- [ ] todo", "- [x] todo"));
  });

  T.test("markdown_editor: stats footer and max chars", async function (fx) {
    var m = await mount(fx, "empty"), el = m.el;
    var foot = el.querySelector(".ah-pm-stats");
    AHTest.eq(foot.querySelector('[data-stat="chars"]').textContent, "0");
    AH.invoke(el, "setValue", "hello world 你好\n\nmore text");
    await sleep(150);
    AHTest.eq(AH.invoke(el, "stats"), { chars: 24, words: 6, paragraphs: 2 });
    AHTest.eq(foot.querySelector('[data-stat="words"]').textContent, "6");
    AHTest.eq(foot.querySelector('[data-stat="limit"]').textContent, "24/10");
    AHTest.ok(foot.querySelector('[data-stat="limit"]').classList.contains("ah-pm-stats__value--warning"));
    AHTest.ok(!foot.querySelector(".ah-pm-stats__warning").hidden);
  });

  T.test("markdown_editor: placeholder, readonly, drag handle move", async function (fx) {
    var m = await mount(fx, "empty"), view = m.view;
    AHTest.eq(view.dom.querySelector(".ah-pm-placeholder").textContent, "Write here");
    AH.invoke(m.el, "setValue", "A\n\nB\n\nC");
    // hover the last block, drag its handle above the first
    var blocks = view.dom.children;
    blocks[2].dispatchEvent(new MouseEvent("mousemove", { bubbles: true }));
    var handle = m.el.querySelector(".ah-pm-block-handle");
    AHTest.ok(handle.classList.contains("ah-pm-block-handle--visible"));
    var drag = handle.querySelector(".ah-pm-block-handle-drag");
    var top = blocks[0].getBoundingClientRect().top;
    drag.dispatchEvent(new MouseEvent("mousedown", { bubbles: true, cancelable: true, button: 0 }));
    document.dispatchEvent(new MouseEvent("mousemove", { bubbles: true, clientY: top + 1 }));
    document.dispatchEvent(new MouseEvent("mouseup", { bubbles: true, clientY: top + 1 }));
    AHTest.eq(AH.invoke(m.el, "getValue"), "C\n\nA\n\nB");
    // keyboard: Alt-ArrowDown moves the block with the cursor
    view.focus();
    view.dispatch(view.state.tr.setSelection(AHProseMirror.state.TextSelection.create(view.state.doc, 1)));
    key(view, "ArrowDown", { altKey: true });
    AHTest.eq(AH.invoke(m.el, "getValue"), "A\n\nC\n\nB");

    var r = await mount(fx, "readonly");
    AHTest.eq(r.view.dom.getAttribute("contenteditable"), "false");
    AHTest.eq(r.view.dom.getAttribute("aria-readonly"), "true");
    AHTest.ok(!r.el.querySelector(".ah-pm-slash-menu"));
    AHTest.ok(r.view.dom.querySelector(".task-list-item-checkbox").disabled);
  });

  T.test("markdown_editor: the textarea works before the editor, destroy restores it", async function (fx) {
    fx.innerHTML = SERVER.basic;
    var el = fx.firstChild, ta = el.querySelector("textarea"), log = events(el);
    AH.mount(fx);
    ta.value = "typed early";
    ta.dispatchEvent(new Event("input", { bubbles: true }));
    AHTest.eq(el.getAttribute("data-ah-value"), "typed early");
    ta.dispatchEvent(new Event("change", { bubbles: true }));
    AHTest.eq(log, ["input:typed early", "change:typed early"]);
    var view = await AH.invoke(el, "ready");
    AHTest.eq(view.state.doc.textContent, "typed early");
    AH.destroy(fx);
    AHTest.ok(!el.querySelector(".ProseMirror"), "ProseMirror removed");
    AHTest.ok(!ta.hidden && !el.classList.contains("ah-md-editor-ready"));
    AH.mount(fx);
    await AH.invoke(el, "ready");
    AHTest.eq(el.querySelectorAll(".ProseMirror").length, 1, "mounts again");
  });
})(window.AHTest, window.jQuery, window.AH);
