{application, stickyNotes,
 [{description, "stickyNotes"},
  {vsn, "0.1"},
  {modules, [
    stickyNotes,
    stickyNotes_app,
    stickyNotes_sup,
    stickyNotes_deps,
    stickyNotes_resource
  ]},
  {registered, []},
  {mod, {stickyNotes_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
